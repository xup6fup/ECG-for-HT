
library(magrittr)
library(glmnet)
library(ggplot2)
library(survival)
library(scales) 
library(cowplot)
library(gridExtra)
library(pROC)

# 0. Data path

postive_cut <- 0.349791174897487

outcome_names <- c('All-cause mortality' = 'all-cause mortality',
                   'New-onset HF' = 'HF')

DATASET_NAME <- c('internal-test' = 'Internal test set', 'community-test' = 'Community test set')

data_path <- './data/raw_data.RData'
plot_path <- './results/Figure 07.pdf'

# 0. KM curve function

KM_PLOT <- function (time, status, show_risk_table = TRUE,
                     x, cont_x = NULL, age = NULL, sex = NULL,
                     MAIN = 'KM curve',
                     km_curve = TRUE, x_lim = c(0, 2, 4, 6), y_lim = 1, xlab_name = 'months', ylab_name = 'IPD',
                     col_list = c('#F79952', '#EA7A81', '#7C2451'),
                     x_name = expression(paste('Low risk'), paste('Median risk'), paste('High risk'))) {
  
  if (!km_curve & (is.null(age) | is.null(sex))) {stop('Need to add age and sex')}
  
  sub_data <- data.frame(time, status, x)
  sub_data[,'x'] <- factor(sub_data[,'x'])
  
  if (!is.null(cont_x)) {sub_data[,'cont_x'] <- cont_x}
  if (!is.null(age)) {sub_data[,'age'] <- age}
  if (!is.null(sex)) {sub_data[,'sex'] <- factor(sex)}
  
  # print(head(sub_data))
  
  sub_data <- sub_data[apply(is.na(sub_data), 1, sum) %in% 0,]
  sub_data[sub_data[,'time'] >= max(x_lim), 'status'] <- 0L
  sub_data[sub_data[,'time'] >= max(x_lim),'time'] <- max(x_lim)
  
  if (sum(sub_data[,'status'] == 1) > 2) {
    
    if (!is.null(cont_x)) {
      
      C_idx_model <- coxph(Surv(time, status) ~ pspline(cont_x, df = 4) + ., data = sub_data[,colnames(sub_data)[!colnames(sub_data) %in% 'x']])
      
      concordance <- concordance(C_idx_model)
      C_idx_txt <- paste0('C-index = ', formatC(concordance[['concordance']], format = 'f', 3), 
                          ' (', formatC(concordance[['concordance']] + qnorm(0.025) * sqrt(concordance[['var']]), format = 'f', 3), 
                          '-', formatC(concordance[['concordance']] + qnorm(0.975) * sqrt(concordance[['var']]), format = 'f', 3), ')')
      
    }
    
    survival_model <- coxph(Surv(time, status) ~ x + ., data = sub_data[,colnames(sub_data)[!colnames(sub_data) %in% 'cont_x']])
    new_data <- data.frame(x = levels(sub_data[,'x']), age = mean(sub_data[,'age']), sex = levels(sub_data[,'sex'])[1])
    
    if (km_curve) {
      
      Predict.Surv <- survfit(as.formula(paste0('Surv(time, status) ~ x')), data = sub_data)
      group_surv_data <- data.frame(group = rep(letters[1:length(x_name)], Predict.Surv$strata),
                                    time = Predict.Surv$time,
                                    risk = (1 - Predict.Surv$surv) * 100) 
      
    } else {
      
      Predict.Surv <- survfit(survival_model, newdata = new_data)
      group_surv_data <- data.frame(group = rep(letters[1:length(x_name)], each = length(Predict.Surv$time)),
                                    time = rep(Predict.Surv$time, 2),
                                    risk = (1 - as.numeric(Predict.Surv$surv)) * 100)
      
    }
    
    group_surv_data <- rbind(group_surv_data, data.frame(group = letters[1:length(x_name)],
                                                         time = 0,
                                                         risk = 0))
    
    for (group in letters[1:length(x_name)]) {
      
      group_surv_data <- rbind(group_surv_data, data.frame(group = group,
                                                           time = max(x_lim),
                                                           risk = max(group_surv_data[group_surv_data[,'group'] %in% group,'risk'])))
      
    }
    
    group_surv_data <- group_surv_data[order(group_surv_data[,'time']),]
    group_surv_data <- group_surv_data[order(group_surv_data[,'group']),]
    group_surv_data[group_surv_data[,'time'] > max(x_lim), 'time'] <- max(x_lim)
    group_surv_data <- group_surv_data[!duplicated(group_surv_data[,c('group', 'time')]),]
    
    max_time <- tapply(group_surv_data[,'time'], group_surv_data[,'group'], max)
    
    y.HR <- summary(survival_model)[['coefficients']][1:(length(x_name) - 1),1]
    se.HR <- summary(survival_model)[['coefficients']][1:(length(x_name) - 1),3]
    y.HR[y.HR > 6] <- 1e5
    se.HR[y.HR > 6] <- 1e6
    HR_txt <- paste0(formatC(exp(y.HR), 2, format = 'f'), ' (',
                     formatC(exp(y.HR - qnorm(0.975) * se.HR), 2, format = 'f'), ', ', 
                     formatC(exp(y.HR + qnorm(0.975) * se.HR), 2, format = 'f'), ')')
    
    HR_txt <- c('Reference', HR_txt)
    
    KM_surv <- survfit(as.formula(paste0('Surv(time, status) ~ x')), data = sub_data)
    
    AT_RISK_TABLE <- data.frame(group = rep(letters[1:length(x_name)], KM_surv$strata),
                                time = KM_surv$time - 1e-5,
                                n.risk = KM_surv$n.risk,
                                surv = KM_surv$surv,
                                stringsAsFactors = FALSE)
    
    
    AT_RISK_TABLE <- rbind(AT_RISK_TABLE, data.frame(group = letters[1:length(x_name)],
                                                     time = 0,
                                                     n.risk = as.numeric(table(sub_data[,'x'])),
                                                     surv = 1))
    
    AT_RISK_TABLE <- AT_RISK_TABLE[order(AT_RISK_TABLE[,'time']),]
    AT_RISK_TABLE <- AT_RISK_TABLE[order(AT_RISK_TABLE[,'group']),]  
    
    AT_RISK_SUMMARY <- data.frame(x = rep(1:length(x_name), each = length(x_lim)),
                                  color = rep(col_list[1:length(x_name)], each = length(x_lim)),
                                  group = rep(letters[1:length(x_name)], each = length(x_lim)),
                                  time = x_lim,
                                  n.risk = NA,
                                  surv = NA,
                                  stringsAsFactors = FALSE)
    
    for (m in 1:length(x_name)) {
      max_time <- max(AT_RISK_TABLE[AT_RISK_TABLE[,'group'] %in% letters[m],'time'])
      for (l in x_lim) {
        summary_pos <- which(AT_RISK_SUMMARY[,'group'] %in% letters[m] & AT_RISK_SUMMARY[,'time'] %in% l)
        if ((l - max_time) > 3) {
          AT_RISK_SUMMARY[summary_pos,'n.risk'] <- 0
          AT_RISK_SUMMARY[summary_pos,'surv'] <- NA
        } else {
          table_pos <- which(AT_RISK_TABLE[,'group'] %in% letters[m] & AT_RISK_TABLE[,'time'] >= min(l, max_time))[1]
          AT_RISK_SUMMARY[summary_pos,'n.risk'] <- AT_RISK_TABLE[table_pos,'n.risk']
          AT_RISK_SUMMARY[summary_pos,'surv'] <- 1 - AT_RISK_TABLE[table_pos,'surv']
        }
      }
    }
    
    AT_RISK_SUMMARY[,'txt'] <- paste0(AT_RISK_SUMMARY[,'n.risk'], '\n(', formatC(AT_RISK_SUMMARY[,'surv'] * 100, 1, format = 'f'), '%)')
    # AT_RISK_SUMMARY[AT_RISK_SUMMARY[,'n.risk'] %in% 0,'txt'] <- ''
    
    time_diff <- max(x_lim) - min(x_lim)
    
    gg_p <- ggplot(AT_RISK_SUMMARY, aes(x = time, y = x, group = group))
    gg_p <- gg_p + geom_text(label = AT_RISK_SUMMARY[,'txt'], color = AT_RISK_SUMMARY[,'color'], size = 3, fontface = 2)
    gg_p <- gg_p + ylim(c(0.5, length(x_name) + 0.5))
    gg_p <- gg_p + xlim(c(min(x_lim) - time_diff * 0.05, max(x_lim) + time_diff * 0.05))
    gg_p <- gg_p + ggtitle('Number at risk/event rate (%)')
    gg_p <- gg_p + theme(plot.title = element_text(color = "#000000", size = 5), legend.position = "none")
    table_p <- gg_p + theme_void()
    
    gg_p <- ggplot(group_surv_data, aes(x = time, y = risk, group = group))
    gg_p <- gg_p + geom_step(aes(color = group), size = 1)
    # gg_p <- gg_p + theme_bw()
    gg_p <- gg_p + theme_classic()
    gg_p <- gg_p + scale_color_manual(values = paste0(col_list[1:length(x_name)], 'A0'))
    gg_p <- gg_p + xlab(xlab_name)
    gg_p <- gg_p + ylab(paste0(ylab_name, " (%)"))
    gg_p <- gg_p + scale_x_continuous(limits = c(0, max(x_lim)), breaks = x_lim, labels = x_lim)
    gg_p <- gg_p + scale_y_continuous(limits = c(0, y_lim * 100), breaks = c(seq(0, y_lim * 100, length.out = 6)))
    gg_p <- gg_p + ggtitle(MAIN)
    
    for (m in 1:length(x_name)) {
      
      gg_p <- gg_p + annotate(geom = "text", x = 0, y = y_lim * (70 + 8.5 * m), label = x_name[m], size = 2.5, color = substr(col_list[m], 1, 7), hjust = 0, fontface = 2)
      gg_p <- gg_p + annotate(geom = "text", x = max(x_lim) * 0.65, y = y_lim * (70 + 8.5 * m), label = HR_txt[m], size = 2.5, color = 'black', hjust = 0)
      
    }
    
    gg_p <- gg_p + annotate(geom = "text", x = max(x_lim) * 0.65, y = y_lim * (70 + 8.5 * (length(x_name) + 1)), label = 'Sex, age-adj HR:', size = 2.5, color = 'black', hjust = 0, fontface = 2)
    if (!is.null(cont_x)) {gg_p <- gg_p + annotate(geom = "text", x = 0, y = y_lim * (70 + 8.5 * (length(x_name) + 2.5)), label = C_idx_txt, size = 2.5, color = 'black', hjust = 0, fontface = 2)}
    gg_p <- gg_p + theme(plot.title = element_text(color = "#000000", size = 12), legend.position = "none")
    
    survival_p <- ggdraw()
    survival_p <- survival_p + draw_plot(gg_p, x = 0, y = 0.25, width = 0.93, height = 0.75)
    survival_p <- survival_p + draw_plot(table_p, x = 0.12, y = 0, width = 0.83, height = 0.25)
    
    if (show_risk_table) {return(survival_p)} else {return(gg_p)}
    
  }
  
}

# 1. Load data

load(data_path)

accuracy_data[,'x'] <- cut(accuracy_data[,'ECG_pred.prob'], breaks = c(-Inf, postive_cut, Inf), label = 1:2)

accuracy_data[,'x2'] <- accuracy_data[,'HT_SEVERITY'] %>% as.integer()
accuracy_data[,'x2'] <- 3 - accuracy_data[,'x2']

# 2. Sub data

dat_list <- list()

current_data <- accuracy_data
current_data[,'status'] <- current_data[,'event[all-cause mortality]']
current_data[,'time'] <- current_data[,'time[all-cause mortality]']
current_data <- current_data[!is.na(current_data[,'status']) & !is.na(current_data[,'time']),]
current_data <- current_data[current_data[,'time'] >= 0,]
current_data[,'time'] <- (current_data[,'time'] + 0.5) / 365.25
dat_list[['All-cause mortality']] <- current_data

current_data <- accuracy_data
current_data[,'status'] <- current_data[,'event[HF]']
current_data[,'time'] <- current_data[,'time[HF]']
current_data <- current_data[!is.na(current_data[,'status']) & !is.na(current_data[,'time']),]
current_data <- current_data[current_data[,'time'] >= 0,]
current_data[,'time'] <- (current_data[,'time'] + 0.5) / 365.25
dat_list[['New-onset HF']] <- current_data

# 3. KM curves

km_p_list <- list()

for (j in 1:length(DATASET_NAME)) {
  
  for (i in 1:length(dat_list)) {
    
    sub_data <- dat_list[[i]]
    sub_data <- sub_data[sub_data[,'DATASET'] %in% names(DATASET_NAME)[j],]
    
    mean_age <- tapply(sub_data[,'AGE'], sub_data[,'x'], mean) %>% round()
    new_x_name <- paste0(c('AI-ECG (-)', 'AI-ECG (+)'), ' (mean age = ', mean_age, ')')
      
    km_p_list[[length(km_p_list) + 1]] <- KM_PLOT(time = sub_data[,'time'], status = sub_data[,'status'],
                                                  x = sub_data[,'x'], cont_x = NULL, age = sub_data[,'AGE'], sex = sub_data[,'GENDER'],
                                                  MAIN = '',
                                                  km_curve = TRUE, x_lim = c(0, 2.5, 5, 7.5, 10), y_lim = 0.5, xlab_name = 'years', ylab_name = names(dat_list)[i],
                                                  col_list = c('#0072B2', '#E69F00'),
                                                  x_name = new_x_name)
    
  }
  
}

# 4. Forest plot

forest_list <- list()

for (q in 1:length(outcome_names)) {
  
  # Get outcome data
  
  current_data <- accuracy_data
  current_data[,'status'] <- current_data[,paste0('event[', outcome_names[q], ']')]
  current_data[,'time'] <- current_data[,paste0('time[', outcome_names[q], ']')]
  current_data <- current_data[!is.na(current_data[,'status']) & !is.na(current_data[,'time']),]
  current_data <- current_data[current_data[,'time'] >= 0,]
  current_data[,'time'] <- (current_data[,'time'] + 0.5) / 365.25
  
  # Get data list
  
  data_list <- list()
  
  check_val <- (current_data[,'HT_SEVERITY'] %in% c('overt-HT', 'subclinical-HT', NA) | !current_data[,'HISTORY[HT]'] %in% 0)

  data_list[[1]] <- current_data[current_data[,'DATASET'] %in% names(DATASET_NAME)[1] & current_data[,'HT_GROUP'] %in% 'HT',]
  data_list[[2]] <- current_data[current_data[,'DATASET'] %in% names(DATASET_NAME)[1] & current_data[,'HT_SEVERITY'] %in% 'overt-HT',]
  data_list[[3]] <- current_data[current_data[,'DATASET'] %in% names(DATASET_NAME)[1] & current_data[,'HT_SEVERITY'] %in% 'subclinical-HT',]
  data_list[[4]] <- current_data[current_data[,'DATASET'] %in% names(DATASET_NAME)[1] & current_data[,'HT_SEVERITY'] %in% 'non-HT',]
  data_list[[5]] <- current_data[current_data[,'DATASET'] %in% names(DATASET_NAME)[2] & current_data[,'HT_GROUP'] %in% 'HT',]
  data_list[[6]] <- current_data[current_data[,'DATASET'] %in% names(DATASET_NAME)[2] & current_data[,'HT_SEVERITY'] %in% 'overt-HT',]
  data_list[[7]] <- current_data[current_data[,'DATASET'] %in% names(DATASET_NAME)[2] & current_data[,'HT_SEVERITY'] %in% 'subclinical-HT',]
  data_list[[8]] <- current_data[current_data[,'DATASET'] %in% names(DATASET_NAME)[2] & current_data[,'HT_SEVERITY'] %in% 'non-HT',]
  
  # Figure parameters
  
  data_names <- c('HT', 'Overt HT', 'Subclinical HT', 'non-HT',
                  'HT', 'Overt HT', 'Subclinical HT', 'non-HT')
  
  data_position <- c(9:6, 4:1)
  
  group_names <- c('Internal test set', 'Community test set')  
  group_position <- c(10, 5)
  
  # For loop
  
  HR_data_list <- list()
  
  for (j in 1:length(data_list)) {
    
    sub_data <- data_list[[j]]

    survival_model <- try(summary(coxph(as.formula(paste0('Surv(time, status) ~ x + AGE + GENDER')), data = sub_data)), silent = TRUE)
    
    if ('try-error' %in% class(survival_model)) {
      
      y.HR <- NA
      se.HR <- NA
      HR_txt <- ''
      
    } else {
      
      y.HR <- survival_model[['coefficients']][1,1]
      se.HR <- survival_model[['coefficients']][1,3]
      HR_txt <- paste0(formatC(exp(y.HR), 2, format = 'f'), ' (',
                       formatC(exp(y.HR - qnorm(0.975) * se.HR), 2, format = 'f'), ', ', 
                       formatC(exp(y.HR + qnorm(0.975) * se.HR), 2, format = 'f'), ')')
      
    }
    
    HR_data_list[[j]] <- data.frame(x = data_position[j],
                                    x_name = data_names[j],
                                    # n = paste0(sum(sub_data[,'status']), '/', nrow(sub_data),
                                    #            ' (', formatC(sum(sub_data[,'status']) / nrow(sub_data) * 100, 1, format = 'f'), '%)'),
                                    n = paste0(sum(sub_data[,'status']), '/', nrow(sub_data)),
                                    y = exp(y.HR), yse = se.HR,
                                    ylo = exp(y.HR - qnorm(0.975) * se.HR),
                                    yhi = exp(y.HR + qnorm(0.975) * se.HR),
                                    txt.HR = HR_txt,
                                    col = '#000000',
                                    size = 1,
                                    stringsAsFactors = FALSE)
    
  }
  
  HR_data <- do.call('rbind', HR_data_list)
  
  y_lim <- c(min(HR_data[,'ylo'], na.rm = TRUE), max(HR_data[,'yhi'], na.rm = TRUE))
  if (y_lim[1] > 0.9) {y_lim[1] <- 0.9} 
  
  diff_scale <- (y_lim[2] / y_lim[1])
  
  y_revised_lim <- y_lim
  y_revised_lim[1] <- y_revised_lim[1] * diff_scale^(-2)
  y_revised_lim[2] <- y_revised_lim[2] * diff_scale^1
  
  # Plotting
  
  forest_p <- ggplot(HR_data, aes(x = x, y = y, ymin = ylo, ymax = yhi))
  
  forest_p <- forest_p + annotate(geom = 'segment', x = 0.4, y = 1, xend = max(group_position) + 1.5, yend = 1, colour = "black", size = 0.4, linetype = 'dotted')
  forest_p <- forest_p + annotate(geom = 'line', x = c(0.5, 0.5), y = y_lim, colour = "black", size = 0.4)
  
  scale_y <- c(0.3, 0.5, 0.7, 1, 1.5, 2, 4, 8, 16)
  scale_y <- scale_y[scale_y > y_lim[1] & scale_y < y_lim[2]]
  
  for (j in 1:length(scale_y)) {
    
    forest_p <- forest_p + annotate(geom = 'segment', x = 0.3, y = scale_y[j], xend = 0.7, yend = scale_y[j], colour = "black", size = 0.4)
    forest_p <- forest_p + annotate(geom = "text", x = -0.3, y = scale_y[j], label = formatC(scale_y[j], 1, format = 'f'), size = 2.5, color = "black", angle = 45)
    
  }
  
  forest_p <- forest_p + annotate(geom = "text", x = -1.2, y = exp(mean(log(y_lim))), label = 'Sex, age-adj HR', size = 5, color = "black", fontface = "bold")
  
  forest_p <- forest_p + geom_pointrange(shape = 15, size = 0.4, fill = 'white', position = position_dodge(width = 0.1), fatten = 5)
  forest_p <- forest_p + geom_errorbar(size = 0.4, width = 0.4, cex = 1)
  forest_p <- forest_p + coord_flip() + xlab('') + ylab('')
  forest_p <- forest_p + scale_y_continuous(limits = y_revised_lim, trans = 'log2', breaks = NULL)
  # forest_p <- forest_p + theme(plot.margin = unit(c(-0.5, 0.5, -1.0, -1.0), "cm"))
  forest_p <- forest_p + theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.background = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.line.x = element_blank(),
                               axis.line.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.ticks.y = element_blank(),
                               legend.position = "none")
  
  forest_p <- forest_p + annotate(geom = "text", x = group_position, y = y_lim[1] * diff_scale^(-2), label = group_names, size = 4, color = "black", fontface = "bold", hjust = 0)
  forest_p <- forest_p + annotate(geom = "text", x = HR_data[,'x'], y = y_lim[1] * diff_scale^(-1.8), label = HR_data[,'x_name'], size = 4, hjust = 0)
  forest_p <- forest_p + annotate(geom = "text", x = HR_data[,'x'], y = y_lim[1] * diff_scale^(-0.4), label = HR_data[,'n'], size = 3.5)
  forest_p <- forest_p + annotate(geom = "text", x = HR_data[,'x'], y = y_lim[2] * diff_scale^(0.7), label = HR_data[,'txt.HR'], size = 3.5)
  
  forest_p <- forest_p + annotate(geom = "text", x = max(group_position) + 1, y = y_lim[1] * diff_scale^(-0.4), label = "event/n", size = 5, color = "black", fontface = "bold")
  forest_p <- forest_p + annotate(geom = "text", x = max(group_position) + 1, y = y_lim[2] * diff_scale^(0.7), label = "HR (95% CI)", size = 5, color = "black", fontface = "bold")
  
  forest_p <- forest_p + annotate(geom = "text", x = max(group_position) + 2.5, y = exp(mean(log(y_lim))), label = paste0(names(outcome_names)[q], ' [AI-ECG(+) vs. AI-ECG(-)]'), size = 5.5, color = "black", fontface = "bold")
  
  forest_list[[q]] <- forest_p
  
}

# 5. Merge plotting

km_plot.1 <- arrangeGrob(km_p_list[[1]], km_p_list[[2]],
                         ncol = 1)

km_plot.2 <- arrangeGrob(km_p_list[[3]], km_p_list[[4]],
                         ncol = 1)

forest_plot <- arrangeGrob(forest_list[[1]], forest_list[[2]],
                           ncol = 1)

final_p <- ggdraw()
final_p <- final_p + draw_plot(km_plot.1, x = 0, y = 0, width = 0.25, height = 0.95)
final_p <- final_p + draw_plot(km_plot.2, x = 0.25, y = 0, width = 0.25, height = 0.95)
final_p <- final_p + draw_plot(forest_plot, x = 0.45, y = 0, width = 0.55, height = 1)
final_p <- final_p + draw_plot_label(c("Internal test set", "Community test set"), c(0.005, 0.255), c(0.97, 0.97), size = 15, hjust = 0)
final_p <- final_p + draw_plot_label(c("A", "B"), c(0.005, 0.505), c(1, 1), size = 18, hjust = 0)

pdf(plot_path, width = 13, height = 9)

print(final_p)

dev.off()


