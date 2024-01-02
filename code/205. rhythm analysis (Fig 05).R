
library(cowplot)
library(xgboost)
library(scales) 
library(glmnet)
library(pROC)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(survival)

# 0. Settings

postive_cut <- 0.349791174897487

outcome_names <- c('All-cause mortality' = 'all-cause mortality',
                   'New-onset HF' = 'HF')

intrested_var.1 <- c('ECG[Rate]', 'ECG[PR]', 'ECG[QRSd]')
intrested_var.2 <- c('ECG[SR]', 'ECG[LVOL]', 'ECG[LVH]', 'ECG[RAE]', 'ECG[RBBB]', 'ECG[AFIB]')

intrested_var <- c(intrested_var.1, intrested_var.2)

names(intrested_var) <- c('Heart rate', 'PR interval', 'QRS duration',
                          'Sinus rhythm', 'Low voltage', 'Left ventricular hypertrophy', 'Right atrial enlargement', 'Right bundle branch block', 'Atrial fibrillation')

col_list.AI = c('#0072B2', '#E69F00')
col_list.HT = c('#D55B00', '#CC79A7', '#009E73')

data_path <- './data/raw_data.RData'
plot_path <- './results/Figure 05.pdf'

# 1. load data

load(data_path)

test_data <- accuracy_data[accuracy_data[,'DATASET'] %in% c('internal-test', 'community-test', 'external-test'),]
test_data[,'AI'] <- factor((test_data[,'ECG_pred.prob'] >= postive_cut) + 0L, 1:0)

for (m in 1:length(intrested_var.2)) {test_data[,intrested_var.2[m]] <- factor(test_data[,intrested_var.2[m]], 0:1)}

test_data <- test_data[!test_data[,intrested_var.2[m]] %in% NA,]
levels(test_data[,'HT_SEVERITY']) <- c("overt-HT", "sub-HT", "non-HT")

# 2. Plotting

p_list <- list()

for (i in 1:length(intrested_var)) {
  
  # Get data
  
  my_dat <- data.frame(y = test_data[,intrested_var[i]],
                       x = test_data[,'AI'],
                       g = test_data[,'HT_SEVERITY'],
                       stringsAsFactors = FALSE)
  
  # Preprocess
  
  if (class(my_dat[,'y']) %in% c('factor')) {
    my_dat[,'y'] <- as.integer(factor(my_dat[,'y'])) - 1L
    my_dat[,'y'] <- my_dat[,'y']
    use_linear <- FALSE
  } else {
    use_linear <- TRUE
  }
  
  # Summary
  
  dat_list <- list()
  
  m <- tapply(my_dat[,'y'], my_dat[,c('x', 'g')], mean, na.rm = TRUE) %>% as.numeric()
  s <- tapply(my_dat[,'y'], my_dat[,c('x', 'g')], sd, na.rm = TRUE) %>% as.numeric()
  n <- tapply(!is.na(my_dat[,'y']), my_dat[,c('x', 'g')], sum) %>% as.numeric()
  
  p_val_1 <- NULL
  
  for (gg in levels(my_dat[,'g'])) {
    
    p_val_1 <- c(p_val_1, t.test(y ~ x, data = my_dat)[['p.value']])
    
  }
  
  p_val_1 <- p_val_1 %>% formatC(., digits = 3, format = 'f') %>% paste0('p = ', .)
  p_val_1[p_val_1 %in% 'p = 0.000'] <- 'p < 0.001'
    
  q3 <- tapply(my_dat[,'y'], my_dat[,c('x', 'g')], quantile, probs = 0.75, na.rm = TRUE) %>% as.numeric()
  q1 <- tapply(my_dat[,'y'], my_dat[,c('x', 'g')], quantile, probs = 0.25, na.rm = TRUE) %>% as.numeric()
  
  dat_list[[1]] <- data.frame(var1 = factor(rep(levels(test_data[,'HT_SEVERITY']), each = 2), levels = levels(test_data[,'HT_SEVERITY'])),
                              var2 = factor(rep(c('AI-ECG (+)', 'AI-ECG (-)'), length(levels(test_data[,'HT_SEVERITY']))), levels = c('AI-ECG (+)', 'AI-ECG (-)')),
                              val = m,
                              se = s,
                              q3 = q3,
                              q1 = q1,
                              iqr = q3 - q1,
                              x = c(1, 2, 3, 4, 5, 6) + 4,
                              col = col_list.AI,
                              stringsAsFactors = FALSE)
  
  m <- tapply(my_dat[,'y'], my_dat[,c('g')], mean, na.rm = TRUE) %>% as.numeric()
  s <- tapply(my_dat[,'y'], my_dat[,c('g')], sd, na.rm = TRUE) %>% as.numeric()
  n <- tapply(!is.na(my_dat[,'y']), my_dat[,c('g')], sum) %>% as.numeric()
  
  p_val <- anova(aov(y ~ g, data = my_dat))[['Pr(>F)']][1]
  p_val <- p_val %>% formatC(., digits = 3, format = 'f') %>% paste0('p = ', .)
  p_val[p_val %in% 'p = 0.000'] <- 'p < 0.001'
  
  q3 <- tapply(my_dat[,'y'], my_dat[,c('g')], quantile, probs = 0.75, na.rm = TRUE) %>% as.numeric()
  q1 <- tapply(my_dat[,'y'], my_dat[,c('g')], quantile, probs = 0.25, na.rm = TRUE) %>% as.numeric()
  
  dat_list[[2]] <- data.frame(var1 = '',
                              var2 = factor(levels(test_data[,'HT_SEVERITY']), levels = levels(test_data[,'HT_SEVERITY'])),
                              val = m,
                              se = s,
                              q3 = q3,
                              q1 = q1,
                              iqr = q3 - q1,
                              x = c(1, 2, 3) + 0,
                              col = col_list.HT,
                              stringsAsFactors = FALSE)
  
  my_summary <- do.call('rbind', dat_list)
  
  my_summary <- my_summary[order(my_summary[,'x']),]
  
  my_summary[,'ci.l'] <- my_summary[,'val'] - my_summary[,'se']
  my_summary[,'ci.u'] <- my_summary[,'val'] + my_summary[,'se']
  
  my_summary[my_summary[,'ci.l'] < 0,'ci.l'] <- 0

  if (use_linear %in% FALSE) {
    
    my_summary[,'val'] <- my_summary[,'val'] * 100
    my_summary[,'ci.l'] <- my_summary[,'val']
    my_summary[,'ci.u'] <- my_summary[,'val']
    
  } else {
    
    my_summary[,'ci.l'] <- my_summary[,'q1'] - my_summary[,'iqr'] * 1.5
    my_summary[,'ci.u'] <- my_summary[,'q3'] + my_summary[,'iqr'] * 1.5
    
  }
  
  my_summary[,'x_name'] <- my_summary[,'var2']
    
  my_summary[,'txt'] <- paste0(formatC(my_summary[,'val'], 3, format = 'f'), ' (', formatC(my_summary[,'ci.l'], 3, format = 'f'), '-', formatC(my_summary[,'ci.u'], 3, format = 'f'), ')')
  
  # Parameters
  
  # diff <- max(my_summary[,'ci.u'], na.rm = TRUE)
  # y_lim <- c(0,  max(my_summary[,'ci.u'], na.rm = TRUE) + diff * 0.2)
  # y_lim[1] <- max(y_lim[1], 0, na.rm = TRUE)
  # diff <- (y_lim[2] - y_lim[1]) / 1.3
  
  diff <- max(my_summary[,'ci.u'], na.rm = TRUE) - min(my_summary[,'ci.l'], na.rm = TRUE)
  y_lim <- c(min(my_summary[,'ci.l'], na.rm = TRUE) - diff * 0.1,  max(my_summary[,'ci.u'], na.rm = TRUE) + diff * 0.2)
  y_lim[1] <- max(y_lim[1], 0, na.rm = TRUE)
  diff <- (y_lim[2] - y_lim[1]) / 1.4
  
  # Plotting
  
  title_txt <- names(intrested_var)[i]
  if (mean(my_dat[,'y'] %in% c(0, 1, 100, NA)) %in% 1) {title_txt <- paste0(title_txt, ' (%)')}
  
  if (use_linear %in% FALSE) {
    
    bar_p <- ggplot(my_summary, aes(x = x, y = val, fill = col))
    bar_p <- bar_p + geom_bar(position = "dodge", stat = "identity")
    # bar_p <- bar_p + geom_errorbar(aes(ymin = ci.l, ymax = ci.u), width = .4, position = position_dodge(.9))
    bar_p <- bar_p + theme_classic()
    bar_p <- bar_p + labs(title = title_txt, x = '', y = '')
    bar_p <- bar_p + theme(plot.title = element_text(color = "#000000", size = 13),
                           legend.position = "none",
                           axis.text.x = element_text(color = "#000000", angle = 45, hjust = 1, size = 12)) + labs(fill = '')
    
    bar_p <- bar_p + scale_fill_manual(values = factor(my_summary[,"col"]) %>% levels())
    
    bar_p <- bar_p + scale_x_continuous(limits = c(0.5, max(my_summary[,'x']) + 0.5),
                                        breaks = my_summary[, 'x'],
                                        labels = my_summary[, 'x_name'])
    
    bar_p <- bar_p + coord_cartesian(ylim = y_lim)
    
    sub_summary <- my_summary[my_summary[,'var1'] %in% '',]
    y_pos <- rep(max(sub_summary[,'ci.u']) + diff * 0.05, 2)
    
    bar_p <- bar_p + annotate("line", x = c(0.6, 3.4), y = y_pos, alpha = 1, colour = "black")
    bar_p <- bar_p + annotate("text", x = 2, y = mean(y_pos) + diff * 0.025, label = paste0(p_val), colour = "black", size = 2.5, vjust = 0)
    
    for (m in 1:3) {
      
      sub_summary <- my_summary[my_summary[,'var1'] %in% levels(test_data[,'HT_SEVERITY'])[m],]
      
      x_pos <- c(-0.4, 0.4) + sub_summary[,'x']
      y_pos <- rep(max(sub_summary[,'ci.u']) + diff * 0.05, 2)
      
      bar_p <- bar_p + annotate("line", x = x_pos, y = y_pos, alpha = 1, colour = "black")
      bar_p <- bar_p + annotate("text", x = mean(x_pos), y = mean(y_pos) + diff * 0.025, label = paste0(levels(test_data[,'HT_SEVERITY'])[m], '\n', p_val_1[m]), colour = "black", size = 2.5, vjust = 0)
      
    }
    
  } else {
    
    new_data.1 <- my_dat[,c('y', 'g')]
    lvl_g <- levels(new_data.1[,'g'])
    lvl_g2 <- levels(my_dat[,'x'])
    new_data.1[,'g'] <- as.integer(new_data.1[,'g'])
    new_data.1[,'set'] <- new_data.1[,'g']
      
    new_data.2 <- my_dat[,c('y'),drop = FALSE]
    new_data.2[,'g'] <- as.integer(my_dat[,'x']) + 2 + as.integer(my_dat[,'g']) * 2
    new_data.2[,'set'] <- as.integer(my_dat[,'x']) + 3

    new_data <- rbind(new_data.1, new_data.2)
    new_data[,'set'] <- factor(new_data[,'set'])
    new_data[new_data[,'g'] %in% NA,'g'] <- 4
    new_data[,'g'] <- factor(new_data[,'g'])
    
    bar_p <- ggplot(new_data, aes(x = g, y = y, colour = set))
    bar_p <- bar_p + theme_classic()
    bar_p <- bar_p + geom_boxplot(aes(group = g, colour = set), width = 0.4, outlier.shape = NA, outlier.size = 0, width.errorbar = 0.3)
    bar_p <- bar_p + scale_color_manual(values = c(col_list.HT, col_list.AI),
                                              labels = c(lvl_g, '', rep(c('AI-ECG (+)', 'AI-ECG (-)'), 3)))    
    
    bar_p <- bar_p + scale_fill_manual(labels = c(lvl_g, '', rep(c('AI-ECG (+)', 'AI-ECG (-)'), 3)))
    bar_p <- bar_p + scale_x_discrete(limits = 1:10, labels = c(lvl_g, '', rep(c('AI-ECG (+)', 'AI-ECG (-)'), 3)))

    
    
    bar_p <- bar_p + labs(title = title_txt, x = '', y = '')
    bar_p <- bar_p + theme(plot.title = element_text(color = "#000000", size = 13),
                           legend.position = "none",
                           axis.text.x = element_text(color = "#000000", angle = 45, hjust = 1, size = 12)) + labs(fill = '')
    
    bar_p <- bar_p + coord_cartesian(ylim = y_lim)
    
    sub_summary <- my_summary[my_summary[,'var1'] %in% '',]
    y_pos <- rep(max(sub_summary[,'ci.u']) + diff * 0.05, 2)
    
    bar_p <- bar_p + annotate("line", x = c(0.6, 3.4), y = y_pos, alpha = 1, colour = "black")
    bar_p <- bar_p + annotate("text", x = 2, y = mean(y_pos) + diff * 0.025, label = paste0(p_val), colour = "black", size = 2.5, vjust = 0)
    
    for (m in 1:3) {
      
      sub_summary <- my_summary[my_summary[,'var1'] %in% levels(test_data[,'HT_SEVERITY'])[m],]
      
      x_pos <- c(-0.4, 0.4) + sub_summary[,'x']
      y_pos <- rep(max(sub_summary[,'ci.u']) + diff * 0.05, 2)
      
      bar_p <- bar_p + annotate("line", x = x_pos, y = y_pos, alpha = 1, colour = "black")
      bar_p <- bar_p + annotate("text", x = mean(x_pos), y = mean(y_pos) + diff * 0.025, label = paste0(levels(test_data[,'HT_SEVERITY'])[m], '\n', p_val_1[m]), colour = "black", size = 2.5, vjust = 0)
      
    }
    
  }
  
  p_list[[i]] <- bar_p
    
}

# 3. Forest plot

forest_list <- list()

for (q in 1:length(outcome_names)) {
  
  # Get outcome data
  
  current_data <- test_data
  current_data[,'status'] <- current_data[,paste0('event[', outcome_names[q], ']')]
  current_data[,'time'] <- current_data[,paste0('time[', outcome_names[q], ']')]
  current_data <- current_data[!is.na(current_data[,'status']) & !is.na(current_data[,'time']),]
  current_data <- current_data[current_data[,'time'] >= 0,]
  current_data[,'time'] <- (current_data[,'time'] + 0.5) / 365.25
  
  # Get data list
  
  data_list <- list()
  
  for (i in 1:length(intrested_var)) {
    
    current_data[,'x'] <- current_data[,intrested_var[i]]
    if (class(current_data[,'x']) %in% c('factor')) {
      current_data[,'x'] <- as.integer(current_data[,'x']) - 1L
    } else {
      print(sd(current_data[,'x'], na.rm = TRUE))
      current_data[,'x'] <- current_data[,'x'] / sd(current_data[,'x'], na.rm = TRUE)
    }
    
    data_list[[i]] <- current_data[,c('status', 'time', 'x', 'AGE', 'GENDER')]
    
  }
    
  # Figure parameters
  
  data_names <- names(intrested_var)
  data_names[1:length(intrested_var.1)] <- paste0(data_names[1:length(intrested_var.1)], ' (per 1 SD)')
  
  data_position <- length(intrested_var):1
  
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
    
    if (exp(y.HR + qnorm(0.975) * se.HR) < 1) {col = '#56B4E9'} else if (exp(y.HR - qnorm(0.975) * se.HR) > 1) {col = '#D55E00'} else {col = '#000000'}
    
    HR_data_list[[j]] <- data.frame(x = data_position[j],
                                    x_name = data_names[j],
                                    # n = paste0(sum(sub_data[,'status']), '/', nrow(sub_data),
                                    #            ' (', formatC(sum(sub_data[,'status']) / nrow(sub_data) * 100, 1, format = 'f'), '%)'),
                                    n = paste0(sum(sub_data[,'status']), '/', nrow(sub_data)),
                                    y = exp(y.HR), yse = se.HR,
                                    ylo = exp(y.HR - qnorm(0.975) * se.HR),
                                    yhi = exp(y.HR + qnorm(0.975) * se.HR),
                                    txt.HR = HR_txt,
                                    color = col,
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
  
  forest_p <- forest_p + annotate(geom = 'segment', x = 0.4, y = 1, xend = max(data_position) + 1.5, yend = 1, colour = "black", size = 0.4, linetype = 'dotted')
  forest_p <- forest_p + annotate(geom = 'line', x = c(0.5, 0.5), y = y_lim, colour = "black", size = 0.4)
  
  scale_y <- c(0.3, 0.5, 0.7, 1, 1.5, 2, 4, 8, 16)
  scale_y <- scale_y[scale_y > y_lim[1] & scale_y < y_lim[2]]
  
  for (j in 1:length(scale_y)) {
    
    forest_p <- forest_p + annotate(geom = 'segment', x = 0.3, y = scale_y[j], xend = 0.7, yend = scale_y[j], colour = "black", size = 0.4)
    forest_p <- forest_p + annotate(geom = "text", x = -0.3, y = scale_y[j], label = formatC(scale_y[j], 1, format = 'f'), size = 2.5, color = "black", angle = 45)
    
  }
  
  forest_p <- forest_p + annotate(geom = "text", x = -1.2, y = exp(mean(log(y_lim))), label = 'Sex, age-adj HR', size = 5, color = "black", fontface = "bold")
  
  forest_p <- forest_p + geom_pointrange(shape = 15, size = 0.4, fill = 'white', colour = HR_data[nrow(HR_data):1,'color'], position = position_dodge(width = 0.1), fatten = 5)
  forest_p <- forest_p + geom_errorbar(size = 0.4, width = 0.4, cex = 1, colour = HR_data[,'color'])
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
  
  forest_p <- forest_p + annotate(geom = "text", x = HR_data[,'x'], y = y_lim[1] * diff_scale^(-2), label = HR_data[,'x_name'], size = 4, hjust = 0)
  forest_p <- forest_p + annotate(geom = "text", x = HR_data[,'x'], y = y_lim[2] * diff_scale^(0.7), label = HR_data[,'txt.HR'], colour = HR_data[,'color'], size = 3.5)
  
  forest_p <- forest_p + annotate(geom = "text", x = max(data_position) + 1, y = y_lim[1] * diff_scale^(-2), label = "ECG feature", size = 4.5, color = "black", fontface = "bold", hjust = 0)
  forest_p <- forest_p + annotate(geom = "text", x = max(data_position) + 1, y = y_lim[2] * diff_scale^(0.7), label = "HR (95% CI)", size = 4.5, color = "black", fontface = "bold")
  
  forest_p <- forest_p + annotate(geom = "text", x = max(data_position) + 2.5, y = exp(mean(log(y_lim))), label = names(outcome_names)[q], size = 6, color = "black", fontface = "bold")
  
  forest_list[[q]] <- forest_p
  
}

# 4. Shared legend

bar_p <- arrangeGrob(p_list[[1]], p_list[[2]], p_list[[3]],
                     p_list[[4]], p_list[[5]], p_list[[6]],
                     p_list[[7]], p_list[[8]], p_list[[9]],
                     ncol = 3)

forest_plot <- arrangeGrob(forest_list[[1]], forest_list[[2]],
                           ncol = 1)

final_p <- ggdraw()
final_p <- final_p + draw_plot(bar_p, x = 0, y = 0, width = 0.6, height = 0.98)
final_p <- final_p + draw_plot(forest_plot, x = 0.57, y = 0, width = 0.43, height = 0.98)
final_p <- final_p + draw_plot_label(LETTERS[1:2], c(0.003, 0.603), c(0.998, 0.998), size = 20, hjust = 0)

pdf(plot_path, width = 16, height = 10)

print(final_p)

dev.off()
