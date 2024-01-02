
library(magrittr)
library(glmnet)
library(ggplot2)
library(xgboost)
library(scales) 
library(cowplot)
library(gridExtra)
library(pROC)

# 0. Settings

show_PRAUC <- FALSE
postive_cut <- 0.349791174897487

data_path <- './data/raw_data.RData'
plot_path <- './results/Figure 04.pdf'

DATASET_NAME <- c('internal-test' = 'Internal test set', 'community-test' = 'Community test set', 'external-test' = 'Isolated test set')

MAIN_ECG_NAME_list <- list()
MAIN_ECG_NAME_list[['TF1']] <- c(expression(paste('HT vs. non-HT')))
MAIN_ECG_NAME_list[['TF2']] <- c(expression(paste('Overt HT vs. non-HT')))
MAIN_ECG_NAME_list[['TF3']] <- c(expression(paste('Subclinical HT vs. non-HT')))

intrested_var <- c('V0', 'POS_G', 'SEX_G', 'AGE_G', 'SEXAGE_G', 'V3', 'V6', 'V7')

col_list.1 <- c('#CC79A7FF', '#CC79A7DF', '#CC79A7BF', '#CC79A79F')
col_list.2 <- c('#D55B00', '#56B4E9')
col_list.3 <- c('#404040')
col_list.4 <- c('#E69F00', '#F0E442', '#0072B2')

# 1. Load data

load(data_path)

accuracy_data[,'TF1'] <- (accuracy_data[,'HT_GROUP'] %in% 'HT') + 0L

accuracy_data[,'TF2'] <- (accuracy_data[,'HT_SEVERITY'] %in% 'overt-HT') + 0L
accuracy_data[!accuracy_data[,'HT_SEVERITY'] %in% c('overt-HT', 'non-HT'), 'TF2'] <- NA

accuracy_data[,'TF3'] <- (accuracy_data[,'HT_SEVERITY'] %in% 'subclinical-HT') + 0L
accuracy_data[!accuracy_data[,'HT_SEVERITY'] %in% c('subclinical-HT', 'non-HT'), 'TF3'] <- NA

# 2. Process

accuracy_data[accuracy_data[,'ECG[POSITION]'] %in% 'Unknown','ECG[POSITION]'] <- 'OPD'
accuracy_data[accuracy_data[,'ECG[POSITION]'] %in% c('OPD', 'PEC'),'ECG[POSITION]'] <- 'OPD'
accuracy_data[accuracy_data[,'ECG[POSITION]'] %in% 'ER','ECG[POSITION]'] <- 'ED'

accuracy_data[,'POS_G'] <- factor(accuracy_data[,'ECG[POSITION]'])

accuracy_data[,'SEX_G'] <- accuracy_data[,'GENDER'] %>% factor(levels = c('female', 'male'))
# accuracy_data[,'AGE_G'] <- cut(accuracy_data[,'AGE'], breaks = c(-Inf, 44.9999, 59.9999, 74.9999, Inf), labels = c('<45 y/o', '45-59 y/o', '60-74 y/o', '>74 y/o'))
accuracy_data[,'AGE_G'] <- cut(accuracy_data[,'AGE'], breaks = c(-Inf, 49.9999, 59.9999, 69.9999, Inf), labels = c('<50 y/o', '50-59 y/o', '60-69 y/o', '>69 y/o'))

accuracy_data[,'SEXAGE_G'] <- as.integer(accuracy_data[,'AGE_G']) * 2 + as.integer(accuracy_data[,'SEX_G']) - 2
accuracy_data[,'SEXAGE_G'] <- factor(accuracy_data[,'SEXAGE_G'])
levels(accuracy_data[,'SEXAGE_G']) <- c('female <50 y/o', 'male <50 y/o', 'female 50-59 y/o', 'male 50-59 y/o', 'female 60-69 y/o', 'male 60-69 y/o', 'female >69 y/o', 'male >69 y/o')

accuracy_data[,'V0'] <- factor('all samples', levels = 'all samples')

accuracy_data[,'V1'] <- factor('without HT', levels = 'without HT')
accuracy_data[accuracy_data[,'HISTORY[HT]'] %in% 1,'V1'] <- NA

accuracy_data[,'V2'] <- factor('without ATD', levels = 'without ATD')
accuracy_data[accuracy_data[,'HISTORY[ATA]'] %in% 1,'V2'] <- NA

accuracy_data[,'V3'] <- factor('(1) without HT/ATD', levels = '(1) without HT/ATD')
accuracy_data[accuracy_data[,'HISTORY[HT]'] %in% 1 | accuracy_data[,'HISTORY[ATA]'] %in% 1,'V3'] <- NA

accuracy_data[,'V4'] <- factor('within 7 days', levels = 'within 7 days')
accuracy_data[abs(accuracy_data[,'TSH[NEAREST_DAY]']) > 7,'V4'] <- NA

accuracy_data[,'V5'] <- factor('within 3 days', levels = 'within 3 days')
accuracy_data[abs(accuracy_data[,'TSH[NEAREST_DAY]']) > 3,'V5'] <- NA

accuracy_data[,'V6'] <- factor('(2) within 24 hours', levels = '(2) within 24 hours')
accuracy_data[abs(accuracy_data[,'TSH[NEAREST_DAY]']) > 1,'V6'] <- NA

accuracy_data[,'V7'] <- factor('<60 y/o & (1) & (2)', levels = '<60 y/o & (1) & (2)')
accuracy_data[abs(accuracy_data[,'TSH[NEAREST_DAY]']) > 1 | accuracy_data[,'AGE'] >= 60 | accuracy_data[,'HISTORY[HT]'] %in% 1 | accuracy_data[,'HISTORY[ATA]'] %in% 1,'V7'] <- NA

# 3. Stratified

p_list <- list()

for (h in 1:length(DATASET_NAME)) {
  
  p_list[[names(DATASET_NAME)[h]]] <- list()
  
  x_name <- 'ECG_pred.prob'
  
  sub_data <- accuracy_data[accuracy_data[,'DATASET'] %in% names(DATASET_NAME)[h],]
  
  for (i in 1:length(MAIN_ECG_NAME_list)) {
    
    y_name <- names(MAIN_ECG_NAME_list)[i]
    
    # Plotting
    
    x_pos <- 1
    
    summary_list <- list()
    
    for (l in 1:length(intrested_var)) {
      
      lvl.var <- accuracy_data[,intrested_var[l]] %>% factor() %>% levels() 
      
      if (length(lvl.var) %in% 4) {
        col_list <- col_list.1
      } else if (length(lvl.var) %in% 1) {
        col_list <- col_list.3
      } else if (length(lvl.var) %in% 3) {
        col_list <- col_list.4
      } else {
        col_list <- paste0(rep(col_list.2, 4), rep(c('FF', 'DF', 'BF', '9F'), each = 2))
      }
      
      for (m in 1:length(lvl.var)) {
        
        sub_sub_test_data <- sub_data[sub_data[,intrested_var[l]] %in% lvl.var[m],]
        
        roc_curve.test <- try(roc(response = sub_sub_test_data[,y_name], predictor = sub_sub_test_data[,x_name]), silent = TRUE)
        
        if (!'try-error' %in% class(roc_curve.test)) {
          
          summary_list[[length(summary_list) + 1]] <- data.frame(var1 = factor(intrested_var[l], levels = intrested_var),
                                                                 var2 = lvl.var[m],
                                                                 col = col_list[m],
                                                                 x = x_pos,
                                                                 val = ci.auc(roc_curve.test)[2],
                                                                 ci.l = ci.auc(roc_curve.test)[1],
                                                                 ci.u = ci.auc(roc_curve.test)[3],
                                                                 stringsAsFactors = FALSE)
          
        }
        
        # else {
        #   
        #   summary_list[[length(summary_list) + 1]] <- data.frame(var1 = factor(intrested_var[l], levels = intrested_var),
        #                                                          var2 = lvl.var[m],
        #                                                          col = col_list[m],
        #                                                          x = x_pos,
        #                                                          val = NA,
        #                                                          ci.l = NA,
        #                                                          ci.u = NA,
        #                                                          stringsAsFactors = FALSE)
        #   
        # }
        
        x_pos <- x_pos + 1
        
      }
      
      x_pos <- x_pos + 0.5
      
    }
    
    my_summary <- do.call('rbind', summary_list)
    my_summary <- my_summary[order(my_summary[,'x']),]
    
    my_summary[,'x_name'] <- my_summary[,'var2']
    my_summary[,'txt'] <- paste0(formatC(my_summary[,'val'], 3, format = 'f'), ' (', formatC(my_summary[,'ci.l'], 3, format = 'f'), '-', formatC(my_summary[,'ci.u'], 3, format = 'f'), ')')
    
    # Bar plot
    
    main_txt <- MAIN_ECG_NAME_list[[i]]
    
    bar_p <- ggplot(my_summary, aes(x = x, y = val, fill = col))
    bar_p <- bar_p + geom_bar(position = "dodge", stat = "identity")
    bar_p <- bar_p + geom_errorbar(aes(ymin = ci.l, ymax = ci.u), width = .4, position = position_dodge(.9))
    bar_p <- bar_p + theme_classic()
    bar_p <- bar_p + labs(title = main_txt, x = '', y = 'AUC')
    bar_p <- bar_p + theme(plot.title = element_text(color = "#000000", size = 12),
                           legend.position = "none",
                           axis.text.x = element_text(color = "#000000", angle = 45, hjust = 1, size = 8)) + labs(fill = '')
    
    bar_p <- bar_p + scale_fill_manual(values = factor(my_summary[,"col"]) %>% levels())
    bar_p <- bar_p + scale_y_continuous(expand = c(0, 0), limits = c(0, 1.0), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    bar_p <- bar_p + scale_x_continuous(limits = c(0.5, max(my_summary[,'x']) + 0.5),
                                        breaks = my_summary[, 'x'],
                                        labels = my_summary[, 'x_name'])
    
    bar_p <- bar_p + annotate(geom = "text", x = my_summary[,'x'], y = 0.02, label = my_summary[,'txt'], size = 2.5, color = "white", angle = 90, fontface = 2, hjust = 0)
    
    p_list[[names(DATASET_NAME)[h]]][[y_name]] <- bar_p
    
  }
  
}


# 4. Plottings

bar_p.1 <- arrangeGrob(p_list[[1]][[1]],
                       p_list[[1]][[2]],
                       p_list[[1]][[3]],
                       ncol = 1)

bar_p.2 <- arrangeGrob(p_list[[2]][[1]],
                       p_list[[2]][[2]],
                       p_list[[2]][[3]],
                       ncol = 1)

final_p <- ggdraw()
final_p <- final_p + draw_plot(bar_p.1, x = 0, y = 0, width = 0.5, height = 0.975)
final_p <- final_p + draw_plot(bar_p.2, x = 0.5, y = 0, width = 0.5, height = 0.975)
final_p <- final_p + draw_plot_label(DATASET_NAME[1:2], c(0.005, 0.505), c(1, 1), size = 15, hjust = 0)

pdf(plot_path, width = 12, height = 10)

print(final_p)

dev.off()