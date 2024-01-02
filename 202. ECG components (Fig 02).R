
library(cowplot)
library(survival)
library(scales) 
library(glmnet)
library(pROC)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(xgboost)

# 0. Data path

data_path <- './data/raw_data.RData'
plot_path <- './results/Figure 02.pdf'

# 0. Settings

DATASET_NAME_LIST <- c('internal-test' = 'Internal test set', 'community-test' = 'Community test set', 'external-test' = 'Isolated test set')

intrested_var_list <- list()

intrested_var_list[[1]] <- list(var = c('POS_G', 'HISTORY[HT]', 'HISTORY[ATA]',
                                        'GENDER', 'AGE', 'BMI',
                                        'DM', 'HTN', 'HLP', 'CKD', 'AMI', 'STK', 'CAD', 'HF', 'Afib', 'COPD'),
                                name = c('Data source', 'History of hyperthyroidism', 'History of anti-thyroid drug', 
                                         'Sex', 'Age', 'Body mass index',
                                         'History of diabetes mellitus', 'History of hypertension', 'History of hyperlipidemia', 'History of chronic kidney disease', 'History of acute myocardial infraction',
                                         'History of stroke', 'History of coronary artery disease', 'History of heart failure', 'History of atrial fibrillation', 'History of chronic obstructive pulmonary disease'))

intrested_var_list[[2]] <- list(var = c('ECG[Rate]', 'ECG[PR]', 'ECG[QRSd]', 'ECG[QT]', 'ECG[QTc]', 'ECG[Axes_P]', 'ECG[Axes_QRS]', 'ECG[Axes_T]',
                                        'ECG[SR]', 'ECG[SA]', 'ECG[SP]', 'ECG[EAR]', 'ECG[JER]', 'ECG[PMR]', 'ECG[ET]', 'ECG[STE]', 'ECG[STD]', 'ECG[ATW]', 'ECG[AQW]', 'ECG[RSRW]', 'ECG[LVOL]', 'ECG[LAD]', 'ECG[RAD]', 'ECG[LVH]', 'ECG[RVH]', 'ECG[LAE]', 'ECG[RAE]', 'ECG[NIVCD]', 'ECG[LFB]', 'ECG[RBBB]', 'ECG[LBBB]', 'ECG[1AVB]', 'ECG[2AVB]', 'ECG[CAVB]', 'ECG[AFIB]', 'ECG[AFLT]', 'ECG[SVT]', 'ECG[VPE]', 'ECG[RVPC]', 'ECG[APC]', 'ECG[VPC]'),
                                name = c('Heart rate', 'PR interval', 'QRS duration', 'QT interval', 'QTc interval', 'P waves axes', 'QRS wave axes', 'T wave axes',
                                         'Sinus rhythm', 'Sinus arrhythmia', 'Sinus pause', 'Ectopic atrial rhythm', 'Junctional rhythm', 'Pacemaker rhythm', 'Early precordial R/S transition', 'ST elevation', 'ST depression', 'Abnormal T wave', 'Abnormal Q wave', "RSR' wave", 'Low voltage', 'Left axis deviation', 'Right axis deviation', 'Left ventricular hypertrophy', 'Right ventricular hypertrophy', 'Left atrial enlargement', 'Right atrial enlargement', 'Nonspecific IVCD', 'Left fascicular block', 'Right bundle branch block', 'Left bundle branch block', 'First degree AV block', 'Second degree AV block', 'Complete degree AV block', 'Atrial fibrillation', 'Atrial flutter', 'Supraventricular tachycardia', 'WPW syndrome', 'Ventricular tachycardia', 'Atrial premature complex', 'Ventricular premature complex'))

intrested_var_list[[3]] <- list(var = c('POS_G', 'HISTORY[HT]', 'HISTORY[ATA]',
                                        'GENDER', 'AGE', 'BMI',
                                        'DM', 'HTN', 'HLP', 'CKD', 'AMI', 'STK', 'CAD', 'HF', 'Afib', 'COPD',
                                        'ECG[Rate]', 'ECG[PR]', 'ECG[QRSd]', 'ECG[QT]', 'ECG[QTc]', 'ECG[Axes_P]', 'ECG[Axes_QRS]', 'ECG[Axes_T]',
                                        'ECG[SR]', 'ECG[SA]', 'ECG[SP]', 'ECG[EAR]', 'ECG[JER]', 'ECG[PMR]', 'ECG[ET]', 'ECG[STE]', 'ECG[STD]', 'ECG[ATW]', 'ECG[AQW]', 'ECG[RSRW]', 'ECG[LVOL]', 'ECG[LAD]', 'ECG[RAD]', 'ECG[LVH]', 'ECG[RVH]', 'ECG[LAE]', 'ECG[RAE]', 'ECG[NIVCD]', 'ECG[LFB]', 'ECG[RBBB]', 'ECG[LBBB]', 'ECG[1AVB]', 'ECG[2AVB]', 'ECG[CAVB]', 'ECG[AFIB]', 'ECG[AFLT]', 'ECG[SVT]', 'ECG[VPE]', 'ECG[RVPC]', 'ECG[APC]', 'ECG[VPC]'),
                                name = c('Data source', 'History of hyperthyroidism', 'History of anti-thyroid drug', 
                                         'Sex', 'Age', 'Body mass index',
                                         'History of diabetes mellitus', 'History of hypertension', 'History of hyperlipidemia', 'History of chronic kidney disease', 'History of acute myocardial infraction',
                                         'History of stroke', 'History of coronary artery disease', 'History of heart failure', 'History of atrial fibrillation', 'History of chronic obstructive pulmonary disease',
                                         'Heart rate', 'PR interval', 'QRS duration', 'QT interval', 'QTc interval', 'P waves axes', 'QRS wave axes', 'T wave axes',
                                         'Sinus rhythm', 'Sinus arrhythmia', 'Sinus pause', 'Ectopic atrial rhythm', 'Junctional rhythm', 'Pacemaker rhythm', 'Early precordial R/S transition', 'ST elevation', 'ST depression', 'Abnormal T wave', 'Abnormal Q wave', "RSR' wave", 'Low voltage', 'Left axis deviation', 'Right axis deviation', 'Left ventricular hypertrophy', 'Right ventricular hypertrophy', 'Left atrial enlargement', 'Right atrial enlargement', 'Nonspecific IVCD', 'Left fascicular block', 'Right bundle branch block', 'Left bundle branch block', 'First degree AV block', 'Second degree AV block', 'Complete degree AV block', 'Atrial fibrillation', 'Atrial flutter', 'Supraventricular tachycardia', 'WPW syndrome', 'Ventricular tachycardia', 'Atrial premature complex', 'Ventricular premature complex'))

intrested_var_list[[4]] <- list(var = c('POS_G', 'HISTORY[HT]', 'HISTORY[ATA]',
                                        'GENDER', 'AGE', 'BMI',
                                        'DM', 'HTN', 'HLP', 'CKD', 'AMI', 'STK', 'CAD', 'HF', 'Afib', 'COPD',
                                        'ECG_pred.prob'),
                                name = c('Data source', 'History of hyperthyroidism', 'History of anti-thyroid drug', 
                                         'Sex', 'Age', 'Body mass index',
                                         'History of diabetes mellitus', 'History of hypertension', 'History of hyperlipidemia', 'History of chronic kidney disease', 'History of acute myocardial infraction',
                                         'History of stroke', 'History of coronary artery disease', 'History of heart failure', 'History of atrial fibrillation', 'History of chronic obstructive pulmonary disease',
                                         'DLM prediction'))

names(intrested_var_list) <- c('Patient characteristics', 'ECG features', 'Combination', 'DLM + patient data')

# 1. Load data

load(data_path)

accuracy_data[accuracy_data[,'ECG[POSITION]'] %in% 'Unknown','ECG[POSITION]'] <- 'OPD'
accuracy_data[accuracy_data[,'ECG[POSITION]'] %in% c('OPD', 'PEC'),'ECG[POSITION]'] <- 'OPD'

accuracy_data[,'POS_G'] <- factor(accuracy_data[,'ECG[POSITION]'])
accuracy_data[,'HISTORY[HT]'] <- factor(accuracy_data[,'HISTORY[HT]'], 0:1)
accuracy_data[,'HISTORY[ATA]'] <- factor(accuracy_data[,'HISTORY[ATA]'], 0:1)

final_data <- accuracy_data
final_data[,'y'] <- as.integer(final_data[,'HT_GROUP'] %in% 'HT') + 0L

# 2. AI-ECG

XGB_list <- list()

for (j in 1:length(intrested_var_list)) {
  
  intrested_var <- intrested_var_list[[j]][['var']]
  intrested_var_name <- intrested_var_list[[j]][['name']]
  names(intrested_var_name) <- intrested_var
  
  train_data <- final_data[final_data[,'DATASET'] %in% 'train', intrested_var] %>% model.matrix(~ ., data = .)
  train_label <- final_data[final_data[,'DATASET'] %in% 'train', 'y']
  
  test_data <- final_data[final_data[,'DATASET'] %in% 'valid', intrested_var] %>% model.matrix(~ ., data = .)
  test_label <- final_data[final_data[,'DATASET'] %in% 'valid', 'y']
  
  all_data <- final_data[,intrested_var] %>% model.matrix(~ ., data = .)
  all_label <- final_data[,'y']
  
  dfold.1 <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
  dfold.2 <- xgb.DMatrix(data = as.matrix(test_data), label = test_label)
  dfold.3 <- xgb.DMatrix(data = as.matrix(all_data), label = all_label)
  
  # Fold-1
  
  XGB_model.1 <- xgb.train(data = dfold.1,
                           watchlist = list(train = dfold.1, test = dfold.2),
                           eta = 0.05,  nrounds = 300, max.depth = 5, early_stopping_rounds = 10,
                           nthread = 5, verbose = FALSE, eval_metric = "auc", objective = "binary:logistic")
  
  final_data[,paste0('combine_', j)] <- predict(XGB_model.1, dfold.3)

  XGB_importance <- xgb.importance(model = XGB_model.1)
  XGB_data.1 <- data.frame(name = XGB_importance[['Feature']], importance = XGB_importance[['Gain']], sign = sign(XGB_importance[['Gain']]), stringsAsFactors = FALSE)
  XGB_data.1[,'importance'] <- XGB_data.1[,'importance'] / max(XGB_data.1[,'importance'])
  
  # # Fold-2
  # 
  # XGB_model.2 <- xgb.train(data = dfold.2,
  #                          watchlist = list(train = dfold.2, test = dfold.1),
  #                          eta = 0.05,  nrounds = 300, max.depth = 5, early_stopping_rounds = 10,
  #                          nthread = 5, verbose = FALSE, eval_metric = "auc", objective = "binary:logistic")
  # 
  # final_data[final_data[,'DATASET'] %in% 'Intervention', paste0('combine_', j)] <- predict(XGB_model.2, dfold.1)
  # 
  # XGB_importance <- xgb.importance(model = XGB_model.2)
  # XGB_data.2 <- data.frame(name = XGB_importance[['Feature']], importance = XGB_importance[['Gain']], sign = sign(XGB_importance[['Gain']]), stringsAsFactors = FALSE)
  # XGB_data.2[,'importance'] <- XGB_data.2[,'importance'] / max(XGB_data.2[,'importance'])
  
  XGB_data.2 <- XGB_data.1
  
  # Preprocess
  
  XGB_data <- merge(XGB_data.1, XGB_data.2, by = 'name', all = TRUE)
  XGB_data[XGB_data[,'importance.x'] %in% NA,'importance.x'] <- 0
  XGB_data[XGB_data[,'importance.y'] %in% NA,'importance.y'] <- 0
  XGB_data[,'importance'] <- (XGB_data[,'importance.x'] + XGB_data[,'importance.y']) / 2
  
  XGB_data[,'revise_name'] <- XGB_data[,'name'] %>% gsub('^`', '', .) %>% gsub('`.*$', '', .)
  XGB_data[,'revise_name'] <- intrested_var_name[XGB_data[,'revise_name']]
  
  for (i in 1:nrow(XGB_data)) {
    if (XGB_data[i,'revise_name'] %in% NA) {
      for (q in 1:length(intrested_var_name)) {
        if (grepl(names(intrested_var_name)[q], XGB_data[i,'name'])) {
          XGB_data[i,'revise_name'] <- intrested_var_name[q]
        }
      }
    }
  }
  
  SUMMARY_XGB <- tapply(XGB_data[,'importance'], XGB_data[,'revise_name'], sum)
  
  FIANL_XGB_DATA <- data.frame(name = names(SUMMARY_XGB), importance = as.numeric(SUMMARY_XGB), stringsAsFactors = FALSE)
  
  FIANL_XGB_DATA <- rbind(data.frame(name = rep('', 20),
                                     importance = rep(0, 20), stringsAsFactors = FALSE),
                          FIANL_XGB_DATA)
  
  FIANL_XGB_DATA[,'importance'] <- FIANL_XGB_DATA[,'importance'] / max(FIANL_XGB_DATA[,'importance'])
  FIANL_XGB_DATA <- FIANL_XGB_DATA[order(FIANL_XGB_DATA[,'importance'], decreasing = FALSE),]
  FIANL_XGB_DATA <- tail(FIANL_XGB_DATA, 15)
  
  FIANL_XGB_DATA[,'x'] <- 1:nrow(FIANL_XGB_DATA)
  FIANL_XGB_DATA[,'txt'] <- formatC(FIANL_XGB_DATA[,'importance'] * 100, 1, format = 'f') %>% paste0(., '%')
  FIANL_XGB_DATA[FIANL_XGB_DATA[,'name'] %in% '','txt'] <- ''
  
  if (j %in% 1) {
    
    FIANL_XGB_DATA[,'group'] <- '1'
    input_col <- c("#56B4E9")
    
  } else if (j %in% 2) {
    
    FIANL_XGB_DATA[,'group'] <- '1'
    input_col <- c("#CC79A7")
    
  } else if (j %in% 3) {
    
    FIANL_XGB_DATA[,'group'] <- '1'
    FIANL_XGB_DATA[FIANL_XGB_DATA[,'name'] %in% intrested_var_list[[2]][['name']],'group'] <- '2'
    input_col <- c("#56B4E9", "#CC79A7")
    
  }
  
  # Plotting
  
  p_XGB <- ggplot(FIANL_XGB_DATA, aes(x = x, y = importance, fill = group, group = group))
  p_XGB <- p_XGB + geom_bar(stat = "identity", position = "dodge")
  p_XGB <- p_XGB + ylab("Related importance") + xlab("") + ggtitle(names(intrested_var_list)[j]) + labs(fill = '')
  p_XGB <- p_XGB + theme(plot.title = element_text(color = "#000000", size = 15, hjust = 0),
                         legend.position = "none",
                         axis.text.x = element_text(size = 10, face = 1),
                         axis.text.y = element_text(size = 10, face = 1))
  p_XGB <- p_XGB + scale_fill_manual(values =  c(input_col))
  
  p_XGB <- p_XGB + scale_y_continuous(expand = c(0, 0), limits = c(0, 1.32), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  p_XGB <- p_XGB + scale_x_continuous(limits = c(0.5, max(FIANL_XGB_DATA[,'x']) + 0.5),
                                      breaks = FIANL_XGB_DATA[,'x'],
                                      labels = FIANL_XGB_DATA[,'name'])
  
  p_XGB <- p_XGB + annotate('text', FIANL_XGB_DATA[,'x'], FIANL_XGB_DATA[,'importance'] + 0.02, label = FIANL_XGB_DATA[,'txt'], hjust = 0)
  p_XGB <- p_XGB + coord_flip()
  
  XGB_list[[j]] <- p_XGB
  
}

# 3. AUC list

p_auc_list <- list()

auc_data <- data.frame(name = c(paste0('combine_', 1:4), 'ECG_pred.prob', intrested_var_list[[3]][['var']]),
                       show_name = c(paste0('Xgboost[', names(intrested_var_list), ']'), 'DLM prediction', intrested_var_list[[3]][['name']]),
                       colour = c('1', '1', '1', '2', '2', rep('3', intrested_var_list[[1]][['var']] %>% length()), rep('4', intrested_var_list[[2]][['var']] %>% length())),
                       auc = NA,
                       auc.l = NA,
                       auc.u = NA,
                       stringsAsFactors = FALSE)

for (DATASET_NAME in names(DATASET_NAME_LIST)) {
  
  for (q in 1:nrow(auc_data)) {
    
    if (auc_data[q,'colour'] %in% c('1', '2')) {
      
      final_data[final_data[,'DATASET'] %in% DATASET_NAME, 'x'] <- final_data[final_data[,'DATASET'] %in% DATASET_NAME, auc_data[q,'name']]
      
    } else {
      
      train_data <- final_data[final_data[,'DATASET'] %in% 'train', auc_data[q,'name'], drop = FALSE] %>% model.matrix(~ ., data = .)
      train_label <- final_data[final_data[,'DATASET'] %in% 'train', 'y']
      
      valid_data <- final_data[final_data[,'DATASET'] %in% 'valid', auc_data[q,'name'], drop = FALSE] %>% model.matrix(~ ., data = .)
      valid_label <- final_data[final_data[,'DATASET'] %in% 'valid', 'y']
      
      test_data <- final_data[final_data[,'DATASET'] %in% DATASET_NAME, auc_data[q,'name'], drop = FALSE] %>% model.matrix(~ ., data = .)
      test_label <- final_data[final_data[,'DATASET'] %in% DATASET_NAME, 'y']
      
      dfold.1 <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
      dfold.2 <- xgb.DMatrix(data = as.matrix(valid_data), label = valid_label)
      dfold.3 <- xgb.DMatrix(data = as.matrix(test_data), label = test_label)
      
      # Fold-1
      
      XGB_model.1 <- xgb.train(data = dfold.1,
                               watchlist = list(train = dfold.1, test = dfold.2),
                               eta = 0.05,  nrounds = 300, max.depth = 5, early_stopping_rounds = 10,
                               nthread = 5, verbose = FALSE, eval_metric = "auc", objective = "binary:logistic")
      
      final_data[final_data[,'DATASET'] %in% DATASET_NAME, 'x'] <- predict(XGB_model.1, dfold.3)
      
    }
    
    # AUC
    
    sub_data <- final_data[final_data[,'DATASET'] %in% DATASET_NAME,]
    
    roc_curve <- roc(response = sub_data[,'y'], predictor = as.numeric(sub_data[,'x']))
    
    auc_data[q,'auc'] <- roc_curve[['auc']]
    auc_data[q,'auc.l'] <- ci.auc(roc_curve)[1]
    auc_data[q,'auc.u'] <- ci.auc(roc_curve)[3]
    
  }
  
  # 4. AUC plot
  
  auc_data <- auc_data[order(auc_data[,'auc'], decreasing = TRUE),]
  
  auc_data[,'txt'] <- paste0(formatC(auc_data[,'auc'], 3, format = 'f'), ' (',
                             formatC(auc_data[,'auc.l'], 3, format = 'f'), '-',
                             formatC(auc_data[,'auc.u'], 3, format = 'f'), ')')
  
  # sub_auc_data <- auc_data[auc_data[,'auc.l'] > 0.5 & !auc_data[,'auc.l'] %in% NA,]
  sub_auc_data <- auc_data[!auc_data[,'auc.l'] %in% NA,]
  sub_auc_data <- sub_auc_data[!duplicated(sub_auc_data[,'show_name']),]
  sub_auc_data <- head(sub_auc_data, 10)
  sub_auc_data[,'x'] <- 1:nrow(sub_auc_data)
  
  p_auc <- ggplot(sub_auc_data, aes(x = x, y = auc, fill = colour, group = colour))
  p_auc <- p_auc + geom_bar(stat = "identity", position = "dodge", color = "black")
  p_auc <- p_auc + geom_errorbar(aes(ymin = auc.l, ymax= auc.u), width = .3, position = position_dodge(.9), color = '#303030A0')
  
  p_auc <- p_auc + ylab("AUC (95% CI)") + xlab("") + ggtitle(DATASET_NAME_LIST[DATASET_NAME]) + labs(fill = '')
  
  p_auc <- p_auc + scale_y_continuous(limits = c(0, 1.05), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = paste0(seq(0, 100, by = 20), '%'))
  p_auc <- p_auc + scale_x_continuous(limits = c(0.5, max(sub_auc_data[,'x']) + 0.5),
                                      breaks = sub_auc_data[,'x'],
                                      labels = sub_auc_data[,'show_name'])
  
  p_auc <- p_auc + annotate('text', sub_auc_data[,'x'], 0.02, label = sub_auc_data[,'txt'], size = 3.5, hjust = 0, colour = 'black', angle = 90, fontface = 1)
  p_auc <- p_auc + scale_fill_manual(values =  c("#D55B00", '#009E73', "#56B4E9", "#CC79A7"))
  
  p_auc <- p_auc + theme(plot.title = element_text(size = 15, face = 2),
                         legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.title.y = element_text(size = 14, face = 1),
                         axis.text.x = element_text(hjust = 1, angle = 45, size = 12, face = 1),
                         axis.text.y = element_text(angle = 0, size = 12, face = 1)) 
  
  p_auc_list[[DATASET_NAME]] <- p_auc
  
}

auc_p <- arrangeGrob(p_auc_list[[1]],
                     p_auc_list[[2]],
                     p_auc_list[[3]],
                     ncol = 3)

# 5. Plotting

final_p <- ggdraw()
final_p <- final_p + draw_plot(XGB_list[[1]], x = -0.01, y = 0.5, width = 0.39, height = 0.48)
final_p <- final_p + draw_plot(XGB_list[[2]], x = 0.38, y = 0.5, width = 0.3, height = 0.48)
final_p <- final_p + draw_plot(XGB_list[[3]], x = 0.68, y = 0.5, width = 0.3, height = 0.48)
final_p <- final_p + draw_plot(auc_p, x = 0.02, y = 0, width = 0.98, height = 0.48)
final_p <- final_p + draw_plot_label(LETTERS[1:2], c(0.005, 0.005), c(1, 0.5), size = 20, hjust = 0)

pdf(plot_path, width = 15, height = 12)

print(final_p)

dev.off()

