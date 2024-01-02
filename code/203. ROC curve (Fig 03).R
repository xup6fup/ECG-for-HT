
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
plot_path <- './results/Figure 03.pdf'

DATASET_NAME <- c('internal-test' = 'Internal test set', 'community-test' = 'Community test set', 'external-test' = 'Isolated test set')
col_list.dataset <- c('internal-test' = '#CC79A7', 'community-test' = '#D55B00', 'external-test' = '#56B4E9')

MAIN_ECG_NAME_list <- list()
MAIN_ECG_NAME_list[['TF1']] <- c(expression(paste('HT vs. non-HT')))
MAIN_ECG_NAME_list[['TF2']] <- c(expression(paste('Overt HT vs. non-HT')))
MAIN_ECG_NAME_list[['TF3']] <- c(expression(paste('Subclinical HT vs. non-HT')))

# 1. Load data

load(data_path)

accuracy_data[,'TF1'] <- (accuracy_data[,'HT_GROUP'] %in% 'HT') + 0L

accuracy_data[,'TF2'] <- (accuracy_data[,'HT_SEVERITY'] %in% 'overt-HT') + 0L
accuracy_data[!accuracy_data[,'HT_SEVERITY'] %in% c('overt-HT', 'non-HT'), 'TF2'] <- NA

accuracy_data[,'TF3'] <- (accuracy_data[,'HT_SEVERITY'] %in% 'subclinical-HT') + 0L
accuracy_data[!accuracy_data[,'HT_SEVERITY'] %in% c('subclinical-HT', 'non-HT'), 'TF3'] <- NA

# Functions

PLOT_ROC_CURVE <- function (x, y, best_cut,
                            title = 'ROC curve', col = '#F8766D',
                            show_PRAUC = FALSE, show_cut = FALSE) {
  
  X <- x[!y %in% NA & !x %in% NA]
  Y <- y[!y %in% NA & !x %in% NA]
  
  if (!0 %in% Y | !1 %in% Y) {stop('y need to include 0 and 1')} else {
    
    roc_curve <- roc(response = Y, predictor = X)
    
    if (show_PRAUC) {
      
      tab <- table(factor(Y, levels = 0:1))
      
      roc_curve[['ppv']] <- tab[2] * roc_curve[['sensitivities']] / (tab[1] * (1 - roc_curve[['specificities']]) + tab[2] * roc_curve[['sensitivities']] + 1e-15)
      roc_curve[['f1']] <- 2 * roc_curve[['ppv']] * roc_curve[['sensitivities']] / (roc_curve[['ppv']] + roc_curve[['sensitivities']] + 1e-15)
      
      pb <- txtProgressBar(max = length(roc_curve[['thresholds']]), style = 3)
      for (l in 2:length(roc_curve[['thresholds']])) {
        if (roc_curve[['ppv']][l] < max(roc_curve[['ppv']][-l:-length(roc_curve[['thresholds']])])) {roc_curve[['ppv']][l] <- max(roc_curve[['ppv']][-l:-length(roc_curve[['thresholds']])])}
        setTxtProgressBar(pb, l)
      }
      close(pb)
      
    }
    
    tab_test <- table(factor(X > best_cut, levels = c(TRUE, FALSE)), factor(Y, levels = c(1, 0)))
    
    sens_test <- tab_test[1,1] / sum(tab_test[,1])
    spec_test <- tab_test[2,2] / sum(tab_test[,2])
    ppv_test <- tab_test[1,1] / sum(tab_test[1,])
    npv_test <- tab_test[2,2] / sum(tab_test[2,])
    F1_test <- (ppv_test^(-1) + sens_test^(-1) + 1e-15)^(-1) * 2
    
    # Show text
    
    if (show_PRAUC) {
      
      roc_data <- data.frame(ppv = roc_curve[['ppv']], sens = roc_curve[['sensitivities']])
      roc_data <- rbind(data.frame(ppv = 0, sens = 1), roc_data, data.frame(ppv = 1, sens = 0))
      roc_data <- roc_data[order(roc_data[,'ppv'], decreasing = TRUE),]
      roc_data <- roc_data[order(roc_data[,'sens']),]
      rownames(roc_data) <- 1:nrow(roc_data)
      
      ppv_test_plot <- max(roc_data[which(roc_data[,'sens'] %in% sens_test), 'ppv'])
      
      if (show_cut) {
        
        roc_txt.1 <- paste0('Cut-off', 
                            '\nPRAUC ', 
                            '\nF score', 
                            '\nSens. ', 
                            '\nSpec. ', 
                            '\nPPV ', 
                            '\nNPV ', 
                            '')
        
        roc_txt.2 <- paste0('', formatC(abs(best_cut), 1 + (best_cut >= -1) * 2, format = 'f'),
                            '\n', formatC(diff(roc_data[,'sens']) %*% roc_data[,'ppv'][-nrow(roc_data)], 4, format = 'f'),
                            '\n', formatC(F1_test, 4, format = 'f'),
                            '\n', formatC(sens_test * 100, 1, format = 'f'),
                            '%\n', formatC(spec_test * 100, 1, format = 'f'),
                            '%\n', formatC(ppv_test * 100, 1, format = 'f'),
                            '%\n', formatC(npv_test * 100, 1, format = 'f'),
                            '%')
        
      } else {
        
        roc_txt.1 <- paste0('PRAUC ', 
                            '\nF score', 
                            '\nSens. ', 
                            '\nSpec. ', 
                            '\nPPV ', 
                            '\nNPV ', 
                            '')
        
        roc_txt.2 <- paste0(formatC(diff(roc_data[,'sens']) %*% roc_data[,'ppv'][-nrow(roc_data)], 4, format = 'f'),
                            '\n', formatC(F1_test, 4, format = 'f'),
                            '\n', formatC(sens_test * 100, 1, format = 'f'),
                            '%\n', formatC(spec_test * 100, 1, format = 'f'),
                            '%\n', formatC(ppv_test * 100, 1, format = 'f'),
                            '%\n', formatC(npv_test * 100, 1, format = 'f'),
                            '%')
        
      }
      
    } else {
      
      roc_data <- data.frame(spec = roc_curve[['specificities']], sens = roc_curve[['sensitivities']])
      roc_data <- rbind(data.frame(spec = 0, sens = 1), roc_data, data.frame(spec = 1, sens = 0))
      roc_data <- roc_data[order(roc_data[,'spec'], decreasing = TRUE),]
      roc_data <- roc_data[order(roc_data[,'sens']),]
      rownames(roc_data) <- 1:nrow(roc_data)
      
      if (show_cut) {
        
        roc_txt.1 <- paste0('Cut-off', 
                            '\nAUC ', 
                            '\nF1 score', 
                            '\nSens. ', 
                            '\nSpec. ', 
                            '\nPPV ', 
                            '\nNPV ', 
                            '')
        
        roc_txt.2 <- paste0('', formatC(abs(best_cut), 1 + (best_cut >= -1) * 2, format = 'f'),
                            '\n', formatC(roc_curve[['auc']], 4, format = 'f'),
                            '\n', formatC(2 * ppv_test * sens_test / (sens_test + ppv_test), 3, format = 'f'),
                            '\n', formatC(sens_test * 100, 1, format = 'f'),
                            '%\n', formatC(spec_test * 100, 1, format = 'f'),
                            '%\n', formatC(ppv_test * 100, 1, format = 'f'),
                            '%\n', formatC(npv_test * 100, 1, format = 'f'),
                            '%')
        
      } else {
        
        roc_txt.1 <- paste0('AUC ', 
                            '\nF1 score', 
                            '\nSens. ', 
                            '\nSpec. ', 
                            '\nPPV ', 
                            '\nNPV ', 
                            '')
        
        roc_txt.2 <- paste0(formatC(roc_curve[['auc']], 3, format = 'f'),
                            '\n', formatC(2 * ppv_test * sens_test / (sens_test + ppv_test), 3, format = 'f'),
                            '\n', formatC(sens_test * 100, 1, format = 'f'),
                            '%\n', formatC(spec_test * 100, 1, format = 'f'),
                            '%\n', formatC(ppv_test * 100, 1, format = 'f'),
                            '%\n', formatC(npv_test * 100, 1, format = 'f'),
                            '%')
        
      }
      
    }
    
    # ROC curve
    
    if (show_PRAUC) {
      
      roc_p <- ggplot(data = roc_data, aes(x = sens, y = ppv))
      roc_p <- roc_p + geom_line(colour = col, size = 1.5)
      roc_p <- roc_p + theme_bw()
      roc_p <- roc_p + coord_equal()
      roc_p <- roc_p + ggtitle(title)
      roc_p <- roc_p + ggtitle(title) + xlab('Sensitivity') + ylab('Positive predictive value')
      roc_p <- roc_p + annotate(geom = "point", x = sens_test, y = ppv_test_plot, shape = 21, size = 5, fill = paste0(col[SET], 'A0'), color = '#000000')
      roc_p <- roc_p + annotate(geom = "text", x = 0.1, y = 0.05, label = roc_txt.1, size = 5, fontface = 2, colour = '#00000080', hjust = 0, vjust = 0)
      roc_p <- roc_p + annotate(geom = "text", x = 0.9, y = 0.05, label = roc_txt.2, size = 5, fontface = 2, colour = '#00000080', hjust = 1, vjust = 0)
      
    } else {
      
      roc_p <- ggplot(data = roc_data, aes(x = spec, y = sens))
      roc_p <- roc_p + geom_line(colour = col, size = 1)
      roc_p <- roc_p + theme_bw()
      roc_p <- roc_p + coord_equal()
      roc_p <- roc_p + ggtitle(title)
      roc_p <- roc_p + ggtitle(title) + xlab('Specificity') + ylab('Sensitivity')
      roc_p <- roc_p + annotate(geom = "point", x = spec_test, y = sens_test, shape = 21, size = 4, fill = paste0(col[SET], 'A0'), color = '#000000')
      roc_p <- roc_p + annotate(geom = "text", x = 0.05, y = 0.00, label = roc_txt.1, size = 2.5, fontface = 2, colour = col[SET], hjust = 0, vjust = 0)
      roc_p <- roc_p + annotate(geom = "text", x = 0.50, y = 0.00, label = roc_txt.2, size = 2.5, fontface = 2, colour = col[SET], hjust = 1, vjust = 0)
      
    }
    
    roc_p <- roc_p + theme(plot.title = element_text(color = "#000000", size = 10),
                           axis.title = element_text(color = "#000000", size = 8),
                           legend.position = "none")
    
    return(roc_p)
    
  }
  
}

# 3. ROC curve

roc_list <- list()

for (i in 1:length(MAIN_ECG_NAME_list)) {
  
  roc_list[[names(MAIN_ECG_NAME_list)[i]]] <- list()
  
  y_name <- names(MAIN_ECG_NAME_list)[i]
  x_name <- 'ECG_pred.prob'
  
  if (!is.null(postive_cut)) {best_cut <- postive_cut} else {
    
    valid_data <- accuracy_data[accuracy_data[,'DATASET'] %in% 'valid',]
    
    # Get cut-off points
    
    roc_curve.valid <- roc(response = valid_data[,y_name], predictor = valid_data[,x_name])
    
    if (show_PRAUC) {
      
      tab <- table(factor(valid_data[,y_name], levels = 0:1))
      roc_curve.valid[['ppv']] <- tab[2] * roc_curve.valid[['sensitivities']] / (tab[1] * (1 - roc_curve.valid[['specificities']]) + tab[2] * roc_curve.valid[['sensitivities']] + 1e-15)
      roc_curve.valid[['f1']] <- 2 * roc_curve.valid[['ppv']] * roc_curve.valid[['sensitivities']] / (roc_curve.valid[['ppv']] + roc_curve.valid[['sensitivities']] + 1e-15)
      
      pb <- txtProgressBar(max = length(roc_curve.valid[['thresholds']]), style = 3)
      for (l in 2:length(roc_curve.valid[['thresholds']])) {
        if (roc_curve.valid[['ppv']][l] < max(roc_curve.valid[['ppv']][-l:-length(roc_curve.valid[['thresholds']])])) {roc_curve.valid[['ppv']][l] <- max(roc_curve.valid[['ppv']][-l:-length(roc_curve.valid[['thresholds']])])}
        setTxtProgressBar(pb, l)
      }
      close(pb)
      
      best_pos <- which.max(roc_curve.valid[['f1']])
      best_cut <- roc_curve.valid[['thresholds']][best_pos]
      
    } else {
      
      best_pos <- which.max(roc_curve.valid[['sensitivities']] + roc_curve.valid[['specificities']])
      best_cut <- roc_curve.valid[['thresholds']][best_pos]
      
    }
    
  }
  
  message(MAIN_ECG_NAME_list[[i]], ': ', best_cut)
  
  for (SET in names(DATASET_NAME)) {
    
    sub_data <- accuracy_data[accuracy_data[,'DATASET'] %in% SET,]
    
    roc_p <- PLOT_ROC_CURVE(x = sub_data[,x_name], y = sub_data[,y_name], best_cut = best_cut,
                            title = MAIN_ECG_NAME_list[[i]], col = col_list.dataset[SET],
                            show_PRAUC = FALSE, show_cut = FALSE)
    
    
    roc_list[[names(MAIN_ECG_NAME_list)[i]]][[SET]] <- roc_p
    
  }
  
}

# 4. Plottings

roc_p.1 <- arrangeGrob(roc_list[[1]][[1]],
                       roc_list[[2]][[1]],
                       roc_list[[3]][[1]],
                       ncol = 3)

roc_p.2 <- arrangeGrob(roc_list[[1]][[2]],
                       roc_list[[2]][[2]],
                       roc_list[[3]][[2]],
                       ncol = 3)

roc_p.3 <- arrangeGrob(roc_list[[1]][[3]],
                       roc_list[[2]][[3]],
                       roc_list[[3]][[3]],
                       ncol = 3)

final_p <- ggdraw()
final_p <- final_p + draw_plot(roc_p.1, x = 0, y = 0.666, width = 1, height = 0.320)
final_p <- final_p + draw_plot(roc_p.2, x = 0, y = 0.333, width = 1, height = 0.320)
final_p <- final_p + draw_plot(roc_p.3, x = 0, y = 0, width = 1, height = 0.320)
final_p <- final_p + draw_plot_label(DATASET_NAME, c(0.005, 0.005, 0.005), c(0.995, 0.661, 0.328), size = 15, colour = col_list.dataset, hjust = 0)

pdf(plot_path, width = 7.2, height = 9)

print(final_p)

dev.off()