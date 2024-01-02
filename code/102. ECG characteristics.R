
library(magrittr)

source('./code/001. Table function.R')

# 0. Settings

postive_cut <- 0.349791174897487

intrested_var.1 <- c('ECG[Rate]', 'ECG[PR]', 'ECG[QRSd]', 'ECG[QT]', 'ECG[QTc]', 'ECG[Axes_P]', 'ECG[Axes_QRS]', 'ECG[Axes_T]')
intrested_var.2 <- c('ECG[SR]', 'ECG[SA]', 'ECG[SP]', 'ECG[EAR]', 'ECG[JER]', 'ECG[PMR]', 'ECG[ET]', 'ECG[STE]', 'ECG[STD]', 'ECG[ATW]', 'ECG[AQW]', 'ECG[RSRW]', 'ECG[LVOL]', 'ECG[LAD]', 'ECG[RAD]', 'ECG[LVH]', 'ECG[RVH]', 'ECG[LAE]', 'ECG[RAE]', 'ECG[LAA]', 'ECG[RAA]', 'ECG[NIVCD]', 'ECG[LFB]', 'ECG[RBBB]', 'ECG[LBBB]', 'ECG[1AVB]', 'ECG[2AVB]', 'ECG[CAVB]', 'ECG[AFIB]', 'ECG[AFLT]', 'ECG[SVT]', 'ECG[VPE]', 'ECG[RVPC]', 'ECG[APC]', 'ECG[VPC]')

data_path <- './data/raw_data.RData'
table_path <- './results/Supplementary Tables 5-7.doc'

# Fig 01 Flow chart

load(data_path)

test_data <- accuracy_data[accuracy_data[,'DATASET'] %in% c('internal-test', 'community-test', 'external-test'),]
test_data[,'AI'] <- factor((test_data[,'ECG_pred.prob'] >= postive_cut) + 0L)

for (m in 1:length(intrested_var.2)) {test_data[,intrested_var.2[m]] <- factor(test_data[,intrested_var.2[m]], 0:1)}

test_data <- test_data[!test_data[,intrested_var.2[m]] %in% NA,]

# 2. Build table

table_list <- list()

# print(table(test_data[,'HT_SEVERITY']))

table_list[[1]] <- Table1(X = test_data[,'HT_SEVERITY'], Y.matrix = test_data[,c(intrested_var.1, intrested_var.2)], x.name = "Group")

# print(table(test_data[,'AI']))

table_list[[2]] <- Table1(X = test_data[,'AI'], Y.matrix = test_data[,c(intrested_var.1, intrested_var.2)], x.name = "Group")

used_sample <- which(test_data[,'AI'] %in% 1)
# print(table(test_data[used_sample,'HT_SEVERITY']))

table_list[[3]] <- Table1(X = test_data[used_sample,'HT_SEVERITY'], Y.matrix = test_data[used_sample,c(intrested_var.1, intrested_var.2)], x.name = "Group")

used_sample <- which(test_data[,'AI'] %in% 0)
# print(table(test_data[used_sample,'HT_SEVERITY']))

table_list[[4]] <- Table1(X = test_data[used_sample,'HT_SEVERITY'], Y.matrix = test_data[used_sample,c(intrested_var.1, intrested_var.2)], x.name = "Group")

# 4. Write out

Table2doc(table_list = table_list, filename = table_path)
