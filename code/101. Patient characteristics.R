
library(magrittr)

source('./code/001. Table function.R')

# 0. Settings

intrested_var.1 <- c('POS_G', 'HISTORY[HT]', 'HISTORY[ATA]', 'NEAREST_DAY_G',
                     'GENDER', 'AGE', 'BMI', 'DM', 'HTN', 'HLP', 'CKD', 'AMI', 'STK', 'CAD', 'HF', 'Afib', 'COPD')

data_path <- './data/raw_data.RData'
table_path <- './results/Supplementary Tables 1-4.doc'

# Fig 01 Flow chart

load(data_path)

# POS_G

accuracy_data[accuracy_data[,'ECG[POSITION]'] %in% 'Unknown','ECG[POSITION]'] <- 'OPD'
accuracy_data[accuracy_data[,'ECG[POSITION]'] %in% c('OPD', 'PEC'),'ECG[POSITION]'] <- 'OPD'

accuracy_data[,'POS_G'] <- factor(accuracy_data[,'ECG[POSITION]'])

# HISTORY[HT] & 'HISTORY[ATA]

accuracy_data[,'HISTORY[HT]'] <- factor(accuracy_data[,'HISTORY[HT]'], 0:1)
accuracy_data[,'HISTORY[ATA]'] <- factor(accuracy_data[,'HISTORY[ATA]'], 0:1)

# NEAREST_DAY_G

accuracy_data[,'NEAREST_DAY_G'] <- factor(0L, levels = 0:1)
accuracy_data[abs(accuracy_data[,'TSH[NEAREST_DAY]']) <= 1,'NEAREST_DAY_G'] <- 1L

# 2. Build table

table_list <- list()

# 2-1. accuracy data

hospital_A <- accuracy_data[accuracy_data[,'DATASET'] %in% c('train', 'valid', 'internal-test'),]
hospital_A[,'DATASET'] <- factor(hospital_A[,'DATASET'], levels = c('train', 'valid', 'internal-test'))

table_list[[1]] <- Table1(X = hospital_A[,'DATASET'], Y.matrix = hospital_A[,intrested_var.1], x.name = "Group")

accuracy_data[,'HOSPITAL'] <- 'A'
accuracy_data[accuracy_data[,'DATASET'] %in% 'community-test','HOSPITAL'] <- 'B'
accuracy_data[accuracy_data[,'DATASET'] %in% 'external-test','HOSPITAL'] <- 'C'

table_list[[2]] <- Table1(X = accuracy_data[,'HOSPITAL'], Y.matrix = accuracy_data[,intrested_var.1], x.name = "Group")

test_data.1 <- accuracy_data[accuracy_data[,'DATASET'] %in% c('internal-test'),]

table_list[[3]] <- Table1(X = test_data.1[,'HT_GROUP'], Y.matrix = test_data.1[,c('HT_SEVERITY', intrested_var.1)], x.name = "Group")

test_data.2 <- accuracy_data[accuracy_data[,'DATASET'] %in% c('community-test'),]

table_list[[4]] <- Table1(X = test_data.2[,'HT_GROUP'], Y.matrix = test_data.2[,c('HT_SEVERITY', intrested_var.1)], x.name = "Group")

test_data.3 <- accuracy_data[accuracy_data[,'DATASET'] %in% c('external-test'),]

table_list[[5]] <- Table1(X = test_data.3[,'HT_GROUP'], Y.matrix = test_data.3[,c('HT_SEVERITY', intrested_var.1)], x.name = "Group")

test_data.1 <- accuracy_data[accuracy_data[,'DATASET'] %in% c('internal-test') & accuracy_data[,'HT_SEVERITY'] %in% c('overt-HT', 'subclinical-HT'),]

table_list[[6]] <- Table1(X = test_data.1[,'HT_SEVERITY'], Y.matrix = test_data.1[,c(intrested_var.1)], x.name = "Group")

test_data.2 <- accuracy_data[accuracy_data[,'DATASET'] %in% c('community-test') & accuracy_data[,'HT_SEVERITY'] %in% c('overt-HT', 'subclinical-HT'),]

table_list[[7]] <- Table1(X = test_data.2[,'HT_SEVERITY'], Y.matrix = test_data.2[,c(intrested_var.1)], x.name = "Group")

test_data.3 <- accuracy_data[accuracy_data[,'DATASET'] %in% c('external-test') & accuracy_data[,'HT_SEVERITY'] %in% c('overt-HT', 'subclinical-HT'),]

table_list[[8]] <- Table1(X = test_data.3[,'HT_SEVERITY'], Y.matrix = test_data.3[,c(intrested_var.1)], x.name = "Group")

# 4. Write out

Table2doc(table_list = table_list, filename = table_path)
