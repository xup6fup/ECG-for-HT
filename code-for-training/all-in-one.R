
library(data.table)

train_dat <- fread('example-for-training/train.csv', data.table = FALSE)
test_dat <- fread('example-for-training/test.csv', data.table = FALSE)

source('./code-for-training/suppoting-code/iterator.R')
source('./code-for-training/suppoting-code/architecture.R')
source('./code-for-training/suppoting-code/optimizer.R')

my.eval.metric.loss <- mx.metric.custom(
  name = "my-loss", 
  function(real, pred) {
    return(as.array(pred))
  }
)

mx.set.seed(0)

my_model <- mx.model.FeedForward.create(symbol = ce_loss, X = my_iter, optimizer = my_optimizer,
                                        array.batch.size = 2, num.round = 30, ctx = mx.gpu(0),
                                        eval.metric = my.eval.metric.loss)

my_model$symbol <- final_pred

test_list <- list()

for (i in 1:nrow(test_dat)) {
  ecg_matrix <- fread(paste0(data_dir, test_dat[i,'UID'], '.csv'), data.table = FALSE)
  test_list[[i]] <- ecg_matrix
}

test_array <- abind(test_list, along = 3)
dim(test_array) <- c(5000, 12, 1, dim(test_array)[3])

pred_test.1 <- predict(model = my_model, X = test_array[1:4096,,,,drop=FALSE], ctx = mx.gpu(0))
pred_test.2 <- predict(model = my_model, X = test_array[905:5000,,,,drop=FALSE], ctx = mx.gpu(0))

test_dat[,'p_HT'] <- (pred_test.1[1,] +  pred_test.2[1,]) / 2
fwrite(test_dat, file = 'example-for-training/my_prediction.csv', na = '', row.names = FALSE, quote = FALSE)
