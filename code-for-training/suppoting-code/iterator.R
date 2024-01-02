
library(magrittr)
library(mxnet)
library(abind)
library(data.table)

data_dir <- 'example-for-training/ecg/'

my_iterator_core <- function (batch_size) {
  
  batch <- 0
  batch_per_epoch <- ceiling(nrow(train_dat) / batch_size)
  
  reset <- function() {batch <<- 0}
  
  iter.next <- function() {
    batch <<- batch + 1
    if (batch > batch_per_epoch) {return(FALSE)} else {return(TRUE)}
  }
  
  value <- function() {
    pos.idx <- sample(which(train_dat[,'HT'] == 1), batch_size / 2)
    neg.idx <- sample(which(train_dat[,'HT'] == 0), batch_size / 2)
    idx <- c(pos.idx, neg.idx)
    batch_list <- list()
    for (i in 1:batch_size) {
      ecg_matrix <- fread(paste0(data_dir, train_dat[idx[i],'UID'], '.csv'), data.table = FALSE)
      batch_list[[i]] <- ecg_matrix
    }
    batch_array <- abind(batch_list, along = 3)
    dim(batch_array) <- c(5000, 12, 1, batch_size)
    batch_array <- batch_array[sample(0:904, size = 1)+1:4096,,,,drop=FALSE]
    label_array <- array(train_dat[idx,'HT'], dim = c(1, batch_size))
    return(list(data = mx.nd.array(batch_array), label = mx.nd.array(label_array)))
  }
  
  return(list(reset = reset, iter.next = iter.next, value = value, batch_size = batch_size, batch = batch))
  
}

my_iterator_func <- setRefClass("Custom_Iter",
                                fields = c("iter", "batch_size"),
                                contains = "Rcpp_MXArrayDataIter",
                                methods = list(
                                  initialize = function(iter, batch_size = 32) {
                                    .self$iter <- my_iterator_core(batch_size = batch_size)
                                    .self
                                  },
                                  value = function(){
                                    .self$iter$value()
                                  },
                                  iter.next = function(){
                                    .self$iter$iter.next()
                                  },
                                  reset = function(){
                                    .self$iter$reset()
                                  },
                                  finalize=function(){
                                  }
                                )
)

my_iter <- my_iterator_func(iter = NULL, batch_size = 2)