library(magrittr)
library(mxnet)

# Functions

pool_module <- function (indata, k = 16, stage = 1) {
  
  conv1 <- mx.symbol.Convolution(data = indata, kernel = c(1, 1), pad = c(0, 0), num_filter = k / 4, name = paste0('pool_module', stage, '.conv1'))
  bn1 <- mx.symbol.BatchNorm(data = conv1, axis = 1, eps = 1e-5, fix.gamma = FALSE, name = paste0('pool_module', stage, '.bn1'))
  relu1 <- mx.symbol.Activation(data = bn1, act_type = "relu", name = paste0('pool_module', stage, '.relu1'))  
  
  conv2 <- mx.symbol.Convolution(data = relu1, kernel = c(3, 1), pad = c(1, 0), stride = c(2, 1), num_filter = k / 4, name = paste0('pool_module', stage, '.conv2'))
  bn2 <- mx.symbol.BatchNorm(data = conv2, axis = 1, eps = 1e-5, fix.gamma = FALSE, name = paste0('pool_module', stage, '.bn2'))
  relu2 <- mx.symbol.Activation(data = bn2, act_type = "relu", name = paste0('pool_module', stage, '.relu2'))  
  
  conv3 <- mx.symbol.Convolution(data = relu2, kernel = c(1, 1), pad = c(0, 0), num_filter = k, name = paste0('pool_module', stage, '.conv3'))
  bn3 <- mx.symbol.BatchNorm(data = conv3, axis = 1, eps = 1e-5, fix.gamma = FALSE, name = paste0('pool_module', stage, '.bn3'))
  relu3 <- mx.symbol.Activation(data = bn3, act_type = "relu", name = paste0('pool_module', stage, '.relu3'))
  
  pool <- mx.symbol.Pooling(data = indata, pool_type = "avg", kernel = c(2, 1), stride = c(2, 1), pad = c(0, 0),
                            name = paste0('pool_module', stage, '.pool'))
  
  concat <- mx.symbol.concat(data = list(pool, relu3), num.args = 2, dim = 1, name = paste0('pool_module', stage, '.concat'))
  
  return(concat)
  
}

SE_unit <- function (indata,
                     NUM_FILTER = 16, Squeeze_coef = 4,
                     NAME = '', GENERATION = 1, STAGE = 1) {
  
  #2. Symbols
  
  pool_squeeze <- mx.symbol.mean(data = indata, axis = 3, keepdims = TRUE,
                                 name = paste0(NAME, '_pool.', GENERATION, '_', STAGE, '_squeeze'))
  
  conv1_squeeze <- mx.symbol.Convolution(data = pool_squeeze, cudnn_off = FALSE, cudnn_tune = 'limited_workspace',
                                         dilate = c(1, 1), kernel = c(1, 1), pad = c(0, 0), stride = c(1, 1),
                                         no_bias = FALSE, num_filter = NUM_FILTER/Squeeze_coef,
                                         name = paste0(NAME, '_conv.', GENERATION, '_', STAGE, '_squeeze'))
  
  relu_squeeze <- mx.symbol.Activation(data = conv1_squeeze, act_type = "relu",
                                       name = paste0(NAME, '_relu.', GENERATION, '_', STAGE, '_squeeze'))
  
  conv2_excitation <- mx.symbol.Convolution(data = relu_squeeze, cudnn_off = FALSE, cudnn_tune = 'limited_workspace',
                                            dilate = c(1, 1), kernel = c(1, 1), pad = c(0, 0), stride = c(1, 1),
                                            no_bias = FALSE, num_filter = NUM_FILTER,
                                            name = paste0(NAME, '_conv.', GENERATION, '_', STAGE, '_excitation'))
  
  sigmoid_excitation <- mx.symbol.Activation(data = conv2_excitation, act_type = "sigmoid",
                                             name = paste0(NAME, '_sigmoid.', GENERATION, '_', STAGE, '_excitation'))
  
  data_scale <- mx.symbol.broadcast_mul(indata, sigmoid_excitation,
                                        name = paste0(NAME, '_data', GENERATION, '_', STAGE, '_scale'))
  
  return(data_scale)
  
}

res_module <- function (indata, k = 32, stage = 1, loop = 1) {
  
  conv1 <- mx.symbol.Convolution(data = indata, kernel = c(1, 1), pad = c(0, 0), num_filter = k / 4, name = paste0('res_module', stage, '_', loop, '.conv1'))
  bn1 <- mx.symbol.BatchNorm(data = conv1, axis = 1, eps = 1e-5, fix.gamma = FALSE, name = paste0('res_module', stage, '_', loop, '.bn1'))
  relu1 <- mx.symbol.Activation(data = bn1, act_type = "relu", name = paste0('res_module', stage, '_', loop, '.relu1'))  
  
  conv2 <- mx.symbol.Convolution(data = relu1, kernel = c(3, 1), pad = c(1, 0), num_filter = k / 4, name = paste0('res_module', stage, '_', loop, '.conv2'))
  bn2 <- mx.symbol.BatchNorm(data = conv2, axis = 1, eps = 1e-5, fix.gamma = FALSE, name = paste0('res_module', stage, '_', loop, '.bn2'))
  relu2 <- mx.symbol.Activation(data = bn2, act_type = "relu", name = paste0('res_module', stage, '_', loop, '.relu2'))  
  
  conv3 <- mx.symbol.Convolution(data = relu2, kernel = c(1, 1), pad = c(0, 0), num_filter = k, name = paste0('res_module', stage, '_', loop, '.conv3'))
  bn3 <- mx.symbol.BatchNorm(data = conv3, axis = 1, eps = 1e-5, fix.gamma = FALSE, name = paste0('res_module', stage, '_', loop, '.bn3'))
  relu3 <- mx.symbol.Activation(data = bn3, act_type = "relu", name = paste0('res_module', stage, '_', loop, '.relu3'))
  
  se_out <- SE_unit(relu3, NUM_FILTER = k, Squeeze_coef = 4, NAME = 'SE_module', GENERATION = stage, STAGE = loop)
  
  plus <- mx.symbol.broadcast_plus(lhs = indata, rhs = se_out, name = paste0('res_module', stage, '_', loop, '.plus'))
  
  return(plus)
  
}

# Data preprocessing

data <- mx.symbol.Variable('data')

bn1_data <- mx.symbol.BatchNorm(data = data, axis = 2, eps = 1e-5, fix.gamma = TRUE, name = 'bn1_data')

conv_data <- mx.symbol.Convolution(data = bn1_data, kernel = c(11, 1), pad = c(5, 0), stride = c(2, 1), num_filter = 16, name = 'conv_data')
bn2_data <- mx.symbol.BatchNorm(data = conv_data, axis = 1, eps = 1e-5, fix.gamma = FALSE, name = 'bn2_data')
relu_data <- mx.symbol.Activation(data = bn2_data, act_type = "relu", name = 'relu_data')

# Loop architecture

k_seq <- c(16, 32, 64, 128, 256, 512)
n_res <- c(3, 3, 6, 6, 3, 3)

res_output <- relu_data

for (i in 1:length(k_seq)) {
  
  pool_output <- pool_module(indata = res_output, k = k_seq[i], stage = i)
  res_output <- pool_output
  for (j in 1:n_res[i]) {res_output <- res_module(indata = res_output, k = k_seq[i] * 2, stage = i, loop = j)}
  
}

final_pool <- mx.symbol.mean(data = res_output, axis = 3, keepdims = FALSE, name = 'final_pool')

feature_list <- mx.symbol.SliceChannel(data = final_pool, num.outputs = 12, axis = 2, squeeze.axis = FALSE, name = 'feature_list')
sigmoid_list <- list()
attention_list <- list()

for (lead in 1:12) {
  
  lead_pred <- mx.symbol.FullyConnected(data = feature_list[[lead]], num.hidden = 1, name = paste0('lead', lead, '_pred'))
  sigmoid_list[[lead]] <- mx.symbol.sigmoid(data = lead_pred, name = paste0('lead', lead, '_sigmoid'))
  
  lead_att1 <- mx.symbol.FullyConnected(data = feature_list[[lead]], num.hidden = 8, no.bias = TRUE, name = paste0('lead', lead, '_att1'))
  lead_bn <- mx.symbol.BatchNorm(data = lead_att1, axis = 1, eps = 1e-5, fix.gamma = FALSE, name = paste0('lead', lead, '_bn'))
  lead_relu <- mx.symbol.Activation(data = lead_bn, act_type = "relu", name = paste0('lead', lead, '_relu'))  
  attention_list[[lead]] <- mx.symbol.FullyConnected(data = lead_relu, num.hidden   = 1, name = paste0('lead', lead, '_att2'))
  
}

pred_concat <- mx.symbol.concat(data = sigmoid_list, num.args = 12, dim = 1, name = 'pred_concat')
attention_concat <- mx.symbol.concat(data = attention_list, num.args = 12, dim = 1, name = 'attention_concat')
attention_score <- mx.symbol.softmax(data = attention_concat, axis = 1, name = 'attention_score')

weighted_pred <- mx.symbol.broadcast_mul(pred_concat, attention_score, name = 'weighted_pred')
final_pred <- mx.symbol.sum(weighted_pred, axis = 1, keepdims = TRUE, name = 'final_pred')

# Loss

label <- mx.symbol.Variable(name = 'label')

eps <- 1e-8
ce_loss_pos <- mx.symbol.broadcast_mul(mx.symbol.log(final_pred + eps), label)
ce_loss_neg <- mx.symbol.broadcast_mul(mx.symbol.log(1 - final_pred + eps), 1 - label)
ce_loss_mean <- 0 - mx.symbol.mean(ce_loss_pos + ce_loss_neg)
ce_loss <- mx.symbol.MakeLoss(ce_loss_mean, name = 'ce_loss')