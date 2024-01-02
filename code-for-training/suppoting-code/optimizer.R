library(mxnet)

my_optimizer <- mx.opt.create(name = "adam", learning.rate = 1e-3, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-08, wd = 1e-4)