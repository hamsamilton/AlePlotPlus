# load libraries and setup
x<-c("ALEPlot", "ggplot2", "stringr","mlbench","pheatmap","dplyr","ALEPlotPlus",
     'testthat')
lapply(x, require, character.only = TRUE)

set.seed(1)
# set the number of observations

#simulate values of x1, x2, and x3 from a normal dist
x1 = rnorm(n, mean = 0, sd = 1)
x2 = rnorm(n, mean = 0, sd = 1)
x3 = rnorm(n, mean = 0, sd = 1)
# calculate noise
eps = rnorm(n, mean = 0,sd = .0000000001)

#estimate response based on underlying model
y <- b0 + b1*x1 + b2*x2 + b3*x3 + b12*x1*x2 + b13*x1*x3 + b23*x2*x3 + eps

df = data.frame(x1 = x1,
                x2 = x2,
                x3 = x3, 
                y = y)
X = df[,-4]
mdl = randomForest::randomForest(y ~.,
                                 data       = df,
                                 importance = T)

pred.fun <- function(X.model, newdata) {
  prd_out = predict(X.model, newdata)
  return(prd_out)
}

ALEInteractions = find_ALE_interacts(X        = X,
                                     X.MODEL  = mdl,
                                     pred.fun = pred.fun,
                                     K        = 20)

test_that('Check interaction values', {
  expect_equal(ALEInteractions$val[1], -1.393119,tolerance= 1e-4)})

ALE1D = calc_ALE_varimps_mean(X        = X,
                              X.MODEL  = mdl,
                              pred.fun = pred.fun,
                              K        = 20)

test_that('Check ALEvarimp values', {
  expect_equal(ALE1D[[1]], 4.829025,tolerance= 1e-4)})

test_that('Testing 2D Plotting', {
  expect_no_error(create_2D_ALEs(X        = X,
                                 X.MODEL  = mdl,
                                 pred.fun = pred.fun,
                                 K        = 20,
                                 savedir  = 'ALEtests'))})

test_that('Testing 1D Plotting', {
  expect_no_error(mkALEplots(X        = X,
                             X.MODEL  = mdl,
                             pred.fun = pred.fun, 
                             K        = 20))})

