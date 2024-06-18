# ALE null model
# load libraries and setup
x<-c("ALEPlot", "ggplot2", "stringr","mlbench","pheatmap","dplyr","ALEPLotPlus")
lapply(x, require, character.only = TRUE)
library(ALEPlot)
library(ggplot2)
library(ALEPlotPlus)
library(stringr)
library(dplyr)
library(pheatmap)
library(mlbench)

# set the number of observations
n = 1000

b0 = 0
b1  = 10
b2  = 5
b3  = 0
b12 = 0
b13 = 0
b23 = 0

#simulate values of x1, x2, and x3 from a normal dist

x1 = rnorm(n, mean = 0, sd = 1)
x2 = rnorm(n, mean = 0, sd = 1)
x3 = rnorm(n, mean = 0, sd = 1)
# calculate noise
eps = rnorm(n, mean = 0,sd = .0000000001)

#estimate response based on underlying model
y <- b0 + b1*x1 + b2*x2 + b3*x3 + b12*x1*x2 + b13*x1*x3 + b23*x2*x3 + eps

df = data.frame(x1 = x1,x2 = x2,x3 = x3, y = y)
X = df[,-4]
mdl = randomForest::randomForest(y ~.,data = df,importance = T)

NullY = sample(y,length(y))
nulldf = df
nulldf$y = NullY
NullModel = randomForest::randomForest(y ~.,data = nulldf,importance = T)

#commented out cversion of code to see the code without the plot pollution the original aleplot functions produce.
pred.fun <- function(X.model, newdata) {
  prd_out = predict(X.model, newdata)
  return(prd_out)
}
Main_var_imps = calc_ALE_varimps_mean(X        = X,
                                      K        = 10,
                                      X.MODEL  = mdl,
                                      pred.fun = pred.fun)

Main_var_imps
Main_var_imp_df = calc_ALE_varimp_df(X       = X, 
                                     X.MODEL = mdl, 
                                     K       = 10, 
                                     pred.fun= pred.fun)

Main_var_impsNull = calc_ALE_varimps_mean(X        = X,
                                      K        = 10,
                                      X.MODEL  = NullModel,
                                      pred.fun = pred.fun)
Main_var_impsNull
Main_var_impsNulldf = calc_ALE_varimp_df(X        = X,
                                          K        = 10,
                                          X.MODEL  = NullModel,
                                          pred.fun = pred.fun)
Main_var_impsNulldf = calc_ALE_varimp_df(X        = X,
                                         K        = 10,
                                         X.MODEL  = NullModel,
                                         pred.fun = pred.fun)

plot(Main_var_impsNulldf[,3])
plot(Main_var_imp_df[,3])

t.test(abs(Main_var_impsNulldf[,3]),
       abs(Main_var_imp_df[,3]),
       paired = T,
       alternative= "less")
t.test(abs(Main_var_impsNulldf[,2]),abs(Main_var_imp_df[,2]),
       paired = T,
       alternative= "less")
