

ALEPlot(X = pimi$X,
        X.model = pimi$X.MODEL,
        pred.fun = pimi$pred.fun,
        J = 'age',
        K = 20)

# get all combos
var_combs = combn(1:ncol(X),m = 2,simplify=T)

# gen ALEplot
int_estimates = lapply(1:ncol(var_combs),function(i){
  print(i)
  i = 7
  var_combs[,i] = c(8,1)
  int_estimate = find_ALE_interact(X        = X,
                                   X.MODEL  = X.MODEL,
                                   pred.fun = pred.fun,
                                   int_vars = var_combs[,i],
                                   K        = K)
        
  