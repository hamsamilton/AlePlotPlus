# The purpose of this is to hold tests which test the package

#' Compare runtime of ALEPlotPlus and vint speed
#'@export
#'
#'
nlist = list(20,100)
plist = list(10)

# Calculates the time to calculate the ALE interactions
CalculateALEExecutionTime <- function(model,TrainingData){
  
  pred.fun  <- function(X.model,
                        newdata){
    Predictions = predict(X.model,
                          newdata)
    return(Predictions)}
  
  pdf("myplot.pdf")
  
  StartTime = Sys.time()
  
  ALEVarInteractions <- find_ALE_interacts(X        = TrainingData,
                                           X.MODEL  = model,
                                           pred.fun = pred.fun,
                                           K        = 40) 
  EndTime = Sys.time()
  dev.off()
  ALEExecutionTime = EndTime - StartTime
  print(ALEExecutionTime)
  return(ALEExecutionTime)}

CalculateAllVints <- function(model,
                              TrainingData){
  
  VarCombs = combn(colnames(TrainingData),
                   m        = 2,
                   simplify = T)
  
  VIPExecutionTimes = lapply(1:ncol(VarCombs), function(i) {
    print(VarCombs[,i])           
    StartTime = Sys.time()
    ImportanceEstimate = vint(model,
                              feature_names = VarCombs[,i])
    EndTime = Sys.time()
    VIPExecutionTime = EndTime - StartTime
    return(VIPExecutionTime)
    
  })
  VIPExecutionTimes = Reduce(`+`,VIPExecutionTimes)
  print(VIPExecutionTimes)
  return(VIPExecutionTimes)}

# make the data and train the model
PrepData = function(p,n){
  
  vardists = lapply(1:p,function(i){
    vals = rnorm(n,
                 mean = 0, 
                 sd   = 10)
    return(vals)
  })
  
  df = vardists %>% 
    as.data.frame() 
  
  # replace the cursed names that are automatically given
  names(df) = paste0("V",
                     seq(1,ncol(df),1))
  return(df)
}     


# CompletionTimesp = lapply(plist, function(p){ print(p)
#     
#   
#   CompletionTimesn = lapply(nlist, function(n){ print(n)
#    
#     
#     df = PrepData(p,n)
#     
#     Y  = rowSums(df) + rnorm(n,
#                              mean = 0, 
#                              sd   = 3)
#     RFmodel = randomForest(x = df,
#                            y = Y)
#     
#     print(df)
#     print(RFmodel$importance)
#    ResultsList <- list(VIPCalcTime = CalculateAllVints(RFmodel,df),
#                        ALECalcTime = CalculateALEExecutionTime(RFmodel,df))
#    return(ResultsList)
#    })
#   
#   CompletionTimesn = restruct_list_of_lists(CompletionTimesn)
#   CompletionTimesn = do.call(cbind,CompletionTimesn) %>% 
#     as.data.frame()
#   rownames(CompletionTimesn) = nlist
#   colnames(df) = c("VIP","ALEs")
#   
#   return(CompletionTimesn)
#    # Store results
# })
# 
# CompletionTimestest = lapply(CompletionTimesp,function(df){
#   rownames(df) = nlist
#   colnames(df) = c("VIP","ALEs")
#   return(df)
# })
# 
#     
# CompletionTimesdf = do.call(cbind,CompletionTimes) %>% as.data.frame()
# 


# 
# names(df) = paste0("V",
#                        seq(1,ncol(df),1))
#     
#     Y = rowSums(df) + rnorm(n,
#                             mean = 0, 
#                             sd   = 3)
#     
#     RFmodel = randomForest(x = df,
#                            y = Y)
#     
#     pred.fun  <- function(X.model,
#                           newdata){
#       Predictions = predict(RFmodel,
#                             newdata)
#       return(Predictions)}
#     
#     ALEVarInteractions <- find_ALE_interacts(X        = df,
#                                              X.MODEL  = RFmodel,
#                                              pred.fun = pred.fun,
#                                              K        = 40) %>% 
#       interacts2_heatmap()
#   
  
    # simulate data
  
  
  # fit model
  
  # start runtime or whatever
  # calculate variable importance
  # end runtime
  
}


################## GOGOGOG
CalcCompletionTimes = function(nlist,p){
  
  CompletionTimesn = lapply(nlist, function(n){ print(n)
    
    
    df = PrepData(p,n)
    
    Y  = rowSums(df) + rnorm(n,
                             mean = 0, 
                             sd   = 3)
    RFmodel = randomForest(x = df,
                           y = Y)
    
    
    ResultsList <- list(VIPCalcTime = CalculateAllVints(RFmodel,df),
                        ALECalcTime = CalculateALEExecutionTime(RFmodel,df))
    return(ResultsList)
  })





CompletionTimesn = restruct_list_of_lists(CompletionTimesn)
CompletionTimesn = do.call(cbind,CompletionTimesn) %>% 
  as.data.frame()
rownames(CompletionTimesn) = nlist
colnames(df) = c("VIP","ALEs")

return(CompletionTimesn)}


nlist = list(100,500,1000)
p = 2
p2 = CalcCompletionTimes(nlist,2)
p = 3
p5 = CalcCompletionTimes(nlist,5)
p = 10
p10 = CalcCompletionTimes(nlist,10)
p = 20
p20 = CalcCompletionTimes(nlist)

p2test = p10
colnames(p10) = c("VIP","ALE")
p2test = p2test %>% 
   rownames_to_column() %>% 
  tidyr::pivot_longer(cols = c(VIP,ALE))  %>% 
  mutate( value = as.numeric(value)) %>% 
  mutate( rowname = as.numeric(rowname)) 

  ggplot(p2test,aes(x = rowname, y = value, color = name)) + geom_point(size = 5) +
    geom_line(size = 2) + 
    labs(y = "Runtime (Seconds)",
         x = "Sample N") + theme_classic() + theme(axis.text = element_text(size = 20),
                                                   axis.title= element_text(size = 20))
    facet.themes

p5n100ResultsList = ResultsList





bench::mark(CalculateAllVints

p = 5
n = 100

resultsdf <- 

vipMem<- bench::mark(CalculateAllVints(RFmodel,df),memory = T,
                     min_iterations = 3)
VIPinfop5n100 <- list(runtime = vipMem$median, 
                      memuse =  vipMem$mem_alloc,
                      mempersec = as.numeric(vipMem$mem_alloc) / as.numeric(vipMem$median))
ALEMem <- bench::mark(CalculateALEExecutionTime(RFmodel,df),memory = T,
                      min_iterations = 3)
ALEinfop5n100 <- list(runtime = ALEMem$median, 
                      memuse =  ALEMem$mem_alloc,
                      mempersec = as.numeric(ALEMem$mem_alloc) / as.numeric(ALEMem$median))
as.numeric(ALEMem$mem_alloc) / as.numeric(ALEMem$median)

#Simulate_Data