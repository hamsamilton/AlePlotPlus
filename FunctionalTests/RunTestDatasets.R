libraries <- c("ALEPlotPlus","ALEPlot","dplyr","vip","MASS",'viridis',
                          "randomForest","mlbench","stringr","ggplot2",'gridExtra')
lapply(libraries, require, character.only = TRUE)

ParentDir = "~/TestALEs/"
dir.create(ParentDir)

TestDataList = list(Boston = SetupBoston(),
                    Pimi   = SetupPimi(),
                    MTCars = SetupMtcars())

for (TestDataName in names(TestDataList)){
  
  DatasetDir = paste0(ParentDir,
                      TestDataName)
  dir.create(DatasetDir)

  TestData = TestDataList[[TestDataName]]
  

  
  ALEInteractions = find_ALE_interacts(X        = TestData$X,
                                       X.MODEL  = TestData$X.MODEL,
                                       pred.fun = TestData$pred.fun,
                                       K        = 20)
  
  InteractionHeatmap = ALEInteractions %>% 
    ALEPlotPlus::interacts2_heatmap()
  
  pheatmap::pheatmap(InteractionHeatmap,
                     cluster_rows = F,
                     cluster_cols = F,
                     inferno(100),
                     filename = paste0(DatasetDir,
                                       "/InteractionHeatmap.pdf"))
  
  # Plot individual ALE interactions
  create_2D_ALEs(X        = TestData$X, 
                 X.MODEL  = TestData$X.MODEL,
                 pred.fun = TestData$pred.fun, 
                 K        = 20,
                 savedir  = DatasetDir)
  
  #1D plts
  ALEPlts = mkALEplots(X        = TestData$X,
                       X.MODEL  = TestData$X.MODEL,
                       pred.fun = TestData$pred.fun, 
                       K        = 20)
  
  ggsave(filename = str_c(DatasetDir,
                          "/1DALEplots.pdf"),
         plot     = marrangeGrob(ALEPlts, 
                                 nrow = 2, 
                                 ncol = 2),
         width    = 15,
         height   = 9)
}
  


ALEplt = ALEPlot::PDPlot(X,
                          X.model=model,
                          pred.fun=pred_fun,
                          J = c(2,6),
                          K = K)


dir.create(savedir)

# get all combos
var_combs = combn(colnames(X),m = 2,simplify=T)

# gen ALEplot
int_estimates = lapply(1:ncol(var_combs),function(i){
  print(i/ncol(var_combs))
  x_ind = colnames(X)[var_combs[1,i]]
  y_ind = colnames(X)[var_combs[2,i]]

  val = vint(model,feature_names=c("mass","age"))
  return(val)})
all_pairs <- combn(paste0("x.", 1:10), m = 2)
res <- NULL
for (i in seq_along(var_combs)) {
  print(i/ncol(var_combs))
  print(var_combs[, i])
  interact <- vint(rf, feature_names = var_combs[, i])
  res <- rbind(res, interact)
}
res
res2 = split_column(res,"Variables","\\*","var1","var2")

newnms = str_split(res$Variables,"\\*",simplify=T)

resdf = data.frame(var1 = newnms[,1],var2 = newnms[,2],zscore = res$Interaction)
adj = create_adjacency_matrix(resdf)
diag(adj) = NA
pheatmap::pheatmap(adj,cluster_rows=F,cluster_cols=F,
                   rev(samthesis::QckRBrwrPllt("Spectral",100)))
# Calculate row sums
row_sums <- rowSums(adj)

# Normalize the matrix by row sums
normalized_matrix <- t(apply(adj, 1, function(x) x / row_sums))

col_sums <- rowSums(normalized_matrix)

normalized_matrix <- t(apply(normalized_matrix, 2, function(x) x / col_sums))

diag(normalized_matrix) = NA
pheatmap::pheatmap(normalized_matrix,cluster_rows=F,cluster_cols=F,
                   rev(samthesis::QckRBrwrPllt("Spectral",100)))

vint(model,feature_names=c("mass","age"))
