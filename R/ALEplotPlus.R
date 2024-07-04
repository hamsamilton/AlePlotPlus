# to install setwd("/Users/samhamilton/Library/Mobile Documents/com~apple~CloudDocs/Thesis_Aim1/libraries/ALEPlotPlus")
# devtools::document() ; devtools::build() ; devtools::install()

#' @import tidyverse
#' @import pheatmap
#' @import randomForest
#' @import RColorBrewer

#' @export
dplyr::`%>%`

#'find_ALE_interacts
#'
#'Return a dataframe of the estimate interactions between predictors in an MLModel
#'using their ALEPlots
#'
#'@param X the dataframe of predictors used to train model
#'@param X.MODEL the machine learning model use
#'@param pred.fun the prediction function used with ALEPlot, see ALEPlot for details
#'@param K the number of bins in which to assess interactions, see ALEPlot for detaal
#'@return a datagrame estimated interaction strengths. Estimated by multiplying each
#'        point by its estimated interaction effect
#'@export
find_ALE_interacts = function(X,X.MODEL,pred.fun,K = 40){

  # get all combos
  var_combs = combn(1:ncol(X),m = 2,simplify=T)

  # gen ALEplot
  int_estimates = lapply(1:ncol(var_combs),function(i){
    print(i)
    int_estimate = find_ALE_interact(X        = X,
                                     X.MODEL  = X.MODEL,
                                     pred.fun = pred.fun,
                                     int_vars = var_combs[,i],
                                     K        = K)
    print(str(int_estimate)) ; print('int_estimate')
    
    int_estimate = data.frame(x   = colnames(X)[var_combs[1,i]],
                              y   = colnames(X)[var_combs[2,i]],
                              val = int_estimate)
    return(int_estimate)})

  int_estimates = do.call("rbind",
                          int_estimates)
  return(int_estimates)}

#'find_ALE_interact
#'
#'Return the average estimated effect of an interaction between preds in an ML
#'model
#'@param X the dataframe of predictors used to train model
#'@param model the machine learning model use
#'@param pred.fun the prediction function used with ALEPlot, see ALEPlot for details
#'@param int_vars a list of the names of the two interacting variables of interest
#'@param K the number of bins in which to assess interactions, see ALEPlot for detail
#'@return a value representing the average estimated effect size of an interaction
#'@export
find_ALE_interact = function(X,X.MODEL,pred.fun,int_vars,K = 40){

  ALEplt = ALEPlot(X,
                   X.model  = X.MODEL,
                   pred.fun = pred.fun,
                   J        = int_vars,
                   K        = K)
  
  x_vec = ALEplt$x.values[[1]]
  y_vec = ALEplt$x.values[[2]]
  z_mat = ALEplt$f.values

  x_vals = X[,int_vars[1]]
  y_vals = X[,int_vars[2]]

  int_estimate = calc_vals(x_points = x_vals,
                   y_points         = y_vals,
                   x_vec            = x_vec,
                   y_vec            = y_vec,
                   z_vec            = z_mat)
  
  print(int_vars)
  print(str(int_estimate))
  

  return(int_estimate)}

#'calc_vals
#'
#'given the X and Y coordinates of the two predictors used to generate an ALEPlot
#'and the ALEPlot info, calculate the interaction effect for each point for the
#'input dataset
#'@param x_points the positions of predictor 1
#'@param y_points the positions of predictors2
#'@param x_vec the vector of x_positions produced by ALEPlot
#'@param y_vec the vector of y_positions produced by ALEPlot
#'@param y_vec the matrix of z positions produced by ALEPlot
#'@return a vector of estimated interaction intensities for each point
#'@export
calc_vals = function(x_points,y_points,x_vec,y_vec,z_vec){
  
 # print('x_vecstrb4') ; print(str(x_vec))
  x_points = x_points %>% as.numeric()
  y_points = y_points %>% as.numeric()
 # x_vec = x_vec %>% as.factor() %>% as.numeric()
 # y_vec = y_vec %>% as.factor() %>% as.numeric()
  
  # print('y_points') ; print(y_points)
  # print('x_points') ; print(x_points)
  # print('x_vec')    ; print(x_vec) 
  # print('y_vec')    ; print(y_vec)
  # print('z_vec')    ; print(z_vec)
  

  # Get points vector length
  num_points = length(x_points)

  z_values = sapply(1:num_points,function(i){
    x_pos = sum(x_points[[i]] >= x_vec)
    y_pos = sum(y_points[[i]] >= y_vec)

    z_val = z_vec[x_pos,y_pos]

    return(z_val)})

  z_values = unlist(z_values)
  return(z_values)
}

#' Format a number to three significant figures
#'
#' This function formats a number to three significant figures.
#' It handles both small and large numbers appropriately.
#'
#' @param x A numeric value to be formatted.
#' @return A character string representing the formatted number.
#' @keywords internal
format_sigfig <- function(x) {
  if (x == 0) {
    return("0")
  }
  exponent = floor(log10(abs(x)))
  if (exponent >= 0 && exponent < 3) {
    return(sprintf("%.2f", x))
  }
  formatted = sprintf("%.2e", x)
  gsub("0*e\\+00$", "", formatted)  # Remove trailing zeros and +00 in the exponent if present
}

#' color.bar
#'
#' Plot a Color Bar
#'
#' This function plots a color bar with a given color scale and axis labels.
#'
#' @param lut A color palette (a vector of color values) used to create the color bar.
#' @param min The minimum value of the scale, placed at the bottom of the color bar.
#' @param max The maximum value of the scale, placed at the top of the color bar (default is -min).
#' @param nticks The number of tick marks on the color bar (default is 4).
#' @param ticks A numeric vector of positions for tick marks on the color bar (default is `seq(min, max, len = nticks)`).
#' @param title A title for the color bar (default is an empty string).
#' @keywords internal
#' 
#' @return Invisible NULL. The function is called for its side effect: producing a plot.
#'
#' @examples
#' color.bar(colorRampPalette(c("light green", "yellow", "orange", "red"))(100), -1)
#'
#' @export

color.bar <- function(lut, min, max=-min, nticks=4, ticks=seq(min, max, len=nticks), title='', label="Effect on Prediction") {
  scale = (length(lut)-1)/(max-min)

  formatted_ticks = sapply(ticks, format_sigfig)

  plot(c(0,10), 
       c(min,max), 
       type = 'n', 
       bty  = 'n', 
       xaxt = 'n', 
       xlab = '', 
       yaxt = 'n',
       ylab = '', 
       main = title)
  
  axis(2, 
       at = ticks, 
       labels = formatted_ticks, 
       las    = 2)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0, y, 10, y+1/scale, col=lut[i], border=NA)
  }

  # Add label if provided
  if (!is.null(label)) {
    text(12, (min + max)/2, label, cex=1.2, srt=90, adj=c(.55, 1),xpd = NA)
  }
}

#'create_2D_ALEs
#'
#'Produces and saves ALEPlots but with better colors and plotted values which
#'overall makes it significantly more legible
#'
#'@param X a dataframe with the same structure as that used to train the model
#'@param X.MODEL a model you are interrogating
#'@param pred.fun the prediction function used to generate ALEPlot...see doc there
#'@param K how many subdivisions to estimate the int coef. for, see ALEPlot
#'@param savedir where to save all the ALEPlots generated
#'@return saves all combinations of plots
#'@export
create_2D_ALEs = function(X,X.MODEL,pred.fun,K = 40,savedir = 'ALE2DPlots'){

  dir.create(savedir,showWarnings = F)

  # get all combos
  var_combs = combn(1:ncol(X),m = 2,simplify=T)

  # gen ALEplot
  int_estimates = lapply(1:ncol(var_combs),function(i){

    x_ind = var_combs[1,i]
    y_ind = var_combs[2,i]

    ALEplt = ALEPlot(X,
                     X.model  = X.MODEL,
                     pred.fun = pred.fun,
                     J        = c(x_ind,
                                  y_ind),
                     K        = K)

    x_vec = ALEplt$x.values[[1]]
    y_vec = ALEplt$x.values[[2]]
    z_mat = ALEplt$f.values

    jpeg(filename = paste0(savedir,
                           '/',
                           str_safe(colnames(X)[x_ind]),
                           str_safe(colnames(X)[y_ind]),
                           ".jpeg"),
         height   = 6,
         width    = 6,
         units    = "in",
         res      = 720)

    clrs = mk.colors_ale(mtrx = z_mat)$clrs
    brks = mk.colors_ale(mtrx = z_mat)$brks

# combine plots with 1 row and 2 columns
# Define the layout matrix
 mat <- matrix(c(1,2,1,2), 
               2,
               2,
               byrow = TRUE)

    # Specify the layout with relative column widths
 layout(mat, 
        widths=c(4,1))

 image(x_vec,
       y_vec,
       z_mat,
       xlab     = colnames(X)[x_ind],
       ylab     = colnames(X)[y_ind],
       xlim     = range(x_vec),
       ylim     = range(y_vec),
       col      = clrs,
       breaks   = brks,
       cex.axis = 1.5,  # Increase the size of axis tick labels
       cex.lab  = 1.5)   # Increase the size of axis labels
 
 contour(x_vec, 
         y_vec, 
         z_mat, 
         add        = TRUE, 
         drawlabels = TRUE)

 points(X[,x_ind],
        X[,y_ind])

 color.bar(clrs, 
           min = min(brks),
           max = max(brks))


 print(paste0(savedir,
              '/',
              str_safe(colnames(X)[x_ind]),
              str_safe(colnames(X)[y_ind]),
              ".jpeg"))

    dev.off()
  })
}

#'mk.color_ale
#'
#'makes_colors for the aleplots
#'@param nclrs how many colors
#'@param clr_st which color set, divergent preferred
#'@param even  whether to set  the middle color to 0
#'@param mtrx  whether to set  the middle color to 0
#'@param logd  whether color scale should be logged
#'@return a list of the color codes, and their accompanying breaks
#'@keywords internal
mk.colors_ale <- function(nclrs = 30, clr_st = "Spectral",even = T, mtrx,logd = F) {

  # get the limits of the heatmap values
  mx.psvl <- max(mtrx,na.rm = T)
  mx.ngvl <- min(mtrx,na.rm = T)
  if(even){ mx.ngvl = abs(mx.ngvl)}
  mxvl <- max(mx.psvl,mx.ngvl)

  # Make the colors
  clrs.4plt <- RColorBrewer::brewer.pal(9,clr_st)
  clr.rmp   <- colorRampPalette(clrs.4plt)
  clrs.4plt <- clr.rmp(nclrs)
  clrs.4plt <- rev(clrs.4plt)


  # Make the breaks
  if(even){
    if(logd){
      brks = exp(log(10)*seq(log10(-mxvl),
                             log10(mxvl),
                             length.out = nclrs + 1))}
    else{
      brks = seq(-mxvl,
                 mxvl,
                 length.out = nclrs + 1)}
    nblw <- sum(brks < - mx.ngvl)
    clrs.4plt <- clrs.4plt[nblw:length(clrs.4plt)]
    nbrks <- brks[nblw:length(brks)]} else{
      if(logd){
        nbrks = exp(log(10)*seq(log10(mx.ngvl),
                                log10(mxvl),
                                length.out = nclrs + 1))}
      else{nbrks = seq(mx.ngvl,
                       mxvl,
                       length.out = nclrs + 1)}
    }

  otpts <- list(clrs = clrs.4plt, brks = nbrks)
  return (otpts)}

#' create_adjacency_matrix
#'
#' Create an adjacency matrix from a data frame with variable combinations and z-scores
#'
#'
#' @param Xvec a vector with the X values
#' @param Yvec a vector with the Y values
#' @param vals values to fill the vector with
#' @return An adjacency matrix
#' @export
#' @examples
#' data <- data.frame(va1 = c("A", "A", "B", "C"),
#'                    var2 = c("B", "C", "C", "D"),
#'                    zscore = c(1.5, 2.0, -0.5, 1.0))
#'
#' adj_matrix <- create_adjacency_matrix(data)
#' print(adj_matrix)
create_adjacency_matrix <- function(Xvec,Yvec,vals) {

  # Create data.frame
  data <- data.frame(X    = Xvec, 
                     Y    = Yvec, 
                     vals = vals)

  # Create a unique list of all variable names
  variables <- unique(c(Xvec, 
                        Yvec))
  # Create an empty adjacency matrix
  adj_matrix <- matrix(0,
                       nrow     = length(variables),
                       ncol     = length(variables),
                       dimnames = list(variables,
                                       variables))

  # Fill the adjacency matrix with z-scores
  for (i in 1:nrow(data)) {
    row_name <- data$X[i]
    col_name <- data$Y[i]
    val      <- data$vals[i]

    adj_matrix[row_name, col_name] <- val
  }

  # Symmetrize the matrix
  adj_matrix <- (adj_matrix + t(adj_matrix))

  return(adj_matrix)
}

#' interacts2_heatmap
#'
#' summarizes the information from the findALEinteractions
#' function into a format compatible with pheatmap
#' @param ALE_interacts the output of findALEinteracts
#' @return a matrix compatible with pheatmap
#' @export
interacts2_heatmap = function(ALE_interacts){

  ALE_interacts = ALE_interacts %>%
    group_by(x,y) %>%
  dplyr::summarize(mn = mean(abs(val)))
ALE_adj = create_adjacency_matrix(Xvec = ALE_interacts$x,
                                  Yvec = ALE_interacts$y,
                                  vals = ALE_interacts$mn)
diag(ALE_adj) = NA

return(ALE_adj)}

#' interacts2_heatmap_AUROC
#'
#' summarizes the information from the findALEinteractions
#' function into a format compatible with pheatmap.
#' A variant that measures importance as the difference in AUC when adjusted
#' for the ALEPlot interaction estimates. THIS FUNCTION IS IN BETA, may or may not work
#' Approach is experimental and not validated, not public atm.
#' between
#' @param ALE_interacts the output of findALEinteracts
#' @param predictions a vector of predictions for the original AUC
#' @param truevals the true values for calculating AUC
#' @return a matrix compatible with pheatmap
#' 
interacts2_heatmap_AUROC = function(ALE_interacts,
                                    predictions,
                                    truevals){

  oldAUC = pltROC(predictions,truevals)$AUC

  ALE_interacts = ALE_interacts %>%
    group_by(x,y) %>%
    dplyr::mutate(val = predictions - val) %>%
    dplyr::summarise(val = oldAUC - pltROC(val,truevals)$AUC)
  ALE_adj = create_adjacency_matrix(Xvec = ALE_interacts$x,
                                    Yvec = ALE_interacts$y,
                                    vals = ALE_interacts$val)
  diag(ALE_adj) = NA
  return(ALE_adj)}

#' ALEPlots are a method to visualize the relationship between individual predictors and an outcome within ordinarily
#' difficult to interpret non-linear machine learning models. This function produces a list of plots with an aleplot
#' for each predictor within an ML model.
#' @param X the data used to train the model
#' @param X,MODEL the model
#' @param pred.fun the prediction function required by ALEPLOT package see details
#' @param K the number of bins split to evaluate the ALE Plot see ALEPLOT package
#' @return a list of ALEplots ordered by variable importance.
#' @export
mkALEplots <- function(X,X.MODEL,K = 40,pred.fun){

    # Apply over columns
  ALEPLOTS <- lapply(colnames(X),function(nm){

    # Produce the ALEPlot Data
    ALEDF <- ALEPlot(X, 
                     X.MODEL, 
                     pred.fun, 
                     nm, 
                     K) %>%
      as.data.frame()

    # Estimate the effects at each point in X, given ALE estimates
    est_effect_df = calc_vals_1D(X[,nm],
                                 ALEDF$x.values,
                                 ALEDF$f.values) %>%
      data.frame(x   = X[,nm],
                 val = .)
    # Produce the ALE Plot
    
    if(is.numeric(ALEDF$x.values)){
    
    ALE_OUT <- ggplot(ALEDF,
                      aes(x = x.values,
                          y = f.values)) +
      geom_line(linewidth = 1.3) +
      geom_rug(data  = est_effect_df, 
               aes(x = x, 
                   y = NULL), 
               sides = "b", 
               color = "red") +
      labs(x = nm,
           y = "Est. Effect on Prediction") + 
      facet.themes()}
    
    if(is.character(ALEDF$x.values)){
      
      ALE_OUT = ggplot(ALEDF,
                       aes(x = x.values,
                           y = f.values)) +
        geom_bar(stat = 'identity') +
        labs(x = nm,
             y = 'Est. Effect on Prediction') +
        facet.themes()}
      
    return(ALE_OUT)
  })
  
  return(ALEPLOTS)
}


#'calc_vals_1D
#'
#'given the X coordinates of the two predictors used to generate an ALEPlot
#'and the ALEPlot info, calculate the est effect for each point for the
#'input dataset
#'@param x_points the positions of predictor 1
#'@param x_vec the vector of x_positions produced by ALEPlot
#'@param z_vec the matrix of z positions produced by ALEPlot
#'@return a vector of estimated effect intensities for each point
#'@export
calc_vals_1D = function(x_points,x_vec,z_vec){

    if(is.character(x_vec)){
      z_values = sapply(1:length(x_points),function(i){
        z_val <- z_vec[x_vec == x_points[[i]]]
        return(z_val)
        })}
        
        
      
    if(is.numeric(x_vec)){
    #interpolate values for greater accuracy
    interp = approx(x_vec,z_vec,xout = seq(min(x_vec),
                                           max(x_vec),
                                           length.out = 200))
  
    z_values = sapply(1:length(x_points),function(i){
      x_pos = sum(x_points[[i]] >= interp$x)
      z_val = interp$y[x_pos]
  
      return(z_val)})}
    
  return(z_values)
}

#'calc_ALE_varimp
#'
#'An internal helper function designed to assist in calculating variable importance scores
#'using the ALEPlot method.
#'The method works as such. Use the ALE method to estimate the effect on prediction
#'for each datapoint. Other functions summarize this data matrix in various ways
#'to produce variable importance scores of different flavors
#'@param X the data used to train the model, without predicted var
#'@param X.MODEL the model
#'@param pred.fun the prediction function required by ALEPLOT package see details
#'@param K the number of bins split to evaluate the ALE Plot see ALEPLOT package
#'@return a matrix of estimated effects for each point, for each predictor
#'@export
calc_ALE_varimp_df = function(X,X.MODEL,K = 40,pred.fun){

  imp_scores <- lapply(colnames(X),function(nm){
    print(nm)
    ALEDF <- ALEPlot(X, 
                     X.MODEL,
                     pred.fun,
                     nm,
                     K) %>% 
      as.data.frame()
    
    imp_score = calc_vals_1D(x_points = X[,nm],
                             x_vec    = ALEDF$x.values,
                             z_vec    = ALEDF$f.values) %>%
    return(imp_score)
  })
  imp_score_mat = do.call(cbind,imp_scores)
  dimnames(imp_score_mat) <- list(rownames(X),colnames(X))

  return(imp_score_mat)
}

#'calc_ALE_varimps_mean
#'
#' Generates variable importance scores for 1D ALEPlots. takes the absolute value of the mean
#'
#'@param X the data used to train the model, without predicted var
#'@param X.MODEL the model
#'@param pred.fun the prediction function required by ALEPLOT package see details
#'@param K the number of bins split to evaluate the ALE Plot see ALEPLOT package
calc_ALE_varimps_mean <- function(X,X.MODEL,pred.fun,K = 40){

  imp_score_avg <- calc_ALE_varimp_df(X,X.MODEL,K,pred.fun) %>% 
    abs() %>%
    colMeans()  %>%
    as.vector()

  names(imp_score_avg) = colnames(X)

  return(imp_score_avg)
}

#'calc_ALE_varimps_AUROC
#'
#'Generates variable importance scores for 1D ALEPlots. This variant is designed
#'to estimate the effect on accuracy of the prediction by adjusting the model predictions
#'by the estimated effects, then comparing the differences in AUROC to understand
#'how much worse the model would be if the variable were factored out.
#'This approach has not been validated yet and so is not made public facing.
#'
#'@param X the data used to train the model, without predicted var
#'@param X.MODEL the model
#'@param pred.fun the prediction function required by ALEPLOT package see details
#'@param K the number of bins split to evaluate the ALE Plot see ALEPLOT package
#'@param predictions vector of model predictions
#'@param truevals a vector of true model predictions
calc_ALE_varimps_AUROC <- function(X,X.MODEL,pred.fun,K = 40,predictions,truevals){

  # calculate variable importance effect matrix
  imp_score_mat <- calc_ALE_varimp_df(X,X.MODEL,K,pred.fun)

  # make the adjusted prection matrix by subtracting the matrix from the column
  adj_imp_score_mat <- predictions - imp_score_mat

  # calculate predicted AUROC
  orig_AUROC <- pltROC(predictions,truevals)$AUC

  deltAUROCs = sapply(colnames(adj_imp_score_mat),function(predictor){

    delt = orig_AUROC - pltROC(adj_imp_score_mat[,predictor],
                               truevals)$AUC
    return(delt)
  })
  return(deltAUROCs)}

#'calc_ALE_varimps_ACC
#'
#'Generates variable importance scores for 1D ALEPlots. This variant is designed
#'to estimate the effect on accuracy of the prediction by adjusting the model predictions
#'by the estimated effects, then comparing the differences in AUROC to understand
#'how much worse the model would be if the variable were factored out.
#'THIS APPROACH IS IN BETA. May or may not work for you.
#'@param X the data used to train the model, without predicted var
#'@param X.MODEL the model
#'@param pred.fun the prediction function required by ALEPLOT package see details
#'@param K the number of bins split to evaluate the ALE Plot see ALEPLOT package
#'@param predictions vector of model predictions
#'@param truevals a vector of true model predictions
calc_ALE_varimps_AUROC <- function(X,X.MODEL,pred.fun,K = 40,predictions,truevals){
  
  # calculate variable importance effect matrix
  imp_score_mat <- calc_ALE_varimp_df(X,X.MODEL,K,pred.fun)
  
  # make the adjusted prection matrix by subtracting the matrix from the column
  adj_imp_score_mat <- predictions - imp_score_mat
  
  # calculate the accuracy of the fitted model
  orig_ACC <-  abs(predictions - truevals)
  
  deltAUROCs = sapply(colnames(adj_imp_score_mat),function(predictor){
    
    delt = abs(adj_imp_score_mat[,predictor] - truevals)
    
    return(delt)
  })
  return(deltAUROCs)}
       

#' a theme for faceted plots which does not include a legend for nice tiling.
#' @export
facet.themes <- function() {
  theme_classic() +  
  theme(axis.title      = element_text(size  = 20,
                                         color = "black"),
        axis.text       = element_text(size  = 20,
                                         color = "black"),
        legend.position = "none")}

#'str_safe
#'
#'This function replaces special symbols with close approximations that dont mess up file systems
#'@param strng string to make safe
#'@return a string with the dangerous parts replaced but still looks very similar to a human reader
#'@export
str_safe = function(strng){
  strng = str_replace_all(strng,"%","％")
  strng = str_replace_all(strng,"/","∕")
  return(strng)}

#'QckRBrwrPllt
#'
#'This is a quick function that combines two RColorBrewer functions into one that
#'makes it more intuitive to use and setup
#'@param name the Rbrewer pallette name
#'@param n how many colors you want
#'@return a color vector
#'@export
QckRBrwrPllt <- function(name,n) {
  plt <- colorRampPalette(brewer.pal(8,name))(n)
  return(plt)
}

#' pltROC
#'
#' Plots an ROC with an AUC with all the points labeled and an x / y abline
#' @param predictions predictions for the ROC as a probability 0 to 1
#' @param truevals binary factor make sure levels are aligned properly
#' @return a list with the plot and the auc info
#' @export
pltROC <- function(predictions,truevals){

  roc_obj_all <- roc(predictions,response = truevals)

  roc_points_all = data.frame(sensitivities   =  roc_obj_all$sensitivities,
                              specificities   =  roc_obj_all$specificities)

  rcplt <- ggroc(roc_obj_all,size = 1.25) +
    geom_abline(slope    = 1,
                intercept= 1,
                linetype = 'dashed',
                color    = "red") +
    geom_point(data = roc_points_all,aes(x = specificities,
                                         y = sensitivities)) +
    annotate("text",
             x     = .25,
             y     = .25,
             size  = 10,
             label = str_c("AUC = ",
                           round(roc_obj_all$auc[1],
                                 2))) +
    scale_x_continuous(expand = expansion(mult = c(.05,.1)),
                           trans  = "reverse") +
    xlab("1 - Specificity") + ylab("Sensitivity") +
    facet.themes() +
    theme(axis.text = element_text(size = 30),
          axis.title= element_text(size = 30))

  outputlist = list(ROCplot = rcplt,AUC = roc_obj_all$auc[1])
  return(outputlist)
}

#' ALEPlot
#'
#' Main function for generating ALEPlots and ALEDFs
#' @param X The data frame of predictor variables to which the supervised learning model was fit. 
#' The names of the predictor variables must be the same as when the model was fit. 
#' The response variable should not be included in X.
#' @param X.model The fitted supervised learning model object (e.g., a tree, random forest, neural network, etc.), 
#' typically an object to which a built-in predict command associated with that object can be applied.
#' @param pred.fun user-supplied function that will be used to predict the response for X.model
#'  for some specified inputs. pred.fun has two arguments. The first argument is 
#'  named X.model and must be the same object as the X.model argument to the 
#'  ALEPlot function. The second argument is named newdata and is a data frame of
#'   predictor values at which the object X.model is to be predicted. 
#'   The output of pred.fun must be a numeric vector of predictions having length
#'    equal to the number of rows of newdata. For most X.model objects, 
#'    pred.fun can simply call the predict function that was written as part of 
#'    that modeling object package, assuming the package contains a predict function. 
#'    An example of where a more customized pred.fun would be used is a multi (> 2) 
#'    class classification problem for which the built-in predict function returns 
#'    a vector of predicted probabilities, one for each response class. 
#'    In this case it may make sense to have pred.fun return the predicted probabilities
#'     (or its log-odds, etc.) for one particular class of interest.
#' @param J A numeric scalar or two-length vector of indices of the predictors 
#' for which the ALE plot will be calculated. J is either a single index
#'  (for a main effects plot) or a pair of indices (for a second-order interaction plot).
#'   For a single index, the corresponding predictor must be either numeric or a factor. 
#'   For a pair of indices, the corresponding predictors must be either both 
#'   numeric or the first a factor and the second numeric.
#'  @param K A numeric scalar that specifies the number of intervals into which 
#'the predictor range is divided when calculating the ALE plot effects.
#'If length(J) = 2, the same K will be used for both predictors,
#' resulting in an array of K^2 cells over the two-dimensional predictor space.
#'Note that the algorithm may adjust (reduce) K internally if the predictors 
#'are discrete and have many repeated values. K is only used if the predictor is numeric. 
#'For factor predictors, the equivalent of K is the number of used levels of
#' @return a list with the plot and the auc info
#' @export
ALEPlot = function (X, X.model, pred.fun, J, K = 40) {
  
  # Handle reordering factors so they can be jittered to assess changes.
  handle_factors <- function(X, var_name, d, K){
    
    # Reorders factor levels by MDS so ALEs can 'Jitter' between them to calculate
    reorder_levels_by_MDS <- function(D.cum, X, var_name){
      print(var_name)
      #MDS
      D1D <- cmdscale(D.cum,
                      k = 1)
      
      # Get ordering indices
      ind.ord <- sort(D1D,
                      index.return = T)$ix
      ord.ind <- sort(ind.ord,
                      index.return = T)$ix
      
      # Order levels based on sorted indices
      lev.ord <- levels(X[, var_name])[ind.ord]
      
      # Order factor values based on revrse ordering
      x.ord <- ord.ind[as.numeric(X[, var_name])]
      
      return(list(ordered_levels = lev.ord, ordered_values = x.ord, ind.ord = ind.ord))}
    
    # calculate the binwidth distances for jittering values
    calc_distances <- function(X, var_name, x.count, d, K){
      D.cum <- matrix(0,K,K)
      D <- D.cum
      
      for(j in setdiff(1:d, var_name)){
        if (class(X[,var_name]) == 'factor') { # numeric variable distanc calculation
          #print(j)
          
          A = table(X[,var_name], X[, j]) / x.count
          
          for(i in 1:(K - 1)){
         #   print('i') ; print(i)
            for (k in (i + 1):K) {
            #  print('k') ; print(k)
              D[i, k] = sum(abs(A[i, ] - A[k, ])) / 2
              D[k, i] = D[i, k]  }}
          D.cum <- D.cum + D}
        else { # numeric variable distance calculation
          q.x.all <- quantile(X[, j],
                              probs = seq(0,
                                          1,
                                          length.out = 100),
                              na.rm = T,
                              names = F)
          x.ecdf = tapply(X[, j], 
                          X[, var_name],
                          ecdf)
          for(i in 1:(K - 1)){
            for(k in (i + 1):K){
              D[i, k] = max(abs(x.ecdf[[i]](q.x.all) - x.ecdf[[k]](q.x.all)))}}
          D.cum <- D.cum + D}}
      
      return(D.cum)}
    
    # Drop unused factor levels and calculate counts and probabilities
    X[var_name] <- droplevels(X[, var_name])
    x.count <- as.numeric(table(X[, var_name]))
    x.prob <- x.count / sum(x.count)
    K <- nlevels(X[, var_name])
    
    D.cum <- calc_distances(X, var_name, x.count, d, K)
    
    # Perform classical multidimensional scaling to order levels
    reorder_info <- reorder_levels_by_MDS(D.cum, X, var_name)
    return(list(x.prob         = x.prob, 
                ordered_levels = reorder_info$ordered_levels,
                ordered_values = reorder_info$ordered_values,
                ind.ord        = reorder_info$ind.ord))}
    
  # Calclulate the delta in the 2D Case given the 4 corners
  CalcDelta2D <- function(X11,X12,X21,X22,pred.fun){
    
    Corners = list(X11, X12, X21, X22)
    Y_HATS = lapply(Corners, function(corner){
      Y_Hat = pred.fun(X.model = X.model,
                       newdata = corner)
      return(Y_Hat)})
    Delta <- (Y_HATS[[4]] - Y_HATS[[3]]) - (Y_HATS[[2]] - Y_HATS[[1]])
    
    return(Delta)}
  
  # Function to discretize a numeric variable into quantile-based bins
  discretize_variable <- function(X, var_name, K) {
    # Compute the break points for the quantiles
    z <- c(min(X[, var_name]), as.numeric(quantile(X[, var_name],
                                               seq(1/K, 
                                                   1, 
                                                   length.out = K), 
                                               type = 1))) %>% 
      unique() # Ensure break points are unique
    
    # Determine the number of bins
    K <- length(z) - 1
    
    # Assign each value in the variable to a bin
    a <- as.numeric(cut(X[, var_name], 
                        breaks         = z, 
                        include.lowest = TRUE))
    
    return(list(break_points = z, num_bins = K, bin_assignments = a))
  }
        
  # Begin main function
  
  N = dim(X)[1]
  d = dim(X)[2]
  if (length(J) == 1) {
    if (class(X[, J]) == "factor") {
      
      # Perform classical multidimensional scaling to get 1D representation, used to 
      reorder_info <- handle_factors(X,J,d,K)
      
      x.prob = reorder_info$x.prob
      levs.ord = reorder_info$ordered_levels
      x.ord = reorder_info$ordered_values
      ind.ord = reorder_info$ind.ord
        
      row.ind.plus <- (1:N)[x.ord < K]
      row.ind.neg  <- (1:N)[x.ord > 1]
      
      # Create modified data for ALE calculations
      X.plus <- X ; X.neg  <- X
      
      X.plus[row.ind.plus, J] <- levs.ord[x.ord[row.ind.plus] + 1]
      X.neg[row.ind.neg, J] <- levs.ord[x.ord[row.ind.neg] - 1]
      
      y.hat      <- pred.fun(X.model = X.model, 
                             newdata = X)
      y.hat.plus <- pred.fun(X.model = X.model, 
                             newdata = X.plus[row.ind.plus,])
      y.hat.neg  <- pred.fun(X.model = X.model, 
                             newdata = X.neg[row.ind.neg,])
     # print('y.hat.plus') ; print(y.hat.plus) ; print('y.hat') ; print(y.hat) ; print('row.ind.plus') ; print(row.ind.plus)
      Delta.plus <- y.hat.plus - y.hat[row.ind.plus]
      Delta.neg <- y.hat[row.ind.neg] - y.hat.neg
      Delta <- as.numeric(tapply(c(Delta.plus, 
                                   Delta.neg), 
                                 c(x.ord[row.ind.plus], 
                                   x.ord[row.ind.neg] - 1),
                                 mean))
      fJ <- c(0, cumsum(Delta)) %>% 
        na.omit()
      fJ = fJ - sum(fJ * x.prob[ind.ord])
      x <- levs.ord}
    
    else if  (class(X[, J]) %in% c("numeric","integer")) {
      
      disc_info <- discretize_variable(X, J, K)
      
      z  = disc_info$break_points 
      K  = disc_info$num_bins
      a1 = disc_info$bin_assignments
      fJ = numeric(K)
      
      X1 = X ; X2 = X
      
      X1[, J] = z[a1]
      X2[, J] = z[a1 + 1]
    
      y.hat1 = pred.fun(X.model = X.model, 
                        newdata = X1)
      y.hat2 = pred.fun(X.model = X.model, 
                        newdata = X2)
      
      Delta = y.hat2 - y.hat1
      Delta = as.numeric(tapply(Delta,
                                a1,
                                mean))
      fJ = c(0,cumsum(Delta))
      b1 <- as.numeric(table(a1))
      
      fJ = fJ - sum((fJ[1:K] + fJ[2:(K + 1)])/2 * b1) / sum(b1)
      x <- z
      
    }
    else print("error:  class(X[,J]) must be either factor or numeric or integer")}
  
  else if (length(J) == 2) {
    
    if (class(X[, J[1]]) == c("factor") & class(X[, J[2]]) == c("factor")){
      print("error: both interacting values can't be factors, feature not yet supported")}
    
    if(class(X[, J[2]]) == 'factor'){
      print('swapping order')
      #Swap the order of the two variables
      new1 = J[[2]]
      J[[2]] = J[[1]]
      J[[1]] = new1}
      
    if (class(X[, J[1]]) == "factor"){
      
      reorder_info <- handle_factors(X,
                                     J[[1]],
                                     d,
                                     K)
      K1 <- nlevels(X[, J[[1]]])
      
      levs.ord = reorder_info$ordered_levels
      x.ord = reorder_info$ordered_values
        
      disc_info = discretize_variable(X,J[[2]],K)  
      
      z2 = disc_info$break_points 
      K2 = disc_info$num_bins
      a2 = disc_info$bin_assignments
      
      row.ind.plus <- (1:N)[x.ord < K1]
      
      X11 = X ; X12 = X ; X21 = X ; X22 = X
      
      X11[row.ind.plus, J[2]] = z2[a2][row.ind.plus]
      X12[row.ind.plus, J[2]] = z2[a2 + 1][row.ind.plus]
      X21[row.ind.plus, J[1]] = levs.ord[x.ord[row.ind.plus] + 1]
      X22[row.ind.plus, J[1]] = levs.ord[x.ord[row.ind.plus] + 1]
      X21[row.ind.plus, J[2]] = z2[a2][row.ind.plus]
      X22[row.ind.plus, J[2]] = z2[a2 + 1][row.ind.plus]
      
      Delta.plus = CalcDelta2D(X11[row.ind.plus,],
                               X12[row.ind.plus,],
                               X21[row.ind.plus,],
                               X22[row.ind.plus,],
                               pred.fun)
      
     # print('Delta.plus') ; print(str(Delta.plus)) ; print(head(Delta.plus))

      row.ind.neg <- (1:N)[x.ord > 1]
      
      X11 = X ; X12 = X ; X21 = X ; X22 = X
      
      X11[row.ind.neg, J[1]] = levs.ord[x.ord[row.ind.neg] - 1]
      X12[row.ind.neg, J[1]] = levs.ord[x.ord[row.ind.neg] - 1]
      X11[row.ind.neg, J[2]] = z2[a2][row.ind.neg]
      X12[row.ind.neg, J[2]] = z2[a2 + 1][row.ind.neg]
      X21[row.ind.neg, J[2]] = z2[a2][row.ind.neg]
      X22[row.ind.neg, J[2]] = z2[a2 + 1][row.ind.neg]
      
      Delta.neg  = CalcDelta2D(X11[row.ind.neg,],
                               X12[row.ind.neg,],
                               X21[row.ind.neg,],
                               X22[row.ind.neg,],
                               pred.fun)
      
   #   print('Delta.neg') ; print(str(Delta.neg)) ; print(head(Delta.neg))
      
      Delta = as.matrix(tapply(c(Delta.plus, 
                                 Delta.neg), 
                               list(c(x.ord[row.ind.plus], 
                                      x.ord[row.ind.neg] - 1), 
                                    a2[c(row.ind.plus, 
                                         row.ind.neg)]), 
                               mean))
      
   #   print('Delta') ; print(Delta)
      
      NA.Delta = is.na(Delta)
      
      NA.ind = which(NA.Delta, 
                     arr.ind = T, 
                     useNames = F)
      
      if (nrow(NA.ind) > 0) {
        notNA.ind = which(!NA.Delta, 
                          arr.ind  = T, 
                          useNames = F)
        range1 = K1 - 1
        range2 = max(z2) - min(z2)
        Z.NA = cbind(NA.ind[, 1] / range1, 
                     (z2[NA.ind[, 2]] + z2[NA.ind[, 2] + 1]) /2 / range2)
        
        Z.notNA = cbind(notNA.ind[, 1] / range1,
                        (z2[notNA.ind[, 2]] + z2[notNA.ind[, 2] + 1])/ 2/ range2)
        
        nbrs <- ann(Z.notNA, 
                    Z.NA, 
                    k       = 1, 
                    verbose = F)$knnIndexDist[,1]
        
        Delta[NA.ind] = Delta[matrix(notNA.ind[nbrs,],
                                     ncol = 2)]}
      
      # Why are we rewriting fJ, this is confusing the crap out of me
      fJ = matrix(0, 
                  K1 - 1, 
                  K2)
   #   print('fJ') ; print(fJ)
      fJ = apply(t(apply(Delta, 
                         1, 
                         cumsum)), 
                 2, 
                 cumsum)
 #     print('fJ1') ; print(fJ)
      fJ = rbind(rep(0, K2), fJ)
      fJ = cbind(rep(0, K1), fJ)
    #  print('fJ2') ; print(fJ)
      
      b = as.matrix(table(x.ord, a2))
      b2 = apply(b, 
                 2, 
                 sum)
      Delta = fJ[, 2:(K2 + 1)] - fJ[, 1:K2]
      b.Delta = b * Delta
      Delta.Ave = apply(b.Delta, 
                        2, 
                        sum) / b2
      fJ2 = c(0, 
              cumsum(Delta.Ave))
      
   #   print('fJ3') ; print(fJ2)
      
      b.ave = matrix((b[1:(K1 - 1), ] + b[2:K1, ]) / 2, 
                     K1 - 1,
                     K2)
      
      b1 = apply(b.ave,
                 1, 
                 sum)
      
      Delta = matrix(fJ[2:K1, ] - fJ[1:(K1 - 1), ], 
                     K1 - 1,
                     K2 + 1)
      
      b.Delta = matrix(b.ave * (Delta[, 1:K2] + Delta[, 2:(K2 + 1)]) / 2, 
                       K1 - 1, 
                       K2)
      
      Delta.Ave = apply(b.Delta,
                        1,
                        sum) / b1
      
      fJ1 = c(0, 
              cumsum(Delta.Ave))
      
      fJ = fJ - outer(fJ1, 
                      rep(1, 
                          K2 + 1)) - outer(rep(1,K1), 
                                           fJ2)
      
      fJ0 = sum(b * (fJ[, 1:K2] + fJ[, 2:(K2 + 1)]) / 2) / sum(b)
      fJ = fJ - fJ0
     
      levs.ord = levs.ord %>% 
        as.factor() %>% 
        as.numeric()
      
      x <- list(levs.ord, z2)
      K <- c(K1, K2)
      
      

    }
    else if (class(X[, J[1]]) %in% c("numeric", "integer")){
      
      z1 = c(min(X[, J[1]]), as.numeric(quantile(X[, J[1]], 
                                                 seq(1/K, 
                                                     1, 
                                                     length.out = K),
                                                 type = 1))) %>% 
        unique()
      K1 = length(z1) - 1
      a1 = as.numeric(cut(X[, J[1]], breaks = z1, include.lowest = TRUE))
      
      z2 = c(min(X[, J[2]]), as.numeric(quantile(X[, J[2]], 
                                                 seq(1/K, 
                                                     1, 
                                                     length.out = K), 
                                                 type = 1))) %>% 
        unique()
      K2 = length(z2) - 1
      a2 = as.numeric(cut(X[, J[2]], breaks = z2, include.lowest = TRUE))
      
      fJ = matrix(0, K1, K2)
     
      
      X11 = X ; X12 = X ; X21 = X ; X22 = X
      
      X11[, J] = cbind(z1[a1], z2[a2])
      X12[, J] = cbind(z1[a1], z2[a2 + 1])
      X21[, J] = cbind(z1[a1 + 1], z2[a2])
      X22[, J] = cbind(z1[a1 + 1], z2[a2 + 1])
      
      Delta = CalcDelta2D(X11,
                          X12,
                          X21,
                          X22,
                          pred.fun)
      
      Delta = as.matrix(tapply(Delta, 
                               list(a1, 
                                    a2), 
                               mean))
      
      # Handling NAs
      NA.Delta = is.na(Delta)
      NA.ind = which(NA.Delta, 
                     arr.ind  = T, 
                     useNames = F)
      
      if (nrow(NA.ind) > 0) {
        notNA.ind = which(!NA.Delta, 
                          arr.ind  = T, 
                          useNames = F)
        range1 = max(z1) - min(z1)
        range2 = max(z2) - min(z2)
        Z.NA = cbind((z1[NA.ind[, 1]] + z1[NA.ind[, 1] + 1])/2/range1, 
                     (z2[NA.ind[, 2]] + z2[NA.ind[, 2] + 1])/2/range2)
        
        Z.notNA = cbind((z1[notNA.ind[, 1]] + z1[notNA.ind[, 1] + 1])/2/range1, 
                        (z2[notNA.ind[, 2]] + z2[notNA.ind[, 2] + 1])/2/range2)
        
        nbrs <- ann(Z.notNA, 
                    Z.NA,
                    k       = 1, 
                    verbose = F)$knnIndexDist[,1]
        Delta[NA.ind] = Delta[matrix(notNA.ind[nbrs,],
                                     ncol = 2)]}
      
      fJ = apply(t(apply(Delta, 
                         1, 
                         cumsum)),
                 2, 
                 cumsum)
      fJ = rbind(rep(0, 
                     K2), 
                 fJ)
      fJ = cbind(rep(0, 
                     K1 + 1), 
                 fJ)
      b = as.matrix(table(a1, 
                          a2))
      b1 = apply(b, 
                 1, 
                 sum)
      
      b2 = apply(b, 
                 2, 
                 sum)
      Delta = fJ[2:(K1 + 1), ] - fJ[1:K1, ]
      b.Delta = b * (Delta[, 1:K2] + Delta[, 2:(K2 + 1)]) / 2
      Delta.Ave = apply(b.Delta, 1, sum)/b1
      fJ1 = c(0, cumsum(Delta.Ave))
      Delta = fJ[, 2:(K2 + 1)] - fJ[, 1:K2]
      b.Delta = b * (Delta[1:K1, ] + Delta[2:(K1 + 1), 
      ])/2
      Delta.Ave = apply(b.Delta, 2, sum) / b2
      fJ2 = c(0, cumsum(Delta.Ave))
      fJ = fJ - outer(fJ1, 
                      rep(1, 
                          K2 + 1)) - outer(rep(1, 
                                               K1 + 1), 
                                           fJ2)
      fJ0 = sum(b * (fJ[1:K1, 1:K2] + 
                     fJ[1:K1, 2:(K2 + 1)] + 
                     fJ[2:(K1 + 1), 1:K2] + 
                     fJ[2:(K1 + 1), 2:(K2 + 1)]) / 4) / sum(b)
      fJ = fJ - fJ0
      x <- list(z1, z2)
      K <- c(K1, K2)

    }
    else print("error:  class(X[,J[1]]) must be either factor or numeric/integer")
  }
  else {print("error:  J must be a vector of length one or two")}
  return(list(K = K, x.values = x, f.values = fJ))}




