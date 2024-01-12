# to install setwd("/Users/samhamilton/Library/Mobile Documents/com~apple~CloudDocs/Thesis_Aim1/libraries/ALEPlotPlus")
# devtools::document() ; devtools::build() ; devtools::install()

#' @import tidyverse
#' @import pheatmap
#' @import randomForest
#' @import ALEPlot
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
    int_estimate = find_ALE_interact(X = X,
                                     X.MODEL = X.MODEL,
                                     pred.fun = pred.fun,
                                     int_vars = var_combs[,i],
                                     K = K)
    int_estimate = data.frame(x = colnames(X)[var_combs[1,i]],
                              y = colnames(X)[var_combs[2,i]],
                              val = int_estimate)
    return(int_estimate)})

  int_estimates = do.call("rbind",int_estimates)
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

  ALEplt = ALEPlot::ALEPlot(X,
                            X.model=X.MODEL,
                            pred.fun=pred.fun,
                            J = int_vars,
                            K = K)
  x_vec = ALEplt$x.values[[1]]
  y_vec = ALEplt$x.values[[2]]
  z_mat = ALEplt$f.values

  x_vals = X[,int_vars[1]]
  y_vals = X[,int_vars[2]]

  int_estimate = calc_vals(x_points = x_vals,
                   y_points = y_vals,
                   x_vec    = x_vec,
                   y_vec    = y_vec,
                   z_vec    = z_mat)

  return(int_estimate)
}


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

  # Get points vector length
  num_points = length(x_points)

  z_values = sapply(1:num_points,function(i){
    x_pos = sum(x_points[[i]] >= x_vec)
    y_pos = sum(y_points[[i]] >= y_vec)

    z_val = z_vec[x_pos,y_pos]

    return(z_val)})

  return(z_values)
}

# Format a number to three significant figures
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

  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, at=ticks, labels=formatted_ticks, las=2)
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
#'@param model a model you are interrogating
#'@param pred_fun the prediction function used to generate ALEPlot...see doc there
#'@param K how many subdivisions to estimate the int coef. for, see ALEPlot
#'@param savedir where to save all the ALEPlots generated
#'@return saves all combinations of plots
#'@export
create_2D_ALEs = function(X,model,pred_fun,K = 40,savedir){

  dir.create(savedir)

  # get all combos
  var_combs = combn(1:ncol(X),m = 2,simplify=T)

  # gen ALEplot
  int_estimates = lapply(1:ncol(var_combs),function(i){

    x_ind = var_combs[1,i]
    y_ind = var_combs[2,i]

    ALEplt = ALEPlot::ALEPlot(X,
                              X.model=model,
                              pred.fun=pred_fun,
                              J = c(x_ind,y_ind),
                              K = K)

    x_vec = ALEplt$x.values[[1]]
    y_vec = ALEplt$x.values[[2]]
    z_mat = ALEplt$f.values

    jpeg(filename = paste0(savedir,
                           '/',
                           str_safe(colnames(X)[x_ind]),
                           str_safe(colnames(X)[y_ind]),
                           ".jpeg"),
         height   =6,
         width    =6,
         units    ="in",
         res = 720)

    clrs = mk.colors_ale(mtrx = z_mat)$clrs
    brks = mk.colors_ale(mtrx = z_mat)$brks

# combine plots with 1 row and 2 columns
# Define the layout matrix
 mat <- matrix(c(1,2,1,2), 2, 2, byrow = TRUE)

    # Specify the layout with relative column widths
 layout(mat, widths=c(4,1))

 image(x_vec, y_vec, z_mat,
       xlab = colnames(X)[x_ind],
       ylab = colnames(X)[y_ind],
       xlim = range(x_vec),
       ylim = range(y_vec),
       col = clrs,
       breaks = brks,
       cex.axis = 1.5,  # Increase the size of axis tick labels
       cex.lab = 1.5   # Increase the size of axis labels
 )

 contour(x_vec, y_vec, z_mat, add = TRUE, drawlabels = TRUE)

 points(X[,x_ind],X[,y_ind])

 color.bar(clrs, min = min(brks), max = max(brks))


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
mk.colors_ale <- function(nclrs = 30, clr_st = "Spectral",even = T, mtrx,logd = F) {

  # get the limits of the heatmap values
  mx.psvl <- max(mtrx,na.rm = T)
  mx.ngvl <- min(mtrx,na.rm = T)
  if(even){ mx.ngvl = abs(mx.ngvl)}
  mxvl <- max(mx.psvl,mx.ngvl)

  # Make the colors
  clrs.4plt <- RColorBrewer::brewer.pal(9,clr_st)
  clr.rmp <- colorRampPalette(clrs.4plt)
  clrs.4plt <- clr.rmp(nclrs)
  clrs.4plt <- rev(clrs.4plt)


  # Make the breaks
  if(even){
    if(logd){
      brks = exp(log(10)*seq(log10(-mxvl),log10(mxvl),length.out = nclrs + 1))}
    else{
      brks = seq(-mxvl,mxvl,length.out = nclrs + 1)}
    nblw <- sum(brks < - mx.ngvl)
    clrs.4plt <- clrs.4plt[nblw:length(clrs.4plt)]
    nbrks <- brks[nblw:length(brks)]} else{
      if(logd){
        nbrks = exp(log(10)*seq(log10(mx.ngvl),log10(mxvl),length.out = nclrs + 1))}
      else{nbrks = seq(mx.ngvl,mxvl,length.out = nclrs + 1)}
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
  data <- data.frame(X = Xvec, Y = Yvec, vals = vals)

  # Create a unique list of all variable names
  variables <- unique(c(Xvec, Yvec))
  # Create an empty adjacency matrix
  adj_matrix <- matrix(0,
                       nrow = length(variables),
                       ncol = length(variables),
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
#' @param ALE_interacts the output of  findALEinteracts
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
#' between
#' @param ALE_interacts the output of findALEinteracts
#' @param predictions a vector of predictions for the original AUC
#' @param truevals the true values for calculating AUC
#' @return a matrix compatible with pheatmap
#' @export
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

#'interacts2_histogram
#'
#' summarizes the information from the findALEinteractions
#' function into a set of histograms for comparison, just a different
#' way to do the data.
#'@param ALE_interacts output of findALEinteractions
#'@param bins the number of histogram bins
#'@param ncol how many columns in the facetplot
#'@export
interacts2_histogram <- function(ALE_interacts,bins = 1000,
                                 ncol = 1){

  ALE_interacts$combo = paste0(ALE_interacts$x,
                               ALE_interacts$y)

  # Create stacked histograms
  plt <- ggplot(ALE_interacts, aes(x = val, fill = combo)) +
    geom_histogram(bins = bins) +
    facet_wrap(~combo,ncol = 1) +
    theme_minimal()
  return(plt)
}

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

  ALEPLOTS <- lapply(colnames(X),function(nm){
    print(nm)
    ALEDF <- ALEPlot(X, X.MODEL, pred.fun, nm, K = K, NA.plot = TRUE) %>%
      as.data.frame()


    est_effect_df = calc_vals_1D(X[,nm],ALEDF$x.values,ALEDF$f.values) %>%
      data.frame(x = X[,nm],val = .)

    ALE_OUT <- ggplot(ALEDF,aes(x = x.values,y = f.values)) +
      geom_line(size = 1.3) +
      geom_point(data = est_effect_df,aes(x = x,y = val),color = "red",shape = 4) +
      labs(x = nm,y = "effect on prediction") + facet.themes()
    return(ALE_OUT)})
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

  #interpolate values for greater accuracy
  interp = approx(x_vec,z_vec,xout = seq(min(x_vec),
                                         max(x_vec),
                                         length.out = 200))

  # Get points vector length
  num_points = length(x_points)

  z_values = sapply(1:num_points,function(i){
    x_pos = sum(x_points[[i]] >= interp$x)

    z_val = interp$y[x_pos]

    return(z_val)})

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
    ALEDF <- ALEPlot(X, X.MODEL, pred.fun, nm, K = K, NA.plot = TRUE)
    ALEDF <- as.data.frame(ALEDF)

    imp_score = calc_vals_1D(x_points=X[,nm],
                             x_vec = ALEDF$x.values,z_vec= ALEDF$f.values) %>%
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

  imp_score_mat <- calc_ALE_varimp_df(X,X.MODEL,K,pred.fun)

  imp_score_avg = imp_score_mat %>%
    abs() %>%
    colMeans() %>%
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

  # calculate predicted AUROC
  orig_AUROC <- pltROC(predictions,truevals)$AUC

  deltAUROCs = sapply(colnames(adj_imp_score_mat),function(predictor){

    delt = orig_AUROC - pltROC(adj_imp_score_mat[,predictor],
                               truevals)$AUC
    return(delt)
  })
  return(deltAUROCs)}




#' a theme for faceted plots which does not include a legend for nice tiling.
#' @export
facet.themes <- function() {ggplot2::theme_classic() +  ggplot2::theme(axis.title = element_text(size = 20,color = "black"),
                                         axis.text = element_text(size = 20,color = "black"),
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
  return(strng)
}


#'QckRBrwrPllt
#'
#'This is a quick function that combines two RColorBrewer functions into one that
#'makes it more intuitive to use and setup
#'@param name the Rbrewer pallette name
#'@param n how many colors you want
#'@return a color vector
#'@export
QckRBrwrPllt <- function(name,n) {
  plt <- colorRampPalette(RColorBrewer::brewer.pal(8,name))(n)
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

  roc_obj_all <- pROC::roc(predictions,response = truevals)

  roc_points_all = data.frame(sensitivities   =  roc_obj_all$sensitivities,
                              specificities   =  roc_obj_all$specificities)

  rcplt <- pROC::ggroc(roc_obj_all,size = 1.25) +
    geom_abline(slope = 1,
                intercept = 1,
                linetype = 'dashed',
                color = "red") +
    geom_point(data = roc_points_all,aes(x = specificities,
                                         y = sensitivities)) +
    annotate("text", x = .25, y = .25, size = 10, label = str_c("AUC = ",
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


