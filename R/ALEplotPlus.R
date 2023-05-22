# to install setwd("/Users/samhamilton/Library/Mobile Documents/com~apple~CloudDocs/Thesis_Aim1/scripts/ALEPlotPlus")
# devtools::document() ; devtools::build() ; devtools::install()

#' @import ggplot2
#' @import dplyr
#' @import pheatmap
#' @import randomForest
#' @import ALEPlot
#' @import ggplot2

#' @export
dplyr::`%>%`

#'find_ALE_interacts
#'
#'Return a dataframe of the estimate interactions between predictors in an MLModel
#'using their ALEPlots
#'
#'@param X the dataframe of predictors used to train model
#'@param model the machine learning model use
#'@param pred_fun the prediction function used with ALEPlot, see ALEPlot for details
#'@param K the number of bins in which to assess interactions, see ALEPlot for detaal
#'@return a datagrame estimated interaction strengths. Estimated by multiplying each
#'        point by its estimated interaction effect
#'@export
find_ALE_interacts = function(X,model,pred_fun,K = 40){

  # get all combos
  var_combs = combn(1:ncol(X),m = 2,simplify=T)

  # gen ALEplot
  int_estimates = lapply(1:ncol(var_combs),function(i){
    int_estimate = find_ALE_interact(X = X,
                                     model = model,
                                     pred_fun = pred_fun,
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
#'@param pred_fun the prediction function used with ALEPlot, see ALEPlot for details
#'@param int_vars a list of the names of the two interacting variables of interest
#'@param K the number of bins in which to assess interactions, see ALEPlot for detail
#'@return a value representing the average estimated effect size of an interaction
#'@export
find_ALE_interact = function(X,model,pred_fun,int_vars,K = 40){

  ALEplt = ALEPlot::ALEPlot(X,
                            X.model=model,
                            pred.fun=pred_fun,
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
    image(x_vec, y_vec, z_mat,
          xlab = colnames(X)[x_ind],
          ylab = colnames(X)[y_ind],
          xlim = range(x_vec),
          ylim = range(y_vec),
          col = clrs,
          breaks = brks)

    contour(x_vec, y_vec, z_mat, add = TRUE, drawlabels = TRUE)

    points(X[,x_ind],X[,y_ind])

    rug(X[,x_ind], side = 1, col = "black")
    rug(X[,y_ind], side = 2, col = "black")
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
mk.colors_ale <- function(nclrs = 30, clr_st = "RdBu",even = T, mtrx,logd = F) {

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
    val <- data$vals[i]

    adj_matrix[row_name, col_name] <- val
  }

  # Symmetrize the matrix
  adj_matrix <- (adj_matrix + t(adj_matrix))

  return(adj_matrix)
}

#' rf_interacts2_heatmap
#'
#' summarizes the information from the findALEinteractions
#' function into a format compatable with pheatmap
#' @param ALE_interacts
#' @return a matrix compatible with pheatmap
#' @export
interacts2_heatmap = function(ALE_interacts){

  ALE_interacts = ALE_interacts %>% group_by(x,y) %>%
  dplyr::summarize(mn = mean(abs(val)))
ALE_adj = create_adjacency_matrix(Xvec = ALE_interacts$x,
                                 Yvec = ALE_interacts$y,
                                 vals = ALE_interacts$mn)
diag(ALE_adj) = NA
return(ALE_adj)}

#'interacts2_histogram
#'
#' summarizes the information from the findALEinteractions
#' function into a set of histograms for comparison, just a different
#' way to do the data.
#'@param the output of findALEinteractions
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

  ORDEREDNAMES <- colnames(X)  # get the variable importance to order the ALEPlots
  ALEPLOTS <- lapply(colnames(X),function(nm){
    ALEDF <- ALEPlot(X, X.MODEL, pred.fun, nm, K = K, NA.plot = TRUE) %>%
      as.data.frame()

    est_effect = calc_vals_1D(X[,nm],ALEDF$x.values,ALEDF$f.values)
    est_effect_df = data.frame(x = X[,nm],val = est_effect)
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
#'given the X coordinates of the two predictors used to generate an ALEPlot
#'and the ALEPlot info, calculate the est effect for each point for the
#'input dataset
#'@param X the data used to train the model, without predicted var
#'@param MODEL the model
#'@param pred.fun the prediction function required by ALEPLOT package see details
#'@param K the number of bins split to evaluate the ALE Plot see ALEPLOT package
#'@return a vector of estimated effect intensities for each point
#'@export
calc_ALE_varimp = function(X,MODEL,K = 40,pred.fun){

  imp_scores <- lapply(colnames(X),function(nm){
    ALEDF <- ALEPlot(X, MODEL, pred.fun, nm, K = K, NA.plot = TRUE)
    ALEDF <- as.data.frame(ALEDF)

    imp_score = calc_vals_1D(x_points=X[,nm],
                             x_vec = ALEDF$x.values,z_vec= ALEDF$f.values) %>%
      abs() %>%
      mean()
    return(imp_score)
  })
  imp_score_df = data.frame(var = colnames(X),varimp = unlist(imp_scores))
  return(imp_score_df)
  }

#' a theeme for faceted plots which does not include a legend for nice tiling.
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

