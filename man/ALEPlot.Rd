% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ALEplotPlus.R
\name{ALEPlot}
\alias{ALEPlot}
\title{ALEPlot}
\usage{
ALEPlot(X, X.model, pred.fun, J, K = 40)
}
\arguments{
\item{X}{The data frame of predictor variables to which the supervised learning model was fit. 
The names of the predictor variables must be the same as when the model was fit. 
The response variable should not be included in X.}

\item{X.model}{The fitted supervised learning model object (e.g., a tree, random forest, neural network, etc.), 
typically an object to which a built-in predict command associated with that object can be applied.}

\item{pred.fun}{user-supplied function that will be used to predict the response for X.model
for some specified inputs. pred.fun has two arguments. The first argument is 
named X.model and must be the same object as the X.model argument to the 
ALEPlot function. The second argument is named newdata and is a data frame of
 predictor values at which the object X.model is to be predicted. 
 The output of pred.fun must be a numeric vector of predictions having length
  equal to the number of rows of newdata. For most X.model objects, 
  pred.fun can simply call the predict function that was written as part of 
  that modeling object package, assuming the package contains a predict function. 
  An example of where a more customized pred.fun would be used is a multi (> 2) 
  class classification problem for which the built-in predict function returns 
  a vector of predicted probabilities, one for each response class. 
  In this case it may make sense to have pred.fun return the predicted probabilities
   (or its log-odds, etc.) for one particular class of interest.}

\item{J}{A numeric scalar or two-length vector of indices of the predictors 
for which the ALE plot will be calculated. J is either a single index
 (for a main effects plot) or a pair of indices (for a second-order interaction plot).
  For a single index, the corresponding predictor must be either numeric or a factor. 
  For a pair of indices, the corresponding predictors must be either both 
  numeric or the first a factor and the second numeric.
 @param K A numeric scalar that specifies the number of intervals into which 
the predictor range is divided when calculating the ALE plot effects.
If length(J) = 2, the same K will be used for both predictors,
resulting in an array of K^2 cells over the two-dimensional predictor space.
Note that the algorithm may adjust (reduce) K internally if the predictors 
are discrete and have many repeated values. K is only used if the predictor is numeric. 
For factor predictors, the equivalent of K is the number of used levels of}
}
\value{
a list with the plot and the auc info
}
\description{
Main function for generating ALEPlots and ALEDFs
}
