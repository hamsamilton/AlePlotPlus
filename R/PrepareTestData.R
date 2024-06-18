#' @import ALEPlotPlus
#' @import ALEPlot
#' @import dplyr
#' @import vip
#' @import MASS
#' @import viridis
#' @import randomForest
#' @import mlbench
#' @import stringr
#' @import ggplot2

# # These are a series of functions which produce the same kind of object used for
# # testing the ALEPlot package, it preprocesses the data, fits a random forest model
# then stores all the details
# specific information in a list where they go within the ALE function
# all functions produce a list of 
# X = the dataset
# X.MODEL = the model
# pred.fun = the prediction function

#'SetupBoston
#'
#'This function sets up the Boston Housing Dataset, for which many predictors 
#'predict the median value of housing
#'CRIM - per capita crime rate by town
#' ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
#' INDUS - proportion of non-retail business acres per town.
#' CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
#' NOX - nitric oxides concentration (parts per 10 million)
#' RM - average number of rooms per dwelling
#' AGE - proportion of owner-occupied units built prior to 1940
#' DIS - weighted distances to five Boston employment centres
#' RAD - index of accessibility to radial highways
#' TAX - full-value property-tax rate per $10,000
#' PTRATIO - pupil-teacher ratio by town
#' B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#' LSTAT - % lower status of the population
#' MEDV - Median value of owner-occupied homes in $1000's
#' 
#'@export
SetupBoston <- function(){
  
  boston = Boston %>% dplyr::select(-chas)
  
  rf = randomForest::randomForest(medv ~ .,
                                  boston)

  X = boston %>% dplyr::select(-medv)
  
  pred_fun <- function(X.model, newdata) {
    
    prd_out = predict(X.model, newdata)
    
    return(prd_out)}
  
  BostonList = list(X = X, X.MODEL = rf, pred.fun = pred_fun)
  
  return(BostonList)
}

#' SetupPima
#'
#' This function sets up the Pima Indian Diabetes Dataset, which contains various predictors for diabetes.
#'
#' The dataset includes the following predictors:
#' \itemize{
#'   \item \strong{Pregnancies} - Number of times pregnant.
#'   \item \strong{Glucose} - Plasma glucose concentration after 2 hours in an oral glucose tolerance test.
#'   \item \strong{BloodPressure} - Diastolic blood pressure (mm Hg).
#'   \item \strong{SkinThickness} - Triceps skin fold thickness (mm).
#'   \item \strong{Insulin} - 2-hour serum insulin (mu U/ml).
#'   \item \strong{BMI} - Body mass index (weight in kg/(height in m)^2).
#'   \item \strong{DiabetesPedigreeFunction} - Diabetes pedigree function (a function which scores likelihood of diabetes based on family history).
#'   \item \strong{Age} - Age in years.
#' }
#'
#' The target variable is:
#' \itemize{
#'   \item \strong{Outcome} - Binary variable indicating if the patient has diabetes (1) or not (0).
#' }
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{X}{A data frame of the Pima Indian Diabetes data without the \code{Outcome} column.}
#'   \item{X.MODEL}{A random forest model object fitted to the Pima Indian Diabetes data.}
#'   \item{pred.fun}{A function to make predictions using the random forest model.}
#' }
#'@export
SetupPimi <- function(){
  
  data(PimaIndiansDiabetes)
  
  pred_fun <- function(X.model, newdata) {
    prd_out = predict(X.model, 
                      newdata, 
                      type="prob")
    return(prd_out[,2])
  }
  
  pimi = PimaIndiansDiabetes
  pimi$diabetes = as.factor(pimi$diabetes == "pos")
  
  X = pimi %>%
    dplyr::select(-diabetes)
  rf = randomForest::randomForest(diabetes ~ .,
                                  pimi)
  
  PimiList = list(X        = X,
                  X.MODEL  = rf,
                  pred.fun = pred_fun)
  
  return(PimiList)
}

#' SetupMTCars
#'
#' This function sets up the mtcars dataset, which contains various predictors for miles per gallon (mpg).
#'
#' The dataset includes the following predictors:
#' \itemize{
#'   \item \strong{mpg} - Miles/(US) gallon.
#'   \item \strong{cyl} - Number of cylinders.
#'   \item \strong{disp} - Displacement (cu.in.).
#'   \item \strong{hp} - Gross horsepower.
#'   \item \strong{drat} - Rear axle ratio.
#'   \item \strong{wt} - Weight (1000 lbs).
#'   \item \strong{qsec} - 1/4 mile time.
#'   \item \strong{vs} - Engine (0 = V-shaped, 1 = straight).
#'   \item \strong{am} - Transmission (0 = automatic, 1 = manual).
#'   \item \strong{gear} - Number of forward gears.
#'   \item \strong{carb} - Number of carburetors.
#' }
#'
#' The target variable is:
#' \itemize{
#'   \item \strong{mpg} - Miles/(US) gallon.
#' }
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{X}{A data frame of the mtcars data without the \code{mpg} column.}
#'   \item{X.MODEL}{A random forest model object fitted to the mtcars data.}
#'   \item{pred.fun}{A function to make predictions using the random forest model.}
#' }
#' @export
SetupMtcars <- function(){
  
  mtcar = mtcars %>% 
    dplyr::select(-vs,
                  -am)
  
  rf = randomForest::randomForest(mpg ~ .,
                                  mtcar)
  
  X = mtcar %>% 
    dplyr::select(-mpg)
  
  pred_fun <- function(X.model, newdata) {
    
    prd_out = predict(X.model, 
                      newdata)
    
    return(prd_out)}
  
  MTcarsList = list(X        = X,
                    X.MODEL  = rf,
                    pred.fun = pred_fun)
  
  return(MTcarsList)
}
  
  
  
  
  
  
  
  
  
  
  