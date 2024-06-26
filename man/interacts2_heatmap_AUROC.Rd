% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ALEplotPlus.R
\name{interacts2_heatmap_AUROC}
\alias{interacts2_heatmap_AUROC}
\title{interacts2_heatmap_AUROC}
\usage{
interacts2_heatmap_AUROC(ALE_interacts, predictions, truevals)
}
\arguments{
\item{ALE_interacts}{the output of findALEinteracts}

\item{predictions}{a vector of predictions for the original AUC}

\item{truevals}{the true values for calculating AUC}
}
\value{
a matrix compatible with pheatmap
}
\description{
summarizes the information from the findALEinteractions
function into a format compatible with pheatmap.
A variant that measures importance as the difference in AUC when adjusted
for the ALEPlot interaction estimates. THIS FUNCTION IS IN BETA, may or may not work
Approach is experimental and not validated, not public atm.
between
}
