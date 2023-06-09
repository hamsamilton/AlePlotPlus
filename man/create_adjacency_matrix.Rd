% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ALEplotPlus.R
\name{create_adjacency_matrix}
\alias{create_adjacency_matrix}
\title{create_adjacency_matrix}
\usage{
create_adjacency_matrix(Xvec, Yvec, vals)
}
\arguments{
\item{Xvec}{a vector with the X values}

\item{Yvec}{a vector with the Y values}

\item{vals}{values to fill the vector with}
}
\value{
An adjacency matrix
}
\description{
Create an adjacency matrix from a data frame with variable combinations and z-scores
}
\examples{
data <- data.frame(va1 = c("A", "A", "B", "C"),
                   var2 = c("B", "C", "C", "D"),
                   zscore = c(1.5, 2.0, -0.5, 1.0))

adj_matrix <- create_adjacency_matrix(data)
print(adj_matrix)
}
