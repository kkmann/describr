% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pvalues.R
\name{compute_pvalue}
\alias{compute_pvalue}
\title{Compute p-value for homogeneity nullhypothesis}
\usage{
compute_pvalue(p, df, varname, groupname, ...)
}
\arguments{
\item{p}{p-value object}

\item{df}{data frame}

\item{varname}{character string giving name of variable to test for homogeneity
with respect to group.}

\item{groupname}{character string giving name of grouping-variable (a factor)
in df.}
}
\value{
a single numeric p-value.
}
\description{
Compute p-value for homogeneity nullhypothesis
}
