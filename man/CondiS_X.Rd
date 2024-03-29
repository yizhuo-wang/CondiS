% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CondiS-X.R
\name{CondiS_X}
\alias{CondiS_X}
\title{CondiS-X Function}
\usage{
CondiS_X(pred_time, status, covariates, method)
}
\arguments{
\item{pred_time}{The imputed follow up time for right-censored data.}

\item{status}{The censoring indicator, normally 0=right censored, 1=event at time.}

\item{covariates}{The additional patient data that is presumably associated with the survival time.}

\item{method}{Choose from 8 machine learning algorithms; the default is "glm".}
}
\description{
This function allows you to improve the imputed survival time by incorporating covariate information.
}
\examples{
CondiS(survival_dat)
}
