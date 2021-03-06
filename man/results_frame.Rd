% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results_frame.R
\name{results_frame}
\alias{results_frame}
\title{Make data.frame from regression models for easy export.}
\usage{
results_frame(..., standard_errors = NULL, t.values = FALSE,
  digits = 2, omit.coef = NULL, omit.coef.label = NULL,
  omit.stat = c("R-squared", "DF"), stars = TRUE,
  intercept.placement = "bottom", covariate.labels = NULL,
  intercept.label = "Constant")
}
\arguments{
\item{...}{Any number of model objects. So far tested with lm and glm-objects.}

\item{standard_errors}{An optional list of vectors of standard_errors. If this is supplied, it will override the standard errors from the model-object(s).}

\item{t.values}{If TRUE, T-values are printed in the final data.frame instead of standard errors. Defaults to FALSE}

\item{digits}{The number of digits to use. This number is global for all resultsing statistics}

\item{omit.coef}{Optional vector of covariate names to omit from the table. These are identified with regex, so take caution with covariates with similar names.}

\item{omit.coef.label}{Optional vector of labels to include in the table to indicate whether covariates were omitted by omit.coef.If provided, the length of this vector must be equal to the length of the vector provided in omit.coefs.}

\item{omit.stat}{Optional vector of model statistics to be omitted from the table. 
Currently, the functions creates the following statistics, use the label in parentheses to omit the respective statistic: Number of observations (N), Adjusted R-squared (Adj. R-squared), R-squared (R-squared), Degrees of freedom (DF), model type (model type), Log-likelihood (LL).
By default, R-squared and DF are omitted.}

\item{stars}{If TRUE, stars indicating significance is printed in the table. Currently prints 5 \% \(\*\), 1 \% \(\*\*\) and 0.1 \% \(\*\*\*\). Defaults to TRUE.}

\item{intercept.placement}{Should the intercept be printed at the "top" of the table, or "bottom"? Defaults to "bottom".}

\item{covariate.labels}{Optional list of covariate labels that overrides the names provided by the model objects. Length must be equal to the number of covariates.}

\item{intercept.label}{What should be the label of the interept? Defaults to "Constant".}
}
\description{
This is a function to create a data frame out of regression results.
}
\examples{
results_frame()
}
\keyword{export}
\keyword{tables}
