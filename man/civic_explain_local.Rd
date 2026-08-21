\name{civic_explain_local}
\alias{civic_explain_local}
\title{Generate local instance-level explanations}
\description{
Explains why the model made a specific prediction for one or more
individual observations. Uses DALEX break-down if available,
falls back to coefficient contributions for GLM and LM models.
}
\usage{
civic_explain_local(explainer, newdata, n_features = 10L)
}
\arguments{
  \item{explainer}{A civic_explainer from civic_explain().}
  \item{newdata}{A data frame of observations to explain.}
  \item{n_features}{Integer. Maximum features to show. Default 10.}
}
\value{
A list of tibbles, one per row of newdata.
}
\examples{
m <- civic_fit(Species ~ ., iris)
ex <- civic_explain(m)
civic_explain_local(ex, iris[1, ])
}

