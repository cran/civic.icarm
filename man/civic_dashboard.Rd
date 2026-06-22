\name{civic_dashboard}
\alias{civic_dashboard}
\title{Generate a 4-panel civic accountability dashboard}
\description{
Produces a single 2x2 summary dashboard combining feature
importance, fairness audit, confusion matrix or actual vs predicted
plot, and performance metrics bar chart. Requires gridExtra.
}
\usage{
civic_dashboard(object, test_data, outcome, protected = NULL,
                positive = NULL, title = NULL, analyst = NULL)
}
\arguments{
  \item{object}{A civic_model from civic_fit().}
  \item{test_data}{A data frame for evaluation.}
  \item{outcome}{Character. Outcome column name.}
  \item{protected}{Character. Protected attribute column (optional).}
  \item{positive}{Character. Positive class for binary (optional).}
  \item{title}{Character. Overall dashboard title (optional).}
  \item{analyst}{Character. Analyst name for caption (optional).}
}
\value{
A grid object printed to the graphics device. Requires gridExtra.
}
\examples{
\dontrun{
data(civic_voting)
sp <- civic_split(civic_voting, stratify = "voted")
m  <- civic_fit(voted ~ age + education,
                sp$train, model = "logistic", positive = "yes")
civic_dashboard(m, sp$test, outcome = "voted",
                protected = "gender", positive = "yes")
}
}

