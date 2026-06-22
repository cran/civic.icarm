\name{predict.civic_model}
\alias{predict.civic_model}
\title{Predict from a civic_model}
\description{
Generates predictions from a fitted civic_model object for any task type.
}
\usage{
\method{predict}{civic_model}(object, newdata,
  type = c("class", "prob"), threshold = 0.5, ...)
}
\arguments{
  \item{object}{A civic_model.}
  \item{newdata}{A data frame for prediction.}
  \item{type}{For classification: "class" or "prob".
    For regression: ignored.}
  \item{threshold}{Decision threshold for binary (default 0.5).}
  \item{...}{Ignored.}
}
\value{
For classification with type = "class": a factor vector.
For classification with type = "prob": a numeric matrix.
For regression: a numeric vector.
}
\examples{
\donttest{
data(civic_voting)
m <- civic_fit(voted ~ age + education, data = civic_voting)
predict(m, civic_voting[1:5, ], type = "class")
predict(m, civic_voting[1:5, ], type = "prob")
}
}

