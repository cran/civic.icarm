\name{plot.civic_model}
\alias{plot.civic_model}
\title{Plot a civic_model object}
\usage{
\method{plot}{civic_model}(x, title = NULL, ...)
}
\arguments{
\item{x}{A \code{civic_model} from \code{\link{civic_fit}}.}
\item{title}{Optional plot title.}
\item{...}{Further arguments passed to \code{\link{civic_plot_importance}}.}
}
\value{A \pkg{ggplot2} object, invisibly.}
\description{
Produces a feature importance lollipop chart directly from a fitted
\code{civic_model} object without requiring an explicit call to
\code{\link{civic_explain}}.
}
\examples{
m <- civic_fit(Species ~ ., iris)
plot(m)
}
