% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_color_mnb.R
\name{scale_fill_discrete_mnb_divergent}
\alias{scale_fill_discrete_mnb_divergent}
\title{MNB divergent fill for ggplot2}
\usage{
scale_fill_discrete_mnb_divergent()
}
\value{
A ggplot2 fill scale object.
}
\description{
This function creates a fill scale using the MNB color palette for ggplot2.
}
\examples{
library(ggplot2)
ggplot2::ggplot(iris) + 
  ggplot2::aes(Sepal.Length, Sepal.Width, fill = Species) +
  ggplot2::geom_point(size = 3, shape = 21) + 
  scale_fill_discrete_mnb_divergent()

}
