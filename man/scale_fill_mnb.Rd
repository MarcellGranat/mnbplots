% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_color_mnb.R
\name{scale_fill_mnb}
\alias{scale_fill_mnb}
\title{MNB Fill Scale for ggplot2}
\usage{
scale_fill_mnb(values = NULL)
}
\arguments{
\item{values}{Optional. A vector of color names. Will be translated to closest MNB color.}
}
\value{
A ggplot2 fill scale object.
}
\description{
This function creates a fill scale using the MNB color palette for ggplot2.
}
\examples{
ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length, fill = Species)) +
  ggplot2::geom_boxplot() +
  scale_fill_mnb()

ggplot2::ggplot(iris) + 
  ggplot2::aes(Sepal.Length, Sepal.Width, fill = Species) +
  ggplot2::geom_boxplot() + 
  scale_fill_mnb(values = c("red4", "blue", "yellow"))

}
