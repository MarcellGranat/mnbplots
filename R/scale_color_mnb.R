#' MNB Color Scale for ggplot2
#'
#' This function creates a color scale using the MNB color palette for ggplot2.
#'
#' @param values Optional. A vector of color names. Will be translated to closest MNB color.
#'
#' @return A ggplot2 color scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot2::ggplot(iris) + 
#'   ggplot2::aes(Sepal.Length, Sepal.Width, color = Species) +
#'   ggplot2::geom_point(size = 3) + 
#'   scale_color_mnb()
#'
#' ggplot2::ggplot(iris) + 
#'   ggplot2::aes(Sepal.Length, Sepal.Width, color = Species) +
#'   ggplot2::geom_point(size = 3) + 
#'   scale_color_mnb(values = c("red4", "blue", "yellow"))
#'
#' @export

scale_color_mnb <- function(values = NULL) {
  if (!is.null(values)) {
    color_values <- closest_mnb_color(values)
  } else {
    color_values <- mnb_colors()
  }
  ggplot2::scale_color_manual(values = color_values)
}

#' MNB Fill Scale for ggplot2
#'
#' This function creates a fill scale using the MNB color palette for ggplot2.
#'
#' @param values Optional. A vector of color names. Will be translated to closest MNB color.
#'
#' @return A ggplot2 fill scale object.
#'
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length, fill = Species)) +
#'   ggplot2::geom_boxplot() +
#'   scale_fill_mnb()
#'
#' ggplot2::ggplot(iris) + 
#'   ggplot2::aes(Sepal.Length, Sepal.Width, fill = Species) +
#'   ggplot2::geom_boxplot() + 
#'   scale_fill_mnb(values = c("red4", "blue", "yellow"))
#' 
#' @export

scale_fill_mnb <- function(values = NULL) {
  if (!is.null(values)) {
    color_values <- closest_mnb_color(values)
  } else {
    color_values <- mnb_colors()
  }
  ggplot2::scale_fill_manual(values = color_values)
}

#' MNB divergent colors for ggplot2
#'
#' This function creates a color scale using the MNB color palette for ggplot2.
#'
#' @return A ggplot2 color scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot2::ggplot(iris) + 
#'   ggplot2::aes(Sepal.Length, Sepal.Width, color = Species) +
#'   ggplot2::geom_point(size = 3) + 
#'   scale_color_discrete_mnb_divergent()
#'
#' @export

scale_color_discrete_mnb_divergent <- function() {
  ggplot2::scale_color_brewer(palette = "RdBu", type = "div")
}

#' MNB divergent fill for ggplot2
#'
#' This function creates a fill scale using the MNB color palette for ggplot2.
#'
#' @return A ggplot2 fill scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot2::ggplot(iris) + 
#'   ggplot2::aes(Sepal.Length, Sepal.Width, fill = Species) +
#'   ggplot2::geom_point(size = 3, shape = 21) + 
#'   scale_fill_discrete_mnb_divergent()
#'
#' @export

scale_fill_discrete_mnb_divergent <- function() {
  ggplot2::scale_fill_brewer(palette = "RdBu", type = "div")
}
