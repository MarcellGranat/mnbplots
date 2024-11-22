#' Determine Big Mark for Number Formatting
#'
#' This function determines the appropriate big mark for number formatting based on the language setting.
#'
#' @return A logical value: TRUE if the language is Hungarian or not set, FALSE otherwise.
#'
#' @keywords internal
.bigmark <- function() {
  if (is.null(options()$lang) || options()$lang == "hu") " " else ","
}

#' Determine Decimal Mark for Number Formatting
#'
#' This function determines the appropriate decimal mark for number formatting based on the language setting.
#'
#' @return A logical value: TRUE if the language is Hungarian or not set, FALSE otherwise.
#'
#' @keywords internal
.decimal <- function() {
  if (is.null(options()$lang) || options()$lang == "hu") "," else "."
}

#' Scale Y-axis with Formatted Numbers
#'
#' This function creates a continuous y-axis scale with numbers formatted according to the current language setting.
#'
#' @return A ggplot2 scale object for the y-axis with formatted numbers.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_y_number()
#'
#' @export
#'
scale_y_number <- function(..., expand = c(0, 0)) {
  ggplot2::scale_y_continuous(
    labels = ~ scales::number(., big.mark = .bigmark(), decimal.mark = .decimal()),
    expand = expand,
    ...
  )
}

#' Scale X-axis with Formatted Numbers
#'
#' This function creates a continuous x-axis scale with numbers formatted according to the current language setting.
#'
#' @return A ggplot2 scale object for the x-axis with formatted numbers.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_x_number()
#'
#' @export
#'
scale_x_number <- function(..., expand = c(0, 0)) {
  ggplot2::scale_x_continuous(
    labels = ~ scales::number(., big.mark = .bigmark(), decimal.mark = .decimal()),
    expand = expand,
    ...
  )
}

#' Scale Y-axis with Formatted Percent
#'
#' This function creates a continuous y-axis scale with numbers formatted according to the current language setting.
#'
#' @return A ggplot2 scale object for the y-axis with formatted numbers.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_y_number()
#'
#' @export
#'
scale_y_percent <- function(..., expand = c(0, 0)) {
  ggplot2::scale_y_continuous(labels = ~ scales::percent(., decimal.mark = .decimal()))
}

#' Scale Y-axis with Formatted Thousend Numbers
#'
#' This function creates a continuous y-axis scale with numbers formatted according to the current language setting.
#'
#' @return A ggplot2 scale object for the y-axis with formatted numbers.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_y_number()
#'
#' @export
#'
scale_k_number <- function(..., expand = c(0, 0)) {
  ggplot2::scale_y_continuous(
    labels = ~ scales::number(. / 1e3, big.mark = .bigmark(), decimal.mark = .decimal()),
    expand = expand
  )
}

#' Scale Y-axis with Formatted Million Numbers
#'
#' This function creates a continuous y-axis scale with numbers formatted according to the current language setting.
#'
#' @return A ggplot2 scale object for the y-axis with formatted numbers.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_y_number()
#'
#' @export
#'
scale_m_number <- function(..., expand = c(0, 0)) {
  ggplot2::scale_y_continuous(
    labels = ~ scales::number(. / 1e6, big.mark = .bigmark(), decimal.mark = .decimal()),
    expand,
    ...
  )
}

#' Scale X-axis with Quarter Labels
#'
#' This function creates a continuous x-axis scale with labels at each quarter.
#'
#' @return A ggplot2 scale object for the x-axis with quarter labels.
#'
#' @export
scale_x_quarter <- function(..., expand = c(0, 0)) {
  ggplot2::scale_x_date(
    labels = date_format("%YQ%q"),
    expand,
    ...
  ) # TODO hun
}