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

.freq_recognition <- function(x) {
  items <- sort(unique(x)) |> 
    head(20) # should be enough, but many tests

  Ds <- base::diff(items) # Time differences in days

  recognised_freq <- dplyr::case_when(
    all(Ds >= 1 & Ds <= 4) ~ "daily",
    all(Ds >= 360 & Ds <= 370) ~ "annual",
    all(Ds >= 28 & Ds <= 32) ~ "monthly",
    all(Ds >= 84 & Ds <= 93) ~ "quarterly",
    all(Ds >= 6 & Ds <= 8) ~ "weekly",
    all(Ds >= 12 & Ds <= 16) ~ "biweekly", # unpausables
    all(Ds >= 56 & Ds <= 64) ~ "bimonthly",
    all(Ds >= 170 & Ds <= 183) ~ "bianual",
    TRUE ~ "unknown"
  )

  if (recognised_freq == "unknown") cli::cli_warn("Freq unrecognised!")
  recognised_freq
}

date_autolabel <- function(x, shorten = TRUE) {
  freq <- .freq_recognition(x)
  interval <- max(x) - min(x)
  n <- length(unique(x))

  q <- (lubridate::month(x, label = FALSE) - 1) %/% 3 + 1
  y <- lubridate::year(x)
  d <- lubridate::day(x)

  if (is.null(options()$lang) || options()$lang == "hu") {
    # hungarian type date
    m <- lubridate::month(x, label = TRUE, abbr = TRUE, locale = "hu_HU")

    if (freq == "daily") {
      paste(y, m, d)
    } else if (freq == "monthly" | freq == "bimonthly") {
      if (n > 10 & shorten) {
        ifelse(m == 1, paste0(m, " ", y), m)
      } else {
        paste0(m, " ", y)
      }
    } else if (freq == "quarterly") {
      if (n > 10 & shorten) {
        ifelse(m == 1, paste0(y, " ", as.roman(q), ". n.év."))
      } else {
        paste0(y, " ", as.roman(q), ". n.év.")
      }
    } else if (freq == "annual" | freq == "biannual") {
      as.character(y)
    } else {
      paste(y, m, d)
    }
  } else {
    # english
    m <- lubridate::month(x, label = TRUE, abbr = TRUE, locale = "en_GB")
    d_end <- dplyr::case_match(
      d,
      1 ~ "st",
      2 ~ "nd",
      3 ~ "rd",
      21 ~ "st",
      22 ~ "nd",
      23 ~ "rd",
      31 ~ "st",
      .default = "th"
    )

    if (freq == "daily") {
      paste0(d, d_end, " of ", m, " ", y)
    } else if (freq == "monthly") {
      if (n > 10 & shorten) {
        ifelse(m == 1, paste0(y, " ", m), m)
      } else {
        paste0(y, " ", m)
      }
    } else if (freq == "quarterly") {
      paste0(y, "Q", q)
    } else if (freq == "annual") {
      as.character(y)
    } else {
      paste0(d, d_end, " of ", m, " ", y)
    }
  }
}

scale_autodate <- function(..., expand = c(0, 0)) {
  ggplot2::scale_x_date(
    labels = date_autolabel,
    expand,
    ...
  )
}


