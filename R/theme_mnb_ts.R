#' MNB Time Series Theme for ggplot2
#'
#' This function creates a custom theme for time series plots using the MNB (Magyar Nemzeti Bank) style guidelines.
#'
#' @param base_size Base font size for the theme. Default is inherited from ggplot2::theme_minimal().
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' ggplot2::ggplot(economics, aes(x = date, y = unemploy)) +
#'   ggplot2::geom_line(color = "#18223e", linewidth = 1.1) +
#'   theme_mnb_ts()
#'
#' @export

theme_mnb_ts <- function(base_size = 20, remove_xlab = TRUE, remove_ylab = TRUE) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Helvetica",color = "black", size = base_size),
      axis.text = ggplot2::element_text(family = "Helvetica",color = "black", size = base_size),
      axis.text.x = ggplot2::element_text(vjust = 1),
      plot.background = ggplot2::element_rect(fill = "transparent"),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.grid.major.x  = ggplot2::element_blank(),
      panel.grid.minor.x  = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linetype = "dashed", color = "#969696", linewidth = .2, lineend = "square"),
      axis.line.x = ggplot2::element_line(color = "#969696"),
      plot.title.position = "plot", 
      axis.ticks.x.bottom = ggplot2::element_line(color = "#969696"),
      axis.title.x = if (remove_xlab) ggplot2::element_blank() else ggplot2::element_text(), 
      axis.title.y = if (remove_ylab) ggplot2::element_blank() else ggplot2::element_text(), # use subtitle
    )
}
