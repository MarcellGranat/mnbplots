#' MNB Color Palette
#'
#' This function returns a vector of colors used in the MNB (Magyar Nemzeti Bank) color palette.
#'
#' @param n Optional. Number of colors to return. If NULL (default), all colors are returned.
#'
#' @return A character vector of color hexadecimal codes.
#'
#' @examples
#' # Get all MNB colors
#' mnb_colors()
#'
#' @export

mnb_colors <- function(n = NULL) {
  colors <- c(
    "#18223e",
    "#6fa0be",
    "#f8c567",
    "#b2242a",
    "#7aa140",
    "#da3232",
    "#e57b2b",
    "#787975",
    "#b9e1eb"
  )
  if (!is.null(n)) colors <- colors[1:n]
  colors
}
