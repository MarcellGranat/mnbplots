#' Find the closest MNB Color
#'
#' This function takes a vector of colors and returns the closest matching color from the MNB color palette.
#'
#' @param colors A character vector of color names or hexadecimal codes.
#' @param limit Numeric. The maximum RGB distance allowed for a match. Default is 200.
#'
#' @return A character vector of the closest matching MNB colors or the original color if no match is found within the limit.
#'
#' @examples
#' closest_mnb_color("red")
#' closest_mnb_color(c("blue", "green", "#FF00FF"))
#'
#' @export

closest_mnb_color <- function(colors, limit = 300) {
  color_list <- mnb_colors() |> 
    append(c("black", "white"))

  mnb_colors_rgb <- color_list |> 
    col2rgb()
  
  sapply(colors, function(color) {
    rgb_distances <- sweep(mnb_colors_rgb, 1, col2rgb(color), FUN = "-") |> 
      abs() |> 
      apply(2, sum)

    if (min(rgb_distances) < limit) {
      color_list[which.min(rgb_distances)]
    } else {
      color
    }
  }) |> 
    as.character() # remove names
}
