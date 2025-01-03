.format_box_text <- function(text, closest_color = TRUE, wrap = 40) {
  color_boxes <- stringr::str_extract_all(text, "\\[[^]]*\\]")[[1]]
  
  replacements <- purrr::map_chr(color_boxes, \(x) {
    text_in_box <- stringr::str_remove_all(x, "\\[|\\]")

    if (text_in_box == "") return("</span>")

    # digit?
    if (text_in_box %in% 1:1000) return(stringr::str_glue("<span style='font-size: {text_in_box}pt'>"))

    if (closest_color) {
      color <- mnbplots::closest_mnb_color(text_in_box)
    } else {
      text_in_box
    }
    
    stringr::str_glue("<span style='color:{color}'>")
  })

  nms <- color_boxes |> 
    stringr::str_replace("\\[", "\\\\[") |> 
    stringr::str_replace("\\]", "\\\\]")

  names(replacements) <- nms

  text <- stringr::str_split_1(text, "\\n")   
  # force before wrap
  if (!is.null(wrap)) text <- stringr::str_wrap(text, width = wrap)
  text <- stringr::str_flatten(text, "\n")
  if (length(replacements) != 0) text <- stringr::str_replace_all(text, replacements)
  stringr::str_replace_all(text, "\\n", "<br>")
}

#' Add Formatted Text Box to ggplot
#'
#' This function adds a formatted text box to a ggplot using the ggtext package.
#'
#' @param label Text to display in the box. Can include color formatting using square brackets, e.g., "[blue]Blue text[]".
#' @param ... Additional arguments passed to ggtext::geom_richtext().
#' @param size Text size. Default is 5.
#' @param base_color Color of the box border. Default is "black".
#' @param base_fill Fill color of the box. Default is "transparent".
#' @param label_size Size of the label. Default is 1.
#' @param text.color Color of the text. Default is "black".
#' @param family Font family. Default is "Cambria".
#' @param closest_color Logical. If TRUE, uses the closest MNB color for color names. Default is TRUE.
#'
#' @return A ggplot2 layer object.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   geom_text_box(label = "Hello [blue]World[]!", x = 0, y = 0)
#'
#' @export
geom_text_box <- function(
  label = "Text \n[blue] blue", 
  ..., 
  size = 5, 
  base_color = "black",
  base_fill = "transparent",
  label_size = 1,
  text.color = "black",
  family = "Cambria",
  closest_color = TRUE) {
  ggtext::geom_richtext(
    label = .format_box_text(label, closest_color),
    ...,
    color = base_color, 
    fill = base_fill, 
    hjust = 0.5, 
    size = size, 
    family = family,
    label.size = label_size,
    text.colour = text.color
  )
}

#' Add Formatted Text Box to ggplot
#'
#' This function adds a formatted text box to a ggplot using the ggtext package.
#'
#' @param label Text to display in the box. Can include color formatting using square brackets, e.g., "[blue]Blue text[]".
#' @param size Text size. Default is 5.
#' @param ... Additional arguments passed to ggtext::geom_richtext().
#'
#' @return A ggplot2 layer object.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   text_box(label = "Hello [blue]World[]!", x = 0, y = 0)
#'
#' @export
text_box <- function(label, size = 5, ...) {
  ggplot2::ggplot(data = data.frame(x = 0, y = 0)) + 
    ggplot2::aes(x, y) +
    geom_text_box(label = label, size = size, ...) + 
    ggplot2::theme_void() + 
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
      panel.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
      panel.grid.major.x  = ggplot2::element_blank(),
      panel.grid.minor.x  = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.switch.pad.wrap = ggplot2::unit(c(0, 0, 0, 0), "lines"),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
    )
}

#' Create Multiple Text Boxes
#'
#' This function creates a ggplot with multiple formatted text boxes.
#'
#' @param chars Number of characters for the base text. Default is 30.
#' @param lines Number of lines for the base text. Default is 3.
#' @param size Text size. Default is 15.
#' @param base_color Color of the box border. Default is "black".
#' @param base_fill Fill color of the box. Default is "transparent".
#' @param labels Vector of labels for the text boxes. Default includes two example labels.
#'
#' @return A ggplot object containing multiple text boxes.
#'
#' @examples
#' text_boxes(labels = c("[blue]First box[]", "[red]Second box[]"))
#'
#' @export
text_boxes <- function(
  ...,
  chars = 40,
  lines = 4,
  size = 15,
  base_color = "black",
  base_fill = "transparent"
) {  
  text_for_base <- stringr::str_c(
    stringr::str_dup("a", chars),
    stringr::str_dup("\n", lines - 1)
  )

  # convert ... to vector names labels
  labels <- c(...)
  
  p <- text_box(label = text_for_base, 
    color = base_color, 
    fill = base_fill, 
    hjust = 0.5, 
    size = size, 
    label.size = 1,
    text.colour = "transparent"
  ) + 
    ggplot2::facet_wrap(~ z, ncol = 1)

  for (i in seq_along(labels)) {
    p <- p + 
      geom_text_box(
        data = data.frame(x = 0, y = 0, z = i), 
        label = labels[i], 
        size = size, 
        hjust = 0.5, 
        color = "transparent",
        fill = "transparent",
        text.color = "black"
      )
  }
  p
}
