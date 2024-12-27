#' Modify SVG Fonts
#'
#' This function modifies the font family and font color in an SVG file.
#'
#' @param input_file A character string specifying the path to the input SVG file.
#' @param output_file A character string specifying the path to the output SVG file. If NULL, the input file will be overwritten.
#' @param family A character string specifying the font family to use. Default is "Calibri".
#' @param font_color A character string specifying the font color to use. Default is "#333333".
#'
#' @return None. The function writes the modified SVG content to the output file.
#' @export
#'

modify_svg_fonts <- function(input_file, output_file = NULL, family = "Calibri", font_color = "#333333") {
  # Read the file
  content <- readLines(input_file, warn = FALSE, encoding = "UTF-8")
  content <- paste(content, collapse = "\n")
  if (is.null(output_file)) {
    output_file <- input_file
  }
  
  # Replace font families with the specified family
  content <- gsub(
    'font-family:\\s*"[^"]*"', 
    paste0('font-family: "', family, '"'),
    content
  )

  content <- gsub(
    'font-family:\\s*[^,;"]*(?=[,;]|\\s*\\})', 
    paste0("font-family: ", family), 
    content, 
    perl = TRUE
  )
  
  # Replace fill color in <text> or <g id="tx elements
  text_pattern <- '(<(?:text|g id="tx[^"]*")[^>]*)(fill="[^"]*"|fill:\\s*[^;"]*)'
  content <- gsub(text_pattern, paste0('\\1fill="', font_color, '"'), content, perl = TRUE)
  
  style_pattern <- '(<(?:text|g id="tx[^"]*")[^>]*style="[^"]*?)(fill:\\s*[^;"]*)'
  content <- gsub(style_pattern, paste0('\\1fill="', font_color, '"'), content, perl = TRUE)
  
  # Write the modified content to the output file
  writeLines(content, output_file, useBytes = TRUE)
}