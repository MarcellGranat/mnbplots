.inst_file <- function(filename) {
  system.file(filename, package = "mnbplots")
}

#' Create a new slide from a template
#'
#' This function creates a new slide file from a specified template.
#'
#' @param filename The name of the new slide file. Default is "slide".
#' @param template The template to use for the new slide. Default is "mnb100".
#' @return None
#' @examples
#' use_slide("slide", "mnb100")
#' @export
use_slide <- function(filename = "slide", template = "mnb100") {
  available_templates <- list.files(.inst_file(""), pattern = ".pptx") |>
    stringr::str_remove(".pptx")

  if (template %in% available_templates) {
    dir.create("_extensions", showWarnings = FALSE)
    file.copy(
      from = .inst_file(paste0(template, ".pptx")),
      to = paste0("_extensions/", template, ".pptx")
    )
  } else {
    cli::cli_abort("{.code template} should be
      one of {.val {available_templates}}")
  }

  filename <- filename |>
    stringr::str_remove("[.].*$") |>
    stringr::str_replace("[^a-zA-Z0-9/]", "_") |>
    stringr::str_replace("[_]+", "_") |>
    stringr::str_replace("^_|_$", "_") |>
    paste0(".qmd")

  file.copy(
    from = .inst_file("template.qmd"),
    to = filename
  )

  if (!file.exists(filename)) {
    cli::cli_alert_success("{.filename} created successfully!")
  }
}
