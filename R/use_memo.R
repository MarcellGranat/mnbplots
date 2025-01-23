#' Create a new memo from a template
#'
#' This function creates a new memo file from a specified template.
#'
#' @param filename The name of the new memo file. Default is "memo".
#' @param template The template to use for the new memo. Default is "mnb100".
#' @return None
#' @examples
#' use_memo("memo", "mnb100")
#' @export

use_memo <- function(filename = "memo", lang = "hu") {

  if (stringr::str_to_lower(lang) %in% c("hu", "hu", "hungary", "hungarian")) {
    doc_file <- "memo.docx"
    qmd_file <- "memo_template.qmd"
  } else {
    doc_file <- "memo_en.docx"
    qmd_file <- "memo_template_en.qmd"
  }


  dir.create("_extensions", showWarnings = FALSE)
  
  file.copy(
    from = .inst_file(doc_file),
    to = paste0("_extensions/", doc_file)
  )

  filename <- filename |>
    stringr::str_remove("[.].*$") |>
    stringr::str_replace("[^a-zA-Z0-9/]", "_") |>
    stringr::str_replace("[_]+", "_") |>
    stringr::str_replace("^_|_$", "_") |>
    paste0(".qmd")

  file.copy(
    from = .inst_file(qmd_file),
    to = filename
  )

  cli::cli_alert_success("{qmd_file} created successfully!")
  utils::file.edit(qmd_file)
}
