#' Create an MNB-styled GT Table
#'
#' This function creates a GT table with MNB (Magyar Nemzeti Bank) styling applied.
#' It formats numbers, percentages, and applies specific styles to the table.
#'
#' @param data A data frame or GT table object to be styled.
#' @param title Optional. A string to be used as the table title.
#' @param comment Optional. A string to be added as a source note to the table.
#'
#' @return A GT table object with MNB styling applied.
#'
#' @examples
#' library(gt)
#' data(mtcars)
#' iris |> 
#'   head() |> 
#'   gt_mnb(title = "Random title", comment = md("Source: This is a built-in dataset")) |> 
#' 
#' @export

gt_mnb <- function(data, title = NULL, comment = NULL) {
  if (is.data.frame(data)) {
    data <- gt::gt(data)
  }
  
  gt_result <- data |> 
    gt::fmt_number(tidyselect::where(is.numeric), decimals = 2) |> 
    gt::fmt_number(
      tidyselect::where(~ is.numeric(.x) && all(as.integer(.x) == .x, na.rm = TRUE)), 
      decimals = 0, sep_mark = .bigmark(), dec_mark = .decimal()
    ) |> 
    gt::fmt_percent(
      tidyselect::where(~ is.numeric(.x) && max(abs(.x), na.rm = TRUE) <= 1), 
      dec_mark = .decimal(), sep_mark = .bigmark()
    ) |> 
    gt::cols_label_with(
      fn = \(x) ifelse(stringr::str_length(x) < 5, stringr::str_to_upper(x), stringr::str_to_sentence(x))
    ) |> 
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#c1e0ec"),
        gt::cell_borders(sides = "all", color = "black", weight = px(.5)),
        gt::cell_text(font = "Calibri", align = "center")
      ),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#c1e0ec"),
        gt::cell_borders(sides = "all", color = "black", weight = px(.5)),
        gt::cell_text(font = "Calibri", align = "center", weight = "bold")
      ),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_borders(sides = "left", style = "hidden"),
      locations = list(
        gt::cells_column_labels(columns = 1), 
        gt::cells_body(columns = 1)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(sides = "right", style = "hidden"),
      locations = list(
        gt::cells_column_labels(columns = last_col()), 
        gt::cells_body(columns = last_col())
      )
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          font = "Calibri",
          align = "left", 
          weight = "bold",
          color = "black"
        )
      ),
      locations = gt::cells_title()
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          font = "Calibri",
          align = "left", 
          color = "black"
        )
      ),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          font = "Calibri",
          align = "center", 
          weight = "bold",
          color = "black"
        )
      ),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          font = "Calibri",
          style = "italic",
          color = "black"
        )
      ),
      locations = gt::cells_source_notes()
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          font = "Calibri",
          align = "left",
          weight = "bold",
          color = "black"
        ),
        gt::cell_fill(color = "#c1e0ec")
      ),
      locations = gt::cells_row_groups()
    ) |> 
    gt::tab_options(
      row_group.as_column = TRUE, 
      table.background.color = "#c1e0ec",
      table.border.top.color = "black",
      table.border.top.width = px(4),
      table.border.bottom.color = "black",
      table.border.bottom.width = px(4),
      column_labels.border.top.color = "#c1e0ec",
      column_labels.border.bottom.style = 'none',
      table_body.border.top.style = "none",
      table_body.border.bottom.style = "none",
      heading.border.bottom.style = "none",
      heading.align = 'left',
      heading.background.color = "#92cbe5",
      heading.title.font.size = px(21),
      heading.subtitle.font.size = px(21),
      table.font.size = px(21),
      table.width = px(1000),
      footnotes.font.size = px(21)
    )
  
  if (!is.null(title)) {
    gt_result <- gt_result |> 
      gt::tab_header(md(title))
  }

  if (!is.null(comment)) {
    gt_result <- gt_result |> 
      gt::tab_source_note(md(comment))
  }
  
  gt_result
}
