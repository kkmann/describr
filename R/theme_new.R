theme_default_tmp <- function(
  text_size = 12, lineheight = 1.2*text_size
) {
  list(
    colwidths = list(
      variables   = unit(25 * text_size / 2 / 72, "in"),
      descriptors = unit(20 * text_size / 2 / 72, "in"),
      levels      = unit(15 * text_size / 2 / 72, "in"),
      pvalues     = unit(10 * text_size / 2 / 72, "in"),
      pvalues_idx = unit( 5 * text_size / 2 / 72, "in"),
      seperators  = unit(text_size / 72, "in")
    ),
    header = theme_header_default(text_size),
    body   = theme_body_default(text_size),
    bottom = theme_bottom_default(text_size)
  )
}

theme_header_default <- function(
  text_size = 12, text_line_height = 1.2*text_size
) {
  list(
    text_size        = text_size,
    text_line_height = text_line_height,
    labels = list(
      variables   = "",
      descriptors = "",
      total       = "Total",
      pvalues     = "p value"
    ),
    style = list(
      variables   = element_table_cell_text(text_size, text_fontface = "bold"),
      descriptors = element_table_cell_text(text_size, text_fontface = "bold"),
      levels      = element_table_cell_text(text_size, text_fontface = "bold"),
      pvalues     = element_table_cell_text(text_size, text_fontface = "bold"),
      grouping    = element_table_cell_text(text_size, text_fontface = "bold"),
      separator   = element_table_horizontal_separator(text_size),
      separator_bottom = element_table_horizontal_separator(
        text_size
      )
    )
  )
}





theme_body_default <- function(
  text_size = 12, text_line_height = 1.2*text_size
) {
  list(
    text_size        = text_size,
    text_line_height = text_line_height,
    style = list(
      variables   = element_table_cell_text(text_size, text_fontface = "bold"),
      descriptors = element_table_cell_text(text_size),
      levels      = element_table_cell_text(text_size),
      pvalues     = element_table_cell_text(text_size),
      separator_variables = element_table_horizontal_separator(
        text_size,
        unit(1.5*1.2*text_size, "pt"),
        separator_line_style = 0
      )
    ),
    descriptor = theme_descriptor_default()
  )
}






theme_descriptor_default <- function(
  text_size = 12, text_line_height = 1.2*text_size
) {
  list(
    text_size        = text_size,
    text_line_height = text_line_height,
    style = list(
      label_cells = element_table_cell_text(text_size, text_align = c("right", "top")),
      value_cells = element_table_cell_text(text_size, text_align = c("right", "center")),
      plot_cell   = element_table_cell_plot(
        text_size,
        background_color = "lightgrey"
      ),
      pvalues     = element_table_cell_text(text_size, text_align = c("right", "top")),
      pval_idx    = element_table_cell_text(text_size/1.5, text_align = c("left", "top")),
      separator   = element_table_horizontal_separator(text_size, separator_line_style = 0)
    )
  )
}






theme_bottom_default <- function(
  text_size = 12, text_line_height = 1.2*text_size
) {
  list(
    text_size        = text_size,
    text_line_height = text_line_height,
    style = list(
      separator = element_table_horizontal_separator(
        text_size
      )
    )
  )
}
