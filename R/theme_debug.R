theme_debug <- function(
  text_size = 12,
  lineheight = 1.2*text_size
) {
  list(
    colwidths = list(
      variables   = unit(25 * text_size / 2 / 72, "in"),
      descriptors = unit(20 * text_size / 2 / 72, "in"),
      levels      = unit(15 * text_size / 2 / 72, "in"),
      pvalues     = unit(10 * text_size / 2 / 72, "in"),
      pvalues_idx = unit( 5 * text_size / 2 / 72, "in"),
      seperators  = unit(text_size / 72 * 2/3, "in")
    ),
    header = theme_header_debug(text_size),
    body   = theme_body_debug(text_size),
    bottom = theme_bottom_debug(text_size)
  )
}

theme_header_debug <- function(
  text_size = 12, text_line_height = 1.2*text_size
) {
  text_default = element_table_cell_text(
    text_size,
    text_fontface = "bold",
    grob_align = c("center", "top"),
    frame_line_style = 2,
    frame_line_color = "red",
    background_transparency = .5
  )
  list(
    text_size        = text_size,
    lineheight       = 1.2*text_size,
    labels = list(
      variables   = "",
      descriptors = "",
      total       = "Total",
      pvalues     = "p value"
    ),
    style = list(
      variables   = text_default,
      descriptors = text_default,
      levels      = text_default,
      pvalues     = text_default,
      grouping    = text_default,
      separator   = element_table_horizontal_separator(text_size),
      separator_bottom = element_table_horizontal_separator(text_size)
    )
  )
}





theme_body_debug <- function(
  text_size = 12, text_line_height = 1.2*text_size
) {
  text_default = element_table_cell_text(
    text_size,
    frame_line_style = 2,
    frame_line_color = "red",
    background_transparency = .5
  )
  list(
    text_size        = text_size,
    text_line_height = text_line_height,
    style = list(
      variables   = element_table_cell_text(
        text_size,
        text_fontface = "bold",
        text_align = c("left", "center"),
        grob_align = c("center", "top"),
        frame_line_style = 2,
        frame_line_color = "red",
        background_transparency = .5
      ),
      descriptors = text_default,
      levels      = text_default,
      pvalues     = text_default,
      separator_variables = element_table_horizontal_separator(
        text_size,
        unit(1.5*1.2*text_size, "pt"),
        separator_line_style = 0
      )
    ),
    descriptor = theme_descriptor_debug()
  )
}






theme_descriptor_debug <- function(
  text_size = 12, text_line_height = 1.2*text_size
) {
  text_default = element_table_cell_text(
    text_size,
    frame_line_style = 2,
    frame_line_color = "red",
    background_transparency = .5
  )
  list(
    text_size        = text_size,
    text_line_height = text_line_height,
    style = list(
      label_cells = element_table_cell_text(
        text_size,
        text_align = c("right", "center"),
        grob_align = c("center", "top"),
        frame_line_style = 2,
        frame_line_color = "red",
        background_transparency = .5
      ),
      value_cells = element_table_cell_text(
        text_size,
        text_align = c("right", "center"),
        frame_line_style = 2,
        frame_line_color = "red",
        background_transparency = .5
      ),
      plot_cell   = element_table_cell_plot(
        text_size,
        background_color = "grey",
        frame_line_style = 2,
        frame_line_color = "red",
        background_transparency = .5
      ),
      pvalues     = element_table_cell_text(
        text_size,
        text_align = c("right", "center"),
        grob_align = c("center", "top"),
        frame_line_style = 2,
        frame_line_color = "red",
        background_transparency = .5
      ),
      pval_idx    = element_table_cell_text(
        text_size/2,
        text_align = c("left", "top"),
        grob_align = c("left", "top"),
        frame_line_style = 2,
        frame_line_color = "red",
        background_transparency = .5
      ),
      separator   = element_table_horizontal_separator(text_size, separator_line_style = 0)
    )
  )
}






theme_bottom_debug <- function(
  text_size = 12, text_line_height = 1.2*text_size
) {
  list(
    text_size        = text_size,
    text_line_height = text_line_height,
    style = list(
      footnote  = element_table_cell_text(
        text_size/2,
        text_padding = unit(text_size, "pt"),
        text_align = c("left", "top"),
        grob_align = c("center", "top"),
        frame_line_style = 2,
        frame_line_color = "red",
        background_transparency = .5
      ),
      separator = element_table_horizontal_separator(
        text_size
      )
    )
  )
}
