element_table_grob <- function(e, content, width, ...) {
  UseMethod("element_table_grob", e)
}





element_table_cell_text <- function(
  text_size,
  text_line_height  = 1.2*text_size,
  text_color        = NULL,
  text_transparency = NULL,
  text_fontfamily   = NULL,
  text_fontface     = NULL,
  text_align        = c("center", "center"),
  text_rotation     = 0,
  text_padding      = unit(0, "cm"),
  background_color  = NULL,
  background_transparency = NULL,
  frame_line_color  = NULL,
  frame_line_size   = 1,
  frame_line_style  = 0
) {

  structure(
    as.list(environment()),
    class = c("element_table_cell_text", "element_table_cell", "element_table")
  )

}



element_table_grob.element_table_cell_text <- function(e, label, width, name) {

  rows <- strsplit(
    splitString(label, availwidth = convertWidth(width - 2*e$text_padding, "in", valueOnly = TRUE)),
    "\n"
  )[[1]]

  if (length(rows) == 0) {
    rows <- ""
  }

  g <- gtable(
    widths = width,
    heights = rep(unit(e$text_line_height, "pt"), length(rows)),
    name = name
  )

  for (i in 1:length(rows)) {
    g <- gtable_add_grob(g,
      justify(textGrob(rows[i],
        check.overlap = FALSE, name = paste0(name, "_line_", i, "_fg"), gp = gpar(
            alpha      = e$text_transparency,
            col        = e$text_color,
            fontfamily = e$text_fontfamily,
            fontsize   = e$text_size,
            fontface   = e$text_fontface,
            lineheight = e$text_line_height / e$text_size
          ), rot = e$text_rotation
        ),
        hjust = e$text_align[[1]], vjust = e$text_align[[2]], hpadding = e$text_padding
      ),
      i, 1, i, 1, -Inf
    )
  }

  cell <- gtable_add_grob(g,
    rectGrob(
      name = paste0(name, "_bg"), gp = gpar(
        fill  = e$background_color,
        alpha = e$background_transparency,
        lty   = e$frame_line_style,
        col   = e$frame_line_color
      )
    ), 1, 1, nrow(g), 1, z = Inf
  )

  return(cell)

}





element_table_horizontal_separator <- function(
  text_size               = 12,
  separator_height        = unit(1.2*text_size / 2, "pt"),
  separator_line_size     = 1,
  separator_color         = NULL,
  separator_transparency  = NULL,
  background_color        = NULL,
  background_transparency = NULL,
  frame_line_color        = NULL,
  frame_line_size         = 1,
  frame_line_style        = 0
) {

  structure(
    as.list(environment()),
    class = c("element_table_horizontal_separator", "element_table")
  )

}



element_table_grob.element_table_horizontal_separator <- function(
  e, widths = unit(1, "npc"),
  label = NULL, width = NULL, name = "horizontal_separator"
) {

  gt <- gtable(
    widths  = widths,
    heights = e$separator_height,
    name    = name
  )

  # add bg
  gt <- gtable_add_grob(gt,
    rectGrob(
      name = paste0(name, "_bg"),
      gp = gpar(
        fill  = e$background_color,
        alpha = e$background_transparency,
        lty   = e$frame_line_style,
        col   = e$frame_line_color
      )
    ), 1, 1, 1, ncol(gt)
  )

  # add line
  gt <- gtable_add_grob(gt,
    linesGrob(
      y    = unit(0.5, "npc"),
      name = paste0(name, "_line"),
      gp   = gpar(
        alpha = e$separator_transparency,
        lty   = e$separator_line_style,
        col   = e$separator_line_color
      )
    ), 1, 1, 1, ncol(gt), -Inf
  )

  return(gt)

}
