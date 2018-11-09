element_table_horizontal_separator <- function(
  text_size               = 12,
  separator_height        = unit(1.2*text_size / 2, "pt"),
  separator_line_style    = 1,
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
  if (e$separator_line_size > 0) {
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
  }

  return(gt)

}
