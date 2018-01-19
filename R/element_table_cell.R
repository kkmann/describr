element_table_grob <- function(e, content, width, dscr = NULL, colname = NULL, ...) {
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
  frame_line_style  = 0,
  grob_align        = c("center", "center")
) {

  structure(
    as.list(environment()),
    class = c("element_table_cell_text", "element_table_cell", "element_table")
  )

}


.as.text.gp <- function(e) {
  gpar(
    alpha      = e$text_transparency,
    col        = e$text_color,
    fontfamily = e$text_fontfamily,
    fontsize   = e$text_size,
    fontface   = e$text_fontface,
    lineheight = e$text_line_height / e$text_size
  )
}



element_table_grob.element_table_cell_text <- function(
  e, label, width, name,
  dscr = NULL, colname = NULL
) {

  if (!is.null(dscr) & !is.null(colname)) {
    if (!is.null(dscr$col_widths_tracker)) {

      # determine preferred colwidth
      vp_tmp <- viewport(gp = .as.text.gp(e))
      pushViewport(vp_tmp)
      preferred_width <- convertWidth(
        1.05*convertWidth(stringWidth(label), "in"), "in")
      popViewport()

      dscr$col_widths_tracker(preferred_width, colname)

    }
  }

  rows <- strsplit(
    splitString(
      label,
      availwidth = convertWidth(width - 2*e$text_padding, "in", valueOnly = TRUE),
      gp = .as.text.gp(e)
    ),
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
      i, 1, i, 1
    )
  }

  cell <- gtable(
    widths  = convertHeight(width, "in"),
    heights = convertHeight(sum(g$heights), "in")
  )

  cell <- gtable_add_grob(cell,
    g, 1, 1, 1, 1, name = paste0(name, "_fg")
  )

  cell <- gtable_add_grob(cell,
    rectGrob(
      name = paste0(name, "_bg"), gp = gpar(
        fill  = e$background_color,
        alpha = e$background_transparency,
        lty   = e$frame_line_style,
        col   = e$frame_line_color
      )
    ), 1, 1, 1, 1, z = -Inf, name = paste0(name, "_bg")
  )

  cell <- justify(
    cell, hjust = e$grob_align[[1]], vjust = e$grob_align[[2]]
  )

  return(cell)

}



element_table_wish_width <- function(e, label, ...) {
  UseMethod("element_table_wish_width", e)
}


element_table_wish_width.element_table_cell_text <- function(e, label) {

  vp_tmp <- viewport(gp = .as.text.gp(e))
  pushViewport(vp_tmp)
  res <- convertWidth(stringWidth(label), "in")
  popViewport()
  # padding?
  res <- convertWidth(res + 2*e$text_padding, "in")
  return(res)

}




element_table_cell_plot <- function(
  text_size,
  plot_height       = unit(3*1.2*text_size, "pt"),
  plot_width        = unit(.5, "in"),
  background_color  = NULL,
  background_transparency = NULL,
  frame_line_color  = NULL,
  frame_line_size   = 1,
  frame_line_style  = 0
) {

  structure(
    as.list(environment()),
    class = c("element_table_cell_plot", "element_table_cell", "element_table")
  )

}



element_table_grob.element_table_cell_plot <- function(e, plot, width, name, pd,
  dscr = NULL, colname = NULL, ...) {

  plotheight <- e$plot_height %>% to_inches()
  if (!is.null(pd$minheight)) {
    plotheight <- max(e$plot_height, pd$minheight) %>% to_inches()
  }

  if (!is.null(dscr) & !is.null(colname)) {
    if (!is.null(dscr$col_widths_tracker)) {
      # determine preferred colwidth

      if (!is.null(pd$minwidth)) {
        preferred_width <- max(e$plot_width, pd$minwidth) %>% to_inches()
      } else {
        preferred_width <- e$plot_width %>% to_inches()
      }

      dscr$col_widths_tracker(preferred_width, colname)

    }
  }

  cell <- gtable(
    widths  = width,
    heights = plotheight,
    name    = name
  )

  # add actual plot as grob, keep only panel (table too small for axis!)
  cell <- gtable_add_grob(cell,
      ggplotGrob(plot + theme_void()) %>% gtable_filter("panel"),
    1, 1, 1, 1, name = paste0(name, "_fg")
  )

  # add backgound
  cell <- gtable_add_grob(cell,
    rectGrob(
      name = paste0(name, "_bg"), gp = gpar(
        fill  = e$background_color,
        alpha = e$background_transparency,
        lty   = e$frame_line_style,
        col   = e$frame_line_color
      )
    ), 1, 1, 1, 1, z = -Inf
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
