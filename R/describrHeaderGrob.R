headerGrob <- function(dscr) { # print the header row of the table

  theme <- dscr$theme

  widths <- .getColWidths(dscr)

  col_names <- c(
    theme$header.colname.variables,
    "",
    theme$header.colname.descriptors,
    "",
    theme$header.colname.total
  )

  if (length(dscr$by) == 1) {
    lvls      <- levels(dscr$df[[dscr$by]])
    lvls_tmp  <- sapply(lvls, function(x) c("", x)) # add seperators
    col_names <- c(col_names, as.character(lvls_tmp))
    # add row for grouping variable
    g <- gtable(
      widths  = widths,
      heights = unit(c(
        theme$header.lineheight,
        theme$header.grouping.seperator.height,
        theme$header.lineheight
      ), "pt")
    )
    startCol <- .startColLevels(dscr)
    endCol   <- .endColLevels(dscr)
    width    <- sum(widths[startCol:endCol])
    g <- gtable_add_grob(g,
                         fixedWidthTextGrob(dscr$by, width,
                                            gp = gpar(), just = c("center", "center"),
                                            x = unit(.5, "npc"), y = unit(.5, "npc")
                         ), t = 1, b = 1, l = startCol, r = endCol
    )
    g <- gtable_add_grob(g,
                         linesGrob(
                           y = unit(.5, "npc"),
                           gp = gpar(lwd = dscr$theme$header.grouping.seperator.size)
                         ), t = 2, b = 2, l = startCol, r = endCol
    )
  } else {
    g <- gtable(
      widths  = widths,
      heights = unit(theme$header.lineheight, "pt")
    )
  }

  for (i in 1:length(col_names)) {
    if (i %% 2 == 1) { # otherwise just seperator, plot later
      g <- gtable_add_grob(g,
                           fixedWidthTextGrob(col_names[i], g$widths[i],
                                              gp = gpar(), just = c("center", "center"),
                                              x = unit(.5, "npc"), y = unit(.5, "npc")
                           ),
                           t = 3, b = 3, l = i, r = i
      )
    }
  }

  g <- justify(g, "center", "center")

  return(g)

}