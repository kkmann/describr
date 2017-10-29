dtable <- function(df, by = NULL, theme = theme_default(), ...) {

  by     = as.character(substitute(by))

  if (length(by) > 1) {
    stop("currently only single stratum supported")
  }
  if (length(by) == 1 & !(by %in% names(df))) {
      stop(sprintf("stratum 'by = %s' not found"))
  }

  core <- list()
  for (i in 1:ncol(df)) {
    core[names(df)[i]] <- NULL
  }


  res <- structure(
    list(
      df    = df,
      by    = by,
      core  = core,
      theme = theme
    ),
    class = "describr"
  )

  return(res)

}



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

headerSeperatorGrob <- function(dscr) {

  g <- gtable(
    .getColWidths(dscr),
    dscr$theme$header.seperator.height
  )

  g <- gtable_add_grob(g,
    linesGrob(
      y = unit(.5, "npc"),
      gp = gpar(lwd = dscr$theme$header.seperator.size)
    ), 1, 1, 1, ncol(g)
  )

  return(g)

}

bottomSeperatorGrob <- function(dscr) {

  g <- gtable(
    .getColWidths(dscr),
    dscr$theme$bottom.seperator.height
  )

  g <- gtable_add_grob(g,
                       linesGrob(
                         y = unit(.5, "npc"),
                         gp = gpar(lwd = dscr$theme$bottom.seperator.size)
                       ), 1, 1, 1, ncol(g)
  )

  return(g)

}



variableGrob <- function(dscr, varname) {

  descriptor_list <- dscr$core[[varname]]
  gtable_list <- lapply(
    descriptor_list,
    function(d) descriptorGrob(d, dscr, varname)
  )
  g <- do.call(rbind, args = gtable_list)

  # add variable label
  g <- gtable_add_cols(g,
    widths = unit.c(
      dscr$theme$header.colwidth.variables,
      dscr$theme$colwidth.variables.seperator
    ),
    pos = 0
  )

  tmp_grob <- fixedWidthTextGrob(
    varname, g$widths[1], gp = gpar(),
    just = c("left", "top"),
    x = unit(0, "npc"), y = unit(1, "npc")
  )
  g <- gtable_add_grob(g,
    tmp_grob, t = 1, b = nrow(g), l = 1, r = 1
  )

  return(g)

}






dtableGrob <- function(dscr,
  x = unit(0, "npc"), y = unit(0, "npc"),
  name = NULL, theme = NULL, vp = NULL
) {

  g <- headerGrob(dscr)

  g <- rbind(g, headerSeperatorGrob(dscr))

  for (varname in names(dscr$core)) {
    g <- rbind(g, variableGrob(dscr, varname))
  }

  g <- rbind(g, bottomSeperatorGrob(dscr))

  return(g)

}



.getColWidths <- function(dscr) {

  theme <- dscr$theme

  widths <- unit.c(
    theme$header.colwidth.variables,
    theme$colwidth.variables.seperator,
    theme$header.colwidth.descriptors,
    theme$colwidth.variables.descriptors,
    theme$header.colwidth.total
  )

  if (length(dscr$by) == 1) {
    lvls      <- levels(dscr$df[[dscr$by]])
    widths    <- unit.c(
      widths,
      rep(unit.c(
          theme$colwidth.others.seperators,
          theme$header.colwidth.others
        ),
        length(lvls)
      )
    )
  }

  return(widths)

}

.startColVars <- function(dscr) {
  return(1)
}

.startColDesc <- function(dscr) {
  return(3)
}

.startColTotal <- function(dscr) {
  return(5)
}

.startColLevels <- function(dscr) {
  if (length(dscr$by) == 1) {
    return(7)
  } else {
    stop()
  }
}

.endColLevels <- function(dscr) {
  if (length(dscr$by) == 1) {
    lvls   <- levels(dscr$df[[dscr$by]])
    return(.startColLevels(dscr) + 2 * length(lvls) - 2)
  } else {
    stop()
  }
}
