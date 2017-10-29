dtable <- function(
  df,
  by = NULL,
  theme = theme_default(),
  theme_new = theme_default_tmp(),
  pvalues = TRUE,
  totals = TRUE
) {

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
      pvalues = pvalues,
      totals = totals,
      theme = theme,
      them_new = theme_new
    ),
    class = "describr"
  )

  return(res)

}



is.stratified <- function(dscr, ...) {
  UseMethod("is.stratified", dscr)
}

is.stratified.describr <- function(dscr, ...) length(dscr$by) == 1



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



descriptorSeperatorGrob <- function(dscr) {

  widths <- .getColWidths(dscr)
  widths <- widths[.startColDesc(dscr):length(widths)] # dont need variable columns

  g <- gtable(
    widths,
    dscr$theme$descriptor.seperator.height
  )

  if (dscr$theme$descriptor.seperator.size > 0) {
    g <- gtable_add_grob(g,
                         linesGrob(
                           y = unit(.5, "npc"),
                           gp = gpar(lwd = dscr$theme$descriptor.seperator.size)
                         ), 1, 1, 1, ncol(g)
    )
  }

  return(g)

}



variableGrob <- function(dscr, varname) {

  descriptor_list <- dscr$core[[varname]]
  gtable_list <- lapply(
    descriptor_list,
    function(d) descriptorGrob(d, dscr, varname)
  )
  i <- 1
  while (i < length(gtable_list)) {
    gtable_list <- append(gtable_list, list(descriptorSeperatorGrob(dscr)), i)
    i <- i + 2
  }
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


variableSeperatorGrob <- function(dscr) {

  g <- gtable(
    .getColWidths(dscr),
    dscr$theme$variable.seperator.height
  )

  if (dscr$theme$variable.seperator.size > 0) {
    g <- gtable_add_grob(g,
                         linesGrob(
                           y = unit(.5, "npc"),
                           gp = gpar(lwd = dscr$theme$variable.seperator.size)
                         ), 1, 1, 1, ncol(g)
    )
  }

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
    if (which(varname == names(dscr$core)) < length(names(dscr$core))) {
      g <- rbind(g, variableSeperatorGrob(dscr))
    }
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
