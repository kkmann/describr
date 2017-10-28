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



.ncol <- function(dscr) {

  # number of columns
  n_col <- 2 # variables / statistics
  if (length(dscr$by) == 0) {
    n_col <- n_col + 1 # no groups, no p values only 'total'
  }
  if (length(dscr$by) == 1) {
    n_col <- n_col + length(levels(dscr$df[[dscr$by]])) + 2 # groups + total + p values
  }
  if (length(dscr$by) > 1) {
    stop("length(by) > 1")
  }

  return(n_col)

}




headerGrob <- function(dscr) { # print the header row of the table

  theme <- dscr$theme

  widths <- unit.c(
    theme$header.colwidth.variables,
    theme$header.colwidth.descriptors,
    theme$header.colwidth.total
  )

  col_names <- c(
    theme$header.colname.variables,
    theme$header.colname.descriptors,
    theme$header.colname.total
  )
  if (length(dscr$by) == 1) {
    lvls      <- levels(dscr$df[[dscr$by]])
    col_names <- c(col_names, lvls)
    widths    <- unit.c(
      widths,
      rep(theme$header.colwidth.others, length(lvls))
    )
  }

  n_col <- length(col_names)

  g <- gtable(
    widths  = widths,
    heights = unit(theme$header.lineheight, "pt")
  )

  for (i in 1:length(col_names)) {
    g <- gtable_add_grob(g,
      fixedWidthTextGrob(col_names[i], g$widths[i],
        gp = gpar(), just = c("center", "center"),
        x = unit(.5, "npc"), y = unit(.5, "npc")
      ),
      t = 1, b = 1, l = i, r = i
    )
  }

  g <- justify(g, "center", "center")

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
  g <- gtable_add_cols(g, widths = dscr$theme$header.colwidth.variables, pos = 0)

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

  for (varname in names(dscr$core)) {
    g <- rbind(g, variableGrob(dscr, varname))
  }

  return(g)

}
