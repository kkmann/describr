dtable <- function(df, by = NULL, ...) {

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
      df   = df,
      by   = by,
      core = core
    ),
    class = "describr"
  )

  return(res)
}





`+.describr` <- function(dscr, d) {

  nc <- list(quote(select), quote(dscr$df))

  for (i in 2:length(attr(d, "select_statement"))) {
    nc <- c(nc, attr(d, "select_statement")[[i]])
  }

  vars <- names(eval(as.call(nc)))

  for (i in 1:length(vars)) {
    dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], d)
  }

  return(dscr)

}





dtableGrob <- function(dscr,
  x = unit(0, "npc"), y = unit(0, "npc"),
  name = NULL, theme = NULL, vp = NULL
) {

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

  # number of rows
  n_row <- 1 # header
  for (i in 1:length(dscr$core)) {
    n_row <- n_row + length(dscr$core[[i]]) # number of descriptors per variable
  }

  g <- gtable(
    widths  = convertWidth(rep(unit(1/n_col, "npc"), n_col), unitTo = "in"),
    heights = unit(rep(0, n_col), "in")
  )

  # add header
  tmp_grob <- fixedWidthTextGrob("Variable", g$widths[1], gp = gpar(), just = c("center", "bottom"), x = unit(.5, "npc"), y = unit(0, "npc"))
  g <- gtable_add_grob(g,
    tmp_grob, t = 1, b = 1, l = 1, r = 1, clip = TRUE
  )
  g$heights[1] <- unit(max(
      convertHeight(g$heights[1], "in", valueOnly = TRUE),
      convertHeight(grobHeight(tmp_grob), "in", valueOnly = TRUE)
    ), "in"
  )
  tmp_grob <- fixedWidthTextGrob("", g$widths[2], gp = gpar(), just = c("center", "bottom"), x = unit(.5, "npc"), y = unit(0, "npc"))
  g <- gtable_add_grob(g,
    tmp_grob, t = 1, b = 1, l = 2, r = 2, clip = TRUE
  )
  g$heights[1] <- unit(max(
      convertHeight(g$heights[1], "in", valueOnly = TRUE),
      convertHeight(grobHeight(tmp_grob), "in", valueOnly = TRUE)
    ), "in"
  )
  tmp_grob <- fixedWidthTextGrob("Total", g$widths[3], gp = gpar(), just = c("center", "bottom") , x = unit(.5, "npc"), y = unit(0, "npc"))
  g <- gtable_add_grob(g,
    tmp_grob, t = 1, b = 1, l = 3, r = 3, clip = TRUE
  )
  g$heights[1] <- unit(max(
      convertHeight(g$heights[1], "in", valueOnly = TRUE),
      convertHeight(grobHeight(tmp_grob), "in", valueOnly = TRUE)
    ), "in"
  )
  if (length(dscr$by) == 1) {
    level_names <- levels(dscr$df[[dscr$by]])
    for (i in 1:length(level_names)) {
      tmp_grob <- fixedWidthTextGrob(level_names[i], g$widths[3 + i],
                                     gp = gpar(), just = c("center", "bottom"),
                                     x = unit(.5, "npc"), y = unit(0, "npc")
                                   )
      g <- gtable_add_grob(g,
        tmp_grob, t = 1, b = 1, l = 3 + i, r = 3 + i, clip = TRUE
      )
      g$heights[1] <- unit(max(
          convertHeight(g$heights[1], "in", valueOnly = TRUE),
          convertHeight(grobHeight(tmp_grob), "in", valueOnly = TRUE)
        ), "in"
      )
    }
    tmp_grob <- fixedWidthTextGrob("p value", g$widths[n_col], gp = gpar(), just = c("center", "bottom"), x = unit(.5, "npc"), y = unit(0, "npc"))
    g <- gtable_add_grob(g,
      tmp_grob, t = 1, b = 1, l = n_col, r = n_col, clip = TRUE
    )
    g$heights[1] <- unit(max(
        convertHeight(g$heights[1], "in", valueOnly = TRUE),
        convertHeight(grobHeight(tmp_grob), "in", valueOnly = TRUE)
      ), "in"
    )
  }

  # add variable labels
  row_ind <- 2
  for (i in 1:length(dscr$core)) {
    delta <- length(dscr$core[[i]]) # how many rows are added?
    tmp_grob <- fixedWidthTextGrob(names(dscr$core)[i], g$widths[1], gp = gpar(), just = c("left", "top"), x = unit(0, "npc"), y = unit(1, "npc"))
    g <- gtable_add_grob(g,
      tmp_grob, t = row_ind, b = row_ind + delta - 1, l = 1, r = 1, clip = TRUE
    )
    row_ind <- row_ind + delta
  }

  # add descriptors
  current_row <- 1
  for (i in 1:length(dscr$core)) {
    varname   <- names(dscr$core)[[i]]
    current_row <- current_row + 1 # new line for new variable
    for (j in 1:length(dscr$core[[i]])) {

      # print label grob
      d <- dscr$core[[i]][[j]]
      current_row <- current_row + j - 1

      tmp_grob <- justify(labelGrob(d, dscr$df[[varname]]), hjust="left", vjust="top")
      g <- gtable_add_grob(g,
        tmp_grob,
        t = current_row, b = current_row, l = 2, r = 2, clip = TRUE
      )
      g$heights[current_row] <- unit(max(
        convertHeight(g$heights[current_row], "in", valueOnly = TRUE),
        convertHeight(grobHeight(tmp_grob), "in", valueOnly = TRUE)
      ), "in"
      )

      # print value grob for total
      tmp_grob <- justify(valueGrob(d, dscr$df[[varname]]), hjust = "left", vjust="top")
      g <- gtable_add_grob(g,
                           tmp_grob,
          t = current_row, b = current_row, l = 3, r = 3, clip = TRUE
      )
      g$heights[current_row] <- unit(max(
        convertHeight(g$heights[current_row], "in", valueOnly = TRUE),
        convertHeight(grobHeight(tmp_grob), "in", valueOnly = TRUE)
      ), "in"
      )
    }
  }

  return(g)

}
