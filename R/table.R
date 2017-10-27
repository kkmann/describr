describe <- function(df, by = NULL, ...) {

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
    widths  = rep(unit(1/n_col, "npc"), n_col),
    heights = rep(unit(1/n_row, "npc"), n_row)
  )

  # add header
  g <- gtable_add_grob(g,
    textGrob("Variable"), t = 1, b = 1, l = 1, r = 1, clip = TRUE
  )
  g <- gtable_add_grob(g,
    textGrob("Descriptor"), t = 1, b = 1, l = 2, r = 2, clip = TRUE
  )
  g <- gtable_add_grob(g,
    textGrob("Total"), t = 1, b = 1, l = 3, r = 3, clip = TRUE
  )
  if (length(dscr$by) == 1) {
    level_names <- levels(dscr$df[[dscr$by]])
    for (i in 1:length(level_names)){
        g <- gtable_add_grob(g,
          textGrob(level_names[i]), t = 1, b = 1, l = 3 + i, r = 3 + i, clip = TRUE
      )
    }
    g <- gtable_add_grob(g,
      textGrob("p value"), t = 1, b = 1, l = n_col, r = n_col, clip = TRUE
    )
  }

  # add variable labels
  row_ind <- 2
  for (i in 1:length(dscr$core)) {
    delta <- length(dscr$core[[i]]) # how many rows are added?
    g <- gtable_add_grob(g,
      textGrob(names(dscr$core)[i]), t = row_ind, b = row_ind + delta - 1, l = 1, r = 1, clip = TRUE
    )
    row_ind <- row_ind + delta
  }

  # add descriptors
  current_row <- 1
  for (i in 1:length(dscr$core)) {
    varname   <- names(dscr$core)[[i]]
    print(varname)
    current_row <- current_row + 1 # new line for new variable
    for (j in 1:length(dscr$core[[i]])) {

      # print label grob
      d <- dscr$core[[i]][[j]]
      current_row <- current_row + j - 1

      print(c(current_row, attr(d, "label")(dscr$df[[varname]])))

      g <- gtable_add_grob(g,
        labelGrob(d, dscr$df[[varname]]),
        t = current_row, b = current_row, l = 2, r = 2, clip = TRUE
      )

      # print value grob for total
      g <- gtable_add_grob(g,
        valueGrob(d, dscr$df[[varname]]),
          t = current_row, b = current_row, l = 3, r = 3, clip = TRUE
      )
    }
  }

  return(g)

}
