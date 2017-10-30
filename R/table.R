dtable <- function(
  df,
  by = NULL,
  theme = theme_default(),
  theme_new = theme_default_tmp(),
  pvalues = FALSE,
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
      df        = df,
      by        = by,
      core      = core,
      pvalues   = pvalues,
      totals    = totals,
      theme     = theme,
      theme_new = theme_new
    ),
    class = "describr"
  )

  return(res)

}



is.stratified <- function(dscr, ...) {
  UseMethod("is.stratified", dscr)
}



is.stratified.describr <- function(dscr, ...) length(dscr$by) == 1





dtableGrob <- function(dscr,
  x = unit(0, "npc"), y = unit(0, "npc"),
  name = NULL, theme = NULL, vp = NULL
) {

  theme <- dscr$theme_new

  # header and header separator
  gt <- headerGrob(dscr)

  gt <- rbind(
    gt,
    element_table_grob(theme$header$style$separator_bottom, widths = gt$widths)
  )

  # variable rows and separators
  for (varname in names(dscr$core)) {

    gt <- rbind(gt, variableGrob(dscr, varname))

    if (which(varname == names(dscr$core)) < length(names(dscr$core))) { # only if not last row
      gt <- rbind(
        gt,
        element_table_grob(theme$body$style$separator_variables, widths = gt$widths)
      )
    }

  }

  # bottom separator and TODO: bottom grob
  gt <- rbind(
    gt,
    element_table_grob(theme$bottom$style$separator, widths = gt$widths)
  )

  return(gt)

}
