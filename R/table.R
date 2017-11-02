dtable <- function(
  df,
  by = NULL,
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

  register_pvalue <- function() { # counter!

    pvalue_labels <- character(0)

    function(label, return = FALSE) {
      if (return) {
        return(pvalue_labels)
      }

      if (label %in% pvalue_labels) {
        return(which(label == pvalue_labels))
      } else {
        pvalue_labels <<- cbind(pvalue_labels, label)
        return(length(pvalue_labels))
      }
    }

  }

  res <- structure(
    list(
      df              = df,
      by              = by,
      core            = core,
      pvalues         = pvalues,
      totals          = totals,
      register_pvalue = register_pvalue(),
      theme_new       = theme_new
    ),
    class = "describr"
  )

  return(res)

}



is.stratified <- function(dscr, ...) {
  UseMethod("is.stratified", dscr)
}



is.stratified.describr <- function(dscr, ...) length(dscr$by) == 1





dtableGrob <- function(dscr, col_widths_tracker = NULL, padding = unit(0, "in")) {

  # create preferred-column-width tracker
  create_col_widths_tracker  <- function(dscr) { # counter!

    col_names      <- create_colnames(dscr)
    max_col_widths <- list()
    for (i in 1:length(col_names)) {
      max_col_widths[[col_names[i]]] <- unit(0, "in")
      if (length(grep("__separator", col_names[i])) == 1) {
        max_col_widths[[col_names[i]]] <- dscr$theme_new$colwidths$seperators
      }
    }

    function(width, colname, return = FALSE) {
      if (return) {
        return(max_col_widths)
      }

      if (colname %in% names(max_col_widths)) {
        max_col_widths[[colname]] <<- convertWidth(
          max(
            max_col_widths[[colname]],
            convertWidth(width + padding, "in")
          ),
        "in")
        return(NULL)
      } else {
        stop("colname unknown")
      }
    }

  }

  dscr$col_widths_tracker <- create_col_widths_tracker(dscr)

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

  # footer
  footer <- bottomGrob(dscr, gt)
  gt <- gtable_add_rows(gt, heights = convertUnit(grobHeight(footer), "in"))
  gt <- gtable_add_grob(gt, footer, nrow(gt), 1, nrow(gt), ncol(gt), name = "footer")

  attr(gt, "describr")  <- dscr
  class(gt) <- c("describr_gtable", class(gt))

  return(gt)

}




optimize_columnwidths <- function(dscr_gtable) {

  dscr <- attr(g, "describr")

  required_colwidths <- dscr$col_widths_tracker(0, 0, TRUE)

  dscr$theme_new$colwidths$variables <- required_colwidths$`__variables__`

  dscr$theme_new$colwidths$descriptors <- required_colwidths$`__descriptors__`

  dscr$theme_new$colwidths$pvalues <- required_colwidths$`__pvalues__`

  dscr$theme_new$colwidths$pvalues_idx <- required_colwidths$`__pvalues_idx__`

  # determine maximal level width
  max_level_width <- unit(0, "in")
  for (colname in names(required_colwidths)) {
    if (length(grep("__level__", colname)) == 1 | colname == "__total__") {
      max_level_width <- convertWidth(
        max(required_colwidths[[colname]], max_level_width),
        "in"
      )
    }
  }

  dscr$theme_new$colwidths$levels <- max_level_width

  return(dtableGrob(dscr))

}





create_colnames <- function(dscr) {

  theme <- dscr$theme_new

  colnames <- "__variables__"

  colnames <- c(colnames, "__descriptors__separator", "__descriptors__")

  # total column
  if (!(is.stratified.describr(dscr) & !dscr$totals)) { # total column always except opt out

    colnames <- c(colnames, "__total__separator", "__total__")

  }

  # levels columns
  if (is.stratified(dscr)) {

    lvls <- levels(dscr$df[[dscr$by]])

    for (i in 1:length(lvls)) {

      colnames <- c(colnames,
        sprintf("__level__%s__separator", lvls[i]),
        sprintf("__level__%s", lvls[i])
      )

    }

  }

  # pvalue columns
  if (dscr$pvalues & is.stratified(dscr)) {

    colnames <- c(colnames, "__pvalues__separator", "__pvalues__", "__pvalues_idx__")

  }

  return(colnames)

}
