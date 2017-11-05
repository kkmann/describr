Descriptor <- function() {

  res <- structure(
    list(
      pvalue = function(variable, group) NA
    ),
    class = c("Descriptor")
  )

  return(res)

}


#' Specify descriptors for a table
#'
#' \code{describe} can be used to specify \emph{which} variables should be
#' described \emph{how}.
#'
#' @param descr \code{\link{describr}} object; the specified descriptors will
#'   be added for the selected variables in \code{dscr}.
#' @param with list of descriptors to be used with the selected variables.
#' @param ... one or more unquoted expressions separated by commas passed to
#'   \code{\link[dplyr]{dplyr}}'s \code{\link[dplyr]{select}}.
#'   All descriptors specified in \code{with} are used for the selected variables.
#'   Currently not used for \code{describe_if}.
#'
#' @return An object of class \code{\link{describr}} where the defined descriptors
#'   are added to the specified variables.
#'
#' @name describe
#' @export
describe <- function(dscr, with, ...) {
  UseMethod("describe", with)
}



describe.default <- function(dscr, with, ...) {

  tmp <- sys.call()

  nc  <- list(quote(select), quote(dscr$df))

  for (i in 4:length(tmp)) {
    nc <- c(nc, tmp[[i]])
  }

  vars <- names(eval(as.call(nc)))

  if (is(with, "Descriptor")) {
    with <- list(with)
  }

  for (i in 1:length(vars)) {
    dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], with)
  }

  return(dscr)

}





#' \code{describe_if} allows to to specify variables by using predicates as in
#' \code{\link[dplyr]{select_if}}.
#'
#' @inheritParams describe
#' @param .predicate A predicate function passed to \code{\link[dplyr]{select_if}}.
#'
#'
#' @name describe
#' @export
describe_if <- function(dscr, .predicate, with, ...) {
  UseMethod("describe_if", with)
}


describe_if.default <- function(dscr, .predicate, with, ...) {

  tmp <- sys.call()

  nc  <- list(
    quote(select_if),
    quote(dscr$df),
    tmp[[3]]
  )

  vars <- names(eval(as.call(nc)))

  if (is(with, "Descriptor")) {
    with <- list(with)
  }

  for (i in 1:length(vars)) {
    dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], with)
  }

  dscr$core[[dscr$by]] <- NULL # always exclude grouping variable!

  return(dscr)

}





descriptorGrob <- function(d, dscr, varname, ...) {
  UseMethod("descriptorGrob", d)
}



descriptorGrob.default <- function(d, dscr, varname, ...) {

  if (!is(dscr, "describr")) {
    stop("'dscr' must be of class describr")
  }

  theme    <- dscr$theme_new

  group    <- dscr$df[[dscr$by]]
  variable <- dscr$df[[varname]]

  grobs    <- as.grob(d, dscr, variable, group)

  # start with descriptor
  gt <- gtable(
    widths  = theme$colwidths$descriptors,
    heights = convertHeight(grobHeight(grobs$`__label__`), "in")
  )
  gt <- gtable_add_grob(gt, grobs$`__label__`, 1, 1, name = "label", clip = "on")

  # total column
  if (!(is.stratified(dscr) & !dscr$totals)) { # total column always except opt out

    # add separator spacing
    gt <- gtable_add_cols(gt,
      unit.c(theme$colwidths$seperators, theme$colwidths$levels),
      pos = -1
    )

    gt <- gtable_add_grob(gt,
      grobs$`__total__`, 1, ncol(gt), name = "total"
    )
    gt$heights <- convertHeight(max(gt$heights, grobHeight(grobs$`__total__`)), "in")

  }

  # levels columns
  if (is.stratified(dscr)) {

    for (lvl in levels(group)) {

      # add separator spacing
      gt <- gtable_add_cols(gt,
        unit.c(theme$colwidths$seperators, theme$colwidths$levels),
        pos = -1
      )

      gt <- gtable_add_grob(gt,
        grobs$levels[[lvl]], 1, ncol(gt), name = lvl
      )
      gt$heights <- convertHeight(max(gt$heights, grobHeight(grobs$levels[[lvl]])), "in")

    }

  }

  # pvalue columns
  if (dscr$pvalues & is.stratified(dscr)) {

    # add separator spacing
    gt <- gtable_add_cols(gt, theme$colwidths$seperators, pos = -1)

    # compute pvalue (actual number)
    p       <- d$pvalue(variable, group)
    p_label <- attr(d$pvalue, "label")
    dscr$register_pvalue(p_label)

    new_element <- element_table_grob(
      theme$body$descriptor$style$pvalues,
      label = sprintf("%.3f", p),
      width = theme$colwidths$pvalues,
      name  = "pvalue",
      dscr = dscr,
      colname = "__pvalues__"
    )
    gt <- gtable_add_cols(gt, widths = theme$colwidths$pvalues, pos = -1)
    gt <- gtable_add_grob(gt,
      new_element, t = 1, ncol(gt), nrow(gt), ncol(gt)
    )


    new_element <- element_table_grob(
      theme$body$descriptor$style$pval_idx,
      label   = sprintf("(%i)", dscr$register_pvalue(p_label)),
      width   = theme$colwidths$pvalues_idx,
      name    = "pvalue_index",
      dscr    = dscr,
      colname = "__pvalues_idx__"
    )
    gt <- gtable_add_cols(gt, widths = theme$colwidths$pvalues_idx, pos = -1)
    gt <- gtable_add_grob(gt,
      new_element, t = 1, ncol(gt), nrow(gt), ncol(gt)
    )

  }

  # gt <- gtable_add_grob(gt,
  #   rectGrob(gp = gpar(alpha = 0.25)), 1, 1, nrow(gt), ncol(gt), z = Inf
  # )

  return(gt)

}
