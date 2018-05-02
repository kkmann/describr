Descriptor <- function() {

  res <- structure(
    list(
      pvalues = function(variable, group) NA
    ),
    class = c("Descriptor")
  )

  return(res)

}


# is called before invoking a descriptor for the first time,
# use this to modify setting e.g. xlim for plots on global scale
setup <- function(d, variable, group, ...) {
  UseMethod("setup", d)
}

setup.default <- function(d, variable, group, ...) {
  d
}

get_label <- function(d, variable_all, ...) {
  UseMethod("get_label", d)
}

get_label.default <- function(d, ...) {
  d$label
}

get_description <- function(d, variable_group, variable_all, ...) {
  UseMethod("get_description", d)
}

get_pvalues <- function(d, variable_all, group, ...) {
  UseMethod("get_pvalues", d)
}

get_pvalues.default <- function(d, variable_all, group) {

  df <- data_frame(
    variable = variable_all,
    group    = group
  )

  pvalues <- list()

  if (length(d$pvalues) > 0) {

    for (i in 1:length(d$pvalues)) {
      pval <- d$pvalues[[i]]
      lbl <- label(pval, df, "variable", "group")
      pvalues[[lbl]] <- compute_pvalue(pval, df, "variable", "group")
    }

  } else {

    return(NULL)

  }

  return(pvalues)

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
#' @param APPEND [=FALSE] flag indicating whether previously defined descriptors
#'   should be overwritten.
#'
#' @return An object of class \code{\link{describr}} where the defined descriptors
#'   are added to the specified variables.
#'
#' @examples
#' library(dplyr)
#' iris %>%
#' describr(by = Species) %>%
#' describe(with = dscr_mean_sd(), contains("Sepal."))
#'
#' iris %>%
#' describr(by = Species) %>%
#' describe(with = dscr_mean_sd(), contains("Sepal.")) %>%
#' describe(with = dscr_mean(), Sepal.Length) # overwrites previous choice completely
#'
#' iris %>%
#' describr(by = Species) %>%
#' describe(with = dscr_mean_sd(), contains("Sepal.")) %>%
#' describe(with = dscr_mean(), Sepal.Length, APPEND = TRUE) # keeps previous choice
#'
#' @name describe
#' @export
describe <- function(dscr, with, append = FALSE, ...) {
  UseMethod("describe", with)
}



describe.default <- function(dscr, with, ..., APPEND = FALSE) {

  # extract variable names as character vector
  vars <- dplyr::select(dscr$df, ...) %>%
    names()

  if (is(with, "Descriptor")) {
    with <- list(with)
  } else {
    stop("'with' must be of class 'Descriptor'")
  }

  if (APPEND) {
    for (i in 1:length(vars)) {
      dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], with)
    }
  } else {
    # overwrite previous descriptors
    for (i in 1:length(vars)) {
      dscr$core[[vars[i]]] <- with
    }
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

  vars <- dplyr::select_if(dscr$df, .predicate = .predicate, ...) %>%
    names()

  if (is(with, "Descriptor")) {
    with <- list(with)
  }

  for (i in 1:length(vars)) {
    dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], with)
  }

  if (is.stratified(dscr)) {
    dscr$core[[dscr$by]] <- NULL # always exclude grouping variable!
  }

  return(dscr)

}




as_grob_list <- function(d, dscr, varname, ...) {
  UseMethod("as_grob_list", d)
}

as_grob_list.Descriptor <- function(td, dscr, varname) {

  variable <- dscr$df[[varname]]

  group <- factor(rep("__total__", length(variable)))

  if (is.stratified(dscr)) {
    group  <- dscr$df[[dscr$by]]
  }

  as_gtable(td, dscr, variable, group)

}



descriptorGrob <- function(d, dscr, varname, ...) {
  UseMethod("descriptorGrob", d)
}

descriptorGrob.default <- function(d, dscr, varname, ...) {

  if (!is(dscr, "describr")) {
    stop("'dscr' must be of class describr")
  }

  theme    <- dscr$theme_new

  variable <- dscr$df[[varname]]
  if (is.stratified(dscr)) {
    group  <- dscr$df[[dscr$by]]
  } else {
    group <- factor(rep("__total__", length(variable)))
  }

  # allow the descriptor to set global parameters
  d <- setup(d, variable, group, ...)

  grobs    <- as_grob_list(d, dscr, varname)

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

      if (is.null(grobs$levels[[lvl]])) {
        stop(sprintf("variable %s does not have level %s, drop empty?", varname, lvl))
      }
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

    # compute pvalues
    pvals <- get_pvalues(d, variable, group)

    if (length(pvals) == 0) {

      gt_pvalues <- gtable(
        heights = rep(unit(0, "in"), 1),
        widths = unit.c(theme$colwidths$pvalues, theme$colwidths$pvalues_idx)
      )

    } else {

      gt_pvalues <- gtable(
        heights = rep(unit(0, "in"), length(pvals)),
        widths = unit.c(theme$colwidths$pvalues, theme$colwidths$pvalues_idx)
      )

      for (i in 1:length(pvals)) {

        dscr$register_pvalue(names(pvals)[i])

        pval_element <- element_table_grob(
          theme$body$descriptor$style$pvalues,
          label = sprintf("%.3f", pvals[[i]]),
          width = theme$colwidths$pvalues,
          name  = "pvalue",
          dscr = dscr,
          colname = "__pvalues__"
        )
        gt_pvalues <- gtable_add_grob(gt_pvalues, pval_element, i, 1, name = "pvalue")
        gt_pvalues$heights[i] <- max(gt_pvalues$heights[i], grobHeight(pval_element)) %>% to_inches()

        idx_element <- element_table_grob(
          theme$body$descriptor$style$pval_idx,
          label   = sprintf("(%i)", dscr$register_pvalue(names(pvals)[i])),
          width   = theme$colwidths$pvalues_idx,
          name    = "pvalue_index",
          dscr    = dscr,
          colname = "__pvalues_idx__"
        )
        gt_pvalues <- gtable_add_grob(gt_pvalues, idx_element, i, 2, name = "pvalue_idx")
        gt_pvalues$heights[i] <- max(gt_pvalues$heights[i], grobHeight(pval_element)) %>% to_inches()

      }

    }

    if (length(pvals) > 1) {

      # need to add dummy rows!
      stop("multiple pvalues not yet supported")

    }

    gt <- cbind(gt, gt_pvalues)

  }

  return(gt)

}
