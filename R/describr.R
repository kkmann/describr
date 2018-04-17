#' @title describr: making univariate descriptive tables fun in R
#'
#' @description This package provides functionality to define descriptive tables
#' with minimum
#' effort and human-readable code using non-standard evaluation.
#' It is designed to work well with the tidyverse and heavily relies on
#' functionality of \code{\link[dplyr]{select}} and
#' \code{\link[dplyr]{select_if}} of the \code{\link[dplyr]{dplyr}} package.
#'
#' The central \code{\link{describr}} object is the basis for defining a
#' table.
#'
#' @docType package
#' @name describr-package
NULL





#' \code{describr} object
#'
#' This is the core object for \code{describr} package.
#' All information required to create a descriptive table for a data frame is
#' stored a \code{describr}-object.
#'
#' @param df a data frame.
#' @param by unquoted name of optional stratifying factor
#' @param theme_new theme to use
#' @param pvalus Flag indicating whether p-values for homogeneity null hypothesis
#'   should be printed if available.
#' @param total Flag indicating whether all descriptors should also be given
#'   for the total sample.
#'
#' @return An object of class \code{describr < list} with fields:
#'   \describe{
#'      \item{\code{df}}{data frame to describe}
#'      \item{\code{by}}{variable name by which to stratify the description
#'        (character or NULL)}
#'      \item{\code{core}}{named list with names corresponding
#'        to the variables in \code{df} which will b described and eac element
#'        is a list of \code{descriptor} objects osed to describe that variable.
#'        TODO: rename!}
#'      \item{\code{group_descriptors}}{list of descriptors used for the grouping
#'        (stratifying) variable \code{by}.}
#'      \item{\code{pvalues}}{Boolean flag indicating whether p-values should be
#'        displayed}
#'      \item{\code{totals}}{Boolean flag indicating whether a total column
#'        should be included when the description is stratified by a grouping
#'        factor.}
#'      \item{\code{theme_new}}{The theme to use for plotting. TODO: rename}
#'   }
#'
#' @export
describr <- function(
  df,
  by        = NULL,
  theme_new = theme_default(),
  pvalues   = FALSE,
  totals    = TRUE,
  caption   = ""
) {

  # capture stratum using non-standard evaluation
  by = as.character(substitute(by))

  # ensure stratum fits
  if (length(by) > 1) stop("currently only single stratum supported")

  if (length(by) == 1) {
    if (!(by %in% names(df))) stop(sprintf("stratum 'by = %s' not found"))

    if (!(is.factor(df[[by]]))) stop(sprintf("'by' must be factor"))
  }

  # create list of descriptors for stratum
  group_descriptors <- list(dscr_n_perc())

  # create list of to hold variabe descrptors
  core <- list()

  # collect all items and add class label
  res <- structure(
    list(
      df                = df,
      by                = by,
      core              = core,
      group_descriptors = group_descriptors,
      pvalues           = pvalues,
      totals            = totals,
      theme_new         = theme_new,
      caption           = caption
    ),
    class = c("describr", "list")
  )

  return(res)

}





#' Make object plottable using \code{grid}
#'
#' \code{as_gtable} turns an object in a \code{gtable} which can be plotted using
#' \code{\link[grid]{grid.draw}}.
#'
#' @param x object to plot.
#'
#' @return A \code{gtable}.
#'
#' @export
as_gtable <- function(x, ...) {
  UseMethod("as_gtable", x)
}




# use print to plot instantly
print.describr <- function(x, ...) {

  grid.newpage()

  as_gtable(x) %>%
    grid.draw()

}

# use print to plot instantly
print.describrGtable <- function(x, ...) {

  grid.newpage()

  grid.draw(x)

}





#' Turns a 'describr' object into a \code{\link[gtable]{gtable}}
#' (actually a \code{describrGtable < gtable}) ready to be plotted using
#' \code{grid.draw()).
#'
#' @name as_gtable
#' @export
as_gtable.describr <- function(dscr, ...) {

  pdf("12345678910.pdf")

  # legacy, should be safe to remove later
  widths_padding_fct = 1.00

  # create preferred-column-width tracker which can be used by table
  # elements to report their preferred widths
  # can be used to determine optimal column widths in a second pass
  dscr$col_widths_tracker <- create_col_widths_tracker(dscr, widths_padding_fct)

  # this counter tracks the used p-values to be noted in the footer
  dscr$register_pvalue <- register_pvalue()

  # abbrevite the theme
  theme <- dscr$theme_new

  # start with header and separator
  gt <- headerGrob(dscr)
  gt <- rbind(gt, element_table_grob(
    theme$header$style$separator_bottom, widths = gt$widths)
  )
  rownames(gt) <- sapply(1:nrow(gt), function(i) sprintf("__header__%i", i))

  # variable rows and separators
  for (varname in names(dscr$core)) {

    vargrob <- variableGrob(dscr, varname)
    rownames(vargrob) <- sapply(1:nrow(vargrob), function(i) sprintf("__variable__%s_%i", varname, i))
    gt <- rbind(gt, vargrob)

    if (which(varname == names(dscr$core)) < length(names(dscr$core))) { # only if not last row
      gt <- rbind(gt,
        element_table_grob(theme$body$style$separator_variables, widths = gt$widths)
      )
    }
    rownames(gt) <- c(rownames(gt), sprintf("__variable__%s_separator", varname))

  }

  # footer
  footer <- bottomGrob(dscr, gt, dscr$caption)
  rownames(footer) <- sapply(1:nrow(footer), function(i) sprintf("__footer__%i", i))
  gt <- gtable_add_rows(gt, heights = convertUnit(grobHeight(footer), "in"))
  gt <- gtable_add_grob(gt, footer, nrow(gt), 1, nrow(gt), ncol(gt), name = "footer")


  # store the initial describer object as attribute
  attr(gt, "describr")  <- dscr
  class(gt) <- c("describrGtable", class(gt))

  dev.off()
  unlink("12345678910.pdf")

  return(gt)

}
