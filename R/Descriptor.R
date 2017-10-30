Descriptor <- function() {

  res <- structure(
    list(),
    class = c("Descriptor")
  )

  return(res)

}



describe <- function(dscr, d, ...) {
  UseMethod("describe", d)
}



describe.default <- function(dscr, d, ...) {

  tmp <- sys.call()

  nc  <- list(quote(select), quote(dscr$df))

  for (i in 4:length(tmp)) {
    nc <- c(nc, tmp[[i]])
  }

  vars <- names(eval(as.call(nc)))

  if (is(d, "Descriptor")) {
    d <- list(d)
  }

  for (i in 1:length(vars)) {
    dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], d)
  }

  return(dscr)

}



describe_if <- function(dscr, .predicate, d, ...) {
  UseMethod("describe_if", d)
}



describe_if.default <- function(dscr, .predicate, d, ...) {

  tmp <- sys.call()

  nc  <- list(
    quote(select_if),
    quote(dscr$df),
    tmp[[3]]
  )

  vars <- names(eval(as.call(nc)))

  if (is(d, "Descriptor")) {
    d <- list(d)
  }

  for (i in 1:length(vars)) {
    dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], d)
  }

  return(dscr)

}





descriptorGrob <- function(d, dscr, varname, ...) {
  UseMethod("descriptorGrob", d)
}



descriptorGrob.default <- function(d, dscr, varname, ...) {

  if (!is(dscr, "describr")) {
    stop("'dscr' must be of class describr")
  }

  theme <- dscr$theme_new

  # start with descriptor
  gt <- labelGrob(d, dscr$df[[varname]], theme$colwidths$descriptors)

  # total column
  if (!(is.stratified.describr(dscr) & !dscr$totals)) { # total column always except opt out

    # add separator spacing
    gt <- gtable_add_cols(gt, theme$colwidths$seperators, pos = -1)

    totals_cell <- valueGrob( # get value for entire data set
      d, dscr$df[[varname]], dscr$df[[varname]], theme$colwidths$levels
    )
    gt <- cbind(gt, totals_cell)

  }

  # levels columns
  if (is.stratified(dscr)) {

    lvls <- levels(dscr$df[[dscr$by]])

    for (i in 1:length(lvls)) {

      # add separator spacing
      gt <- gtable_add_cols(gt, theme$colwidths$seperators, pos = -1)

      lvl_cell <- valueGrob( # get value for entire data set
        d, dscr$df[[varname]][dscr$df[[dscr$by]] == lvls[i]], dscr$df[[varname]], theme$colwidths$levels
      )
      gt <- cbind(gt, lvl_cell)

    }

  }

  # pvalue columns
  if (dscr$pvalues & is.stratified(dscr)) {

    # add separator spacing
    gt <- gtable_add_cols(gt, theme$colwidths$seperators, pos = -1)

    stop("not implemented")

  }

  return(gt)

}




labelGrob <- function(d, data_complete, width, ...) {
  UseMethod("labelGrob", d)
}





valueGrob <- function(d, data, data_subset, data_complete, width, ...) {
  UseMethod("valueGrob", d)
}
