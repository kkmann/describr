Descriptor <- function() {

  res <- structure(
    list(),
    class = c("Descriptor")
  )

  return(res)

}



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
  gt <- grobs$`__label__`

  # total column
  if (!(is.stratified.describr(dscr) & !dscr$totals)) { # total column always except opt out

    # add separator spacing
    gt <- gtable_add_cols(gt, theme$colwidths$seperators, pos = -1)

    gt <- cbind(gt, grobs$`__total__`)

  }

  # levels columns
  if (is.stratified(dscr)) {

    for (lvl in levels(group)) {

      # add separator spacing
      gt <- gtable_add_cols(gt, theme$colwidths$seperators, pos = -1)

      gt <- cbind(gt, grobs$levels[[lvl]])

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





addseparators <- function(gt, dscr) {

  theme <- dscr$theme_new



}




as.grob <- function(d, dscr, variable, group, ...) {
  UseMethod("as.grob", d)
}






labelGrob <- function(d, data_complete, width, ...) {
  UseMethod("labelGrob", d)
}





valueGrob <- function(d, data, data_subset, data_complete, width, ...) {
  UseMethod("valueGrob", d)
}
