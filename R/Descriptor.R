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

  for (i in 1:length(vars)) {
    dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], list(d))
  }

  return(dscr)

}



describe_if <- function(dscr, d, .predicate, ...) {
  UseMethod("describe_if", d)
}



describe_if.default <- function(dscr, d, .predicate, ...) {

  tmp <- sys.call()

  nc  <- list(quote(select_if), quote(dscr$df))

  for (i in 4:length(tmp)) {
    nc <- c(nc, tmp[[i]])
  }

  vars <- names(eval(as.call(nc)))

  for (i in 1:length(vars)) {
    dscr$core[[vars[i]]] <- c(dscr$core[[vars[i]]], list(d))
  }

  return(dscr)

}



labelGrob <- function(d, data, width = NULL, name = NULL, gp = NULL, vp = NULL, ...) {
  UseMethod("labelGrob", d)
}



valueGrob <- function(d, data, width = NULL, name = NULL, gp = NULL, vp = NULL, ...) {
  UseMethod("valueGrob", d)
}