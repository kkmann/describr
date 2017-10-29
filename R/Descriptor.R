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
    stop("'dscr' must be of class dscribr")
  }

  widths <- .getColWidths(dscr)
  widths <- widths[.startColDesc(dscr):length(widths)] # dont need variable columns

  if (length(dscr$by) == 1) {
    df   <- dscr$df %>% select_(varname, dscr$by) %>% group_by_(dscr$by)
    lvls <- levels(dscr$df[[dscr$by]])
  } else {
    df <- dscr$df %>% select_(varname)
  }
  g <- gtable(widths = widths, heights = unit(0, "npc"))

  grobs <- list(
    lbl   = labelGrob(d, df[[varname]]), # label uses ungrouped data
    sep   = rectGrob(), # seperator
    total = valueGrob(d, df[[varname]]) # get value for entire data set
  )
  if (length(dscr$by) > 0) {
    for (i in 1:length(levels(dscr$df[[dscr$by]]))) {
      lvl <- levels(dscr$df[[dscr$by]])[i]
      grobs <- c(grobs,
        list(rectGrob()),
        list(valueGrob(d, df[[varname]][df[[dscr$by]] == lvl]))
      )
    }
  }
  if (length(grobs) != length(widths)) {
    stop("DEBUG")
  }
  for (i in 1:length(grobs)) {
    if (i %% 2 == 1) {
      g <- gtable_add_grob(g, grobs[[i]], 1, i, 1, i) # adjust height
      g$heights <- convertHeight(
        unit.pmax(g$heights, grobHeight(grobs[[i]])),
        unitTo = "in"
      )
    }
  }

  return(g)

}




labelGrob <- function(d, data, width = NULL, name = NULL, gp = NULL, vp = NULL, ...) {
  UseMethod("labelGrob", d)
}



valueGrob <- function(d, data, width = NULL, name = NULL, gp = NULL, vp = NULL, ...) {
  UseMethod("valueGrob", d)
}