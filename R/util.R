create_colnames <- function(dscr) {


  colnames <- "__variables__"

  colnames <- c(colnames, "__descriptors__separator", "__descriptors__")

  # total column
  if (!(is.stratified(dscr) & !dscr$totals)) { # total column always except opt out

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





create_col_widths_tracker  <- function(dscr, widths_padding_fct) { # counter!

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
          convertWidth(width * widths_padding_fct, "in")
        ),
        "in")
      return(NULL)
    } else {
      stop(sprintf("colname '%s' unknown", colname))
    }
  }

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




# true if dscr has non-null list of strata
is.stratified <- function(dscr, ...) length(dscr$by) == 1




justify <- function(x, hjust="center", vjust="center",
                    hpadding = unit(0, "cm"), vpadding = unit(0, "cm")){

  w <- grobWidth(x) #sum(x$widths)
  h <- grobHeight(x) #sum(x$heights)
  xj <- switch(hjust,
               center = 0.5,
               left = 0.5*w + hpadding,
               right=grid::unit(1,"npc") - 0.5*w - hpadding)
  yj <- switch(vjust,
               center = 0.5,
               bottom = 0.5*h + vpadding,
               top=grid::unit(1,"npc") - 0.5*h - vpadding)
  x$vp <- grid::viewport(x=xj, y=yj)
  return(x)
}

# based on paul murell / rgraphics
splitString <- function(text, availwidth = 1, gp = gpar()) {

  vp_tmp <- viewport(gp = gp)
  pushViewport(vp_tmp)

  strings   <- strsplit(text, " ")[[1]]
  newstring <- ""
  linewidth <- convertWidth(stringWidth(newstring), "in", valueOnly = TRUE)
  gapwidth  <- convertWidth(stringWidth(" "), "in", valueOnly = TRUE)

  i <- 1
  while (i <= length(strings)) {

    width <- convertWidth(stringWidth(strings[i]), "in", valueOnly = TRUE)

    if (width > availwidth) { # word is too wide, split

      old_length <- length(strings)
      if (i == 1) {
        strings <- c("", strings)
      }
      if (i == old_length & i > 1) {
        strings <- c(strings[1:(length(strings) - 1)], "", strings[length(strings)])
      }
      if (i > 1 & i < old_length) {
        strings <- c(strings[1:(i - 1)], "", strings[i], strings[(i + 1):length(strings)])
      }
      j <- 0
      while (j <= nchar(strings[i + 1]) - 1 & convertWidth(stringWidth(paste0(substring(strings[i + 1], 1, j + 1), "-")), "in", valueOnly = TRUE) < availwidth ) {
        j <- j + 1
      }
      strings[i] <- paste0(substring(strings[i + 1], 1, j), "-")
      strings[i + 1] <- substring(strings[i + 1], j + 1, nchar(strings[i + 1]))
    }
    width <- convertWidth(stringWidth(strings[i]), "in", valueOnly = TRUE)
    if (i == 1) {
      newstring <- paste0(newstring, strings[i])
      linewidth <- width
    } else {
      if (linewidth + gapwidth + width < availwidth) {
        sep <- " "
        linewidth <- linewidth + gapwidth + width
      } else {
        sep <- "\n"
        linewidth <- width
      }
      newstring <- paste(newstring, strings[i], sep=sep)
    }
    i <- i + 1
  }
  popViewport()
  newstring
}


fixedWidthTextGrob <- function(label, width = unit(1, "ncp"), ...) {
  textGrob(splitString(
      label, availwidth = convertWidth(width, "in", valueOnly = TRUE)
    ), ...
  )
}

cols <- function(dscr) {

  theme <- dscr$theme

  widths <- list(
    variables = list(col = 2)
  )

  widths <- unit.c(
    theme$header.colwidth.variables,
    theme$colwidth.variables.seperator,
    theme$header.colwidth.descriptors,
    theme$colwidth.variables.descriptors,
    theme$header.colwidth.total
  )

  if (length(dscr$by) == 1) {
    lvls      <- levels(dscr$df[[dscr$by]])
    widths    <- unit.c(
      widths,
      rep(unit.c(
        theme$colwidth.others.seperators,
        theme$header.colwidth.others
      ),
      length(lvls)
      )
    )
  }

  return(widths)

}




# stop if not df[[varname]] a factor with at least two levels?
.is_proper_group <- function(df, varname) {

  if (!(varname %in% names(df))) {
    stop("varname not found in df")
  }

  var <- df[[varname]]

  if (!is.factor(var)) {
    stop("must be a factor")
  }

  if (length(levels(var)) < 2) {
    stop("must have at least two levels")
  }

}


to_inches <- function(u) {
  convertUnit(u, "in")
}