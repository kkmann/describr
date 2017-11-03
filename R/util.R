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

