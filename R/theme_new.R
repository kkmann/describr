theme_default_tmp <- function(
  size = 12, lineheight = 1.2*size
) {
  list(
    colwidths = list(
      variables   = unit(25 * size / 2 / 72, "in"),
      descriptors = unit(20 * size / 2 / 72, "in"),
      levels      = unit(15 * size / 2 / 72, "in"),
      pvalues     = unit(10 * size / 2 / 72, "in") ,
      seperators   = unit(size / 72, "in")
    ),
    theme_header = theme_header_default(size)
  )
}

theme_header_default <- function(
  size = 12, lineheight = 1.2*size
) {
  list(
    size = size,
    lineheight = lineheight
  )
}
