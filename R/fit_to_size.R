fit_to_size <- function(dscr_gtable, width = NULL, height = NULL, ...) {
  UseMethod("fit_to_size", dscr_gtable)
}

fit_to_size.describr <- function(dscr, width = NULL, height = NULL, ...) {

  dscr %>%
    as_gtable() %>%
    fit_to_size(width, height, ...)

}

fit_to_size.describrGtable <- function(
  dscr_gtable, width = NULL, height = NULL, ...
) {

  if (is.null(width))
    width <- dev.size("in")[1]

  if (is.null(height))
    height <- dev.size("in")[2]

  dscr_gtable %>%
    optimize_columnwidths(maxwidth = unit(width, "in")) %>%
    split_pages(maxheight = unit(height, "in")) ->
  grob_list

  return(grob_list)

}