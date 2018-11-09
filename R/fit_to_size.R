fit_to_height <- function(dscr_gtable, height = NULL, ...) {
  UseMethod("fit_to_height", dscr_gtable)
}

fit_to_height.describr <- function(dscr, height = NULL, ...) {

  dscr %>%
    as_gtable() %>%
    fit_to_height(height, ...)

}

fit_to_height.describrGtable <- function(
  dscr_gtable, height = NULL, ...
) {

  if (is.null(height))
    height <- dev.size("in")[2]

  dscr_gtable %>%
    split_pages(maxheight = unit(height, "in")) ->
  grob_list

  res <- structure(
    grob_list,
    class = c("describrGrobList", "list")
  )

  return(res)

}

print.describrGrobList <- function(x, ...) {

  lapply(x, print)

}