#' Create PNG file from plottable object
#'
#' \code{to_pdf} plots an object directly into one ore multiple PDF files.
#'
#' @param x object to plot.
#' @param name name used for output file(s), might be extended if multiple plots
#'   are generated and no ending is needed (e.g. 'my_file').
#' @param ... further options for specific methods.
#'
#' @export
to_png <- function(x, name, ...) {
  UseMethod("to_png", x)
}



#' For a \code{\link{describr}} or \code{describrGtable} object,
#' the object it first turned into a
#' plottable object respecting the given size constraints (potential split-up)
#' and then plotted to pmg using \code{grid.draw()}.
#'
#' @inheritParams to_png
#' @param maxwidth maximal plotting width in inches (plain numeric).
#' @param maxheight maximal plotting heigth in inches (plain numeric).
#' @param dpi dots per inch
#'
#' @name to_png
#' @export
to_png.describr <- function(x, name, maxwidth = Inf, maxheight = Inf, dpi = 96, ...) {

  as_gtable(x) %>%
    optimize_columnwidths(maxwidth = unit(maxwidth, "in")) %>%
    split_pages(maxheight = unit(maxheight, "in")) ->
    grob_list

  for (i in 1:length(grob_list)) {

    png(
      sprintf("%s_%i.png", name, i),
      width  = ceiling(dpi * convertUnit(sum(grob_list[[i]]$widths), "in", valueOnly = TRUE)),
      height = ceiling(dpi * convertUnit(sum(grob_list[[i]]$heights), "in", valueOnly = TRUE))
    )

    grid.draw(grob_list[[i]])

    dev.off()

  }

}



#' @inheritParams to_png.describr
#'
#' @name to_png
#' @export
to_png.describrGtable <- function(x, name, maxwidth = Inf, maxheight = Inf, dpi = 96, ...) {

  width <- convertUnit(x, "in", valueOnly = TRUE)

  if (width <= maxwidth) {
    x %>%
      split_pages(maxheight = unit(maxheight, "in")) ->
      grob_list
  } else {
    x %>%
      optimize_columnwidths(maxwidth = unit(maxwidth, "in")) %>%
      split_pages(maxheight = unit(maxheight, "in")) ->
      grob_list
  }

  for (i in 1:length(grob_list)) {

    pdf(
      sprintf("%s_%i.png", name, i),
      width  = ceiling(dpi * convertUnit(sum(grob_list[[i]]$widths), "in", valueOnly = TRUE)),
      height = ceiling(dpi * convertUnit(sum(grob_list[[i]]$heights), "in", valueOnly = TRUE))
    )

    grid.draw(grob_list[[i]])

    dev.off()

  }

}



#' For a list of \code{describrGtable} objects they are plotted directly to
#' png files of suitable size.
#'
#' @inheritParams to_png
#'
#' @name to_png
#' @export
to_png.list <- function(x, name, dpi = 96, ...) {

  for (i in 1:length(x)) {

    png(
      sprintf("%s_%i.png", name, i),
      width  = ceiling(dpi * convertUnit(sum(grob_list[[i]]$widths), "in", valueOnly = TRUE)),
      height = ceiling(dpi * convertUnit(sum(grob_list[[i]]$heights), "in", valueOnly = TRUE))
    )

    grid.draw(x[[i]])

    dev.off()

  }

}
