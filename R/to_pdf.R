#' Create PDF file from plottable object
#'
#' \code{to_pdf} plots an object directly into one ore multiple PDF files.
#'
#' @param x object to plot.
#' @param name name used for output file(s), might be extended if multiple plots
#'   are generated and no ending is needed (e.g. 'my_file').
#' @param ... further options for specific methods.
#'
#' @export
to_pdf <- function(x, name, ...) {
  UseMethod("to_pdf", x)
}



#' For a \code{\link{describr}} or \code{describrGtable} object,
#' the object it first turned into a
#' plottable object respecting the given size constraints (potentiall split-up)
#' and then plotted to pdf using \code{grid.draw()}.
#'
#' @inheritParams to_pdf
#' @param maxwidth maximal plotting width in inches (plain numeric).
#' @param maxheight maximal plotting heigth in inches (plain numeric).
#'
#' @name to_pdf
#' @export
to_pdf.describr <- function(x, name, maxwidth = Inf, maxheight = Inf, ...) {

  as_gtable(x) %>%
    optimize_columnwidths(maxwidth = unit(maxwidth, "in")) %>%
    split_pages(maxheight = unit(maxheight, "in")) ->
  grob_list

  for (i in 1:length(grob_list)) {

    pdf(sprintf("%s_%i.pdf", name, i))

    width  <- convertUnit(sum(grob_list[[i]]$widths), "in", valueOnly = TRUE)
    height <- convertUnit(sum(grob_list[[i]]$heights), "in", valueOnly = TRUE)

    dev.off()

    pdf(
      sprintf("%s_%i.pdf", name, i),
      width  = width,
      height = height
    )

    grid.draw(grob_list[[i]])

    dev.off()

  }

}



#' @inheritParams to_pdf.describr
#'
#' @name to_pdf
#' @export
to_pdf.describrGtable <- function(x, name, maxwidth = Inf, maxheight = Inf, ...) {

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
      sprintf("%s_%i.pdf", name, i),
      width  = convertUnit(sum(grob_list[[i]]$widths), "in", valueOnly = TRUE),
      height = convertUnit(sum(grob_list[[i]]$heights), "in", valueOnly = TRUE)
    )

    grid.draw(grob_list[[i]])

    dev.off()

  }

}



#' For a list of \code{describrGtable} objects they are plotted directly to
#' pdf files of suitable size.
#'
#' @inheritParams to_pdf
#'
#' @name to_pdf
#' @export
to_pdf.list <- function(x, name, ...) {

  for (i in 1:length(x)) {

    pdf(
      sprintf("%s_%i.pdf", name, i),
      width  = convertUnit(sum(x[[i]]$widths), "in", valueOnly = TRUE),
      height = convertUnit(sum(x[[i]]$heights), "in", valueOnly = TRUE)
    )

    grid.draw(x[[i]])

    dev.off()

  }

}
