bottomGrob <- function(dscr, gtable, footnote_text = "") {

  theme <- dscr$theme_new$bottom

  if (is.stratified(dscr) & dscr$pvalues) {
    pvalue_labels         <- dscr$register_pvalue("", return = TRUE)
    pvalue_footnotes_list <- list()
    for (i in 1:length(pvalue_labels)) {
      pvalue_footnotes_list <- c(
        pvalue_footnotes_list,
        sprintf("(%i): %s", i, pvalue_labels[i])
      )
    }
    pvalues_footnote <- do.call(
      paste,
      args = c(pvalue_footnotes_list, list(sep = ", "))
    )
  } else {
    pvalues_footnote <- ""
  }

  width <- convertUnit(sum(gtable$widths), "in")

  final_footnote <- element_table_grob(
    theme$style$footnote,
    pvalues_footnote,
    width = width,
    name = "footnote"
  )

  final_footnote2 <- element_table_grob(
    theme$style$caption,
    footnote_text,
    width = width,
    name = "footnote"
  )

  separator <- element_table_grob(
    theme$style$separator,
    widths = width,
    name = "bottom_separator"
  )

  blank_line <- element_table_grob(
    element_table_horizontal_separator(text_size = theme$style$caption$text_size, separator_line_size = 0),
    widths = width,
    name = "bottom_blank_line"
  )

  if (nchar(footnote_text) > 0) {

    footer <- rbind(
      separator,
      final_footnote,
      blank_line,
      final_footnote2
    )

  } else {

    footer <- rbind(
      separator,
      final_footnote
    )

  }

  return(footer)

}
