bottomGrob <- function(dscr, gtable, footnote_text = "") {

  theme <- dscr$theme_new$bottom

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

  width <- convertUnit(sum(gtable$widths), "in")

  final_footnote <- element_table_grob(
    theme$style$footnote,
    sprintf("%s %s.", footnote_text, pvalues_footnote),
    width = width,
    name = "footnote"
  )

  separator <- element_table_grob(
    theme$style$separator,
    widths = width,
    name = "bottom_separator"
  )

  footer <- rbind(separator, final_footnote)

  return(footer)

}
