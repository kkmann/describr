theme_default <- function() {
  structure(
    list(
      header.fontsize             = 12,
      header.lineheight           = 1.2 * 12,
      header.colname.variables    = "Variable",
      header.colname.descriptors  = "",
      header.colname.total        = "Total",
      header.colname.pvalues      = "p value",
      header.colwidth.variables   = unit(1, "in"),
      header.colwidth.descriptors = unit(1, "in"),
      header.colwidth.total       = unit(1, "in"),
      header.colwidth.pvalues     = unit(.5, "in"),
      header.colwidth.others      = unit(1.5, "in")
    ),
    class = "dtable_theme"
  )
}
