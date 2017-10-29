theme_default <- function() {
  structure(
    list(
      colwidth.variables.seperator = unit(12, "pt"),
      colwidth.variables.descriptors = unit(12, "pt"),
      colwidth.others.seperators  = unit(12, "pt"),
      header.fontsize             = 12, # pt
      header.lineheight           = 1.2 * 12, # pt
      header.colname.variables    = "Variable",
      header.colname.descriptors  = "",
      header.colname.total        = "Total",
      header.colname.pvalues      = "p value",
      header.colwidth.variables   = unit(1, "in"),
      header.colwidth.descriptors = unit(1, "in"),
      header.colwidth.total       = unit(1.25, "in"),
      header.colwidth.pvalues     = unit(.5, "in"),
      header.colwidth.others      = unit(1.25, "in"),
      header.seperator.height     = unit(1.2 * 12 , "pt"),
      header.seperator.size       = 1.5, # pt
      header.grouping.seperator.height = unit(1.2 * 12, "pt"),
      header.grouping.seperator.size = 1.5,

      variable.seperator.height = unit(31, "pt"),
      variable.seperator.size   = 0, # pt

      descriptor.seperator.height = unit(1.2 * 12 / 2, "pt"),
      descriptor.seperator.size   = 0, # pt

      bottom.seperator.height     = unit(1.2 * 12, "pt"),
      bottom.seperator.size       = 1.5 # pt
    ),
    class = "dtable_theme"
  )
}
