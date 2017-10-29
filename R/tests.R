Test <- function(
  label = "",
  pval = function(df) 1,
  format = function(pval = NULL) {
    if (is.null(pval))
      return("")
    if (pval < .001) {
      return("p < 0.001")
    } else {
      return(sprintf("%.3f", pval))
    }
  }
) {
  res <- structure(
    list(
      label  = label,
      pval   = pval,
      format = format
    ),
    class = c("Test")
  )

  return(res)

}



testGrob <- function(dscr, t, varname, ...) {
  UseMethod("testGrob", d)
}

testGrob.Test <- function(dscr, t, varname) {

}