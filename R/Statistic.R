Statistic <- function(stat, label, digits = 2, format = sprintf("%%.%if", digits)) {
  res <- TextDescriptor(
    f = function(data) sprintf(format, stat(data)),
    label = function(data) label
  )
  attr(res, "class") <- c("Statistic", attr(res, "class"))
  return(res)
}