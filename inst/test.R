library(describr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)

dummy      <- Statistic(mean, "bla")

supermean  <- TextDescriptor(
  function(data) sapply(c(mean(data), mean(data)/2), function(x) sprintf("%4.1f", x)),
  function(data) c("mean", "half mean")
)


histogram <- PlotDescriptor(
  ggplot(aes(x)) + geom_histogram(),
  function(data) "histogram"
)

dtable(iris,by = Species) %>%
  describe_if(dummy, is.numeric) %>%
  describe(list(supermean, histogram), Sepal.Width) ->
  dt

g <- dtableGrob(dt)

# g <- justify(g, "left", "top")

grid.draw(g)
