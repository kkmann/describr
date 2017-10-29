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

g <- headerGrob(dt)

g <- descriptorGrob(dummy, dt, "Sepal.Length")

g <- variableGrob(dt, "Sepal.Length")

g <- dtableGrob(dt)

# g <- justify(g, "left", "top")

h <- convertHeight(grobHeight(g), "in", valueOnly = TRUE)
w <- convertWidth(grobWidth(g), "in", valueOnly = TRUE)

pdf("test.pdf", w, h)
grid.draw(g)
dev.off()

