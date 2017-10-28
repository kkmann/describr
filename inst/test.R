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

dtable(iris,by = Species) %>%
  describe_if(dummy, is.numeric) %>%
  describe(list(supermean, supermean), Sepal.Width) ->
  dt

g <- headerGrob(dt)

g2 <- variableGrob(dt, "Sepal.Width")

grid.draw(g2)

g <- dtableGrob(dt)

g <- justify(g, "left", "top")

grid.draw(g)

# grid.draw(valueGrob(tmp3, iris$Sepal.Length, width = unit(2, "cm")))


# grid.draw(grobTree(rectGrob(), dtableGrob(tmp4)))


test <- Descriptor()

dtable(iris, by = Species) %>%
  describe(tmp3, Sepal.Width) -> la

dtable(iris, by = Species) %>%
  describe_if(list(test, test), is.numeric) -> la2

