library(describr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)
tmp  <- describe(iris, by = Species)



tmp2 <- Statistic(mean, "bla")
tmp3 <- TextDescriptor(
  function(data) sapply(c(mean(data), mean(data)/2), function(x) sprintf("%4.1f", x)),
  function(data) c("mean", "half mean")
)

tmp4 <- tmp + tmp2(Sepal.Length:Petal.Length) + tmp3(Sepal.Length)

g <- dtableGrob(tmp4)

g <- justify(g, "left", "top")

grid.draw(g)

# grid.draw(valueGrob(tmp3, iris$Sepal.Length, width = unit(2, "cm")))


# grid.draw(grobTree(rectGrob(), dtableGrob(tmp4)))


test <- Descriptor()

dtable(iris, by = Species) %>%
  describe(test, Sepal.Width) -> la
la$core$Sepal.Width$variables
