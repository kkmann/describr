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

grid.draw(valueGrob(tmp3, iris$Sepal.Length, width = unit(2, "cm")))


dtableGrob(tmp4)

pdf(file = "test.pdf", width = 8, height = 12)
grid.draw(dtableGrob(tmp4))
dev.off()
