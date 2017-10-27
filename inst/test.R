library(describr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)
tmp  <- describe(iris, by = Species)

tmp2 <- Statistic(mean, "bla")
tmp3 <- Statistic(mean, "blubb")

tmp4 <- tmp + tmp2(Sepal.Length:Petal.Length) + tmp3(Sepal.Length)


dtableGrob(tmp4)
grid.draw(dtableGrob(tmp4))

tmp <- viewport()
grid.draw(valueGrob(tmp2, iris$Petal.Length, vp = tmp))
grid.draw(labelGrob(tmp2, iris$Petal.Length, vp = tmp))
