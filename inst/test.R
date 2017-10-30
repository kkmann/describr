library(describr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)

dtable(iris, by = Species) %>%
  describe_if(
    is.numeric,
    list(dscr_n_perc, dscr_mean_sd)
  ) %>%
  describe_if(
    is.factor,
    dscr_freq
  ) %>%
  describe(
    list(dscr_histogram, dscr_boxplot),
    Sepal.Width
  ) ->
dt

# g <- headerGrob(dt)

# g <- descriptorGrob(dummy, dt, "Sepal.Length")
#
# g <- variableGrob(dt, "Sepal.Length")

g <- dtableGrob(dt)

grid.newpage()
grid.draw(g)

g <- justify(g, "left", "top")

h <- convertHeight(grobHeight(g), "in", valueOnly = TRUE)
w <- convertWidth(grobWidth(g), "in", valueOnly = TRUE)

pdf("test.pdf", 1.1*w, h)
grid.draw(g)
dev.off()

