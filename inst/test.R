library(describr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)

dtable(iris, by = Species, pvalue = TRUE) %>%
  describe_if(
    is.numeric,
    with = list(dscr_n_perc, dscr_mean_sd)
  ) %>%
  describe_if(
    is.factor,
    with = dscr_freq
  ) %>%
  describe(
    with = list(dscr_histogram, dscr_boxplot),
    Sepal.Width
  ) ->
dt

# dt$theme_new$colwidths$variables <- unit(1, "in")

g <- dtableGrob(dt)

grid.newpage()
grid.draw(g)

g2 <- optimize_columnwidths(g)


grid.newpage()
grid.draw(g2)

g <- justify(g, "left", "top")

h <- convertHeight(grobHeight(g), "in", valueOnly = TRUE)
w <- convertWidth(grobWidth(g), "in", valueOnly = TRUE)

pdf("test.pdf", 1.1*w, h)
grid.draw(g)
dev.off()
