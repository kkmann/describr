library(tidyverse)

context("Iris example")


test_that("describe", {

  dtable(iris, by = Species) %>%
    describe(
      list(dscr_histogram, dscr_boxplot),
      Sepal.Width, Sepal.Length
    ) ->
  dt
  expect_equal(1, 1)

})

test_that("describe_if", {

  dtable(iris, by = Species) %>%
    describe_if(
      is.numeric,
      list(dscr_n_perc, dscr_mean_sd)
    ) ->
  dt
  expect_equal(1, 1)

})

test_that("Iris complete", {

  dtable(iris,by = Species) %>%
    describe_if(
      is.numeric,
      list(dscr_n_perc, dscr_mean_sd)
    ) %>%
    describe_if(is.factor, dscr_freq) %>%
    describe(list(dscr_histogram, dscr_boxplot),
      Sepal.Width
    ) %>%
    dtableGrob() %>%
    {grid.newpage()
      grid.draw(.)}
  expect_equal(1, 1)

})
