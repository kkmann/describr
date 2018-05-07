# describr - publication quality descriptive tables with R

[![Travis-CI Build Status](https://travis-ci.org/kkmann/describr.svg?branch=master)](https://travis-ci.org/kkmann/describr)

Tired of fiddling with boring 'univariat descriptive tables' that everybody (should) want but nobody wants to do - `describr` (hopefully) resolves this issue and makes generating nice-looking descriptive tables (almost) fun!

## Installation

Make sure that the `devtools` package is installed and run

    devtools::install_github('kkmann/describr')
    
to install the most recent version (might be unstable). 

## Intro

The package provides functions `describr`, `describe`, `describe_if`, and several default
descriptive statistics / graphs but is designed to be extended by custom descriptive 
statistics or graphics. 
The main purpose is to typeset tables of univarite descriptive statistics with given
size constraints (as for instance in .pdf documents generated using `knitr`).

A simple example can be found at /inst/example.Rmd which essentially does the following:
  
    # create dataset with dummy factor
    iris %>%
      mutate(
        `I am a factor with very long variable-name` = factor(
          sample(1:4, nrow(iris), replace = TRUE),
          labels = c("hi", "ho", "xXx", "I am a level with long level-name")
        )
      ) ->
    df_iris_test

    # describe data frame
    df_iris_test %>%
      describr(
        by = Species, 
        pvalues = TRUE,
        theme_new = theme_default(text_size = 9) # unit: pt
      ) %>%
      describe_if(
        is.numeric,
        with = list(
          dscr_mean_sd(),
          dscr_median_q1_q3(),
          dscr_min_max(),
          dscr_histogram()
        )
      ) %>%
      describe_if(
        is.factor,
        with = list(dscr_freq(), dscr_factor_barchart())
      ) %>%
      fit_to_size()
      
Here, `fit_to_size()` ensures that the table is fit to the size of the
currently active plotting device. 
This is especially handy when using `knitr`'s `fig.width` or `fig.height`
options.
Further note that tables which are too long for the available size are 
automatically split to multiple plot allowing them to be spread over 
multiple pages.


## ToDo

1. Rework the theming system
2. Rework graphics code to be more robust/readable 
3. Document interface for custom descriptors
4. Better column width optimization
5. Bugs, bugs, bugs...
