dscr_homogeneity_pvalue <- function() {
  structure(
    list(),
    class = "dscr_homogeneity_pvalue"
  )
}



#' Compute p-value for homogeneity nullhypothesis
#'
#' @param p p-value object
#' @param df data frame
#' @param varname character string giving name of variable to test for homogeneity
#'   with respect to group.
#' @param groupname character string giving name of grouping-variable (a factor)
#'   in df.
#'
#' @return a single numeric p-value.
#'
#' @export
compute_pvalue <- function(p, df, varname, groupname, ...) {
  UseMethod("compute_pvalue", p)
}



#' Get a label string from object
#'
#' @param x object
#' @param ... other parameters passed to methods
#'
#' @return A string identifying x.
#'
#' @export
label <- function(x, ...) {
  UseMethod("label", x)
}





# Homogeneity of location =====================================================

# Welch test -------------------------------------------------------------------

#' p-value for Welch test (homogeineity of means)
#'
#' Computes p-values for Welch test (two groups) or Welch ANOVA (more levels)
#' using \code{\link{oneway.test}}.#'
#'
#' @exportClass
dscr_Welch_ANOVA <- function(
  label_two_groups = "Welch test",
  label_more_groups = "Welch ANOVA"
) {

  structure(
    list(
      label_two_groups = label_two_groups,
      label_more_groups = label_more_groups
    ),
    class = c("dscr_Welch_ANOVA", "dscr_homogeneity_pvalue")
  )

}

compute_pvalue.dscr_Welch_ANOVA <- function(p, df, varname, groupname, ...) {

  # reduces to two-sample t test if there are only two levels

  if (!is.numeric(df[[varname]]))
    stop("variable must be numeric")

  .is_proper_group(df, groupname)

  frml <- as.formula(sprintf("%s ~ %s", varname, groupname))
  tryCatch(
    oneway.test(frml, data = df, var.equal = FALSE)$p.value,
    error = function(e) NA
  )

}

label.dscr_Welch_ANOVA <- function(p, df, varname, groupname, ...) {

  if (!is.numeric(df[[varname]]))
    stop("variable must be numeric")

  .is_proper_group(df, groupname)

  nlevels <- length(levels(df[["groupname"]]))

  if (nlevels == 2) {
    return(p$label_two_groups)
  } else {
    return(p$label_more_groups)
  }

}



# Kruskal-Wallis test-----------------------------------------------------------

#' p-value for Kruskal-Wallis test (homogeineity of locations)
#'
#' Computes p-values for Wilcoxon rank-sum test (two groups) or Kruskal-Wallis
#' test (more levels) using \code{\link{wilcox.test}} and
#' \code{\link{kruskal.test}}.
#' Both tests are equivalent but an exact p-value is only returned for the
#' two-group case.
#'
#' @exportClass
dscr_Kruskal <- function(
  label_two_groups = "Mann-Whitney test",
  label_more_groups = "Kruskal-Wallis test"
) {

  structure(
    list(
      label_two_groups = label_two_groups,
      label_more_groups = label_more_groups
    ),
    class = c("dscr_Kruskal", "dscr_homogeneity_pvalue")
  )

}

compute_pvalue.dscr_Kruskal <- function(p, df, varname, groupname, ...) {

  # reduces to two-sample wilcox.test if there are only two levels

  if (!is.numeric(df[[varname]]))
    stop("variable must be numeric")

  .is_proper_group(df, groupname)

  frml <- as.formula(sprintf("%s ~ %s", varname, groupname))

  if (nlevels == 2) {
    res <- tryCatch(
      wilcox.test(frml, data = df, exact = TRUE)$p.value,
      error = function(e) NA
    )
  } else {
    res <- tryCatch(
      kruskal.test(frml, data = df, exact = TRUE)$p.value,
      error = function(e) NA
    )
  }

  return(res)

}

label.dscr_Kruskal <- function(p, df, varname, groupname, ...) {

  if (!is.numeric(df[[varname]]))
    stop("variable must be numeric")

  .is_proper_group(df, groupname)

  nlevels <- length(levels(df[["groupname"]]))

  if (nlevels == 2) {
    return(p$label_two_groups)
  } else {
    return(p$label_more_groups)
  }

}





# Homogeneity of variation ####################################################








# Homogeneity of distribution =================================================

# Chi-Squared test ------------------------------------------------------------

#' p-value for Chi-squared test (homogeineity of distributions)
#'
#' Computes p-values for Chi-squared test using \code{\link{chisq.test}}.
#' A one-sample Chi-squared test can be conducted by using \code{groupname==varname}.
#'
#' @exportClass
dscr_chisq <- function(
  label_one_sample  = "Chi-squared test",
  label_cross_table = "Chi-squared test"
) {

  structure(
    list(
      label_one_sample = label_one_sample,
      label_cross_table = label_cross_table
    ),
    class = c("dscr_chisq", "dscr_homogeneity_pvalue")
  )

}

compute_pvalue.dscr_chisq <- function(p, df, varname, groupname, ...) {

  if (!is.factor(df[[varname]]))
    stop("variable must be a factor")

  .is_proper_group(df, groupname)

  if (varname == groupname) {
    res <- tryCatch(
      chisq.test(ddf[[groupname]] %>% table(), correct = FALSE)$p.value,
      error = function(e) NA
    )
  } else {
    res <- tryCatch(
      chisq.test(df[, c(varname, groupname)] %>% table(), correct = FALSE)$p.value,
      error = function(e) NA
    )
  }

  return(res)

}

label.dscr_chisq <- function(p, df, varname, groupname, ...) {

  if (!is.factor(df[[varname]]))
    stop("variable must be a factor")

  .is_proper_group(df, groupname)

  if (varname == groupname) {
    return(p$label_one_sample)
  } else {
    return(p$label_cross_table)
  }

}




# Anderson-Darling test --------------------------------------------------------

#' p-value for Anderson-Darling test (homogeineity of distributions)
#'
#' Computes asymptotic p-values for k-sample ANderson-Darling test using
#' \code{\link[kSamples]{ad.test}}.
#'
#' @exportClass
dscr_anderson_darling <- function(
  label = "Anderson-Darling",
  version = 1 # 1 o r 2 different verisonsto compute p value
) {

  structure(
    list(
      label = label,
      version = version
    ),
    class = c("dscr_anderson_darling", "dscr_homogeneity_pvalue")
  )

}

compute_pvalue.dscr_anderson_darling <- function(p, df, varname, groupname, ...) {

  if (!is.numeric(df[[varname]]))
    stop("variable must be numeric")

  .is_proper_group(df, groupname)

  frml <- as.formula(sprintf("%s ~ %s", varname, groupname))

  tryCatch(
    {
      tmp <- kSamples::ad.test(
        frml, data = df, method = "asymptotic"
      )
      return(tmp$ad[p$version, " asympt. P-value"])
    },
    error = function(e) NA
  )

}

label.dscr_chisq <- function(p, df, varname, groupname, ...) {

  if (!is.factor(df[[varname]]))
    stop("variable must be a factor")

  .is_proper_group(df, groupname)

  return(p$label)

}


