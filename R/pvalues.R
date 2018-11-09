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




# no test ======================================================================
#' Dummy class if a descriptor does not provide a p-value
#'
#' @exportClass
dscr_no_test <- function(
  label = "no test performed"
) {

  structure(
    list(
      label = label
    ),
    class = c("dscr_no_test", "dscr_homogeneity_pvalue")
  )

}

compute_pvalue.dscr_no_test <- function(p, df, varname, groupname, ...) NA

label.dscr_no_test <- function(p, df, varname, groupname, ...) p$label




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

  nlevels <- length(levels(df[["groupname"]]))

  if (nlevels == 2) {
    res <- tryCatch(
      wilcox.test(frml, data = df, exact = TRUE)$p.value,
      error = function(e) {warning(e); return(NA)}
    )
  } else {
    res <- tryCatch(
      kruskal.test(frml, data = df)$p.value,
      error = function(e) {warning(e); return(NA)}
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

# Levine  -------------------------------------------------

#' p-value for Levene test (homogeineity of variation)
#'
#' Uses \code{\link[car]{leveneTest}}.
#'
#' @exportClass
dscr_levene <- function(
  label  = "Levene-test"
) {

  structure(
    list(
      label = label
    ),
    class = c("dscr_levene", "dscr_homogeneity_pvalue")
  )

}

compute_pvalue.dscr_levene <- function(p, df, varname, groupname, ...) {

  if (!is.numeric(df[[varname]]))
    stop("variable must be numeric")

  .is_proper_group(df, groupname)

  frml <- as.formula(sprintf("%s ~ %s", varname, groupname))

  res <- tryCatch(
    car::leveneTest(
      frml,
      data = df[, c(varname, groupname)]
    )$`Pr(>F)`,
    error = function(e) {warning(e); NA}
  )

  return(res)

}

label.dscr_levene <- function(p, df, varname, groupname, ...) p$label






# Homogeneity of distribution =================================================


# Cross-table chi-squared test -------------------------------------------------

#' p-value for chi-squared test (homogeineity of distributions)
#'
#' Computes cross-table p-values for Chi-squared test using \code{\link{chisq.test}}.
#'
#' @exportClass
dscr_cross_table_chisq <- function(
  label  = "Chi-squared test"
) {

  structure(
    list(
      label = label
    ),
    class = c("dscr_cross_table_chisq", "dscr_homogeneity_pvalue")
  )

}

compute_pvalue.dscr_cross_table_chisq <- function(p, df, varname, groupname, ...) {

  if (!is.factor(df[[varname]]))
    stop("variable must be a factor")

  .is_proper_group(df, groupname)

  res <- tryCatch(
    chisq.test(df[, c(varname, groupname)] %>% table(), correct = FALSE)$p.value,
    error = function(e) {warning(e); NA}
  )

  return(res)

}

label.dscr_cross_table_chisq <- function(p, df, varname, groupname, ...) p$label




# Chi-squared test for uniformity ----------------------------------------------

#' p-value for chi-squared test (homogeineity of distributions)
#'
#' Computes one-sample p-values for chi-squared test using \code{\link{chisq.test}}
#' (deviation from discrete uniform distribution).
#'
#' @exportClass
dscr_one_sample_chisq <- function(
  label  = "one-sample Chi-squared test for discrete uniformity"
) {

  structure(
    list(
      label = label
    ),
    class = c("dscr_one_sample_chisq", "dscr_homogeneity_pvalue")
  )

}

compute_pvalue.dscr_one_sample_chisq <- function(p, df, varname, groupname, ...) {

  .is_proper_group(df, groupname)


  res <- tryCatch(
    chisq.test(df[[groupname]] %>% table(), correct = FALSE)$p.value,
    error = function(e) {warning(e); NA}
  )

  return(res)

}

label.dscr_one_sample_chisq <- function(p, df, varname, groupname, ...) p$label




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
    error = function(e) {warning(e); NA}
  )

}

label.dscr_anderson_darling <- function(p, df, varname, groupname, ...) {

  return(p$label)

}


