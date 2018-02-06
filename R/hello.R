#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'


.onLoad <- function(lib, pkg) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Alexandria Rhoads",
    devtools.desc.author = '"Alexandria Rhoads <alex@inspireenvironmental.com> [aut, cre]"',
    devtools.desc.license = "MIT",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

#' Calculates contrast variance or standard error of difference equation
#'
#' This function calculates standard error of difference equation or numerator of W-S estimate
#' @param x a numerical vector of variances
#' @param c a numerical vector of coefficents in difference equation
#' @param n a numerical vector of sample sizes
#' @export
SEd <- function(x, c, n) {
    sum(x*(c^2/n))
}

#' Calculates Welch-Satterthwaite degrees of freedom (df) and estimate
#'
#' This function finds W-S df and estimate
#' @param x a numerical vector of variances
#' @param c a numerical vector of coefficents in difference equation
#' @param n a numerical vector of sample sizes
#' @export
WS <- function(x, c, n) {
  df=n-1;
  list(df = sum((1/df)*(x*(c^2/n))^2),
       estimate = (sum(x*(c^2/n))^2)/(sum((1/df)*(x*(c^2/n))^2)))
}

#' Calculates intervals
#'
#' This function finds upper, lower bounds
#' @import stats
#' @param x a numerical vector of dataset
#' @param y numerical vector of other dataset
#' @param parm estimate W-S value
#' @param se constrast variance/standard error of difference equation
#' @param level the confidence level required
#' @export
confin.bio <- function(x, y, se, parm, level) {
  level < 1;
  mean_x <- mean(x);
  mean_y <- mean(y);
  c((mean_x - mean_y), qt(level,parm)*se)
}
