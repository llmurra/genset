# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Genset Wrapper
#' Generate Data Sets for Class Demonstrations
#'
#' Generate data sets to demonstrate the importance
#' of multiple regression. \code{'genset'} generates a
#' data set from an initial data set to have the same
#' summary statistics (mean, median, and standard
#' deviation) but opposing regression results.
#' The initial data set will have one response variable
#' (continuous) and two predictor variables
#' (continous or one continuous and one categorical
#' with 2 levels) that are statistically significant
#' in a linear regression model.
#'
#' @import stats
#'
#' @usage genset(y, x1, x2, method, option, n, decrease, output)
#'
#' @param y a vector containing the response variable (continuous),
#' @param x1 a vector containing the first predictor variable (continuous)
#' @param x2 a vector containing the second predictor variable (continuous or
#' categorical with 2 levels). If variable is categorical
#' then argument is \code{factor(x2)}
#' @param method the method \code{1} or \code{2} to
#' be used to generate the data set. \code{1} (default)
#' rearranges the values within each variable,
#' and \code{2} is a perturbation method that makes
#' subtle changes to the values of the variables
#' @param option the variable(s) that will not be
#' statistically significant in the new data set
#' (\code{"x1"} (default), \code{"x2"} or \code{"both"})
#' @param n maximum number of iterations
#' @param decrease decreases the signficance level when \code{TRUE},
#' default is \code{FALSE}
#' @param output print each interation when \code{TRUE}, default is
#' \code{FALSE}
#'
#' @export
#'
#' @author Lori Murray & John Wilson
#'
#' @details The summary statistics are within a
#' (predetermined) tolerance level, and when rounded
#' will be the same as the original data set. We use
#' the standard convention 0.05 as the significance
#' level. The default for the number of iterations is
#' \code{n=2000}. Less than \code{n=2000} may or may
#' not be sufficient and is dependent on the initial
#' data set.
#'
#' @return Returns an object of class "data.frame"
#' containing the generated data set: (in order) the
#' response variable, first predictor variable and
#' second predictor variable.
#'
#' @references Murray, L. and Wilson, J. (2020). The
#' Need for Regression: Generating Multiple Data Sets
#' with Identical Summary Statistics but Differing
#' Conclusions. {\emph{Decision Sciences Journal of
#' Innovative Education.}} Accepted for publication.
#'
#' @examples
#' ## Choose variables of interest
#' y <- mtcars$mpg
#' x1 <- mtcars$hp
#' x2 <- mtcars$wt
#' ## Create a dataframe
#' set1 <- data.frame(y, x1, x2)
#' ## Check summary statistics
#' multi.fun <- function(x) {
#' c(mean = mean(x), media=median(x), sd=sd(x))
#' }
#' round(multi.fun(set1$y), 0)
#' round(multi.fun(set1$x1), 1)
#' round(multi.fun(set1$x2), 1)
#' ## Fit linear regression model
#' ## to verify regressors are statistically
#' ## significant (p-value < 0.05)
#' summary(lm(y ~ x1, x2, data=set1))
#'
#' ## Set seed to reproduce same data set
#' set.seed(101)
#' set2 <- genset(y, x1, x2, method=1, option="x1", n=1000)
#' ## Verify summary statistics match set 1
#' round(multi.fun(set2$y), 0)
#' round(multi.fun(set2$x1), 1)
#' round(multi.fun(set2$x2), 1)
#' ## Fit linear regression model
#' ## to verify x1 is not statistically
#' ## significant (p-value > 0.05)
#' summary(lm(y ~ x1 + x2, data=set2))


genset <- function(y, x1, x2, method=c(1,2), option=c("x1", "x2", "both"), n, decrease=FALSE, output=FALSE)
{
  x2.factor <- (class(x2) == "factor")

  if ( missing(n) )
    n <- 2000

  alpha = c(0.05, 0.05)

  if ( (x2.factor) && (option=="both") ) stop("option=both not supported")

  if (decrease == FALSE)
  {
    if (option == "x1") opt <- c("above", "below")
    if (option == "x2") opt <- c("below", "above")
    if (option == "both") opt <- c("above", "above")
  } else {
    if (option == "x1") opt <- c("below", "above")
    if (option == "x2") opt <- c("above", "below")
    if (option == "both") opt <- c("below", "below")
  }

  if (method==1)
  {
    if (!x2.factor)
    {
      result <- g1(y, x1, x2, alpha=alpha, opt=opt, n=n, output=output)
    } else {
      result <- g3(y, x1, x2, alpha=alpha, opt=opt, n=n, output=output)
    }
  }

  if (method==2)
  {
    if (!x2.factor)
    {
      result <- g2(y, x1, x2, alpha=alpha, opt=opt, n=n, output=output)
    } else {
      result <- g4(y, x1, x2, alpha=alpha, opt=opt, n=n, output=output)
    }
  }

  return (result)
}

g1 <- function(y, x1, x2, alpha, opt, n=2000, output=FALSE)
{
  x1.opt = opt[1]
  x2.opt = opt[2]

  x1.alpha = alpha[1]
  x2.alpha = alpha[2]

  # Best sample will be returned
  step <- 0
  y.best <- y
  x1.best <- x1
  x2.best <- x2

  fit.coef <- summary(lm(y ~ x1 + x2))
  x1.pvalue <- fit.coef$coefficients[2,4]
  x2.pvalue <- fit.coef$coefficients[3,4]

  dist <- getDist(x1.pvalue, x1.alpha, x1.opt) + getDist(x2.pvalue, x2.alpha, x2.opt)

  x1.miss <- 0
  x2.miss <- 0
  y.miss <- 0

  x1.keep <- 0
  x2.keep <- 0
  y.keep <- 0

  for (i in 1:n)
  {
    # Change x1
    {
      x1 <- sample(x1)

      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      x1.miss <- x1.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        x1.miss <- 0
        x1.keep <- x1.keep + 1
      }
    }

    # Change x2
    {
      x2 <- sample(x2)

      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      x2.miss <- x2.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        x2.miss <- 0
        x2.keep <- x2.keep + 1
      }
    }

    # Change y
    {
      y <- sample(y)

      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      y.miss <- y.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        y.miss <- 0
        y.keep <- y.keep + 1
      }
    }

    if (output) print(sprintf("%1.8f %1.8f %1.8f %d (%d) %d (%d) %d (%d) %d (%d)", x1.pvalue, x2.pvalue, dist, x1.keep, x1.miss, x2.keep, x2.miss, y.keep, y.miss, i, step))
  }

  y <- y.best
  x1 <- x1.best
  x2 <- x2.best

  data.set <- data.frame(y, x1, x2)

  return (data.set)
}

g2 <- function(y, x1, x2, alpha, opt, n=2000, output=FALSE)
{
  x1.opt = opt[1]
  x2.opt = opt[2]

  x1.alpha = alpha[1]
  x2.alpha = alpha[2]

  # Best sample will be returned
  step <- 0
  y.best <- y
  x1.best <- x1
  x2.best <- x2

  # Get summary stats for x1
  x1.stats <- getStats(x1)
  x1.digits <- max(sapply(x1, getDigits))
  x1.h <- max(x1.stats[2] * 0.05, 1/(10^x1.digits*3))
  x1.tol <- getTol(x1)

  # Get summary stats for x2
  x2.stats <- getStats(x2)
  x2.digits <- max(sapply(x2, getDigits))
  x2.h <- max(x2.stats[2] * 0.05, 1/(10^x2.digits*3))
  x2.tol <- getTol(x2)

  # Get summary stats for y
  y.stats <- getStats(y)
  y.digits <- max(sapply(y, getDigits))
  y.h <- max(y.stats[2] * 0.05, 1/(10^y.digits*3))
  y.tol <- getTol(y)

  fit.coef <- summary(lm(y ~ x1 + x2))
  x1.pvalue <- fit.coef$coefficients[2,4]
  x2.pvalue <- fit.coef$coefficients[3,4]

  dist <- getDist(x1.pvalue, x1.alpha, x1.opt) + getDist(x2.pvalue, x2.alpha, x2.opt)

  x1.miss <- 0
  x2.miss <- 0
  y.miss <- 0

  x1.keep <- 0
  x2.keep <- 0
  y.keep <- 0

  for (i in 1:n)
  {
    # Change x1
    {
      x1 <- getSample(x1, x1.h, x1.tol, x1.digits, x1.stats)

      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      x1.miss <- x1.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        x1.miss <- 0
        x1.keep <- x1.keep + 1
      }
    }

    # Change x2
    {
      x2 <- getSample(x2, x2.h, x2.tol, x2.digits, x2.stats)

      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      x2.miss <- x2.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        x2.miss <- 0
        x2.keep <- x2.keep + 1
      }
    }

    # Change y
    {
      y <- getSample(y, y.h, y.tol, y.digits, y.stats)

      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      y.miss <- y.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        y.miss <- 0
        y.keep <- y.keep + 1
      }
    }

    if (output) print(sprintf("%1.8f %1.8f %1.8f %d (%d) %d (%d) %d (%d) %d (%d)", x1.pvalue, x2.pvalue, dist, x1.keep, x1.miss, x2.keep, x2.miss, y.keep, y.miss, i, step))
  }

  y <- y.best
  x1 <- x1.best
  x2 <- x2.best

  data.set <- data.frame(y, x1, x2)

  return (data.set)
}

g3 <- function(y, x1, x2, alpha, opt, n=1000, output=FALSE)
{
  x1.opt = opt[1]
  x2.opt = opt[2]

  x1.alpha = alpha[1]
  x2.alpha = alpha[2]

  # Stratify the data
  data.full <- data.frame(y, x1, x2)
  data.level1 <- subset(data.full, x2==levels(x2)[1])
  data.level2 <- subset(data.full, x2==levels(x2)[2])

  y1 <- data.level1$y
  x1.1 <- data.level1$x1
  x2.1 <- data.level1$x2
  m1 <- length(y1)

  y2 <- data.level2$y
  x1.2 <- data.level2$x1
  x2.2 <- data.level2$x2
  m2 <- length(y2)

  # Recombine for model fitting
  y <- c(data.level1$y, data.level2$y)
  x1 <- c(data.level1$x1, data.level2$x1)
  x2 <- factor(c(data.level1$x2, data.level2$x2), labels=levels(x2))

  # Best sample will be returned
  step <- 0
  y.best <- y
  x1.best <- x1
  x2.best <- x2

  # Initial p-values
  fit.coef <- summary(lm(y ~ x1 + x2))
  x1.pvalue <- fit.coef$coefficients[2,4]
  x2.pvalue <- fit.coef$coefficients[3,4]

  dist <- getDist(x1.pvalue, x1.alpha, x1.opt) + getDist(x2.pvalue, x2.alpha, x2.opt)

  x1.miss <- 0
  y.miss <- 0

  x1.keep <- 0
  y.keep <- 0

  for (i in 1:n)
  {
    # Change x1.1
    {
      x1.1 <- sample(x1.1)
      x1 <- c(x1.1, x1.2)

      # fit regression model
      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      x1.miss <- x1.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        x1.miss <- 0
        x1.keep <- x1.keep + 1
      }
    }

    # Change x1.2
    {
      x1.2 <- sample(x1.2)
      x1 <- c(x1.1, x1.2)

      # fit regression model
      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      x1.miss <- x1.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        x1.miss <- 0
        x1.keep <- x1.keep + 1
      }
    }

    # Change y1
    {
      y1 <- sample(y1)
      y <- c(y1, y2)

      # fit regression model
      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      y.miss <- y.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        y.miss <- 0
        y.keep <- y.keep + 1
      }
    }

    # Change y2
    {
      y2 <- sample(y2)
      y <- c(y1, y2)

      # fit regression model
      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      y.miss <- y.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        y.miss <- 0
        y.keep <- y.keep + 1
      }
    }

    if (output) print(sprintf("%1.8f %1.8f %1.8f %d (%d) %d (%d) %d (%d)", x1.pvalue, x2.pvalue, dist, x1.keep, x1.miss, y.keep, y.miss, i, step))
  }

  y <- y.best
  x1 <- x1.best
  x2 <- x2.best

  data.set <- data.frame(y, x1, x2)

  return(data.set)
}

g4 <- function(y, x1, x2, alpha, opt, n=2000, output=FALSE)
{
  x1.opt = opt[1]
  x2.opt = opt[2]

  x1.alpha = alpha[1]
  x2.alpha = alpha[2]

  # Stratify the data
  data.full <- data.frame(y, x1, x2)
  data.level1 <- subset(data.full, x2==levels(x2)[1])
  data.level2 <- subset(data.full, x2==levels(x2)[2])

  y1 <- data.level1$y
  x1.1 <- data.level1$x1
  x2.1 <- data.level1$x2
  m1 <- length(y1)

  y2 <- data.level2$y
  x1.2 <- data.level2$x1
  x2.2 <- data.level2$x2
  m2 <- length(y2)

  # Recombine for model fitting
  y <- c(data.level1$y, data.level2$y)
  x1 <- c(data.level1$x1, data.level2$x1)
  x2 <- factor(c(data.level1$x2, data.level2$x2), labels=levels(x2))

  # Best sample will be returned
  step <- 0
  y.best <- y
  x1.best <- x1
  x2.best <- x2

  # Get summary stats for x1
  x1.1.stats <- getStats(x1.1)
  x1.2.stats <- getStats(x1.2)
  x1.digits <- max(sapply(x1, getDigits))
  x1.stats <- getStats(x1)
  x1.h <- max(x1.stats[2] * 0.05, 1/(10^x1.digits*2))
  x1.tol <- getTol(x1)

  # Get summary stats for y
  y1.stats <- getStats(y1)
  y2.stats <- getStats(y2)
  y.digits <- max(sapply(y, getDigits))
  y.stats <- getStats(y)
  y.h <- max(y.stats[2] * 0.05, 1/(10^y.digits*2))
  y.tol <- getTol(y)

  # Initial p-values
  fit.coef <- summary(lm(y ~ x1 + x2))
  x1.pvalue <- fit.coef$coefficients[2,4]
  x2.pvalue <- fit.coef$coefficients[3,4]

  dist <- getDist(x1.pvalue, x1.alpha, x1.opt) + getDist(x2.pvalue, x2.alpha, x2.opt)

  x1.miss <- 0
  y.miss <- 0

  x1.keep <- 0
  y.keep <- 0

  for (i in 1:n)
  {
    # Change x1.1
    {
      x1.1 <- getSample(x1.1, x1.h, x1.tol, x1.digits, x1.1.stats)

      x1 <- c(x1.1, x1.2)

      # fit regression model
      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      x1.miss <- x1.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        x1.miss <- 0
        x1.keep <- x1.keep + 1
      }
    }

    # Change x1.2
    {
      x1.2 <- getSample(x1.2, x1.h, x1.tol, x1.digits, x1.2.stats)

      x1 <- c(x1.1, x1.2)

      # fit regression model
      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      x1.miss <- x1.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        x1.miss <- 0
        x1.keep <- x1.keep + 1
      }
    }

    # Change y1
    {
      y1 <- getSample(y1, y.h, y.tol, y.digits, y1.stats)

      y <- c(y1, y2)

      # fit regression model
      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      y.miss <- y.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        y.miss <- 0
        y.keep <- y.keep + 1
      }
    }

    # Change y2
    {
      y2 <- getSample(y2, y.h, y.tol, y.digits, y2.stats)

      y <- c(y1, y2)

      # fit regression model
      fit.coef <- summary(lm(y ~ x1 + x2))
      fit.x1.pvalue <- fit.coef$coefficients[2,4]
      fit.x2.pvalue <- fit.coef$coefficients[3,4]

      y.miss <- y.miss + 1

      accept1 <- ( checkpValue(fit.x1.pvalue, x1.alpha, x1.opt) && checkpValue(fit.x2.pvalue, x2.alpha, x2.opt) )
      new.dist <- getDist(fit.x1.pvalue, x1.alpha, x1.opt) + getDist(fit.x2.pvalue, x2.alpha, x2.opt)
      accept2 <- (new.dist > dist)

      if (accept1 && accept2)
      {
        step <- i
        y.best <- y
        x1.best <- x1
        x2.best <- x2
        x1.pvalue <- fit.x1.pvalue
        x2.pvalue <- fit.x2.pvalue
        dist <- new.dist
        y.miss <- 0
        y.keep <- y.keep + 1
      }
    }

    if (output) print(sprintf("%1.8f %1.8f %1.8f %d (%d) %d (%d) %d (%d)", x1.pvalue, x2.pvalue, dist, x1.keep, x1.miss, y.keep, y.miss, i, step))
  }

  y <- y.best
  x1 <- x1.best
  x2 <- x2.best

  data.set <- data.frame(y, x1, x2)

  return(data.set)
}

# Function to check that data is within tol of stats
checkStats <- function(data, stats, tol)
{
  data.stats <- getStats(data)
  result <- all( data.stats >= (stats-tol) & data.stats <= (stats+tol) )

  return(result)
}

# Function to check if pvalue has reached alpha
checkpValue <- function(pvalue, alpha, dir, bound=c(0.01, 0.21))
{
  result <- FALSE

  if (dir == "above") result <- (pvalue > alpha) && (pvalue <= bound[2])

  if (dir == "below") result <- (pvalue < alpha)

  return (result)
}

# Function to return the summary stats for the data (mean, sd, median, min & max)
getStats <- function(data)
{
  stats <- numeric(5)
  stats[1] <- mean(data)
  stats[2] <- sd(data)
  stats[3] <- median(data)
  stats[4] <- min(data)
  stats[5] <- max(data)

  return(stats)
}

# Function to return the number of decimal places
getDigits <- function(x)
{
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

# Function to return a new sample with same stats as data
getSample <- function(data, h, tol, digits, stats)
{
  if ( missing(digits) )
    digits <- getDigits(data)

  if ( missing(stats) )
    stats <- getStats(data)

  n <- length(data)

  repeat
  {
    data.new <- data + rnorm(n, mean=0, sd=h)
    data.new <- round(data.new, digits)

    if (checkStats(data.new, stats, tol)) break
  }

  return(data.new)
}

# Function to return the tolerance vector (mean, sd, median, min & max)
getTol <- function(data)
{
  stats <- getStats(data)
  digits <- max(sapply(data, getDigits))

  # tol1 is 2% of the range used for mean and sd
  tol1 <- (stats[5] - stats[4]) * 0.02

  # tol2 is based on the number of sig digits and is used for min & max
  tol2 <- max(tol1, 5/(10^digits))

  # tol3 is based on the number of sig digits and number of obs and is used for median
  tol3 <- 1/(10^digits)
  if (length(data)%%2 == 0) tol3 <- tol3/2

  tol <- c(rep(tol1, 2), tol3, rep(tol2, 2))

  return(tol)
}

# Function to get how close the pvalue is to alpha
getDist <- function(pvalue, alpha, dir)
{
  #if (dir == "above") result <- max(0, pvalue - alpha)

  #if (dir == "below") result <- max(0, alpha - pvalue)

  if (dir == "above") result <- pvalue - alpha

  if (dir == "below") result <- alpha - pvalue

  return (result)
}
