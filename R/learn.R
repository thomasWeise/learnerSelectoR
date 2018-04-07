#' @include sample.R
#' @include rank.R
#' @include Result.R
#' @include learn_check.R
#' @include learn_compare.R

# the default selection method treats the data as list or vector
.def.selector <- function(data, selection) data[selection]

# Execute the expression and ignore all errors
# @param exp the expression
.ignore.errors <- function(exp) {
  tryCatch(suppressWarnings(exp), error=function(e) { })
}

#' @title Apply a Set of Machine Learning Methods and Produce a Final Result
#'   with the Most Promising One
#' @description This method applies a set of machine learning methods to a
#' machine learning task. It does so by first performing cross-valdation to
#' choose the method which most likely can generalize well, and then applies
#' this method to the complete dataset.
#'
#' It supports using different data representations.
#'
#' The learning effort value \code{q} tells the system how much effort to invest
#' into the learning. The value is handed down to the learners which may
#' interpret it in a meaningful way. \code{q=0} is the smallest possible effort
#' value and means "Learn as fast as possible, don't care about the result
#' quality." \code{q=1} is the largest possible effort value, meaning "Do
#' whatever you can to increase the learning quality, don't care about the
#' runtime." During the cross-validation, phase the learners are applied with
#' \code{q^3}, which should speed-up this selection procedure, while maintaining
#' that \code{q=1} would still excert maximum effort. During the final
#' application of the selected learner, \code{q} is supplied as-is.
#'
#' @param data the data set based on which we perform the learning task
#' @param data.size the number of elements in the data, i.e., how many samples
#'   we can use, which will be the basis for cross-validation
#' @param representations a vector or list of data representations which should
#'   be used for learning, or \code{NULL} to only use the original \code{data}.
#' @param selector a function which returns a sub-set of a data representation
#'   made suitable for learning: it takes as input a data representation and an
#'   integer array with the selected items and returns a corresponding subset.
#'   If the second argument is \code{NULL}, the selector should return the
#'   complete represented data. Via the selector function, it is also possible
#'   to cache quality measures or something.
#' @param test.selector similar to selector, but used to derive the data for
#'   testing, by default equal to \code{selector}
#' @param test.quality the quality metric used to get the solution quality on
#'   the test data
#' @param q the effort to spent in learning, a value between 0 (min) and 1
#'   (max). Higher values may lead to much more computational time, lower values
#'   to potentially lower result quality.
#' @param threshold the relative difference between two test qualities below
#'   which we will pick the the "smaller" result
#' @param learners a list of learners, functions which accept the output of
#'   the selectors and return an instance of \code{\link{learning.Result}}
#' @return the result of the learning process
#' @export learning.learn
#' @importFrom methods is
learning.learn <- function(data,
                           data.size,
                           learners,
                           test.quality,
                           selector=.def.selector,
                           representations=NULL,
                           test.selector=selector,
                           q=0.75,
                           threshold=4e-3) {

  # Check all arguments
  .learn.check.data(data);
  representations <- .learn.check.representations(data, representations);
  representations.length <- length(representations);
  .learn.check.data.size(data.size);
  .learn.check.selector(selector);
  learners.length <- .learn.check.learners(learners);
  .learn.check.selector(test.selector);
  .learn.check.test.quality(test.quality);
  .learn.check.threshold(threshold);

  # In total, we have representations.length * learners.length possible things
  # that we can do with the data.
  choices <- unlist(lapply(X=1:representations.length, FUN=
                function(i) lapply(X=1:learners.length, FUN=
                function(j) list(representation=i,
                                       learner=j))), recursive = FALSE);
  choices.length <- length(choices);
  choices.rankSum <- rep(x=0L, times=choices.length);
  choices.range <- 1:choices.length;

  # for each division of data
  index   <- 0L;
  if(q >= 1) {
    q.end   <- 1;
    q.train <- 1;
  } else if(q <= 0) {
    q.end   <- 0;
    q.train <- 0;
  } else {
    q.end   <- min(1, max(0, q));
    q.train <- min(1, max(0, q * q * q));
  }
  for(sample in learning.sample.uniform(data.size)) {
    index <- index + 1L;
    # first we select the training data for each representation
    data.use <- lapply(X  =representations,
                       FUN=function(x) selector(x, sample$training, index));
    # then we apply all learners to training data
    results <- lapply(X=choices, FUN=function(c) {
      result <- NULL;
      .ignore.errors(
        result <- learners[[c$learner]](
                   data.use[[c$representation]],
                   q.train));
      if(is(result, "learning.Result")) {
        return(result);
      }
      return(NULL); });
    # now we can select the test data for each representation
    data.use <- lapply(X  =representations,
                       FUN=function(x) test.selector(x, sample$test, index));
    # now get the quality on the test set, setting all undefined qualities to positive infinity
    qualities <- vapply(X=choices.range,
                    FUN=function(c) {
                      res <- results[[c]];
                      if(!(is(res, "learning.Result"))) { return(+Inf); }
                      quality <- (+Inf);
                      .ignore.errors(
                        quality <- test.quality(data.use[[choices[[c]]$representation]], res)
                      );
                      if(learning.checkQuality(quality)) {
                        return(quality);
                      }
                      return(+Inf); }, FUN.VALUE = +Inf);
    # update the rank sums
    choices.rankSum <- choices.rankSum + rank.by.comparison(data=choices.range,
                                                            comparator = function(a, b) {
          return(.learn.compare.qAndR(qualities[a],
                                      qualities[b],
                                      threshold,
                                      results[[a]],
                                      results[[b]])); } );

    choices.rankSum <- force(choices.rankSum);
  }

  # OK, now we have applied the methods to all training and test data.
  # We got the rank sums, where smaller sums should imply better generalization capabilities.
  # We now need to apply the methods to the complete data set, starting with the best-ranked approach.
  # If that one fails, we try the next one.

  best <- NULL;
  lastRank <- (-1L);
  data.use <- vector("list", representations.length);
  index <- index + 1L;
  for(i in order(choices.rankSum)) {

    # check if we are finished
    rank <- choices.rankSum[i];

    if(rank > lastRank) {
      # ok, we have transcended the ranks by one.
      if(!(is.null(best))) {
        break;
      }
      lastRank <- rank;
    }

    # apply the next better-ranked method
    method <- choices[[i]];
    data.this <- data.use[[method$representation]];
    if(is.null(data.this) || is.na(data.this)) {
      data.this <- selector(representations[[method$representation]],
                            NULL, index);
      data.use[[method$representation]] <- data.this;
    }

    result <- NULL;
    .ignore.errors(result <- learners[[method$learner]](data.this, q.end));

    # check if this is the best solution we got so far
    if(is(result, "learning.Result")) {
      if(is.null(best)) { best <- result; }
      else {
        if(.learn.compare.r(result, best, threshold) < 0L) {
          best <- result;
        }
      }
    }
  }

  # finalize the result
  if(!(is.null(best))) {
    best <- learning.Result.finalize(best);
    best <- force(best);
  }
  return(best);
}
