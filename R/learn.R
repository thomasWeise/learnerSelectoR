#' @include sample.R
#' @include rank.R
#' @include Result.R
#' @include learn_check.R
#' @include learn_compare.R

.def.selector <- function(data, selection) data[selection]

# ignore the argument
# @param e ignored
.ignore<-function(e) { }

# Execute the expression and ignore all errors
# @param exp the expression
.ignore.errors <- function(exp) {
  tryCatch(exp, error=.ignore, warning=.ignore)
}

#' @title Apply a Set of Machine Learning Methods and Produce a Final Result with the Most Promising One
#' @description This method applies a set of machine learning methods to a
#'   machine learning task. It does so by first performing cross-valdation to
#'   choose the method which most likely can generalize well, and then applies
#' this method to the complete dataset.
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
#' @param threshold the relative difference between two test qualities below
#'   which we will pick the the "smaller" result
#' @param learners a list of learners, functions which accept the output of
#'   preprocessor and return an instance of \code{\link{learning.Result}}
#' @return the result of the learning process
#' @export learning.learn
#' @importFrom methods is
learning.learn <- function(data,
                           data.size,
                           representations=NULL,
                           selector=.def.selector,
                           learners,
                           test.selector=selector,
                           test.quality,
                           threshold=4e-3) {

  # Check all arguments
  .learn.check.data(data);
  representations <- .learn.check.representations(data, representations);
  representations.length <- base::length(representations);
  .learn.check.data.size(data.size);
  .learn.check.selector(selector);
  learners.length <- .learn.check.learners(learners);
  .learn.check.selector(test.selector);
  .learn.check.test.quality(test.quality);
  .learn.check.threshold(threshold);

  # In total, we have representations.length * learners.length possible things
  # that we can do with the data.
  choices <- base::unlist(base::lapply(X=1:representations.length, FUN=
                function(i) base::lapply(X=1:learners.length, FUN=
                function(j) base::list(representation=i,
                                       learner=j))), recursive = FALSE);
  choices.length <- base::length(choices);
  choices.rankSum <- base::rep(x=0L, times=choices.length);
  choices.range <- 1:choices.length;

  # for each division of data
  index <- 0L;
  for(sample in learning.sample.uniform(data.size)) {
    index <- index + 1L;
    # first we select the training data for each representation
    data.use <- base::lapply(X=representations, FUN = function(x) selector(x, sample$training, index));
    # then we apply all learners to training data
    results <- base::lapply(X=choices, FUN=function(c) {
      result <- NULL;
      .ignore.errors(result <- learners[[c$learner]](data.use[[c$representation]]));
      if(methods::is(result, "learning.Result")) {
        return(result);
      }
      return(NULL); });
    # now we can select the test data for each representation
    data.use <- base::lapply(X=representations, FUN = function(x) test.selector(x, sample$test, index));
    # now get the quality on the test set, setting all undefined qualities to positive infinity
    qualities <- base::vapply(X=choices.range,
                              FUN=function(c) {
                                res <- results[[c]];
                                if(!(methods::is(res, "learning.Result"))) { return(+Inf); }
                                q <- (+Inf);
                                .ignore.errors(
                                 q <- test.quality(data.use[[choices[[c]]$representation]], res)
                                );
                                if(learning.checkQuality(q)) { return(q); }
                                return(+Inf); }, FUN.VALUE = +Inf);
    # update the rank sums
    choices.rankSum <- choices.rankSum + rank.by.comparison(data=choices.range,
                                                            comparator = function(a, b) {
          return(.learn.compare.qAndR(qualities[a],
                                      qualities[b],
                                      threshold,
                                      results[[a]],
                                      results[[b]])); } );

    choices.rankSum <- base::force(choices.rankSum);
  }

  # OK, now we have applied the methods to all training and test data.
  # We got the rank sums, where smaller sums should imply better generalization capabilities.
  # We now need to apply the methods to the complete data set, starting with the best-ranked approach.
  # If that one fails, we try the next one.

  best <- NULL;
  lastRank <- (-1L);
  data.use <- vector("list", representations.length);
  index <- index + 1L;
  for(i in base::order(choices.rankSum)) {

    # check if we are finished
    rank <- choices.rankSum[i];

    if(rank > lastRank) {
      # ok, we have transcended the ranks by one.
      if(!(base::is.null(best))) {
        break;
      }
      lastRank <- rank;
    }

    # apply the next better-ranked method
    method <- choices[[i]];
    data.this <- data.use[[method$representation]];
    if(base::is.null(data.this) || base::is.na(data.this)) {
      data.this <- selector(representations[[method$representation]], NULL, index);
      data.use[[method$representation]] <- data.this;
    }

    result <- NULL;
    .ignore.errors(result <- learners[[method$learner]](data.this));

    # check if this is the best solution we got so far
    if(methods::is(result, "learning.Result")) {
      if(base::is.null(best)) { best <- result; }
      else {
        if(.learn.compare.r(result, best, threshold) < 0L) {
          best <- result;
        }
      }
    }
  }

  # finalize the result
  if(!(base::is.null(best))) {
    best <- learning.Result.finalize(best);
    best <- base::force(best);
  }
  return(best);
}