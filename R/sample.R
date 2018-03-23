#' @title Uniformly Randomly Create a Set of Divisions of \code{n} Elements into
#'   Test and Training Sets
#' @description Given a number \code{n} of objects, create a list of divisions
#'   of these \code{n} objects into (larger) training and (smaller) test sets.
#'   The resulting list itself contains lists each having two elements, a vector
#'   \code{training} and a vector \code{test}. Both vectors are disjoint (except
#'   in case \code{n=1}) and together contain all elements from \code{1:n}. The
#'   list of lists can then use to perform multiple runs of a learning
#'   algorithm, always putting in the training data selected according to the
#'   \code{training} vector and checking the performance on the data selected
#'   according to the \code{test} vector.
#'
#'   The idea is that the produced list can be used to perform cross-validation
#'   like learning.
#' @param n the integer number of samples to be drawn
#' @return a list of lists, where each list contains two vectors \code{training} and \code{test}.
#' @export learning.sample.uniform
learning.sample.uniform <- function(n) {
  if((n <= 0) || (n > .Machine$integer.max)) {
    stop("Number of elements to sample from must be a positive number in the integer range.");
  }
  t <- as.integer(n);
  if(t != n) {
    stop("Number of elements to sample from must be an integer greater than 0.");
  }
  n <- t;

  # if there are less than three samples...
  if(n <= 2L) {
    o <- c(1L);
    if(n <= 1L) {
      # if there is only one sample, we use it for training and test...
      return(list(list(training=o, test=o)));
    }
    # for two samples, we have two single-set choices
    t <- c(2L);
    return(list(
            list(training=o, test=t),
            list(training=t, test=o)));
  }

  if(n <= 25) {
    # for up to 25 samples, we do leave-one-out
    return(lapply(X=n:1, FUN=
                    function(i) {
                      if(i<=1) {
                        training<-2:n;
                      } else {
                        if(i>=n) {
                          training <- 1:(i-1);
                        } else {
                          training <- c(1:(i-1), (i+1):n);
                        }
                      }
                      return(list(training=training, test=c(i)));
                    }));
  }

  # if we have sufficiently many data samples, we perform a ten-fold cross-validation.
  all <- sample.int(n=n);
  return(lapply(X=0:9, FUN=
          function(i) {
            start <- as.integer((i*n)/10) + 1;
            end <- as.integer(((i+1)*n)/10);

            test <- NULL;

            if(start > 1) {
              training <- c(all[1:(start-1)]);
            } else {
              training <- NULL;
            }
            if(end < n) {
              training <- c(training, all[(end+1):n])
            }

            # we sort the training and test set in order to allow for more cache-friendly selections.
            return(list(training=sort(training), test=sort(all[start:end])));
          }));
}