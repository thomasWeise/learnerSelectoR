#' @title Rank a Given Data Set based on a Comparator Function
#' @description Rank the elements in the set \code{data} based on binary
#'   comparisons via a \code{comparator} function. The function
#'   \code{comparator} accepts two parameters, \code{a} and \code{b}, which will
#'   be two elements from \code{data}. The function then returns a negative if
#'   the first element (\code{a}) should receive a lower rank than the second
#'   element (\code{b}). It returns a positive value if \code{b} should be
#'   ranked smaller than \code{a}. It in all other cases (i.e., if \code{0} or
#'   \code{NaN} are returned), neither element is preferred.
#' @param data the data set, a list or vector of data elements
#' @param comparator a binary comparator function
#' @return a vector of ranks
#' @export rank.by.comparison
rank.by.comparison <- function(data, comparator) {
  # get length of data
  n <- length(data);

  if(n <= 0) {
    stop("There must be at least one element to rank.");
  }
  if(n <= 1) {
    return(c(1L));
  }

  # create list (looks strange, I don't know how to do it better)
  losses <- rep(0L, n);
  equals <- rep(0L, n);

  # unfortunately, quadratic complexity ... I don't have a good sorting method
  # handy/implemented for this, for now it will do.
  for(i in 2:n) {
    a <- data[[i]];
    for(j in 1:(i-1)) {
      res <- comparator(a, data[[j]]);
      if(res < 0L) {
        losses[j] <- losses[j] + 1L;
      } else {
        if(res > 0L) {
          losses[i] <- losses[i] + 1L;
        } else {
          equals[i] <- equals[i] + 1L;
          equals[j] <- equals[j] + 1L;
        }
      }
    }
  }

  # Finalize the computation and return a vector with the ranks.
  # If all ranks are integers, an integer vector is returned.
  return(sapply(X=1:n,
                      FUN=function(c) {
                        rank <- losses[c] + 1L;
                        if(equals[c] <= 0L) { return(rank); }
                        t1 <- 0.5*equals[c];
                        t2 <- as.integer(t1);
                        if(t1 == t2) { return(rank + t2); }
                        return(rank + t1);
                      },
                      simplify="array", USE.NAMES=FALSE));
}
