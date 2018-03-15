#' @include check.R

# Compare two models based on their qualities and size
# @param a.quality either a single quality value or a vector with two elements,
#   the training and test quality, of the first approach
# @param b.quality either a single quality value or a vector with two elements,
#   the training and test quality, of the second approach
# @param threshold the quality equality threshold
# @param a.size the size of the model built by the first approach
# @param b.size the size of the model built by the second approach
# @return -1L if the first model is better, 1L if the second model is better, 0L
#   if there is no difference
.learn.compare <- function(a.quality, b.quality, threshold, a.size, b.size) {
  qa <- a.quality[1];
  qb <- b.quality[1];
  # our primary criterion is the quality
  if(learning.checkQuality(qa)) {
    # a has a finite quality
    if(learning.checkQuality(qb)) {
      # b too, so we can compare them
      if(qa != qb) {
        # the qualities are not identical
        diff <- base::abs( 2 * ((qa - qb) / (qa + qb)) );
        if((!(base::is.finite(diff))) || (diff > threshold)) {
          # and either too big to compute the threshold, or the difference
          # is bigger than what can be ignored - so we compare them
          if(qa < qb) { return(-1L); }
          if(qa > qb) { return(1L); }
        } # difference is small
      } # qualities are the same
    } else { # quality a is finite, b is not
      return(-1L);
    }
  } else { # quality a is not finite
    if(learning.checkQuality(qb)) {
      return(1L); # but b is, so b is better
    }
    # both qualities are infinite, so no one wins
  }

  # if the qualities are either identical or the differences are small, we
  # pick the smaller model/result
  if(a.size < b.size) { return(-1L); } # a is smaller
  if(a.size > b.size) { return(1L); } # a is bigger

  # ok, so models have same size ... check again qualities, ignore thresholds
  if(qa < qb) { return(-1L); }
  if(qa > qb) { return(1L); }

  # test qualities are either all non-finite or identical, model sizes are
  # same, so we now check the training qualities
  if(base::length(a.quality) > 1) {
    qa <- a.quality[2];
    qb <- b.quality[2];

    if(learning.checkQuality(qa)) {
      # a has a finite quality
      if(learning.checkQuality(qb)) {
        # b too, so we can compare them
        if(qa < qb) { return(-1L); }
        if(qa > qb) { return(1L); }
        return(0L);
      } else { # quality a is finite, b is not
        return(-1L);
      }
    } else { # quality a is not finite
      if(learning.checkQuality(qb)) {
        return(1L); # but b is, so b is better
      }
      # both qualities are infinite, so no one wins
    }
  }

  return(0L); # no discernable difference
}

.learn.compare.qAndR <- function(a.quality, b.quality, threshold, a.result, b.result) {
  if(base::is.null(a.result)) {
    a.size <- .Machine$integer.max;
    a.testq <- +Inf;
  } else {
    a.size <- a.result@size;
    a.testq <- a.result@quality;
  }

  if(base::is.null(b.result)) {
    b.size <- .Machine$integer.max;
    b.testq <- +Inf;
  } else {
    b.size <- b.result@size;
    b.testq <- b.result@quality;
  }

  return(.learn.compare( base::c(a.quality, a.testq),
                         base::c(b.quality, b.testq),
                         threshold,
                         a.size,
                         b.size));
}

.learn.compare.r <- function(a.result, b.result, threshold) {
  if(base::is.null(a.result)) {
    a.size <- .Machine$integer.max;
    a.quality <- +Inf;
  } else {
    a.size <- a.result@size;
    a.quality <- a.result@quality;
  }

  if(base::is.null(b.result)) {
    b.size <- .Machine$integer.max;
    b.quality <- +Inf;
  } else {
    b.size <- b.result@size;
    b.quality <- b.result@quality;
  }

  return(.learn.compare( a.quality,
                         b.quality,
                         threshold,
                         a.size,
                         b.size));
}