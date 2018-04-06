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
.learn.compare <- function(a.quality.1, a.quality.2,
                           b.quality.1, b.quality.2,
                           threshold,
                           a.size, b.size) {

  # our primary criterion is the quality
  if(is.finite(a.quality.1) && (a.quality.1 >= 0)) {
    # a has a finite quality
    if(is.finite(b.quality.1) && (b.quality.1 >= 0)) {
      # b too, so we can compare them
      if(a.quality.1 != b.quality.1) {
        # the qualities are not identical
        diff <- abs( 2 * ((a.quality.1 - b.quality.1) / (a.quality.1 + b.quality.1)) );
        if((!(is.finite(diff))) || (diff > threshold)) {
          # and either too big to compute the threshold, or the difference
          # is bigger than what can be ignored - so we compare them
          if(a.quality.1 < b.quality.1) { return(-1L); }
          if(a.quality.1 > b.quality.1) { return(1L); }
        } # difference is small
      } # qualities are the same
    } else { # quality a is finite, b is not
      return(-1L);
    }
  } else { # quality a is not finite
    if(is.finite(b.quality.1) && (b.quality.1 >= 0)) {
      return(1L); # but b is, so b is better
    }
    # both qualities are infinite, so no one wins
  }

  # if the qualities are either identical or the differences are small, we
  # pick the smaller model/result
  if(a.size < b.size) { return(-1L); } # a is smaller
  if(a.size > b.size) { return(1L); } # a is bigger

  # ok, so models have same size ... check again qualities, ignore thresholds
  if(a.quality.1 < b.quality.1) { return(-1L); }
  if(a.quality.1 > b.quality.1) { return(1L); }

  # test qualities are either all non-finite or identical, model sizes are
  # same, so we now check the training qualities
  if(is.finite(a.quality.2) && (a.quality.2 >= 0)) {# == learning.checkQuality(a.quality.2)) {
    # a has a finite quality
    if(is.finite(b.quality.2) && (b.quality.2 >= 0)) {# == learning.checkQuality(b.quality.2)) {
      # b too, so we can compare them
      if(a.quality.2 < b.quality.2) { return(-1L); }
      if(a.quality.2 > b.quality.2) { return(1L); }
      return(0L);
    } else { # quality a is finite, b is not
      return(-1L);
    }
  } else { # quality a is not finite
    if(is.finite(b.quality.2) && (b.quality.2 >= 0)) {# == learning.checkQuality(b.quality.2)) {
      return(1L); # but b is, so b is better
    }
    # both qualities are infinite, so no one wins
  }


  return(0L); # no discernable difference
}

.learn.compare.qAndR <- function(a.quality, b.quality, threshold, a.result, b.result) {
  if(is.null(a.result)) {
    a.size <- .Machine$integer.max;
    a.testq <- +Inf;
  } else {
    a.size <- a.result@size;
    a.testq <- a.result@quality;
  }

  if(is.null(b.result)) {
    b.size <- .Machine$integer.max;
    b.testq <- +Inf;
  } else {
    b.size <- b.result@size;
    b.testq <- b.result@quality;
  }

  return(.learn.compare( a.quality,
                         a.testq,
                         b.quality,
                         b.testq,
                         threshold,
                         a.size,
                         b.size));
}

.learn.compare.r <- function(a.result, b.result, threshold) {
  if(is.null(a.result)) {
    if(is.null(b.result)) { return(0L); }
    return(1L);
  } else {
    if(is.null(b.result)) { return(-1L); }
  }

  return(.learn.compare( a.result@quality,
                         a.result@quality,
                         b.result@quality,
                         b.result@quality,
                         threshold,
                         a.result@size,
                         b.result@size));
}
