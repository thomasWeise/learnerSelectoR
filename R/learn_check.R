# check a selector
#' @importFrom utilizeR function.args
.learn.check.selector <- function(selector) {
  if(is.null(selector) || (!(is.function(selector)))) {
    stop("A selector must be a non-null function.");
  }
  if(!(identical(function.args(selector), c("data", "selection", "index")))) {
    stop("A selector must be a function with exactly three arguments: 'data', 'selection', and 'index'.");
  }
}

# check the data
.learn.check.data <- function(data) {
  # Check the data.
  if(is.null(data)) {
    stop("Data cannot be null.");
  }
}

# check the representations
.learn.check.representations <- function(data, representations) {
  # Check the data representations
  if(is.null(representations)) {
    # If representations is NULL, we use data directly
    return(list(data));
  }
  # Check whether the data representations are a valid list of non-zero length
  if(!(is.list(representations))) {
    stop("The 'representations' must be a list.");
  }
  if(length(representations) <= 0) {
    stop("The 'representations' list must include at least one element.");
  }
  return(representations);
}

# check the data size
.learn.check.data.size <- function(data.size) {
  # Check if the data size is a positive integer
  if((!(is.integer(data.size))) || (data.size <= 0L) || (length(data.size) != 1)) {
    stop("'data.size' must an integer greater than 0.");
  }
}

# check the learners
#' @importFrom utilizeR function.args
.learn.check.learners <- function(learners) {
  # Check the learners.
  if(is.null(learners) || (!(is.list(learners)))) {
    stop("The 'learners' must be a list.");
  }
  learners.length <- length(learners);
  if(learners.length <= 0) {
    stop("The 'learners' list must contain at least one learner.");
  }
  for(learner in learners) {
    if(is.null(learner) || (!(is.function(learner)))) {
      stop("All learners must be non-null functions.")
    }
    if(!(identical(function.args(learner), c("data", "q")))) {
      stop("A learner must be a function with exactly two arguments, named 'data' (the data to learn) and 'q' (effort, between 1 (max) and 0 (min)).");
    }
  }
  return(learners.length);
}

# check the test quality metric
#' @importFrom utilizeR function.args
.learn.check.test.quality <- function(test.quality) {
  # Check the quality function
  if(is.null(test.quality) || (!(is.function(test.quality)))) {
    stop("A test.quality metric must be a non-null binary function.");
  }
  if(!(identical(function.args(test.quality), c("data", "result")))) {
    stop("A test.quality must be a biary function with exactly two arguments: 'data' and 'result'.");
  }
}

# check the threshold
.learn.check.threshold <- function(threshold) {
  # Check the threshold
  if(!(is.finite(threshold) || (threshold < 0) || (length(threshold) != 1L))) {
    stop("Threshold must be positive finite number.");
  }
}
