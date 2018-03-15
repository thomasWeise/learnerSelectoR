# check a selector
.learn.check.selector <- function(selector) {
  if(base::is.null(selector) || (!(base::is.function(selector)))) {
    stop("A selector must be a non-null function.");
  }
  if(base::is.primitive(selector)) {
    selector.args <- base::formals(base::args(selector));
  } else {
    selector.args <- base::formals(selector);
  }
  if(!(base::identical(base::names(selector.args), c("data", "selection", "index")))) {
    stop("A selector must be a function with exactly three arguments: 'data', 'selection', and 'index'.");
  }
}

# check the data
.learn.check.data <- function(data) {
  # Check the data.
  if(base::is.null(data)) {
    stop("Data cannot be null.");
  }
}

# check the representations
.learn.check.representations <- function(data, representations) {
  # Check the data representations
  if(base::is.null(representations)) {
    # If representations is NULL, we use data directly
    return(base::list(data));
  }
  # Check whether the data representations are a valid list of non-zero length
  if(!(base::is.list(representations))) {
    stop("The 'representations' must be a list.");
  }
  if(base::length(representations) <= 0) {
    stop("The 'representations' list must include at least one element.");
  }
  return(representations);
}

# check the data size
.learn.check.data.size <- function(data.size) {
  # Check if the data size is a positive integer
  if((!(base::is.integer(data.size))) || (data.size <= 0L) || (base::length(data.size) != 1)) {
    stop("'data.size' must an integer greater than 0.");
  }
}

# check the learners
.learn.check.learners <- function(learners) {
  # Check the learners.
  if(base::is.null(learners) || (!(base::is.list(learners)))) {
    stop("The 'learners' must be a list.");
  }
  learners.length <- base::length(learners);
  if(learners.length <= 0) {
    stop("The 'learners' list must contain at least one learner.");
  }
  for(learner in learners) {
    if(base::is.null(learner) || (!(base::is.function(learner)))) {
      stop("All learners must be non-null functions.")
    }
    if(base::is.primitive(learner)) {
      learner.args <- base::formals(base::args(learner));
    } else {
      learner.args <- base::formals(learner);
    }
    if(!(base::identical(base::names(learner.args), c("data")))) {
      stop("A learner must be a function with exactly one argument named 'data'.");
    }
  }
  return(learners.length);
}

# check the test quality metric
.learn.check.test.quality <- function(test.quality) {
  # Check the quality function
  if(base::is.null(test.quality) || (!(base::is.function(test.quality)))) {
    stop("A test.quality metric must be a non-null binary function.");
  }
  if(base::is.primitive(test.quality)) {
    test.quality.args <- base::formals(base::args(test.quality));
  } else {
    test.quality.args <- base::formals(test.quality);
  }
  if(!(base::identical(base::names(test.quality.args), c("data", "result")))) {
    stop("A test.quality must be a biary function with exactly two arguments: 'data' and 'selection'.");
  }
}

# check the threshold
.learn.check.threshold <- function(threshold) {
  # Check the threshold
  if(!(base::is.finite(threshold) || (threshold < 0) || (base::length(threshold) != 1L))) {
    stop("Threshold must be positive finite number.");
  }
}