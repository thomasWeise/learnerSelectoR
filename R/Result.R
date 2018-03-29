#' @include check.R

#' @title The Result of the Application of a Machine Learning or Optimization Technique
#' @description This class is a base class for capturing the result of the
#'   application of a Machine Learning or optimization method. Actually, this
#'   base class only stores a quality measure value, which must be a finite
#'   positive real number or zero and a measure of the "size" of the solution,
#'   e.g., the number of parameters that have been determined, which must be a
#'   positive integer number or zero. Sub-classes can then add more information,
#'   such as the data of the actual solution.
#' @slot quality the quality of the result, smaller values are better
#' @slot size the size of the result, e.g., the number of fixed parameters
#' @exportClass learning.Result
#' @importFrom methods setClass representation prototype
learning.Result <- setClass(
  Class = "learning.Result",
  representation = representation(quality="numeric",
                                  size="integer"),
  prototype = prototype(size=.Machine$integer.max),
  validity = function(object) {
    if(!(learning.checkQuality(object@quality))) {
      return("Quality value of learning result must a positive, finite number or 0.");
    }
    if(!(learning.checkSize(object@size))) {
      return("Size of learning result must be a positive integer or (integer) zero.");
    }
    return(TRUE);
  }
)


#' @title Create a New Instance of \code{\link{learning.Result}}.
#' @description Create a New Instance of \code{\link{learning.Result}}.
#' @param quality the quality of the result, smaller values are better
#' @param size the size of the result, e.g., the number of fixed parameters
#' @return the new instance
#' @importFrom methods new validObject
#' @export learning.Result.new
learning.Result.new <- function(quality, size=.Machine$integer.max) {
  result <- new("learning.Result", quality=quality, size=size);
  result <- force(result);
  result@quality <- force(result@quality);
  result@size <- force(result@size);
  result <- force(result);
  validObject(result);
  return(result);
}

#' @title Finalize a Learning Result
#'
#' @description This method can be invoked after a model learning process has
#'   completely finished. It should canonicalize the result as far as possible
#'   and may perform other tasks. It will not change any characteristics of the
#'   result, though. Since this work may be time-consuming, we put it in an
#'   extra method. Assume, for instance, that your learning task is to fit many
#'   models to a data set and then select a specific model for future use. You
#'   would invoke \code{learning.Result.finalize} only on this selected model.
#' @name learning.Result.finalize
#' @param object the instance of \code{\link{learning.Result}}
#' @return the finalized \code{\link{learning.Result}} instance
#' @importFrom methods setGeneric
#' @exportMethod learning.Result.finalize
#' @docType methods
#' @aliases learning.Result.finalize
#' @rdname learning.Result.finalize
setGeneric(
  name="learning.Result.finalize",
  def=function(object) {
    standardGeneric("learning.Result.finalize")
  }
)

#' @importFrom methods setMethod
#' @aliases learning.Result.finalize,learning.Result-method
#' @rdname learning.Result.finalize
setMethod(
  f="learning.Result.finalize",
  signature="learning.Result",
  definition=function(object) {
    return(object)
  }
)
