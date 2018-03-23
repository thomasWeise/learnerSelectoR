#' @title Check a Learning Quality Value
#' @description Check whether a value \code{quality} is a permissible value for
#'   the \code{quality} field of \code{\link{learning.Result}}.
#' @param quality the quality value
#' @return \code{TRUE} if the quality value is permissible, \code{FALSE} otherwise
#' @export learning.checkQuality
learning.checkQuality <- function(quality)  is.finite(quality) &&
                                            (quality >= 0) &&
                                            (length(quality) == 1L)

#' @title Check a Learning Size Value
#' @description Check whether a value \code{size} is a permissible value for the
#'   \code{size} field of \code{\link{learning.Result}}.
#' @param size the size value
#' @return \code{TRUE} if the size  value is permissible, \code{FALSE} otherwise
#' @export learning.checkSize
learning.checkSize <- function(size)  is.integer(size) && (size >= 0) &&
                                      (size <= .Machine$integer.max) &&
                                      (length(size) == 1L)
