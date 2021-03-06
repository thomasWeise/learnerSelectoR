% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample.R
\name{learning.sample.uniform}
\alias{learning.sample.uniform}
\title{Uniformly Randomly Create a Set of Divisions of \code{n} Elements into
  Test and Training Sets}
\usage{
learning.sample.uniform(n)
}
\arguments{
\item{n}{the integer number of samples to be drawn}
}
\value{
a list of lists, where each list contains two vectors \code{training} and \code{test}.
}
\description{
Given a number \code{n} of objects, create a list of divisions
  of these \code{n} objects into (larger) training and (smaller) test sets.
  The resulting list itself contains lists each having two elements, a vector
  \code{training} and a vector \code{test}. Both vectors are disjoint (except
  in case \code{n=1}) and together contain all elements from \code{1:n}. The
  list of lists can then use to perform multiple runs of a learning
  algorithm, always putting in the training data selected according to the
  \code{training} vector and checking the performance on the data selected
  according to the \code{test} vector.

  The idea is that the produced list can be used to perform cross-validation
  like learning.
}
