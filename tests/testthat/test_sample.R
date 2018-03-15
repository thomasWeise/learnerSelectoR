library("learnerSelectoR")
context("learning.sample.uniform")

test_that("learning.sample.uniform for small values", {
  expect_identical(
    learning.sample.uniform(1L),
    list( list(training=c(1L), test=c(1L))));

  expect_identical(
    learning.sample.uniform(2L),
    list( list(training=c(1L), test=c(2L)),
          list(training=c(2L), test=c(1L))));

  expect_identical(
    learning.sample.uniform(3),
    list( list(training=c(1L, 2L), test=c(3L)),
          list(training=c(1L, 3L), test=c(2L)),
          list(training=c(2L, 3L), test=c(1L))));

  expect_identical(
    learning.sample.uniform(4),
    list( list(training=c(1L, 2L, 3L), test=c(4L)),
          list(training=c(1L, 2L, 4L), test=c(3L)),
          list(training=c(1L, 3L, 4L), test=c(2L)),
          list(training=c(2L, 3L, 4L), test=c(1L))));

  expect_identical(
    learning.sample.uniform(5),
    list( list(training=c(1L, 2L, 3L, 4L), test=c(5L)),
          list(training=c(1L, 2L, 3L, 5L), test=c(4L)),
          list(training=c(1L, 2L, 4L, 5L), test=c(3L)),
          list(training=c(1L, 3L, 4L, 5L), test=c(2L)),
          list(training=c(2L, 3L, 4L, 5L), test=c(1L))));

  expect_identical(
    learning.sample.uniform(6),
    list( list(training=c(1L, 2L, 3L, 4L, 5L), test=c(6L)),
          list(training=c(1L, 2L, 3L, 4L, 6L), test=c(5L)),
          list(training=c(1L, 2L, 3L, 5L, 6L), test=c(4L)),
          list(training=c(1L, 2L, 4L, 5L, 6L), test=c(3L)),
          list(training=c(1L, 3L, 4L, 5L, 6L), test=c(2L)),
          list(training=c(2L, 3L, 4L, 5L, 6L), test=c(1L))));
})

test_that("learning.sample.uniform for bigger values", {
  for(n in 2:100) {
    samples <- learning.sample.uniform(n);
    expect_true(!is.null(samples));
    expect_true(is.list(samples));
    expect_true(length(samples) >= min(n, 10));
    complete <- 1:n;

    for(sample in samples) {
      expect_true(!is.null(sample));
      expect_true(is.list(sample));
      expect_true(length(sample) == 2L);
      training <- sample$training;
      expect_true(!(is.null(training)));
      expect_true(is.vector(training));
      expect_true(all(is.integer(training)));

      test <- sample$test;
      expect_true(!(is.null(test)));
      expect_true(is.vector(test));
      expect_true(all(is.integer(test)));

      expect_identical( sort(c(training, test)), complete);
    }

    for(i in 2:length(samples)) {
      test <- samples[[i]]$test;
      for(j in 1:(i-1)) {
        expect_false(identical(test, samples[[j]]$test));
      }
    }
  }
})
