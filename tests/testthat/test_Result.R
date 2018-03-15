library("learnerSelectoR")
context("learning.Result")

test_that("Test learning.Result constructor", {
  size <- 10L
  quality <- 12;
  instance <- new("learning.Result", quality=quality, size=size);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@size, size);


  quality <- 12;
  instance <- new("learning.Result", quality=quality);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@size, .Machine$integer.max);
})

test_that("Test learning.Result constructor error", {
  size <- 1
  quality <- 12;
  expect_error(new("learning.Result", quality=quality, size=size));

  size <- -1L;
  quality <- 12;
  expect_error(new("learning.Result", quality=quality, size=size));

  size <- 1L;
  quality <- -12;
  expect_error(new("learning.Result", quality=quality, size=size));

  size <- 7L;
  expect_error(new("learning.Result", size=size));

  instance <- new("learning.Result");
  expect_error(validObject(instance));
})

test_that("Test learning.Result.new", {
  size <- 10L
  quality <- 12;
  instance <- learning.Result.new(quality=quality, size=size);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@size, size);


  quality <- 12;
  instance <- learning.Result.new(quality=quality);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@size, .Machine$integer.max);
})

test_that("Test learning.Result.finalize", {
  size <- 10L
  quality <- 12;
  instance <- learning.Result.new(quality=quality, size=size);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@size, size);
  copy <- learning.Result.finalize(instance);
  expect_identical(instance, copy);
})

test_that("Test learning.Result.new error", {
  size <- 1
  quality <- 12;
  expect_error(learning.Result.new(quality=quality, size=size));

  size <- -1L;
  quality <- 12;
  expect_error(learning.Result.new(quality=quality, size=size));

  size <- 1L;
  quality <- -12;
  expect_error(learning.Result.new(quality=quality, size=size));

  size <- 7L;
  expect_error(learning.Result.new(size=size));

  expect_error(learning.Result.new());
})
