library("learnerSelectoR")
context("learning.check")

test_that("learning.checkQuality", {
  expect_true(learning.checkQuality(1));
  expect_true(learning.checkQuality(0));
  expect_true(learning.checkQuality(10));
  expect_false(learning.checkQuality(-1));
  expect_false(learning.checkQuality(+Inf));
  expect_false(learning.checkQuality(NA));
  expect_false(learning.checkQuality(-Inf));
  expect_false(learning.checkQuality(NaN));
  expect_false(learning.checkQuality(c(1,3)));
})

test_that("learning.checkSize", {
  expect_true(learning.checkSize(1L));
  expect_true(learning.checkSize(0L));
  expect_true(learning.checkSize(10L));
  expect_false(learning.checkSize(-1));
  expect_false(learning.checkSize(-1L));
  expect_false(learning.checkSize(1));
  expect_false(learning.checkSize(0));
  expect_false(learning.checkSize(10));
  expect_false(learning.checkSize(+Inf));
  expect_false(learning.checkSize(NA));
  expect_false(learning.checkSize(-Inf));
  expect_false(learning.checkSize(NaN));
  expect_false(learning.checkSize(c(1L,2L)));
})
