library("learnerSelectoR")
context(".learn.compare")

test_that(".learn.compare", {
  expect_identical(.learn.compare(1, 0, 1, 0, 0, 1L, 1L), 0L);
  expect_identical(.learn.compare(1, 1, 2, 1, 0, 1L, 1L), -1L);
  expect_identical(.learn.compare(2, 2, 1, 2, 0, 1L, 1L), 1L);
  expect_identical(.learn.compare(1, 0, 1, 0, 0, 1L, 2L), -1L);
  expect_identical(.learn.compare(1, 1, 1, 1, 0, 2L, 1L), 1L);

  expect_identical(.learn.compare(1, 1, 1, 1, 0, 1L, 1L), 0L);
  expect_identical(.learn.compare(1, 1, 1, 2, 0, 1L, 1L), -1L);
  expect_identical(.learn.compare(1, 2, 1, 1, 0, 1L, 1L), 1L);
  expect_identical(.learn.compare(1, 1, 1, 1, 0, 1L, 2L), -1L);
  expect_identical(.learn.compare(1, 1, 1, 1, 0, 2L, 1L), 1L);
  expect_identical(.learn.compare(1, 2, 1, 1, 0, 1L, 2L), -1L);
  expect_identical(.learn.compare(1, 1, 1, 2, 0, 2L, 1L), 1L);
  expect_identical(.learn.compare(1, 1, 1, 2, 0, 1L, 2L), -1L);
  expect_identical(.learn.compare(1, 2, 1, 1, 0, 2L, 1L), 1L);

  expect_identical(.learn.compare(1, 1, 1.1, 1, 0.1, 1L, 2L), -1L);
  expect_identical(.learn.compare(1, 0, 1.1, 0, 0.1, 2L, 1L), 1L);
  expect_identical(.learn.compare(1, 2, 1.1, 2, 0.1, 1L, 1L), -1L);
  expect_identical(.learn.compare(1.1, 0, 1, 0, 0.1, 1L, 2L), -1L);
  expect_identical(.learn.compare(1.1, 1, 1, 1, 0.1, 2L, 1L), 1L);
  expect_identical(.learn.compare(1.1, 0, 1, 0, 0.1, 1L, 1L), 1L);
})
