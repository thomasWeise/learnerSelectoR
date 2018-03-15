library("learnerSelectoR")
context("rank.by.comparison")

test_that("rank.by.comparison for small numbers", {
  expect_identical(
    rank.by.comparison(c(1L), function(a, b) a-b),
    c(1L));


  expect_identical(
    rank.by.comparison(c(1L, 2L), function(a, b) a-b),
    c(1L, 2L));

  expect_identical(
    rank.by.comparison(c(2L, 1L), function(a, b) a-b),
    c(2L, 1L));

  expect_identical(
    rank.by.comparison(c(1L, 2L), function(a, b) b-a),
    c(2L, 1L));

  expect_identical(
    rank.by.comparison(c(2L, 1L), function(a, b) b-a),
    c(1L, 2L));

  expect_identical(
    rank.by.comparison(c(1L, 2L), function(a, b) 0),
    c(1.5, 1.5));



  expect_identical(
    rank.by.comparison(c(1L, 2L, 3L), function(a, b) a-b),
    c(1L, 2L, 3L));
  expect_identical(
    rank.by.comparison(c(1L, 3L, 2L), function(a, b) a-b),
    c(1L, 3L, 2L));
  expect_identical(
    rank.by.comparison(c(3L, 2L, 1L), function(a, b) a-b),
    c(3L, 2L, 1L));

  expect_identical(
    rank.by.comparison(c(1L, 2L, 3L), function(a, b) b-a),
    c(3L, 2L, 1L));
  expect_identical(
    rank.by.comparison(c(1L, 3L, 2L), function(a, b) b-a),
    c(3L, 1L, 2L));
  expect_identical(
    rank.by.comparison(c(3L, 2L, 1L), function(a, b) b-a),
    c(1L, 2L, 3L));

  expect_identical(
    rank.by.comparison(c(1L, 2L, 3L), function(a, b) 0),
    c(2L, 2L, 2L));
  expect_identical(
    rank.by.comparison(c(1L, 3L, 2L), function(a, b) 0),
    c(2L, 2L, 2L));
  expect_identical(
    rank.by.comparison(c(3L, 2L, 1L), function(a, b) 0),
    c(2L, 2L, 2L));

})


test_that("rank.by.comparison for random results", {
  for(i in 1:25) {
    n <- (2L+as.integer(runif(n=1, min=1L, max=75L)));
    if(runif(n=1) <= 0.5) { data <- runif(n=n); }
    else                  { data <- sample(x=n, size=n, replace=(runif(n=1) <= 0.5)); }
    smaller <- (runif(n=1) <= 0.5);
    if(smaller) { func <- function(a, b) a-b; }
    else        { func <- function(a, b) b-a; }

    ranks <- rank.by.comparison(data, func);
    for(i in 1:n) {
      ri <- ranks[i];
      expect_gt(ri, 0);
      expect_lte(ri, n);
      rii <- as.integer(ri)
      expect_true((ri==rii) || ((ri - rii) == 0.5));

      if(i > 1) {
        for(j in 1:(i-1)) {
          if(data[i] < data[j]) {
            if(smaller) {
              expect_lt(ri, ranks[j]);
            } else {
              expect_gt(ri, ranks[j]);
            }
          } else {
            if(data[i] > data[j]) {
              if(smaller) {
                expect_gt(ri, ranks[j]);
              } else {
                expect_lt(ri, ranks[j]);
              }
            } else {
              expect_identical(ri, ranks[j]);
            }
          }
        }
      }
    }
  }
})
