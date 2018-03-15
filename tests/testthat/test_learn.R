library("learnerSelectoR")
context("learning.learn")

test_that("learning.learn full test", {
  # make a blueprint function: an exponential quadratic equation
  f.x.par <- function(x, par) exp(par[1] + (x * (par[2] + (x * par[3]))));
  # set the 'true' parameters
  orig <- c(0.3, 1.5, -0.25);
  # generate the 'true' process
  f.x <- function(x) f.x.par(x, orig);
  # generate some data
  x.true <- (-100:100) / 50;
  y.true <- f.x(x.true);
  # add noise
  x.noisy <- rnorm(n=length(x.true), mean=x.true, sd=0.05);
  y.noisy <- abs(rnorm(n=length(y.true), mean=y.true, sd=0.05*abs(y.true)));
  # create data set
  data <- list(x=x.noisy, y=y.noisy, backward=identity);

  # create representations
  representation.1 <- data;
  representation.2 <- list(x=data$x, y=log(data$y),   backward=exp);
  representation.3 <- list(x=data$x, y=sqrt(data$y),  backward=function(x) x*x);
  representation.4 <- list(x=data$x, y=exp(data$y),   backward=function(x) log(abs(x)));
  representations <- list(representation.1,
                          representation.2,
                          representation.3,
                          representation.4);

  # create the models
  model.1 <- list(model=function(x, par) (par[1] + (x * (par[2] + (x * par[3])))), size=3L);
  model.2 <- list(model=function(x, par) (par[1] + (x * (par[2]))), size=2L);
  model.3 <- list(model=function(x, par) exp(par[1] + (x * par[2])), size=2L);
  model.4 <- list(model=function(x, par) exp(par[1] + (x * x * par[2])), size=2L);
  model.5 <- list(model=function(x, par) exp(par[1] + (x * (par[2] + (x * (par[3] + (x * par[4])))))), size=4L);
  model.6 <- list(model=function(x, par) (par[1] + (x * (par[2] + (x * (par[3] + (x * par[4])))))), size=4L);

  # our internal class for representing learning results
  setClass(Class = "Model.Fitted",
           representation = representation(par="numeric", model="list", backward="function"),
           contains = "learning.Result");

  # create the learners
  require(minpack.lm);
  learners <- lapply(X=list(model.1,
                            model.2,
                            model.3,
                            model.4,
                            model.5,
                            model.6),
                     FUN=function(model) {
                       model<-force(model);
                       return(function(data) {
                         tryCatch(
                           fitted <- nls.lm(par=runif(n=model$size, min=-1, max=1),
                                            fn=function(par) data$y-model$model(data$x, par)),
                           warning=function(e) {} );
                         if(is.null(fitted)) { return(NULL); }
                         if(!(is.finite(fitted$deviance))) { return(NULL); }
                         fitted <- force(fitted$par);
                         q <- data$metric(f=model$model, par=fitted);
                         q <- force(q);
                         if((!(is.finite(q))) || (q < 0)) { return(NULL); }
                         result <- (new("Model.Fitted",
                                    quality=q,
                                    size=model$size,
                                    par=fitted,
                                    model=model,
                                    backward=data$backward));
                         result <- force(result);
                         return(result);
                       })
                     });

  # create the selector for the data
  selector <- function(data, selection, index) {
    selected <- data;
    if(!is.null(selection)) {
      selected$x <- selected$x[selection];
      selected$y <- selected$y[selection];
    }
    # also create a metric
    selected$metric <- function(f, par) base::sqrt(base::mean( (selected$y - f(selected$x, par)) ^2 ) )
    selected$metric <- force(selected$metric);
    selected <- force(selected);
    return(selected);
  }

  # create the selector for the data
  test.selector <- function(data, selection, index) { selector(representation.1, selection) }


  # create the test quality function
  test.quality <- function(data, result) data$metric(
                      f=function(x, par) result@backward(result@model$model(x, par)),
                      par=result@par)

  # run the learner (finally)
  result <- learning.learn(data=data,
                           data.size=length(data$x),
                           representations=representations,
                           learners=learners,
                           selector=selector,
                           test.selector=test.selector,
                           test.quality=test.quality);

  expect_is(result, "Model.Fitted");
  expect_true(learning.checkQuality(result@quality));
  expect_true(learning.checkSize(result@size));
  expect_gte(result@size, 2);
  expect_lte(result@size, 4);
  expect_true(all(is.finite(result@par)));
  expect_true(is.function(result@backward));
  expect_true(is.list(result@model));
})


test_that("learning.learn test with single representation", {
  # make a blueprint function: an exponential quadratic equation
  f.x.par <- function(x, par) exp(par[1] + (x * (par[2] + (x * par[3]))));
  # set the 'true' parameters
  orig <- c(0.3, 1.5, -0.25);
  # generate the 'true' process
  f.x <- function(x) f.x.par(x, orig);
  # generate some data
  x.true <- (-100:100) / 50;
  y.true <- f.x(x.true);
  # add noise
  x.noisy <- rnorm(n=length(x.true), mean=x.true, sd=0.05);
  y.noisy <- abs(rnorm(n=length(y.true), mean=y.true, sd=0.05*abs(y.true)));
  # create data set
  data.real <- list(x=x.noisy, y=y.noisy, backward=identity);


  # create the models
  model.1 <- list(model=function(x, par) (par[1] + (x * (par[2] + (x * par[3])))), size=3L);
  model.2 <- list(model=function(x, par) (par[1] + (x * (par[2]))), size=2L);
  model.3 <- list(model=function(x, par) exp(par[1] + (x * par[2])), size=2L);
  model.4 <- list(model=function(x, par) exp(par[1] + (x * x * par[2])), size=2L);
  model.5 <- list(model=function(x, par) exp(par[1] + (x * (par[2] + (x * (par[3] + (x * par[4])))))), size=4L);
  model.6 <- list(model=function(x, par) (par[1] + (x * (par[2] + (x * (par[3] + (x * par[4])))))), size=4L);

  # our internal class for representing learning results
  setClass(Class = "Model.Fitted",
           representation = representation(par="numeric", model="list", backward="function"),
           contains = "learning.Result");

  # create the learners
  require(minpack.lm);
  learners <- lapply(X=list(model.1,
                            model.2,
                            model.3,
                            model.4,
                            model.5,
                            model.6),
                     FUN=function(model) {
                       model<-force(model);
                       return(function(data) {
                         tryCatch(
                           fitted <- nls.lm(par=runif(n=model$size, min=-1, max=1),
                                            fn=function(par) data$y-model$model(data$x, par)),
                           warning=function(e) {} );
                         if(is.null(fitted)) { return(NULL); }
                         if(!(is.finite(fitted$deviance))) { return(NULL); }
                         fitted <- force(fitted$par);
                         q <- data$metric(f=model$model, par=fitted);
                         q <- force(q);
                         if((!(is.finite(q))) || (q < 0)) { return(NULL); }
                         result <- (new("Model.Fitted",
                                        quality=q,
                                        size=model$size,
                                        par=fitted,
                                        model=model,
                                        backward=data$backward));
                         result <- force(result);
                         return(result);
                       })
                     });

  # create the selector for the data
  selector <- function(data, selection, index) {
    selected <- data;
    if(!is.null(selection)) {
      selected$x <- selected$x[selection];
      selected$y <- selected$y[selection];
    }
    # also create a metric
    selected$metric <- function(f, par) base::sqrt(base::mean( (selected$y - f(selected$x, par)) ^2 ) )
    selected$metric <- force(selected$metric);
    selected <- force(selected);
    return(selected);
  }

  # create the selector for the data
  test.selector <- function(data, selection, index) { selector(data.real, selection) }


  # create the test quality function
  test.quality <- function(data, result) data$metric(
    f=function(x, par) result@backward(result@model$model(x, par)),
    par=result@par)

  # run the learner (finally)
  result <- learning.learn(data=data.real,
                           data.size=length(data.real$x),
                           representations=NULL,
                           learners=learners,
                           selector=selector,
                           test.selector=test.selector,
                           test.quality=test.quality);

  expect_is(result, "Model.Fitted");
  expect_true(learning.checkQuality(result@quality));
  expect_true(learning.checkSize(result@size));
  expect_gte(result@size, 2);
  expect_lte(result@size, 4);
  expect_true(all(is.finite(result@par)));
  expect_true(is.function(result@backward));
  expect_true(is.list(result@model));
})



test_that("learning.learn with a single model that does not fit too well", {
  # make a blueprint function: an exponential quadratic equation
  f.x.par <- function(x, par) exp(par[1] + (x * (par[2] + (x * par[3]))));
  # set the 'true' parameters
  orig <- c(0.3, 1.5, -0.25);
  # generate the 'true' process
  f.x <- function(x) f.x.par(x, orig);
  # generate some data
  x.true <- (-100:100) / 50;
  y.true <- f.x(x.true);
  # add noise
  x.noisy <- rnorm(n=length(x.true), mean=x.true, sd=0.05);
  y.noisy <- abs(rnorm(n=length(y.true), mean=y.true, sd=0.05*abs(y.true)));
  # create data set
  data <- list(x=x.noisy, y=y.noisy, backward=identity);

  # create representations
  representation.1 <- data;
  representation.2 <- list(x=data$x, y=log(data$y),   backward=exp);
  representation.3 <- list(x=data$x, y=sqrt(data$y),  backward=function(x) x*x);
  representation.4 <- list(x=data$x, y=exp(data$y),   backward=function(x) log(abs(x)));
  representations <- list(representation.1,
                          representation.2,
                          representation.3,
                          representation.4);

  # create the models
  model.1 <- list(model=function(x, par) (par[1] + (x * (par[2]))), size=2L);

  # our internal class for representing learning results
  setClass(Class = "Model.Fitted",
           representation = representation(par="numeric", model="list", backward="function"),
           contains = "learning.Result");

  # create the learners
  require(minpack.lm);
  learners <- lapply(X=list(model.1),
                     FUN=function(model) {
                       model<-force(model);
                       return(function(data) {
                         tryCatch(
                           fitted <- nls.lm(par=runif(n=model$size, min=-1, max=1),
                                            fn=function(par) data$y-model$model(data$x, par)),
                           warning=function(e) {} );
                         if(is.null(fitted)) { return(NULL); }
                         if(!(is.finite(fitted$deviance))) { return(NULL); }
                         fitted <- force(fitted$par);
                         q <- data$metric(f=model$model, par=fitted);
                         q <- force(q);
                         if((!(is.finite(q))) || (q < 0)) { return(NULL); }
                         result <- (new("Model.Fitted",
                                        quality=q,
                                        size=model$size,
                                        par=fitted,
                                        model=model,
                                        backward=data$backward));
                         result <- force(result);
                         return(result);
                       })
                     });

  # create the selector for the data
  selector <- function(data, selection, index) {
    selected <- data;
    if(!is.null(selection)) {
      selected$x <- selected$x[selection];
      selected$y <- selected$y[selection];
    }
    # also create a metric
    selected$metric <- function(f, par) base::sqrt(base::mean( (selected$y - f(selected$x, par)) ^2 ) )
    selected$metric <- force(selected$metric);
    selected <- force(selected);
    return(selected);
  }

  # create the selector for the data
  test.selector <- function(data, selection, index) { selector(representation.1, selection) }


  # create the test quality function
  test.quality <- function(data, result) data$metric(
    f=function(x, par) result@backward(result@model$model(x, par)),
    par=result@par)

  # run the learner (finally)
  result <- learning.learn(data=data,
                           data.size=length(data$x),
                           representations=representations,
                           learners=learners,
                           selector=selector,
                           test.selector=test.selector,
                           test.quality=test.quality);

  expect_is(result, "Model.Fitted");
  expect_true(learning.checkQuality(result@quality));
  expect_true(learning.checkSize(result@size));
  expect_gte(result@size, 2);
  expect_lte(result@size, 4);
  expect_true(all(is.finite(result@par)));
  expect_true(is.function(result@backward));
  expect_true(is.list(result@model));
})




test_that("learning.learn test with single representation and model", {
  # make a blueprint function: an exponential quadratic equation
  f.x.par <- function(x, par) exp(par[1] + (x * (par[2] + (x * par[3]))));
  # set the 'true' parameters
  orig <- c(0.3, 1.5, -0.25);
  # generate the 'true' process
  f.x <- function(x) f.x.par(x, orig);
  # generate some data
  x.true <- (-100:100) / 50;
  y.true <- f.x(x.true);
  # add noise
  x.noisy <- rnorm(n=length(x.true), mean=x.true, sd=0.05);
  y.noisy <- abs(rnorm(n=length(y.true), mean=y.true, sd=0.05*abs(y.true)));
  # create data set
  data.real <- list(x=x.noisy, y=y.noisy, backward=identity);

  # create the models
  model.1 <- list(model=function(x, par) exp(par[1] + (x * par[2])), size=2L);

  # our internal class for representing learning results
  setClass(Class = "Model.Fitted",
           representation = representation(par="numeric", model="list", backward="function"),
           contains = "learning.Result");

  # create the learners
  require(minpack.lm);
  learners <- lapply(X=list(model.1),
                     FUN=function(model) {
                       model<-force(model);
                       return(function(data) {
                         tryCatch(
                           fitted <- nls.lm(par=runif(n=model$size, min=-1, max=1),
                                            fn=function(par) data$y-model$model(data$x, par)),
                           warning=function(e) {} );
                         if(is.null(fitted)) { return(NULL); }
                         if(!(is.finite(fitted$deviance))) { return(NULL); }
                         fitted <- force(fitted$par);
                         q <- data$metric(f=model$model, par=fitted);
                         q <- force(q);
                         if((!(is.finite(q))) || (q < 0)) { return(NULL); }
                         result <- (new("Model.Fitted",
                                        quality=q,
                                        size=model$size,
                                        par=fitted,
                                        model=model,
                                        backward=data$backward));
                         result <- force(result);
                         return(result);
                       })
                     });

  # create the selector for the data
  selector <- function(data, selection, index) {
    selected <- data;
    if(!is.null(selection)) {
      selected$x <- selected$x[selection];
      selected$y <- selected$y[selection];
    }
    # also create a metric
    selected$metric <- function(f, par) base::sqrt(base::mean( (selected$y - f(selected$x, par)) ^2 ) )
    selected$metric <- force(selected$metric);
    selected <- force(selected);
    return(selected);
  }

  # create the selector for the data
  test.selector <- function(data, selection, index) { selector(data.real, selection) }


  # create the test quality function
  test.quality <- function(data, result) data$metric(
    f=function(x, par) result@backward(result@model$model(x, par)),
    par=result@par)

  # run the learner (finally)
  result <- learning.learn(data=data.real,
                           data.size=length(data.real$x),
                           representations=NULL,
                           learners=learners,
                           selector=selector,
                           test.selector=test.selector,
                           test.quality=test.quality);

  expect_is(result, "Model.Fitted");
  expect_true(learning.checkQuality(result@quality));
  expect_true(learning.checkSize(result@size));
  expect_gte(result@size, 2);
  expect_lte(result@size, 4);
  expect_true(all(is.finite(result@par)));
  expect_true(is.function(result@backward));
  expect_true(is.list(result@model));
})
