# R Package for Automatically Selecting and Applying a (Machine) Learning Method

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/learnerSelectoR/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/learnerSelectoR/)

## Introduction
This package provides an infrastructure for selecting the right Machine Learning
method to process a given dataset automatically and then also applies it. This package thus
is something which will likely be used under-the-hood by our other packages to come. The goal
is to make a better, more robust choice regarding which Machine Learning method to use than
just applying all to the complete data and picking the best results. However, we here are
still at a very early stage in this development.

This package is intented to work together with other packages for model fitting or regression,
classification, and clustering. The goal is to provide an abstract method to select the right
model or learning algorithm: The main method, `learning.learn` is provided with
the input data and the size of the input data. It does not need to know the exact nature of
the data, though. It is further provided with functions that can be used to partition the
data based on a selection of sample indices. This way, it can internally realize
cross-validation. The method also is provided a set of learning methods (or models) in form
of a list of functions. After choosing the learning method or model which generalizes
better than the others and produces models of small size, this method is then applied to the
complete dataset and its result is returned.

The learning method can also receive the input data in different representations: Let's say
you want to fit a model to some x-y data. Then you can do that both on the original data as
well as on log-scaled data. (Testing is always done on the original data). This way you have
two possible realizations of the same model. The one which generalizes better is chosen.
    
## Installation

You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/learnerSelectoR")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")
    
## License

The copyright holder of this package is Prof. Dr. Thomas Weise (see Contact).
The package is licensed under the  GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007.
    
## Contact

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
