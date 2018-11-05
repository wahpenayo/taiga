# overview

Taiga \cite{taiga-2015} is a machine learning library written in
Clojure. 

As of December 2015, Taiga offers a general pattern for random forest models, 
with specific implementations for least squares regression, 
binary classification, and binary class probability estimation. 

Random forest regression/classification is widely used in machine learning, 
performing 'remarkably well' on many data sets 
(p.590 \cite{hastie-tibshirani-friedman-2009}).
It is also arguably the simplest useful machine learning algorithm
--- so simple that a working implementation can be easily produced
in a day or 2. 

As of December 2015, Taiga is about 700 lines of Clojure.
It is based on an earlier tutorial implementation of random forest
regression, in about 200 lines of Clojure, which was in fact written
over the long New Year's weekend of 2012\cite{rfrk-video-2012}.

Taiga represents both data and models in orders of
magnitude less memory than the widely used R
randomForest\cite{r-randomForest} package.
Taiga is much faster than R (*TODO: some real measurements.*), 
running on a single core, and its speed scales roughly linearly with the
number of cores.
