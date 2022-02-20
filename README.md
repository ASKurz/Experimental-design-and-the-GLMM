# Experimental-design-and-the-GLMM

A while back, I [announced on twitter](https://twitter.com/SolomonKurz/status/1464274116341641228) that I wanted to put together a book showing how to use strategies from the generalized linear mixed model (GLMM) to analyze experimental data. This is the public-facing home for that project.

Welcome.

I should unpack a couple terms. I’m using *GLMM* in an expansive way to include not only single- and multilevel models using familiar likelihoods like the Gaussian and binomial, but I’m also thinking in terms of distributional models where all parameters in the likelihood function can have a linear model and of distinctly non-linear model types including splines. By *experimental data*, I mean fully randomized true experiments (between persons, within them, and both) and also quasi-experimental variants, where some of that control is lost for various pragmatic reasons. I plan to aim this textbook at students and researchers within the social sciences. Given my background in clinical psychology, the content will bend in that direction. However, I am keen to bring in examples from other areas within psychology and perhaps other social sciences, as well.

Books of this kind often use examples from the scientific literature, and this will too. However, I intend to go further and want to provide students with data sets from or based on the examples. If you’re going to learn how to analyze data from a randomized controlled trial, then why not practice with data resembling one of those trials? Which brings me to my main point:

**You can help me write this book.**

The primary purpose of this repository is to collect suggestions for studies with data you would want to learn how to analyze. I am particularly eager to include studies whose authors used open science practices, such as open data. Yet since these practices aren’t widespread in the social sciences, studies using older closed practices are also of interest.

## Some of the research designs I’d like to include:

* posttest-only control group designs
* pretest-posttest control group designs
* Solomon four-group designs
* factorial designs
* control-group designs with many measurement occasions
* quasi-experiment variants of all of the above
* multiple baseline designs
* AB designs
* ABAB designs
* BAB designs
* ABAC designs

## Some of the data types I’d like to explore fitting:

* Approximately-continuous data with the Gaussian and Student-t likelihoods
* Bounded count data with the binomial likelihood
* Unbounded counts with the Poisson and negative-binomial likelihoods
* Non-negative continuous data with the lognormal, exponential, gamma, or Weibull likelihoods
* Continuous data with clear lower and upper limits with the zero-one-inflated beta likelihood
* Nominal data with the softmax approach
* Ordinal data with the cumulative probit

Just like with real-world data, the examples will contend with complications like
* missing data
* multiple sources of nesting
* unequal variances
* zero inflation

## Content areas I’d love to get study suggestions from:

* behavior-therapy classics
* COVID-19
* HIV/AIDS trials
* large-scale replications
* multi-site trials
* PTSD trials
* Sex research
* Substance use trials

