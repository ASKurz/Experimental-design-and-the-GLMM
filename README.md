# Experimental design and the GLMM

A while back, I [announced on twitter](https://twitter.com/SolomonKurz/status/1464274116341641228) that I wanted to put together a book showing how to use strategies from the generalized linear mixed model (GLMM) to analyze experimental data. This is the public-facing home for that project.

Welcome.

I should unpack a couple terms. I’m using *GLMM* in an expansive way to include not only single- and multilevel models using familiar likelihoods like the Gaussian and binomial, but I’m also thinking in terms of distributional models where all parameters in the likelihood function can have a linear model and of distinctly non-linear model types including splines. By *experimental data*, I mean fully randomized true experiments (between persons, within them, and both) and also quasi-experimental variants, where some of that control is lost for various pragmatic reasons. I plan to aim this textbook at students and researchers within the social sciences. Given my background in clinical psychology, the content will bend in that direction. However, I am keen to bring in examples from other areas within psychology and perhaps other social sciences, as well.

Books of this kind often use examples from the scientific literature, and this will too. However, I intend to go further and want to provide students with data sets from or based on the examples. If you’re going to learn how to analyze data from a randomized controlled trial, then why not practice with data resembling one of those trials? Which brings me to my main point:

**You can help me write this book!**

The primary purpose of this repository is to collect suggestions for studies with data you would want to learn how to analyze. I am particularly eager to include studies whose authors used open science practices, such as open data. Yet since these practices aren’t widespread in the social sciences, studies using older closed practices are also of interest.

## Some of the research designs I’d like to include:

* posttest-only control group designs
* pretest-posttest control group designs
* Solomon four-group designs (possibly the three-group variant, too)
* factorial designs
* crossover designs
* control-group designs with many measurement occasions
* post-test only designs with a large number of conditions
* quasi-experiment variants of all of the above
* single-group posttest-only designs
* single-group pretest-posttest designs
* multiple baseline designs (primarily across persons, but also across settings or behaviors)
* AB designs
* ABA designs
* BAB designs
* ABAB designs (ABABAB, ...)
* multiple-treatment designs (ABAC, ABCBCA, ...)
* changing-criterion designs

A collaborator and I are in the early-stages of a study that will use an encouragement design and will leverage the instrumental variable approach in the analytic strategy. Kristoffer Magnusson has [written interesting things](https://rpsychologist.com/therapists-effects-longitudinal) on the perils of ignoring therapist effects. Which is all to say, I’m open to including studies that range from methodically simple to intimidating.

## Some of the data types I’d like to explore fitting:

* approximately-continuous data with the Gaussian and Student-t likelihoods
* bounded count data with the binomial likelihood
* unbounded counts with the Poisson and negative-binomial likelihoods
* non-negative continuous data with the lognormal, exponential, gamma, or Weibull likelihoods
* continuous data with clear lower and upper limits with the zero-one-inflated beta likelihood
* nominal data with the softmax approach
* ordinal data with the cumulative probit

Just like with real-world data, the examples will contend with complications like
* missing data
* multiple sources of nesting
* unequal variances (by group, persons, or both)
* zero inflation

## Content areas I’d love to get study suggestions from:

* behavior-therapy classics
* cognitive/developmental interventions
* computerized reaction-time tasks (e.g., Stroop)
* COVID-19
* data vis
* education interventions (e.g., DARE)
* HIV/AIDS trials
* large-scale replications
* marriage and other relationships
* meta-science
* multi-site trials
* non-human animal research (dogs, rats, chicks...)
* PTSD trials
* sex research
* substance use trials

Other interesting topic areas are okay, too. For any of these topic areas, the studies could gold-star examples of their method, or have substantial flaws. They could have large effect sizes, or they could show little difference among conditions.

## Other things the book will contain:

* data sets saved as external files for all analyses in the book
* code for all analyses and figures in the book
* discussions on topics like 
  - effect sizes,
  - simulation-based power analyses, 
  - common pitfalls like Simpson’s paradox, the ecological fallacy, and the table 1 fallacy
* discussions on priors, such as 
  - weakly-informative priors, 
  - weakly-regularizing reference priors, 
  - priors based on previous research, 
  - prior-predictive simulations

I'm leaning towards including light discussions of the potential-outcomes framework. I'm also thinking about using DAGs to (we'll see).

## Things this book will avoid or minimize:

* Bayes factors
* finite mixture models (e.g., growth mixture models)
  - I will, however, include mixtures in the form of zero-inflated likelihoods and so on.
* introductory programming
* introductory statistics
* latent variables
* NHST
* the type-I/type-II error framework

I’m not necessarily saying these things are bad. They’re just not going to be the focus of this book.

