Mason et al (2014)
================
A Solomon Kurz
2022-11-07

Load our primary packages.

``` r
library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)
```

## Communication at recess

School-aged children diagnosed with autism spectrum disorder often miss
opportunities to play with other children during recess. Mason et al
(2014; <https://doi.org/10.1016/j.rasd.2013.12.014>) used a nice
multiple baseline AB design to evaluate a social skills instructional
program to increase the frequency three children diagnosed with ASD
displayed communicative acts during recess.

``` r
# load the data
load(file = "/Users/solomonkurz/Dropbox/Experimental-design-and-the-GLMM/sketches/data/mason2014.rda")

mason2014 <- mason2014 %>% 
  mutate(session01 = session0 / max(session0))

# what is this?
glimpse(mason2014)
```

    ## Rows: 63
    ## Columns: 6
    ## $ id        <fct> Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, Sam, …
    ## $ session   <dbl> 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 19, 21, 22, 25, 32, 36, 42, 1, 2, 3, 4, 5, 6, 7, …
    ## $ session0  <dbl> 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 18, 20, 21, 24, 31, 35, 41, 0, 1, 2, 3, 4, 5, 6, 8…
    ## $ phase     <fct> A, A, A, A, A, A, B, B, B, B, B, B, B, B, B, B, B, B, B, A, A, A, A, A, A, A, A, A, B, B, …
    ## $ count     <dbl> 4, 13, 4, 6, 6, 6, 38, 20, 34, 41, 17, 16, 38, 21, 28, 35, 56, 21, 40, 5, 8, 2, 5, 1, 4, 1…
    ## $ session01 <dbl> 0.04347826, 0.06521739, 0.08695652, 0.10869565, 0.13043478, 0.15217391, 0.17391304, 0.1956…

The participant pseudonyms are listed in the `id` column. The session
number is coded $1, \dots, N$ in the `session` column and coded the same
minus $1$ in the `session0` column. The two phases `A` and `B` are saved
in the `phase` column. The number of filled pauses is in the `count`
column and the number of times the children displayed communicative acts
during recess is recorded in the `count` column.

### EDA.

We can look at the data with a version of Figure 2 from the original
article.

``` r
# adjust the global plotting theme
theme_set(
  theme_gray(base_size = 13) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = 1))
)

lines <- mason2014 %>% 
  distinct(id) %>% 
  mutate(xintercept = c(8.5, 13, 23.5))

mason2014 %>% 
  ggplot(aes(x = session, y = count, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_point(size = 2) +
  geom_line() +
  scale_color_manual("Phase", values = c("red3", "blue3")) +
  scale_y_continuous("Number of communicative acts", limits = c(0, 60)) +
  facet_wrap(~ id, ncol = 1)
```

<img src="Mason-et-al--2014-_files/figure-gfm/unnamed-chunk-4-1.png" width="480" />

We could also use the `geom_smooth()` method to get a sense of the
linear trends across students and conditions.

``` r
mason2014 %>% 
  ggplot(aes(x = session, y = count, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, size = 1/2) +
  scale_color_manual("Phase", values = c("red3", "blue3")) +
  scale_y_continuous("Number of communicative acts", limits = c(0, 60)) +
  facet_wrap(~ id, ncol = 1)
```

<img src="Mason-et-al--2014-_files/figure-gfm/unnamed-chunk-5-1.png" width="480" />

We explicitly removed the standard error ribbons from the plot with
`se = FALSE` because, IMO, they would be invalid. They’re based on OLS
estimation, which isn’t particularly appropriate for data of this kind.
However, the lines are okay for quick-and-dirty exploratory plots.

## Models: First round

The first model will be the simple conditional-means model with the
Poisson likelihood. If we let the outcome variable `count` vary across
$i$ kids and $j$ time points, we can express the model as

$$
\begin{align*}
\text{count}_{ij} & \sim \operatorname{Poisson}(\lambda_{ij}) \\
\log(\lambda_{ij}) & = \beta_0 + \beta_1 \text{phase}_{ij} + u_{0i} + u_{1i} \\
\begin{bmatrix} u_{0i} \\ u_{1i} \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \begin{bmatrix} 0 \\ 0 \end{bmatrix}, \mathbf \Sigma 
    \right) \\
\mathbf \Sigma & = \mathbf{SRS} \\
\mathbf S & =  \begin{bmatrix} \sigma_0 & \\ 0 & \sigma_1 \end{bmatrix} \\
\mathbf R & =  \begin{bmatrix} 1 & \\ \rho & 1 \end{bmatrix},
\end{align*}
$$

where the left side of the equation in the second line,
$\log(\lambda_{ij})$, clarifies we are using the conventional log link
to ensure the model only predicts positive counts. The $\beta_0$
parameter is the population average number of counts at baseline A phase
and $\beta_1$ is population average change in counts during the
post-intervention B phase. The kid-specific deviations around the
population means are depicted by the $u_{0i}$ and $u_{1i}$ terms, which
are modeled as bivariate normal with means of zero and the
variance/covariance matrix $\mathbf \Sigma$. Following the **brms**
convention, we decompose $\mathbf \Sigma$ into the matrix of standard
deviations $\mathbf S$ and the correlation matrix $\mathbf R$ which,
though somewhat confusing at first, will make it easier for the software
to fit the model and ultimately easier for us to assign the priors.

The next issue is how we might contend with the priors. Since this is a
small data set with only three kids, I recommend we use some domain
knowledge and a little courage of heart to set moderately-informative
and regularizing priors. Let’s start with the population average during
the A phase, $\beta_0$. We know from the outset that this intervention
is designed to increase counts in a population of kids for whom the
number of counts is typically low. If you peruse the behavior-analytic
literature a bit, you’ll also notice that in cases like this, counts of
this kind tend to be near zero, but not necessarily all at zero. As a
start, we could propose the average count might be 5, which we will
express as $\log(5)$ because we are modeling on the log scale. It’s
typical to use Gaussian priors for parameters of this kind. If we were
to use $\mathcal N(\log(5), 0.89392)$, that would give us a 99% prior
probability the mean was between 0.5 and 50, and a 90% probability it’s
between 1.15 and 21.75, which seems like a good place to start.

With a little help from **ggdist**, here’s what the
$\mathcal N(\log(5), 0.89392)$ distribution looks like on the
exponentiated count scale.

``` r
# log(5) is about 1.609438
prior(lognormal(1.609438, 0.89392)) %>% 
  parse_dist() %>%

  ggplot(aes(y = prior, dist = .dist, args = .args)) +
  stat_dist_halfeye(.width = c(.9, .99)) +
  scale_x_continuous("count", breaks = c(0, 5, 21.75, 50),
                     labels = c("0", "5", 21.75, "50")) +
  scale_y_discrete(NULL, breaks = NULL, expand = expansion(add = 0.06)) +
  coord_cartesian(xlim = c(0, 50))
```

<img src="Mason-et-al--2014-_files/figure-gfm/unnamed-chunk-6-1.png" width="432" />

The median is marked off by the dot and the 99% and 90% intervals are
depicted by the thinner an thicker horizontal lines at the base. In case
it wasn’t clear, a normal distribution on the log scale is a lognormal
distribution on the exponentiated count scale.

Next we consider $\beta_1$, the change in average counts for the B
phase. We know that behavior analysts often go for effect sizes that are
large enough to see plainly with your eyes. A difference in 1 on the log
scale is pretty good. For example, assuming a population mean of
$\log(5)$, an estimate of $\beta_1 = 1$ would mean an average increase
of 8.6 counts to a new average of 13.6 counts, which seems generous.

``` r
b0 <- log(5)
b1 <- 1

# average increase
exp(b0 + b1) - exp(b0)
```

    ## [1] 8.591409

``` r
# phase B average
exp(b0 + b1)
```

    ## [1] 13.59141

Setting $\beta_1 \sim \mathcal N(0, 1)$ would rule out outrageously
large changes, but easily allow for moderately large like we might
expect. If you wanted to be more conservative, I could easily see a
justification for something as small as
$\beta_1 \sim \mathcal N(0, 0.5)$.

Our next issue has to do with the level-2 variance parameters $\sigma_0$
and $\sigma_1$. When we have many more upper-level groups (i.e., kids),
a nice default is the exponential distribution with a mean of 1, which
puts a lot of prior mass between zero and 1, but gently tapers off to
the right. With only 3 kids in the data, we might want to use a little
bit of theory to motivate a stronger prior. We know that from the
outset, behavior analysts believe that people are not exchangeable in
the way hydrogen atoms are. Individual differences are important and
behavior analysts prefer models and analytic strategies which highlight
those differences. For our model, this suggests we should use priors
that push the variance parameters away from zero. However, we also know
that given this is a study of children from a well-defined psychiatric
subpopulation, we probably shouldn’t expect to see massive differences
among the kids. So we want a prior that can both nudge the variance
parameters away from zero, but still keep the model from entertaining
very large values. The gamma distribution can fit this criteria. In [his
text](https://sites.google.com/site/doingbayesiandataanalysis/),
Kruschke provided a few convenience functions which allow researchers to
derive the $\alpha$ and $\beta$ parameters that correspond to gamma
distributions with pre-defined means and standard deviations, or modes
and standard deviations. Here we’ll use the function that uses the mode
and standard deviation and request a gamma with a mode of 0.5 and a
standard deviation of 0.25.

``` r
gamma_a_b_from_omega_sigma <- function(mode, sd) {
  
  if (mode <= 0) stop("mode must be > 0")
  if (sd   <= 0) stop("sd must be > 0")
  rate <- (mode + sqrt(mode^2 + 4 * sd^2)) / (2 * sd^2)
  shape <- 1 + mode * rate
  
  return(list(shape = shape, rate = rate))
  
}

gamma_a_b_from_omega_sigma(mode = 0.5, sd = 0.25)
```

    ## $shape
    ## [1] 5.828427
    ## 
    ## $rate
    ## [1] 9.656854

We can use **ggdist** functions to plot the this gamma prior and compare
it with the familiar exponential prior for which $\lambda = 1$.

``` r
c(prior(exp(1)),
  prior(gamma(5.828427, 9.656854))) %>% 
  parse_dist() %>%
  mutate(prior = fct_rev(prior)) %>% 
  
  ggplot(aes(y = prior, dist = .dist, args = .args)) +
  geom_vline(xintercept = 0.5, color = "white") +
  stat_dist_halfeye(.width = c(.9, .99)) +
  scale_y_discrete(expand = expansion(add = 0.06)) +
  labs(x = "prior",
       y = NULL) +
  coord_cartesian(xlim = c(0, 5))
```

<img src="Mason-et-al--2014-_files/figure-gfm/unnamed-chunk-9-1.png" width="432" />

This gamma prior would put 99% of the prior mas between 0.15 and 1.44
and 90% of the mass between 0.26 and 1.06.

``` r
g <- gamma_a_b_from_omega_sigma(mode = 0.5, sd = 0.25)

# what is the 99% interval for this gamma?
qgamma(p = c(.005, .995), shape = g$shape, rate = g$rate)
```

    ## [1] 0.1506647 1.4379874

``` r
# what is the 90% interval for this gamma?
qgamma(p = c(.050, .950), shape = g$shape, rate = g$rate)
```

    ## [1] 0.2589262 1.0647510

To my eye, this prior would allow for kid-level differences to range
anywhere form small to moderately large while also fulfilling our
desires to keep the posterior off of the zero boundary and discourage
outrageously large differences. Finally, we’ll need to set a prior for
the correlation $\rho$ between the two level-2 $\textit{SD}$ parameters.
Here we’ll use an $\operatorname{LKJ}(4)$, which will regularize towards
zero. With those priors in place, we can specify the full model as

$$
\begin{align*}
\text{count}_{ij} & \sim \operatorname{Poisson}(\lambda_{ij}) \\
\log(\lambda_{ij}) & = \beta_0 + \beta_1 \text{phase}_{ij} + u_{0i} + u_{1i} \\
\begin{bmatrix} u_{0i} \\ u_{1i} \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \begin{bmatrix} 0 \\ 0 \end{bmatrix}, \mathbf{SRS} 
    \right) \\
\mathbf S & =  \begin{bmatrix} \sigma_0 & \\ 0 & \sigma_1 \end{bmatrix} \\
\mathbf R & =  \begin{bmatrix} 1 & \\ \rho & 1 \end{bmatrix} \\
\beta_0 & \sim \operatorname{Normal}(\log(5), 0.89392) \\
\beta_1  & \sim \operatorname{Normal}(0, 1) \\
\sigma_0, \sigma_1 & \sim \operatorname{Gamma}(5.828427, 9.656854) \\
\rho    & \sim \operatorname{LKJ}(4).
\end{align*}
$$

Here’s how to fit the model with `brm()`.

``` r
# conditional means model
fit1 <- brm(
  data = mason2014,
  family = poisson,
  count ~ 0 + Intercept + phase + (1 + phase | id),
  prior = c(prior(normal(log(5), 0.89392), class = b, coef = Intercept),
            prior(normal(0, 1), class = b),
            prior(gamma(5.828427, 9.656854), class = sd),
            prior(lkj(4), class = cor)),
  cores = 4,
  seed = 1,
  control = list(adapt_delta = .95),
  file = "fits/fit1.mason2014"
)
```

You’ll note that even with our carefully-chosen priors, we still had to
adjust the `adapt_delta` settings. In my experience, this is not
uncommon with so few level-2 groups.

Before we start interpreting the model, let’s fit the next two
alternative models. The first is the unconditional growth model

$$
\begin{align*}
\text{count}_{ij} & \sim \operatorname{Poisson}(\lambda_{ij}) \\
\log(\lambda_{ij}) & = \beta_0 + \beta_1 \text{session01}_{ij} + u_{0i} + u_{1i} \\
\begin{bmatrix} u_{0i} \\ u_{1i} \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \begin{bmatrix} 0 \\ 0 \end{bmatrix}, \mathbf{SRS} 
    \right) \\
\mathbf S & =  \begin{bmatrix} \sigma_0 & \\ 0 & \sigma_1 \end{bmatrix} \\
\mathbf R & =  \begin{bmatrix} 1 & \\ \rho & 1 \end{bmatrix} \\
\beta_0 & \sim \operatorname{Normal}(\log(5), 0.89392) \\
\beta_1  & \sim \operatorname{Normal}(0, 1) \\
\sigma_0, \sigma_1 & \sim \operatorname{Gamma}(5.828427, 9.656854) \\
\rho    & \sim \operatorname{LKJ}(4),
\end{align*}
$$

which has the same basic structure as the conditional means model we
just fit. The only difference is we have switched out the `phase`
variable for a continuous measure of time, `session01`. This variable
has been scales so that the first session is 0, the last session (47)
has been scaled to 1, all the intermediary sessions are the
corresponding factions. In principle, you could use the `session`
variable, instead, but `session01` will make it easier for the HMC
algorithm to fit the model and it will make it easier to set the prior
on $\beta_1$, which is now interpreted as the average change in counts
from the first session to the last. In this case, we will just continue
using the regularizing $\mathcal N(0, 1)$.

Our third model is the conditional growth model

$$
\begin{align*}
\text{count}_{ij} & \sim \operatorname{Poisson}(\lambda_{ij}) \\
\log(\lambda_{ij}) & = \beta_0 + \beta_1 \text{session01}_{ij} + \beta_2 \text{phase}_{ij} + \beta_3 \text{session01}_{ij}\text{phase}_{ij} \\
                   & \;\;\; + u_{0i} + u_{1i} + u_{2i} + u_{3i} \\
\begin{bmatrix} u_{0i} \\ u_{1i} \\ u_{2i} \\ u_{3i} \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \mathbf 0, \mathbf{SRS} 
    \right) \\
\mathbf S & = 
  \begin{bmatrix} 
    \sigma_0 \\ 
    0 & \sigma_1 \\ 
    0 & 0 & \sigma_2 \\ 
    0 & 0 & 0 & \sigma_3 
  \end{bmatrix} \\
\mathbf R & = 
  \begin{bmatrix} 
    1 & \\ 
    \rho_{21} & 1 \\ 
    \rho_{31} & \rho_{32} & 1 \\ 
    \rho_{41} & \rho_{42} & \rho_{43} & 1 
  \end{bmatrix} \\
\beta_0 & \sim \operatorname{Normal}(\log(5), 0.89392) \\
\beta_1, \dots, \beta_3  & \sim \operatorname{Normal}(0, 1) \\
\sigma_0, \dots,  \sigma_3 & \sim \operatorname{Gamma}(5.828427, 9.656854) \\
\rho    & \sim \operatorname{LKJ}(2),
\end{align*}
$$

where $\beta_0$ is the population average count at the first session
within the A `phase`; $\beta_1$ is the population average change by the
final session (47), presuming the A `phase`; $\beta_2$ is the population
average change in counts for the B `phase`, from the perspective of the
first trial; and $\beta_3$ is the population average interaction for
`phase` and `session01`, which allows the growth trajectories to differ
by `phase`. As we allow all four parameters to vary by kid, we end up
with four $u_i$ parameters and a corresponding $4 \times 4$ matrix for
both $\mathbf S$ and $\mathbf R$. Given the size of the $\mathbf R$
matrix, we have reduced the prior to $\operatorname{LKJ}(2)$.

Here’s how to fit both models.

``` r
# unconditional growth model
fit2 <- brm(
  data = mason2014,
  family = poisson,
  count ~ 0 + Intercept + session01 + (1 + session01 | id),
  prior = c(prior(normal(log(5), 0.89392), class = b, coef = Intercept),
            prior(normal(0, 1), class = b),
            prior(gamma(5.828427, 9.656854), class = sd),
            prior(lkj(4), class = cor)),
  cores = 4,
  seed = 1,
  file = "fits/fit2.mason2014"
)

# conditional growth model
fit3 <- brm(
  data = mason2014,
  family = poisson,
  count ~ 0 + Intercept + session01 + phase + session01:phase + (1 + session01 + phase + session01:phase | id),
  prior = c(prior(normal(log(5), 0.805), class = b, coef = Intercept),
            prior(normal(0, 1), class = b),
            prior(gamma(5.828427, 9.656854), class = sd),
            prior(lkj(2), class = cor)),
  cores = 4,
  seed = 1,
  file = "fits/fit3.mason2014"
)
```

### Summarize and compare.

Behold the model summaries.

``` r
summary(fit1)
```

    ##  Family: poisson 
    ##   Links: mu = log 
    ## Formula: count ~ 0 + Intercept + phase + (1 + phase | id) 
    ##    Data: mason2014 (Number of observations: 63) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~id (Number of levels: 3) 
    ##                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)             0.50      0.19     0.20     0.95 1.00     2091     2293
    ## sd(phaseB)                0.47      0.21     0.16     0.97 1.00     2305     2584
    ## cor(Intercept,phaseB)    -0.08      0.35    -0.72     0.59 1.00     2203     2381
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     1.57      0.31     0.95     2.21 1.00     1306     1675
    ## phaseB        1.63      0.30     1.00     2.16 1.00     2011     2232
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
summary(fit2)
```

    ##  Family: poisson 
    ##   Links: mu = log 
    ## Formula: count ~ 0 + Intercept + session01 + (1 + session01 | id) 
    ##    Data: mason2014 (Number of observations: 63) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~id (Number of levels: 3) 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                0.62      0.20     0.32     1.09 1.00     2028     2421
    ## sd(session01)                0.52      0.21     0.20     1.02 1.00     1875     2569
    ## cor(Intercept,session01)    -0.15      0.32    -0.73     0.47 1.00     2701     2609
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     2.03      0.34     1.32     2.69 1.00     1248     1800
    ## session01     1.61      0.35     0.82     2.23 1.00     1605     1501
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
summary(fit3)
```

    ##  Family: poisson 
    ##   Links: mu = log 
    ## Formula: count ~ 0 + Intercept + session01 + phase + session01:phase + (1 + session01 + phase + session01:phase | id) 
    ##    Data: mason2014 (Number of observations: 63) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~id (Number of levels: 3) 
    ##                                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                       0.50      0.20     0.20     0.96 1.00     3861     3363
    ## sd(session01)                       0.52      0.22     0.18     1.03 1.00     4938     2770
    ## sd(phaseB)                          0.48      0.20     0.18     0.97 1.00     4001     3083
    ## sd(session01:phaseB)                0.53      0.22     0.19     1.03 1.00     4836     2578
    ## cor(Intercept,session01)           -0.00      0.39    -0.72     0.74 1.00     5229     2186
    ## cor(Intercept,phaseB)              -0.05      0.40    -0.76     0.70 1.00     4295     3053
    ## cor(session01,phaseB)              -0.04      0.39    -0.75     0.69 1.00     3659     2970
    ## cor(Intercept,session01:phaseB)    -0.03      0.38    -0.72     0.69 1.00     5838     2883
    ## cor(session01,session01:phaseB)    -0.12      0.38    -0.78     0.64 1.00     3998     3115
    ## cor(phaseB,session01:phaseB)       -0.04      0.39    -0.74     0.72 1.00     3674     2634
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept            1.61      0.30     1.02     2.18 1.00     1747     2419
    ## session01           -0.44      0.61    -1.65     0.75 1.00     3442     3257
    ## phaseB               1.41      0.31     0.75     2.00 1.00     2295     2523
    ## session01:phaseB     0.75      0.63    -0.48     2.00 1.00     3377     3291
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

As you look through the parameter summaries, you’ll notice all of the
posteriors are will within the expected ranges we set with our priors.
Though we’ll come back to it in more detail, later, notice how the
$\sigma$ posteriors compare with their priors.

We might compare how well the models make sense of the data from a
cross-validation perspective with the LOO-CV estimates.

``` r
fit1 <- add_criterion(fit1, criterion = "loo")
fit2 <- add_criterion(fit2, criterion = "loo")
fit3 <- add_criterion(fit3, criterion = "loo")

loo_compare(fit1, fit2, fit3, criterion = "loo") %>% 
  print(simplify = FALSE)
```

    ##      elpd_diff se_diff elpd_loo se_elpd_loo p_loo  se_p_loo looic  se_looic
    ## fit1    0.0       0.0  -234.4     20.4        15.3    3.6    468.7   40.9  
    ## fit3   -1.0       4.0  -235.4     20.0        20.1    4.1    470.7   40.0  
    ## fit2 -108.9      26.0  -343.2     28.4        31.2    5.2    686.5   56.7

Numerically, there is very little difference between the conditional
means model and the conditional growth models, but both are clearly
superior to the unconditional growth model, which might be seen as the
null model in this case. Thus we appear to be on good footing to presume
statistical evidence for the effectiveness of the intervention.

### Compare the models with plots.

We might want to compare the three models in a series of plots. The
first step is to define a data set containing the `id`, `phase` and
`session01` values we want to compute the model predictions from. In
this case, I found it conceptually easier to define three sub-data sets
by the levels of `id` and then combine them at the end with the `nd`
data frame.

``` r
# Sam
# length(seq(from = 3, to = 8, by = 0.5))
# length(seq(from = 9, to = 42, by = 0.5))
nd_sam <- tibble(
  id      = "Sam",
  phase   = rep(LETTERS[1:2], times = c(11, 67)),
  session = c(seq(from = 3, to = 8,  by = 0.5),
              seq(from = 9, to = 42, by = 0.5))
)

# Ed
# length(seq(from = 1, to = 11, by = 0.5))
# length(seq(from = 15, to = 45, by = 0.5))
nd_ed <- tibble(
  id      = "Ed",
  phase   = rep(LETTERS[1:2], times = c(21, 61)),
  session = c(seq(from = 1,  to = 11, by = 0.5),
              seq(from = 15, to = 45, by = 0.5))
)

# Brian
# length(seq(from = 2, to = 22, by = 0.5))
# length(seq(from = 25, to = 47, by = 0.5))
nd_brian <- tibble(
  id      = "Brian",
  phase   = rep(LETTERS[1:2], times = c(41, 45)),
  session = c(seq(from = 2,  to = 22, by = 0.5),
              seq(from = 25, to = 47, by = 0.5))
)

# combine
nd <- bind_rows(
  nd_sam, nd_ed, nd_brian
) %>% 
  mutate(session01 = (session - 1) / max(session - 1))

# take a look
glimpse(nd)
```

    ## Rows: 246
    ## Columns: 4
    ## $ id        <chr> "Sam", "Sam", "Sam", "Sam", "Sam", "Sam", "Sam", "Sam", "Sam", "Sam", "Sam", "Sam", "Sam",…
    ## $ phase     <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", …
    ## $ session   <dbl> 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 9.0, 9.5, 10.0, 10.5, 11.0, 11.5, 1…
    ## $ session01 <dbl> 0.04347826, 0.05434783, 0.06521739, 0.07608696, 0.08695652, 0.09782609, 0.10869565, 0.1195…

Now use the `nd` data to compute the fitted lines for each of the three
models with `fitted()`, saving the results as `f1` through `f3`.

``` r
f1 <- fitted(fit1, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)

f2 <- fitted(fit2, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)

f3 <- fitted(fit3, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)
```

We’re ready to make the subplot for each model separately and then
combine them into one large plot.

``` r
p1 <- f1 %>% 
  ggplot(aes(x = session, group = phase, fill = phase, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = mason2014,
             aes(y = count)) +
  scale_fill_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  scale_color_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(subtitle = "The conditional means model (fit1)",
       y = "communicative acts") +
  facet_wrap(~ id)

p2 <- f2 %>% 
  ggplot(aes(x = session, group = phase, fill = phase, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = mason2014,
             aes(y = count)) +
  scale_fill_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  scale_color_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(subtitle = "The unconditional growth model (fit2)",
       y = "communicative acts") +
  facet_wrap(~ id)

p3 <- f3 %>% 
  ggplot(aes(x = session, group = phase, fill = phase, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = mason2014,
             aes(y = count)) +
  scale_fill_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  scale_color_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(subtitle = "The conditional growth model (fit3)",
       y = "communicative acts") +
  facet_wrap(~ id)

# combine
(p1 / p2 / p3) & theme(plot.title.position = "plot")
```

<img src="Mason-et-al--2014-_files/figure-gfm/unnamed-chunk-15-1.png" width="768" />

The plot help clarify the similar LOO-CV results for `fit1` and `fit3`.
Within each phase of the intervention, there was relatively little
linear change across the three kids. The bulk of the change in the mean
count occurred in the transition between the phases of the intervention,
for all three kids. However, it was only the full model `fit3` that
allowed us to see this clearly.

## Models: Second round

The plots above and the earlier exploratory plots all show how the
behavioral counts varied around their central tendencies less in the A
phase than in the B phase. As its sole parameter $\lambda$ controls both
the mean and the variance, the Poisson distribution naturally accounts
for this difference in variation. However, yoking the mean and variance
is restrictive in that sometimes behavioral counts have more variation
than expected simply by their mean. In such cases, the negative-binomial
distribution is a good alternative. We can generalize the full
conditional Poisson growth model `fit3` to a conditional
negative-binomial growth model as

$$
\begin{align*}
\text{count}_{ij} & \sim \operatorname{Negative Binomial}(\lambda_{ij}, \phi) \\
\log(\lambda_{ij}) & = \beta_0 + \beta_1 \text{session01}_{ij} + \beta_2 \text{phase}_{ij} + \beta_3 \text{session01}_{ij}\text{phase}_{ij} \\
                   & \;\;\; + u_{0i} + u_{1i} + u_{2i} + u_{3i} \\
\begin{bmatrix} u_{0i} \\ u_{1i} \\ u_{2i} \\ u_{3i} \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \mathbf 0, \mathbf{SRS} 
    \right) \\
\mathbf S & = 
  \begin{bmatrix} 
    \sigma_0 & & & \\ 
    0 & \sigma_1 & &\\ 
    0 & 0 & \sigma_2 \\ 
    0 & 0 & 0 & \sigma_3 
  \end{bmatrix} \\
\mathbf R & = 
  \begin{bmatrix} 
    1 & \\ 
    \rho_{21} & 1 \\ 
    \rho_{31} & \rho_{32} & 1 \\ 
    \rho_{41} & \rho_{42} & \rho_{43} & 1 
  \end{bmatrix} \\
\beta_0 & \sim \operatorname{Normal}(\log(5), 0.89392) \\
\beta_1, \dots, \beta_3 & \sim \operatorname{Normal}(0, 1) \\
\sigma_0, \dots, \sigma_3 & \sim \operatorname{Gamma}(5.828427, 9.656854) \\
\rho & \sim \operatorname{LKJ}(2) \\
\phi & \sim \operatorname{Gamma}(0.01, 0.01),
\end{align*}
$$

where all parameters are the same, except for the dispersion precision
$\phi$, to which we have assigned the **brms** default prior
$\operatorname{Gamma}(0.01, 0.01)$. To get a sense of that prior, we
might plot.

``` r
prior(gamma(0.01, 0.01), class = sd)%>% 
  parse_dist() %>%

  ggplot(aes(y = 0, dist = .dist, args = .args)) +
  stat_dist_halfeye(.width = .99, size = 2, n = 2e3) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("gamma(0.01, 0.01)") +
  coord_cartesian(xlim = c(0, 100))
```

<img src="Mason-et-al--2014-_files/figure-gfm/unnamed-chunk-16-1.png" width="312" />

The $\operatorname{Gamma}(0.01, 0.01)$ prior places 99% of the prior
mass between about 0 and 56, but has a very long right tail, which can
allow for quite large values of $\phi$.

``` r
qgamma(p = c(.005, .995), shape = 0.01, rate = 0.01) %>% round(digits = 1)
```

    ## [1]  0.0 55.6

With this parameterization, lower values of $\phi$ (say in the
single-digit range), indicate relatively large deviations from the
Poisson distribution. As $\phi$ increases into the double-digit range,
those deviations decrease. As $\phi$ approaches the triple-digit range
and beyond, the negative-binomial distribution merges into the Poisson.
The **brms** default $\phi \sim \operatorname{Gamma}(0.01, 0.01)$ allows
the precision to take on values form any of those ranges, as needed, and
is a fine place to start.

This first negative-binomial model is limited in that it holds $\phi$
constant across our three kids. We can relax that assumption by fitting
a distributional negative-binomial model

$$
\begin{align*}
\text{count}_{ij} & \sim \operatorname{Negative Binomial}(\lambda_{ij}, \phi_i) \\
\log(\lambda_{ij}) & = \beta_0 + \beta_1 \text{session01}_{ij} + \beta_2 \text{phase}_{ij} + \beta_3 \text{session01}_{ij}\text{phase}_{ij} \\
                   & \;\;\; + u_{\beta_0i} + u_{\beta_1i} + u_{\beta_2i} + u_{\beta_3i} \\
\log(\phi_i) & = \eta_0 + u_{\eta_0 i} \\
\begin{bmatrix} u_{\beta_0i} \\ u_{\beta_1i} \\ u_{\beta_2i} \\ u_{\beta_3i} \\ u_{\eta_0i} \\ \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \mathbf 0, \mathbf{SRS} 
    \right) \\
\mathbf S & = 
  \begin{bmatrix} 
    \sigma_{\beta_0} \\ 
    0 & \sigma_{\beta_1} \\ 
    0 & 0 & \sigma_{\beta_2} \\ 
    0 & 0 & 0 & \sigma_{\beta_3} \\ 
    0 & 0 & 0 & 0 & \sigma_{\eta_0} 
  \end{bmatrix} \\
\mathbf R & = 
  \begin{bmatrix} 
    1 & \\ 
    \rho_{21} & 1 \\ 
    \rho_{31} & \rho_{32} & 1 \\ 
    \rho_{41} & \rho_{42} & \rho_{43} & 1 \\ 
    0 & 0 & 0 & 0 & 1 
  \end{bmatrix} \\
\beta_0 & \sim \operatorname{Normal}(\log(5), 0.89392) \\
\beta_1, \dots, \beta_3 & \sim \operatorname{Normal}(0, 1) \\
\eta_0 & \sim \operatorname{Student-t}(3, 0, 2.5) \\ 
\sigma_{\beta_0}, \dots, \sigma_{\eta_0} & \sim \operatorname{Gamma}(5.828427, 9.656854) \\
\rho & \sim \operatorname{LKJ}(2),
\end{align*}
$$

where $\log(\phi_i)$ now varies across kids via a linear model with a
grand mean $\eta_0$ and kid-specific deviations around the mean
$u_{\eta_0 i}$. This increases both $\mathbf S$ and $\mathbf R$ matrices
to a $5 \times 5$ structure and the $u_{\eta_0 i}$ deviations are
orthogonal to the other four deviation terms. We have used the **brms**
default for $\eta_0$, which will allow for a large range of $\phi$
values on the log scale. For simplicity, we extend the
$\operatorname{Gamma}(5.828427, 9.656854)$ prior to the new
$\sigma_{\eta_0}$ variance parameter, to match the others. This
specification indicates our uncertainty about where the central tendency
for $\phi$ might be, but where ever it ends up, it should not vary
drastically by kid.

Our final model extends the distributional approach further in that we
also allow $\log(\phi)$ to vary by `phase` within each kid.

$$
\begin{align*}
\text{count}_{ij} & \sim \operatorname{Negative Binomial}(\lambda_{ij}, \phi_i) \\
\log(\lambda_{ij}) & = \beta_0 + \beta_1 \text{session01}_{ij} + \beta_2 \text{phase}_{ij} + \beta_3 \text{session01}_{ij}\text{phase}_{ij} \\
                   & \;\;\; + u_{\beta_0i} + u_{\beta_1i} + u_{\beta_2i} + u_{\beta_3i} \\
\log(\phi_i) & = \eta_0 + \eta_1 \text{phase}_{ij} + u_{\eta_0 i} + u_{\eta_1 i} \\
\begin{bmatrix} u_{\beta_0i} \\ u_{\beta_1i} \\ u_{\beta_2i} \\ u_{\beta_3i} \\ u_{\eta_0i} \\ u_{\eta_1i} \\ \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \mathbf 0, \mathbf{SRS} 
    \right) \\
\mathbf S & = 
  \begin{bmatrix} 
    \sigma_{\beta_0} \\ 
    0 & \sigma_{\beta_1} \\ 
    0 & 0 & \sigma_{\beta_2} \\ 
    0 & 0 & 0 & \sigma_{\beta_3} \\ 
    0 & 0 & 0 & 0 & \sigma_{\eta_0}  \\ 
    0 & 0 & 0 & 0 & 0 & \sigma_{\eta_1} 
  \end{bmatrix} \\
\mathbf R & = 
  \begin{bmatrix} 
    1 & \\ 
    \rho_{21} & 1 \\ 
    \rho_{31} & \rho_{32} & 1 \\ 
    \rho_{41} & \rho_{42} & \rho_{43} & 1 \\ 
    0 & 0 & 0 & 0 & 1 \\ 
    0 & 0 & 0 & 0 & \rho_{65} & 1 
  \end{bmatrix} \\
\beta_0 & \sim \operatorname{Normal}(\log(5), 0.89392) \\
\beta_1, \dots, \beta_3 & \sim \operatorname{Normal}(0, 1) \\
\eta_0 & \sim \operatorname{Student-t}(3, 0, 2.5) \\ 
\eta_1 & \sim \operatorname{Normal}(0, 1) \\
\sigma_{\beta_0}, \dots, \sigma_{\eta_1} & \sim \operatorname{Gamma}(5.828427, 9.656854) \\
\rho & \sim \operatorname{LKJ}(2).
\end{align*}
$$

For simplicity, we assigned the weakly-regularizing $\mathcal N(0, 1)$
prior to $\eta_1$, much like with the $\beta_1$ through $\beta_3$
parameters. The new free parameter in the $\mathbf R$ matrix,
$\rho_{65}$, will allow the random intercepts and slopes for the
$\log(\phi_i)$ model to correlate with each other.

Here’s how to fit the three new models with `brm()`.

``` r
# NB conditional growth model
fit4 <- brm(
  data = mason2014,
  family = negbinomial,
  count ~ 0 + Intercept + session01 + phase + session01:phase + (1 + session01 + phase + session01:phase | id),
  prior = c(prior(normal(log(5), 0.89392), class = b, coef = Intercept),
            prior(normal(0, 1), class = b),
            prior(gamma(5.828427, 9.656854), class = sd),
            prior(gamma(0.01, 0.01), class = shape),
            prior(lkj(2), class = cor)),
  cores = 4,
  seed = 1,
  file = "fits/fit4.mason2014"
)

# random NB conditional growth model
fit5 <- brm(
  data = mason2014,
  family = negbinomial,
  bf(count ~ 0 + Intercept + session01 + phase + session01:phase + (1 + session01 + phase + session01:phase | id),
     shape ~ 1 + (1 | id)),
  prior = c(prior(normal(log(5), 0.89392), class = b, coef = Intercept),
            prior(student_t(3, 0, 2.5), class = Intercept, dpar = shape),
            prior(normal(0, 1), class = b),
            prior(gamma(5.828427, 9.656854), class = sd),
            prior(gamma(5.828427, 9.656854), class = sd, dpar = shape),
            prior(lkj(2), class = cor)),
  cores = 4,
  seed = 1,
  file = "fits/fit5.mason2014"
)

# conditional random NB conditional growth model
fit6 <- brm(
  data = mason2014,
  family = negbinomial,
  bf(count ~ 0 + Intercept + session01 + phase + session01:phase + (1 + session01 + phase + session01:phase | id),
     shape ~ 0 + Intercept + phase + (1 + phase | id)),
  prior = c(prior(normal(log(5), 0.89392), class = b, coef = Intercept),
            prior(student_t(3, 0, 2.5), class = b, coef = Intercept, dpar = shape),
            prior(normal(0, 1), class = b),
            prior(normal(0, 1), class = b, dpar = shape),
            prior(gamma(5.828427, 9.656854), class = sd),
            prior(gamma(5.828427, 9.656854), class = sd, dpar = shape),
            prior(lkj(2), class = cor)),
  cores = 4,
  seed = 1,
  file = "fits/fit6.mason2014"
)
```

### Summarize and compare: Round 2.

Behold the summaries for our three new models.

``` r
summary(fit4)
```

    ##  Family: negbinomial 
    ##   Links: mu = log; shape = identity 
    ## Formula: count ~ 0 + Intercept + session01 + phase + session01:phase + (1 + session01 + phase + session01:phase | id) 
    ##    Data: mason2014 (Number of observations: 63) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~id (Number of levels: 3) 
    ##                                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                       0.51      0.20     0.20     0.98 1.00     3456     2949
    ## sd(session01)                       0.53      0.22     0.20     1.04 1.00     4294     2651
    ## sd(phaseB)                          0.49      0.21     0.17     1.00 1.00     3618     2936
    ## sd(session01:phaseB)                0.53      0.22     0.20     1.03 1.00     5711     2906
    ## cor(Intercept,session01)            0.00      0.38    -0.72     0.71 1.00     5348     2903
    ## cor(Intercept,phaseB)              -0.05      0.40    -0.78     0.69 1.00     4707     2889
    ## cor(session01,phaseB)              -0.05      0.39    -0.75     0.69 1.00     3507     3244
    ## cor(Intercept,session01:phaseB)    -0.02      0.39    -0.74     0.73 1.00     4537     2616
    ## cor(session01,session01:phaseB)    -0.10      0.38    -0.78     0.66 1.00     3819     3098
    ## cor(phaseB,session01:phaseB)       -0.06      0.39    -0.75     0.68 1.00     3377     3048
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept            1.61      0.31     1.00     2.24 1.00     1769     2236
    ## session01           -0.31      0.67    -1.62     0.97 1.00     3824     3357
    ## phaseB               1.41      0.37     0.61     2.07 1.00     2495     2111
    ## session01:phaseB     0.64      0.68    -0.67     2.00 1.00     3348     2973
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## shape    10.04      3.27     5.17    17.69 1.00     5530     2859
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
summary(fit5)
```

    ##  Family: negbinomial 
    ##   Links: mu = log; shape = log 
    ## Formula: count ~ 0 + Intercept + session01 + phase + session01:phase + (1 + session01 + phase + session01:phase | id) 
    ##          shape ~ 1 + (1 | id)
    ##    Data: mason2014 (Number of observations: 63) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~id (Number of levels: 3) 
    ##                                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                       0.50      0.20     0.19     0.97 1.00     3043     2549
    ## sd(session01)                       0.54      0.22     0.20     1.06 1.00     4459     3061
    ## sd(phaseB)                          0.49      0.21     0.18     0.99 1.00     2916     2821
    ## sd(session01:phaseB)                0.53      0.22     0.19     1.04 1.00     4387     2769
    ## sd(shape_Intercept)                 0.66      0.25     0.26     1.24 1.00     3595     2543
    ## cor(Intercept,session01)           -0.01      0.38    -0.72     0.70 1.00     4848     2353
    ## cor(Intercept,phaseB)              -0.05      0.39    -0.75     0.70 1.00     3767     2508
    ## cor(session01,phaseB)              -0.05      0.38    -0.74     0.68 1.00     3597     2944
    ## cor(Intercept,session01:phaseB)    -0.02      0.38    -0.71     0.70 1.00     4969     2902
    ## cor(session01,session01:phaseB)    -0.11      0.38    -0.78     0.66 1.00     3805     3108
    ## cor(phaseB,session01:phaseB)       -0.06      0.38    -0.74     0.67 1.00     3487     3035
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## shape_Intercept      2.28      0.53     1.24     3.35 1.00     2312     2347
    ## Intercept            1.61      0.31     0.96     2.20 1.00     1985     2286
    ## session01           -0.29      0.66    -1.58     0.97 1.00     2878     2901
    ## phaseB               1.43      0.36     0.67     2.11 1.00     2340     2270
    ## session01:phaseB     0.58      0.67    -0.71     1.86 1.00     2932     2694
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
summary(fit6)
```

    ##  Family: negbinomial 
    ##   Links: mu = log; shape = log 
    ## Formula: count ~ 0 + Intercept + session01 + phase + session01:phase + (1 + session01 + phase + session01:phase | id) 
    ##          shape ~ 0 + Intercept + phase + (1 + phase | id)
    ##    Data: mason2014 (Number of observations: 63) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~id (Number of levels: 3) 
    ##                                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                         0.49      0.20     0.19     0.95 1.00     3288     2645
    ## sd(session01)                         0.54      0.22     0.20     1.03 1.00     4253     1973
    ## sd(phaseB)                            0.50      0.21     0.19     1.01 1.00     2909     2703
    ## sd(session01:phaseB)                  0.53      0.22     0.20     1.03 1.00     4321     2731
    ## sd(shape_Intercept)                   0.60      0.24     0.22     1.14 1.00     3727     2617
    ## sd(shape_phaseB)                      0.64      0.26     0.24     1.22 1.00     3551     2484
    ## cor(Intercept,session01)             -0.00      0.38    -0.72     0.70 1.00     4995     2655
    ## cor(Intercept,phaseB)                -0.06      0.39    -0.77     0.67 1.00     3936     2677
    ## cor(session01,phaseB)                -0.05      0.38    -0.74     0.69 1.00     3036     2928
    ## cor(Intercept,session01:phaseB)      -0.03      0.38    -0.71     0.70 1.00     4363     2942
    ## cor(session01,session01:phaseB)      -0.10      0.39    -0.77     0.66 1.00     3290     2906
    ## cor(phaseB,session01:phaseB)         -0.06      0.39    -0.76     0.69 1.00     3286     3038
    ## cor(shape_Intercept,shape_phaseB)     0.06      0.44    -0.77     0.83 1.00     3009     2762
    ## 
    ## Population-Level Effects: 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept            1.60      0.30     0.98     2.19 1.00     1770     2363
    ## session01           -0.29      0.66    -1.61     1.01 1.00     3083     2707
    ## phaseB               1.42      0.35     0.69     2.09 1.00     2371     2234
    ## session01:phaseB     0.58      0.68    -0.75     1.89 1.00     2662     2544
    ## shape_Intercept      2.08      0.80     0.69     3.81 1.00     2769     2709
    ## shape_phaseB         0.28      0.70    -1.15     1.56 1.00     3278     2925
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

You’ll notice that for all of them, the posteriors for $\phi$ tended
toward the low to moderately-low end of the scale.

We might compute the LOO-CV estimates and compare these models with the
simpler Poisson `fit3`.

``` r
fit4 <- add_criterion(fit4, criterion = "loo")
fit5 <- add_criterion(fit5, criterion = "loo")
fit6 <- add_criterion(fit6, criterion = "loo")

loo_compare(fit3, fit4, fit5, fit6,
            criterion = "loo") %>% 
  print(simplify = FALSE)
```

    ##      elpd_diff se_diff elpd_loo se_elpd_loo p_loo  se_p_loo looic  se_looic
    ## fit6    0.0       0.0  -206.4      7.9         9.4    1.6    412.8   15.7  
    ## fit5   -0.1       0.5  -206.5      7.9         8.8    1.5    413.0   15.9  
    ## fit4   -1.8       1.5  -208.2      8.4         8.3    1.6    416.4   16.8  
    ## fit3  -28.9      13.7  -235.4     20.0        20.1    4.1    470.7   40.0

Although all of our negative binomial models show similar LOO-IC values,
all three are clearly better fits for the data than the Poisson model
`fit3`.

### Compare the models with plots: Round 2.

This time we’ll compute both the trajectories for the conditional means
with `fitted()` and the posterior predictive distributions with
`predict()`.

``` r
f4 <- fitted(fit4, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)

f5 <- fitted(fit5, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)

f6 <- fitted(fit6, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)

set.seed(1)
p3 <- predict(fit3, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)

set.seed(1)
p4 <- predict(fit4, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)

set.seed(1)
p5 <- predict(fit5, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)

set.seed(1)
p6 <- predict(fit6, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd)
```

Now plot.

``` r
plot1 <- f3 %>% 
  ggplot(aes(x = session, group = phase, fill = phase, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_ribbon(data = p3,
              aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = mason2014,
             aes(y = count)) +
  scale_fill_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  scale_color_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(subtitle = "Poisson (fit3)",
       y = "communicative\nacts") +
  facet_wrap(~ id)

plot2 <- f4 %>% 
  ggplot(aes(x = session, group = phase, fill = phase, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_ribbon(data = p4,
              aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = mason2014,
             aes(y = count)) +
  scale_fill_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  scale_color_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(subtitle = "Simple negative-binomial (fit4)",
       y = "communicative\nacts") +
  facet_wrap(~ id)

plot3 <- f5 %>% 
  ggplot(aes(x = session, group = phase, fill = phase, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_ribbon(data = p5,
              aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = mason2014,
             aes(y = count)) +
  scale_fill_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  scale_color_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(subtitle = "Simple distributional negative-binomial (fit5)",
       y = "communicative\nacts") +
  facet_wrap(~ id)

plot4 <- f6 %>% 
  ggplot(aes(x = session, group = phase, fill = phase, color = phase)) +
  geom_vline(data = lines,
             aes(xintercept = xintercept),
             color = "white") +
  geom_ribbon(data = p6,
              aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4, size = 0) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = mason2014,
             aes(y = count)) +
  scale_fill_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  scale_color_manual("Phase", values = c("red3", "blue3"), breaks = NULL) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(subtitle = "Conditional distributional negative-binomial (fit6)",
       y = "communicative\nacts") +
  facet_wrap(~ id)

# combine
(plot1 / plot2 / plot3 / plot4) & theme(plot.title.position = "plot")
```

<img src="Mason-et-al--2014-_files/figure-gfm/unnamed-chunk-21-1.png" width="768" />

The jagged outer bands in each plot depict the posterior-predictive
distributions. In good-fitting models, those bands should contain about
95% of the data. The bands from the Poisson model (top row) are a little
too narrow and more data points are outside of their bounds than
expected. The wider bands in each of the negative-binomial models do a
much better job capturing the data. Although the distributional models
`fit5` and `fit6` are interesting from a theoretical perspective, the
simpler conventional negative-binomial model `fit4` does a pretty good
job, too.

### Plot $\sigma$ posteriors agains the priors.

We spent a lot of time earlier in the section reasoning about how we
might set the priors for the $\sigma$ parameters. We can plot the
posteriors for those parameters against their prior distributions to get
a sense of how much the data updated the priors. Here we’ll practice
with the largest model, `fit6`.

``` r
as_draws_df(fit6) %>% 
  select(starts_with("sd_")) %>% 
  rename(`sigma[beta[0]]` = sd_id__Intercept,
         `sigma[beta[1]]` = sd_id__session01,
         `sigma[beta[2]]` = sd_id__phaseB,
         `sigma[beta[3]]` = `sd_id__session01:phaseB`,
         `sigma[eta[0]]` = sd_id__shape_Intercept,
         `sigma[eta[1]]` = sd_id__shape_phaseB) %>% 
  pivot_longer(everything()) %>% 
  
  ggplot(aes(x = value, y = ..density..)) +
  geom_histogram(boundary = 0, binwidth = 0.05) +
  geom_line(data = tibble(value = seq(0, 2, by = 0.01),
       density = dgamma(value, 5.828427, 9.656854)),
       aes(y = density),
       color = "red") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "How much did the posterior update relative to the prior?",
       subtitle = "The fit6 posteriors are in dark gray. The priors are the red lines.",
       x = "marginal posterior") +
  coord_cartesian(xlim = c(0, 2)) +
  facet_wrap(~ name, labeller = label_parsed) +
  theme(panel.grid = element_blank())
```

<img src="Mason-et-al--2014-_files/figure-gfm/unnamed-chunk-22-1.png" width="576" />

The marginal posteriors for the $\sigma$ parameters connected to the
mean model, $\sigma_{\beta_0}, \dots, \sigma_{\beta_3}$, all shrank a
little towards zero. The marginal posteriors for the $\sigma$ parameters
connected to the dispersion model are almost identical to the priors. In
both cases, the results help clarify we need data from more than 3 kids
to return high-quality data-informed posteriors for the level-2 variance
parameters. This is why it’s wise to fret about your $\sigma$ priors
when fitting multilevel growth models to small-$n$ data sets. You can
still fit the models, but the priors will be doing some heavy lifting.

## Session information

``` r
sessionInfo()
```

    ## R version 4.2.0 (2022-04-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur/Monterey 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] patchwork_1.1.2 tidybayes_3.0.2 brms_2.18.0     Rcpp_1.0.9      forcats_0.5.1   stringr_1.4.1  
    ##  [7] dplyr_1.0.10    purrr_0.3.4     readr_2.1.2     tidyr_1.2.1     tibble_3.1.8    ggplot2_3.3.6  
    ## [13] tidyverse_1.3.2
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] readxl_1.4.1         backports_1.4.1      plyr_1.8.7           igraph_1.3.4         svUnit_1.0.6        
    ##   [6] splines_4.2.0        crosstalk_1.2.0      TH.data_1.1-1        rstantools_2.2.0     inline_0.3.19       
    ##  [11] digest_0.6.29        htmltools_0.5.3      fansi_1.0.3          magrittr_2.0.3       checkmate_2.1.0     
    ##  [16] googlesheets4_1.0.1  tzdb_0.3.0           modelr_0.1.8         RcppParallel_5.1.5   matrixStats_0.62.0  
    ##  [21] xts_0.12.1           sandwich_3.0-2       prettyunits_1.1.1    colorspace_2.0-3     rvest_1.0.2         
    ##  [26] ggdist_3.2.0         haven_2.5.1          xfun_0.33            callr_3.7.1          crayon_1.5.1        
    ##  [31] jsonlite_1.8.0       lme4_1.1-30          survival_3.4-0       zoo_1.8-10           glue_1.6.2          
    ##  [36] gtable_0.3.1         gargle_1.2.0         emmeans_1.8.0        distributional_0.3.1 pkgbuild_1.3.1      
    ##  [41] rstan_2.21.7         abind_1.4-5          scales_1.2.1         mvtnorm_1.1-3        DBI_1.1.3           
    ##  [46] miniUI_0.1.1.1       xtable_1.8-4         diffobj_0.3.5        stats4_4.2.0         StanHeaders_2.21.0-7
    ##  [51] DT_0.24              htmlwidgets_1.5.4    httr_1.4.4           threejs_0.3.3        arrayhelpers_1.1-0  
    ##  [56] posterior_1.3.1      ellipsis_0.3.2       pkgconfig_2.0.3      loo_2.5.1            farver_2.1.1        
    ##  [61] dbplyr_2.2.1         utf8_1.2.2           labeling_0.4.2       tidyselect_1.1.2     rlang_1.0.6         
    ##  [66] reshape2_1.4.4       later_1.3.0          munsell_0.5.0        cellranger_1.1.0     tools_4.2.0         
    ##  [71] cli_3.4.0            generics_0.1.3       broom_1.0.1          ggridges_0.5.3       evaluate_0.16       
    ##  [76] fastmap_1.1.0        yaml_2.3.5           processx_3.7.0       knitr_1.40           fs_1.5.2            
    ##  [81] nlme_3.1-159         mime_0.12            projpred_2.2.1       xml2_1.3.3           compiler_4.2.0      
    ##  [86] bayesplot_1.9.0      shinythemes_1.2.0    rstudioapi_0.13      gamm4_0.2-6          reprex_2.0.2        
    ##  [91] stringi_1.7.8        highr_0.9            ps_1.7.1             Brobdingnag_1.2-8    lattice_0.20-45     
    ##  [96] Matrix_1.4-1         nloptr_2.0.3         markdown_1.1         shinyjs_2.1.0        tensorA_0.36.2      
    ## [101] vctrs_0.4.1          pillar_1.8.1         lifecycle_1.0.2      bridgesampling_1.1-2 estimability_1.4.1  
    ## [106] httpuv_1.6.5         R6_2.5.1             promises_1.2.0.1     gridExtra_2.3        codetools_0.2-18    
    ## [111] boot_1.3-28          colourpicker_1.1.1   MASS_7.3-58.1        gtools_3.9.3         assertthat_0.2.1    
    ## [116] withr_2.5.0          shinystan_2.6.0      multcomp_1.4-20      mgcv_1.8-40          parallel_4.2.0      
    ## [121] hms_1.1.1            grid_4.2.0           coda_0.19-4          minqa_1.2.4          rmarkdown_2.16      
    ## [126] googledrive_2.0.0    shiny_1.7.2          lubridate_1.8.0      base64enc_0.1-3      dygraphs_1.1.1.6
