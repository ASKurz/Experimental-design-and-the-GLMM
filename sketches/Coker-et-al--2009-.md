Coker et al (2009)
================
A Solomon Kurz
2022-05-23

Load our primary packages.

``` r
library(tidyverse)
library(brms)
library(tidybayes)
```

## Constraint-induced movement therapy on baby diagnosed with hemiplegic cerebral palsy

Coker et al (2009; <https://doi.org/10.3233/NRE-2009-0469>) used a
multivariate single-case ABAB design to evaluate a modified
constraint-induced movement therapy on baby diagnosed with hemiplegic
cerebral palsy.

``` r
# load the data
load(file = "/Users/solomonkurz/Dropbox/Experimental-design-and-the-GLMM/sketches/data/coker2009.rda")

# what is this?
glimpse(coker2009)
```

    ## Rows: 57
    ## Columns: 7
    ## $ behavior <chr> "reach", "reach", "reach", "reach", "reach", "reach", "reach", "reach", "reach", "reach", "…
    ## $ trial    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 1, 2, 3, 4, 5, 6, 7, 8, …
    ## $ trial0   <dbl> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 0, 1, 2, 3, 4, 5, 6, 7, 8…
    ## $ ptrial   <int> 1, 2, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 1, 2, 3, 4, 5, 6, 1, 2, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2…
    ## $ ptrial0  <dbl> 0, 1, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 0, 1, 2, 3, 4, 5, 0, 1, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1…
    ## $ phase    <fct> A1, A1, B1, B1, B1, B1, B1, B1, B1, B1, A2, A2, A2, B2, B2, B2, B2, B2, B2, A1, A1, B1, B1,…
    ## $ total    <dbl> 0, 11, 8, 4, 12, 23, 15, 11, 29, 50, 42, 1, 13, 3, 35, 20, 46, 10, 18, 6, 22, 9, 9, 11, 21,…

The data are in the long format with respect to the three behavior types
listed in the `behavior` column. Throughout the intervention, the baby
was videotaped during 10 minutes of unstructured play. A blinded
examiner coded how many times the baby used his affected right arm and
hand during developmentally appropriate tasks such as (a) reaching for
an object, (b) weightbearing (stabalizing himself) , and (c) approaching
midline during each 10-minutes play session. These were measured across
19 successive trials, which are listed in the `trial` column. The
`trial0` column is `trial -`, to help the intercept in regression
models. Each of the behaviors were assessed within four experimental
phases (i.e., A1, B1, A2, B2), which are recorded in the `phase` column.
The `ptrial` and `ptrial0` columns list the trials within each of the
four phases, restarting the sequence at the beginning of each `phase`.
The `total` column is the number of observed behaviors within a given
`trial`.

### EDA.

Here are the number of trials within each experimental phase. They are
the same for each `behavior`.

``` r
coker2009 %>%
  filter(behavior == "reach") %>% 
  count(phase)
```

    ## # A tibble: 4 × 2
    ##   phase     n
    ##   <fct> <int>
    ## 1 A1        2
    ## 2 B1        8
    ## 3 A2        3
    ## 4 B2        6

Here’s a version of Figure 2 in the original paper.

``` r
coker2009 %>% 
  mutate(Phase = ifelse(str_detect(phase, "A"), "A", "B")) %>% 
  
  ggplot(aes(x = trial, y = total, group = phase, color = Phase)) +
  geom_vline(xintercept = c(2.5, 10.5, 13.5), color = "white") +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 19, by = 2)) +
  scale_color_viridis_d(option = "A", end = .6) +
  ylab("# motor behaviors observed") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ behavior, ncol = 1)
```

<img src="Coker-et-al--2009-_files/figure-gfm/unnamed-chunk-5-1.png" width="432" />

We could also use the `geom_smooth()` method to get a sense of the
linear treands across behaviors and phases.

``` r
coker2009 %>% 
  mutate(Phase = ifelse(str_detect(phase, "A"), "A", "B")) %>% 
  
  ggplot(aes(x = trial, y = total, group = phase, color = Phase)) +
  geom_vline(xintercept = c(2.5, 10.5, 13.5), color = "white") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, size = 1/2) +
  scale_x_continuous(breaks = seq(from = 1, to = 19, by = 2)) +
  scale_color_viridis_d(option = "A", end = .6) +
  ylab("# motor behaviors observed") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ behavior, ncol = 1)
```

<img src="Coker-et-al--2009-_files/figure-gfm/unnamed-chunk-6-1.png" width="432" />

We explicitly removed the standard error ribbons from the plot with
`se = FALSE` because, IMO, they would be invalid. They’re based on OLS
estimation, which isn’t particularly appropriate for that of this kind.
However, the lines are okay for quick-and-dirty exploratory plots.

## Models

There are a handful of different models we might use to analyze these
data. We’ll consider three. In all cases, we’ll model the behavioral
counts with the Poisson likelihood, which is canonical for unbounded
counts. As the data are in the long format with respect to the
`behavior` types, the model will be multilevel in that behavioral counts
will be nested within the three levels of `behavior`.

The first model will be the unconditional growth model

![
\\begin{align\*}
\\text{total}\_{ij} & \\sim \\operatorname{Poisson}(\\lambda\_{ij}) \\\\
\\log(\\lambda\_{ij}) & = a_i + b_i \\text{trial0}\_{ij} \\\\
a_i & = \\alpha + u_i \\\\
b_i & = \\beta + v_i \\\\
\\begin{bmatrix} u_i \\\\ v_i \\end{bmatrix} & \\sim
  \\operatorname{Normal} \\left ( 
    \\begin{bmatrix} 0 \\\\ 0 \\end{bmatrix}, \\mathbf \\Sigma 
    \\right) \\\\
\\mathbf \\Sigma & = \\mathbf{SRS} \\\\
\\mathbf S & =  \\begin{bmatrix} \\sigma_u & \\\\ 0 & \\sigma_v \\end{bmatrix} \\\\
\\mathbf R & =  \\begin{bmatrix} 1 & \\\\ \\rho & 1 \\end{bmatrix} \\\\
\\alpha & \\sim \\operatorname{Normal}(\\log(5), 0.5) \\\\
\\beta  & \\sim \\operatorname{Normal}(0, 0.5) \\\\
\\sigma_u & \\sim \\operatorname{Exponential}(2) \\\\
\\sigma_v & \\sim \\operatorname{Exponential}(2) \\\\
\\rho    & \\sim \\operatorname{LKJ}(2),
\\end{align\*}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Balign%2A%7D%0A%5Ctext%7Btotal%7D_%7Bij%7D%20%26%20%5Csim%20%5Coperatorname%7BPoisson%7D%28%5Clambda_%7Bij%7D%29%20%5C%5C%0A%5Clog%28%5Clambda_%7Bij%7D%29%20%26%20%3D%20a_i%20%2B%20b_i%20%5Ctext%7Btrial0%7D_%7Bij%7D%20%5C%5C%0Aa_i%20%26%20%3D%20%5Calpha%20%2B%20u_i%20%5C%5C%0Ab_i%20%26%20%3D%20%5Cbeta%20%2B%20v_i%20%5C%5C%0A%5Cbegin%7Bbmatrix%7D%20u_i%20%5C%5C%20v_i%20%5Cend%7Bbmatrix%7D%20%26%20%5Csim%0A%20%20%5Coperatorname%7BNormal%7D%20%5Cleft%20%28%20%0A%20%20%20%20%5Cbegin%7Bbmatrix%7D%200%20%5C%5C%200%20%5Cend%7Bbmatrix%7D%2C%20%5Cmathbf%20%5CSigma%20%0A%20%20%20%20%5Cright%29%20%5C%5C%0A%5Cmathbf%20%5CSigma%20%26%20%3D%20%5Cmathbf%7BSRS%7D%20%5C%5C%0A%5Cmathbf%20S%20%26%20%3D%20%20%5Cbegin%7Bbmatrix%7D%20%5Csigma_u%20%26%20%5C%5C%200%20%26%20%5Csigma_v%20%5Cend%7Bbmatrix%7D%20%5C%5C%0A%5Cmathbf%20R%20%26%20%3D%20%20%5Cbegin%7Bbmatrix%7D%201%20%26%20%5C%5C%20%5Crho%20%26%201%20%5Cend%7Bbmatrix%7D%20%5C%5C%0A%5Calpha%20%26%20%5Csim%20%5Coperatorname%7BNormal%7D%28%5Clog%285%29%2C%200.5%29%20%5C%5C%0A%5Cbeta%20%20%26%20%5Csim%20%5Coperatorname%7BNormal%7D%280%2C%200.5%29%20%5C%5C%0A%5Csigma_u%20%26%20%5Csim%20%5Coperatorname%7BExponential%7D%282%29%20%5C%5C%0A%5Csigma_v%20%26%20%5Csim%20%5Coperatorname%7BExponential%7D%282%29%20%5C%5C%0A%5Crho%20%20%20%20%26%20%5Csim%20%5Coperatorname%7BLKJ%7D%282%29%2C%0A%5Cend%7Balign%2A%7D%0A "
\begin{align*}
\text{total}_{ij} & \sim \operatorname{Poisson}(\lambda_{ij}) \\
\log(\lambda_{ij}) & = a_i + b_i \text{trial0}_{ij} \\
a_i & = \alpha + u_i \\
b_i & = \beta + v_i \\
\begin{bmatrix} u_i \\ v_i \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \begin{bmatrix} 0 \\ 0 \end{bmatrix}, \mathbf \Sigma 
    \right) \\
\mathbf \Sigma & = \mathbf{SRS} \\
\mathbf S & =  \begin{bmatrix} \sigma_u & \\ 0 & \sigma_v \end{bmatrix} \\
\mathbf R & =  \begin{bmatrix} 1 & \\ \rho & 1 \end{bmatrix} \\
\alpha & \sim \operatorname{Normal}(\log(5), 0.5) \\
\beta  & \sim \operatorname{Normal}(0, 0.5) \\
\sigma_u & \sim \operatorname{Exponential}(2) \\
\sigma_v & \sim \operatorname{Exponential}(2) \\
\rho    & \sim \operatorname{LKJ}(2),
\end{align*}
")

where the behavioral count variable `total` varies across
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i")
behaviors and
![j](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;j "j")
trials. There is an overall intercept
![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
and `behavior`-specific deviations
![(u_i)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28u_i%29 "(u_i)")
around that overall intercept. There is an overall slope across `trial0`
![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta"),
which has `behavior`-specific deviations
![(v_i)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28v_i%29 "(v_i)")
around that overall parameter. Per by convention, those two deviation
parameters are modeled as multivariate normal with the
variance/covariance
![\\mathbf \\Sigma](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%20%5CSigma "\mathbf \Sigma")
decomposed into a standard deviation matrix
![\\mathbf S](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%20S "\mathbf S")
and correlation matrix
![\\mathbf R](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%20R "\mathbf R").

The second model allows for `phase`-specific means:

![
\\begin{align\*}
\\text{total}\_{ij} & \\sim \\operatorname{Poisson}(\\lambda\_{ij}) \\\\
\\log(\\lambda\_{ij}) & = a\_{\[\\text{phase}\]i} \\\\
a\_{\[\\text{phase}\]i} & = \\alpha\_{\[\\text{phase}\]} + u\_{\[\\text{phase}\]i} \\\\
\\begin{bmatrix} u\_{\[1\]i} \\\\ u\_{\[2\]i} \\\\ u\_{\[3\]i} \\\\ u\_{\[4\]i} \\end{bmatrix} & \\sim
  \\operatorname{Normal} \\left ( 
    \\begin{bmatrix} 0 \\\\ 0 \\\\ 0 \\\\ 0 \\end{bmatrix}, \\mathbf{SRS} 
    \\right) \\\\
\\mathbf S & = \\begin{bmatrix} 
  \\sigma_1 & & & \\\\ 
  0 & \\sigma_2 & & \\\\ 
  0 & 0 & \\sigma_3 & \\\\ 
  0 & 0 & 0 & \\sigma_4 \\end{bmatrix} \\\\
\\mathbf R & = \\begin{bmatrix} 
  1 & & & \\\\ 
  \\rho\_{21} & 1 \\\\ 
  \\rho\_{31} & \\rho\_{32} & 1 \\\\ 
  \\rho\_{41} & \\rho\_{42} & \\rho\_{43} & 1 \\end{bmatrix} \\\\
\\alpha\_{\[\\text{phase}\]} & \\sim \\operatorname{Normal}(\\log(5), 1) \\\\
\\sigma_1, \\dots, \\sigma_4 & \\sim \\operatorname{Exponential}(2) \\\\
\\mathbf R & \\sim \\operatorname{LKJ}(1).
\\end{align\*}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Balign%2A%7D%0A%5Ctext%7Btotal%7D_%7Bij%7D%20%26%20%5Csim%20%5Coperatorname%7BPoisson%7D%28%5Clambda_%7Bij%7D%29%20%5C%5C%0A%5Clog%28%5Clambda_%7Bij%7D%29%20%26%20%3D%20a_%7B%5B%5Ctext%7Bphase%7D%5Di%7D%20%5C%5C%0Aa_%7B%5B%5Ctext%7Bphase%7D%5Di%7D%20%26%20%3D%20%5Calpha_%7B%5B%5Ctext%7Bphase%7D%5D%7D%20%2B%20u_%7B%5B%5Ctext%7Bphase%7D%5Di%7D%20%5C%5C%0A%5Cbegin%7Bbmatrix%7D%20u_%7B%5B1%5Di%7D%20%5C%5C%20u_%7B%5B2%5Di%7D%20%5C%5C%20u_%7B%5B3%5Di%7D%20%5C%5C%20u_%7B%5B4%5Di%7D%20%5Cend%7Bbmatrix%7D%20%26%20%5Csim%0A%20%20%5Coperatorname%7BNormal%7D%20%5Cleft%20%28%20%0A%20%20%20%20%5Cbegin%7Bbmatrix%7D%200%20%5C%5C%200%20%5C%5C%200%20%5C%5C%200%20%5Cend%7Bbmatrix%7D%2C%20%5Cmathbf%7BSRS%7D%20%0A%20%20%20%20%5Cright%29%20%5C%5C%0A%5Cmathbf%20S%20%26%20%3D%20%5Cbegin%7Bbmatrix%7D%20%0A%20%20%5Csigma_1%20%26%20%26%20%26%20%5C%5C%20%0A%20%200%20%26%20%5Csigma_2%20%26%20%26%20%5C%5C%20%0A%20%200%20%26%200%20%26%20%5Csigma_3%20%26%20%5C%5C%20%0A%20%200%20%26%200%20%26%200%20%26%20%5Csigma_4%20%5Cend%7Bbmatrix%7D%20%5C%5C%0A%5Cmathbf%20R%20%26%20%3D%20%5Cbegin%7Bbmatrix%7D%20%0A%20%201%20%26%20%26%20%26%20%5C%5C%20%0A%20%20%5Crho_%7B21%7D%20%26%201%20%5C%5C%20%0A%20%20%5Crho_%7B31%7D%20%26%20%5Crho_%7B32%7D%20%26%201%20%5C%5C%20%0A%20%20%5Crho_%7B41%7D%20%26%20%5Crho_%7B42%7D%20%26%20%5Crho_%7B43%7D%20%26%201%20%5Cend%7Bbmatrix%7D%20%5C%5C%0A%5Calpha_%7B%5B%5Ctext%7Bphase%7D%5D%7D%20%26%20%5Csim%20%5Coperatorname%7BNormal%7D%28%5Clog%285%29%2C%201%29%20%5C%5C%0A%5Csigma_1%2C%20%5Cdots%2C%20%5Csigma_4%20%26%20%5Csim%20%5Coperatorname%7BExponential%7D%282%29%20%5C%5C%0A%5Cmathbf%20R%20%26%20%5Csim%20%5Coperatorname%7BLKJ%7D%281%29.%0A%5Cend%7Balign%2A%7D%0A "
\begin{align*}
\text{total}_{ij} & \sim \operatorname{Poisson}(\lambda_{ij}) \\
\log(\lambda_{ij}) & = a_{[\text{phase}]i} \\
a_{[\text{phase}]i} & = \alpha_{[\text{phase}]} + u_{[\text{phase}]i} \\
\begin{bmatrix} u_{[1]i} \\ u_{[2]i} \\ u_{[3]i} \\ u_{[4]i} \end{bmatrix} & \sim
  \operatorname{Normal} \left ( 
    \begin{bmatrix} 0 \\ 0 \\ 0 \\ 0 \end{bmatrix}, \mathbf{SRS} 
    \right) \\
\mathbf S & = \begin{bmatrix} 
  \sigma_1 & & & \\ 
  0 & \sigma_2 & & \\ 
  0 & 0 & \sigma_3 & \\ 
  0 & 0 & 0 & \sigma_4 \end{bmatrix} \\
\mathbf R & = \begin{bmatrix} 
  1 & & & \\ 
  \rho_{21} & 1 \\ 
  \rho_{31} & \rho_{32} & 1 \\ 
  \rho_{41} & \rho_{42} & \rho_{43} & 1 \end{bmatrix} \\
\alpha_{[\text{phase}]} & \sim \operatorname{Normal}(\log(5), 1) \\
\sigma_1, \dots, \sigma_4 & \sim \operatorname{Exponential}(2) \\
\mathbf R & \sim \operatorname{LKJ}(1).
\end{align*}
")

Here the
![{\[\\text{phase}\]}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%7B%5B%5Ctext%7Bphase%7D%5D%7D "{[\text{phase}]}")
subscript indicates the
![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
parameter is indexed by the four levels of `phase`, which all also vary
across the three levels of `behavior` as indicated by the
`behavior`-specific deviations around those four grand means
![u\_{\[\\text{phase}\]i}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;u_%7B%5B%5Ctext%7Bphase%7D%5Di%7D "u_{[\text{phase}]i}").
Those four deviation parameters are modeled as multivariate normal in
the typical way, with the
![\\mathbf S](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%20S "\mathbf S")
and
![\\mathbf R](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%20R "\mathbf R")
matrices now following a
![4 \\times 4](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;4%20%5Ctimes%204 "4 \times 4")
structure.

The final model is what you might call the full or theory-based model.
It’s a conditional growth model of the form

![
\\begin{align\*}
\\text{total}\_{ij} & \\sim \\operatorname{Poisson}(\\lambda\_{ij}) \\\\
\\log(\\lambda\_{ij}) & = b\_{0i} + b_1 \\text{B1}\_{ij} + b\_{2i} \\text{A2}\_{ij} + b\_{3i} \\text{B2}\_{ij} + b\_{4i} \\text{trial0}\_{ij} + b\_{5i} (\\text{B1} \\cdot \\text{trial0})\_{ij}+ b\_{6i} (\\text{A2} \\cdot \\text{trial0})\_{ij} + b\_{7i} (\\text{B2} \\cdot \\text{trial0})\_{ij} \\\\
b\_{0i} & = \\beta_0 + u\_{0i}  \\\\
b\_{1i} & = \\beta_1 + u\_{1i}  \\\\
b\_{2i} & = \\beta_2 + u\_{2i}  \\\\
b\_{3i} & = \\beta_3 + u\_{3i}  \\\\
b\_{4i} & = \\beta_4 + u\_{4i}  \\\\
b\_{5i} & = \\beta_5 + u\_{5i}  \\\\
b\_{6i} & = \\beta_6 + u\_{6i}  \\\\
b\_{7i} & = \\beta_7 + u\_{7i}  \\\\
\\begin{bmatrix} u\_{0i} \\\\ \\vdots \\\\ u\_{7i} \\end{bmatrix} & \\sim
  \\operatorname{Normal}(\\mathbf 0, \\mathbf{SRS}) \\\\
\\beta_0 & \\sim \\operatorname{Normal}(\\log(5), 0.5) \\\\
\\beta_1, \\dots, \\beta_7  & \\sim \\operatorname{Normal}(0, 0.5) \\\\
\\sigma_1, \\dots, \\sigma_8 & \\sim \\operatorname{Exponential}(2) \\\\
\\mathbf R & \\sim \\operatorname{LKJ}(1),
\\end{align\*}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Balign%2A%7D%0A%5Ctext%7Btotal%7D_%7Bij%7D%20%26%20%5Csim%20%5Coperatorname%7BPoisson%7D%28%5Clambda_%7Bij%7D%29%20%5C%5C%0A%5Clog%28%5Clambda_%7Bij%7D%29%20%26%20%3D%20b_%7B0i%7D%20%2B%20b_1%20%5Ctext%7BB1%7D_%7Bij%7D%20%2B%20b_%7B2i%7D%20%5Ctext%7BA2%7D_%7Bij%7D%20%2B%20b_%7B3i%7D%20%5Ctext%7BB2%7D_%7Bij%7D%20%2B%20b_%7B4i%7D%20%5Ctext%7Btrial0%7D_%7Bij%7D%20%2B%20b_%7B5i%7D%20%28%5Ctext%7BB1%7D%20%5Ccdot%20%5Ctext%7Btrial0%7D%29_%7Bij%7D%2B%20b_%7B6i%7D%20%28%5Ctext%7BA2%7D%20%5Ccdot%20%5Ctext%7Btrial0%7D%29_%7Bij%7D%20%2B%20b_%7B7i%7D%20%28%5Ctext%7BB2%7D%20%5Ccdot%20%5Ctext%7Btrial0%7D%29_%7Bij%7D%20%5C%5C%0Ab_%7B0i%7D%20%26%20%3D%20%5Cbeta_0%20%2B%20u_%7B0i%7D%20%20%5C%5C%0Ab_%7B1i%7D%20%26%20%3D%20%5Cbeta_1%20%2B%20u_%7B1i%7D%20%20%5C%5C%0Ab_%7B2i%7D%20%26%20%3D%20%5Cbeta_2%20%2B%20u_%7B2i%7D%20%20%5C%5C%0Ab_%7B3i%7D%20%26%20%3D%20%5Cbeta_3%20%2B%20u_%7B3i%7D%20%20%5C%5C%0Ab_%7B4i%7D%20%26%20%3D%20%5Cbeta_4%20%2B%20u_%7B4i%7D%20%20%5C%5C%0Ab_%7B5i%7D%20%26%20%3D%20%5Cbeta_5%20%2B%20u_%7B5i%7D%20%20%5C%5C%0Ab_%7B6i%7D%20%26%20%3D%20%5Cbeta_6%20%2B%20u_%7B6i%7D%20%20%5C%5C%0Ab_%7B7i%7D%20%26%20%3D%20%5Cbeta_7%20%2B%20u_%7B7i%7D%20%20%5C%5C%0A%5Cbegin%7Bbmatrix%7D%20u_%7B0i%7D%20%5C%5C%20%5Cvdots%20%5C%5C%20u_%7B7i%7D%20%5Cend%7Bbmatrix%7D%20%26%20%5Csim%0A%20%20%5Coperatorname%7BNormal%7D%28%5Cmathbf%200%2C%20%5Cmathbf%7BSRS%7D%29%20%5C%5C%0A%5Cbeta_0%20%26%20%5Csim%20%5Coperatorname%7BNormal%7D%28%5Clog%285%29%2C%200.5%29%20%5C%5C%0A%5Cbeta_1%2C%20%5Cdots%2C%20%5Cbeta_7%20%20%26%20%5Csim%20%5Coperatorname%7BNormal%7D%280%2C%200.5%29%20%5C%5C%0A%5Csigma_1%2C%20%5Cdots%2C%20%5Csigma_8%20%26%20%5Csim%20%5Coperatorname%7BExponential%7D%282%29%20%5C%5C%0A%5Cmathbf%20R%20%26%20%5Csim%20%5Coperatorname%7BLKJ%7D%281%29%2C%0A%5Cend%7Balign%2A%7D%0A "
\begin{align*}
\text{total}_{ij} & \sim \operatorname{Poisson}(\lambda_{ij}) \\
\log(\lambda_{ij}) & = b_{0i} + b_1 \text{B1}_{ij} + b_{2i} \text{A2}_{ij} + b_{3i} \text{B2}_{ij} + b_{4i} \text{trial0}_{ij} + b_{5i} (\text{B1} \cdot \text{trial0})_{ij}+ b_{6i} (\text{A2} \cdot \text{trial0})_{ij} + b_{7i} (\text{B2} \cdot \text{trial0})_{ij} \\
b_{0i} & = \beta_0 + u_{0i}  \\
b_{1i} & = \beta_1 + u_{1i}  \\
b_{2i} & = \beta_2 + u_{2i}  \\
b_{3i} & = \beta_3 + u_{3i}  \\
b_{4i} & = \beta_4 + u_{4i}  \\
b_{5i} & = \beta_5 + u_{5i}  \\
b_{6i} & = \beta_6 + u_{6i}  \\
b_{7i} & = \beta_7 + u_{7i}  \\
\begin{bmatrix} u_{0i} \\ \vdots \\ u_{7i} \end{bmatrix} & \sim
  \operatorname{Normal}(\mathbf 0, \mathbf{SRS}) \\
\beta_0 & \sim \operatorname{Normal}(\log(5), 0.5) \\
\beta_1, \dots, \beta_7  & \sim \operatorname{Normal}(0, 0.5) \\
\sigma_1, \dots, \sigma_8 & \sim \operatorname{Exponential}(2) \\
\mathbf R & \sim \operatorname{LKJ}(1),
\end{align*}
")

where now the intercepts and `ptrial0` slopes vary across the four
levels of `phase`. Given that all these also vary across the three
levels of `behavior`, this results in an
![8 \\times 8](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;8%20%5Ctimes%208 "8 \times 8")
structure for the
![\\mathbf S](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%20S "\mathbf S")
and
![\\mathbf R](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%20R "\mathbf R")
matrices.

Here’s how to fit the models with **brms**.

``` r
# unconditional growth
fit1 <- brm(
  data = coker2009,
  family = poisson,
  total ~ 0 + Intercept + trial0 + (1 + trial0 | behavior),
  prior = c(prior(normal(log(5), 0.5), class = b, coef = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(exponential(2), class = sd),
            prior(lkj(2), class = cor)),
  cores = 4, seed = 1,
  control = list(adapt_delta = .9995,
                 max_treedepth = 12),
  file = "fits/fit1.coker2009"
)

# means by phase
fit2 <- brm(
  data = coker2009,
  family = poisson,
  total ~ 0 + phase + (0 + phase | behavior),
  prior = c(prior(normal(log(5), 1), class = b),
            prior(exponential(2), class = sd),
            prior(lkj(1), class = cor)),
  cores = 4, seed = 1,
  control = list(adapt_delta = .999,
                 max_treedepth = 12),
  file = "fits/fit2.coker2009"
)

# conditional growth model (within-phase growth)
fit3 <- brm(
  data = coker2009,
  family = poisson,
  total ~ 0 + Intercept + phase + ptrial0 + phase:ptrial0 + (1 + phase + ptrial0 + phase:ptrial0 | behavior),
  prior = c(prior(normal(log(5), 0.5), class = b, coef = Intercept),
            prior(normal(0, 0.5), class = b),
            prior(exponential(2), class = sd),
            prior(lkj(1), class = cor)),
  cores = 4, seed = 1,
  control = list(adapt_delta = .9999,
                 max_treedepth = 12),
  file = "fits/fit3.coker2009"
)
```

As is often the case in when you have few levels for the multilevel
grouping term (i.e., the levels of `behavior`), all three models
required adjustments to the parameters within the `control` settings to
keep the MCMC chains healthy.

Rather than looking at the model summaries with `print()`, we just plot.

``` r
model <- c("Unconditional growth", "Means by phase", "Within-phase growth")

rbind(
  fitted(fit1),
  fitted(fit2),
  fitted(fit3)
) %>% 
  data.frame() %>% 
  bind_cols(
    bind_rows(coker2009, coker2009, coker2009)
  ) %>% 
  mutate(Phase = ifelse(str_detect(phase, "A"), "A", "B"),
         model = rep(model, each = n() / 3) %>% factor(., levels = model))  %>% 
  
  ggplot(aes(x = trial, group = phase)) +
  geom_vline(xintercept = c(2.5, 10.5, 13.5), color = "grey50", linetype = 2, size = 1/4) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = Phase),
              alpha = 1/3) +
  geom_line(aes(y = Estimate, color = Phase)) +
  geom_point(aes(y = total),
             size = 2/3) +
  scale_fill_viridis_d(option = "A", begin = .2, end = .6) +
  scale_color_viridis_d(option = "A", begin = .2, end = .6) +
  scale_x_continuous(breaks = seq(from = 1, to = 19, by = 2)) +
  labs(title = "Constraint-induced movement therapy on a baby diagnosed with hemiplegic cerebral palsy",
       "behavor count",
       subtitle = "The three behavior types are separated by the facet columns. The three competing models are depicted by\nthe faceted columns, starting with the simplest (unconditinoal growth) at the top and ending with the full\ntheoretically-justified conditional growth model on the bottom.",
       x = "Trial",
       y = "Count") +
  facet_grid(model ~ behavior) +
  theme_linedraw() +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot",
        strip.background = element_blank(),
        strip.text = element_text(color = "black"))
```

<img src="Coker-et-al--2009-_files/figure-gfm/unnamed-chunk-8-1.png" width="768" />

IMO, you’re best off using `fit3`, the full theoretically-justified
conditional growth model. However, you could compare the models by their
out-of-sample performance with their LOO-CV estimates and contrasts.

``` r
fit1 <- add_criterion(fit1, criterion = "loo")
fit2 <- add_criterion(fit2, criterion = "loo")
fit3 <- add_criterion(fit3, criterion = "loo")

loo_compare(fit1, fit2, fit3) %>% 
  print(simplify = F)
```

    ##      elpd_diff se_diff elpd_loo se_elpd_loo p_loo  se_p_loo looic  se_looic
    ## fit3    0.0       0.0  -312.0     33.7        77.6   13.9    624.0   67.4  
    ## fit1  -14.7      23.5  -326.7     36.9        32.3    6.5    653.4   73.8  
    ## fit2  -43.8      27.7  -355.8     38.5        74.0   14.3    711.6   77.1

Numerically, the full model `fit3` made the best sense of the data from
a cross-validation perspective. Note, however, how large the standard
errors are relative to the differences, particularly for `fit3` relative
to `fit1`. What does this mean? If you study the top and bottom columns
of the facet plot, above, you’ll see that the results from the
experiment are somewhat ambiguous. The unconditional growth model,
alone, made pretty good sense of the data. Why might that be? We’re
measuring behaviors in a baby and what do babies do if not develop? I
suspect Corker and colleagues would have provided a more convincing
demonstration of their intervention had they collected data on more
trials, particularly during the A phases. Yes, there’s enough
information in the data to suggest the intervention was probably
helpful. But the evidence is weak and no-one should be breaking open the
Champagne.

Here’s how you might compute effect sizes (unstandardized mean
differences) with the full model `fit3`.

``` r
nd <- coker2009 %>% 
  group_by(behavior, phase) %>% 
  summarise(ptrial0 = mean(ptrial0)) %>% 
  ungroup() %>% 
  mutate(row = 1:n())

fitted(fit3,
       newdata = nd,
       summary = F) %>% 
  data.frame() %>% 
  set_names(1:12) %>% 
  mutate(draw = 1:n()) %>% 
  pivot_longer(-draw, 
               names_to = "row",
               names_transform = list(row = as.integer)) %>% 
  left_join(nd, by = "row") %>% 
  select(-row, -ptrial0) %>% 
  pivot_wider(names_from = phase, values_from = value) %>% 
  mutate(`B1 - A1` = B1 - A1,
         `B2 - A2` = B2 - A2) %>% 
  mutate(`B - A` = (`B1 - A1` + `B2 - A2`) / 2) %>% 
  pivot_longer(contains("-"), names_to = "contrast") %>% 
  mutate(contrast = factor(contrast, levels = c("B1 - A1", "B2 - A2", "B - A"))) %>% 
  
  ggplot(aes(x = value, y = 0)) +
  geom_vline(xintercept = 0, size = 1/4, linetype = 2, color = "grey25") +
  tidybayes::stat_halfeye(.width = .95, size = 1) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Effect sizes from the single-case multilevel Poisson conditional growth model",
       subtitle = "The gray shapes are the posterior distributions for the effect sizes.\nThe dots and horizontal lines at their bases are the posterior means and 95% credible intervals.",
       x = "unstandardized mean differences in behavioral counts") +
  facet_grid(contrast ~ behavior) +
  theme_linedraw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.text = element_text(color = "black"))
```

<img src="Coker-et-al--2009-_files/figure-gfm/unnamed-chunk-10-1.png" width="720" />

## Session information

``` r
sessionInfo()
```

    ## R version 4.2.0 (2022-04-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.7
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
    ##  [1] tidybayes_3.0.2      brms_2.17.3          Rcpp_1.0.8.3         forcats_0.5.1        stringr_1.4.0       
    ##  [6] dplyr_1.0.9          purrr_0.3.4          readr_2.1.2          tidyr_1.2.0          tibble_3.1.7        
    ## [11] ggplot2_3.3.6        tidyverse_1.3.1.9000
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] readxl_1.4.0         backports_1.4.1      plyr_1.8.7           igraph_1.3.1         splines_4.2.0       
    ##   [6] svUnit_1.0.6         crosstalk_1.2.0      TH.data_1.1-1        rstantools_2.2.0     inline_0.3.19       
    ##  [11] digest_0.6.29        htmltools_0.5.2      fansi_1.0.3          magrittr_2.0.3       checkmate_2.1.0     
    ##  [16] googlesheets4_1.0.0  tzdb_0.3.0           modelr_0.1.8         dtplyr_1.2.1         RcppParallel_5.1.5  
    ##  [21] matrixStats_0.62.0   xts_0.12.1           sandwich_3.0-1       prettyunits_1.1.1    colorspace_2.0-3    
    ##  [26] rvest_1.0.2          ggdist_3.1.1         haven_2.5.0          xfun_0.31            callr_3.7.0         
    ##  [31] crayon_1.5.1         jsonlite_1.8.0       survival_3.3-1       zoo_1.8-10           glue_1.6.2          
    ##  [36] gtable_0.3.0         gargle_1.2.0         emmeans_1.7.3        V8_4.1.0             distributional_0.3.0
    ##  [41] pkgbuild_1.3.1       rstan_2.26.11        abind_1.4-5          scales_1.2.0         mvtnorm_1.1-3       
    ##  [46] DBI_1.1.2            miniUI_0.1.1.1       viridisLite_0.4.0    xtable_1.8-4         diffobj_0.3.5       
    ##  [51] stats4_4.2.0         StanHeaders_2.26.11  DT_0.22              htmlwidgets_1.5.4    httr_1.4.3          
    ##  [56] threejs_0.3.3        arrayhelpers_1.1-0   posterior_1.2.1      ellipsis_0.3.2       pkgconfig_2.0.3     
    ##  [61] loo_2.5.1            farver_2.1.0         dbplyr_2.1.1.9000    utf8_1.2.2           labeling_0.4.2      
    ##  [66] tidyselect_1.1.2     rlang_1.0.2          reshape2_1.4.4       later_1.3.0          munsell_0.5.0       
    ##  [71] cellranger_1.1.0     tools_4.2.0          cli_3.3.0            generics_0.1.2       broom_0.8.0         
    ##  [76] ggridges_0.5.3       evaluate_0.15        fastmap_1.1.0        yaml_2.3.5           processx_3.5.3      
    ##  [81] knitr_1.39           fs_1.5.2             nlme_3.1-157         mime_0.12            xml2_1.3.3          
    ##  [86] compiler_4.2.0       bayesplot_1.9.0      shinythemes_1.2.0    rstudioapi_0.13      curl_4.3.2          
    ##  [91] reprex_2.0.1         stringi_1.7.6        highr_0.9            ps_1.7.0             Brobdingnag_1.2-7   
    ##  [96] lattice_0.20-45      Matrix_1.4-1         markdown_1.1         shinyjs_2.1.0        tensorA_0.36.2      
    ## [101] vctrs_0.4.1          pillar_1.7.0         lifecycle_1.0.1      bridgesampling_1.1-2 estimability_1.3    
    ## [106] data.table_1.14.2    httpuv_1.6.5         R6_2.5.1             promises_1.2.0.1     gridExtra_2.3       
    ## [111] codetools_0.2-18     colourpicker_1.1.1   MASS_7.3-56          gtools_3.9.2         assertthat_0.2.1    
    ## [116] withr_2.5.0          shinystan_2.6.0      multcomp_1.4-19      mgcv_1.8-40          parallel_4.2.0      
    ## [121] hms_1.1.1            grid_4.2.0           coda_0.19-4          rmarkdown_2.14       googledrive_2.0.0   
    ## [126] shiny_1.7.1          lubridate_1.8.0      base64enc_0.1-3      dygraphs_1.1.1.6
