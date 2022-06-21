Wagenmakers et al (2016), Albohn only
================
A Solomon Kurz
2022-06-21

Using a registered replication report approach, Wagenmakers and
colleagues (2016; <https://doi.org/10.1177/1745691616674458>) replicated
Study 1 from the highly-cited paper by Strack, Martin, and Stepper
(1988; <https://doi.org/10.1037/0022-3514.54.5.768>). In 17 labs across
several countries, participants were randomized into the *pout* or
*smile* conditions. The primary variable is the ratings on 4
moderately-funny cartoons. For the sake of this file, we will analyze
the data from one of the labs.

## Data

Wagenmakers and colleagues kindly made their data, materials, and code
public on the OSF at <https://osf.io/hgi2y/>.

Load the data and the primary **R** packages.

``` r
# packages
# library(tidyverse)

library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

library(brms)

# load the data
load(file = "data/wagenmakers2016.rda")

# what is this?
glimpse(wagenmakers2016)
```

    ## Rows: 2,612
    ## Columns: 24
    ## $ lab                                   <chr> "Albohn", "Albohn", "Albohn", "Albohn", "Albohn", "Albohn", "A…
    ## $ sn                                    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,…
    ## $ subject_no                            <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,…
    ## $ participant_id                        <chr> "1001", "1002", "1003", "1004", "1005", "1006", "1007", "1008"…
    ## $ condition                             <dbl> 1, 0, 0, 0, 1, 1, NA, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1…
    ## $ correct1                              <dbl> 0, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ correct2                              <dbl> 0, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ correct3                              <dbl> 0, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ correct4                              <dbl> 0, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ correct_total                         <dbl> 0, 4, 4, 0, 4, 4, NA, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 4, 4…
    ## $ task1                                 <dbl> 3, 5, 9, 1, 6, 6, NA, 7, 5, 5, 6, 6, 3, 6, 9, 3, 7, 6, 7, 7, 1…
    ## $ task2                                 <dbl> 2, 7, 7, 1, 3, 2, NA, 4, 4, 6, 3, 6, 6, 5, 4, 5, 3, 2, 8, 8, 1…
    ## $ rating1                               <dbl> 8, 5, 0, 3, 4, 5, NA, 2, 5, 3, 3, 3, 1, 5, 4, 1, 7, 3, 2, 5, 2…
    ## $ rating2                               <dbl> NA, 4, 0, 4, 7, 5, NA, 5, 6, 3, 3, 6, 0, 6, 8, 1, 6, 6, 5, 6, …
    ## $ rating3                               <dbl> NA, 6, 1, 3, 5, 4, NA, 3, 3, 2, 3, 5, 0, 5, 0, 1, 3, 3, 3, 6, …
    ## $ rating4                               <dbl> NA, 7, 0, 2, 3, 6, NA, 0, 5, 5, 5, 7, 0, 5, 1, 2, 5, 5, 2, 8, …
    ## $ self_reported_task_performance        <dbl> NA, 7, 5, 4, 7, 4, NA, 8, 4, 3, 3, 4, 7, 9, 8, 7, 2, 9, 7, 4, …
    ## $ comprehended_cartoons                 <dbl> 1, 1, 1, 2, 1, 1, NA, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ aware_of_the_goal                     <dbl> NA, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, …
    ## $ participants_guess_of_the_studys_goal <chr> NA, "To track how well people listen / take odd requests serio…
    ## $ age                                   <dbl> 18, 18, 21, 18, 19, 18, NA, 18, 18, 18, 19, 18, 18, 18, 18, 18…
    ## $ gender                                <dbl> 1, 0, 0, 0, 1, 1, NA, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1…
    ## $ student                               <dbl> 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ occupation_or_field_of_study          <chr> "Business", "Business", "Psychology", "Biology", "Finance", "E…

The full `wagenmakers2016` data set contains data from 17 reserach labs.
In this file, we’ll focus on the data collected by the Albohn lab. Here
we filter the `wagenmakers2016` and save the results as `albohn`.

``` r
albohn <- wagenmakers2016 %>% 
  filter(lab == "Albohn")
```

Wagenmakers and colleagues had several exclusion criteria, which they
detailed in the *Exclusion criteria* subsection of their *Method*
(pp. 4-5). They excluded the data from participants

-   whose average cartoon rating exceeded 2.5 standard deviations from
    the group mean in their condition,
-   who correctly guessed the goal of the study,
-   who answered “No” to the question “Did you understand the
    cartoons?”, and
-   who held the pen incorrectly for two or more of the four cartoons.

In this analysis, we will exclude participants based on the latter three
criteria. However, I am apposed to removing data from those whose
average ratings are 2.5 standard deviations from the group mean. A good
model should be able to handle unusual observations, like that. Here we
prune the data.

``` r
# start with n = 166
albohn <- albohn %>% 
  # n = 161 (drop 2 who were 1 and 3 who were NA)
  filter(aware_of_the_goal == 0) %>% 
  # n = 151 (drop 8 who were 2 and 2 who were NA)
  filter(comprehended_cartoons == 1) %>% 
  # n = 140 (drop 11 who were 0)
  filter(correct_total > 2)

# new sample size
albohn %>% 
  count(condition)
```

    ## # A tibble: 2 × 2
    ##   condition     n
    ##       <dbl> <int>
    ## 1         0    72
    ## 2         1    68

Data from 140 participants remain. Happily, the group sizes are similar.

Now we’ll simplify the data by removing the unnecessary columns and then
make the data long with respect to the four cartoons.

``` r
albohn <- albohn %>% 
  select(sn, rating1:rating4, condition) %>% 
  # convert to the long format W/R/T cartoon
  pivot_longer(rating1:rating4,
               names_to = "cartoon",
               values_to = "rating") %>% 
  mutate(cartoon = str_remove(cartoon, "rating") %>% as.double()) %>% 
  mutate(ratingf    = factor(rating, levels = 0:9, ordered = TRUE),
         conditionf = ifelse(condition == 0, "pout", "smile"))

# what is this?
head(albohn)
```

    ## # A tibble: 6 × 6
    ##      sn condition cartoon rating ratingf conditionf
    ##   <int>     <dbl>   <dbl>  <dbl> <ord>   <chr>     
    ## 1     2         0       1      5 5       pout      
    ## 2     2         0       2      4 4       pout      
    ## 3     2         0       3      6 6       pout      
    ## 4     2         0       4      7 7       pout      
    ## 5     3         0       1      0 0       pout      
    ## 6     3         0       2      0 0       pout

The remaining columns in the data are:

-   `sn` is the participant index.
-   `condition` is the dummy-coded experimental index for which `0` is
    the pout condition and `1` is the smiling condition.
-   `cartoon` is the index for the four *Far Side* cartoons.
-   `rating` is the rating for the cartoons on the 0-9 Likert-type
    scale, saved as a numeral.
-   `ratingf` is the ordered factor version of `rating`.
-   `conditionf` is the factor version of `Condition`.

### EDA.

For our exploratory data analyses, it might help if we start with
faceted bar plot.

``` r
albohn %>% 
  mutate(cartoon = str_c("cartoon: ", cartoon)) %>% 
  
  ggplot(aes(x = rating)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:9) +
  facet_grid(conditionf ~ cartoon) +
  theme(panel.grid = element_blank())
```

<img src="Wagenmakers-et-al--2016--Albohn-only_files/figure-gfm/unnamed-chunk-6-1.png" width="672" />
