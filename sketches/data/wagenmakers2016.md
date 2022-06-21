wagenmakers2016
================
A Solomon Kurz
2022-06-21

The purpose of this file is to consolidate and reformat the data from
Wagenmakers et al (2016; <https://doi.org/10.1177/1745691616674458>).
The original data are openly available on the OSF at
<https://osf.io/9j72u/>, downloadable in a zip file called
`Data_FINAL.zip`. However, within that file, the data are saved in 17
separate `.csv` files, by research lab. We want them saved in a single
file.

Load the **tidyverse**.

``` r
# library(tidyverse)

library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
```

Load the files and wrangle.

``` r
# data path and individual lab file names
data_path <- "Data_FINAL/"

# column names for the .csv data
col_names <- c("subject_no", "participant_id", "condition", str_c("correct", 1:4), "correct_total", str_c("task", 1:2), str_c("rating", 1:4), "self_reported_task_performance", "comprehended_cartoons", "aware_of_the_goal", "participants_guess_of_the_studys_goal", "age", "gender", "student", "occupation_or_field_of_study")

wagenmakers2016 <- tibble(files = list.files(data_path, pattern = "*_Data.csv")) %>% 
  mutate(path = str_c(data_path, files)) %>% 
  mutate(data = map(path, ~read_csv(., 
                                    col_names = col_names, 
                                    skip = 2, 
                                    col_select = 1:22, 
                                    col_types = "dcdddddddddddddddcdddc"))) %>% 
  unnest(data) %>% 
  mutate(lab = str_extract(files, "[^_]+")) %>% 
  # update one of the lab names
  mutate(lab = ifelse(lab == "檢dogru", "Özdoğru", lab)) %>% 
  # add unique participant identifier
  mutate(sn = 1:n()) %>% 
  # rearrange columns
  select(lab, sn, everything(), -files, -path) 
```

    ## Warning: One or more parsing issues, see `problems()` for details
    ## One or more parsing issues, see `problems()` for details

``` r
# what?
glimpse(wagenmakers2016)
```

    ## Rows: 2,612
    ## Columns: 24
    ## $ lab                                   <chr> "Albohn", "Albohn", "Albohn", "A…
    ## $ sn                                    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1…
    ## $ subject_no                            <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1…
    ## $ participant_id                        <chr> "1001", "1002", "1003", "1004", …
    ## $ condition                             <dbl> 1, 0, 0, 0, 1, 1, NA, 0, 0, 0, 1…
    ## $ correct1                              <dbl> 0, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1…
    ## $ correct2                              <dbl> 0, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1…
    ## $ correct3                              <dbl> 0, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1…
    ## $ correct4                              <dbl> 0, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1…
    ## $ correct_total                         <dbl> 0, 4, 4, 0, 4, 4, NA, 4, 4, 4, 4…
    ## $ task1                                 <dbl> 3, 5, 9, 1, 6, 6, NA, 7, 5, 5, 6…
    ## $ task2                                 <dbl> 2, 7, 7, 1, 3, 2, NA, 4, 4, 6, 3…
    ## $ rating1                               <dbl> 8, 5, 0, 3, 4, 5, NA, 2, 5, 3, 3…
    ## $ rating2                               <dbl> NA, 4, 0, 4, 7, 5, NA, 5, 6, 3, …
    ## $ rating3                               <dbl> NA, 6, 1, 3, 5, 4, NA, 3, 3, 2, …
    ## $ rating4                               <dbl> NA, 7, 0, 2, 3, 6, NA, 0, 5, 5, …
    ## $ self_reported_task_performance        <dbl> NA, 7, 5, 4, 7, 4, NA, 8, 4, 3, …
    ## $ comprehended_cartoons                 <dbl> 1, 1, 1, 2, 1, 1, NA, 1, 1, 1, 1…
    ## $ aware_of_the_goal                     <dbl> NA, 0, 0, 0, 0, 0, NA, 0, 0, 0, …
    ## $ participants_guess_of_the_studys_goal <chr> NA, "To track how well people li…
    ## $ age                                   <dbl> 18, 18, 21, 18, 19, 18, NA, 18, …
    ## $ gender                                <dbl> 1, 0, 0, 0, 1, 1, NA, 1, 0, 0, 0…
    ## $ student                               <dbl> 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1…
    ## $ occupation_or_field_of_study          <chr> "Business", "Business", "Psychol…

Do note, the `wagenmakers2016` data frame contains all cases, even those
which are to be dropped by the exclusion criteria. Wagenmakers and
colleagues had several exclusion criteria, which they detailed in the
*Exclusion criteria* subsection of their *Method* (pp. 4-5). They
excluded the data from participants:

-   whose average cartoon rating exceeded 2.5 standard deviations from
    the group mean in their condition,
-   who correctly guessed the goal of the study (see
    `aware_of_the_goal`),
-   who answered “No” to the question “Did you understand the cartoons?”
    (see `comprehended_cartoons`), and
-   who held the pen incorrectly for two or more of the four cartoons
    (see `correct_total`).

Here’s how one might filter based on the last 3 of the 4 criteria.

``` r
# start with n = 2612
wagenmakers2016 %>%
  # n = 2436 (drop 87 who were 1 and 89 who were NA)
  filter(aware_of_the_goal == 0) %>% 
  # n = 2190 (drop 232 who were 0, 8 who were 2, and 6 who were NA)
  filter(comprehended_cartoons == 1) %>% 
  # n = 1913 (drop 199 who were 0, 27 who were 1, 49 who were 2, and 2 who were NA)
  filter(correct_total > 2)
```

    ## # A tibble: 1,913 × 24
    ##    lab       sn subject_no participant_id condition correct1 correct2 correct3
    ##    <chr>  <int>      <dbl> <chr>              <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 Albohn     2          2 1002                   0        1        1        1
    ##  2 Albohn     3          3 1003                   0        1        1        1
    ##  3 Albohn     5          5 1005                   1        1        1        1
    ##  4 Albohn     6          6 1006                   1        1        1        1
    ##  5 Albohn     8          8 1008                   0        1        1        1
    ##  6 Albohn     9          9 1009                   0        1        1        1
    ##  7 Albohn    10         10 1010                   0        1        1        1
    ##  8 Albohn    11         11 1011                   1        1        1        1
    ##  9 Albohn    12         12 1012                   1        1        1        1
    ## 10 Albohn    14         14 1014                   0        1        1        1
    ## # … with 1,903 more rows, and 16 more variables: correct4 <dbl>,
    ## #   correct_total <dbl>, task1 <dbl>, task2 <dbl>, rating1 <dbl>,
    ## #   rating2 <dbl>, rating3 <dbl>, rating4 <dbl>,
    ## #   self_reported_task_performance <dbl>, comprehended_cartoons <dbl>,
    ## #   aware_of_the_goal <dbl>, participants_guess_of_the_studys_goal <chr>,
    ## #   age <dbl>, gender <dbl>, student <dbl>, occupation_or_field_of_study <chr>

## Save

Now save the results in an external `.rda` file.

``` r
save(wagenmakers2016, file = "wagenmakers2016.rda")
```

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
    ## [1] readr_2.1.2   purrr_0.3.4   stringr_1.4.0 tidyr_1.2.0   dplyr_1.0.9  
    ## [6] tibble_3.1.7  ggplot2_3.3.6
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] pillar_1.7.0     compiler_4.2.0   tools_4.2.0      bit_4.0.4       
    ##  [5] digest_0.6.29    evaluate_0.15    lifecycle_1.0.1  gtable_0.3.0    
    ##  [9] pkgconfig_2.0.3  rlang_1.0.2      cli_3.3.0        DBI_1.1.2       
    ## [13] rstudioapi_0.13  parallel_4.2.0   yaml_2.3.5       xfun_0.31       
    ## [17] fastmap_1.1.0    withr_2.5.0      knitr_1.39       hms_1.1.1       
    ## [21] generics_0.1.2   vctrs_0.4.1      bit64_4.0.5      grid_4.2.0      
    ## [25] tidyselect_1.1.2 glue_1.6.2       R6_2.5.1         fansi_1.0.3     
    ## [29] vroom_1.5.7      rmarkdown_2.14   tzdb_0.3.0       magrittr_2.0.3  
    ## [33] scales_1.2.0     ellipsis_0.3.2   htmltools_0.5.2  assertthat_0.2.1
    ## [37] colorspace_2.0-3 utf8_1.2.2       stringi_1.7.6    munsell_0.5.0   
    ## [41] crayon_1.5.1
