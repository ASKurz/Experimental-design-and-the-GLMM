clarke2021
================
A Solomon Kurz
2022-10-20

The purpose of this file is to reformat the data from Clarke et al
(2021; <https://doi.org/10.1111/add.15072>). The original data are
openly available on the OSF at <https://osf.io/pr8zu/>, and they were
downloaded for this project on June 27, 2022.

Load the **tidyverse**.

``` r
library(tidyverse)
```

Upload the data file for the primary outcomes as reported in the Results
section.

``` r
d <- readxl::read_excel("Clarke et al (2021)/Alcohol study 2 full dataset.xlsx",
                        na = "-999")

# what is this?
glimpse(d)
```

    ## Rows: 6,024
    ## Columns: 28
    ## $ PID                                                                               <dbl> …
    ## $ Date                                                                              <dttm> …
    ## $ Condition                                                                         <dbl> …
    ## $ `Which alcohol do you prefer?`                                                    <dbl> …
    ## $ `Primary outcome`                                                                 <dbl> …
    ## $ `NEA TOTAL`                                                                       <dbl> …
    ## $ `NEA Average`                                                                     <dbl> …
    ## $ Reactance                                                                         <dbl> …
    ## $ Avoidance                                                                         <dbl> …
    ## $ `Disease Total`                                                                   <dbl> …
    ## $ `Acceptability condition`                                                         <dbl> …
    ## $ Acceptability                                                                     <dbl> …
    ## $ `Time pressure condition`                                                         <dbl> …
    ## $ `Time task - alcoholic products selected`                                         <dbl> …
    ## $ Age                                                                               <dbl> …
    ## $ Gender                                                                            <dbl> …
    ## $ Ethnicity                                                                         <dbl> …
    ## $ BMI                                                                               <dbl> …
    ## $ `BMI score`                                                                       <dbl> …
    ## $ Education                                                                         <dbl> …
    ## $ Income                                                                            <chr> …
    ## $ `How often do you have a drink containing alcohol?`                               <dbl> …
    ## $ `How many units of alcohol do you drink on a typical day when you are drinking?`  <dbl> …
    ## $ `How often have you drunk 6 or more units on a single occasion in the last year?` <dbl> …
    ## $ `AUDIT score`                                                                     <dbl> …
    ## $ `Total alcohol consumed UNITS`                                                    <dbl> …
    ## $ `Weekly consumption category`                                                     <dbl> …
    ## $ `Response Type Multi-Value Text Set`                                              <chr> …

``` r
head(d)
```

    ## # A tibble: 6 × 28
    ##     PID Date                Condition Which al…¹ Prima…² NEA T…³ NEA A…⁴ React…⁵
    ##   <dbl> <dttm>                  <dbl>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1     1 2019-01-30 03:23:20         4          1       0      16    4          5
    ## 2     2 2019-01-30 03:30:31         2          2       0      20    5          4
    ## 3     7 2019-01-30 08:38:16         3          1       1       4    1          4
    ## 4     3 2019-01-30 08:39:22         1          1       0      19    4.75       1
    ## 5     9 2019-01-30 08:42:08         4          2       1      15    3.75       6
    ## 6     8 2019-01-30 08:43:15         3          2       1      22    5.5        7
    ## # … with 20 more variables: Avoidance <dbl>, `Disease Total` <dbl>,
    ## #   `Acceptability condition` <dbl>, Acceptability <dbl>,
    ## #   `Time pressure condition` <dbl>,
    ## #   `Time task - alcoholic products selected` <dbl>, Age <dbl>, Gender <dbl>,
    ## #   Ethnicity <dbl>, BMI <dbl>, `BMI score` <dbl>, Education <dbl>,
    ## #   Income <chr>, `How often do you have a drink containing alcohol?` <dbl>,
    ## #   `How many units of alcohol do you drink on a typical day when you are drinking?` <dbl>, …

The names in the `.xlsx` file are descriptive, but cumbersome for
programming. Here we rename several of the variables to follow the
tidyverse style (see
<https://style.tidyverse.org/syntax.html#object-names>).

``` r
d <- d %>% 
  rename(age     = Age,
         alcohol = `Primary outcome`,
         audit   = `AUDIT score`)
```

The original participant identification indicator `PID` is undesirably
non-sequential. Here we make a new sequential `id` variable.

``` r
d <- d %>% 
  mutate(id = 1:n())
```

The four numerals in the `Gender` might be better expressed as levels in
a factor variable.

``` r
d <- d %>% 
  mutate(gender = factor(Gender,
                         levels = 1:4,
                         labels = c("Male", "Female", "Other", "Prefer not to say")))
```

The four experimental conditions are encoded in the numeric `Condition`
variable. Here we add a complimentary `hwl` factor variable to provide a
more descriptive account. We then add two dummy variables, `image` and
`text`.

``` r
d <- d %>%
  mutate(hwl = case_when(
    Condition == 1 ~ "control",
    Condition == 2 ~ "text only",
    Condition == 3 ~ "image only",
    Condition == 4 ~ "image and text")) %>% 
  mutate(hwl = factor(hwl, levels = c("control", "image only", "text only", "image and text")))  %>% 
  mutate(image = ifelse(hwl %in% c("image only", "image and text"), 1, 0),
         text  = ifelse(hwl %in% c("text only", "image and text"), 1, 0))
```

Though the four levels of the `Condition` variable follow a certain
pleasing logic, Clarke and colleagues described the experimental
conditions as Group 1 through Group 4 in the Results of the paper (e.g.,
Table 3 on page 47). Critically, their Group 1 corresponds to
`Condition == 4` in the data and their Group 4 corresponds to
`Condition == 1`. Here we make that ordering adjustment for a new
variable called `group`.

``` r
d <- d %>% 
  mutate(group = recode(Condition,
                        `1` = 4, 
                        `4` = 1))
```

Now subset the data file and save the results as `clarke2021`.

``` r
clarke2021 <- d %>% 
  select(id, age, gender, audit, alcohol, group, hwl, image, text)

# what?
head(clarke2021)
```

    ## # A tibble: 6 × 9
    ##      id   age gender audit alcohol group hwl            image  text
    ##   <int> <dbl> <fct>  <dbl>   <dbl> <dbl> <fct>          <dbl> <dbl>
    ## 1     1    90 Female     8       0     1 image and text     1     1
    ## 2     2    88 Female     9       0     2 text only          0     1
    ## 3     3    34 Male       3       1     3 image only         1     0
    ## 4     4    54 Male       7       0     4 control            0     0
    ## 5     5    45 Male      10       1     1 image and text     1     1
    ## 6     6    40 Male       7       1     3 image only         1     0

## Save

Save the results in an external `.rda` file.

``` r
save(clarke2021, file = "clarke2021.rda")
```

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
    ## [1] forcats_0.5.1   stringr_1.4.1   dplyr_1.0.10    purrr_0.3.4    
    ## [5] readr_2.1.2     tidyr_1.2.1     tibble_3.1.8    ggplot2_3.3.6  
    ## [9] tidyverse_1.3.2
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.2    xfun_0.33           haven_2.5.1        
    ##  [4] gargle_1.2.0        colorspace_2.0-3    vctrs_0.4.1        
    ##  [7] generics_0.1.3      htmltools_0.5.3     yaml_2.3.5         
    ## [10] utf8_1.2.2          rlang_1.0.6         pillar_1.8.1       
    ## [13] withr_2.5.0         glue_1.6.2          DBI_1.1.3          
    ## [16] dbplyr_2.2.1        modelr_0.1.8        readxl_1.4.1       
    ## [19] lifecycle_1.0.2     munsell_0.5.0       gtable_0.3.1       
    ## [22] cellranger_1.1.0    rvest_1.0.2         evaluate_0.16      
    ## [25] knitr_1.40          tzdb_0.3.0          fastmap_1.1.0      
    ## [28] fansi_1.0.3         broom_1.0.1         scales_1.2.1       
    ## [31] backports_1.4.1     googlesheets4_1.0.1 jsonlite_1.8.0     
    ## [34] fs_1.5.2            hms_1.1.1           digest_0.6.29      
    ## [37] stringi_1.7.8       grid_4.2.0          cli_3.4.0          
    ## [40] tools_4.2.0         magrittr_2.0.3      crayon_1.5.1       
    ## [43] pkgconfig_2.0.3     ellipsis_0.3.2      xml2_1.3.3         
    ## [46] reprex_2.0.2        googledrive_2.0.0   lubridate_1.8.0    
    ## [49] assertthat_0.2.1    rmarkdown_2.16      httr_1.4.4         
    ## [52] rstudioapi_0.13     R6_2.5.1            compiler_4.2.0
