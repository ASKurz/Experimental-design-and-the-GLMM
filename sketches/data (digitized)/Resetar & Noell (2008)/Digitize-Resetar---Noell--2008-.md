Digitize Resetar & Noell (2008)
================
A Solomon Kurz
2022-11-14

The purpose of this file is to digitize the data from Resetar & Noell
(2008; <https://doi.org/10.1901/jaba.2008.41-447>). We don’t have their
actual data file, but they displayed their data in Figure 1 (p. 450).

Load the **juicr** package.

``` r
library(juicr)
```

Use the `juicr::GUI_juicr()` function to digitize the data, one
participant at a time, from Figure 1.

``` r
GUI_juicr("pics/count01.png", figureWindowSize = c(1000, 400))
GUI_juicr("pics/count02.png", figureWindowSize = c(1000, 400))
GUI_juicr("pics/count03.png", figureWindowSize = c(1000, 400))
GUI_juicr("pics/count04.png", figureWindowSize = c(1000, 400))
```

The digitized data were saved in a series of `.csv` files named
`count01_juicr_extracted_points.csv` through
`count04_juicr_extracted_points.csv` within the `Resetar & Noell (2008)`
folder.

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
    ## [1] juicr_0.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] rstudioapi_0.13     knitr_1.40          magrittr_2.0.3      BiocGenerics_0.42.0 EBImage_4.38.0     
    ##  [6] lattice_0.20-45     jpeg_0.1-9          rlang_1.0.6         fastmap_1.1.0       stringr_1.4.1      
    ## [11] tcltk_4.2.0         tools_4.2.0         grid_4.2.0          xfun_0.33           png_0.1-7          
    ## [16] cli_3.4.1           htmltools_0.5.3     yaml_2.3.5          abind_1.4-5         digest_0.6.30      
    ## [21] htmlwidgets_1.5.4   fftwtools_0.9-11    bitops_1.0-7        RCurl_1.98-1.8      evaluate_0.18      
    ## [26] rmarkdown_2.16      tiff_0.1-11         stringi_1.7.8       compiler_4.2.0      XML_3.99-0.10      
    ## [31] locfit_1.5-9.6
