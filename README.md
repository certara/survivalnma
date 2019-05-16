# survivalnma

`survivalnma` is an R package for conducting of Bayesian network meta-analyses of parametric survival curves created at Certara by Witold Wiecek and Savvas Pafitis.

This package is meant to facilitate use of the existing Bayesian NMA 
models which were already presented in (1) and (2). Running such WinBUGS models is 
a slow process and when errors occur, they are hard to understand 
for anyone except the advanced users. Our aim is to make data preparation, 
running of the models and reporting as smooth as possible, to make the inference 
accessible and NMA workflows reproducible.

## Installation

We are considering CRAN release of the package (or its parts). 
The current development version can be downloaded from GitHub and compiled locally. 

```r
devtools::install_github("certara/survivalnma", build_vignettes = TRUE)
library(survivalnma)
```

## Basic example

For `survivalnma` we prepared functions for conducting a single analysis, comparing multiple models, 
convenience tools for data preparation, many NMA-specific plotting function and more. Not all are included in this open-source release, but we will consider them for future releases.

For minimal use case you can try a built-in dataset:

```r
# We read data from some included data files:
nma_df <- data.frame(
  stringsAsFactors = FALSE,
  "treatment" = c("Suni", "Cabo", "Suni", "Pazo"),
  "study" = c("Study 1", "Study 1", "Study 2", "Study 2"),
  "baseline" = c("Suni", "Suni", "Suni", "Suni"),
  "filepath" = sapply(c("CABOSUN_os_suni_KM.txt",
                        "CABOSUN_os_cabo_KM.txt",
                        "Mot_OS_Suni_KM.txt",
                        "Mot_OS_Pazo_KM.txt"), function(x)
                        system.file("extdata", x, package = "survivalnma", mustWork = TRUE)))

# example of simple survivalnma models (fixed effects)
fit_wbl <- survivalnma(nma_df, "weibull") 
```

Typically a survival plot and hazards plot are examined:

```r
fit_wbl 
survival_plot(fit_wbl)
hazard_plot(fit_wbl)
```

## Fully documented NMA workflow

`survivalnma` is meant as a user-friendly introduction to parametric NMA models and 
alternative to working with models more directly in WinBUGS.
We have created a complete vignette covering both theory and practical steps in 
conducting NMA analysis with `suvnma`. In R console try

```r
vignettes('survivalnma_guide')
```

Documentation is also available for all exported functions, starting from `?survnma`.

## References

(1) Ouwens, Mario J. N. M., Zoe Philips, and Jeroen P. Jansen. “Network Meta-Analysis of Parametric Survival Curves.” Research Synthesis Methods 1, no. 3–4 (July 2010): 258–71. https://doi.org/10.1002/jrsm.25

(2) Jansen, Jeroen P. “Network Meta-Analysis of Survival Data with Fractional Polynomials.” BMC Medical Research Methodology 11, no. 1 (May 6, 2011): 61. https://doi.org/10.1186/1471-2288-11-61
