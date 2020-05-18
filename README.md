[![Build Status](https://travis-ci.org/itfeature/Week4Rpackage.svg?branch=master)](https://travis-ci.org/itfeature/Week4Rpackage)

# Build R Project Week 4 - Coursera
By Muhammad imdad Ullah

This R package is the Week 4 final assignment for the **Building R Packages** on Coursera

Date: March 18-May-2020

## Installation

To install this package to run on your system, please first install and load the devtools package. Then you may install and load this package:

```{r}
install_github('itfeature/Week4Rpackage' , build_vignettes = TRUE)
library(week4)
```

## Vignette

If you install with build_vignettes = TRUE, then you can read the introduction vignette: 
```{r}
vignette('week4', package = 'week4')
```
