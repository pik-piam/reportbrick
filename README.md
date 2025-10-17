# Reporting package for BRICK

R package **reportbrick**, version **0.12.4**

[![CRAN status](https://www.r-pkg.org/badges/version/reportbrick)](https://cran.r-project.org/package=reportbrick) [![R build status](https://github.com/pik-piam/reportbrick/workflows/check/badge.svg)](https://github.com/pik-piam/reportbrick/actions) [![codecov](https://codecov.io/gh/pik-piam/reportbrick/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/reportbrick) [![r-universe](https://pik-piam.r-universe.dev/badges/reportbrick)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

This package contains BRICK-specific routines to report model results. The main functionality is to generate a mif-file from a given BRICK model run folder.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("reportbrick")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Robin Hasse <robin.hasse@pik-potsdam.de>.

## Citation

To cite package **reportbrick** in publications use:

Hasse R, Rosemann R (2025). "reportbrick: Reporting package for BRICK - Version 0.12.4."

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {reportbrick: Reporting package for BRICK - Version 0.12.4},
  author = {Robin Hasse and Ricarda Rosemann},
  date = {2025-10-17},
  year = {2025},
}
```
