# MAgPIE outputs R package for MAgPIE version 4.x

R package **m4fsdp**, version **0.1**

[![CRAN status](https://www.r-pkg.org/badges/version/magpie4)](https://cran.r-project.org/package=magpie4) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158582.svg)](https://doi.org/10.5281/zenodo.1158582) [![R build status](https://github.com/pik-piam/magpie4/workflows/check/badge.svg)](https://github.com/pik-piam/magpie4/actions) [![codecov](https://codecov.io/gh/pik-piam/magpie4/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/magpie4) [![r-universe](https://pik-piam.r-universe.dev/badges/magpie4)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

Output routines for extracting results from the MAgPIE framework (versions 4.x) for the FSDP project.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("magpie4")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Benjamin Leon Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

To cite package **m4fsdp** in publications use:

Bodirsky B, Humpenoeder F, Dietrich J (2022). _m4fsdp: MAgPIE outputs R package for MAgPIE version 4.x_ for the FSDP project. doi: X (URL: X), R package version 0.1, <URL: https://github.com/pik-piam/m4fsdp>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {magpie4: MAgPIE outputs R package for MAgPIE version 4.x for the FSDP project},
  author = {Benjamin Leon Bodirsky Jan Philipp Dietrich},
  year = {2022},
  note = {R package version 0.1},
  doi = {X},
  url = {https://github.com/pik-piam/m4fsdp},
}
```
