# MAgPIE outputs R package for MAgPIE version 4.x to create outputs for FSDP project

R package **m4fsdp**, version **0.9.6**

[![CRAN status](https://www.r-pkg.org/badges/version/m4fsdp)](https://cran.r-project.org/package=m4fsdp)  [![R build status](https://github.com/pik-piam/m4fsdp/workflows/check/badge.svg)](https://github.com/pik-piam/m4fsdp/actions) [![codecov](https://codecov.io/gh/pik-piam/m4fsdp/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/m4fsdp) [![r-universe](https://pik-piam.r-universe.dev/badges/m4fsdp)](https://pik-piam.r-universe.dev/ui#builds)

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
install.packages("m4fsdp")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Benjamin Leon Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

To cite package **m4fsdp** in publications use:

Bodirsky B, Dietrich J, Humpenoeder F (2022). _m4fsdp: MAgPIE outputs R package for MAgPIE version 4.x to create outputs for FSDP project_. doi:? <https://doi.org/%3F>, R package version 0.9.6, <https://github.com/pik-piam/m4fsdp>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {m4fsdp: MAgPIE outputs R package for MAgPIE version 4.x to create outputs for FSDP project},
  author = {Benjamin Leon Bodirsky and Jan Philipp Dietrich and Florian Humpenoeder},
  year = {2022},
  note = {R package version 0.9.6},
  doi = {?},
  url = {https://github.com/pik-piam/m4fsdp},
}
```
