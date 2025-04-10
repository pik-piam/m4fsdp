# MAgPIE outputs R package for MAgPIE version 4.x to create outputs
    for FSDP project

R package **m4fsdp**, version **0.58.4**

[![CRAN status](https://www.r-pkg.org/badges/version/m4fsdp)](https://cran.r-project.org/package=m4fsdp) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7899913.svg)](https://doi.org/10.5281/zenodo.7899913) [![R build status](https://github.com/pik-piam/m4fsdp/workflows/check/badge.svg)](https://github.com/pik-piam/m4fsdp/actions) [![codecov](https://codecov.io/gh/pik-piam/m4fsdp/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/m4fsdp) [![r-universe](https://pik-piam.r-universe.dev/badges/m4fsdp)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Output routines for extracting results from the MAgPIE
    framework (versions 4.x) for the FSDP project.


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

Bodirsky B, Chen D, Crawford M, Leip D, von Jeetze P, Beier F, Molina Bacca E, Dietrich J, Humpenoeder F (2025). "m4fsdp: MAgPIE outputs R package for MAgPIE version 4.x to create outputs for FSDP project." doi:10.5281/zenodo.7899913 <https://doi.org/10.5281/zenodo.7899913>, Version: 0.58.4, <https://github.com/pik-piam/m4fsdp>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {m4fsdp: MAgPIE outputs R package for MAgPIE version 4.x to create outputs
    for FSDP project},
  author = {Benjamin Leon Bodirsky and David Chen and Michael Crawford and Debbora Leip and Patrick {von Jeetze} and Felicitas Beier and Edna {Molina Bacca} and Jan Philipp Dietrich and Florian Humpenoeder},
  doi = {10.5281/zenodo.7899913},
  date = {2025-04-10},
  year = {2025},
  url = {https://github.com/pik-piam/m4fsdp},
  note = {Version: 0.58.4},
}
```
