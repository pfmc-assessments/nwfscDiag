# nwfscDiag: Diagnostic Package for West Coast Groundfish Assessments

The package provides the functionality to conduct model diagnostics for SS models.  The standard diagnostic included in this package are standard required analysis for West Coast Groundfish stock assessments.  The package was designed to perform model diagnostics and create plots and tables in a standardized format. The standardized approach will facilitate the use of these outputs in the assessment template approach [sa4ss](https://github.com/nwfsc-assess/sa4ss).

The diagnostics created by the package are:
- jitter runs to ensure model convergence at the MLE
- retrospective runs to examine model sensitivity to recent data 
- likelihood profiles across parameters 

## Installation

nwfscDiag can be installed via github:
```
install.packages("remotes")
remotes::install_github("nwfsc-assess/nwfscDiag")
```
## Running the code
The package depends upon a few other packages and they should be installed upon installation of the package. The dependant packages are:
```
install.packages('dplyr')
remotes::install_github('r4ss/r4ss')
```
A new version of r4ss package was released on July 29, 2022. This release included some significant changes that included changes to function names and function inputs.  The current version of the nwfscDiag 1.1.1 package is designed to work with the latest release of r4ss. Please see release version 1.0.1 to use earlier versions of r4ss. 

## Reporting problems

Please report any issues with this package by posting a new github issue at <https://github.com/nwfsc-assess/nwfscDiag/issues>. 
