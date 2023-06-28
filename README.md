[![r-cmd-check](https://github.com/pfmc-assessments/nwfscDiag/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/pfmc-assessments/nwfscDiag/actions/workflows/r-cmd-check.yml)

# nwfscDiag: Diagnostic Package for West Coast Groundfish Assessments

The package provides the functionality to conduct model diagnostics for Stock Synthesis (SS3) models.  The standard diagnostic included in this package are standard required analysis for U.S. West Coast Groundfish stock assessments managed by the Pacific Fisheries Management Council. The package was designed to perform model diagnostics and create plots and tables in a standardized format. The standardized approach will facilitate the use of these outputs in the assessment template approach [sa4ss](https://github.com/pfmc-assessments/sa4ss).

The diagnostics created by the package are:
- jitter runs to ensure model convergence at the MLE,
- retrospective runs to examine model sensitivity to recent data, and  
- likelihood profiles across parameters. 

This package does not maintain backward compatibility with previous versions of Stock Synthesis. However, if needed user can download older package versions that may work with older versions (3.30.+) of Stock Synthesis.

## Installation

nwfscDiag can be installed via github:
```
install.packages("remotes")
remotes::install_github("pfmc-assessments/nwfscDiag")
```
## Running the code
The package depends upon a few other packages and they should be installed upon installation of the package. The dependent packages are:
```
install.packages('dplyr')
remotes::install_github('r4ss/r4ss')
```
A new version of r4ss package was released on July 29, 2022. This release included some significant changes that included changes to function names and function inputs.  The current version of the nwfscDiag 1.1.2 package is designed to work with the latest release of r4ss. Please see release version 1.0.1 to use earlier versions of r4ss. 

## Reporting problems

Please report any issues with this package by posting a new github issue at <https://github.com/pfmc-assessments/nwfscDiag/issues>. 
