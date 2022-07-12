library(testthat)
library(r4ss)
library(nwfscDiag)

# Use "extdata" in "inst" because its loaded with R packages
example_path <- system.file("extdata", package = "nwfscDiag")

test_check("nwfscDiag")