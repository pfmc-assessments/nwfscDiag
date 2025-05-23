% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_jitter.R
\name{run_jitter}
\alias{run_jitter}
\title{Run \link[r4ss:jitter]{r4ss::jitter} based on \code{model_settings}}
\usage{
run_jitter(mydir, model_settings)
}
\arguments{
\item{mydir}{Directory where model files are located.
There is no default entry for \code{mydir}.
An example of user input could be
\code{mydir = file.path("C:/my_models", "base_model")}.}

\item{model_settings}{A list of all model settings.
Typically, created using \link{get_settings} but users can create their own list,
as long as it has all of the necessary components for the function at hand.}
}
\value{
A list of quantities across jitters called \code{jitter_output}
}
\description{
Code to run jitters for a model
Output will be saved in an Rdata object called "jitter_output"
}
\author{
Chantel Wetzel
}
