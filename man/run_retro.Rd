% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_retro.R
\name{run_retro}
\alias{run_retro}
\title{Run \link[r4ss:retro]{r4ss::retro} based on \code{model_settings}}
\usage{
run_retro(mydir, model_settings)
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
Nothing is explicitly returned from \code{retro_wrapper}.
The following objects are saved to the disk.
\itemize{
\item \code{mohnsrho.csv} with the following columns:
\itemize{
\item type: the type of Mohn's rho
\itemize{
\item \href{https://academic.oup.com/icesjms/article/56/4/473/624639}{Mohn (1999)},
\item Woods Hole Mohn's rho \href{https://archive.nefmc.org/tech/council_mtg_docs/Sept\%202009/Herring/Doc\%209_Retro\%20Working\%20Group\%20Report.pdf}{(Legault 2009)} used by the \href{https://www.fisheries.noaa.gov/about/northeast-fisheries-science-center}{Northeast Fisheries Science Center (NEFSC)}, and
\item \href{https://doi.org/10.1093/icesjms/fsu198}{Hurtado-Ferro et al. (2015)} used by the \href{https://www.fisheries.noaa.gov/about/alaska-fisheries-science-center}{Alaska Fisheries Science Center (AFSC)}
}
\item Quantity: the stock assessment quantity of interest
\item values: the Mohn's rho values
}
\item A set of figures from \link[r4ss:SSplotComparisons]{r4ss::SSplotComparisons}
\item \code{retrofigures4doc.csv} for use with \code{sa4ss::add_figure} to add retro peels of
spawning stock biomass (\emph{SSB}) and fraction unfished,
complete with captions and alternative text.

\code{apply(utils::read.csv(file.path("..", paste0(mod_loc, "_retro"), "retrofigures4doc.csv")), 1, function(x) do.call(sa4ss::add_figure, as.list(x)))}
\item \code{mohnsrho.tex} for use with \code{sa4ss::read_child()}
inside of an environment with \code{results = "asis"}
to include a table of Mohn's rho values in a document.

\code{sa4ss::read_child(file.path(paste0(params[["model"]], "_retro"), "mohnsrho.tex"))}
\item \code{retro_output.Rdata} with a list of R objects.
}
}
\description{
Create a folder containing retrospective runs for a given base-model folder.
Runs are created using settings specified in \code{model_settings} and
is formatted for inclusion in a document.
}
\seealso{
The following functions interact with \code{retro_wrapper}:
\itemize{
\item \link{run_diagnostics}: calls \code{retro_wrapper}
\item \link[r4ss:retro]{r4ss::retro}: the workhorse of \code{retro_wrapper} that does the peels
}
}
\author{
Chantel Wetzel
}
