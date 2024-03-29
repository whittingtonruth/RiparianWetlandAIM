% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverweightedmeans.R
\name{coverweightedmean}
\alias{coverweightedmean}
\alias{cwm_metrics}
\title{Calculate cover-weighted averages of numeric plant traits}
\usage{
coverweightedmean(cover_species, planttraits)

cwm_metrics(cover_species, header, masterspecieslist)
}
\arguments{
\item{cover_species}{A tall/long-format data frame. Use the data frame from the \code{pct_AbsoluteSpeciesCover} output, then join desired species traits.}

\item{planttraits}{character vector. The field name(s) of numeric plant traits found in the \code{masterspecieslist} which will be used to calculate a cover-weighted mean.}

\item{masterspecieslist}{Data frame. The centrally managed master species list should be used.}
}
\value{
Data frame of a cover weighted mean (CWM) of plant traits selected for each plot found in the \code{cover_species} dataframe.
}
\description{
Function used to calculate a cover-weighted average (or CWM) of given plant traits for species found on plot. Nonvascular species are not included in cover weighted averages.
}
