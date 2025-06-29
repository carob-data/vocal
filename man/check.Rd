\name{check}

\alias{check_variables}
\alias{check_values}
\alias{check_date}
\alias{check_span}
\alias{check_datespan}
\alias{check_lonlat}
\alias{check_caps}

\title{
Functions for compliance checking
}

\description{
Functions for compliance checking
}


\usage{
check_variables(x, trms, required=TRUE)
check_values(x, trms)
check_lonlat(x)
check_caps(x, vars, minchar=5, frac=0)
check_span(x, start, end, smin=0, smax=366)
check_date(x, name, trms=NULL)
check_datespan(x, startdate, enddate, smin=0, smax=366)
}
 
\arguments{
  \item{x}{data.frame to check}
  \item{trms}{data.frame with the vocabulary. E.g. \code{accepted_values("github:carob-data/terminag")}}
  \item{required}{logical. If \code{TRUE} the presence of required variables is checked}
  \item{name}{character. The variable name. For example, "planting_date"}
  \item{vars}{character. The variables to check}
  \item{minchar}{positive integer}
  \item{frac}{numeric}
  \item{start}{numeric}
  \item{end}{numeric}
  \item{smin}{numeric}
  \item{smax}{numeric}
  \item{startdate}{Date}
  \item{enddate}{Date}
}

\value{
data.frame
}

