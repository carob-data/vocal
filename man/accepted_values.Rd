\name{accepted_values}

\alias{accepted_values}

\title{
Get accepted values for a variable
}

\description{
Get the accepted values for a variable. 
}


\usage{
accepted_values(name, voc="carob-data/terminag")
}
 
\arguments{
  \item{name}{character. The variable name. For example, "country", "crop", or "fertilizer_type"}
  \item{voc}{character. path to the github repo of the vocabulary. E.g. "carob-data/terminag"}
}

\value{
character
}

\examples{
v <- accepted_values("fertilizer_type")
head(v)
}