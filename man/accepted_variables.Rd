\name{accepted_variables}

\alias{accepted_variables}

\title{
Get accepted variables
}

\description{
Get a data.frame with accepted variable names
}

\usage{
accepted_variables(type, group, voc="reagro/terminag")
}
 
\arguments{
  \item{type}{character. Either "records" (for the actual data) or "metadata" (for the metadata)}
  \item{group}{character. The group name}
  \item{voc}{character. path to the github repo of the vocabulary. E.g. "reagro/terminag"}
}

\value{
character
}

