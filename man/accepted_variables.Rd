\name{accepted_variables}

\alias{accepted_variables}

\title{
Get accepted variables
}

\description{
Get a data.frame with accepted variable names
}

\usage{
accepted_variables(include=NULL)
}
 
\arguments{
  \item{include}{character or NULL. Can be used to use a subset of variables groups}
}

\value{
data.frame
}

\examples{
set_vocabulary("github:controvoc/terminag")
v <- accepted_variables("crop")
head(v)
}
