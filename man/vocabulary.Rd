\name{vocabulary}

\alias{check_vocabulary}
\alias{get_vocabulary}
\alias{set_vocabulary}

\title{
Set a vocabulary, or get its name. Or check if it is up-to-date
}

\description{
Set the vocabulary, or get its name. Or check if it is up-to-date
}


\usage{
set_vocabulary(voc)
get_vocabulary()
check_vocabulary(update=TRUE, force=FALSE, quiet=TRUE)
}
 
\arguments{
  \item{voc}{character. A vocabulary name such as "github:carob-data/terminag"}
  \item{update}{logical. Shoud the vocabulary be updated if it out-of-date?}
  \item{force}{logical. If \code{FALSE} only one check is done per session}
  \item{quiet}{logical. If \code{TRUE} messages are not printed}
}

\value{
character
}

\examples{
set_vocabulary("github:carob-data/terminag")
get_vocabulary()

check_vocabulary(update=FALSE)
}

