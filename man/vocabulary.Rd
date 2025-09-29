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
set_vocabulary(name, update=TRUE, force=FALSE, quiet=FALSE)
get_vocabulary()
check_vocabulary(update=TRUE, force=FALSE, delay=0, quiet=FALSE)
}
 
\arguments{
  \item{name}{character. A vocabulary name such as "github:controvoc/terminag"}
  \item{update}{logical. Shoud the vocabulary be updated if it is out-of-date?}
  \item{force}{logical. If \code{FALSE} only one check is done per session}
  \item{delay}{non-negative number. The minimum number of hours between updates}
  \item{quiet}{logical. If \code{TRUE} messages are not printed}
}

\value{
character
}

\examples{
set_vocabulary("github:controvoc/terminag")
get_vocabulary()

check_vocabulary(update=FALSE)
}

