\name{read.facereader}
\alias{facereadeR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read.facereader
}
\description{
This function reads in FaceReader 6.x text files, strips excess front matter, and aggragates them into a single data frame for further analysis.
}
\usage{
read.facereader(path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
The complete file path to location of your text files.
}
}
\details{
The file path (\code{path}) should lead to a directory that contains all of the text files that will be read into the R workspace.
}
\value{
Returns aggregated and stripped data to be further processed with \code{\link{clean.facereader}} functions.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Daniel N. Albohn
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{clean.facereader}}
}
\examples{
data <- read.facereader("C:/My/Hard/Drive/Data/")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
