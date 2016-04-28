\name{create.epochs}
\alias{create.epochs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.epochs(data, sample.rate, epoch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{sample.rate}{
%%     ~~Describe \code{sample.rate} here~~
}
  \item{epoch}{
%%     ~~Describe \code{epoch} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, sample.rate, epoch) 
{
    data$Time2 <- as.numeric(data$Video.Time)
    sample.rate <- sample.rate
    epoch <- epoch
    epoch1 <- epoch * sample.rate
    epoch2 <- (epoch * 2) * sample.rate
    epoch3 <- (epoch * 3) * sample.rate
    epoch4 <- (epoch * 4) * sample.rate
    data$epoch <- ifelse(data$Time2 < epoch1, "15s", ifelse((data$Time2 > 
        epoch1 & data$Time2 <= epoch2), "30s", ifelse((data$Time2 > 
        epoch2 & data$Time2 <= epoch3), "45s", ifelse((data$Time2 > 
        epoch3 & data$Time2 <= epoch4), "60s", ifelse(data$Time2 > 
        epoch4, "60s+", NA)))))
    data$epoch <- as.factor(data$epoch)
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
