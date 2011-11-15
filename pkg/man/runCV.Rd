\name{runCV}
\alias{runCV}

\title{Running CV}
\description{
  Compute a running CV using the fts library}


\usage{actogram(x, datetime, periods, ...)
}

\arguments{
     \item{x}{a numeric vector}
     \item{datetime}{a \code{\link{as.POSIXct}} vector}
     \item{periods}{integer: number of periods in window, see \code{\link{moving.sd}}}
}	 

\details{
none
}


\examples{
setseed(111222333)
require(actogram)
d = data.frame(x = abs(unlist(lapply(1:10, function(x) rnorm(sd = sample(c(1, 5, 10), 1), mean = 2, n = 1000))))+1,
			datetime = seq.POSIXt(from =Sys.time(), to = Sys.time()+3600*24*10,  length.out = length(x) ) )

actogram(abs(x) ~ datetime, d)

d1 = with(d, runCV(x, datetime, periods = 5))

actogram( CV~ datetime, d1)

d1$act = ifelse(d1$CV > median(d1$CV), 1, 0)

actogram( act ~ datetime, d1)

}


	

\author{
Author:  Mihai Valcu
}






















