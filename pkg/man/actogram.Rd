\name{actogram}
\alias{actogram}

\title{Actogram plots}
\description{
  Single and double-plotted conditional actograms}


\usage{actogram(formula, 
					dat, 
					groups, 
					strip.left.classes, 
					strip.left.cex = .8, 
					doublePlot = TRUE,
					strip.left = TRUE,
					rug = FALSE,
					type = "h",					
					scale = 2,
					xaxt = TRUE,
					groups.key = TRUE, 
					xlab = "Hours", 
					ylab = "Activity",
                    layout,	...)
}

\arguments{
     \item{formula}{a formula of form  \emph{y~x}, where 
					\emph{y} is a numeric vector indicating the activity and 
					\emph{x} is a \code{\link{POSIXct}} vector or a character vector which can be converted by
						\code{\link{as.POSIXct}} }
     \item{dat}{a \code{\link{data.frame}} containing activity and time}
     \item{groups}{passed to \code{\link{xyplot}}}
     \item{strip.left.classes}{a grouping factor used to identify the left strip color classes}
     \item{strip.left.cex}{strip.left.cex}
     \item{doublePlot}{a logical vector}
     \item{strip.left}{a logical vector, if \code{TRUE}, default, draw  strips to the left of each panel}
     \item{scale}{a numeric vector of length 1 allowing for downscaling of activity spikes}
     \item{xaxt}{a logical value indicating whether x-axis should be drawn on the plot}
     \item{groups.key}{a logical value indicating the legend associated with groups should be printed}
     \item{xlab}{character string giving label name for the x-axis}
     \item{ylab}{character string giving label name for the y-axis}
	 \item{layout}{vector specifying the graph layout (number of columns, number of rows)}
     \item{...}{further arguments to be passed to \code{\link{xyplot}}}
}	 

\details{
   \code{actogram} is a thin wrapper around xyplot.
 The left strip color is set using  \code{\link{trellis.par.get}("superpose.polygon")$col}
}


\examples{
require(actogram)
data(sesa)
# a simple actogram without left strips
actogram(activity ~ datetime_,  dat = sesa, subset = ID == 2, strip.left = FALSE, main = "actogram")

# left strips indicating the date
actogram(activity ~ datetime_,  dat = sesa, subset = ID == 2, main = "actogram")

# breeding stage information marked by the strip color
actogram(activity ~ datetime_,  strip.left.classes = "breeding_stage", dat = sesa[sesa$ID==2, ], main = "actogram")

# a conditional actogram using all decorations
actogram(activity ~ datetime_,  strip.left.classes = "breeding_stage", groups = "ID" , dat = sesa, main = "conditional actogram")

}	

\author{
Author:  Mihai Valcu \email{valcu@orn.mpg.de},  Marina Lehmann \email{marina.lehmann@uni-konstanz.de}
}






















