\name{map.scale}

\alias{map.scale}

\title{British Isles Coastline map scale}

\description{Function for drawing a simple scale on a blighty map}

\usage{map.scale(pos="AUTO", width=1)}

\arguments{
\item{pos}{Can be \code{AUTO} (default), \code{select}, or a vector with the coordinates for the \emph{centre} of the scale.}
\item{width}{Integer value for the width of the bar.}
}

\value{No return value}

\author{David Lucy <\email{dlucy@maths.ed.ac.uk}>\url{http://www.maths.ed.ac.uk/~dlucy/}}

\note{This function draws a simple bar for a scale with the length of the bar in km beneath it. \code{AUTO} (default) draws the scale in the North-East corner of the map. \code{select} prompts for locator to allow the user to place the \emph{centre} of the scale manually. If you send a vector then the vector gives the position of the \emph{centre} of the scale.

You can't use this with other high level plotting commands (such as \code{oz}) because \code{blighty()} stores the mapping parameters from it's last invocation in an object called \code{blighty.mapinfo} from which the position, and size, of the scale is calculated.}

\seealso{\code{\link{blighty}} \code{\link{north.pointer}} \code{\link[oz]{oz}}}

\examples{
blighty()			# draw coastline
map.scale(pos="AUTO", width=2)	# automatic placing
map.scale(width=2)		# same as above
\dontrun{map.scale(pos="select")}	  # locator to select
map.scale(pos=c(400,400))	# position vector}

\keyword{hplot}
