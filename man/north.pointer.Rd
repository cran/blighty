\name{north.pointer}

\alias{north.pointer}

\title{British Isles Coastline North pointer}

\description{Function for drawing a north pointer on a blighty map}

\usage{north.pointer(pos="AUTO")}

\arguments{
\item{pos}{Can be \code{AUTO}, \code{select}, or a vector with the coords. \code{AUTO} places the North pointer in the North-East corner of the map. \code{select} allows the user to select the centre of the \emph{N} manually. Otherwise a vector of coordinates can be sent.
}}

\value{No return value}

\author{David Lucy <\email{d.lucy@lancaster.ac.uk}>\url{http://www.maths.lancs.ac.uk/~lucy/}}

\note{Draws a simple North pointer on a map. The postion refers to the centre of the \emph{N} on the pointer.

You can't use this with other high level plotting commands (such as \code{oz}) because \code{blighty()} stores the mapping parameters from it's last invocation in an object called \code{blighty.mapinfo} from which the position, and size, of the scale is calculated.}

\seealso{\code{\link{map.scale}} \code{\link{blighty}} \code{\link[oz]{oz}}}

\examples{
blighty()				# draw coastline
north.pointer(pos="AUTO")		# automatic placing
north.pointer()				# same as above
\dontrun{north.pointer(pos="select")}	# uses locator to find the position
north.pointer(pos=c(500,500))		# vector to determine the centre of the
					# N of the pointer
}
\keyword{hplot}
