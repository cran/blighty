\name{blighty}

\alias{blighty}

\title{United Kingdom Coastline}

\description{Function for drawing the coastlines of the united Kingdom}

\usage{blighty(place, grid=FALSE, xlimits, ylimits)}

\arguments{
\item{place}{country of the United Kingdom - \code{"Scotland"}, \code{"Wales"}, \code{"England"}, \code{"UK"} - default \code{"UK"}}
\item{grid}{\code{TRUE} or \code{FALSE} - places a grid in OS km over the map - default \code{FALSE}}
\item{xlimits}{vector of limits in OS km in which to draw the map}
\item{ylimits}{vector of limits in OS km in which to draw the map}
}

\value{returns "Map complete" when finished.}

\author{David Lucy <\email{dlucy@maths.ed.ac.uk}> 

\note{

Inspired by the package \code{oz}. The original coordinates were taken from a scan of a map of England, Scotland and Wales in my 1936 edition of \emph{The World of Wonder} to avoid any copyright problems. The scan was then imported into \bold{xfig} and the coastline and internal boundarys were digitised using the polyline function. Each section of coastline was then saved as seperate file. A small amount of hand editing of each file allows them to be read in as coordinate points in \bold{R} (functions availible from the author). The problem with using such an early work from which to take the primary image is that bits which were present then may not be now as coastal errosion may have affected the East of England.

The coordinate system was taken from various easily recognisable points of the UK coastline. The calculated error on these is in 100s of metres, but I suspect the real error is far greater. Still, most features for which a grid reference from an Ordinance Survey map can be plotted on easily.

Unfortunately \emph{The World of Wonder} did not include a map of Ireland or the Northern Isles, so apologies to the Irish, Shetlanders and Orcadians.

There is a slight error when plotting England as a bit of the South-East coast of Scotland gets in there, needs fixing.

Don't use the \code{xlimits} and \code{ylimits} to zoom in too far as the point resolution isn't really up to it.

It can also take a few minutes when drawing the whole of the United Kingdom, this is mainly because of the west coast of Scotland and Western Isles which because of their shape take up loads of data points.
}

\examples{
blighty()		# UK coastline
blighty(grid=TRUE)	# UK coastline with grid
blighty("Scotland")	# Scotland and the Western Isles
blighty("Wales")	# Wales and Anglesey

blighty("England", xlimits=c(400,600), ylimits=c(50, 200))
box()			# south of England and the Isle of Wight
			# and the Thames Estuary}

\keyword{misc}
