\name{blighty}

\alias{blighty}

\title{British Isles Coastlines}

\description{Function for drawing the coastlines of the British Isles}

\usage{blighty(place, grid=FALSE, xlimits, ylimits, xpadding=0, ypadding=0, parcol, parbor, parwdh, sarcol, sarbor, sarwdh, parang, parden, sarang, sarden, tlncol, tlnwdh, grdcol, grdwdh)}

\arguments{
\item{place}{map set - a list of objects to plot - see notes - default - \code{"set.UK"}}
\item{grid}{\code{TRUE} or \code{FALSE} - places a grid in OS km over the map - default - \code{FALSE}}
\item{xlimits}{vector of limits in OS km in which to draw the map}
\item{ylimits}{vector of limits in OS km in which to draw the map}
\item{xpadding}{kms to leave to either side between the edges and map in the \emph{x} direction - default = 0}
\item{ypadding}{kms to leave to either side between the edges and map in the \emph{y} direction - default = 0}
\item{parcol}{primary area colour - \code{"transparent"} for no shading - default - \code{par("bg")}}
\item{parbor}{primary area colour - \code{"transparent"} for invisible - default - \code{par("fg")}}
\item{parwdh}{primary area border width - default 1}
\item{sarcol}{secondary area colour - \code{"transparent"} for no shading - default - \code{par("bg")}}
\item{sarbor}{secondary area colour - \code{"transparent"} for invisible - default - \code{par("fg")}}
\item{sarwdh}{secondary area border width - default 1}
\item{parang}{angle of hatching lines for primary areas - default \code{NULL} - \code{parden} must be set to positive if hatching is to appear - look at \code{\link[base]{polygon}} for further information}
\item{parden}{density of hatching lines for primary areas in lines per inch - default - \code{NULL} - \code{parden} must be set to positive if hatching is to appear - look at \code{\link[base]{polygon}} for further information}
\item{sarang}{angle of hatching lines for secondary areas - default \code{NULL} - \code{parden} must be set to positive if hatching is to appear - look at \code{\link[base]{polygon}} for further information}
\item{sarden}{density of hatching lines for secondary areas in lines per inch - default - \code{NULL} - \code{parden} must be set to positive if hatching is to appear - look at \code{\link[base]{polygon}} for further information}
\item{tlncol}{colour of lines for non-area objects - default - \code{par{"fg"}}}
\item{tlnwdh}{width of lines for non-area objects - default = 1} 
\item{grdcol}{colour of grid lines - default - \code{par{"fg"}}}
\item{grdwdh}{width of grid lines - default = 1}
}

\value{returns the object \code{blighty.mapinfo} in the global environment to give information for \code{\link{north.pointer}} and  \code{\link{map.scale}}}

\author{David Lucy <\email{dlucy@maths.ed.ac.uk}>\url{http://www.maths.ed.ac.uk/~dlucy/}}

\note{
Inspired by the package \code{\link[oz]{oz}}, \code{blighty()} draws the coastline of many of the British Isles. The output is suitable for general purpose illustrations, distribution diagrams; it is not heavyweight GIS or meant for calculation in any way.

The original coordinates were taken from a scan of a map of England, Scotland and Wales in my 1936 edition of \emph{The World of Wonder} to avoid any copyright problems. The scan was then imported into \bold{xfig} and the coastline and internal boundarys were digitised using the polyline function. Each section of coastline was then saved as seperate file. A small amount of hand editing of each file allows them to be read in as coordinate points in \bold{R} (functions availible from the author and in the /misc directory of the package). The problem with using such an early work from which to take the primary image is that bits which were present then may not be now as coastal errosion may have affected the East of England.

The coordinate system was taken from various easily recognisable points of the UK coastline. The calculated error on these is in 100s of metres, the real error from comparison is about 3km. Still, most features for which a grid reference from an Ordinance Survey map can be plotted on easily.

Unfortunately \emph{The World of Wonder} did not include a map of Ireland or the Northern Isles, however, the Irish will be delighted to know that I got some suitable maps from a tourest guide to Ireland which will be incorperated into the next release as soon as I can get a grid system to tie Ireland into mainland Britain. For \code{blighty_2-0.0} the Orkneys and Shetlands  added. They can be displayed using \code{"set.Scotland.all"}. 

Don't use the \code{xlimits} and \code{ylimits} to zoom in too far as the point resolution isn't really up to it. There is now a warning for this - basically the maps start to look bad at anything less than 200km.

It can also take a few seconds when drawing all of the British Isles, this is mainly because of the west coast of Scotland and Western Isles which because of their shape take up loads of data points.

For \code{blighty_2-0.0} the maps have been hand edited to tidy them up a lot, the Scilly Isles and Lundy have been added, various inconsistencies have been fixed, and the examples changed.

At the request of numerous people there are now a set of primitive tools in the package \code{\misc} directory to enable people to construct and use their own maps. To get this download the source \code{blighty_x.y-z.tar.gz} to a directory, \code{tar -zxvf} it, then look in the \code{blighty_x.y-z.tar.gz/misc} directory - although it is part of the distribution it is not installed as such, therefore needs manual unpacking.

New functions which draw scales and North pointers have also been added as from \code{blighty_2-0.0}.

Basically blighty is an elborate wrapper for \code{\link[base]{polygon}} and \code{\link[base]{points}} with some data files added. As such it will plot more or less any sets of coordinate points - so is by no means restricted to the distributed objects. At the moment it has three object types (although more can easily be added), one is a primary area - used for a landmass, the secondary area is a shaope within the landmass, such as a lake, the tertiary feature type is a non-area type such as a river. The type of feature is encoded into the data files, \code{blighty()} then knows how to treat the object. Unfortunately secondary area objects will not cut out an area of the primary objects - so unless the parent primary area object is \code{"transparent"}, the secondary area object can be \code{"transparent"}, but it will merely show the primary area colour. It would be good to rectify this, but I can see no way at the moment of doing so with the present data structure.

Additional place sets are: \code{"set.all"}, \code{"set.England"}, \code{"set.Wales"}, \code{"set.Scotland"} and \code{"set.Scotland.all"}, although it is simple to specify your own - see additional documentation in \code{\misc} directory.

}

\seealso{\code{\link{map.scale}} \code{\link{north.pointer}} \code{\link[oz]{oz}}}

\examples{
blighty()		# UK coastline
blighty(grid=TRUE)	# UK coastline with grid
points(399,91)		# add in Poole OS coords last digit is 1km
text(400,50,"Poole")

blighty("set.Scotland")	# Scotland and the Western Isles
points(327, 672)	# OS coords for Authurs Seat
text(327, 655, "Edinburgh")

blighty("set.Wales")	# Wales and Anglesey

blighty("set.England", xlimits=c(400,600), ylimits=c(50, 200))
box()			# south of England and the Isle of Wight
			# and the Thames Estuary


blighty()		# do a plot then add the capital cities

x <- c(532, 327, 317)	# make up two vectors of points
y <- c(181, 672, 175)	# for capital cities

names <- c("London", "Edinburgh", "Cardiff") # vector of names
 
points(x,y, col="red")	# add in the points in red
 
text(x + 20, y, labels=names, adj=0)
			# add some labels displaced by 20km to the right

blighty(parcol="gray")	# use a basic shading

blighty("set.Scotland", parcol="blue", parbor="red", sarcol="red", tlncol="green", tlnwdh=3)
			# looks hideous but demonstates some of the shading
			# functions

blighty(parang=45, parden=30, parbor="transparent", parcol="black", sarbor="transparent", tlncol="transparent")
			# more of a picture for logos etc
}
\keyword{hplot}
