# the blighty package by David Lucy - plots the coastline of the British Isles
# sadly doesn't do Ireland as I hadn't a map of it, nor the Northern Isles.

blighty <- function(place="UK", grid=FALSE, xlimits, ylimits)
{

# get the vector of objectnames
objectnames <- getobjectnames()

# assign a vector of objects given the area of the UK selected
switch(place,
"UK" = objects <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27),
"Scotland" = objects <- c(1,5,8,11,13,14,15,16,17,18,19,21,22,23,24,25),
"Wales" = objects <- c(2,10,12),
"England" = objects <- c(1,2,3,4,6,7,9,27),
objects <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,16,27))

# assign a vector of x-ordinates dependent on the area of the UK selected
if(missing(xlimits)){
	switch(place,
	"UK" = xlimits <- c(0, 750),
	"Scotland" = xlimits <- c(0, 450),
	"Wales" = xlimits <- c(150, 400),
	"England" = xlimits <- c(100, 650),
	xlimits <- c(0, 750))}

# assign a vector of y-ordinates dependent on the area of the UK selected
if(missing(ylimits)){
	switch(place,
	"UK" = ylimits <- c(0, 1000),
	"Scotland" = ylimits <- c(500, 1000),
	"Wales" = ylimits <- c(150, 400),
	"England" = ylimits <- c(0, 650),
	ylimits <- c(0, 750))}

# calculate suitable plot limits to keep the map with a more or less
# truly square grid
lims <- sqlimits(xlimits, ylimits)

# setup the plot extremes
par(usr = c(lims$xlims[1], lims$xlims[2], lims$ylims[1], lims$ylims[2]))

# add to the plot all requested objects
	for(ctr in objects)
		{
		data(list = objectnames[ctr])
		points(get(objectnames[ctr]), type="l")
		}

# if gridding has been requested put on the OS coordinates and gridlines and box
if(grid == "TRUE")
	{
	box()
	xmarks <- pretty(lims$xlims)
	ymarks <- pretty(lims$ylims)

	axis(1, at=xmarks, labels=TRUE)
	abline(v=xmarks)
	axis(2, at=ymarks, labels=TRUE)
	abline(h=ymarks)
	}

return("Map complete")
}
