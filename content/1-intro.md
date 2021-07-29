---
title: Geospatial Analysis in R
nav: Geospatial Analysis in R
---

We are going to create a simple probability distribution model for the iconic <a href='https://www.youtube.com/watch?v=XjAcyTXRunY'>Alberts Lyrebird</a>
We have point data for sightings of alberts lyrebirds, and we have raster layers for several environmental variables, such as elevation, protection status and vegetation cover.
We will extract the the environmental variable data for each point that a lyrebird was sighted. 
We will then create pseudo false points(i.e. places were lyrebirds were not sighted???) and extract environmental data from those points.
Finally we will create a glm probability curve and project that into a raster that indicated the probability of seeing a lyrebird across our study area.

Although this is a species distribution model, the same principals can be used to model anything across space.

The data for this workshop is located <a href='./data' target='_blank'>here</a>, download all contents including the folders within this link. 

Copy the following R script and save it. Open it up in Rstudio, you will need to change the directory in line 35 getwd() to the directory where you saved the above data.


```R

# Brett Parker, senior functional analyst at eResearch Griffith University
# email: b.parker1@griffith.edu.au


# Our study area spans over 2 states, south east Queensland and the northern rivers of NSW. Like most governments, the data is spread across multiple agencies, and is in different formats.
# So we will need to do some geographical pre-processing on the data

rm(list = ls()) # removes any earlier data that is stored in the working memory
graphics.off() # turns off previous graphical parameters, i.e. (par(mfrow = ))

#Make sure the following packages are installed, you can run the following command to install them
# To install a package use the install.packages() command
#install.packages("Package Name")

## load required libaries
require(raster) # good for raster data https://cran.r-project.org/web/packages/raster/raster.pdf
require(rgdal) # general GIS https://cran.r-project.org/web/packages/rgdal/rgdal.pdf
require(rgeos) # good for vector data https://cran.r-project.org/web/packages/rgeos/rgeos.pdf
require(ggplot2) # make visually nice graphs of spatial layers
require(sf) # https://cran.r-project.org/web/packages/sf/sf.pdf
require(tidyr) # general data wrangling


set.seed(1) # provides consistent results for functions/algorithms that have random elements in them, i.e. k-means clustering

## sets the computer path where data is stored, for mac \ works,
# for windows \\ or / works
setwd('~/Documents/R_spatial_workshop/FinalFolder')

#Vectors
## import vector layers "." indicates to look in the current working directory. If you are unsure of your current working directory run the command getwd()
# readOGR is from the rgdal package. Its used to read in vector layers
#Lets read in thew National park's for Qld, its 1 shapefile that contains every Qld NP. NP's will be used to create a binary classification of protected, or not protected.
qldNP <- readOGR("NationalParksQld/." ,"protAreas")
#Lets have a look at the metadata 
qldNP
#Lets have a look at the names of the variables attached to the shapefile
names(qldNP)

#lets read in a csv and transform it into a shapefile
lyrebird <- read.csv("~/Documents/R_spatial_workshop/FinalFolder/lyreBirdOccurance.csv", stringsAsFactors = FALSE) # read in a CSV as a dataframe
lyrebird # It has alot of columns. There are 2 columns, Longitud_1 and Latitude_1 which are the coordinates.
coordinates(lyrebird) <- c('Longitud_1', 'Latitude_1') # Makes the points spatial objects
crs(lyrebird) <- crs('+proj=longlat +datum=WGS84 +no_defs +towgs84=0,0,0') # and puts the points into an appropriate projection



#Lets get the names of the national parks
qldNP$ESTATENAME # It prints all NP's in qld

crs(qldNP) # coordinate system

bbox(qldNP) # extent
extent(qldNP) #extent

plot(qldNP[1,]) #subset a spatial layer to certain polygons, lines, or points
plot(subset(qldNP, qldNP$ESTATENAME == "Moreton Island National Park")) # subset a spatial layer to certain polygons, lines, or points by a variable name 
# using plot is quick and dirty, I would recommend ggplot2 for publication quality maps

#Rasters
## import raster layers, if in a different file to one in setwd() command include path within commas
veg <- stack("veg20160912.tif") # stack imports a multi-layer raster, similar to satellite images
#Lets look at the metadata for the veg raster
veg # notice there are 4 raster images
dem <- raster("dem.tif") #raster imports a single layer raster
dem # notice a single raster

nlayers(veg) # number of layers in a raster stack
veg <- veg[[2]] # subset raster stack used the [[]] to get just layer number 2. This layer is foilage projective cover

res(veg) # resolution

extent(veg) # extent
bbox(veg) # also extent

# veg is measurement of foilage projective cover between 0 and 100, 
# However the foilage projective cover in this raster layer is a measure between 100 and 200
cellStats(veg, stat='min', na.rm=TRUE)
cellStats(veg, stat='max', na.rm=TRUE)
# We can perform raster calculations very easily in R
# we will subtract 100 from each raster cell
veg <- veg - 100 
cellStats(veg, stat='min', na.rm=TRUE)
cellStats(veg, stat='max', na.rm=TRUE)

plot(dem)
plot(subset(qldNP, qldNP$ESTATENAME == "Lamington National Park"), add=T)  # put add=T to overlay
plot(veg)
plot(subset(qldNP, qldNP$ESTATENAME == "Lamington National Park"), add=T) #why can't we see Lamington NP layer?

## check that CRS and resolution allign for all datasets

veg # all information about layer

crs(veg) # coordinate system (CRS)
crs(qldNP)

# They are very different projections. They don't align.
# You should only perform analysis on multiple layers with the same crs. GUI (i.e. ArcGIS and QGIS) will
# allow you to analyse layers with different crs's, however it does some stuff under the hood, and its not recommended 
# as errors can be introduced.

################ Lets change the CRS 

# We will reproject the crs to projected, as the study area is small and the unit of measurement will be in meters. Where as geographic is
# in degrees, and should be used for large study areas.

# Vector
lyrebird <-  spTransform(lyrebird, CRS("+init=EPSG:3577")) # can use EPSG codes for CRS, google EPSG and CRS
qldNP <-  spTransform(qldNP, CRS("+init=EPSG:3577")) # These are protected areas for QLD


# Raster
dem <- projectRaster(dem, crs = CRS("+init=EPSG:3577"), res = 90, method = 'bilinear') # we will also change the resolution to 90x90m so that processing times are quicker. When you reproject a raster you need to tell it the method to use, here we will use bilinear
# The following will not work, always use projectRaster()
#crs(dem) <- CRS("+init=EPSG:3577")


### crop datasets to smaller extent need to be in the same CRS

# each of the layers have different extents and coordinate systems
# in R, we need all layers to have the same extent if we want to anlyse them
# so we will create a bounding box to clip all spatial layers to. Think of the bounding box as your study area 
studyArea <- extent(c(2024235,2056686,-3267605,-3217716))  

# check shapefile extent
dem <- crop(dem, studyArea)
veg <- crop(veg, studyArea)
# crop/clip shapefile extent
qldNP <- crop(qldNP, studyArea)
lyrebird <- crop(lyrebird, studyArea)

## derive slope from the dem. We will use this in or smd model
slopeAndAspect <- terrain(dem, opt=c('slope', 'aspect'), unit='degrees', neighbors=8)

plot(slopeAndAspect$slope) # plot still has the split screen 

plot(slopeAndAspect$slope)

writeRaster(slopeAndAspect, "slopeAspect.tif", "GTiff", overwrite = T) # write a raster to working directory

plot(veg) # NA values associated with shadow from steep cliff, Landsat sensor cannot detect vegetation

# We can use a focal filter to fill in NA values with an average value based on the values of all neighboring cells. For more info about focal filter: http://wiki.gis.com/wiki/index.php/Focal_Operation
# The command focal is used for this, however it cannot perform this operation on NA values only. Instead it will perform the focal filter on all cells.
# First we need to create a function that finds NA cells to pass to the focal filter.
# The following finds na values
fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( round(mean(x, na.rm=TRUE),0) )
  } else {
    return( round(x[i],0) )
  }
} 

veg <- focal(veg, w = matrix(1,7,7), fun = fill.na, pad = TRUE, na.rm = FALSE) # use focal filter to fill in NA cells
plot(veg)

# Now lets read in the National parks that fall in our NSW study area. Each NP in NSW is its own shapefile 
### To reduce repetition in our code, we will use a for loop to import, change CRS, crop multiple files to the studyArea. They will then get added to a list

nswNP <- NULL # Create an empty list to add each shapefile to
nameShp <- c("borderRangeNP", "limpinwoodNP", "numinbahNP", "wollumbinNP") # names of all shapefiles to import

for (i in 1:length(nameShp)){ # Should we vectorise this for the actual useR workshop?
  Shp <- readOGR(dsn = paste0(getwd(), '/NationalParksNSW/.'), layer = nameShp[i]) # import. paste0 joins strings together without spaces, getwd() prints the current working directory
  Shp <- spTransform(Shp, CRS("+init=EPSG:3577"))   # change CRS
  Shp <- crop(Shp, studyArea)                         # crop to studyArea 
  Shp <- Shp[,'NAME']                             # Only keep the name variable
  Shp$NAME <- as.character(Shp$NAME)              # coerce variable to character
  nswNP <- c(nswNP, list(Shp))                      # add each layer to list

  print(paste0('Done shapefile ', nameShp[i], '.shp')) # Good to keep you sane for very long lists
}

# Lets create a single national parks layer for the study area. We need to spatially join them so that they are a single layer 
# the bind command is used to join shapefile layers.
# we call bind and the list of layer to the do.call function
# do.call is a very handy function that lets you call any function without the need to write multiple arguments 1-by-1 https://www.stat.berkeley.edu/~s133/Docall.html
protAreas <- c(nswNP, qldNP) # first we need to append qld national park layers to the list that contains nsw national park layers
protAreas <- do.call(bind, protAreas)

##check that it worked, i.e. you have a single layer
plot(dem)
plot(protAreas, add = T)


# Lets turn protAreas (i.e. NP's) into a binary raster (i.e. 0=not protected and 1=protected)
protAreasRast <- rasterize(protAreas, dem) # here we create a protected Areas raster with the same properties as the dem raster. i.e. same extent, crs and cell size
plot(protAreasRast) # We can see that each national park has a different cell value

# Since we are not interested in individual national parks, just if its protected, we will turn the protAreasRast into a binary layer, i.e. 0 = not protected and 1 = protected
protAreasRast <- reclassify(protAreasRast, c(-Inf,0,0, 1,Inf,1, NA,NA,0)) # reclassify is used to change raster cell values. it follows from,to,new_value, 
                                                                          #i.e. all cell values from -Infinity, and all cell values to 0, reclassify them to 0
plot(protAreasRast) # That looks better :)

# Arrange all of the raster data into a single stack, i.e. a multiple layer raster
dat <- stack(dem,veg)
# get error as resolutions are different, extent CRS, and cell size need to be the same
extent(veg) == extent(dem) # this will return TRUE if the extents are the same 

veg <- projectRaster(veg, dem) # We can use the projectRaster function to match one raster to the attributes of another raster.

dat <- stack(dem, slopeAndAspect$slope, slopeAndAspect$aspect, veg, protAreasRast)
names(dat) # check layer names, layer.1 and layer.2 are a bit meaningless, lets change them
names(dat[[4]]) <- 'fpc' # change layer names to something that makes sense. use [[]] to subset multiple layer rasters
names(dat[[5]]) <- 'protected' # change layer names to something that makes sense

###
###### Time for a break
###

### Exploratory statistics, this is where R really shines as opposed to ArcGIS
# in the following code we will mix GIS operations with common statistical operations 
# How many sightings in each NP
noPerNP <- raster::extract(protAreas,lyrebird) # Identify how many lyrebirds are in each NP
                                              # Need to add the package in front i.e. raster::extract
                                              # as there are multiple loaded packages with extract functions
barplot(table(noPerNP$NAME), las = 2, cex.names = 0.75) # Visualise it, but we only have NSW national parks.

#Lets get Qld NP's as well. We need to find out the variable name of Qld NP's
qldNP # looks like the variable for NP names is ESTATENAME
qldNP$ESTATENAME # Yep that gets us the names of Qld NP's
# We will need to re-name the variable ESTATENAME to NAME in the original qldNP shapefile layer. 
# Then when we join the layers using bind, national park names for qld and nsw will appear in NAME variable
names(qldNP)[3] <- "NAME"
#Now we need to join qld and nsw NP shapefiles and turn them into a raster. These sets are the same as we did above
protAreas <- c(nswNP, qldNP)# first we need to append qld national park layers to the list that contains nsw national park layers
protAreas <- do.call(bind, protAreas)
noPerNP <- raster::extract(protAreas,lyrebird)
barplot(table(noPerNP$NAME), las = 2, cex.names = 0.75) # Thats better, but what about unprotected areas.

noPerNP # Unprotected areas are NA, this is because the polygons in the protAreas layer were only for NP's, i.e. evrything else was NA

noPerNP[is.na(noPerNP)] <- "Unprotected Areas"
barplot(table(noPerNP$NAME), las = 2, cex.names = 0.75) # nice :0


# Lets identify the average elevation, slope and vegetation cover of each National Park
# To do this we will first need to isolate the raster cell values that correspond with each NP, then we can get the average value
# first we will use a raster function called mask to isolate the cell values
# mask uses a "masking" layer (can be raster or a spatial polygon) to cookie cut another raster layer
# the output is a raster layer with the same cell values within the mask layer and NA values (or another specified value) outside the mask layer
masked <- list() # create an empty list
length(masked) <- length(protAreas) # specify the length of the list (good practice particularly for long lists)

for(i in 1:length(protAreas)){ # for each national park in the study area
  masked[[i]] <- mask(dat, protAreas[i,]) # mask the dat raster stack (elevation, slope, aspect and veg cover) for each protected area.
  names(masked)[i] <- protAreas$NAME[i] # give it an intelligible name (i.e. the name of the NP)
  print(paste0('Done protected area ', i, ' of ', length(protAreas))) # Just to keep you informed on where the loop is up to
}

#Lets have a look at the output
masked
# We have a list of raster bricks. 
masked[1][[1]] # subset elevation (dem) for springbrook NP
# or the following will get the same information
masked$`Border Ranges`$dem # we can see that the elevation of boarder ranges varies between 138 and 1148 meters in elevation
barplot(masked$`Border Ranges`$dem) # A barplot of elevation for Border ranges is a bit more informative

# now that we have isolated the cell values for each NP, we can get the mean for each environmental variable (dem,slope,aspect,vegetation cover)

# The following function takes a list (in this case 'masked') and applies the raster function cellStats to every element of the list, it returns a named vector
# We are passing in a function called cellStats, which gets simple statistics for all the cells in a raster
# In the following case were getting the mean, and omitting NA values (which is the masked value for cell outside to the NP)
# To get dem we have subset each layer to $dem in the cellstats function
mean_dem <- sapply(masked, function(x) cellStats(x$dem, mean, na.rm = TRUE))
mean_dem
#names(mean_dem) # Just to remind you what everything is

# What is the average slope for each National Park
mean_slope <- sapply(masked, function(x) cellStats(x$slope, mean, na.rm = TRUE))
mean_slope
barplot(mean_slope, las = 2, cex.names = 0.75)

#####
### More exploratory analysis

# Some of the observations are clumped,let's see how associated these clumps are with major tourist destinations
dests <- read.csv('tourist_dests.csv', stringsAsFactors = FALSE) # read in a CSV as a dataframe
dests # it has 3 columns, a feature of interest (in this case a tourist hotspot, and lat and long for coordinates). Its a points shapefile
coordinates(dests) <- c('lon', 'lat') # makes the points spatial objects
crs(dests) <- crs('+proj=longlat +datum=WGS84 +no_defs +towgs84=0,0,0') # and puts the points into an appropriate projection

dests <-  spTransform(dests, CRS('+init=EPSG:3577')) # reproject the points to a more usable projection
dests <- gBuffer(dests, 1000, byid = TRUE, id = dests$Feature) # add a 1000m buffer around each point

plot(dests);points(lyrebird) # visualise the major tourist destinations with buffers as well as lyrebird observations

extab <- raster::extract(dests, lyrebird) # ID how many lyrebird observations exist for each tourist destination
round((table(extab$Feature)/nrow(extab)) * 100, 2) # What percentage of all observations are centred around these tourist destinations

###### Modelling
## extract data for occurances  

datExtract <- raster::extract(dat, lyrebird) #get environmental data for the raster cells where lyrebird were sighted

df_ext <- as.data.frame(datExtract) # extracts writes to a matrix, coerce the matrix to a data.frame
df_ext <- na.omit(df_ext) # get rid of all observations which have NA values in any field

# Generate background data points (these points do not represent absence data, but just characterise what makes the locations of presence data distinct from the background)
bgPoints <- sampleRandom(veg, length(lyrebird), sp = TRUE)
# sampleRandom creates a random points dataset 
# length(lyrebird) says that we want the same number of observations for background data as we do for presence data.
# Effectively what the data would be if there was no influence of any of the explanatory variables on observing a lyrebird presence

plot(dat$dem) # The elevation data
plot(bgPoints, col = 'red', add=T) # plot the randomly generated points
plot(lyrebird, col = 'blue', add=T) # plot the genuine observations

# obtain the data for environmental variables for the background points
bg <- raster::extract(dat, bgPoints)
bg <- as.data.frame(bg) 
bg <- na.omit(bg) 

# Add the presence/background variable to both data.frames
df_ext$PB <- 1 # Presence = 1
bg$PB <- 0 # Presence = 0

df <- rbind(df_ext, bg)


## Now some stats - 
### Perform a simple linear model
# The model uses a normal (Gaussian) distribution and models whether a point is background or observation as a function of all variables
mod <- glm(PB ~ ., data = df, family = 'gaussian')
simplm <- step(mod) # Identify what variables can be excluded in a simplified model

# use the simplified formula
lm2 <- glm(simplm$formula, data = df)

# Have a look at the model
summary(lm2)
termplot(lm2)

# Likelihood of observing a lyrebird increase above ~750 metres
# More likely to see a lyrebird when the slope is <25 degrees
# More likely to see a lyrebird when facing west, however, is not a linear varible, it's circular, so this isn't a good variable
# More likely to see a lyrebird when fpc >75%

# Why is protection not significant?

cellSize <- area(dat$dem, na.rm = TRUE, weights = FALSE)
# calculates size of every cell in the raster - only works for rasters with a long-lat projection
# Because this is only exploratory we'll let that slide for now
cellSize <- cellSize[!is.na(cellSize)] # removes NA cells
rasterArea <- length(cellSize) * median(cellSize) # returns the median cell size * number of cells, or the estimation of total area

gArea(protAreas)/rasterArea
plot(studyArea);plot(protAreas, col = 'green', add = TRUE)

# So much of the background data is protected that it is unlikely that it would come up as significant,
# If this were a real study there would be a lot more background data and protection may come up as significant

### Extrapolation and prediction
# predict and plot likelihood of presence within study region
predmap <- predict(dat, lm2)
plot(predmap)

# plot predicted presence/absence at an arbitrary threshold (0.50)
predmap.thresh <- predmap > 0.90
plot(predmap.thresh)

```