# epm package Demonstration

setwd('~/Dropbox/epmWorkshop')

library(epm)
library(sf)
library(ape)

######################################
# First step: Read in all data types

# 1. Geographic data. Here, we will do what we did in the spatial demo. We will extract the squirrels from the IUCN mammals dataset and create a list, where each list element is the spatial data for a species. Importantly, the names of the list are the names of the species.

# filename and location of IUCN mammals shapefile
IUCNfile <- "~/Dropbox/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp"

# load the shapefile as a simple features object
mammals <- st_read(IUCNfile, stringsAsFactors = FALSE)

land <- st_read('ne_50m_land/ne_50m_land.shp')


# Identify the species of interest
allsp <- unique(mammals$binomial)
squirrelSp <- grep("Spermophilus\\s|Citellus\\s|Tamias\\s|Sciurus\\s|Glaucomys\\s|Marmota\\s|Cynomys\\s", allsp, value = TRUE, ignore.case = TRUE)
head(squirrelSp)


# Let's now pull each of these species range polygons and store them as a list.

spList <- vector('list', length(squirrelSp))
names(spList) <- squirrelSp

for (i in 1:length(squirrelSp)) {
    ind <- which(mammals$binomial == squirrelSp[i])
    spList[[i]] <- mammals[ind,]
}

# Check for potential geometry issues and (hopefully) repair where needed.

library(lwgeom)

for (i in 1:length(spList)) {
    if (!any(st_is_valid(spList[[i]]))) {
        message('\trepairing poly ', i)
        spList[[i]] <- st_make_valid(spList[[i]])
        if (!any(st_is_valid(spList[[i]]))) {
        	message('\t\tstill broken...')
    	}
    }
}

# We will want to work in an equal area projection. This is the North America Albers Equal Area Projection.
EAproj <- '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'

# Let's now transform each range polygon to this projection. 
spListEA <- lapply(spList, function(x) st_transform(x, crs = EAproj))


# We need to make some adjustments to account for synonymy issues.

names(spList) <- gsub('Neotamias', 'Tamias', names(spList))
names(spListEA) <- gsub('Neotamias', 'Tamias', names(spListEA))

# Replace all spaces with underscores (not strictly necessary, but this will match the phylogenetic data later on)

names(spList) <- gsub('\\s+', '_', names(spList))
names(spListEA) <- gsub('\\s+', '_', names(spListEA))



# 2. Phylogeny
## Here, I will read in the all-mammals MCC tree I generated for the phylo tutorial. 

treefile <- 'Upham_mammals_allExtant_mcctree.tre'

# This is a newick file, so I will read in with read.tree
mammaltree <- read.tree(treefile)
mammaltree

# This tree has family and order attached to the tip labels. This is useful information, but will cause problems. The pattern is Genus_species_FAMILY_ORDER

# We will use the strsplit() function to split each tip label into pieces, where the splits will happen wherever there are underscores.
# Here is an example
strsplit(mammaltree$tip.label[1], '_')

newLabels <- character(Ntip(mammaltree))
for (i in 1:Ntip(mammaltree)) {
	tmp <- strsplit(mammaltree$tip.label[i], '_')[[1]]
	newLabels[i] <- paste0(tmp[1], '_', tmp[2])
}

# more complicated, but we could have done this with regular expression
gsub('(\\w+)_(\\w+)_(\\w+)_(\\w+)', '\\1_\\2', mammaltree$tip.label[1], ignore.case = TRUE)

mammaltree$tip.label <- newLabels
mammaltree

length(intersect(names(spListEA), mammaltree$tip.label))



# 3. Morphological data

traits <- read.csv('squirrelShapeData.csv', stringsAsFactors = FALSE, row.names = 1, header = FALSE)
traits[1:5, 1:5]

length(intersect(names(spListEA), rownames(traits)))

# How many taxa are shared across the 3 sets?
length(Reduce(intersect, list(names(spListEA), mammaltree$tip.label, rownames(traits))))



#############################################################

# Now we are ready to create an epmGrid object.

# We will opt for 50km resolution hexagonal grids, using the areaCutoff approach for grid conversion. 

extentPoly <- createEPMgrid(spListEA, resolution = 50000, method = 'centroid', cellType = 'hexagon', retainSmallRanges = TRUE, extent = 'interactive')

landEA <- st_transform(land, crs = st_crs(spListEA[[1]]))

plot(st_geometry(landEA), lwd = 0.5)
extentSF <- st_as_sfc(extentPoly, crs = st_crs(spListEA[[1]]))
plot(extentSF, add = TRUE, border = 'red')

squirrelEPM <- createEPMgrid(spListEA, resolution = 50000, method = 'centroid', cellType = 'hexagon', retainSmallRanges = TRUE, extent = extentPoly) # took me ~ 3 minutes

squareEPM <- createEPMgrid(spListEA, resolution = 50000, method = 'centroid', cellType = 'square', retainSmallRanges = TRUE, extent = extentPoly) # took me ~ 15 seconds

# you can save these EPM grids by writing them to file. 
# write.epmGrid(squirrelEPM, 'squirrelEPMdemo')

squirrelEPM <- read.epmGrid('squirrelEPMdemo.rds')


squirrelEPM

plotSpRange(squirrelEPM, taxon = 'Tamias_dorsalis')

######################################
## ADD PHYLO AND TRAIT DATA

squirrelEPM <- addPhylo(squirrelEPM, mammaltree)
squirrelEPM <- addTraits(squirrelEPM, data = traits)

squirrelEPM

#######################################

# Basic plotting

library(viridis)

plot(squirrelEPM)

plot(squirrelEPM, col = plasma)

plot(squirrelEPM, col = c('midnight blue', 'blue', 'light blue', 'violet'))

plot(squirrelEPM, basemap = 'interactive', alpha = 0.5)

# This turns off the interactive mapping. I've now added this internally.
tmap::tmap_mode('plot')

# create an interactive plot where you can click to see which species occur somewhere. 
identify(squirrelEPM, pal = viridis)

####################################
# Calculate some grid metrics

# a multivariate shape metric
disp <- gridMetrics(squirrelEPM, metric = 'disparity')
disp
plot(disp, col = turbo)

# minimum nearest neighbor distance
minNN <- gridMetrics(squirrelEPM, metric = 'min_NN_dist')
plot(minNN, lwd = 0.1, border = gray(0.9))
    
# a univariate trait (we will pretend we have a non-shape data table, and calculate the mean of the first column)
meanCol1 <- gridMetrics(squirrelEPM, metric = 'mean', var = 1)
plot(meanCol1)

# phylogenetic signal
blomberg <- gridMetrics(squirrelEPM, metric = 'phylosignal')

# phylo disparity
phyloDisp <- gridMetrics(squirrelEPM, metric = 'phyloDisparity')
plot(phyloDisp, lwd = 0.1, border = gray(0.9))
plot(phyloDisp, lwd = 0.01, borderCol = 'white')

# range weighted metric
phyloWE <- gridMetrics(squirrelEPM, metric = 'phyloWeightedEndemism')
plot(phyloWE, lwd = 0.1)
plot(phyloWE, log = TRUE, lwd = 0.1)



#######################################
# Turnover metrics - beta diversity

phylobeta <- phylogenetic_betadiv(squirrelEPM, radius = 100000, component = 'full')
plot(phylobeta, pal = viridis, lwd = 0.15)


########################################
# Other functions that you can explore:

pts <- rbind.data.frame(
    c(-120.5, 38.82),
    c(-84.02, 42.75),
    c(-117.95, 55.53))
colnames(pts) <- c('x', 'y')
ptsSF <- st_as_sf(pts, coords = 1:2, crs = "epsg:4326")
pts <- st_coordinates(st_transform(ptsSF, crs = proj))

epmToPhyloComm(squirrelEPM, sites = pts)
# can be passed over to other R packages, like picante or PhyloMeasures, etc. 

# for getting the species that are found at particular coordinates
extractFromEpmGrid(squirrelEPM, pts)
extractFromEpmGrid(squirrelEPM, pts, collapse = FALSE)

# if you have a raster, and you would like to resample it to a hexagonal grid
library(terra)
topoFile <- 'roughness_10KMmd_GMTEDmd.tif'

topo <- rast(topoFile)
landRast <- rasterize(vect(land), topo)
topo <- terra::mask(topo, mask = landRast)

e <- st_transform(st_make_grid(squirrelEPM[[1]], n = 1), crs = 4326)
e <- st_bbox(e)[c('xmin', 'xmax', 'ymin', 'ymax')]
topoCrop <- crop(topo, e)
topoCrop <- project(topoCrop, EAproj)

plot(topoCrop)

topoCrop <- aggregate(topoCrop, 4)

topoHex <- rasterToGrid(topoCrop, squirrelEPM)

plot(topoCrop, col = viridis(100))
plot(topoHex, pal = viridis)

# This topographic roughness raster has now been converted to the same grid as the EPMgrid object. 
