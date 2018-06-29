library(stringr)
library(ggmap)
require(rgdal)

# Parse voter file
data <- read.table(file = "ALA_20171114.txt", sep="\t", quote = "", comment.char = "")
# data <- read.table(file = "BAK_20171114.txt", sep="\t", quote = "", comment.char = "")

# Choose columns of interest
voter_data <- data[c(1, 8:10, 25, 30: 32)]

# rename columns
colnames(voter_data)[which(names(voter_data) == "V1")] <- "County Code"
colnames(voter_data)[which(names(voter_data) == "V8")] <- "Address Line 1"
colnames(voter_data)[which(names(voter_data) == "V9")] <- "Address Line 2"
colnames(voter_data)[which(names(voter_data) == "V10")] <- "City"
colnames(voter_data)[which(names(voter_data) == "V25")] <- "Precinct"
colnames(voter_data)[which(names(voter_data) == "V30")] <- "Congressional District"
colnames(voter_data)[which(names(voter_data) == "V31")] <- "House District"
colnames(voter_data)[which(names(voter_data) == "V32")] <- "Senate District"

# Load in VTD shapefile
vtds <- readOGR(dsn = "tl_2012_12_vtd10")

# get precincts from voter files
voter_precincts <- unique(voter_data$Precinct)

# get precincts from shapefile
vtd_precincts <- sort(as.numeric(unique(vtds@data$VTDST10)))

# check if all precincts in voter file match directly to precinct name from shapefile 
length(which(voter_precincts %in% vtd_precincts)) - length(voter_precincts)

