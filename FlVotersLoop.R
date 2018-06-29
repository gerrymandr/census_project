library(stringr)
library(ggmap)
require(rgdal)


# Load in VTD shapefile

vtds <- readOGR(dsn = "tl_2012_12_vtd10")
censusData <- vtds@data
censusData$COUNTYFP10 <- as.numeric(censusData$COUNTYFP10)

# Load file linking county FIPS codes to three-letter county codes

nameLink <- read.csv("countyNames.csv")


#Loop through each county to pull out precinct identifiers

setwd("C:/Users/elhai/Documents/Summers/Summer 2018/VRDI/Census/flvoters_moon")
for(i in 1:67){
  countyCensus <- censusData[censusData$COUNTYFP10 == i]
  code <- nameLink$Code2[nameLink$FIPS2 == i] ##get three-letter county code corresponding to county FIPS code
  countyVoterFile <- read.table(file = paste(code, "_20171114.txt", sep = ""), sep="\t", quote = "", comment.char = "")
  ##use grep to match precinct code from voter file to code from Census file
}



# # files <- list.files(pattern=".*.txt")
# # get precincts from voter files
# voter_precincts <- unique(voter_data$Precinct)
# 
# # get precincts from shapefile
# vtd_precincts <- sort(as.numeric(unique(vtds@data$VTDST10)))
# 
# # check if all precincts in voter file match directly to precinct name from shapefile 
# length(voter_precincts %in% vtd_precincts) - length(voter_precincts)