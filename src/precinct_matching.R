###
### Claire Kelling
###  Census Project, Voting Rights Data Institute
###  Looking at matching the precinct names from the shape 
###       file to the precinct from the voterfile.
###

# Packages:
devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
library(sp)
library(spdep)
library(classInt)
library(fields)
library(ggplot2)
library(dplyr)
library(ade4) 
library(igraph) 
library(CARBayesdata)
library(CARBayes)
library(gridExtra)
library(xtable)
library(stringi)
library(rgdal)
library(ngspatial)
library(plyr)
library(readxl)
library(qdap)
library(plyr)


#precinct shape file
prec_shp <- readOGR(dsn = "Shapefiles/SplitPrecincts", layer = "SplitPrecincts20161108")
#plot(prec_shp)

#voterfile shape file
v_file <- read_excel("VRFile-A.xlsx")#, range = "C1:C388888")
v_file2 <- read_excel("VRFile-B.xlsx", colnames = F)#, range = "C1:328518")

#publicly available shapefile
prec_shp2 <- readOGR(dsn = "VR_ElectionPrecinctShapeFile", layer = "TravisCountyElectionPrecincts")

#Id's for precinct shape file
prec_shp_id <- prec_shp2@data$PCTCODE
prec_shp_id <- as.numeric(as.character(prec_shp_id))
prec_shp_id2 <- prec_shp2@data$Precinct
prec_shp_id2 <- as.numeric(as.character(prec_shp_id2))
length(unique(prec_shp_id))

colnames(v_file2) <- colnames(v_file)
vf_prec_id <- c(unique(v_file$PCTCOD), unique(v_file2$PCTCOD))
vf_prec_id <- unique(vf_prec_id)

length(which(vf_prec_id %in% prec_shp_id)) - length(vf_prec_id) #all of them are in prec_shp_id
#length(which(vf_prec_id %in% prec_shp_id2)) - length(vf_prec_id) #all of them are in prec_shp_id2

#election results at precinct level and then pair that with the Ohio shape files (subset of the files)

# look into Open Street Map in R for geocoding
# https://github.com/hrbrmstr/nominatim

v_file$Address <- paste2(v_file[,c("BLKNUM", "STRDIR", "STRNAM", "STRTYP", 
                        "RSCITY", "RSTATE", "RZIPCD")], sep = " ", handle.na = F)
v_file$Address <- gsub("NA "," ",v_file$Address)

write.csv(v_file, file = "v_file.csv")

#osm_geocode(c("1600 Pennsylvania Ave, Washington, DC.",
#              "1600 Amphitheatre Parkway, Mountain View, CA",
#              "Seattle, Washington"))


#aggregate v_file by precinct and plot
tot_vf <- rbind(v_file, v_file2)

#count voterfiles by precinct
agg_dat <- count(tot_vf, "PCTCOD")
agg_dat <- agg_dat[complete.cases(agg_dat),] #get rid of one NA value

# Now we want to join to shape file to plot
colnames(prec_shp2@data)
prec_shp2@data$PCTCODE <- as.numeric(as.character(prec_shp2@data$PCTCODE))
prec_shp2@data <- dplyr::left_join(prec_shp2@data, agg_dat, by = c(PCTCODE = "PCTCOD"))
prec_shp2@data$id <- rownames(prec_shp2@data)


sp_f     <- fortify(prec_shp2)
sp_f     <- join(sp_f,prec_shp2@data, by="id")
vote_by_prec <- ggplot()+ 
  #geom_polygon(data=dist_shp,aes(long,lat, group = group), fill = NA, col = "red") + 
  geom_polygon(data = sp_f, aes(long, lat, group = group, fill = (freq))) + 
  coord_equal() +
  labs(fill = "Count") +
  ggtitle("Voterfile Count by Precinct, Travis County")+ 
  scale_fill_gradient2(low = "white", high = "blue")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
vote_by_prec

