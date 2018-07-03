###
### Claire Kelling
###  Census Project, Voting Rights Data Institute
###  Looking at the most high risk Census Tracts in terms of the 
###       Low Response Score, given by the Census.
###

library(tidycensus)
library(purrr)

### How to get specific Census data for every tract in the US:
#https://walkerke.github.io/2017/05/tidycensus-every-tract/:
# Un-comment below and set your API key
#census_api_key("")
# 
# Hispanic: B03001_003
#  by race: B03002_012

us <- unique(fips_codes$state)[1:51]

totalpop <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B01003_001", 
          state = x)
})

#------------------------------
hisppop <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B03001_003", 
          state = x)
})

#get acs estimates for national level
class(hisppop$estimate)

#create variable for percent hispanic
perc_hisp <- as.data.frame(cbind(totalpop$GEOID, as.numeric(hisppop$estimate/totalpop$estimate)))
colnames(perc_hisp) <- c("GEOID", "perc_hisp")
perc_hisp$GEOID <- as.numeric(as.character(perc_hisp$GEOID))
perc_hisp$perc_hisp <- as.numeric(as.character(perc_hisp$perc_hisp))

#################
### Now, use the data that is downloaded from Planning Database:
### https://www.census.gov/research/data/planning_database/2016/
temp <- tempfile()
download.file("https://www.census.gov/research/data/planning_database/2016/docs/pdb2016trv8_us.zip",
              temp)
data <- read.csv(unz(temp, "pdb2016trv8_us.csv"))
unlink(temp)

hist(data$Low_Response_Score, main = "Histogram of Low Response Scores", xlab ="LRS")

#Subsetting dataframe
plot_dat <- data[,c("GIDTR", "State", "State_name", "County", "Hispanic_ACS_10_14",
                    "Tot_Population_ACS_10_14", "Low_Response_Score",
                    "Prs_Blw_Pov_Lev_ACS_10_14", "pct_Mobile_Homes_ACS_10_14", 
                    "NH_Blk_alone_ACS_10_14", "Pop_18_24_ACS_10_14")]
plot_dat$Hisp_perc <- plot_dat$Hispanic_ACS_10_14/plot_dat$Tot_Population_ACS_10_14
plot_dat$pov_perc <- plot_dat$Prs_Blw_Pov_Lev_ACS_10_14/plot_dat$Tot_Population_ACS_10_14
plot_dat <- plot_dat[complete.cases(plot_dat), ]


plot_LRS <- function(state, var){
  plot_lrs <- ggplot(data = plot_dat, aes_string(x = var, 'Low_Response_Score')) +
    geom_point(alpha = 0) +
    geom_point(data = plot_dat[which(plot_dat$State_name == paste(state)),], 
               aes_string(var, 'Low_Response_Score'), alpha = 0.2)+
    geom_line(data = plot_dat, aes(y = mean(plot_dat$Low_Response_Score)), col = "red")+
    geom_smooth(aes(col = "National"),se=F, size = 1.5, method = "loess")+
    geom_smooth(data = plot_dat[which(plot_dat$State_name == paste(state)),], 
                aes_string(var, 'Low_Response_Score', col = "state"), se =F, size = 1.5, method = "loess")+
    ggtitle(paste(var, "vs Low Response Score,", state))
  return(plot_lrs)
}

state <- "Texas"
var <- "Hisp_perc"
plot_LRS(state, var)


#look at North Carolina
plot_LRS("California", "Hisp_perc")
plot_LRS("Texas", "Hisp_perc")
plot_LRS("North Carolina", "Hisp_perc")

plot_LRS("California", "pov_perc")
plot_LRS("Texas", "pov_perc")

#plot of South Texas
south_texas <- c(7,13,25,29,47,57,61,123,127,131,163,175,177,187,215,239,247,249,255,261,271,273,
                 283,285,297,311,321,323,327,355,391,409,427,463,469,479,481,489,493,505,507)
south_texas_dat <- plot_dat[which(plot_dat$State_name == "Texas" & 
                                    plot_dat$County %in% south_texas),]
texas_dat <- plot_dat[which(plot_dat$State_name == "Texas"),]

state <- "South Texas"
plot_stex <- ggplot(data = texas_dat, aes_string(x = var, 'Low_Response_Score')) +
  geom_point(alpha = 0.1, col = "blue") +
  geom_point(data = south_texas_dat, 
             aes_string(var, 'Low_Response_Score'))+
  geom_smooth(aes(col = "Texas"),se=F, size = 1.5)+
  geom_smooth(data = south_texas_dat, 
              aes_string(var, 'Low_Response_Score', col = "state"), se =F, size = 1.5)+
  ggtitle(paste(var, "vs Low Response Score,", state))


high_risk <- plot_dat[which(plot_dat$Low_Response_Score > 30),]

#which states occur most frequently: California, New York, Texas
sort(table(high_risk$State_name), decreasing = TRUE)[1:3]

plot_hrisk <- ggplot(data = plot_dat, aes_string(x = var, 'Low_Response_Score')) +
  geom_point(alpha = 0.1, col = "blue") +
  geom_point(data = high_risk, 
             aes_string(var, 'Low_Response_Score'))+
  geom_smooth(aes(col = "National"),se=F, size = 1.5)+
  geom_smooth(data = high_risk, 
              aes_string(var, 'Low_Response_Score', col = "state"), se =F, size = 1.5)+
  ggtitle(paste(var, "vs Low Response Score,", state))


mean(south_texas_dat$Low_Response_Score) #25.36059
mean(plot_dat$Low_Response_Score) #21.12681
mean(plot_dat$Low_Response_Score[which(plot_dat$State_name == "Texas")]) #23.31725

hist(plot_dat$Low_Response_Score)
hist(south_texas_dat$Low_Response_Score)

## Creating a variable for our own non-response risk

met_dat <- data[,c("GIDTR", "State", "State_name", "County", "Hispanic_ACS_10_14",
                    "Tot_Population_ACS_10_14", "Low_Response_Score",
                    "Prs_Blw_Pov_Lev_ACS_10_14", "pct_Mobile_Homes_ACS_10_14", 
                    "NH_Blk_alone_ACS_10_14", "Pop_18_24_ACS_10_14",
                   "Othr_Lang_ACS_10_14", "Pop_under_5_ACS_10_14",
                   "NH_AIAN_alone_ACS_10_14","Not_HS_Grad_ACS_10_14","NH_Asian_alone_ACS_10_14",
                   "Born_foreign_ACS_10_14","US_Cit_Nat_ACS_10_14","NON_US_Cit_ACS_10_14",
                   "Renter_Occp_HU_ACS_10_14","Tot_Housing_Units_ACS_10_14")]

#young children 0 and 5 
#homelessness, percent renter
#education without high school degree
#percent native

met_dat$black_perc <- met_dat$NH_Blk_alone_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$Hisp_perc <- met_dat$Hispanic_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$pov_perc <- met_dat$Prs_Blw_Pov_Lev_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$lang_perc <- met_dat$Othr_Lang_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$youth_perc <- met_dat$Pop_under_5_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$nat_perc <- met_dat$NH_AIAN_alone_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$educ_perc <- met_dat$Not_HS_Grad_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$asian_perc <- met_dat$NH_Asian_alone_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$foreign_perc <- met_dat$Born_foreign_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$natcitz_perc <- met_dat$US_Cit_Nat_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$noncitz_perc <- met_dat$NON_US_Cit_ACS_10_14/met_dat$Tot_Population_ACS_10_14
met_dat$renter_perc <- met_dat$Renter_Occp_HU_ACS_10_14/met_dat$Tot_Housing_Units_ACS_10_14
met_dat <- met_dat[complete.cases(met_dat), ]

met_dat <- met_dat %>% mutate_each_(funs(scale(.) %>% as.vector), 
                                    vars=c("black_perc",
                                           "Hisp_perc","pov_perc","lang_perc",
                                           "pct_Mobile_Homes_ACS_10_14","youth_perc",
                                           "nat_perc", "educ_perc","asian_perc",
                                           "foreign_perc","natcitz_perc","noncitz_perc",
                                           "renter_perc"))

met_dat$new_score <- rowMeans(met_dat[,c("black_perc",
                                         "Hisp_perc","pov_perc","lang_perc",
                                         "pct_Mobile_Homes_ACS_10_14","youth_perc",
                                         "nat_perc", "educ_perc","asian_perc",
                                         "foreign_perc","natcitz_perc","noncitz_perc",
                                         "renter_perc")])


ggplot(data = met_dat, aes_string(x = "new_score", 'Low_Response_Score')) +
  geom_point(alpha = 0.1, col = "blue")

cor(met_dat$Low_Response_Score,met_dat$new_score)

#future step : principal component analysis

# Now, we would like to scale the LRS between 0 and 2, multiply this by estimates for the undercount
# and then calculate the estimated total population based on this undercount.

met_dat$sc_LRS <- scale(met_dat$Low_Response_Score, center = 1)
hist(met_dat$sc_LRS)
class(met_dat$sc_LRS)
met_dat$sc_LRS <- as.numeric(met_dat$sc_LRS)

exp_und <- 0.1

met_dat$sc_und <- exp_und*met_dat$sc_LRS
hist(met_dat$sc_und)

met_dat$und_pop <- met_dat$Tot_Population_ACS_10_14 - 
  (met_dat$Hispanic_ACS_10_14*met_dat$sc_und)

met_dat$und_count <- met_dat$Tot_Population_ACS_10_14 - met_dat$und_pop

prec_shp <- readOGR(dsn = "C:/Users/ckell/Desktop/Summer 2018/MGGG/Census/cb_2017_48_tract_500k", layer = "cb_2017_48_tract_500k")
plot(prec_shp)
head(unique(prec_shp@data$GEOID))
prec_shp@data$GEOID <- as.numeric(as.character(prec_shp@data$GEOID))
colnames(met_dat)[1] <- "GEOID"

prec_shp@data <- dplyr::left_join(prec_shp@data, met_dat[,c("GEOID", "und_count")], by = c(GEOID = "GEOID"))
nrow(prec_shp@data)

#plotting
prec_shp@data$id <- rownames(prec_shp@data)

sp_f     <- fortify(prec_shp)
sp_f     <- join(sp_f,prec_shp@data, by="id")


und_by_prec <- ggplot()+ 
  #geom_polygon(data=dist_shp,aes(long,lat, group = group), fill = NA, col = "red") + 
  geom_polygon(data = sp_f, aes(long, lat, group = group, fill = (und_count))) + 
  coord_equal() +
  labs(fill = "Undercount") +
  ggtitle("Scaled Undercount Estimate by Census Tract")+ 
  scale_fill_gradient2(low = "white", high = "red")
und_by_prec


prec_shp <- readOGR(dsn = "C:/Users/ckell/Desktop/Summer 2018/MGGG/Census/cb_2017_06_tract_500k", layer = "cb_2017_06_tract_500k")
plot(prec_shp)
head(unique(prec_shp@data$GEOID))
prec_shp@data$GEOID <- as.numeric(as.character(prec_shp@data$GEOID))
#colnames(met_dat)[1] <- "GEOID"

prec_shp@data <- dplyr::left_join(prec_shp@data, met_dat[,c("GEOID", "und_count")], by = c(GEOID = "GEOID"))
nrow(prec_shp@data)

#plotting
prec_shp@data$id <- rownames(prec_shp@data)

sp_f     <- fortify(prec_shp)
sp_f     <- join(sp_f,prec_shp@data, by="id")


und_by_prec <- ggplot()+ 
  #geom_polygon(data=dist_shp,aes(long,lat, group = group), fill = NA, col = "red") + 
  geom_polygon(data = sp_f, aes(long, lat, group = group, fill = (und_count))) + 
  coord_equal() +
  labs(fill = "Undercount") +
  ggtitle("Scaled Undercount Estimate by Census Tract")+ 
  scale_fill_gradient2(low = "white", high = "red")
und_by_prec

#comment
