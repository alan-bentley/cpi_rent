#-----------------------------------------------------------------------------------------------------------------
#  Bond_stats_region Script
#  
#  Alan Bentley, March 2018
#
#  Some summary stats to help understand Tenancy Bond data
#
#
#------------------------------------------------------------------------------------------------------------------

library(scales)
library(dplyr) #For data manipulation

root_dir <- "//wd.govt.nz/dfs/SharedData/Wellington/Bowen Street/R&N/Networks/Housing Information & Modelling/housing_projects/cpi_rent/"

load(file = paste0(root_dir, "/data_intermediate/house_long.rda"))


#1. Some summary stats
house_size <- house %>% group_by(quarter_f) %>% summarise(periodID = length(unique(PropertyID)))

length(unique(house$quarter_f)) #number of quarters


#1.2 Get higher aggregate regions

#Use geo classification from mbie Bond Data on website

geo <- read.csv(url("http://www.mbie.govt.nz/info-services/housing-property/sector-information-and-statistics/rental-bond-data/Geographical%20Table.csv"))

#1.3 create CPI regions (custom aggregation of regional councils)

geocpi <- select(geo,-Water, -TA) %>%
   mutate(cpi_region = ifelse(Region=="Auckland","Auckland",
                              ifelse(Region=="Wellington","Wellington",
                                     ifelse(Region=="Canterbury","Canterbury",
                                            ifelse(Region %in% c("Marlborough",
                                                                 "Tasman",
                                                                 "Nelson",
                                                                 "West Coast",
                                                                 "Otago",
                                                                 "Southland"),"Rest of South Island",
                                                   "Rest of North Island")))))
mod_data3 <- left_join(mod_data,geocpi, by="SAU")

cpi_regions <- unique(mod_data3$cpi_region)
cpi_regions <- subset(cpi_regions,!is.na(cpi_regions))


#2. Number of times each property appears in the data (occurance)

#2.1 mean and median occurances
occ_summary <- function (dat=house, start, end, region) {
   
   select(dat, Date.Lodged, PropertyID, quarter_f, cpi_region) %>%
      filter(as.Date(Date.Lodged,"%Y-%m-%d") >= as.Date(start,"%Y-%m-%d")
             & as.Date(Date.Lodged,"%Y-%m-%d") < as.Date(end,"%Y-%m-%d") &
                cpi_region==region) %>%
      group_by(PropertyID) %>%
      mutate(Property_Obs = length(unique(quarter_f))) %>%
      ungroup() %>%
      summarise (Ave_obs_q = mean(Property_Obs), Median_obs_q = median(Property_Obs)) %>%
      mutate(win_start=start, win_end = end, win_days = as.Date(end,"%Y-%m-%d") - as.Date(start,"%Y-%m-%d"),
             region=region) %>%
      as.data.frame()
   
}

#function test
# occurance_summary <- occ_summary(dat= mod_data3, start="2016-01-01",
# end="2017-12-31", region="Wellington")

#2.2 distribution
occ_dist <- function (dat=house, start, end, region) {
   
   select(dat, Date.Lodged, PropertyID, quarter_f, cpi_region) %>%
      filter(as.Date(Date.Lodged,"%Y-%m-%d") >= as.Date(start,"%Y-%m-%d")
             & as.Date(Date.Lodged,"%Y-%m-%d") < as.Date(end,"%Y-%m-%d") &
                cpi_region==region) %>%
      group_by(PropertyID) %>%
      mutate(Observations = as.character(length(unique(quarter_f)))) %>%
         group_by(Observations) %>%
         summarise (count = length(unique(PropertyID))) %>%
         mutate(prop=count/sum(count), win_start=start, win_end = end, 
                win_days = as.Date(end,"%Y-%m-%d") - as.Date(start,"%Y-%m-%d"),
                region=region) %>%
      as.data.frame()
   
}

#function test
# occurance_distribution <- occ_dist(dat= mod_data3, start="2017-01-01",
# end="2018-01-01", region="Wellington")

#3. Loop over data windows

occ_sum <- c()
occ_dis <- c()

for (region in cpi_regions) {

for(y_end in 1994:2018){

for (y in 1993:2018) {
   occurance_summary <- occ_summary(dat= mod_data3, start=paste0(as.character(y),"-01-01"), 
                                    end=paste0(as.character(y_end),"-01-01"), region=region)
   cat(paste("\nRunning summary years:",y,"to",y_end))
   occ_sum <- rbind(occ_sum,occurance_summary)
   
   occurance_distribution <- occ_dist(dat= mod_data3, start=paste0(as.character(y),"-01-01"), 
                                      end=paste0(as.character(y_end),"-01-01"), region=region)
   cat(paste("\nRunning distribtuion years:",y,"to",y_end))
   occ_dis <- rbind(occ_dis,occurance_distribution)
}}}

occ_sum2 <- filter(occ_sum, win_days > 0)
occ_dis2 <- filter(occ_dis, win_days > 0)

#3.1 Export data

write.csv(occ_sum2,paste0(root_dir, "/data_intermediate/occurance_summary_region2.csv"), row.names=FALSE)
write.csv(occ_dis2,paste0(root_dir, "/data_intermediate/occurance_distribution_region2.csv"), row.names=FALSE)

### END