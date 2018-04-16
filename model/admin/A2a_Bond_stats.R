#-----------------------------------------------------------------------------------------------------------------
#  Bond_stats Script
#  
#  Alan Bentley, January 2018
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

#2. Number of times each property appears in the data (occurance)

#2.1 mean and median occurances
occ_summary <- function (dat=house, start, end) {
   
   select(dat, Date.Lodged, PropertyID, quarter_f) %>%
      filter(as.Date(Date.Lodged,"%Y-%m-%d") >= as.Date(start,"%Y-%m-%d")
             & as.Date(Date.Lodged,"%Y-%m-%d") < as.Date(end,"%Y-%m-%d")) %>%
      group_by(PropertyID) %>%
      mutate(Property_Obs = length(unique(quarter_f))) %>%
      ungroup() %>%
      summarise (Ave_obs_q = mean(Property_Obs), Median_obs_q = median(Property_Obs)) %>%
      mutate(win_start=start, win_end = end, win_days = as.Date(end,"%Y-%m-%d") - as.Date(start,"%Y-%m-%d")) %>%
      as.data.frame()
   
}

#function test
# occurance_summary <- occ_summary(dat= house, start="1993-01-01",
# end="2017-12-31")

#2.2 distribution
occ_dist <- function (dat=house, start, end) {
   
   select(dat, Date.Lodged, PropertyID, quarter_f) %>%
      filter(as.Date(Date.Lodged,"%Y-%m-%d") >= as.Date(start,"%Y-%m-%d")
             & as.Date(Date.Lodged,"%Y-%m-%d") < as.Date(end,"%Y-%m-%d")) %>%
      group_by(PropertyID) %>%
      mutate(Observations = as.character(length(unique(quarter_f)))) %>%
         group_by(Observations) %>%
         summarise (count = length(unique(PropertyID))) %>%
         mutate(prop=count/sum(count), win_start=start, win_end = end, 
                win_days = as.Date(end,"%Y-%m-%d") - as.Date(start,"%Y-%m-%d")) %>%
      as.data.frame()
   
}

#function test
# occurance_distribution <- occ_dist(dat= house, start="2017-01-01", 
                                   # end="2018-01-01")

#3. Loop over data windows

occ_sum <- c()
occ_dis <- c()

for(y_end in 1994:2018){

for (y in 1993:2018) {
   occurance_summary <- occ_summary(dat= house, start=paste0(as.character(y),"-01-01"), 
                                    end=paste0(as.character(y_end),"-01-01"))
   cat(paste("\nRunning summary years:",y,"to",y_end))
   occ_sum <- rbind(occ_sum,occurance_summary)
   
   occurance_distribution <- occ_dist(dat= house, start=paste0(as.character(y),"-01-01"), 
                                      end=paste0(as.character(y_end),"-01-01"))
   cat(paste("\nRunning distribtuion years:",y,"to",y_end))
   occ_dis <- rbind(occ_dis,occurance_distribution)
}}

occ_sum2 <- filter(occ_sum, win_days > 0)
occ_dis2 <- filter(occ_dis, win_days > 0)

#3.1 Export data

write.csv(occ_sum2,paste0(root_dir, "/data_intermediate/occurance_summary.csv"), row.names=FALSE)
write.csv(occ_dis2,paste0(root_dir, "/data_intermediate/occurance_distribution.csv"), row.names=FALSE)

### END
