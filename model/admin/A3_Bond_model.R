#-----------------------------------------------------------------------------------------------------------------
#  Bond_model Script
#  
#  Alan Bentley, April 2018
#
#  Runs some models of rent price inflation
#
#
#------------------------------------------------------------------------------------------------------------------

library(MatrixModels) #For liner model (with sparse matrix)
library(zoo) #For dates (dealing with quarters)
library(dplyr) #For data manipulation
library(tidyr)
library(ggplot2)
library(scales)
library(tibble)

#1. Data prep

#1.1 Load the data
root_dir <- "//wd.govt.nz/dfs/SharedData/Wellington/Bowen Street/R&N/Networks/Housing Information & Modelling/housing_projects/cpi_rent/"

load(file = paste0(root_dir, "/data_intermediate/house_long.rda"))

#1.2 Keep the variables needed
mod_data <- select(house, Rent, quarter, quarter_f, PropertyID, Property, TA,SAU, Bedrooms, Property_Type, Date.Lodged, Date.Closed) %>%
   filter(Property_Type !="UNK" & SAU !=-1 & Bedrooms !="unknown") %>%
   mutate(date = as.Date(quarter),
          month = format(as.Date(Date.Lodged),"%Y-%m"),
          month_f = as.factor(month))

#####

#1.3.1 Take a sample of Properties (to test the code faster)
# set.seed(67)
# propsamp <- as.character(sample(unique(mod_data$PropertyID), 10000))
# 
# mod_data <- mod_data %>%
#    filter(PropertyID %in% propsamp)


#1.4 determine active bonds (ie. open and not closed, or open and unknown end date)

quarters <- unique(mod_data$quarter_f) #list of unique quarters
months <- unique(mod_data$month_f) #list of unique months

wgt_active <- c()
for (q in levels(quarters)) {
   active <- mod_data %>%
      filter(as.yearqtr(Date.Lodged)<=q & as.yearqtr(Date.Closed)>=q|
                as.yearqtr(Date.Lodged)<=q & is.na(Date.Closed)) %>%
      group_by(TA, Bedrooms) %>% #TA by bedrooms by property type weighted
      summarise(active=n(),
                active_exp=sum(Rent),
                quarter=q)
   wgt_active <- rbind(wgt_active,active)
}

#1.5 sum lodged bonds and lodged bond 'expenditure'
wgt_current <- mod_data %>%
   group_by(quarter_f, TA, Bedrooms) %>%
   summarise(lodged=n(), exp=sum(Rent)) %>%
   arrange(desc(exp)) %>%
   mutate(quarter=as.character(quarter_f)) %>%
   ungroup() %>%
   select(-quarter_f) %>%
   as.data.frame()

#1.6 Add weights to mod_data
wgt <- left_join(wgt_active, wgt_current, by=c("quarter","TA","Bedrooms"))

#1.7 Save weights
write.csv(wgt,paste0(root_dir, "/data_intermediate/wgt2.csv"), row.names=FALSE)

#1.8 Add weights to bond data
mod_data2 <- left_join(mod_data, wgt %>% rename(quarter_f=quarter), by=c("quarter_f","TA","Bedrooms")) %>%
               mutate(quarter_f = as.factor(quarter),
                      sampleweight=active_exp/exp) %>%
            group_by(quarter) %>%
            mutate(wi=Rent/sum(Rent), #expenditure share
                   wis=Rent*sampleweight/(sum(Rent*sampleweight))) #'stock-sample weighted' exp share

#2. Run the model 

#2.1 Property fixed-effects model

#2.1.1 Create function Time Product Dummy (TPD) function

# MatrixModel::glm4 is used as sparse=T option needs much less memory than base lm

TPD <- function (data, weights=NULL) {
   period <- sort(unique(data$quarter_f)) #list of unique time periods
   td <- exp(unname(coef(glm4(log(Rent) ~ quarter_f + PropertyID, weights=weights, data=data, sparse=T)))[2:length(period)])
   index <- c(1,td) #Add initial time period (excluded from regression to identify model)
   data.frame(period,index) # Join time periods and index, as a data frame
}

#2.1.2 Test function using all the data and OLS
mod <- TPD(data=mod_data2) %>% mutate(wgt_type="OLS")

#2.1.3 Assess sensitivity to different types of weighting
mod2 <- TPD(data=mod_data2, weights=mod_data2$wi) %>% mutate(wgt_type="expshare")
mod2b <- TPD(data=mod_data2, weights=sqrt(mod_data2$wi)) %>% mutate(wgt_type="sqrt_expshare")
mod3 <- TPD(data=mod_data2, weights=mod_data2$wis) %>% mutate(wgt_type="sample_expshare")
mod3b <- TPD(data=mod_data2, weights=sqrt(mod_data2$wis)) %>% mutate(wgt_type="sqrt_sample_expshare")

modwgt <- rbind(mod,mod2,mod2b,mod3,mod3b) %>%
   group_by(wgt_type) %>%
   mutate(change.a=index/lag(index,4)-1)

write.csv(modwgt,paste0(root_dir, "/data_intermediate/models_wgt.csv"), row.names=FALSE)

#2.2 Use rolling data window (test various window lengths)

#2.2.1 Create function to subset data (create data window) and append these into one file

#Rolling Window, multilateral model
RW_MLM <- function (data, lenwin) {
   
   rw_mod <- c()
   
   lenwin_c <- lenwin-1 #number of period-on-period changes in window
   numwin <- length(unique(data$date))-lenwin_c #number of windows
   
   for (i in 1:numwin){
      
      window_data <- subset(data,date >= sort(unique(data$date))[i] & 
                                      date <= sort(unique(data$date))[i+lenwin_c])
      
      cat(paste("Window", i, "of", numwin, "(Window length = ", lenwin, "quarters) \n")) #show progress
      
      m2 <- TPD(data=window_data, weights=window_data$wi) %>%
         rownames_to_column("pos") %>%
         mutate(ratio_change.q = index/lag(index,1),
                align = ifelse(pos=="2","start",
                               ifelse(pos==as.character(trunc(lenwin/2)+1),"mid",
                                      ifelse(pos==as.character(lenwin),"end","other"))),
                window = i,
                length = lenwin)
      
      rw_mod <- rbind(rw_mod, m2)
      
   }
   return(rw_mod)
   }

#2.2.2 test the function (using one window length)
# MWM <- RW_MLM(mod_data2, 32)

#2.3.1 Run model for different data window lengths

rw_models <- c()

for (lenwin in 2:90) {
   MWM <- RW_MLM(mod_data2, lenwin)
   rw_models <- rbind(rw_models, MWM)
} 

#2.3.2 Tidy up alignment naming

#2.3.2.1 For window length = 2, align has been labelled "start" yet this is also "end" and "mid" so add these on
l2extra_end <- subset(rw_models, length ==2) %>%
   mutate(align = "end")
l2extra_mid <- subset(rw_models, length ==2) %>%
   mutate(align = "mid")

#2.3.2.2 For window length = 3, align labelled "start" is also "mid" so add this on
l3extra_mid <- subset(rw_models, length ==3 & align =="start") %>%
   mutate(align = "mid")

rw_models <- rbind(rw_models, l2extra_end, l2extra_mid, l3extra_mid)

#2.3.3 Save the different models (windows of different length and over time)

write.csv(rw_models,paste0(root_dir, "/data_intermediate/models2.csv"), row.names=FALSE)

#2.4 Chain time series

#2.4.1 Calc geometric mean of ratio_change, across rolling windows (of given length)
rw_models_mean <- rw_models %>%
   filter(pos!="1") %>%
   group_by(period, length) %>%
   summarise(ratio_change.q = exp(mean(log(ratio_change.q), na.rm=T)),
             align="mean", pos="mean") %>%
   as.data.frame()

rw_models_b <- rw_models %>%
   select(-index, -window) %>%
   mutate(pos=as.character(pos))

rw_models2 <- rbind(rw_models_b,rw_models_mean)

#2.4.2 Chain quarterly series
rw_models3 <- rw_models2 %>%
   filter(pos!="1") %>%
   group_by(length,pos,align) %>%
   mutate(index2 = cumprod(ratio_change.q),
          change.a = index2/lag(index2,4)-1,
          change.q = ratio_change.q-1)

#2.4.3 Save chained models (windows of different length and index chain alignment position)
write.csv(rw_models3,paste0(root_dir, "/data_intermediate/models_chained2.csv"), row.names=FALSE)

#3.1 Apply splice (to include 'no revision' of historical series constraint)

#3.1.1 Calculate and apply revision factors

#3.1.1.1 Loop over window lengths and overlap positions

newwin <- c()

for(lenwin in 2:80) {
   
   for(posi in 2:(lenwin-1)){
      
      oldwin_p <- rw_models %>%
         ungroup() %>%
         subset(length==lenwin) %>%
         filter(align =="end" |pos==as.character(posi)) %>%
         group_by(window) %>%
         mutate(commonpre_pi = index/lag(index,1), #common movement in old window (for splice position, i)
                window_plus1 = as.integer(window+1),
                linkpos = posi) %>%
         ungroup() %>%
         subset(align=="end") %>%
         select(window_plus1,length,commonpre_pi, linkpos) %>%
         rename(window=window_plus1)
      
      newwin_p <- rw_models %>%
         ungroup() %>%
         subset(length==lenwin) %>%
         filter(pos==as.character(lenwin-1) |pos==as.character(posi-1)) %>%
         merge(oldwin_p,by=c("window","length")) %>%
         group_by(window) %>%
         mutate(common_pi = ifelse(linkpos==2,index,
                                   index/index[pos==as.character(posi-1)]), #common movement in new window (for splice position, i)
                pos_splice = common_pi/commonpre_pi) %>% #splice revision factor (for splice position, i)
         subset(pos==as.character(lenwin-1)) %>%
         select(window,length,common_pi, pos_splice, linkpos)
      
      overlap <- rw_models %>%
         ungroup() %>%
         subset(length==lenwin) %>%
         filter(align =="end") %>%
         merge(newwin_p,by=c("window","length")) %>%
         group_by(window) %>%
         mutate(     pos_splice_rc.q = ratio_change.q*pos_splice) %>% #price change for period (for splice position, i)
         subset(align =="end") %>%
         select(length,window,period,linkpos, pos_splice_rc.q)
      
      newwin <- rbind(newwin,overlap)
      
   }}

#3.1.1.2 Mean splice
meanwin <- newwin %>%
   ungroup() %>%
   group_by(length,period) %>%
   summarise(mean_splice_rc.q = exp(mean(log(pos_splice_rc.q), na.rm=T))) #Geometric mean of all possible overlaps

meanwin2 <- meanwin %>%
   group_by(length) %>%
   arrange(period) %>%
   mutate(index = cumprod(mean_splice_rc.q),
          change.a = index/lag(index,4)-1,
          splice_type="Mean")

#3.1.1.3 Add spliced indexes (including revision catch-up factor) to original models

splice_gen <- right_join(rw_models,newwin,by=c("length","window","period"))

#3.1.1.4 Add matching columns for no revision (end alignment)

splice_gen_end <- filter(rw_models, align=="end") %>%
                     filter(window!="1") %>%
                     mutate(linkpos=length,
                            pos_splice_rc.q=ratio_change.q)

splice_gen_end2 <- rbind(splice_gen, splice_gen_end)

#3.1.2 Chain spliced series

splice_gen2 <- subset(splice_gen_end2, !is.na(pos_splice_rc.q) & align=="end") %>% #Keep end aligned as revision factors have been applied
   group_by(length,linkpos) %>%
   mutate(date=as.Date(as.yearqtr(period))) %>%
   arrange(date,length,window,linkpos) %>%
   mutate(index2_pos = cumprod(pos_splice_rc.q)) %>%
   as.data.frame()


splice_gen3 <- select(splice_gen2,period,window,length,index2_pos, linkpos) %>%
   group_by(length, linkpos) %>%
   mutate(date=as.Date(as.yearqtr(period))) %>%
   arrange(date,length,window,linkpos) %>%
   rename(index=index2_pos) %>%
   mutate(change.a = index/lag(index,4)-1,
          splice_type=ifelse(linkpos==2,"Window",
                             ifelse(linkpos==trunc(length/2)+1,"Half Window",
                                    ifelse(linkpos==length,"Movement",
                                           "Other")))) %>%
   ungroup()

splice_gen4 <- splice_gen3 %>%
         select(-linkpos, -date)

splicemean <- select(meanwin2, -mean_splice_rc.q) %>%
   mutate(splice_type = "Mean",
          window = "Mean") %>%
   as.data.frame()

splice_gen5 <- rbind(splice_gen4, splicemean)

write.csv(splice_gen5,paste0(root_dir, "/data_intermediate/models_splice2.csv"), row.names=FALSE)

#4. Run for each region

#4.1 Get higher aggregate regions

#4.1.1 use geo classification from mbie Bond Data on website

geo <- read.csv(url("http://www.mbie.govt.nz/info-services/housing-property/sector-information-and-statistics/rental-bond-data/Geographical%20Table.csv"))

#4.1.2 create CPI regions (custom aggregation of regional councils)

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
mod_data3 <- left_join(mod_data2,geocpi, by="SAU")

cpi_regions <- unique(mod_data3$cpi_region)
cpi_regions <- subset(cpi_regions,!is.na(cpi_regions))

#4.2 Run model for each cpi region

rw_models_reg <- c()

for (region in cpi_regions) {
   for (lenwin in 2:90) {
      #Run model for region
   MWM_reg <- mod_data3 %>%
      filter(cpi_region==region) %>%
      RW_MLM(lenwin=lenwin) %>%
      mutate(region=region)
   rw_models_reg <- rbind(rw_models_reg, MWM_reg)
   }}


write.csv(rw_models_reg,paste0(root_dir, "/data_intermediate/models_r5.csv"), row.names=FALSE)

#4.2.2 Split file for size

rw_models_rega <- filter(rw_models_reg, region==unique(rw_models_reg$region)[1])
rw_models_regb <- filter(rw_models_reg, region==unique(rw_models_reg$region)[2])
rw_models_regc <- filter(rw_models_reg, region==unique(rw_models_reg$region)[3])
rw_models_regd <- filter(rw_models_reg, region==unique(rw_models_reg$region)[4])
rw_models_rege <- filter(rw_models_reg, region==unique(rw_models_reg$region)[5])

write.csv(rw_models_rega,paste0(root_dir, "/data_intermediate/models_r5a.csv"), row.names=FALSE)
write.csv(rw_models_regb,paste0(root_dir, "/data_intermediate/models_r5b.csv"), row.names=FALSE)
write.csv(rw_models_regc,paste0(root_dir, "/data_intermediate/models_r5c.csv"), row.names=FALSE)
write.csv(rw_models_regd,paste0(root_dir, "/data_intermediate/models_r5d.csv"), row.names=FALSE)
write.csv(rw_models_rege,paste0(root_dir, "/data_intermediate/models_r5e.csv"), row.names=FALSE)


#4.3 Geometric mean of ratio_change, across rolling windows (of given length)
rw_models_mean_r <- rw_models_reg %>%
   filter(pos!="1") %>%
   group_by(period, length, region) %>%
   summarise(ratio_change.q = exp(mean(log(ratio_change.q), na.rm=T)),
             align="mean", pos="mean") %>%
   as.data.frame()

rw_models_b_r <- rw_models_reg %>%
   select(-index, -window) %>%
   mutate(pos=as.character(pos))

rw_models2_r <- rbind(rw_models_b_r,rw_models_mean_r)

#4.4 Chain quarterly series
rw_models3_r <- rw_models2_r %>%
   filter(pos!="1") %>%
   group_by(length,pos,align, region) %>%
   mutate(index2 = cumprod(ratio_change.q),
          change.a = index2/lag(index2,4)-1,
          change.q = ratio_change.q-1)

write.csv(rw_models3_r,paste0(root_dir, "/data_intermediate/models_chained_r5.csv"), row.names=FALSE)

#4.4.2 Split file for size

rw_models3_ra <- filter(rw_models3_r, region==unique(rw_models3_r$region)[1])
rw_models3_rb <- filter(rw_models3_r, region==unique(rw_models3_r$region)[2])
rw_models3_rc <- filter(rw_models3_r, region==unique(rw_models3_r$region)[3])
rw_models3_rd <- filter(rw_models3_r, region==unique(rw_models3_r$region)[4])
rw_models3_re <- filter(rw_models3_r, region==unique(rw_models3_r$region)[5])

write.csv(rw_models3_ra,paste0(root_dir, "/data_intermediate/models_chained_r5a.csv"), row.names=FALSE)
write.csv(rw_models3_rb,paste0(root_dir, "/data_intermediate/models_chained_r5b.csv"), row.names=FALSE)
write.csv(rw_models3_rc,paste0(root_dir, "/data_intermediate/models_chained_r5c.csv"), row.names=FALSE)
write.csv(rw_models3_rd,paste0(root_dir, "/data_intermediate/models_chained_r5d.csv"), row.names=FALSE)
write.csv(rw_models3_re,paste0(root_dir, "/data_intermediate/models_chained_r5e.csv"), row.names=FALSE)

### END
