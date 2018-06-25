#-----------------------------------------------------------------------------------------------------------------
#  Bond_stock_model Script
#  
#  Alan Bentley, May 2018
#
#  Runs some models of rent price inflation, to minic a 'stock' approach
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

#0. Set root path

root <- "//wd.govt.nz/dfs/SharedData/Wellington/Bowen Street/R&N/Networks/Housing Information & Modelling/housing_reporting/cpi_rent"

#1. Data prep

load(file = paste0(root, "/data_intermediate/house.rda"))

house <- house %>%
   select(-c(ID, Date.VestedtoCrown, quarter, Bond.Amount))

#####

#1.3.1 Take a sample of Properties (to test the code faster)
set.seed(67)
propsamp <- as.character(sample(unique(house$PropertyID), 20000))

house_closed <- house %>%
   filter(PropertyID %in% propsamp) %>%
   mutate(PropertyID=as.factor(as.character(PropertyID))) %>%
   arrange(Date.Lodged) %>%
   filter(!is.na(Date.Closed))

house_notclosed <- house %>%
   filter(PropertyID %in% propsamp) %>%
   mutate(PropertyID=as.factor(as.character(PropertyID))) %>%
   arrange(Date.Lodged) %>%
   filter(is.na(Date.Closed)) %>%
   mutate(Date.Closed=as.Date("9999-01-01","%Y-%m-%d")) #Set unknown closed date

house <- rbind(house_closed,house_notclosed)

#1.4.1 Add all time periods
quarters <- unique(house$quarter_f) #list of unique quarters
quarter <- data.frame(quarters) %>%
   mutate(quarter_f=as.factor(quarters)) %>%
   select(-quarters)

#1.5.1 Carry forward opening bonds, unless closed
house_stock <- c()

for (p in levels(house$PropertyID)) {
   
   property_imp <- house %>%
      filter(PropertyID %in% p) %>%
      right_join(quarter,by="quarter_f") %>%
      mutate(q_closed=as.yearqtr(Date.Closed)) %>%
      na.locf() #Zoo::Last Observation Carried Forward
   # filter  (as.Date(Date.Lodged) >= as.Date(as.yearqtr(quarter_f))-(365.25*2)) #Cut off imputation after 2 years
   
   house_stock <- rbind(house_stock,property_imp)
}

house_stock <- house_stock %>%
   mutate(Rent=as.numeric(Rent),
          quarter=as.yearqtr(quarter_f),
          quarter_f=as.factor(quarter_f),
          PropertyID=as.factor(PropertyID),
          Property=as.numeric(Property),
          TA=as.factor(TA),
          SAU=as.integer(SAU),
          Bedrooms=as.factor(Bedrooms),
          Property_Type=as.factor(Property_Type),
          wi=Rent/sum(Rent),
          date = as.Date(quarter)) %>%
   select(-c(Date.Closed,q_closed))

mod_data <- select(house_stock, Rent, quarter, quarter_f, PropertyID, Property, TA,SAU, Bedrooms, Property_Type, Date.Lodged, Private_Bond) %>%
   filter(Property_Type !="UNK" & SAU !=-1 & Bedrooms !="unknown") %>%
   mutate(date = as.Date(quarter))

#1.2 Add weights to bond data
mod_data2 <- mod_data %>%
   group_by(quarter) %>%
   mutate(wi=Rent/sum(Rent)) #expenditure share, by quarter

#2. Create the model function 

#2.1 Property fixed-effects model, Time Product Dummy (TPD) function

# MatrixModel::glm4 is used as sparse=T option needs much less memory than base lm

# TPD <- function (data, weights=NULL, show_ave=F) {
#    period <- sort(unique(data$quarter_f)) #list of unique time periods
#    tpd <- exp(unname(coef(glm4(log(Rent) ~ quarter_f + PropertyID, weights=weights, data=data, sparse=T)))[2:length(period)])
#    td <- exp(unname(coef(glm4(log(Rent) ~ quarter_f, weights=weights, data=data, sparse=T)))[2:length(period)])
#    index <- c(1,tpd) #Add initial time period (excluded from regression to identify model)
#    geomean <- c(1,td) #Average price
#    if (show_ave) {
#       data.frame(period,index,geomean) 
#    }
#    else {
#    data.frame(period,index) # Join time periods and index, as a data frame
#    }
# }

TPD <- function (data, weights=NULL, show_ave=F) {
   period <- sort(unique(data$quarter_f)) #list of unique time periods
   tpd <- exp(unname(coef(
      tryCatch({
         glm4(log(Rent) ~ quarter_f + PropertyID, weights=weights, data=data, sparse=T)
      }, error=function(e) {
         glm(log(Rent) ~ quarter_f + PropertyID, weights=weights, data=data)
      })
   ))[2:length(period)])
   td <- exp(unname(coef(
      tryCatch({
         glm4(log(Rent) ~ quarter_f, weights=weights, data=data, sparse=T)
      }, error=function(e) {
         glm(log(Rent) ~ quarter_f, weights=weights, data=data)
      })
   ))[2:length(period)])
   index <- c(1,tpd) #Add initial time period (excluded from regression to identify model)
   geomean <- c(1,td) #Average price
   if (show_ave) {
      data.frame(period,index,geomean)
   }
   else {
      data.frame(period,index) # Join time periods and index, as a data frame
   }
}


#2.2 Create function to use rolling data window

#Rolling Window, multilateral model
RW_MLM <- function (data, lenwin) {
   
#2.2.1 Subset data (create data window) and append these into one file
   
   rw_models <- c()
   
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
      
      rw_models <- rbind(rw_models, m2)
   }
      
#2.2.2  Chain time series
   
   # Calc geometric mean of ratio_change, across rolling windows (of given length)
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
   
   #Chain quarterly series
   rw_models3 <- rw_models2 %>%
      filter(pos!="1") %>%
      group_by(length,pos,align) %>%
      mutate(index = cumprod(ratio_change.q)) %>%
      ungroup() %>%
      filter(align=="mean") %>% #keep mean chain alignment
      select(-c(pos,ratio_change.q,align,length))
   
   #Change in geometric mean (index series) for comparison
   ave <- TPD(data=data, weights=data$wi, show_ave=T) %>%
      rename(index_pooled=index)
   
   rw_models4 <- right_join(rw_models3,ave) %>%
      mutate(index=ifelse(is.na(index),1,index)) %>%
      rename(index_revisable=index)
   
   #2.2.3  Apply splice (to include 'no revision' of historical series constraint)
   
   #Calculate and apply revision factors
   
   #Loop over window lengths and overlap positions
   
   newwin <- c()
   
   for(lenwin in 32) {
      
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
   
   #Mean splice
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
   
   # Add spliced indexes (including revision catch-up factor) to original models
   
   splice_gen <- right_join(rw_models,newwin,by=c("length","window","period"))
   
   # Add matching columns for no revision (end alignment)
   
   splice_gen_end <- filter(rw_models, align=="end") %>%
      filter(window!="1") %>%
      mutate(linkpos=length,
             pos_splice_rc.q=ratio_change.q)
   
   splice_gen_end2 <- rbind(splice_gen, splice_gen_end)
   
   # Chain spliced series
   
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
      ungroup() %>%
      select(-linkpos, -date)
   
   splicemean <- select(meanwin2, -mean_splice_rc.q) %>%
      mutate(splice_type = "Mean",
             window = "Mean") %>%
      as.data.frame()
   
   splice_gen4 <- rbind(splice_gen3, splicemean) %>%
      filter(splice_type == "Mean" & window == "Mean") %>%
      select(-c(splice_type,window,length,change.a))
   
   rw_models5 <- left_join(rw_models4,splice_gen4)
   
   rw_models5$index[32] <- 1
   
   rw_models6 <- rw_models5 %>%
      mutate(index_non_revisable=ifelse(is.na(index),index_revisable, index*index_revisable[32])) %>%
      select(-index)
   
   rw_models7 <- gather(rw_models6, index_type, index, -period) 
   
   return(rw_models7)
   }

#3 (For stock) assess sensitivity to imputation length

rw_models_stock_sim <- c()

for (imp in c(1,2,3,4,5)) {
   for (lenwin in 32) {
      #Run model for region
      MWM_reg <- mod_data %>%
         filter (as.Date(Date.Lodged) >= as.Date(as.yearqtr(quarter_f))-(365.25*imp)) %>%
         group_by(quarter) %>%
         mutate(wi=Rent/sum(Rent)) %>% #expenditure share, by quarter
         ungroup %>%
         RW_MLM(lenwin=lenwin) %>%
         mutate(imp_length=imp)
      rw_models_stock_sim <- rbind(rw_models_stock_sim, MWM_reg)
   }}

root_dir <- "//wd.govt.nz/dfs/SharedData/Wellington/Bowen Street/R&N/Networks/Housing Information & Modelling/housing_projects/cpi_rent/"

write.csv(rw_models_stock_sim,paste0(root_dir, "/data_intermediate/stock_mod_sim3b.csv"), row.names=FALSE)

rw_models_stock_sim2 <- rw_models_stock_sim %>%
   filter(as.yearqtr(period)<="2017 Q4")

write.csv(rw_models_stock_sim2,paste0(root_dir, "/data_intermediate/stock_mod_sim4.csv"), row.names=FALSE)
