#-----------------------------------------------------------------------------------------------------------------
#  HOUSE Load *Long* Script (Long version of the script uses Lodged Bond view to get longer timeseries, pre-2000)
#  
#  Alan Bentley, January 2018
#
#  Based on Zac Clark's house load script used for the urban development capacity measures
#
#  //wd.govt.nz/dfs/SharedData/Wellington/Bowen Street/R&N/Networks/Housing Information & Modelling/housing_projects/NPS_UDC/grooming_code/HOUSE_load.R
#
#------------------------------------------------------------------------------------------------------------------
  
library(RODBC) #For reading the database
library(zoo) #For dates (dealing with quarters)
library(dplyr) #For data manipulation

root_dir <- "//wd.govt.nz/dfs/SharedData/Wellington/Bowen Street/R&N/Networks/Housing Information & Modelling/housing_projects/cpi_rent/"

#1. Read the data

# Lodged Bond view is used to get a longer time series

# Meshblock coding (from the address field) is condidered better using IDI consistent data
# But data only available from 2000 onwards

SQL2 <- "SELECT [ID]
      ,[Rent]
,[Bond.Amount]
,[Date.Lodged]
,[Date.Closed]
,[SAU]
,[TA]
,[Region]
,[Bedrooms]
,[Landlord.Group]
,[Property.Type]
,[Property]
,[Vested_to_Crown]
,[Date.VestedtoCrown]
FROM [House_DWH].[dbo].[vw_Lodged_Bonds]"

# Read data from House
house <- sqlQuery(odbcConnect("EBT_Data1179"), SQL2, stringsAsFactors = TRUE)

#2. Tidy the data a bit

#2.1 Format date variables (Use lodgement dates as don't have tenancy start and end dates on lodged bonds view)

house$Date.Lodged <- as.Date(house$Date.Lodged,"%Y-%m-%d")
house$Date.Closed <- as.Date(house$Date.Closed,"%Y-%m-%d")

#2.2 Set rent values of 0 to NA (0 is a missing data code for this field)
house$Rent[house$Rent<=0] <- NA

#2.3 Group dwellings with 5 or more bedrooms into "5+" category and set 0 to unkwown 
house$Bedrooms <- as.numeric(house$Bedrooms)
house$Bedrooms[house$Bedrooms>4] <- "5+"
house$Bedrooms[house$Bedrooms==0] <- "unknown"
house$Bedrooms <- as.factor(house$Bedrooms)

#2.4 Make quarters
house$quarter <- as.yearqtr(house$Date.Lodged)
house$quarter_closed <- as.yearqtr(as.Date(house$Date.Closed,"%Y-%m-%d"))

#2.5 Create private bond subset 
#Include: private, private company, property manangement group, private trust, private trust
# Excludes: GOVT, Housing NZ, Not recorded, Local authoriy
house$Private_Bond <- house$Landlord.Group %in% c("PRV","PMC","PRC","PRT","PRO")

#2.6 Group property types up into broader categories 
house$Property_Type <- as.character(house$Property.Type )
house$Property_Type[house$Property.Type  %in% c("Flat", "Apartment")] <- "Flat or Apartment"
house$Property_Type[house$Property.Type  %in% c("Boarding house", "Room")] <- "Room or Boarding house"  
house$Property_Type[house$Property.Type  == "Not Recorded"] <- "unknown"
house$Property_Type <- factor(house$Property_Type)

#2.7 Remove duplicate rows due to multiple tenants, doesn't matter which row we choose
house <- house %>% distinct(bond_id, .keep_all = TRUE)

house <- subset(as.data.frame(house), as.Date(Date.Lodged,"%Y-%m-%d") >= "1993-01-01" #keep since
                & as.Date(Date.Lodged,"%Y-%m-%d") < "2018-01-01" #until 2018
                 & !is.na(Rent) #remove crital missing data
                 & !is.na(quarter_f) & !is.na(PropertyID)
                 # & TA != "Chatham Islands" #remove Chatham Island
                 & Vested_to_Crown == "N") #remove HNZ properties

#2.8 Make factors (for fixed-effects model)
house$quarter_f <- as.factor(house$quarter)
house$PropertyID <- as.factor(house$Property)

#3. Output the file
save(house, file = paste0(root_dir, "data_intermediate/house_long.rda"))

### END
