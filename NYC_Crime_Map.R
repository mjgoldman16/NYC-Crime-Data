library(data.table)
library(dplyr)
library(chron)
library(tidyr)
#D[using i, calculate j, grouped by k]

###TO DO:
#ADD IN BORO AND CRIME GRAPHS
#FILTER SO ALL DATA IS PROPER CASE/NOT A BROKEN CAPSLOCK.(USE REGULAR EXPRESSIONS TO MAKE SURE SPACES ARE ALL SET)
#*ADD ABOUT THE DATA/ABOUT ME PAGE
#HEATMAP OF CRIMES
#DON'T SHOW THE ADDED COLUMNS IN THE DT TABLE
#

###STRETCH GOALS:
#reverse look up 
#Alter map color to something nicer
#make it so boro and crime stats are just one tab with an absolute panel on top to pick which one you want/what to focus on
#
#
#


#Loading in in the original file. 5580035 rows
#Commited so you don't have to re-load the table into R. Just use the _original
nyc_crimes_original = fread(input="D:/NYC-Data-Science/Shiny-Project/Data/NYPD_Complaint_Data_Historic.csv",
                   header=TRUE)

#In order to keep the original file
nyc_crimes = nyc_crimes_original

#Removing columsn that are not relevant to mapping of the crime location
nyc_crimes[,c("CMPLNT_NUM","CMPLNT_TO_DT","CMPLNT_TO_TM","RPT_DT",
              "CRM_ATPT_CPTD_CD","JURIS_DESC","ADDR_PCT_CD","LOC_OF_OCCUR_DESC", 
              "X_COORD_CD","Y_COORD_CD", "Lat_Lon") := NULL]

#Only want felonies
nyc_crimes = nyc_crimes[LAW_CAT_CD == "FELONY"]

#***testing accuracy of the data to see if there are missing/inconsistent columns
nyc_felony_desc = nyc_crimes %>% distinct(KY_CD, OFNS_DESC, PD_CD, PD_DESC) %>% 
  arrange(KY_CD)
nyc_felony_desc = as.data.table(nyc_felony_desc)

#Want to make it so you have the name of the public housing. 
# (there are a few instances where they don't have a name, so it should just say Public Housing or Park)

#Include the public housing name and/or park name, if available. 
nyc_crimes[,PREM_TYP_DESC := ifelse(PREM_TYP_DESC == "RESIDENCE - PUBLIC HOUSING" & HADEVELOPT != "",
                                    paste(PREM_TYP_DESC,HADEVELOPT,sep= " - "),
                                    ifelse(PREM_TYP_DESC == "PARK/PLAYGROUND" & PARKS_NM != "",
                                           paste(PREM_TYP_DESC,PARKS_NM,sep= " - "),
                                           PREM_TYP_DESC))]

#Filtering down to just get the 7 major crimes: Murder, Rape, Assault, Burglary, Robbery, 
# Grand Larceny, and Grand Larceny Auto. Filtering on KY_CD since some OFNS_DESC have missing
# entries but filled keys (eg: 105, 106, 109). Also has additional PD_DESC  filters (more detailed 
# description) which includes some of these crimes, but different KY_CD key. eg(PD_CD = RAPE 1, but CD 
# is 233 is 104).

#KY_CD code:
#101 = Murder/non-neglegent manslaughter, 104 = rape, 105 = robbery, 106 = felony assault
# 107 = burglary, 109 = gran larceny, 110 = grand larceny of motor vehicle
#Other interesting crimes to potentially look at: 124 = kidnapping, 119 = DUI, 117 = dangerous drugs
# 118 = dangerous weapons

                          #Murders
nyc_crimes = nyc_crimes[KY_CD == 101 |
                          #Rapes
                          KY_CD == 104 |
                          PD_CD == 157 | #Rape 1 that was misclassified 
                          #Assault
                          KY_CD == 106 |
                          PD_CD == 109 | #Assault 2,1, unclassified  that was misclassified
                          #Burglary
                          KY_CD == 107 |
                          #Robbery
                          KY_CD == 105 | #an instance of Grand larceny with a robbery CD code. PD_CD = 419 ***WILL FIX***
                          PD_CD == 397 | #Robbery, open area unclassified that was misclassified
                          #Grand Larceny (including auto)
                          KY_CD == 109 |
                          KY_CD == 110 |
                          PD_CD == 405 | # Grand larceny by credit card theft that was misclassified
                          PD_CD == 438]  # Grand larceny from unattended building that was misclassified 

##Fixing input errors
#creating (very depressing) functions to fix the incorrect input
fix_RA = function() { list(104, "RAPE")}
fix_AS = function() { list(106, "FELONY ASSAULT")}
fix_RO = function() { list(105, "ROBBERY")}
fix_GL = function() { list(109, "GRAND LARCENY")}
fix_GLA = function(){ list(110, "GRAND LARCENY OF MOTOR VEHICLE")}

#applying functions
nyc_crimes = nyc_crimes[PD_CD == 157, c("KY_CD", "OFNS_DESC") := fix_RA()]
nyc_crimes = nyc_crimes[PD_CD == 109, c("KY_CD", "OFNS_DESC") := fix_AS()]
nyc_crimes = nyc_crimes[PD_CD == 397, c("KY_CD", "OFNS_DESC") := fix_RO()]
nyc_crimes = nyc_crimes[PD_CD == 405 | PD_CD == 438 | PD_CD == 419, c("KY_CD", "OFNS_DESC") := fix_GL()]
nyc_crimes = nyc_crimes[PD_CD == 441, c("KY_CD", "OFNS_DESC") := fix_GLA()]

#Entries of fraud/theft were misclassified. Similar for remove public administration/unclassified and
#kidnapping. Don't want to drop murders though (since their PD_CD is NA)
nyc_crimes = nyc_crimes[!(PD_CD == 739 | PD_CD == 779 | PD_CD == 186) | is.na(PD_CD)]


#Cleaning the dates and times. Dropping data before 1/1/2006
nyc_crimes$DATE = as.Date(nyc_crimes$CMPLNT_FR_DT,"%m/%d/%Y")
nyc_crimes = nyc_crimes[DATE>"2005-12-31" & !(is.na(DATE))]

#Edit time to be a time factor
#[xx]*** THIS SEEMS REDUNDANT. WILL ASK ABOUT IT
nyc_crimes$TIME = format(as.POSIXct(nyc_crimes$CMPLNT_FR_TM,format="%H:%M:%S"),
                         format="%H:%M:%S")
nyc_crimes$HOUR = as.numeric(substr(nyc_crimes$TIME,1,2))

#Some data is marked as midnight incorrectly (24:00:00). Convert this to be midnight the same day (00:00:00)
#rather than out of bounds
nyc_crimes = nyc_crimes[is.na(TIME)& CMPLNT_FR_TM=="24:00:00",TIME:=0]
nyc_crimes = setorder(nyc_crimes,DATE,TIME)


nyc_crimes[,c("TIME_OF_DAY"):= ifelse(TIME<"05:59:59", "Early Morning",
                                      ifelse(TIME < "11:59:59", "Morning",
                                             ifelse(TIME < "17:59:59", "Afternoon",
                                                    "Evening")))]

#Include the day of the week for the crimes
nyc_crimes[,c("DOW"):=c(weekdays(DATE))]
nyc_crimes$DOW = factor(nyc_crimes$DOW, levels = c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
nyc_crimes[,c("YEAR"):=c(year(DATE))]
nyc_crimes[,c("MONTH"):=c(month(DATE))]
nyc_crimes$MONTH = factor(month.name[nyc_crimes$MONTH], levels=month.name[1:12])
nyc_crimes[,c("MONTH_YEAR"):=paste(MONTH,YEAR)]
# nyc_crimes$MONTH = levels("January", "February",)
#Dropping the data with no borough label (only ~250 observations, 7 of which have lat/long data, so won't affect mapping much)
nyc_crimes = nyc_crimes[!(BORO_NM=="")]


#Delete columns that are combined together or that were copied
nyc_crimes = nyc_crimes[,c("PARKS_NM","HADEVELOPT","LAW_CAT_CD","CMPLNT_FR_DT","CMPLNT_FR_TM", "KY_CD", "PD_CD") := NULL]
nyc_crimes = setcolorder(nyc_crimes, c("DATE", "YEAR", "MONTH", "MONTH_YEAR", "TIME", "HOUR", "TIME_OF_DAY", "DOW", "OFNS_DESC", "PD_DESC", 
                                       "BORO_NM", "PREM_TYP_DESC", "Latitude", "Longitude"))

#ADDING IN POPULATION DATA
pop_data = fread(input="D:/NYC-Data-Science/Shiny-Project/Data/NYC borough population.csv",
                            header=TRUE)
pop_data = pop_data[,r:=NULL]
pop_data = gather(pop_data, key="YEAR",value="POPULATION", 2:13)
pop_data$YEAR = as.numeric(pop_data$YEAR)
pop_data$POPULATION = as.numeric(pop_data$POPULATION)
nyc_crimes = left_join(nyc_crimes,pop_data,by = c("YEAR","BORO_NM"))


write.csv(nyc_crimes, "D:/NYC-Data-Science/Shiny-Project/Data/NYC_CRIMES_SEMICLEAN.csv")
# saveRDS(nyc_crimes, )
#save to rds file 

# View(nyc_crimes)

#still not an exact match to the summary data listed on the website:
#http://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/seven-major-felony-offenses-2000-2016.xls

