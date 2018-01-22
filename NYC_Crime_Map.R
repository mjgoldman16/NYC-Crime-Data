library(data.table)
library(dplyr)
library(chron)
#D[using i, calculate j, grouped by k]

#Loading in in the original file. 5580035 rows
# nyc_crimes_original = fread(input="D:/NYC-Data-Science/Shiny Project/Data/NYPD_Complaint_Data_Historic.csv",
#                    header=TRUE)

#***TESTING
nyc_crimes = nyc_crimes_original

#Removing columsn that are not relevant to mapping of the crime location
nyc_crimes[,c("CMPLNT_NUM","CMPLNT_TO_DT","CMPLNT_TO_TM","RPT_DT",
              "CRM_ATPT_CPTD_CD","JURIS_DESC","ADDR_PCT_CD","LOC_OF_OCCUR_DESC", 
              "X_COORD_CD","Y_COORD_CD", "Lat_Lon") := NULL]

#Only want felonies

nyc_crimes = nyc_crimes[LAW_CAT_CD == "FELONY"]

#***TESTING
nyc_felony_desc = nyc_crimes %>% distinct(KY_CD, OFNS_DESC, PD_CD, PD_DESC) %>% 
  arrange(KY_CD)
nyc_felony_desc = as.data.table(nyc_felony_desc)

#Removing rows that do not have a lattitude or longittude(?) No. Want to save them for
# the stats page. Maybe make a separate table to do a maps table and also make a stats table?
# if not, have the map have an auto filter for observations that have both lat and long

#Want to make it so you have the name of the public housing. 
# (there are a few instances where they don't have a name, so it should just say Public Housing or Park)

#Include the public housing name and/or park name, if available. 
nyc_crimes[,PREM_TYP_DESC := ifelse(PREM_TYP_DESC == "RESIDENCE - PUBLIC HOUSING" & HADEVELOPT != "",
                                    paste(PREM_TYP_DESC,HADEVELOPT,sep= " - "),
                                    ifelse(PREM_TYP_DESC == "PARK/PLAYGROUND" & PARKS_NM != "",
                                           paste(PREM_TYP_DESC,PARKS_NM,sep= " - "),
                                           PREM_TYP_DESC))]

#lowering down to just get the 7 major crimes: Murder, Rape, Assault, Burglary, Robbery, 
# Grand Larceny, and Grand Larceny Auto. Filtering on key since some OFNS_DESC have missing
# entries but filled keys (eg: 105, 106, 109). Also has additional PD_DESC (more detailed 
# description) which includes some of these crimes, but are limited by a different KY_CD key.
# eg(PD_CD = RAPE 1, but CD is 233 is 104)
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

#applying functions
nyc_felony_desc = nyc_felony_desc[PD_CD == 157, c("KY_CD", "OFNS_DESC") := fix_RA()]
nyc_felony_desc = nyc_felony_desc[PD_CD == 109, c("KY_CD", "OFNS_DESC") := fix_AS()]
nyc_felony_desc = nyc_felony_desc[PD_CD == 397, c("KY_CD", "OFNS_DESC") := fix_RO()]
nyc_felony_desc = nyc_felony_desc[PD_CD == 405 | PD_CD == 438, c("KY_CD", "OFNS_DESC") := fix_GL()]


#Cleaning the dates and times. Dropping data before 1/1/2006 (likely inaccurate)
nyc_crimes$DATE = as.Date(nyc_crimes$CMPLNT_FR_DT,"%m/%d/%Y")
nyc_crimes = nyc_crimes[DATE>"2005-12-31" & !(is.na(DATE))]

#Edit time to be a time factor
#[xx]*** THIS SEEMS REDUNDANT. WILL ASK ABOUT IT
nyc_crimes$TIME = format(as.POSIXct(nyc_crimes$CMPLNT_FR_TM,format="%H:%M:%S"),
                         format="%H:%M:%S")

#Some data is marked as midnight the 24:00:00. Convert this to be midnight the same day, 
#rather than out of bounds
nyc_crimes = nyc_crimes[is.na(TIME)& CMPLNT_FR_TM=="24:00:00",TIME:=0]
nyc_crimes = setorder(nyc_crimes,DATE,TIME)

nyc_crimes[,c("DOW"):=c(weekdays(DATE))]
nyc_crimes = nyc_crimes[,c("PARKS_NM","HADEVELOPT","LAW_CAT_CD","CMPLNT_FR_DT","CMPLNT_FR_TM") := NULL]
nyc_crimes = setcolorder(nyc_crimes, c("DATE", "TIME", "DOW","KY_CD", "OFNS_DESC", "PD_CD", "PD_DESC", 
                                       "BORO_NM", "PREM_TYP_DESC", "Latitude", "Longitude"))

nyc_crimes[,.N,by=.(OFNS_DESC,year(DATE))]

write.csv(nyc_crimes, "D:/NYC-Data-Science/Shiny Project/Data/NYC_CRIMES_SEMICLEAN.csv")

View(nyc_crimes)

View(nyc_felony_desc)

#COMPARING DATA TO NYC 
