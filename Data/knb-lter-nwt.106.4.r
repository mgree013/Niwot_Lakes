# Package ID: knb-lter-nwt.106.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake ice clearance and formation data for Green Lakes Valley, 1968 - ongoing.
# Data set creator:  T. Nelson (Nel) Caine -  
# Data set creator:  Jennifer F. Morse -  
# Data set creator:    - Niwot Ridge LTER 
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/106/4/b402de1219b6be34708d5aeb0f77fc63" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "year",     
                    "lake",     
                    "date",     
                    "yday",     
                    "ice_cover",     
                    "breaking_ice",     
                    "complete_ice_clearance",     
                    "complete_ice_formation",     
                    "notes"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$lake)!="factor") dt1$lake<- as.factor(dt1$lake)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$yday)=="factor") dt1$yday <-as.numeric(levels(dt1$yday))[as.integer(dt1$yday) ]               
if (class(dt1$yday)=="character") dt1$yday <-as.numeric(dt1$yday)
if (class(dt1$ice_cover)=="factor") dt1$ice_cover <-as.numeric(levels(dt1$ice_cover))[as.integer(dt1$ice_cover) ]               
if (class(dt1$ice_cover)=="character") dt1$ice_cover <-as.numeric(dt1$ice_cover)
if (class(dt1$breaking_ice)!="factor") dt1$breaking_ice<- as.factor(dt1$breaking_ice)
if (class(dt1$complete_ice_clearance)!="factor") dt1$complete_ice_clearance<- as.factor(dt1$complete_ice_clearance)
if (class(dt1$complete_ice_formation)!="factor") dt1$complete_ice_formation<- as.factor(dt1$complete_ice_formation)
if (class(dt1$notes)!="factor") dt1$notes<- as.factor(dt1$notes)
                
# Convert Missing Values to NA for non-dates
                
dt1$breaking_ice <- as.factor(ifelse((trimws(as.character(dt1$breaking_ice))==trimws("NaN")),NA,as.character(dt1$breaking_ice)))
dt1$complete_ice_clearance <- as.factor(ifelse((trimws(as.character(dt1$complete_ice_clearance))==trimws("NaN")),NA,as.character(dt1$complete_ice_clearance)))
dt1$complete_ice_formation <- as.factor(ifelse((trimws(as.character(dt1$complete_ice_formation))==trimws("NaN")),NA,as.character(dt1$complete_ice_formation)))
dt1$notes <- as.factor(ifelse((trimws(as.character(dt1$notes))==trimws("NaN")),NA,as.character(dt1$notes)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year)
summary(lake)
summary(date)
summary(yday)
summary(ice_cover)
summary(breaking_ice)
summary(complete_ice_clearance)
summary(complete_ice_formation)
summary(notes) 
                # Get more details on character variables
                 
summary(as.factor(dt1$lake)) 
summary(as.factor(dt1$breaking_ice)) 
summary(as.factor(dt1$complete_ice_clearance)) 
summary(as.factor(dt1$complete_ice_formation)) 
summary(as.factor(dt1$notes))
detach(dt1)               
        




