# Package ID: knb-lter-nwt.106.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake ice clearance and formation data for Green Lakes Valley from 1968 - ongoing..
# Data set creator:  T. Nelson (Nel) Caine -  
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/106/2/b402de1219b6be34708d5aeb0f77fc63" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "year",     
                    "lake",     
                    "break_jday",     
                    "break_date",     
                    "clear_jday",     
                    "clear_date",     
                    "form_jday",     
                    "form_date"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$lake)!="factor") dt1$lake<- as.factor(dt1$lake)
if (class(dt1$break_jday)=="factor") dt1$break_jday <-as.numeric(levels(dt1$break_jday))[as.integer(dt1$break_jday) ]               
if (class(dt1$break_jday)=="character") dt1$break_jday <-as.numeric(dt1$break_jday)                                   
# attempting to convert dt1$break_date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1break_date<-as.Date(dt1$break_date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1break_date) == length(tmp1break_date[!is.na(tmp1break_date)])){dt1$break_date <- tmp1break_date } else {print("Date conversion failed for dt1$break_date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1break_date) 
if (class(dt1$clear_jday)=="factor") dt1$clear_jday <-as.numeric(levels(dt1$clear_jday))[as.integer(dt1$clear_jday) ]               
if (class(dt1$clear_jday)=="character") dt1$clear_jday <-as.numeric(dt1$clear_jday)                                   
# attempting to convert dt1$clear_date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1clear_date<-as.Date(dt1$clear_date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1clear_date) == length(tmp1clear_date[!is.na(tmp1clear_date)])){dt1$clear_date <- tmp1clear_date } else {print("Date conversion failed for dt1$clear_date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1clear_date) 
if (class(dt1$form_jday)=="factor") dt1$form_jday <-as.numeric(levels(dt1$form_jday))[as.integer(dt1$form_jday) ]               
if (class(dt1$form_jday)=="character") dt1$form_jday <-as.numeric(dt1$form_jday)                                   
# attempting to convert dt1$form_date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1form_date<-as.Date(dt1$form_date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1form_date) == length(tmp1form_date[!is.na(tmp1form_date)])){dt1$form_date <- tmp1form_date } else {print("Date conversion failed for dt1$form_date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1form_date) 
                
# Convert Missing Values to NA for non-dates
                
dt1$lake <- as.factor(ifelse((trimws(as.character(dt1$lake))==trimws("NaN")),NA,as.character(dt1$lake)))
dt1$break_jday <- ifelse((trimws(as.character(dt1$break_jday))==trimws("NaN")),NA,dt1$break_jday)               
suppressWarnings(dt1$break_jday <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$break_jday))==as.character(as.numeric("NaN"))),NA,dt1$break_jday))
dt1$clear_jday <- ifelse((trimws(as.character(dt1$clear_jday))==trimws("NaN")),NA,dt1$clear_jday)               
suppressWarnings(dt1$clear_jday <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$clear_jday))==as.character(as.numeric("NaN"))),NA,dt1$clear_jday))
dt1$form_jday <- ifelse((trimws(as.character(dt1$form_jday))==trimws("NaN")),NA,dt1$form_jday)               
suppressWarnings(dt1$form_jday <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$form_jday))==as.character(as.numeric("NaN"))),NA,dt1$form_jday))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year)
summary(lake)
summary(break_jday)
summary(break_date)
summary(clear_jday)
summary(clear_date)
summary(form_jday)
summary(form_date) 
                # Get more details on character variables
                 
summary(as.factor(dt1$lake))
detach(dt1)               
        




