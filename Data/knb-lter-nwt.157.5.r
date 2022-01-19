# Package ID: knb-lter-nwt.157.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Water quality data for Green Lakes Valley, 2000 - ongoing..
# Data set creator:  Diane Mcnight -  
# Data set creator:  Kelly Loria -  
# Data set creator:    - Niwot Ridge LTER 
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/157/5/977f9b22dfca3da36f162c5d9f2ebc45" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "site",     
                    "local_site",     
                    "year",     
                    "location",     
                    "depth",     
                    "date",     
                    "time",     
                    "chl_a",     
                    "pH",     
                    "temp",     
                    "std_conduct",     
                    "conduct",     
                    "DO",     
                    "sat",     
                    "nitrate",     
                    "secchi",     
                    "PAR",     
                    "flag",     
                    "comments"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)
if (class(dt1$location)!="factor") dt1$location<- as.factor(dt1$location)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$chl_a)=="factor") dt1$chl_a <-as.numeric(levels(dt1$chl_a))[as.integer(dt1$chl_a) ]               
if (class(dt1$chl_a)=="character") dt1$chl_a <-as.numeric(dt1$chl_a)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$temp)=="factor") dt1$temp <-as.numeric(levels(dt1$temp))[as.integer(dt1$temp) ]               
if (class(dt1$temp)=="character") dt1$temp <-as.numeric(dt1$temp)
if (class(dt1$std_conduct)=="factor") dt1$std_conduct <-as.numeric(levels(dt1$std_conduct))[as.integer(dt1$std_conduct) ]               
if (class(dt1$std_conduct)=="character") dt1$std_conduct <-as.numeric(dt1$std_conduct)
if (class(dt1$conduct)=="factor") dt1$conduct <-as.numeric(levels(dt1$conduct))[as.integer(dt1$conduct) ]               
if (class(dt1$conduct)=="character") dt1$conduct <-as.numeric(dt1$conduct)
if (class(dt1$DO)=="factor") dt1$DO <-as.numeric(levels(dt1$DO))[as.integer(dt1$DO) ]               
if (class(dt1$DO)=="character") dt1$DO <-as.numeric(dt1$DO)
if (class(dt1$sat)=="factor") dt1$sat <-as.numeric(levels(dt1$sat))[as.integer(dt1$sat) ]               
if (class(dt1$sat)=="character") dt1$sat <-as.numeric(dt1$sat)
if (class(dt1$nitrate)=="factor") dt1$nitrate <-as.numeric(levels(dt1$nitrate))[as.integer(dt1$nitrate) ]               
if (class(dt1$nitrate)=="character") dt1$nitrate <-as.numeric(dt1$nitrate)
if (class(dt1$secchi)=="factor") dt1$secchi <-as.numeric(levels(dt1$secchi))[as.integer(dt1$secchi) ]               
if (class(dt1$secchi)=="character") dt1$secchi <-as.numeric(dt1$secchi)
if (class(dt1$PAR)=="factor") dt1$PAR <-as.numeric(levels(dt1$PAR))[as.integer(dt1$PAR) ]               
if (class(dt1$PAR)=="character") dt1$PAR <-as.numeric(dt1$PAR)
if (class(dt1$flag)!="factor") dt1$flag<- as.factor(dt1$flag)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)
                
# Convert Missing Values to NA for non-dates
                
dt1$site <- as.factor(ifelse((trimws(as.character(dt1$site))==trimws("NaN")),NA,as.character(dt1$site)))
dt1$location <- as.factor(ifelse((trimws(as.character(dt1$location))==trimws("NaN")),NA,as.character(dt1$location)))
dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NaN")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NaN"))),NA,dt1$depth))
dt1$chl_a <- ifelse((trimws(as.character(dt1$chl_a))==trimws("NaN")),NA,dt1$chl_a)               
suppressWarnings(dt1$chl_a <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$chl_a))==as.character(as.numeric("NaN"))),NA,dt1$chl_a))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NaN")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NaN"))),NA,dt1$pH))
dt1$temp <- ifelse((trimws(as.character(dt1$temp))==trimws("NaN")),NA,dt1$temp)               
suppressWarnings(dt1$temp <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$temp))==as.character(as.numeric("NaN"))),NA,dt1$temp))
dt1$std_conduct <- ifelse((trimws(as.character(dt1$std_conduct))==trimws("NaN")),NA,dt1$std_conduct)               
suppressWarnings(dt1$std_conduct <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$std_conduct))==as.character(as.numeric("NaN"))),NA,dt1$std_conduct))
dt1$conduct <- ifelse((trimws(as.character(dt1$conduct))==trimws("NaN")),NA,dt1$conduct)               
suppressWarnings(dt1$conduct <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$conduct))==as.character(as.numeric("NaN"))),NA,dt1$conduct))
dt1$DO <- ifelse((trimws(as.character(dt1$DO))==trimws("NaN")),NA,dt1$DO)               
suppressWarnings(dt1$DO <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$DO))==as.character(as.numeric("NaN"))),NA,dt1$DO))
dt1$sat <- ifelse((trimws(as.character(dt1$sat))==trimws("NaN")),NA,dt1$sat)               
suppressWarnings(dt1$sat <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$sat))==as.character(as.numeric("NaN"))),NA,dt1$sat))
dt1$nitrate <- ifelse((trimws(as.character(dt1$nitrate))==trimws("NaN")),NA,dt1$nitrate)               
suppressWarnings(dt1$nitrate <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$nitrate))==as.character(as.numeric("NaN"))),NA,dt1$nitrate))
dt1$secchi <- ifelse((trimws(as.character(dt1$secchi))==trimws("NaN")),NA,dt1$secchi)               
suppressWarnings(dt1$secchi <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$secchi))==as.character(as.numeric("NaN"))),NA,dt1$secchi))
dt1$PAR <- ifelse((trimws(as.character(dt1$PAR))==trimws("NaN")),NA,dt1$PAR)               
suppressWarnings(dt1$PAR <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$PAR))==as.character(as.numeric("NaN"))),NA,dt1$PAR))
dt1$flag <- as.factor(ifelse((trimws(as.character(dt1$flag))==trimws("NaN")),NA,as.character(dt1$flag)))
dt1$comments <- as.factor(ifelse((trimws(as.character(dt1$comments))==trimws("NaN")),NA,as.character(dt1$comments)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(site)
summary(local_site)
summary(year)
summary(location)
summary(depth)
summary(date)
summary(time)
summary(chl_a)
summary(pH)
summary(temp)
summary(std_conduct)
summary(conduct)
summary(DO)
summary(sat)
summary(nitrate)
summary(secchi)
summary(PAR)
summary(flag)
summary(comments) 
                # Get more details on character variables
                 
summary(as.factor(dt1$site)) 
summary(as.factor(dt1$local_site)) 
summary(as.factor(dt1$location)) 
summary(as.factor(dt1$flag)) 
summary(as.factor(dt1$comments))
detach(dt1)               
        




