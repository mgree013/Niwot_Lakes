# Package ID: knb-lter-nwt.161.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Zooplankton community composition and trait data for Green Lake 4, 2012 to ongoing.
# Data set creator:  Pieter T Johnson -  
# Data set creator:  Kelly A Loria -  
# Data set creator:    - Niwot Ridge LTER 
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/161/3/07eb39a7a6e51dffb4d6276d10500e44" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "LTER_site",     
                    "local_site",     
                    "year",     
                    "date",     
                    "location",     
                    "liters_examined",     
                    "taxon",     
                    "magnification",     
                    "scope_size",     
                    "size",     
                    "male",     
                    "gravid",     
                    "egg_count",     
                    "neonate_count",     
                    "ephippia",     
                    "melanin",     
                    "melanization_score",     
                    "Notes"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$LTER_site)!="factor") dt1$LTER_site<- as.factor(dt1$LTER_site)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$location)!="factor") dt1$location<- as.factor(dt1$location)
if (class(dt1$liters_examined)=="factor") dt1$liters_examined <-as.numeric(levels(dt1$liters_examined))[as.integer(dt1$liters_examined) ]               
if (class(dt1$liters_examined)=="character") dt1$liters_examined <-as.numeric(dt1$liters_examined)
if (class(dt1$taxon)!="factor") dt1$taxon<- as.factor(dt1$taxon)
if (class(dt1$magnification)!="factor") dt1$magnification<- as.factor(dt1$magnification)
if (class(dt1$scope_size)=="factor") dt1$scope_size <-as.numeric(levels(dt1$scope_size))[as.integer(dt1$scope_size) ]               
if (class(dt1$scope_size)=="character") dt1$scope_size <-as.numeric(dt1$scope_size)
if (class(dt1$size)=="factor") dt1$size <-as.numeric(levels(dt1$size))[as.integer(dt1$size) ]               
if (class(dt1$size)=="character") dt1$size <-as.numeric(dt1$size)
if (class(dt1$male)!="factor") dt1$male<- as.factor(dt1$male)
if (class(dt1$gravid)!="factor") dt1$gravid<- as.factor(dt1$gravid)
if (class(dt1$egg_count)=="factor") dt1$egg_count <-as.numeric(levels(dt1$egg_count))[as.integer(dt1$egg_count) ]               
if (class(dt1$egg_count)=="character") dt1$egg_count <-as.numeric(dt1$egg_count)
if (class(dt1$neonate_count)=="factor") dt1$neonate_count <-as.numeric(levels(dt1$neonate_count))[as.integer(dt1$neonate_count) ]               
if (class(dt1$neonate_count)=="character") dt1$neonate_count <-as.numeric(dt1$neonate_count)
if (class(dt1$ephippia)!="factor") dt1$ephippia<- as.factor(dt1$ephippia)
if (class(dt1$melanin)!="factor") dt1$melanin<- as.factor(dt1$melanin)
if (class(dt1$melanization_score)!="factor") dt1$melanization_score<- as.factor(dt1$melanization_score)
if (class(dt1$Notes)!="factor") dt1$Notes<- as.factor(dt1$Notes)
                
# Convert Missing Values to NA for non-dates
                
dt1$location <- as.factor(ifelse((trimws(as.character(dt1$location))==trimws("NaN")),NA,as.character(dt1$location)))
dt1$liters_examined <- ifelse((trimws(as.character(dt1$liters_examined))==trimws("NaN")),NA,dt1$liters_examined)               
suppressWarnings(dt1$liters_examined <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$liters_examined))==as.character(as.numeric("NaN"))),NA,dt1$liters_examined))
dt1$taxon <- as.factor(ifelse((trimws(as.character(dt1$taxon))==trimws("NaN")),NA,as.character(dt1$taxon)))
dt1$magnification <- as.factor(ifelse((trimws(as.character(dt1$magnification))==trimws("NaN")),NA,as.character(dt1$magnification)))
dt1$scope_size <- ifelse((trimws(as.character(dt1$scope_size))==trimws("NaN")),NA,dt1$scope_size)               
suppressWarnings(dt1$scope_size <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$scope_size))==as.character(as.numeric("NaN"))),NA,dt1$scope_size))
dt1$size <- ifelse((trimws(as.character(dt1$size))==trimws("NaN")),NA,dt1$size)               
suppressWarnings(dt1$size <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$size))==as.character(as.numeric("NaN"))),NA,dt1$size))
dt1$male <- as.factor(ifelse((trimws(as.character(dt1$male))==trimws("NaN")),NA,as.character(dt1$male)))
dt1$gravid <- as.factor(ifelse((trimws(as.character(dt1$gravid))==trimws("NaN")),NA,as.character(dt1$gravid)))
dt1$egg_count <- ifelse((trimws(as.character(dt1$egg_count))==trimws("NaN")),NA,dt1$egg_count)               
suppressWarnings(dt1$egg_count <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$egg_count))==as.character(as.numeric("NaN"))),NA,dt1$egg_count))
dt1$neonate_count <- ifelse((trimws(as.character(dt1$neonate_count))==trimws("NaN")),NA,dt1$neonate_count)               
suppressWarnings(dt1$neonate_count <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$neonate_count))==as.character(as.numeric("NaN"))),NA,dt1$neonate_count))
dt1$ephippia <- as.factor(ifelse((trimws(as.character(dt1$ephippia))==trimws("NaN")),NA,as.character(dt1$ephippia)))
dt1$melanin <- as.factor(ifelse((trimws(as.character(dt1$melanin))==trimws("NaN")),NA,as.character(dt1$melanin)))
dt1$melanization_score <- as.factor(ifelse((trimws(as.character(dt1$melanization_score))==trimws("NaN")),NA,as.character(dt1$melanization_score)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(local_site)
summary(year)
summary(date)
summary(location)
summary(liters_examined)
summary(taxon)
summary(magnification)
summary(scope_size)
summary(size)
summary(male)
summary(gravid)
summary(egg_count)
summary(neonate_count)
summary(ephippia)
summary(melanin)
summary(melanization_score)
summary(Notes) 
                # Get more details on character variables
                 
summary(as.factor(dt1$LTER_site)) 
summary(as.factor(dt1$local_site)) 
summary(as.factor(dt1$location)) 
summary(as.factor(dt1$taxon)) 
summary(as.factor(dt1$magnification)) 
summary(as.factor(dt1$male)) 
summary(as.factor(dt1$gravid)) 
summary(as.factor(dt1$ephippia)) 
summary(as.factor(dt1$melanin)) 
summary(as.factor(dt1$melanization_score)) 
summary(as.factor(dt1$Notes))
detach(dt1)               
         

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/161/3/3ebfe6838cc9717c95906036de9efc59" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "LTER_site",     
                    "local_site",     
                    "location",     
                    "date",     
                    "liters_examined",     
                    "pull_number",     
                    "taxon",     
                    "count",     
                    "density"    ), check.names=TRUE)
               
unlink(infile2)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt2$LTER_site)!="factor") dt2$LTER_site<- as.factor(dt2$LTER_site)
if (class(dt2$local_site)!="factor") dt2$local_site<- as.factor(dt2$local_site)
if (class(dt2$location)!="factor") dt2$location<- as.factor(dt2$location)                                   
# attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2date) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2date) 
if (class(dt2$liters_examined)=="factor") dt2$liters_examined <-as.numeric(levels(dt2$liters_examined))[as.integer(dt2$liters_examined) ]               
if (class(dt2$liters_examined)=="character") dt2$liters_examined <-as.numeric(dt2$liters_examined)
if (class(dt2$pull_number)!="factor") dt2$pull_number<- as.factor(dt2$pull_number)
if (class(dt2$taxon)!="factor") dt2$taxon<- as.factor(dt2$taxon)
if (class(dt2$count)!="factor") dt2$count<- as.factor(dt2$count)
if (class(dt2$density)=="factor") dt2$density <-as.numeric(levels(dt2$density))[as.integer(dt2$density) ]               
if (class(dt2$density)=="character") dt2$density <-as.numeric(dt2$density)
                
# Convert Missing Values to NA for non-dates
                
dt2$liters_examined <- ifelse((trimws(as.character(dt2$liters_examined))==trimws("NaN")),NA,dt2$liters_examined)               
suppressWarnings(dt2$liters_examined <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$liters_examined))==as.character(as.numeric("NaN"))),NA,dt2$liters_examined))
dt2$pull_number <- as.factor(ifelse((trimws(as.character(dt2$pull_number))==trimws("NaN")),NA,as.character(dt2$pull_number)))
dt2$taxon <- as.factor(ifelse((trimws(as.character(dt2$taxon))==trimws("NaN")),NA,as.character(dt2$taxon)))
dt2$count <- as.factor(ifelse((trimws(as.character(dt2$count))==trimws("NaN")),NA,as.character(dt2$count)))
dt2$density <- ifelse((trimws(as.character(dt2$density))==trimws("NaN")),NA,dt2$density)               
suppressWarnings(dt2$density <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$density))==as.character(as.numeric("NaN"))),NA,dt2$density))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(local_site)
summary(location)
summary(date)
summary(liters_examined)
summary(pull_number)
summary(taxon)
summary(count)
summary(density) 
                # Get more details on character variables
                 
summary(as.factor(dt2$LTER_site)) 
summary(as.factor(dt2$local_site)) 
summary(as.factor(dt2$location)) 
summary(as.factor(dt2$pull_number)) 
summary(as.factor(dt2$taxon)) 
summary(as.factor(dt2$count))
detach(dt2)               
        




