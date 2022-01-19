# Package ID: knb-lter-nwt.64.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Microscope-based phytoplankton community composition for Green Lake 4, 2000 - 2007..
# Data set creator:  Diane Mcnight -  
# Data set creator:  Eric Sokal -  
# Data set creator:  Katherina Hell -  
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/64/1/ba551a6ef62de966a791e9a3b23e3f75" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "date",     
                    "year",     
                    "depth",     
                    "phylum",     
                    "taxon",     
                    "cell_count"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$phylum)!="factor") dt1$phylum<- as.factor(dt1$phylum)
if (class(dt1$taxon)!="factor") dt1$taxon<- as.factor(dt1$taxon)
if (class(dt1$cell_count)!="factor") dt1$cell_count<- as.factor(dt1$cell_count)
                
# Convert Missing Values to NA for non-dates
                
dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NaN")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NaN"))),NA,dt1$depth))
dt1$phylum <- as.factor(ifelse((trimws(as.character(dt1$phylum))==trimws("NaN")),NA,as.character(dt1$phylum)))
dt1$taxon <- as.factor(ifelse((trimws(as.character(dt1$taxon))==trimws("NaN")),NA,as.character(dt1$taxon)))
dt1$cell_count <- as.factor(ifelse((trimws(as.character(dt1$cell_count))==trimws("NaN")),NA,as.character(dt1$cell_count)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(date)
summary(year)
summary(depth)
summary(phylum)
summary(taxon)
summary(cell_count) 
                # Get more details on character variables
                 
summary(as.factor(dt1$phylum)) 
summary(as.factor(dt1$taxon)) 
summary(as.factor(dt1$cell_count))
detach(dt1)               
        




