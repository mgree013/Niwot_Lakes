# Package ID: knb-lter-nwt.12.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake water quality, chemistry and zooplankton composition for 16 lakes surrounding the Green Lakes Valley, 2016.
# Data set creator:  Pieter Johnson -  
# Data set creator:  Kelly Loria -  
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/12/1/2619d9d5c07fa2822883df2ea17ffd52" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "local_site",     
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
                    "secchi",     
                    "PAR"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
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
if (class(dt1$secchi)=="factor") dt1$secchi <-as.numeric(levels(dt1$secchi))[as.integer(dt1$secchi) ]               
if (class(dt1$secchi)=="character") dt1$secchi <-as.numeric(dt1$secchi)
if (class(dt1$PAR)=="factor") dt1$PAR <-as.numeric(levels(dt1$PAR))[as.integer(dt1$PAR) ]               
if (class(dt1$PAR)=="character") dt1$PAR <-as.numeric(dt1$PAR)
                
# Convert Missing Values to NA for non-dates
                
dt1$local_site <- as.factor(ifelse((trimws(as.character(dt1$local_site))==trimws("NA")),NA,as.character(dt1$local_site)))
dt1$location <- as.factor(ifelse((trimws(as.character(dt1$location))==trimws("NA")),NA,as.character(dt1$location)))
dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NA")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NA"))),NA,dt1$depth))
dt1$chl_a <- ifelse((trimws(as.character(dt1$chl_a))==trimws("NA")),NA,dt1$chl_a)               
suppressWarnings(dt1$chl_a <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$chl_a))==as.character(as.numeric("NA"))),NA,dt1$chl_a))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NA")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NA"))),NA,dt1$pH))
dt1$temp <- ifelse((trimws(as.character(dt1$temp))==trimws("NA")),NA,dt1$temp)               
suppressWarnings(dt1$temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$temp))==as.character(as.numeric("NA"))),NA,dt1$temp))
dt1$std_conduct <- ifelse((trimws(as.character(dt1$std_conduct))==trimws("NA")),NA,dt1$std_conduct)               
suppressWarnings(dt1$std_conduct <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$std_conduct))==as.character(as.numeric("NA"))),NA,dt1$std_conduct))
dt1$conduct <- ifelse((trimws(as.character(dt1$conduct))==trimws("NA")),NA,dt1$conduct)               
suppressWarnings(dt1$conduct <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$conduct))==as.character(as.numeric("NA"))),NA,dt1$conduct))
dt1$DO <- ifelse((trimws(as.character(dt1$DO))==trimws("NA")),NA,dt1$DO)               
suppressWarnings(dt1$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO))==as.character(as.numeric("NA"))),NA,dt1$DO))
dt1$sat <- ifelse((trimws(as.character(dt1$sat))==trimws("NA")),NA,dt1$sat)               
suppressWarnings(dt1$sat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$sat))==as.character(as.numeric("NA"))),NA,dt1$sat))
dt1$secchi <- ifelse((trimws(as.character(dt1$secchi))==trimws("NA")),NA,dt1$secchi)               
suppressWarnings(dt1$secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$secchi))==as.character(as.numeric("NA"))),NA,dt1$secchi))
dt1$PAR <- ifelse((trimws(as.character(dt1$PAR))==trimws("NA")),NA,dt1$PAR)               
suppressWarnings(dt1$PAR <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$PAR))==as.character(as.numeric("NA"))),NA,dt1$PAR))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(local_site)
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
summary(secchi)
summary(PAR) 
                # Get more details on character variables
                 
summary(as.factor(dt1$local_site)) 
summary(as.factor(dt1$location))
detach(dt1)               
         

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/12/1/9ba7bca3cc35f53282d31c5bdd47dfa6" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "local_site",     
                    "location",     
                    "depth",     
                    "date",     
                    "time",     
                    "Cl.hyphen.",     
                    "SO4.hyphen.",     
                    "TDN",     
                    "NO3.hyphen.",     
                    "TDP",     
                    "DOP",     
                    "IP",     
                    "PO4.hyphen.",     
                    "DOC",     
                    "Fl",     
                    "peak",     
                    "UV_abs",     
                    "SUVA"    ), check.names=TRUE)
               
unlink(infile2)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt2$local_site)!="factor") dt2$local_site<- as.factor(dt2$local_site)
if (class(dt2$location)!="factor") dt2$location<- as.factor(dt2$location)
if (class(dt2$depth)=="factor") dt2$depth <-as.numeric(levels(dt2$depth))[as.integer(dt2$depth) ]               
if (class(dt2$depth)=="character") dt2$depth <-as.numeric(dt2$depth)                                   
# attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2date) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2date) 
if (class(dt2$Cl.hyphen.)=="factor") dt2$Cl.hyphen. <-as.numeric(levels(dt2$Cl.hyphen.))[as.integer(dt2$Cl.hyphen.) ]               
if (class(dt2$Cl.hyphen.)=="character") dt2$Cl.hyphen. <-as.numeric(dt2$Cl.hyphen.)
if (class(dt2$SO4.hyphen.)=="factor") dt2$SO4.hyphen. <-as.numeric(levels(dt2$SO4.hyphen.))[as.integer(dt2$SO4.hyphen.) ]               
if (class(dt2$SO4.hyphen.)=="character") dt2$SO4.hyphen. <-as.numeric(dt2$SO4.hyphen.)
if (class(dt2$TDN)=="factor") dt2$TDN <-as.numeric(levels(dt2$TDN))[as.integer(dt2$TDN) ]               
if (class(dt2$TDN)=="character") dt2$TDN <-as.numeric(dt2$TDN)
if (class(dt2$NO3.hyphen.)=="factor") dt2$NO3.hyphen. <-as.numeric(levels(dt2$NO3.hyphen.))[as.integer(dt2$NO3.hyphen.) ]               
if (class(dt2$NO3.hyphen.)=="character") dt2$NO3.hyphen. <-as.numeric(dt2$NO3.hyphen.)
if (class(dt2$TDP)=="factor") dt2$TDP <-as.numeric(levels(dt2$TDP))[as.integer(dt2$TDP) ]               
if (class(dt2$TDP)=="character") dt2$TDP <-as.numeric(dt2$TDP)
if (class(dt2$DOP)=="factor") dt2$DOP <-as.numeric(levels(dt2$DOP))[as.integer(dt2$DOP) ]               
if (class(dt2$DOP)=="character") dt2$DOP <-as.numeric(dt2$DOP)
if (class(dt2$IP)=="factor") dt2$IP <-as.numeric(levels(dt2$IP))[as.integer(dt2$IP) ]               
if (class(dt2$IP)=="character") dt2$IP <-as.numeric(dt2$IP)
if (class(dt2$PO4.hyphen.)=="factor") dt2$PO4.hyphen. <-as.numeric(levels(dt2$PO4.hyphen.))[as.integer(dt2$PO4.hyphen.) ]               
if (class(dt2$PO4.hyphen.)=="character") dt2$PO4.hyphen. <-as.numeric(dt2$PO4.hyphen.)
if (class(dt2$DOC)=="factor") dt2$DOC <-as.numeric(levels(dt2$DOC))[as.integer(dt2$DOC) ]               
if (class(dt2$DOC)=="character") dt2$DOC <-as.numeric(dt2$DOC)
if (class(dt2$Fl)=="factor") dt2$Fl <-as.numeric(levels(dt2$Fl))[as.integer(dt2$Fl) ]               
if (class(dt2$Fl)=="character") dt2$Fl <-as.numeric(dt2$Fl)
if (class(dt2$peak)=="factor") dt2$peak <-as.numeric(levels(dt2$peak))[as.integer(dt2$peak) ]               
if (class(dt2$peak)=="character") dt2$peak <-as.numeric(dt2$peak)
if (class(dt2$UV_abs)=="factor") dt2$UV_abs <-as.numeric(levels(dt2$UV_abs))[as.integer(dt2$UV_abs) ]               
if (class(dt2$UV_abs)=="character") dt2$UV_abs <-as.numeric(dt2$UV_abs)
if (class(dt2$SUVA)=="factor") dt2$SUVA <-as.numeric(levels(dt2$SUVA))[as.integer(dt2$SUVA) ]               
if (class(dt2$SUVA)=="character") dt2$SUVA <-as.numeric(dt2$SUVA)
                
# Convert Missing Values to NA for non-dates
                
dt2$local_site <- as.factor(ifelse((trimws(as.character(dt2$local_site))==trimws("NaN")),NA,as.character(dt2$local_site)))
dt2$location <- as.factor(ifelse((trimws(as.character(dt2$location))==trimws("NaN")),NA,as.character(dt2$location)))
dt2$depth <- ifelse((trimws(as.character(dt2$depth))==trimws("NaN")),NA,dt2$depth)               
suppressWarnings(dt2$depth <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$depth))==as.character(as.numeric("NaN"))),NA,dt2$depth))
dt2$Cl.hyphen. <- ifelse((trimws(as.character(dt2$Cl.hyphen.))==trimws("NaN")),NA,dt2$Cl.hyphen.)               
suppressWarnings(dt2$Cl.hyphen. <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$Cl.hyphen.))==as.character(as.numeric("NaN"))),NA,dt2$Cl.hyphen.))
dt2$SO4.hyphen. <- ifelse((trimws(as.character(dt2$SO4.hyphen.))==trimws("NaN")),NA,dt2$SO4.hyphen.)               
suppressWarnings(dt2$SO4.hyphen. <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$SO4.hyphen.))==as.character(as.numeric("NaN"))),NA,dt2$SO4.hyphen.))
dt2$TDN <- ifelse((trimws(as.character(dt2$TDN))==trimws("NaN")),NA,dt2$TDN)               
suppressWarnings(dt2$TDN <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$TDN))==as.character(as.numeric("NaN"))),NA,dt2$TDN))
dt2$NO3.hyphen. <- ifelse((trimws(as.character(dt2$NO3.hyphen.))==trimws("NaN")),NA,dt2$NO3.hyphen.)               
suppressWarnings(dt2$NO3.hyphen. <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$NO3.hyphen.))==as.character(as.numeric("NaN"))),NA,dt2$NO3.hyphen.))
dt2$NO3.hyphen. <- ifelse((trimws(as.character(dt2$NO3.hyphen.))==trimws("u")),NA,dt2$NO3.hyphen.)               
suppressWarnings(dt2$NO3.hyphen. <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt2$NO3.hyphen.))==as.character(as.numeric("u"))),NA,dt2$NO3.hyphen.))
dt2$TDP <- ifelse((trimws(as.character(dt2$TDP))==trimws("NaN")),NA,dt2$TDP)               
suppressWarnings(dt2$TDP <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$TDP))==as.character(as.numeric("NaN"))),NA,dt2$TDP))
dt2$TDP <- ifelse((trimws(as.character(dt2$TDP))==trimws("u")),NA,dt2$TDP)               
suppressWarnings(dt2$TDP <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt2$TDP))==as.character(as.numeric("u"))),NA,dt2$TDP))
dt2$DOP <- ifelse((trimws(as.character(dt2$DOP))==trimws("NaN")),NA,dt2$DOP)               
suppressWarnings(dt2$DOP <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$DOP))==as.character(as.numeric("NaN"))),NA,dt2$DOP))
dt2$DOP <- ifelse((trimws(as.character(dt2$DOP))==trimws("u")),NA,dt2$DOP)               
suppressWarnings(dt2$DOP <- ifelse(!is.na(as.numeric("u")) & (trimws(as.character(dt2$DOP))==as.character(as.numeric("u"))),NA,dt2$DOP))
dt2$DOP <- ifelse((trimws(as.character(dt2$DOP))==trimws("EQCL")),NA,dt2$DOP)               
suppressWarnings(dt2$DOP <- ifelse(!is.na(as.numeric("EQCL")) & (trimws(as.character(dt2$DOP))==as.character(as.numeric("EQCL"))),NA,dt2$DOP))
dt2$IP <- ifelse((trimws(as.character(dt2$IP))==trimws("NaN")),NA,dt2$IP)               
suppressWarnings(dt2$IP <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$IP))==as.character(as.numeric("NaN"))),NA,dt2$IP))
dt2$PO4.hyphen. <- ifelse((trimws(as.character(dt2$PO4.hyphen.))==trimws("NaN")),NA,dt2$PO4.hyphen.)               
suppressWarnings(dt2$PO4.hyphen. <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$PO4.hyphen.))==as.character(as.numeric("NaN"))),NA,dt2$PO4.hyphen.))
dt2$DOC <- ifelse((trimws(as.character(dt2$DOC))==trimws("NaN")),NA,dt2$DOC)               
suppressWarnings(dt2$DOC <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$DOC))==as.character(as.numeric("NaN"))),NA,dt2$DOC))
dt2$Fl <- ifelse((trimws(as.character(dt2$Fl))==trimws("NaN")),NA,dt2$Fl)               
suppressWarnings(dt2$Fl <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$Fl))==as.character(as.numeric("NaN"))),NA,dt2$Fl))
dt2$peak <- ifelse((trimws(as.character(dt2$peak))==trimws("NaN")),NA,dt2$peak)               
suppressWarnings(dt2$peak <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$peak))==as.character(as.numeric("NaN"))),NA,dt2$peak))
dt2$UV_abs <- ifelse((trimws(as.character(dt2$UV_abs))==trimws("NaN")),NA,dt2$UV_abs)               
suppressWarnings(dt2$UV_abs <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$UV_abs))==as.character(as.numeric("NaN"))),NA,dt2$UV_abs))
dt2$SUVA <- ifelse((trimws(as.character(dt2$SUVA))==trimws("NaN")),NA,dt2$SUVA)               
suppressWarnings(dt2$SUVA <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt2$SUVA))==as.character(as.numeric("NaN"))),NA,dt2$SUVA))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(local_site)
summary(location)
summary(depth)
summary(date)
summary(time)
summary(Cl.hyphen.)
summary(SO4.hyphen.)
summary(TDN)
summary(NO3.hyphen.)
summary(TDP)
summary(DOP)
summary(IP)
summary(PO4.hyphen.)
summary(DOC)
summary(Fl)
summary(peak)
summary(UV_abs)
summary(SUVA) 
                # Get more details on character variables
                 
summary(as.factor(dt2$local_site)) 
summary(as.factor(dt2$location))
detach(dt2)               
         

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/12/1/a90e64bccb9d6d406c3ec49ae5953ff0" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")

                   
 dt3 <-read.csv(infile3,header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "local_site",     
                    "date",     
                    "year",     
                    "depth",     
                    "net_um",     
                    "liters_examined",     
                    "taxon_name",     
                    "number_of_taxon",     
                    "taxon_density",     
                    "mean_taxon_length",     
                    "number_of_individuals_measured",     
                    "total_taxon_counted",     
                    "total_taxon_density"    ), check.names=TRUE)
               
unlink(infile3)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt3$local_site)!="factor") dt3$local_site<- as.factor(dt3$local_site)                                   
# attempting to convert dt3$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3date<-as.Date(dt3$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3date) == length(tmp3date[!is.na(tmp3date)])){dt3$date <- tmp3date } else {print("Date conversion failed for dt3$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3date) 
if (class(dt3$depth)=="factor") dt3$depth <-as.numeric(levels(dt3$depth))[as.integer(dt3$depth) ]               
if (class(dt3$depth)=="character") dt3$depth <-as.numeric(dt3$depth)
if (class(dt3$net_um)=="factor") dt3$net_um <-as.numeric(levels(dt3$net_um))[as.integer(dt3$net_um) ]               
if (class(dt3$net_um)=="character") dt3$net_um <-as.numeric(dt3$net_um)
if (class(dt3$liters_examined)=="factor") dt3$liters_examined <-as.numeric(levels(dt3$liters_examined))[as.integer(dt3$liters_examined) ]               
if (class(dt3$liters_examined)=="character") dt3$liters_examined <-as.numeric(dt3$liters_examined)
if (class(dt3$taxon_name)!="factor") dt3$taxon_name<- as.factor(dt3$taxon_name)
if (class(dt3$number_of_taxon)=="factor") dt3$number_of_taxon <-as.numeric(levels(dt3$number_of_taxon))[as.integer(dt3$number_of_taxon) ]               
if (class(dt3$number_of_taxon)=="character") dt3$number_of_taxon <-as.numeric(dt3$number_of_taxon)
if (class(dt3$taxon_density)=="factor") dt3$taxon_density <-as.numeric(levels(dt3$taxon_density))[as.integer(dt3$taxon_density) ]               
if (class(dt3$taxon_density)=="character") dt3$taxon_density <-as.numeric(dt3$taxon_density)
if (class(dt3$mean_taxon_length)=="factor") dt3$mean_taxon_length <-as.numeric(levels(dt3$mean_taxon_length))[as.integer(dt3$mean_taxon_length) ]               
if (class(dt3$mean_taxon_length)=="character") dt3$mean_taxon_length <-as.numeric(dt3$mean_taxon_length)
if (class(dt3$number_of_individuals_measured)=="factor") dt3$number_of_individuals_measured <-as.numeric(levels(dt3$number_of_individuals_measured))[as.integer(dt3$number_of_individuals_measured) ]               
if (class(dt3$number_of_individuals_measured)=="character") dt3$number_of_individuals_measured <-as.numeric(dt3$number_of_individuals_measured)
if (class(dt3$total_taxon_counted)=="factor") dt3$total_taxon_counted <-as.numeric(levels(dt3$total_taxon_counted))[as.integer(dt3$total_taxon_counted) ]               
if (class(dt3$total_taxon_counted)=="character") dt3$total_taxon_counted <-as.numeric(dt3$total_taxon_counted)
if (class(dt3$total_taxon_density)=="factor") dt3$total_taxon_density <-as.numeric(levels(dt3$total_taxon_density))[as.integer(dt3$total_taxon_density) ]               
if (class(dt3$total_taxon_density)=="character") dt3$total_taxon_density <-as.numeric(dt3$total_taxon_density)
                
# Convert Missing Values to NA for non-dates
                
dt3$local_site <- as.factor(ifelse((trimws(as.character(dt3$local_site))==trimws("NA")),NA,as.character(dt3$local_site)))
dt3$depth <- ifelse((trimws(as.character(dt3$depth))==trimws("NA")),NA,dt3$depth)               
suppressWarnings(dt3$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$depth))==as.character(as.numeric("NA"))),NA,dt3$depth))
dt3$net_um <- ifelse((trimws(as.character(dt3$net_um))==trimws("NA")),NA,dt3$net_um)               
suppressWarnings(dt3$net_um <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$net_um))==as.character(as.numeric("NA"))),NA,dt3$net_um))
dt3$liters_examined <- ifelse((trimws(as.character(dt3$liters_examined))==trimws("NA")),NA,dt3$liters_examined)               
suppressWarnings(dt3$liters_examined <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$liters_examined))==as.character(as.numeric("NA"))),NA,dt3$liters_examined))
dt3$taxon_name <- as.factor(ifelse((trimws(as.character(dt3$taxon_name))==trimws("NA")),NA,as.character(dt3$taxon_name)))
dt3$number_of_taxon <- ifelse((trimws(as.character(dt3$number_of_taxon))==trimws("NA")),NA,dt3$number_of_taxon)               
suppressWarnings(dt3$number_of_taxon <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$number_of_taxon))==as.character(as.numeric("NA"))),NA,dt3$number_of_taxon))
dt3$taxon_density <- ifelse((trimws(as.character(dt3$taxon_density))==trimws("NA")),NA,dt3$taxon_density)               
suppressWarnings(dt3$taxon_density <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$taxon_density))==as.character(as.numeric("NA"))),NA,dt3$taxon_density))
dt3$mean_taxon_length <- ifelse((trimws(as.character(dt3$mean_taxon_length))==trimws("NA")),NA,dt3$mean_taxon_length)               
suppressWarnings(dt3$mean_taxon_length <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$mean_taxon_length))==as.character(as.numeric("NA"))),NA,dt3$mean_taxon_length))
dt3$number_of_individuals_measured <- ifelse((trimws(as.character(dt3$number_of_individuals_measured))==trimws("NA")),NA,dt3$number_of_individuals_measured)               
suppressWarnings(dt3$number_of_individuals_measured <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$number_of_individuals_measured))==as.character(as.numeric("NA"))),NA,dt3$number_of_individuals_measured))
dt3$total_taxon_counted <- ifelse((trimws(as.character(dt3$total_taxon_counted))==trimws("NA")),NA,dt3$total_taxon_counted)               
suppressWarnings(dt3$total_taxon_counted <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$total_taxon_counted))==as.character(as.numeric("NA"))),NA,dt3$total_taxon_counted))
dt3$total_taxon_density <- ifelse((trimws(as.character(dt3$total_taxon_density))==trimws("NA")),NA,dt3$total_taxon_density)               
suppressWarnings(dt3$total_taxon_density <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$total_taxon_density))==as.character(as.numeric("NA"))),NA,dt3$total_taxon_density))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(local_site)
summary(date)
summary(year)
summary(depth)
summary(net_um)
summary(liters_examined)
summary(taxon_name)
summary(number_of_taxon)
summary(taxon_density)
summary(mean_taxon_length)
summary(number_of_individuals_measured)
summary(total_taxon_counted)
summary(total_taxon_density) 
                # Get more details on character variables
                 
summary(as.factor(dt3$local_site)) 
summary(as.factor(dt3$taxon_name))
detach(dt3)               
        




