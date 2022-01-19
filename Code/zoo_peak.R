#rm(list=ls())

#find peak day for every year
   #for density 
   #for Diversity metrics
library(ggplot2)
library(reshape)
library(lubridate)
library(tidyverse)
library(vegan)
#load in Data

source("Data/knb-lter-nwt.161.3.r")
#work the data so it's easier to use
site_species <- cast(dt2, date+local_site+location ~ taxon, value='density')
#site_species$date <- as.Date(site_species$date)
site_species <- as.data.frame(site_species)
site_species[is.na(site_species)] <- 0

#couldn't think of a good variable name so named theHolyHandGrenade (tHHG)
#So this prints out a list of the days with highest total density along with the value found for those days
#TO DO SPLIT THE LAKES GL4 GL1 ALB


peakData <- data.frame()#lake, year, date, var, val)
yearRange <- c(2009:2019)
for(zone in c("GL4","GL1","ALB")){
   for (i in yearRange){
      tHHG <- site_species[(i == year(site_species$date)) & (zone == site_species$local_site) ,]
      if(max(tHHG$total) != max()){
         print(paste(zone,lubridate::yday(tHHG$date[which.max(tHHG$total)]), max(tHHG$total)))
         peakData <- rbind(peakData,c(zone, i, lubridate::yday(tHHG$date[which.max(tHHG$total)]), "zoo_Density", max(tHHG$total) ))
      }else{
         print(paste(zone,"has no density data for",i))
         peakData <- rbind(peakData,c(zone, i,NA , "zoo_Density", NA ))
      }
   }
}

names(peakData) <- c("lake", "year", "date", "variable", "value")

#peakData$date <- lubridate::ymd( peakData$date)
#rm(peakData)

#-----
#Now let's do it for other stuff
#you'll want to run Mathew's code, I've add the parts we will need
#We're going to use the diversity metrics that 

site_species<-site_species%>% unite("Site.loc.date", c(local_site,date,location))%>%
   remove_rownames()%>%
   column_to_rownames('Site.loc.date')
#1B) Calculate Diversity Metrics
#look up what these diversity metrics mean!
diversity<-site_species%>%
   dplyr::select(-c(Unknown, total))%>% # remove unknown and total densities
   transmute(N0=rowSums(site_species > 0), #species Richness
             H= diversity(site_species), ## Shannon entropy
             N1 =exp(H),## Shannon diversity (number of abundant species)
             N2 =diversity(site_species, "inv"),## Simpson diversity (number of dominant species)
             J= H/log(N0),## Pielou evenness
             E10= (N1/N0),## Shannon evenness (Hill's ratio)
             E20= (N2/N0),## Simpson evenness (Hill's ratio)
             Com.Size=rowSums(site_species), #total density
             betas.LCBD=beta.div(site_species, method="hellinger",sqrt.D=TRUE)$LCBD) #Local contribution to beta diversity
rownames(diversity)<-rownames(site_species)
diversity<-diversity%>%
   rownames_to_column("Site.loc.date")%>%
   separate("Site.loc.date", sep="_" ,into=c("Site", "date", "location"))%>%
   mutate(day_of_year=lubridate::yday(date))
   #separate("date", sep="-" ,into=c("year", "month", "day"))


#A little reordering so we can mess with the data a bit better
diversity <- diversity[,c(1:3,13,4:12 )]

#Okay so we know we use the same year range 
#I don't think we can get away without using a nested for loop and 
# we might need to deal with the  inf values

# 
for(divColumn in c(5:length(colnames(diversity)))){
   for(zone in c("GL4","GL1","ALB")){
      for (i in yearRange){
         tHHG <- diversity[(i == year(diversity$date)) & (zone == diversity$Site) , c(1:4,divColumn)]
         if(max(tHHG[,5]) != max()){
            print(paste(zone, lubridate::yday(tHHG$date[which.max(tHHG[,5])]), max(tHHG[,5])))
            peakData <- rbind(peakData,c(zone, i,lubridate::yday(tHHG$date[which.max(tHHG[,5])]), colnames(tHHG)[5], max(tHHG[,5]) ))
         }else{
            print(paste(zone,"has no density data for",i))
            peakData <- rbind(peakData,c(zone, i,NA , colnames(tHHG)[5], NA ))
         }
      }
   }
}

peakData <- peakData[!is.na(peakData$value),] 
#okay Awesome Now we have a data frame with the day in which every variable is at it's peak for every year
# that has observed Lets mess with that in the plot world

plot( peakData$year,peakData$date)

peakData$date <- factor(peakData$date, levels = c(1:365))

ggplot(peakData[peakData$variable == "zoo_Density",], aes(x = year, y = date, color = lake))+
   geom_point()+
   geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")
   theme_bw()
   #geom_line(peakData[peakData$variable == "zoo_Density",], aes(x = year, y = date, color = lake))
   #stat_smooth(aes(x = year, y = date, color = lake), method = "lm",formula = y~x, se= F)

peakData$year<-as.numeric(peakData$year)
peakData$date<-as.numeric(peakData$date)

peakData%>%
   ggplot(aes(x = year, y = date, color = lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "loess", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scale="free")

peakData%>%
   filter(variable=="zoo_Density")%>%
   ggplot(aes(x = year, y = date, color = lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "loess", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_grid(~lake)

#####################################################################################################################################

#combine ice out with peakData (relate to Ice out)
#Read in ice data

# Package ID: knb-lter-nwt.106.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake ice clearance and formation data for Green Lakes Valley from 1968 - ongoing..
# Data set creator:  T. Nelson (Nel) Caine -  
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#Change the names of lakes in ice out data to match our other datasets
ice_out<-Ice_out%>%
   mutate_if(is.factor ,str_replace_all, pattern = 'Green1', replacement = 'GL1')%>%
   mutate_if(is.character ,str_replace_all, pattern = 'Green2', replacement = 'GL2')%>%
   mutate_if(is.character ,str_replace_all, pattern = 'Green4', replacement = 'GL4')%>%
   mutate_if(is.character ,str_replace_all, pattern = 'Green5', replacement = 'GL5')%>%
   mutate_if(is.character ,str_replace_all, pattern = 'Albion', replacement = 'ALB')%>%
   filter(form_jday <350)
   
str(ice_out)

ice_out%>%
   ggplot(aes(x=year,y=break_jday,colour=lake))+
   geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Break Up")+
   facet_grid(~lake)

ice_out%>%
   ggplot(aes(x=year,y=clear_jday,colour=lake))+
   geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Clear")+
   facet_grid(~lake)

ice_out%>%
   ggplot(aes(x=year,y=form_jday,colour=lake))+
   geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Formation")+
   facet_grid(~lake)

ice_out%>%
   ggplot(aes(x=year,y=form_jday,colour=lake))+
   geom_point()+ geom_smooth(method="lm",se=T)+ylab("Day of Year of Lake Ice Formation")+theme_bw()
##########################################################################################
#ICe out data through time
ice_out%>%
   filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
   ggplot(aes(x=year,y=break_jday,colour=lake))+
   geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Break Up")+
   facet_grid(~lake)

ice_out%>%
   filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
   ggplot(aes(x=year,y=clear_jday,colour=lake))+
   geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Clear")+
   facet_grid(~lake)

ice_out%>%
   filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
   ggplot(aes(x=year,y=form_jday,colour=lake))+
   geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Formation")+
   facet_grid(~lake)

ice_out%>%
   filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
   ggplot(aes(x=year,y=clear_jday))+
   geom_point()+ geom_smooth(method="lm")

ice_out%>%
   filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
   ggplot(aes(x=year,y=break_jday))+
   geom_point()+ geom_smooth(method="lm")

ice_out%>%
   filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
   ggplot(aes(x=year,y=form_jday))+
   geom_point()+ geom_smooth(method="lm")

##########################################################################################
#Combine Ice out with diversity and peak data
all_data<-left_join(peakData,ice_out, by=c("lake","year"))
str(all_data)
all_data$value<-as.numeric(all_data$value)

#Lets plot how diversity/peak is related to ice out date


#day of
all_data%>%
   ggplot(aes(x = clear_jday, y = date))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scales="free")


all_data%>%
   ggplot(aes(x = break_jday, y = date))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scales="free")

all_data%>%
   ggplot(aes(x = form_jday, y = date))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scales="free")


#Values
all_data%>%
   ggplot(aes(x = clear_jday, y = value))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scales="free")

all_data%>%
   ggplot(aes(x = break_jday, y = value))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scales="free")

all_data%>%
   ggplot(aes(x = form_jday, y = value))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scales="free")

#Lakes and day
all_data%>%
   ggplot(aes(x = clear_jday, y = date, colour=lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_grid(lake~variable, scales="free")

all_data%>%
   ggplot(aes(x = break_jday, y = date, colour=lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_grid(lake~variable, scales="free")

all_data%>%
   ggplot(aes(x = form_jday, y = date, colour=lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_grid(lake~variable, scales="free")

#LAkes and value
all_data%>%
   ggplot(aes(x = clear_jday, y = value, colour=lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_grid(lake~variable, scales="free")

all_data%>%
   ggplot(aes(x = break_jday, y = value, colour=lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_grid(lake~variable, scales="free")

all_data%>%
   ggplot(aes(x = form_jday, y = value, colour=lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_grid(lake~variable, scales="free")


####################
ice_out%>%
   ggplot(aes(x=year,y=clear_jday,colour=lake))+
   geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Clear")+
   facet_grid(~lake)


all_data%>%
   filter(variable=="N0"| variable=="Com.Size")%>%
   ggplot(aes(x = clear_jday, y = date))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scales="free")
N0_data<-all_data%>%filter(variable=="N0")
dog<-lm(N0_data$date~N0_data$clear_jday)
summary(dog)



all_data%>%
   filter(variable=="N0"|variable=="betas.LCBD"| variable=="Com.Size")%>%
   ggplot(aes(x = clear_jday, y = value))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(~variable, scales="free")

betas_data<-all_data%>%filter(variable=="betas.LCBD")
dog<-lm(betas_data$value~betas_data$clear_jday)
summary(dog)

Com.Size_data<-all_data%>%filter(variable=="Com.Size")
dog<-lm(Com.Size_data$date~Com.Size_data$clear_jday)
summary(dog)


all_data%>%
   filter(variable=="N0"|variable=="betas.LCBD"| variable=="Com.Size")%>%
   ggplot(aes(x = clear_jday, y = date, colour=lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_grid(lake~variable, scales="free")

all_data%>%
   filter(variable=="N0"|variable=="betas.LCBD"| variable=="Com.Size")%>%
   ggplot(aes(x = clear_jday, y = value, colour=lake))+
   geom_point()+
   #geom_line()+
   geom_smooth(method = "lm", se = F) +
   ggtitle("Peak Zoo density doy for Lakes ALB, GL1, and GL4")+
   theme_bw()+
   facet_wrap(lake~variable, scales="free")
