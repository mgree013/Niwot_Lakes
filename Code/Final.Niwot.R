#NIWOT Zooplankton Analysis
#Author: Matthew D Green
#Date: June 28, 2021

################################################################################################################
#Load packages
library(ggplot2)
library(reshape)
library(lubridate)
library(tidyverse)
library(vegan)
library(ggbiplot)
library(adespatial)
library(dplyr)
library(FD)
library(sf)
library(rgl)
library(viridis)

#library(devtools)
#install.packages("rgl")
#install_github("sdray/adespatial")
#install_github("r-spatial/sf")
#install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")

################################################################################################################
#load in Data



###############################################
#Load Ice data
source("Data/knb-lter-nwt.106.2.r")

ice_out<-dt1

ice_out<-ice_out%>%
  mutate_if(is.factor ,str_replace_all, pattern = 'Green1', replacement = 'GL1')%>%
  mutate_if(is.character ,str_replace_all, pattern = 'Green2', replacement = 'GL2')%>%
  mutate_if(is.character ,str_replace_all, pattern = 'Green3', replacement = 'GL3')%>%
  mutate_if(is.character ,str_replace_all, pattern = 'Green4', replacement = 'GL4')%>%
  mutate_if(is.character ,str_replace_all, pattern = 'Green5', replacement = 'GL5')%>%
  mutate_if(is.character ,str_replace_all, pattern = 'Albion', replacement = 'ALB')#%>%
  #filter(complete_ice_formation <350)

str(ice_out)

################################################################################################################
#Species Data
source("Data/knb-lter-nwt.161.3.r")

##Make Site x species matrix (wide format)
site_species <- cast(dt2, date+local_site+location ~ taxon, value='density')
site_species <- as.data.frame(site_species)
site_species[is.na(site_species)] <- 0
str(site_species)

site_species<-site_species%>%
  filter(location=="lake")%>%
  unite("Site.loc.date", c(local_site,date,location))%>%
  remove_rownames()%>%
  column_to_rownames('Site.loc.date')

#plot species time series
org_species<-site_species%>%
  dplyr::select(-c(Unknown, total))%>% # remove unknonwn and total denstiies
  mutate(H_gibberum=H.gibberum+`H.gibberum neonate`)%>%
  mutate(D_pulicaria=D.pulicaria+`D.pulicaria neonate`)%>%
  mutate(D_rosea=D.rosea+`D.rosea neonate`)%>%
  dplyr::select(-c(D.rosea,`D.rosea neonate`,D.pulicaria,`D.pulicaria neonate`,H.gibberum,`H.gibberum neonate`))%>%
  dplyr::select(-c(Chronomid, nauplii, Mite, Nematoda, Odonata))

pivot_org_species<-org_species%>%
  rownames_to_column("Site.loc.date")%>%
  separate("Site.loc.date", sep="_" ,into=c("Site", "date", "location"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))%>%
  pivot_longer(Ascomorpha:D_rosea, names_to="taxon", values_to="density")%>%
  dplyr::rename(lake=Site)

pivot_org_species%>%
  filter(lake=="GL4")%>%
  filter(density >0)%>%
  filter(taxon=="H.shoshone"| taxon=="D_pulicaria")%>%
  ggplot(aes(x=day_of_year, y=(density), colour=taxon))+
  geom_point()+
  geom_smooth(methods="loess")+
  facet_grid(year~taxon, scales="free")

pivot_org_species%>%
  filter(lake=="ALB")%>%
  filter(density >0)%>%
  #filter(taxon=="H.shoshone"| taxon=="D_pulicaria")%>%
  ggplot(aes(x=day_of_year, y=(density), colour=taxon))+
  geom_point()+
  geom_smooth(methods="loess")+
  facet_grid(year~taxon, scales="free")

pivot_org_species$year<-as.integer(pivot_org_species$year)
org_data_ice_sp<-left_join(pivot_org_species,ice_out, by=c("lake","year"))
str(org_species)
str(ice_out)

org_data_ice_sp%>%
  filter(lake=="GL4")%>%
  filter(density >0)%>%
  ggplot(aes(x=day_of_year, y=(density), colour=taxon))+
  geom_point()+
  geom_smooth(methods="loess")+
  facet_grid(year~taxon, scales="free")

################################################################################################################
#1B) Calculate Diversity Metrics
#look up what these diversity metrics mean!

#pelagic:
#non:chronmid, nauplii, mite, nematoda, odonata

diversity<-site_species%>%
  dplyr::select(-c(Unknown, total))%>% # remove unknonwn and total denstiies
  mutate(H_gibberum=H.gibberum+`H.gibberum neonate`)%>%
  mutate(D_pulicaria=D.pulicaria+`D.pulicaria neonate`)%>%
  mutate(D_rosea=D.rosea+`D.rosea neonate`)%>%
  dplyr::select(-c(D.rosea,`D.rosea neonate`,D.pulicaria,`D.pulicaria neonate`,H.gibberum,`H.gibberum neonate`))%>%
  dplyr::select(-c(Chronomid, nauplii, Odonata))%>%
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

#Organize our diversity data set
diversity<-diversity%>%
  rownames_to_column("Site.loc.date")%>%
  separate("Site.loc.date", sep="_" ,into=c("Site", "date", "location"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))

diversity%>%
  filter(Site=="GL1"|Site=="GL4"|Site=="ALB")%>%
  ggplot(aes(x=day_of_year,y=log(Com.Size),colour=year))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  facet_grid(~Site)

#Summary Data 
diversity_data<-diversity%>%
  filter(Site=="GL1"|Site=="GL4"|Site=="ALB")%>%
  group_by(Site,year)%>%
  dplyr::summarize(day.max.N0=day_of_year[which.max(N0)],day.max.N1=day_of_year[which.max(N1)],day.max.Density=day_of_year[which.max(Com.Size)],day.max.Beta=day_of_year[which.max(betas.LCBD)],
                   day.min.N0=day_of_year[which.min(N0)],day.min.N1=day_of_year[which.min(N1)],day.min.Density=day_of_year[which.min(Com.Size)],day.min.Beta=day_of_year[which.min(betas.LCBD)],
                   day.max.min.N0=day.max.N0-day.min.N0,day.max.min.Density=day.max.Density-day.min.Density,day.max.min.Beta=day.max.Beta-day.min.Beta,
                   day.min.max.N0=day.min.N0-day.max.N0,day.min.max.Density=day.min.Density-day.max.Density,day.min.max.Beta=day.min.Beta-day.max.Beta,
                   max.N0=max(N0),max.N1=max(N1),max.Density=max(Com.Size),max.Beta=max(betas.LCBD),
                   min.N0=min(N0),min.N1=min(N1),min.Density=min(Com.Size),min.Beta=min(betas.LCBD),
                   max.min.N0=max.N0-min.N0,max.min.Density=max.Density-min.Density,max.min.Beta=max.Beta-min.Beta,
                   min.max.N0=min.N0-max.N0,min.max.Density=min.Density-max.Density,min.max.Beta=min.Beta-max.Beta,
                   cv.N0=raster::cv(N0),cv.Density=raster::cv(Com.Size),cv.Beta=raster::cv(betas.LCBD))



###################################################################################################################################################################################
#Combine Ice out with diversity and peak data
diversity_data<-diversity_data%>%dplyr::rename(lake=Site)
diversity_data$year<-as.integer(diversity_data$year)
str(diversity_data)
str(ice_out)
all_data<-left_join(diversity_data,ice_out, by=c("lake","year"))
str(all_data)

allz_data<-all_data%>%
  mutate(growing_season=form_jday-clear_jday)
################################################################################################################################################################################
#Explore How max, Min and Max-Min influenced by ice phenology
allz_data%>%
  gather(day.max.Beta,day.max.Density,day.max.N0,day.max.N1, key = "var", value = "value") %>% 
  ggplot(aes(x = growing_season, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  ylab("Day of Year")+xlab("Length of Growing Season")+
  facet_grid(~var, scales="free")

dog<-lm(day.max.N0~growing_season,allz_data)
summary(dog)

allz_data%>%
  gather(max.Beta,max.Density,max.N0,max.N1, key = "var", value = "value") %>% 
  ggplot(aes(x = growing_season, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

#plugin form_jday and brreak_jday for clear_jday for further exploration

all_data%>%
  gather(day.max.Beta,day.max.Density,day.max.N0,day.max.N1, key = "var", value = "value") %>% 
  ggplot(aes(x = clear_jday, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

dog<-lm(all_data$day.max.N0~all_data$clear_jday)
summary(dog)

all_data%>%
  gather(day.min.Beta,day.min.Density,day.min.N0,day.min.N1, key = "var", value = "value") %>% 
  ggplot(aes(x = form_jday, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

all_data%>%
  gather(day.min.max.Density,day.min.max.N0, key = "var", value = "value") %>% 
  ggplot(aes(x = clear_jday, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")


#Explore how ice phenolgoy infleunce values of No,beta,and density

all_data%>%
  gather(max.Density,max.N0, key = "var", value = "value") %>% 
  ggplot(aes(x = break_jday, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

dog<-lm(all_data$max.N0~all_data$clear_jday)
summary(dog)

all_data%>%
  gather(break_jday,form_jday,clear_jday, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = day.max.N1,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_grid(~var, scales="free")

all_data%>%
  gather(min.max.Beta,min.max.Density,min.max.N0, key = "var", value = "value") %>% 
  ggplot(aes(x = break_jday, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

all_data%>%
  gather(max.min.Density,max.min.N0, key = "var", value = "value") %>% 
  ggplot(aes(x = break_jday, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

dog<-lm(all_data$max.min.Density~all_data$clear_jday)
summary(dog)

all_data%>%
  gather(cv.Density,cv.N0, key = "var", value = "value") %>% 
  ggplot(aes(x = break_jday, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

dog<-lm(all_data$cv.Density~all_data$clear_jday)
summary(dog)

####################
#time series anlaysis of diversity
diversity<-diversity%>%dplyr::rename(lake=Site)
diversity$year<-as.integer(diversity$year)
time_diversity<-left_join(diversity,ice_out, by=c("lake","year"))

time_diversity_GL1<-time_diversity%>%
  filter(lake=="GL1")%>%
  mutate(scale_form_day=form_jday-294 , scale_clear_day=clear_jday-153)

time_diversity_GL4<-time_diversity%>%
  filter(lake=="GL4")%>%
  mutate(scale_form_day=form_jday-288 , scale_clear_day=clear_jday-168)

time_diversity_ALB<-time_diversity%>%
  filter(lake=="ALB")%>%
  mutate(scale_form_day=form_jday-300 , scale_clear_day=clear_jday-169)

all_time_data_div<-rbind(time_diversity_ALB,time_diversity_GL4,time_diversity_GL1)

all_time_data_div%>%
  gather(N0,N1,betas.LCBD,Com.Size, key = "var", value = "value") %>% 
  ggplot(aes(x = scale_form_day, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

all_time_data_div%>%
  gather(N0,N1,betas.LCBD,Com.Size, key = "var", value = "value") %>% 
  ggplot(aes(x = scale_clear_day, y = value,colour=var))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d()+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ 
  facet_wrap(~var, scales="free")

################################################################################################################################################################################
#Species NMDS (Spatial)

ordintation<-site_species%>%
  dplyr::select(-c(Unknown, total))%>% # remove unknown and total densities
  mutate(H_gibberum=H.gibberum+`H.gibberum neonate`)%>%
  mutate(D_pulicaria=D.pulicaria+`D.pulicaria neonate`)%>%
  mutate(D_rosea=D.rosea+`D.rosea neonate`)%>%
  dplyr::select(-c(D.rosea,`D.rosea neonate`,D.pulicaria,`D.pulicaria neonate`,H.gibberum,`H.gibberum neonate`))%>%
  dplyr::select(-c(Chronomid, nauplii, Odonata))%>%
  rownames_to_column("Site.loc.date")%>%
  mutate(Sum=rowSums(site_species))%>%
  filter(Sum>0)%>%
  dplyr::select(-c(Sum))

ordintation.env<-ordintation%>%
  separate("Site.loc.date", sep="_" ,into=c("Site", "date", "location"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))


set.seed(101)
dune.rel<-decostand(ordintation[,2:18],"total") #standardize community data
dune.bray<-vegdist(dune.rel) #calcualte dissimilarity among sites (i.e. dissimiliarity matrix)
dune.nmds=metaMDS(dune.rel, k=2, try=100) #NMDS codedune.nmds
stressplot(dune.nmds) #this tells us if our plot is going to work, and it looks good

#group by sites
plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 18) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordihull(dune.nmds, groups=ordintation.env$Site, draw="polygon", color=c(1:5),label=T)
ordisurf(dune.nmds, ordintation.env$day_of_year, prioirty=,labcex=0.9, add = T,col="forestgreen")
eniros<-cbind(ordintation.env$year,ordintation.env$day_of_year)
mds.data.envfit = envfit(dune.nmds, eniros)

plot(mds.data.envfit, col = "black", labels = c("Year", "DOY"), lwd = 2)
#PERMANOVA
adonis2(dune.bray ~ ordintation.env$Site+ ordintation.env$day_of_year+ordintation.env$year, permutations = 999)


mod2 <- betadisper(dune.bray,ordintation.env$Site)
mod2
permutest(mod2, permutations = 99)
anova(mod2)
plot(mod2)
boxplot(mod2)
plot(TukeyHSD(mod2))
TukeyHSD(mod2)
################################################################################################################
#FILTER SITES BY INTERST
site_speciess<-site_species%>% rownames_to_column("Site.loc.date")%>%
  separate("Site.loc.date", sep="_" ,into=c("Site", "date", "location"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  #separate("date", sep="-" ,into=c("year", "month", "day"))%>%
  filter(Site=="GL1"|Site=="GL4"|Site=="ALB")%>%
  unite("Site.loc.date", c(Site,date,location))%>%
  remove_rownames()%>%
  column_to_rownames('Site.loc.date')

ordintation<-site_speciess%>%
  dplyr::select(-c(Unknown, total,day_of_year))%>%
  rownames_to_column("Site.loc.date")%>%
  mutate(Sum=rowSums(site_speciess))%>%
  filter(Sum>0)%>%
  dplyr::select(-c(Sum))

ordintation.env<-(ordintation.env [which(rowSums(ordintation.env[,6:22]) > 0), ])
str(ordintation.env)
rowSums(ordintation.env[,6:22])

ordintation.env<-ordintation%>%
  #rownames_to_column(var="Site.loc.date")%>%
  separate("Site.loc.date", sep="_" ,into=c("Site", "date", "location"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))
rowSums(ordintation[,2:22])
set.seed(101)

dune.rel<-decostand(ordintation.env[,6:22],"total") #standardize community data
dune.bray<-vegdist(dune.rel, method = "bray") #calcualte dissimilarity among sites (i.e. dissimiliarity matrix)
dune.nmds=metaMDS(dune.rel, k=2, try=100) #NMDS code
dune.nmds
stressplot(dune.nmds) #this tells us if our plot is going to work, and it looks good

#group by sites
plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 18) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordihull(dune.nmds, groups=ordintation.env$Site, draw="polygon", color=c(1:2),label=T)
ordisurf(dune.nmds, ordintation.env$day_of_year, prioirty=,labcex=0.9, add = T,col="forestgreen")

eniros<-cbind(ordintation.env$year,ordintation.env$day_of_year)
mds.data.envfit = envfit(dune.nmds, eniros)

plot(mds.data.envfit, col = "black", labels = c("Year", "DOY"), lwd = 2)
#PERMANOVA
adonis2(dune.bray ~ ordintation.env$Site, permutations = 999)

mod2 <- betadisper(dune.bray,ordintation.env$Site)
mod2
permutest(mod2, permutations = 99)
anova(mod2)
plot(mod2)
boxplot(mod2)
plot(TukeyHSD(mod2))
TukeyHSD(mod2)
################################################################################################################
#####Env Ordination####

source("Data/knb-lter-nwt.10.2.r")
dt1$lake_network_number<-if_else(dt1$local_site=="GL5",1,if_else(dt1$local_site=="GL4",2,if_else(dt1$local_site=="GL3",3,if_else(dt1$local_site=="GL2",4,if_else(dt1$local_site=="ALB",5,0)))))
dt1$elevation<-if_else(dt1$local_site=="GL5",3621,if_else(dt1$local_site=="GL4",3563,if_else(dt1$local_site=="GL3",3455,if_else(dt1$local_site=="GL2",3408,if_else(dt1$local_site=="ALB",3357,3431)))))

env1<-dt1%>%
  dplyr::select(-c(LTER_site,time))
#%>%mutate(day_of_year=lubridate::yday(date))%>%
  #separate("date", sep="-" ,into=c("year", "month", "day"))

#water temp/chl data
source("Data/knb-lter-nwt.157.5.r")

env2<-dt1%>%dplyr::select(c(local_site,date,chl_a, pH, temp,location,depth, year))# %>%
  #mutate(day_of_year=lubridate::yday(date))%>%
  #separate("date", sep="-" ,into=c("year", "month", "day"))

#############################################
env_var<-dplyr::right_join(env1,env2, by=c("location","date" ,"year", "depth", "local_site"))

env_varz<-env_var%>%
  filter(location=="LAKE")%>%
  dplyr::select(c(local_site,date,year,depth,chl_a,temp,elevation,pH.y,conduct ,ANC,H.plus.,Ca.plus..plus., Mg.plus..plus., Na.plus., K.plus., Cl.hyphen.,NO3.hyphen., SO4.hyphen..hyphen.,     Si, cat_sum,  an_sum,   TDN,   DON))%>%
  drop_na()


dog.stream <-prcomp(env_varz[,4:23], center = TRUE,scale. = TRUE) #PCA code
dog.stream #this shows us hpw our data lines up PC1
summary(dog.stream)
z<-ggbiplot(dog.stream, labels=rownames(interaction(env_varz$local_site)), groups=interaction(env_varz$local_site), ellipse=TRUE)
z<-z+xlim(-4,3)+ylim(-3,4.5)
z
dog.stream
#PERMANOVA
adonis2(env_varz[,4:23]~local_site+as.factor(year)+as.factor(depth),method="euclidean", data=env_varz)

#FIlter by site of interst
env_varzz<-env_varz%>%
  filter(local_site=="GL1"|local_site=="GL4"|local_site=="ALB")

dog.stream <-prcomp(env_varzz[,4:23], center = TRUE,scale. = TRUE) #PCA code
dog.stream #this shows us hpw our data lines up PC1
summary(dog.stream)
p<-ggbiplot(dog.stream, labels=rownames(interaction(env_varzz$local_site)), groups=interaction(env_varzz$local_site), ellipse=TRUE)
p

dog.stream
#PERMANOVA
adonis2(env_varzz[,4:23]~local_site+as.factor(year)+as.factor(depth),method="euclidean", data=env_varzz)


################################################################################################################################################################################
#Plot Ice_out dat
ice_out%>%
  ggplot(aes(x=year,y=break_jday,colour=lake))+
  geom_point()+ geom_smooth(method="lm")+
  scale_color_viridis_d()+
  ylab("Day of Year of Lake Ice Break Up")+
  facet_grid(~lake)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

ice_out%>%
  ggplot(aes(x=year,y=clear_jday,colour=lake))+
  geom_point()+ geom_smooth(method="lm")+  scale_color_viridis_d()+
  ylab("Day of Year of Lake Ice Clear")+
  facet_grid(~lake)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

ice_out%>%
  ggplot(aes(x=year,y=break_jday,colour=lake))+
  scale_color_viridis_d()+
  geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Clear")+
  facet_grid(~lake)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

ice_out%>%
  ggplot(aes(x=year,y=form_jday,colour=lake))+
  scale_color_viridis_d()+
  geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Formation")+
  facet_grid(~lake)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

ice_out%>%
  ggplot(aes(x=year,y=form_jday,colour=lake))+
  scale_color_viridis_d()+
  geom_point()+ geom_smooth(method="lm",se=F)+ylab("Day of Year of Lake Ice Formation")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                              panel.border = element_blank(),panel.background = element_blank())
##########################################################################################
#ICe out data through time
ice_out%>%
  filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
  ggplot(aes(x=year,y=break_jday,colour=lake))+
  scale_color_viridis_d()+
  geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Break Up")+
  facet_grid(~lake)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

ice_out%>%
  filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
  ggplot(aes(x=year,y=clear_jday,colour=lake))+
  scale_color_viridis_d()+
  geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Clear")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

alb<-ice_out%>%filter(lake=="GL1")
dog<-lm(alb$form_jday~alb$year)
summary(dog)

ice_out%>%
  filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
  ggplot(aes(x=year,y=form_jday,colour=lake))+
  geom_point()+ geom_smooth(method="lm")+ylab("Day of Year of Lake Ice Formation")+
  facet_grid(~lake)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

ice_out%>%
  filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
  ggplot(aes(x=year,y=clear_jday))+
  geom_point()+ geom_smooth(method="lm")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

ice_out%>%
  filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
  ggplot(aes(x=year,y=break_jday))+
  geom_point()+ geom_smooth(method="lm")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

ice_out%>%
  filter(lake=="GL1" | lake=="GL4"|lake=="ALB")%>%
  ggplot(aes(x=year,y=form_jday))+
  geom_point()+ geom_smooth(method="lm")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())



################################################################################################################################################################################################################
########Combine environmental,water chem and biolgoy data
#only analyzing biology and environemntal data where we have all that data available

env_vars<-env_var%>% dplyr::rename(Site = local_site) %>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))

env_vars$Site<-as.character(env_vars$Site)

#Environmental Water_chem, Zoo
source("Data/knb-lter-nwt.157.5.r")

#problem is here with joining data sets, not really joining
all<-left_join(env_vars,ordintation.env, by=c("Site", "year", "month","day", "day_of_year"))

water_temp<-dt1%>%dplyr::select(c(local_site,date,chl_a, pH, temp))%>% dplyr::rename(Site = local_site)%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))

water_temp$Site<-as.character(water_temp$Site)

all_datas<-left_join(all,water_temp, by=c("Site", "year", "month","day", "day_of_year"))#%>%drop_na()

#Relate environmetnal and biological data
#calulate diversity
species<-all_datas%>%dplyr::select(c(Keratella, Mite, Nematoda, Notholca, Ostracoda, D.thomasi, H.shoshone, Kellicottia,Ascomorpha, Asplanchna, Bosmina, Chaoboridae, Chydoridae, Colletheca,D.pulicaria, D.rosea))
env<-all_datas%>%dplyr::select(-c(Keratella, Mite, Nematoda, Notholca, Ostracoda, D.thomasi, H.shoshone, Kellicottia,Ascomorpha, Asplanchna, Bosmina, Chaoboridae, Chydoridae, Colletheca,D.pulicaria, D.rosea))

diversity<-species%>%
  #dplyr::select(-c(Unknown, total))%>% # remove unknonwn and total denstiies
  transmute(N0=rowSums(species > 0), #species Richness
            H= diversity(species), ## Shannon entropy
            N1 =exp(H),## Shannon diversity (number of abundant species)
            N2 =diversity(species, "inv"),## Simpson diversity (number of dominant species)
            J= H/log(N0),## Pielou evenness
            E10= (N1/N0),## Shannon evenness (Hill's ratio)
            E20= (N2/N0),## Simpson evenness (Hill's ratio)
            Com.Size=rowSums(species)) #total density
            #betas.LCBD=beta.div(species, method="hellinger",sqrt.D=TRUE)$LCBD) #Local contribution to beta diversity
#PCA
env_var<-env%>%
  dplyr::select(c(Site,month,day,year,depth,elevation,pH.x,pH.y,temp.y,chl_a.y,conduct ,ANC,H.plus.,Ca.plus..plus., Mg.plus..plus., Na.plus., K.plus., Cl.hyphen.,NO3.hyphen., SO4.hyphen..hyphen.,Si, cat_sum, an_sum, TDN,DON))%>%
  drop_na()

dog.stream <-prcomp(env_var[,7:25], center = TRUE,scale. = TRUE) #PCA code
dog.stream #this shows us hpw our data lines up PC1
summary(dog.stream)
ggbiplot(dog.stream, labels=rownames(interaction(env_var$Site)), groups=interaction(env_var$Site), ellipse=TRUE)

dog.stream
#PERMANOVA
adonis2(env_var[,7:25]~lake_network_number+Site+as.factor(year),method="euclidean", data=env_var)

pc_scores <- data.frame(dog.stream$x[,1:4])
head(pc_scores)

combined<-cbind(diversity,env,pc_scores)


#Environmental tredns Aacrooss all three lakes
combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=Com.Size))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(Site~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=betas.LCBD))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=N0))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=N1))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var,scales="free")

#Among lake difference
combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=Com.Size,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=betas.LCBD,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=N0,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var,scales="free")

#Effect of year
combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=Com.Size,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(year~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,pH.x,conduct,ANC,chl_a,pH.y,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=Com.Size,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(year~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=betas.LCBD,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,pH.x,conduct,ANC,chl_a,pH.y,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=betas.LCBD,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(year~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,lake_network_number,elevation,pH.x,conduct,ANC, H.plus.,Ca.plus..plus., Mg.plus..plus.,chl_a,pH.y,Na.plus.,K.plus.,Cl.hyphen.,NO3.hyphen.,SO4.hyphen..hyphen.,Si,cat_sum,an_sum,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=value,y=N0,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~var,scales="free")

combined%>%
  gather(N0,N1,N2, key = "var", value = "value") %>% 
  ggplot(aes(x=PC1,y=value,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(year~var,scales="free")

combined%>%
  gather(E10,E20, key = "var", value = "value") %>% 
  ggplot(aes(x=PC1,y=value,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(year~var,scales="free")

combined%>%
  gather(betas.LCBD, key = "var", value = "value") %>% 
  ggplot(aes(x=PC1,y=value,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~var,scales="free")

combined%>%
  gather(PC1,PC2,PC3,PC4,pH.x,conduct,ANC,chl_a,pH.y,TDN,DON,day_of_year,temp, key = "var", value = "value") %>% 
  ggplot(aes(x=PC1,y=N1,colour=Site))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(year~var,scales="free")

################################################################################################################################################################
#Traits
source("Data/knb-lter-nwt.161.3.r")

traits<-dt1%>%
  dplyr::select(-c(Notes, LTER_site))

#explore Trait Data

traits%>%
  ggplot(aes(x=local_site,y=size,fill=local_site))+
  geom_boxplot()

traits%>%
  ggplot(aes(x=taxon,y=size,fill=local_site))+
  geom_boxplot()+
  facet_wrap(~local_site)
#body size larger in GL4 and GL5, most likely due to no fish

traits%>%
  ggplot(aes(x=local_site,y=size,fill=local_site))+
  geom_boxplot()+
  facet_wrap(~taxon)

traitsy<-traits%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))


traitsy%>%
  filter(local_site =="GL4")%>%
  ggplot(aes(x=day_of_year,y=size,colour=taxon))+
  geom_point()+
  geom_smooth(method="loess")+
  facet_wrap(year~taxon)

traitsy%>%
  filter(local_site =="GL4")%>%
  filter(taxon =="DAPUL" | taxon=="HESP")%>%
  filter(day_of_year >200)%>%
  ggplot(aes(x=day_of_year,y=log(size+1),colour=taxon))+
  geom_point()+
  geom_smooth(method="loess")+
  facet_wrap(year~taxon)

#Link traits to sites with environmental data
#Fix size and magnification data!
traitss<-traits%>% unite("Site.loc.date", c(local_site,date,location))%>%
  dplyr::select(-c(neonate_count, ephippia, melanin, melanization_score, magnification, scope_size))
  
av.triats<-traitss%>%group_by(taxon)%>%
  dplyr::summarize(body_size=mean(size))%>%
  pivot_wider(names_from = "taxon",values_from="body_size")

av.triatss<-av.triats%>%
  dplyr::rename(Bosmina= BOS,Chronomid=CHRON, Chydoridae=CHYD, D.pulicaria=DAPUL, D.thomasi=DITH,D.rosea=DROS,Ephippia=EPHIP, Ergasilus=ERGA, H.shoshone=HESP, H.gibberum=HOLO,Mite=MITE,nauplii= NAUP,Nematoda=NEMA,Neonate=NEO,Ostracoda=OSTRA)%>%
  dplyr::select(-c(Ephippia,Ergasilus,Neonate))%>%
  pivot_longer(Bosmina:Ostracoda,names_to = "taxon", values_to="size")%>%
  column_to_rownames("taxon")

site_sp_fn_traits<-site_speciess%>%
  dplyr::select(c(Bosmina,Chronomid, Chydoridae, D.pulicaria, D.thomasi,D.rosea, H.shoshone, H.gibberum,Mite,nauplii,Nematoda,Ostracoda))%>%rownames_to_column("site")%>%
 filter(site !="GL4_2009-08-04_inlet", site !="GL4_2009-08-04_lake", site != "GL1_2009-08-04_lake", site !="GL1_2009-08-11_lake")%>%
column_to_rownames("site")

rowSums(site_sp_fn_traits)
if(dim(site_sp_fn_traits)[2]!=dim(av.triatss)[1])stop("error:differentnumberofspeciesin'traits'and'abundances'matrices")

tres = dbFD(av.triatss,site_sp_fn_traits, corr = ("lingoes"),
            stand.FRic = TRUE, calc.FDiv = TRUE)

CWM<-as.data.frame(tres$CWM)
Fric<-as.data.frame(tres$FRic)
FDis<-as.data.frame(tres$FDis)
FEve=as.data.frame(tres$FEve)

datasz<-cbind(CWM,Fric,FDis,FEve)
datasz<-datasz%>%rownames_to_column("Site.loc.date")%>%separate("Site.loc.date", into=c("Site",  "date","loc"), sep="_")%>%
 separate("date", into=c("year", "month", "day"), sep="-")%>%
 dplyr::rename(Site=Site,loc=loc,year=year,month=month,day=day,CWM_size=size,Fric=`tres$FRic`,FDis=`tres$FDis` ,FEve=  `tres$FEve`)

datasz%>%
  gather(CWM_size,FDis,FEve, key = "var", value = "value") %>% 
  ggplot(aes(x=Site,y=value,fill=Site))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+  
  facet_grid(~var,scales="free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

#variation in body size

#Add in Roland Body Size Data thats missing from NIWOT
