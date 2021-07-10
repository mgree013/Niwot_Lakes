#knb-lter-newt.161.3.r
#Data Explration NIWOT
#Monday August 24, 2020

#Install packages and load them
library(ggplot2)
library(tidyverse)
library(reshape)
library(vegan)
library(adespatial)
library(lubridate)

#run the other  attatched r script to load the data entitled "knb-lter-newt.161.3.r'

#Lets look at our data
str(dt1)
summary(dt1)
str(dt2)
summary(dt2)

#Data set dt1 is zooplanlton traits
#Data set set dt2 has info on species density
#We also need to locate datasets with environmental variables!

###############################################################################################################################################################################################################################################
#1) Zooplankton Diversity


#1A)Organize Data
#Make Data Site*Species  Data Frame (the data is in long format currently and we want it wider)

#Lets reshape the dataset to have our columns as species names and rows as unique location,sites, and date.
site_species <- cast(dt2, date+local_site+location ~ taxon, value='density')
site_species <- as.data.frame(site_species)
site_species[is.na(site_species)] <- 0
str(site_species)

#Make one column for site, location and date, well remove these later
site_species<-site_species%>% unite("Site.loc.date", c(local_site,date,location))%>%
  remove_rownames()%>%
  column_to_rownames('Site.loc.date')

#1B) Calculate Diversity Metrics
#look up what these diversity metrics mean!

diversity<-site_species%>%
  dplyr::select(-c(Unknown, total))%>% # remove unknonwn and total denstiies
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

#Orangize our diversity data set
diversity<-diversity%>%
  rownames_to_column("Site.loc.date")%>%
  separate("Site.loc.date", sep="_" ,into=c("Site", "date", "location"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))

########################################################################################################################
#1B) PLotting diversity data

#Spatial (among Sites)
diversity%>%
  gather(N0,N1,E10,Com.Size, betas.LCBD, key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(Site), y = value, fill=Site)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#So with temporal data sets, were intiersted initially in two temporal scales (among years and within Years

#Among years (X axis= Year)
diversity%>%
  gather(N0,N1,E10,Com.Size, betas.LCBD, key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(year), y = value, fill=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  facet_wrap(Site~ var, scales = "free") +
  theme_bw()

#lets remove the sites without much tmeporal sampling

diversity%>%
  filter(Site=="ALB" | Site=="GL4" |Site=="GL1")%>%
  gather(N0,N1,E10,Com.Size, betas.LCBD, key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(year), y = value, fill=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  facet_wrap(Site~ var, scales = "free") +
  theme_bw()


#Within year variaiton (x-axis= day of year)
diversity%>%
  gather(N0,N1,E10,Com.Size, betas.LCBD, key = "var", value = "value") %>% 
  ggplot(aes(x = day_of_year, y = value, colour=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "loess")+
  facet_wrap(Site~ var, scales = "free") +
  theme_bw()

diversity%>%
  filter(Site=="ALB" | Site=="GL4" |Site=="GL1")%>%
  gather(N0,N1,E10,Com.Size, betas.LCBD, key = "var", value = "value") %>% 
  ggplot(aes(x = day_of_year, y = value, colour=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "loess", se=F)+
  facet_wrap(Site~ var, scales = "free") +
  theme_bw()


#look at sites and diversity metrics indiviudally
diversity%>%
  filter(Site=="GL4")%>%
  ggplot(aes(x = day_of_year, y = Com.Size, colour=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  #geom_smooth(method = "loess", se=F)+
  #facet_wrap(~year, scales = "free") +
  xlim(0,360)+
  theme_bw()

diversity%>%
  filter(Site=="GL1")%>%
  ggplot(aes(x = day_of_year, y = N0, colour=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "loess",se=F)+
  theme_bw()

diversity%>%
  filter(Site=="ALB")%>%
  ggplot(aes(x = day_of_year, y = Com.Size, colour=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "loess")+
  theme_bw()

################################################################################################################################################################################################################################################
#Multivariate Statistics

#NMDS (Non-Metric Multideminsional Scaling)
#https://en.wikipedia.org/wiki/Multidimensional_scaling

#We have a lot of different species at a lot of sites, which makes this data a bit comlex
#one way to simlpify things is by using ordination techniques which reduces the deminsionality of our data to a few axes
#each species and sites/year/location gets a score and its a good way to visaulize community data

ordintation<-site_species%>%
  dplyr::select(-c(Unknown, total))%>%
  rownames_to_column("Site.loc.date")%>%
  mutate(Sum=rowSums(site_species))%>%
  filter(Sum>0)%>%
  dplyr::select(-c(Sum))


ordintation.env<-ordintation%>%
  separate("Site.loc.date", sep="_" ,into=c("Site", "date", "location"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))


set.seed(101)
dune.rel<-decostand(ordintation[,2:24],"total") #standardize community data
dune.bray<-vegdist(dune.rel) #calcualte dissimilarity among sites (i.e. dissimiliarity matrix)
dune.nmds=metaMDS(dune.rel, k=2, try=100) #NMDS code
dune.nmds
stressplot(dune.nmds) #this tells us if our plot is going to work, and it looks good

#group by sites
plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 18) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordihull(dune.nmds, groups=ordintation.env$Site, draw="polygon", color=c(1:5),label=T)
ordisurf(dune.nmds, ordintation.env$day_of_year, prioirty=,labcex=0.9, add = T,col="forestgreen")

eniros<-cbind(ordintation.env$year,ordintation.env$day_of_year)
mds.data.envfit = envfit(dune.nmds, eniros)
#Group by Year
plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 1) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordihull(dune.nmds, groups=ordintation.env$year, draw="polygon", label=T)
ordisurf(dune.nmds, ordintation.env$day_of_year, prioirty=,labcex=0.9, add = T,col="forestgreen")

plot(mds.data.envfit, col = "black", labels = c("Year", "DOY"), lwd = 2)
#PERMANOVA
adonis2(dune.bray ~ ordintation.env$day_of_year*ordintation.env$year+ordintation.env$Site, permutations = 999)


#Temporal ordination
temp.env.gl4<-ordintation.env%>%filter(Site != "GL1" & Site != "GL3"& Site != "GL5")
temp.sp.gl4<-temp.env.gl4[,c(6:28)]
set.seed(101)
dune.rel<-decostand(temp.sp.gl4,"total") #standardize community data
dune.bray<-vegdist(dune.rel) #calcualte dissimilarity among sites (i.e. dissimiliarity matrix)
dune.nmds=metaMDS(dune.rel, k=2, try=100) #NMDS code
dune.nmds
stressplot(dune.nmds) #this tells us if our plot is going to work, and it looks good
temp.env.gl4$year<-as.numeric(temp.env.gl4$year)
#group by sites
plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2", main="Zoo Community Structure by Site")
text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 18) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordihull(dune.nmds, groups=temp.env.gl4$Site, draw="polygon", col=c(1,2,3), label=T)
ordisurf(dune.nmds, temp.env.gl4$day_of_year, prioirty=,labcex=0.9, add = T,col="forestgreen")
#ordisurf(dune.nmds, temp.env.gl4$year, prioirty=,labcex=0.9, add = T,col="forestgreen")

eniros<-cbind(temp.env.gl4$year,temp.env.gl4$day_of_year)
mds.data.envfit = envfit(dune.nmds, eniros)
plot(mds.data.envfit, col = "black", labels = c("Year", "DOY"), lwd = 2)
adonis2(dune.bray ~ temp.env.gl4$year*temp.env.gl4$day_of_year, permutations = 999)

ordiarrows(dune.nmds, temp.env.gl4$year, label = TRUE)

mod <- betadisper(dune.bray, temp.env.gl4$day_of_year) 
a<-plot(mod, main="PCOA GL1")
b<-plot(mod, main="PCOA GL4")
c<-plot(mod, main="PCOA ALB")
par(mfrow=c(1,3))
a
b
c

#it looks like most variaiton explained by Sites, then year, then day of year.
########################################################################################################################################################################################
#Combine Data Sets

env_vars<-env_var%>% dplyr::rename(Site = local_site) %>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))



#Environmental Water_chem, Zoo
all<-left_join(env_vars,ordintation.env, by=c("Site", "year", "month","day", "day_of_year"))%>%drop_na()

water_temp<-dt1%>%dplyr::select(c(local_site,date,chl_a, pH, temp))%>% dplyr::rename(Site = local_site) %>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))

all_data<-left_join(all,water_temp, by=c("Site", "year", "month","day", "day_of_year"))%>%drop_na()

               