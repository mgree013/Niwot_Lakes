#knb-lter-nwt.64.1 
#Data Explration NIWOT
#Monday Sept 1, 2020

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

#Data set dt1 is phytoplabkton composition only for GL4
#We also need to locate datasets with environmental variables!

###############################################################################################################################################################################################################################################
#1) Zooplankton Diversity


#1A)Organize Data
#Make Data Site*Species  Data Frame (the data is in long format currently and we want it wider)

#Lets reshape the dataset to have our columns as species names and rows as unique location,sites, and date.
site_species <- cast(dt1, date+depth ~ taxon, value='cell_count')
site_species <- as.data.frame(site_species)
site_species[is.na(site_species)] <- 0
str(site_species)

#Make one column for site, location and date, well remove these later
site_species<-site_species%>% unite("Site.loc.date", c(depth,date))%>%
  remove_rownames()%>%
  column_to_rownames('Site.loc.date')

#1B) Calculate Diversity Metrics
#look up what these diversity metrics mean!

diversity<-site_species%>%
  dplyr::select(-c("Unknowns", "Unknown bacilariophyte", "Unknown chlorophyte"))%>% # remove unknonwn and total denstiies
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
  separate("Site.loc.date", sep="_" ,into=c("depth", "date"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))


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
  facet_wrap(~ var, scales = "free") +
  theme_bw()


#Within year variaiton (x-axis= day of year)
diversity%>%
  gather(N0,N1,E10,Com.Size, betas.LCBD, key = "var", value = "value") %>% 
  ggplot(aes(x = day_of_year, y = value, colour=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "loess")+
  facet_wrap(year~ var, scales = "free") +
  theme_bw()


#look at sites and diversity metrics indiviudally
diversity%>%
  ggplot(aes(x = day_of_year, y = N0, colour=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  #geom_smooth(method = "loess", se=F)+
  #facet_wrap(~year, scales = "free") +
  #xlim(0,360)+
  theme_bw()

diversity%>%
  ggplot(aes(x = day_of_year, y = E10, colour=year)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  theme_bw()


################################################################################################################################################################################################################################################
#Multivariate Statistics

#NMDS (Non-Metric Multideminsional Scaling)
#https://en.wikipedia.org/wiki/Multidimensional_scaling

#We have a lot of different species at a lot of sites, which makes this data a bit comlex
#one way to simlpify things is by using ordination techniques which reduces the deminsionality of our data to a few axes
#each species and sites/year/location gets a score and its a good way to visaulize community data

ordintation<-site_species%>%
  dplyr::select(-c("Unknowns", "Unknown bacilariophyte", "Unknown chlorophyte"))%>% # remove unknonwn and total denstiies
  rownames_to_column("Site.loc.date")%>%
  mutate(Sum=rowSums(site_species))%>%
  filter(Sum>0)%>%
  dplyr::select(-c(Sum))


ordintation.env<-ordintation%>%
  separate("Site.loc.date", sep="_" ,into=c("depth", "date"))%>%
  mutate(day_of_year=lubridate::yday(date))%>%
  separate("date", sep="-" ,into=c("year", "month", "day"))



dune.rel<-decostand(ordintation[,2:24],"total") #standardize community data
dune.bray<-vegdist(dune.rel) #calcualte dissimilarity among sites (i.e. dissimiliarity matrix)
dune.nmds=metaMDS(dune.rel, k=2, try=100) #NMDS code
dune.nmds
stressplot(dune.nmds) #this tells us if our plot is going to work, and it looks good

#group by sites
plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 18) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordihull(dune.nmds, groups=ordintation.env$year, draw="polygon", label=T)
ordisurf(dune.nmds, ordintation.env$day_of_year, prioirty=,labcex=0.9, add = T,col="forestgreen")

#Group by Year
plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 1) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordihull(dune.nmds, groups=ordintation.env$year, draw="polygon", label=T)
ordisurf(dune.nmds, ordintation.env$day_of_year, prioirty=,labcex=0.9, add = T,col="forestgreen")


#PERMANOVA
adonis2(dune.bray ~ ordintation.env$day_of_year+ ordintation.env$year+ordintation.env$depth, permutations = 999)

adonis2(dune.bray ~ ordintation.env$day_of_year+ ordintation.env$year+ordintation.env$depth, permutations = 999)

#it looks like most variaiton explained by  year, then day of year, and not so much by depth
########################################################################################################################################################################################
#TO DO:

#Explore trends in the various diversity metrics and look at how variation is structured among year and within years
########################################################