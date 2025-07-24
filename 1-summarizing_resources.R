library(tidyverse)
library(stringr)
#Import and combine data####
##Pull food resource STOICH data from NEON sites in latest stoich database release####
#set to location of file on computer
STOICH_20241220 <- read_csv("January 2025 analysis/STOICH_Beta_Release_Full_Join_20241220.csv")
str(STOICH_20241220)
##Pull NEON site code list and add siteID column to stoich data####
#load in NEON site name to code sheet to convert names to 4 letter code
NEON_SiteMap_Table <- read_csv("January 2025 analysis/NEON-SiteMap-Table.csv")
#add siteID column to match NEON invert data (need to make siteNAME in table all lowercase to match STOICH)
NEON_SiteMap_Table$siteName<-tolower(NEON_SiteMap_Table$siteName)
#remove lake and prairie pothole sites
NEON_SiteMap_Table<-NEON_SiteMap_Table%>%
  filter(!str_detect(siteName, regex("lake", ignore_case = TRUE)))%>%
  filter(!str_detect(siteName, regex("pothole", ignore_case = TRUE)))
#fix rio issue in stoich database
names<- STOICH_20241220%>%
  summarise(unique(Name.Site))%>%
  arrange()

STOICH_20241220<-STOICH_20241220%>%
  mutate(Name.Site= case_when(Name.Site=="río cupeyes neon"~ "rio cupeyes neon",
                              Name.Site=="río yahuecas neon" ~ "rio yahuecas neon",
                              .default = Name.Site))
#add siteID and keep only sites in NEON_SiteMap_Table
STOICH_20241220<-STOICH_20241220%>%
  left_join(NEON_SiteMap_Table[,1:2], join_by("Name.Site"=="siteName"))%>%
  rename(siteID = siteCode)%>%
  drop_na(siteID)
#Filter to only periphyton and seston data####
periphyton_data<-STOICH_20241220%>%
  select(siteID,SampleDate.SampleEvent,Name.Site,Latitude.Site,Longitude.Site,EcosystemType.Site,Type.OrganismStoichiometry,
         CarbonMean.OrganismStoichiometry, CarbonUnits.OrganismStoichiometry,
         NitrogenMean.OrganismStoichiometry, NitrogenUnits.OrganismStoichiometry,
         PhosphorusMean.OrganismStoichiometry, PhosphorusUnits.OrganismStoichiometry,
         CarbonToNitrogenRatio.OrganismStoichiometry, CarbonToPhosphorusRatio.OrganismStoichiometry,
         NitrogenToPhosphorusRatio.OrganismStoichiometry)%>%
  filter(str_detect(Name.Site,"neon"),
         Type.OrganismStoichiometry %in% c("Periphyton"),
         CarbonUnits.OrganismStoichiometry != "ugC_L")

seston_data<-STOICH_20241220%>%
  select(siteID,SampleDate.SampleEvent,Name.Site,Latitude.Site,Longitude.Site,EcosystemType.Site,Type.OrganismStoichiometry,
         CarbonMean.OrganismStoichiometry, CarbonUnits.OrganismStoichiometry,
         NitrogenMean.OrganismStoichiometry, NitrogenUnits.OrganismStoichiometry,
         PhosphorusMean.OrganismStoichiometry, PhosphorusUnits.OrganismStoichiometry,
         CarbonToNitrogenRatio.OrganismStoichiometry, CarbonToPhosphorusRatio.OrganismStoichiometry,
         NitrogenToPhosphorusRatio.OrganismStoichiometry)%>%
  filter(str_detect(Name.Site,"neon"),
         Type.OrganismStoichiometry %in% c("Seston"))

#Summarise each by location####

summarized_peri<- periphyton_data %>% 
  group_by(siteID) %>% 
  summarise(mean_C_ug_cm2=mean(CarbonMean.OrganismStoichiometry,na.rm=TRUE),
            mean_N_ug_cm2=mean(NitrogenMean.OrganismStoichiometry,na.rm=TRUE),
            mean_P_ug_cm2=mean(PhosphorusMean.OrganismStoichiometry,na.rm=TRUE),
            mean_CN=mean(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            mean_CP=mean(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            mean_NP=mean(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            min_CN=min(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            min_CP=min(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            min_NP=min(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            max_CN=max(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            max_CP=max(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            max_NP=max(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE))

summarized_seston<- seston_data %>% 
  group_by(siteID) %>% 
  summarise(mean_C_ug_cm2=mean(CarbonMean.OrganismStoichiometry,na.rm=TRUE),
            mean_N_ug_cm2=mean(NitrogenMean.OrganismStoichiometry,na.rm=TRUE),
            mean_P_ug_cm2=mean(PhosphorusMean.OrganismStoichiometry,na.rm=TRUE),
            mean_CN=mean(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            mean_CP=mean(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            mean_NP=mean(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            min_CN=min(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            min_CP=min(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            min_NP=min(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            max_CN=max(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            max_CP=max(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            max_NP=max(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE))

write.csv(summarized_peri,'January 2025 analysis/generated data/site_level_average_periphyton.csv', na="", row.names = F)
write.csv(summarized_seston,'January 2025 analysis/generated data/site_level_average_seston.csv', na="", row.names = F)
