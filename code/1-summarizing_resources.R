library(tidyverse)
library(stringr)
#Import and combine data####
##Import previously filtered STOICH NEON food resource data from NEON sites####
STOICH_periphyton <- read_csv("January 2025 analysis/STOICH_periphyton.csv")
STOICH_seston <- read_csv("January 2025 analysis/STOICH_seston.csv")
#Filter to only periphyton and seston data####
periphyton_data<-STOICH_periphyton%>%
  select(siteID,SampleDate.SampleEvent,Name.Site,Latitude.Site,Longitude.Site,EcosystemType.Site,Type.OrganismStoichiometry,
         CarbonMean.OrganismStoichiometry, CarbonUnits.OrganismStoichiometry,
         NitrogenMean.OrganismStoichiometry, NitrogenUnits.OrganismStoichiometry,
         PhosphorusMean.OrganismStoichiometry, PhosphorusUnits.OrganismStoichiometry,
         CarbonToNitrogenRatio.OrganismStoichiometry, CarbonToPhosphorusRatio.OrganismStoichiometry,
         NitrogenToPhosphorusRatio.OrganismStoichiometry)%>%
  filter(str_detect(Name.Site,"neon"),
         Type.OrganismStoichiometry %in% c("Periphyton"),
         CarbonUnits.OrganismStoichiometry != "ugC_L")

seston_data<-STOICH_seston%>%
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
