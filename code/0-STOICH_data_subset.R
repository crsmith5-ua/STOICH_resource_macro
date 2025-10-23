library(tidyverse)
library(stringr)
#Resource STOICH data####
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
##Filter to only periphyton and seston data####
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

write.csv(periphyton_data,'January 2025 analysis/STOICH_periphyton.csv', na="", row.names = F)
write.csv(seston_data,'January 2025 analysis/STOICH_seston.csv', na="", row.names = F)

#Invertebrate STOICH data####
##Pull invertebrate STOICH data from NEON sites in latest stoich database release####
STOICH_20241220 <- read_csv("January 2025 analysis/STOICH_Beta_Release_Full_Join_20241220.csv")
str(STOICH_20241220)
#fix rio issue in stoich database
names<- STOICH_20241220%>%
  summarise(unique(Name.Site))%>%
  arrange()

STOICH_20241220<-STOICH_20241220%>%
  mutate(Name.Site= case_when(Name.Site=="río cupeyes neon"~ "rio cupeyes neon",
                              Name.Site=="río yahuecas neon" ~ "rio yahuecas neon",
                              .default = Name.Site))
#Filter to only NEON sites
Invert_stoich_NEON<-STOICH_20241220%>%
  select(SampleDate.SampleEvent,Name.Site,Latitude.Site,Longitude.Site,EcosystemType.Site,Type.OrganismStoichiometry,
         TaxonomicKingdom.OrganismStoichiometry, TaxonomicOrder.OrganismStoichiometry, TaxonomicFamily.OrganismStoichiometry,
         TaxonomicGenus.OrganismStoichiometry, DevelopmentStage.OrganismStoichiometry, OrganismSizeNotes.OrganismStoichiometry,
         CarbonMean.OrganismStoichiometry, CarbonUnits.OrganismStoichiometry,
         NitrogenMean.OrganismStoichiometry, NitrogenUnits.OrganismStoichiometry,
         PhosphorusMean.OrganismStoichiometry, PhosphorusUnits.OrganismStoichiometry,
         CarbonToNitrogenRatio.OrganismStoichiometry,CarbonToPhosphorusRatio.OrganismStoichiometry, NitrogenToPhosphorusRatio.OrganismStoichiometry)%>%
  filter(str_detect(Name.Site,"neon"),
         Type.OrganismStoichiometry %in% c("Insecta"," Invertebrates (Other)"))

Invert_stoich_NEON<-Invert_stoich_NEON%>%
  left_join(NEON_SiteMap_Table[,1:2], join_by("Name.Site"=="siteName"))%>%
  rename(siteID = siteCode)%>%
  drop_na(siteID)

data_range_stoich<-Invert_stoich_NEON%>%
  group_by(siteID)%>%
  summarise(startDate=min(SampleDate.SampleEvent), endDate=max(SampleDate.SampleEvent), n=length(unique(SampleDate.SampleEvent)))%>%
  print(n=34)

#make all identifier columns start with capital (Order, family genus) to match FFG and NEON count data
Invert_stoich_NEON<-Invert_stoich_NEON%>%
  mutate(TaxonomicKingdom.OrganismStoichiometry=str_to_sentence(TaxonomicKingdom.OrganismStoichiometry),
         TaxonomicOrder.OrganismStoichiometry=str_to_sentence(TaxonomicOrder.OrganismStoichiometry),
         TaxonomicFamily.OrganismStoichiometry=str_to_sentence(TaxonomicFamily.OrganismStoichiometry),
         TaxonomicGenus.OrganismStoichiometry=str_to_sentence(TaxonomicGenus.OrganismStoichiometry))

Invert_stoich_NEON<-Invert_stoich_NEON%>%
  rename(order=TaxonomicOrder.OrganismStoichiometry,
         family=TaxonomicFamily.OrganismStoichiometry,
         genus=TaxonomicGenus.OrganismStoichiometry,
         invertebrateLifeStage=DevelopmentStage.OrganismStoichiometry,
         C=CarbonMean.OrganismStoichiometry,
         N=NitrogenMean.OrganismStoichiometry,
         P=PhosphorusMean.OrganismStoichiometry,
         CN=CarbonToNitrogenRatio.OrganismStoichiometry,
         CP=CarbonToPhosphorusRatio.OrganismStoichiometry,
         NP=NitrogenToPhosphorusRatio.OrganismStoichiometry)

#change all juvenile life stage to larva to match NEON data
Invert_stoich_NEON<-Invert_stoich_NEON%>%
  mutate(invertebrateLifeStage=case_when(invertebrateLifeStage=="juvenile"~"larva",
                                         invertebrateLifeStage=="larva"~"larva",
                                         invertebrateLifeStage=="adult"~"adult",
                                         invertebrateLifeStage=="pupa"~"pupa"))

write.csv(Invert_stoich_NEON,'January 2025 analysis/STOICH_invert.csv', na="", row.names = F)
