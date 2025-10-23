library(neonUtilities)
library(tidyverse)
library(stringr)
library(readxl)
#download NEON data over time range-skip to line 22 if already run ####
#will get a rate limit reached warning, just let it keep going
NEON_taxa_utilities<-loadByProduct(dpID="DP1.20120.001",
                                   site="all",
                                   startdate ="2014-07-01",
                                   enddate ="2022-12-31",
                                   package="expanded", check.size=F)

#check time range and get number of sampling events at each site
data_range<-NEON_taxa_utilities$inv_taxonomyProcessed%>%
  group_by(siteID)%>%
  summarise(startDate=min(collectDate), endDate=max(collectDate), n=length(unique(collectDate)))%>%
  print(n=34)
##Filter to processed taxonomy####
NEON_taxa_processed<-NEON_taxa_utilities$inv_taxonomyProcessed
write.csv(NEON_taxa_processed,'January 2025 analysis/generated data/NEON_taxa_processed.csv', na="", row.names = F)
##load NEON taxa processed if already saved from above
NEON_taxa_processed <- read_csv("January 2025 analysis/generated data/NEON_taxa_processed.csv")
##load in NEON site name to code sheet to filter lake and prairie pothole sites####
NEON_SiteMap_Table <- read_csv("January 2025 analysis/NEON-SiteMap-Table.csv")
#add siteID column to match NEON invert data (need to make siteNAME in table all lowercase to match STOICH)
NEON_SiteMap_Table$siteName<-tolower(NEON_SiteMap_Table$siteName)

NEON_SiteMap_Table<-NEON_SiteMap_Table%>%
  filter(!str_detect(siteName, regex("lake", ignore_case = TRUE)))%>%
  filter(!str_detect(siteName, regex("pothole", ignore_case = TRUE)))
#Keep only site in the NEON_SiteMap_Table
NEON_taxa_processed<-NEON_taxa_processed%>%
  semi_join(NEON_SiteMap_Table[,1], join_by("siteID"=="siteCode"))
##Separate sampler type and scale to abundance per m2####
NEON_taxa_processed<-NEON_taxa_processed%>%
  mutate(sampleID=str_remove(sampleID,"SS."))%>%
  separate(sampleID, c("Site","Date","Sampler","Sample_num","note"))

NEON_taxa_processed<-NEON_taxa_processed%>%
  filter(Sampler %in% c("SURBER","HESS","PONAR"))%>%
  filter(is.na(note)==T)%>%
  mutate(area_m2=case_when(Sampler == "SURBER" ~ 0.09,
                           Sampler =="HESS"~0.09,
                           Sampler=="PONAR"~0.023))%>%
  mutate(abund_m2=area_m2*estimatedTotalCount)

##Calculate biomass per m2####
#length-mass relationship by order
benke<-read.csv("January 2025 analysis/benke_length_mass.csv")
#add order level conversions and calculate
NEON_taxa_processed<-NEON_taxa_processed%>%
  left_join(benke)

NEON_taxa_processed<-NEON_taxa_processed%>%
  mutate(biomass_mg_m2=a*sizeClass^(b)*abund_m2)

##Calculate mean biomass per site per taxa####
#remove taxa without biomass
NEON_taxa_processed<-NEON_taxa_processed%>%
  filter(is.na(biomass_mg_m2)==F)
#add lowest taxa column for summarizing--drop anything ID above family here, lose about 887 of 90,333
NEON_taxa_processed<-NEON_taxa_processed%>%
  mutate(lowest_id= case_when(is.na(genus)==F~ genus,
                              is.na(genus)==T~family))%>%
  filter(is.na(lowest_id)==F)

NEON_taxa_sum<-NEON_taxa_processed%>%
  group_by(siteID,order, family, genus,invertebrateLifeStage, lowest_id)%>%
  summarise(mean_biomass_mg_m2=mean(biomass_mg_m2),
            sd_biomass_mg_m2=sd(biomass_mg_m2))
  
#write to csv
write.csv(NEON_taxa_sum,'January 2025 analysis/generated data/NEON_count_sum.csv', na="", row.names = F)

#Load STOICH invert data####
STOICH_invert <- read_csv("January 2025 analysis/STOICH_invert.csv")
##get list of available taxa overall####
Invert_stoich_taxa<-STOICH_invert%>%
  group_by(order, family, genus)%>%
  summarise()%>%
  drop_na(family)

##make genus level summary of available stoich data by site####
Invert_stoich_genus_site<-STOICH_invert%>%
  filter(is.na(genus)==F)%>%
  filter(is.na(P)==F)%>%
  group_by(siteID, order, family, genus, invertebrateLifeStage)%>%
  summarise(mean_CN=mean(CN, na.rm=T),
            mean_CP=mean(CP, na.rm=T),
            mean_NP=mean(NP, na.rm=T),
            min_CN=min(CN, na.rm=T),
            min_CP=min(CP, na.rm=T),
            min_NP=min(NP, na.rm=T),
            max_CN=max(CN, na.rm=T),
            max_CP=max(CP, na.rm=T),
            max_NP=max(NP, na.rm=T))


#write to csv
write.csv(Invert_stoich_genus_site, 'January 2025 analysis/generated data/Invert_stoich_genus_site.csv', na="", row.names = F)

##make family level summary of available stoich data by site####
Invert_stoich_family_site<-STOICH_invert%>%
  filter(is.na(family)==F)%>%
  filter(is.na(P)==F)%>%
  group_by(siteID, order, family, invertebrateLifeStage)%>%
  summarise(mean_CN=mean(CN, na.rm=T),
            mean_CP=mean(CP, na.rm=T),
            mean_NP=mean(NP, na.rm=T),
            min_CN=min(CN, na.rm=T),
            min_CP=min(CP, na.rm=T),
            min_NP=min(NP, na.rm=T),
            max_CN=max(CN, na.rm=T),
            max_CP=max(CP, na.rm=T),
            max_NP=max(NP, na.rm=T))

#write to csv
write.csv(Invert_stoich_family_site,  'January 2025 analysis/generated data/Invert_stoich_family_site.csv', na="", row.names = F)

##make genus level summary of available stoich data overall####
Invert_stoich_genus<-STOICH_invert%>%
  filter(is.na(genus)==F)%>%
  filter(is.na(P)==F)%>%
  group_by(order, family, genus, invertebrateLifeStage)%>%
  summarise(mean_CN=mean(CN, na.rm=T),
            mean_CP=mean(CP, na.rm=T),
            mean_NP=mean(NP, na.rm=T),
            min_CN=min(CN, na.rm=T),
            min_CP=min(CP, na.rm=T),
            min_NP=min(NP, na.rm=T),
            max_CN=max(CN, na.rm=T),
            max_CP=max(CP, na.rm=T),
            max_NP=max(NP, na.rm=T))

#write to csv
write.csv(Invert_stoich_genus,'January 2025 analysis/generated data/Invert_stoich_genus.csv', na="", row.names = F)

##make family level summary of available stoich data overall####
Invert_stoich_family<-STOICH_invert%>%
  filter(is.na(family)==F)%>%
  filter(is.na(P)==F)%>%
  group_by(order, family, invertebrateLifeStage)%>%
  summarise(mean_CN=mean(CN, na.rm=T),
            mean_CP=mean(CP, na.rm=T),
            mean_NP=mean(NP, na.rm=T),
            min_CN=min(CN, na.rm=T),
            min_CP=min(CP, na.rm=T),
            min_NP=min(NP, na.rm=T),
            max_CN=max(CN, na.rm=T),
            max_CP=max(CP, na.rm=T),
            max_NP=max(NP, na.rm=T))
#write to csv
write.csv(Invert_stoich_family,  'January 2025 analysis/generated data/Invert_stoich_family.csv', na="", row.names = F)

