library(tidyverse)
library(readxl)
#import neon summarized data as well as stoich data summarized across different levels####
NEON_count_sum <- read_csv("January 2025 analysis/generated data/NEON_count_sum.csv")
Invert_stoich_genus_site<- read_csv("January 2025 analysis/generated data/Invert_stoich_genus_site.csv")
Invert_stoich_family_site <- read_csv("January 2025 analysis/generated data/Invert_stoich_family_site.csv")
Invert_stoich_genus <- read_csv("January 2025 analysis/generated data/Invert_stoich_genus.csv")
Invert_stoich_family<- read_csv("January 2025 analysis/generated data/Invert_stoich_family.csv")
site_level_average_seston <- read_csv("January 2025 analysis/generated data/site_level_average_seston.csv")
site_level_average_periphyton <- read_csv("January 2025 analysis/generated data/site_level_average_periphyton.csv")
#import and correct ffg list
taxa_FFG <- read_excel("January 2025 analysis/taxa_FFG.xlsx")

taxa_FFG_genus<- taxa_FFG%>%
  select(!Species_Organism)%>%
  distinct()
#drop genus and get distinct options, a few family have overlap so chose the most prevalent
taxa_FFG_family<-taxa_FFG%>%
  select(!c(Species_Organism,genus))%>%
  distinct()

taxa_FFG_family<-taxa_FFG_family[-c(2,12,25,27,29,30,35,44,48,51,53,81,97),]
#Add stoichiometry data to mean NEON biomass data####
##Scenario 1####
#first fill in by genus by site,then family by site, overall genus , overall family
NEON_taxa_stoich_scenario1<-NEON_count_sum%>%
  left_join(Invert_stoich_genus_site)%>%
  left_join(Invert_stoich_family_site, join_by(siteID, order, family, invertebrateLifeStage))%>%
  left_join(Invert_stoich_genus, join_by(order, family, genus, invertebrateLifeStage))%>%
  left_join(Invert_stoich_family, join_by(order, family, invertebrateLifeStage))

#Coalesce columns in order of addition, label match level and select only final values
NEON_taxa_stoich_scenario1<-NEON_taxa_stoich_scenario1%>%
  mutate(mean_CN=coalesce(mean_CN.x, mean_CN.y, mean_CN.x.x, mean_CN.y.y),
         mean_CP=coalesce(mean_CP.x, mean_CP.y, mean_CP.x.x, mean_CP.y.y),
         mean_NP=coalesce(mean_NP.x, mean_NP.y, mean_NP.x.x, mean_NP.y.y))%>%
  mutate(match_level=case_when(is.na(mean_CN.x)==F~"local_genus",
                                  is.na(mean_CN.x)==T & is.na(mean_CN.y)==F~"local_family",
                                  is.na(mean_CN.x)==T & is.na(mean_CN.y)==T& is.na(mean_CN.x.x)==F~"overall_genus",
                                  is.na(mean_CN.x)==T & is.na(mean_CN.y)==T& is.na(mean_CN.x.x)==T& is.na(mean_CN.y.y)==F~"overall_family",
                                  is.na(mean_CN.x)==T & is.na(mean_CN.y)==T& is.na(mean_CN.x.x)==T& is.na(mean_CN.y.y)==T~NA))%>%
  select(siteID:sd_biomass_mg_m2, mean_CN, mean_CP, mean_NP, match_level)
#filter missing stoich data--3,666->2313
NEON_taxa_stoich_scenario1<-NEON_taxa_stoich_scenario1%>%
  filter(is.na(mean_CN)==F & is.na(mean_CP)==F)
###Add FFG first by genus, then family if not available####
NEON_taxa_stoich_scenario1<-NEON_taxa_stoich_scenario1%>%
  left_join(drop_na(taxa_FFG_genus[,c(3,4)]))%>%
  left_join(drop_na(taxa_FFG_family[,c(2,3)]),join_by("family"=="family"))

NEON_taxa_stoich_scenario1<-NEON_taxa_stoich_scenario1%>%
  mutate(ffg=coalesce(FFG.x, FFG.y))%>%
  select(!c(FFG.x,FFG.y))%>%
  distinct()
#Filter to FFG of interest 2313 ->1822
NEON_taxa_stoich_scenario1<-NEON_taxa_stoich_scenario1%>%
  filter(ffg %in% c("HB", "CG", "CF"))

###summarise whole community stoich by site and functional feeding group after calculating total biomass per sample and scaling by relative biomass####
#add in appropriate food resource mean stoich
scenario1_comm_peri_ffg<-NEON_taxa_stoich_scenario1%>%
  filter(ffg =="HB")%>%
  group_by(siteID)%>%
  mutate(total_biomass_mg_m2=sum(mean_biomass_mg_m2))%>%
  ungroup()%>%
  mutate(mean_CN=mean_biomass_mg_m2/total_biomass_mg_m2*mean_CN,
         mean_CP=mean_biomass_mg_m2/total_biomass_mg_m2*mean_CP,
         mean_NP=mean_biomass_mg_m2/total_biomass_mg_m2*mean_NP)%>%
  group_by(siteID, ffg)%>%
  summarise(taxa_CN=sum(mean_CN),
            taxa_CP=sum(mean_CP),
            taxa_NP=sum(mean_NP))%>%
  left_join(site_level_average_periphyton[,c(1,5:7)])%>%
  mutate(scenario=1)

scenario1_comm_ses_ffg<-NEON_taxa_stoich_scenario1%>%
  filter(ffg %in% c("CF","CG"))%>%
  group_by(siteID, ffg)%>%
  mutate(total_biomass_mg_m2=sum(mean_biomass_mg_m2))%>%
  ungroup()%>%
  mutate(mean_CN=mean_biomass_mg_m2/total_biomass_mg_m2*mean_CN,
         mean_CP=mean_biomass_mg_m2/total_biomass_mg_m2*mean_CP,
         mean_NP=mean_biomass_mg_m2/total_biomass_mg_m2*mean_NP)%>%
  group_by(siteID, ffg)%>%
  summarise(taxa_CN=sum(mean_CN),
            taxa_CP=sum(mean_CP),
            taxa_NP=sum(mean_NP))%>%
  left_join(site_level_average_seston[,c(1,5:7)])%>%
  mutate(scenario=1)

scenario1_comm_ffg<-bind_rows(scenario1_comm_peri_ffg, scenario1_comm_ses_ffg)%>%drop_na()

##Scenario 2 ####
#first fill in by overall genus ,then overall family
NEON_taxa_stoich_scenario2<-NEON_count_sum%>%
  left_join(Invert_stoich_genus, join_by(order, family, genus, invertebrateLifeStage))%>%
  left_join(Invert_stoich_family, join_by(order, family, invertebrateLifeStage))

#Coalesce columns in order of addition, label match level and select only final values
NEON_taxa_stoich_scenario2<-NEON_taxa_stoich_scenario2%>%
  mutate(mean_CN=coalesce(mean_CN.x, mean_CN.y),
         mean_CP=coalesce(mean_CP.x, mean_CP.y),
         mean_NP=coalesce(mean_NP.x, mean_NP.y))%>%
  mutate(match_level=case_when(is.na(mean_CN.x)==F~"overall_genus",
                               is.na(mean_CN.x)==T & is.na(mean_CN.y)==F~"overall_family",
                               is.na(mean_CN.x)==T & is.na(mean_CN.y)==T~NA))%>%
  select(siteID:sd_biomass_mg_m2, mean_CN, mean_CP, mean_NP, match_level)
#filter missing stoich data--3666->2313
NEON_taxa_stoich_scenario2<-NEON_taxa_stoich_scenario2%>%
  filter(is.na(mean_CN)==F & is.na(mean_CP)==F)
###Add FFG first by genus, then family if not available####
NEON_taxa_stoich_scenario2<-NEON_taxa_stoich_scenario2%>%
  left_join(drop_na(taxa_FFG_genus[,c(3,4)]))%>%
  left_join(drop_na(taxa_FFG_family[,c(2,3)]),join_by("family"=="family"))

NEON_taxa_stoich_scenario2<-NEON_taxa_stoich_scenario2%>%
  mutate(ffg=coalesce(FFG.x, FFG.y))%>%
  select(!c(FFG.x,FFG.y))%>%
  distinct()
#Filter to FFG of interest 2313 ->1822
NEON_taxa_stoich_scenario2<-NEON_taxa_stoich_scenario2%>%
  filter(ffg %in% c("HB", "CG", "CF"))

###summarise whole community stoich by site and functional feeding group after calculating biomass of C,N,P and C:N,C:P and N:P####
scenario2_comm_peri_ffg<-NEON_taxa_stoich_scenario2%>%
  filter(ffg=="HB")%>%
  group_by(siteID)%>%
  mutate(total_biomass_mg_m2=sum(mean_biomass_mg_m2))%>%
  ungroup()%>%
  mutate(mean_CN=mean_biomass_mg_m2/total_biomass_mg_m2*mean_CN,
         mean_CP=mean_biomass_mg_m2/total_biomass_mg_m2*mean_CP,
         mean_NP=mean_biomass_mg_m2/total_biomass_mg_m2*mean_NP)%>%
  group_by(siteID, ffg)%>%
  summarise(taxa_CN=sum(mean_CN),
            taxa_CP=sum(mean_CP),
            taxa_NP=sum(mean_NP))%>%
  left_join(site_level_average_periphyton[,c(1,5:7)])%>%
  mutate(scenario=2)

scenario2_comm_ses_ffg<-NEON_taxa_stoich_scenario2%>%
  filter(ffg%in% c("CF","CG"))%>%
  group_by(siteID, ffg)%>%
  mutate(total_biomass_mg_m2=sum(mean_biomass_mg_m2))%>%
  ungroup()%>%
  mutate(mean_CN=mean_biomass_mg_m2/total_biomass_mg_m2*mean_CN,
         mean_CP=mean_biomass_mg_m2/total_biomass_mg_m2*mean_CP,
         mean_NP=mean_biomass_mg_m2/total_biomass_mg_m2*mean_NP)%>%
  group_by(siteID, ffg)%>%
  summarise(taxa_CN=sum(mean_CN),
            taxa_CP=sum(mean_CP),
            taxa_NP=sum(mean_NP))%>%
  left_join(site_level_average_seston[,c(1,5:7)])%>%
  mutate(scenario=2)

scenario2_comm_ffg<-bind_rows(scenario2_comm_peri_ffg,scenario2_comm_ses_ffg)%>%drop_na()
##Scenario 3####
###Generate an overall taxa min, max from Invert_stoich_family for each FFG ####
Invert_stoich_min_max<-Invert_stoich_family%>%
  left_join(drop_na(taxa_FFG_family[,c(2,3)]),join_by("family"=="family"))%>%
  filter(FFG %in% c("HB", "CG", "CF"))%>%
  group_by(FFG)%>%
  reframe(min_CN=min(min_CN),
          max_CN=max(max_CN),
          min_CP=min(min_CP),
          max_CP=max(max_CP),
          min_NP=min(min_NP),
          max_NP=max(max_NP))

###Periphyton dataset ####
#create periphyton dataset by asking if the resource at a site is within the taxa range, if yes, taxa stoich=resource,
#if greater then max of the ratio, if less, then min of the ratio--herbivores only
Neon_taxa_peri_scenario3<-site_level_average_periphyton%>%
  mutate(taxa_CN=case_when(Invert_stoich_min_max[3,]$min_CN<mean_CN & mean_CN<Invert_stoich_min_max[3,]$max_CN~mean_CN,
                           mean_CN<Invert_stoich_min_max[3,]$min_CN~Invert_stoich_min_max[3,]$min_CN,
                           mean_CN>Invert_stoich_min_max[3,]$max_CN~Invert_stoich_min_max[3,]$max_CN),
         taxa_CP=case_when(Invert_stoich_min_max[3,]$min_CP<mean_CP & mean_CP<Invert_stoich_min_max[3,]$max_CP~mean_CP,
                           mean_CP<Invert_stoich_min_max[3,]$min_CP~Invert_stoich_min_max[3,]$min_CP,
                           mean_CP>Invert_stoich_min_max[3,]$max_CP~Invert_stoich_min_max[3,]$max_CP),
         taxa_NP=case_when(Invert_stoich_min_max[3,]$min_NP<mean_NP & mean_NP<Invert_stoich_min_max[3,]$max_NP~mean_NP,
                           mean_NP<Invert_stoich_min_max[3,]$min_NP~Invert_stoich_min_max[3,]$min_NP,
                           mean_NP>Invert_stoich_min_max[3,]$max_NP~Invert_stoich_min_max[3,]$max_NP))%>%
  select(siteID, mean_CN, mean_CP, mean_NP, taxa_CN, taxa_CP, taxa_NP)%>%
  mutate(scenario=3, ffg="HB")

###Seston dataset#### 
#CF
#create seston dataset by asking if the resource at a site is within the taxa range, if yes, taxa stoich=resource,
#if greater then max of the ratio, if less, then min of the ratio--cf only
Neon_taxa_ses_cf_scenario3<-site_level_average_seston%>%
  mutate(taxa_CN=case_when(Invert_stoich_min_max[1,]$min_CN<mean_CN & mean_CN<Invert_stoich_min_max[1,]$max_CN~mean_CN,
                           mean_CN<Invert_stoich_min_max[1,]$min_CN~Invert_stoich_min_max[1,]$min_CN,
                           mean_CN>Invert_stoich_min_max[1,]$max_CN~Invert_stoich_min_max[1,]$max_CN),
         taxa_CP=case_when(Invert_stoich_min_max[1,]$min_CP<mean_CP & mean_CP<Invert_stoich_min_max[1,]$max_CP~mean_CP,
                           mean_CP<Invert_stoich_min_max[1,]$min_CP~Invert_stoich_min_max[1,]$min_CP,
                           mean_CP>Invert_stoich_min_max[1,]$max_CP~Invert_stoich_min_max[1,]$max_CP),
         taxa_NP=case_when(Invert_stoich_min_max[1,]$min_NP<mean_NP & mean_NP<Invert_stoich_min_max[1,]$max_NP~mean_NP,
                           mean_NP<Invert_stoich_min_max[1,]$min_NP~Invert_stoich_min_max[1,]$min_NP,
                           mean_NP>Invert_stoich_min_max[1,]$max_NP~Invert_stoich_min_max[1,]$max_NP))%>%
  select(siteID, mean_CN, mean_CP, mean_NP, taxa_CN, taxa_CP, taxa_NP)%>%
  mutate(scenario=3, ffg="CF")

#CG
#create seston dataset by asking if the resource at a site is within the taxa range, if yes, taxa stoich=resource,
#if greater then max of the ratio, if less, then min of the ratio--cg only
Neon_taxa_ses_cg_scenario3<-site_level_average_seston%>%
  mutate(taxa_CN=case_when(Invert_stoich_min_max[2,]$min_CN<mean_CN & mean_CN<Invert_stoich_min_max[2,]$max_CN~mean_CN,
                           mean_CN<Invert_stoich_min_max[2,]$min_CN~Invert_stoich_min_max[2,]$min_CN,
                           mean_CN>Invert_stoich_min_max[2,]$max_CN~Invert_stoich_min_max[2,]$max_CN),
         taxa_CP=case_when(Invert_stoich_min_max[2,]$min_CP<mean_CP & mean_CP<Invert_stoich_min_max[2,]$max_CP~mean_CP,
                           mean_CP<Invert_stoich_min_max[2,]$min_CP~Invert_stoich_min_max[2,]$min_CP,
                           mean_CP>Invert_stoich_min_max[2,]$max_CP~Invert_stoich_min_max[2,]$max_CP),
         taxa_NP=case_when(Invert_stoich_min_max[2,]$min_NP<mean_NP & mean_NP<Invert_stoich_min_max[2,]$max_NP~mean_NP,
                           mean_NP<Invert_stoich_min_max[2,]$min_NP~Invert_stoich_min_max[2,]$min_NP,
                           mean_NP>Invert_stoich_min_max[2,]$max_NP~Invert_stoich_min_max[2,]$max_NP))%>%
  select(siteID, mean_CN, mean_CP, mean_NP, taxa_CN, taxa_CP, taxa_NP)%>%
  mutate(scenario=3, ffg="CG")

###combine into single scenario 3 data set####
scenario3_comm_ffg<-bind_rows(Neon_taxa_peri_scenario3, Neon_taxa_ses_cf_scenario3, Neon_taxa_ses_cg_scenario3)

##Scenario 4 ####
###Generate an site level taxa min, max from Invert_stoich_family for each FFG ####
Invert_stoich_min_max_site<-Invert_stoich_family_site%>%
  left_join(drop_na(taxa_FFG_family[,c(2,3)]),join_by("family"=="family"))%>%
  filter(FFG %in% c("HB", "CG", "CF"))%>%
  group_by(siteID,FFG)%>%
  reframe(taxa_min_CN=min(min_CN),
          taxa_min_CP=min(min_CP),
          taxa_min_NP=min(min_NP),
          taxa_max_CN=max(max_CN),
          taxa_max_CP=max(max_CP),
          taxa_max_NP=max(max_NP))

###Periphyton dataset ####
#create periphyton dataset by asking if the resource at a site is within the site level taxa range, if yes, taxa stoich=resource,
#if greater then max of the ratio, if less, then min of the ratio--herbivores only
Invert_stoich_min_max_site_HB<-Invert_stoich_min_max_site%>%
  filter(FFG=="HB")

Neon_taxa_peri_scenario4<-site_level_average_periphyton%>%
  left_join(Invert_stoich_min_max_site_HB)%>%
  drop_na(taxa_min_CN)%>%
  group_by(siteID)%>%
  mutate(taxa_CN=case_when(mean_CN>taxa_min_CN & mean_CN<taxa_max_CN~mean_CN,
                           mean_CN<taxa_min_CN~taxa_min_CN,
                           mean_CN>taxa_max_CN~taxa_max_CN),
         taxa_CP=case_when(mean_CP>taxa_min_CP & mean_CP<taxa_max_CP~mean_CP,
                           mean_CP<taxa_min_CP~taxa_min_CP,
                           mean_CP>taxa_max_CP~taxa_max_CP),
         taxa_NP=case_when(mean_NP>taxa_min_NP & mean_NP<taxa_max_NP~mean_NP,
                           mean_NP<taxa_min_NP~taxa_min_NP,
                           mean_NP>taxa_max_NP~taxa_max_NP))%>%
  select(siteID, mean_CN, mean_CP, mean_NP, taxa_CN, taxa_CP, taxa_NP)%>%
  mutate(scenario=4, ffg="HB")

###Seston dataset#### 
#CF
#create seston dataset by asking if the resource at a site is within the taxa range, if yes, taxa stoich=resource,
#if greater then max of the ratio, if less, then min of the ratio--cf only
Invert_stoich_min_max_site_CF<-Invert_stoich_min_max_site%>%
  filter(FFG=="CF")

Neon_taxa_ses_CF_scenario4<-site_level_average_periphyton%>%
  left_join(Invert_stoich_min_max_site_CF)%>%
  drop_na(taxa_min_CN)%>%
  group_by(siteID)%>%
  mutate(taxa_CN=case_when(mean_CN>taxa_min_CN & mean_CN<taxa_max_CN~mean_CN,
                           mean_CN<taxa_min_CN~taxa_min_CN,
                           mean_CN>taxa_max_CN~taxa_max_CN),
         taxa_CP=case_when(mean_CP>taxa_min_CP & mean_CP<taxa_max_CP~mean_CP,
                           mean_CP<taxa_min_CP~taxa_min_CP,
                           mean_CP>taxa_max_CP~taxa_max_CP),
         taxa_NP=case_when(mean_NP>taxa_min_NP & mean_NP<taxa_max_NP~mean_NP,
                           mean_NP<taxa_min_NP~taxa_min_NP,
                           mean_NP>taxa_max_NP~taxa_max_NP))%>%
  select(siteID, mean_CN, mean_CP, mean_NP, taxa_CN, taxa_CP, taxa_NP)%>%
  mutate(scenario=4, ffg="CF")

#CG
#create seston dataset by asking if the resource at a site is within the taxa range, if yes, taxa stoich=resource,
#if greater then max of the ratio, if less, then min of the ratio--cg only
Invert_stoich_min_max_site_CG<-Invert_stoich_min_max_site%>%
  filter(FFG=="CG")

Neon_taxa_ses_CG_scenario4<-site_level_average_periphyton%>%
  left_join(Invert_stoich_min_max_site_CG)%>%
  drop_na(taxa_min_CN)%>%
  group_by(siteID)%>%
  mutate(taxa_CN=case_when(mean_CN>taxa_min_CN & mean_CN<taxa_max_CN~mean_CN,
                           mean_CN<taxa_min_CN~taxa_min_CN,
                           mean_CN>taxa_max_CN~taxa_max_CN),
         taxa_CP=case_when(mean_CP>taxa_min_CP & mean_CP<taxa_max_CP~mean_CP,
                           mean_CP<taxa_min_CP~taxa_min_CP,
                           mean_CP>taxa_max_CP~taxa_max_CP),
         taxa_NP=case_when(mean_NP>taxa_min_NP & mean_NP<taxa_max_NP~mean_NP,
                           mean_NP<taxa_min_NP~taxa_min_NP,
                           mean_NP>taxa_max_NP~taxa_max_NP))%>%
  select(siteID, mean_CN, mean_CP, mean_NP, taxa_CN, taxa_CP, taxa_NP)%>%
  mutate(scenario=4, ffg="CG")

###combine into single scenario 4 data set####
scenario4_comm_ffg<-bind_rows(Neon_taxa_peri_scenario4, Neon_taxa_ses_CF_scenario4, Neon_taxa_ses_CG_scenario4)

#Bind all datasets into single one and write to csv####
scenarios<-bind_rows(scenario1_comm_ffg, scenario2_comm_ffg, scenario3_comm_ffg, scenario4_comm_ffg)

scenarios<-scenarios%>%
  rename(res_CN=mean_CN,
         res_CP=mean_CP,
         res_NP=mean_NP)
#write to csv
write.csv(scenarios,  'January 2025 analysis/generated data/scenarios.csv', na="", row.names = F)

