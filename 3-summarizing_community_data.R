#########################
library(dplyr)
library(ggplot2)
library(lubridate)
##########################

abund<-read_csv("NEON_count_taxa_2025-01-07.csv")
abund$Date<-date(abund$collectDate)


stoich_raw<-read.csv("NEON_stoich_raw_2025-01-07.csv")
stoich_raw$Date<-ymd(stoich_raw$SampleDate.SampleEvent)


benke<-read.csv("./data/benke_length_mass.csv")

######### reducing stoich

stoich_site <- stoich_raw %>% 
  select(siteID,TaxonomicOrder.OrganismStoichiometry,TaxonomicFamily.OrganismStoichiometry,
                                     TaxonomicGenus.OrganismStoichiometry,PhosphorusMean.OrganismStoichiometry,NitrogenMean.OrganismStoichiometry,
                                     CarbonMean.OrganismStoichiometry,CarbonToNitrogenRatio.OrganismStoichiometry,CarbonToPhosphorusRatio.OrganismStoichiometry,
                                     NitrogenToPhosphorusRatio.OrganismStoichiometry) %>% 
  group_by(siteID,TaxonomicOrder.OrganismStoichiometry,TaxonomicFamily.OrganismStoichiometry,TaxonomicGenus.OrganismStoichiometry) %>% 
  summarise(p_content_org=mean(PhosphorusMean.OrganismStoichiometry,na.rm=TRUE),
            n_content_org=mean(NitrogenMean.OrganismStoichiometry,na.rm=TRUE),
            c_content_org=mean(CarbonMean.OrganismStoichiometry,na.rm=TRUE),
            CN_molar_org=mean(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            CP_molar_org=mean(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            NP_molr_org=mean(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE))


stoich_global <- stoich_raw %>% 
  select(TaxonomicOrder.OrganismStoichiometry,TaxonomicFamily.OrganismStoichiometry,
         TaxonomicGenus.OrganismStoichiometry,PhosphorusMean.OrganismStoichiometry,NitrogenMean.OrganismStoichiometry,
         CarbonMean.OrganismStoichiometry,CarbonToNitrogenRatio.OrganismStoichiometry,CarbonToPhosphorusRatio.OrganismStoichiometry,
         NitrogenToPhosphorusRatio.OrganismStoichiometry) %>% 
  group_by(TaxonomicOrder.OrganismStoichiometry,TaxonomicFamily.OrganismStoichiometry,TaxonomicGenus.OrganismStoichiometry) %>% 
  summarise(p_content_org=mean(PhosphorusMean.OrganismStoichiometry,na.rm=TRUE),
            n_content_org=mean(NitrogenMean.OrganismStoichiometry,na.rm=TRUE),
            c_content_org=mean(CarbonMean.OrganismStoichiometry,na.rm=TRUE),
            CN_molar_org=mean(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            CP_molar_org=mean(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            NP_molr_org=mean(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE))

stoich_site_fam <- stoich_raw %>% 
  select(siteID,TaxonomicOrder.OrganismStoichiometry,TaxonomicFamily.OrganismStoichiometry,
         ,PhosphorusMean.OrganismStoichiometry,NitrogenMean.OrganismStoichiometry,
         CarbonMean.OrganismStoichiometry,CarbonToNitrogenRatio.OrganismStoichiometry,CarbonToPhosphorusRatio.OrganismStoichiometry,
         NitrogenToPhosphorusRatio.OrganismStoichiometry) %>% 
  group_by(siteID,TaxonomicOrder.OrganismStoichiometry,TaxonomicFamily.OrganismStoichiometry) %>% 
  summarise(p_content_org=mean(PhosphorusMean.OrganismStoichiometry,na.rm=TRUE),
            n_content_org=mean(NitrogenMean.OrganismStoichiometry,na.rm=TRUE),
            c_content_org=mean(CarbonMean.OrganismStoichiometry,na.rm=TRUE),
            CN_molar_org=mean(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            CP_molar_org=mean(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            NP_molr_org=mean(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE))


stoich_global_fam <- stoich_raw %>% 
  select(TaxonomicOrder.OrganismStoichiometry,TaxonomicFamily.OrganismStoichiometry,
         PhosphorusMean.OrganismStoichiometry,NitrogenMean.OrganismStoichiometry,
         CarbonMean.OrganismStoichiometry,CarbonToNitrogenRatio.OrganismStoichiometry,CarbonToPhosphorusRatio.OrganismStoichiometry,
         NitrogenToPhosphorusRatio.OrganismStoichiometry) %>% 
  group_by(TaxonomicOrder.OrganismStoichiometry,TaxonomicFamily.OrganismStoichiometry) %>% 
  summarise(p_content_org=mean(PhosphorusMean.OrganismStoichiometry,na.rm=TRUE),
            n_content_org=mean(NitrogenMean.OrganismStoichiometry,na.rm=TRUE),
            c_content_org=mean(CarbonMean.OrganismStoichiometry,na.rm=TRUE),
            CN_molar_org=mean(CarbonToNitrogenRatio.OrganismStoichiometry,na.rm=TRUE),
            CP_molar_org=mean(CarbonToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE),
            NP_molr_org=mean(NitrogenToPhosphorusRatio.OrganismStoichiometry,na.rm=TRUE))

###############

stoich_site[is.na(stoich_site$TaxonomicGenus.OrganismStoichiometry),"TaxonomicGenus.OrganismStoichiometry"]<-""
stoich_site[is.na(stoich_site$TaxonomicFamily.OrganismStoichiometry),"TaxonomicFamily.OrganismStoichiometry"]<-""

stoich_global[is.na(stoich_global$TaxonomicGenus.OrganismStoichiometry),"TaxonomicGenus.OrganismStoichiometry"]<-""
stoich_global[is.na(stoich_global$TaxonomicFamily.OrganismStoichiometry),"TaxonomicFamily.OrganismStoichiometry"]<-""


stoich_site$ord_fam_gen<-tolower(paste(stoich_site$TaxonomicOrder.OrganismStoichiometry,stoich_site$TaxonomicFamily.OrganismStoichiometry,stoich_site$TaxonomicGenus.OrganismStoichiometry,sep="_"))
stoich_site$ord_fam<-tolower(paste(stoich_site$TaxonomicOrder.OrganismStoichiometry,stoich_site$TaxonomicFamily.OrganismStoichiometry,sep="_"))

stoich_global$ord_fam_gen<-tolower(paste(stoich_global$TaxonomicOrder.OrganismStoichiometry,stoich_global$TaxonomicFamily.OrganismStoichiometry,stoich_global$TaxonomicGenus.OrganismStoichiometry,sep="_"))
stoich_global$ord_fam<-tolower(paste(stoich_global$TaxonomicOrder.OrganismStoichiometry,stoich_global$TaxonomicFamily.OrganismStoichiometry,sep="_"))


stoich_site_fam[is.na(stoich_site_fam$TaxonomicFamily.OrganismStoichiometry),"TaxonomicFamily.OrganismStoichiometry"]<-""
stoich_global_fam[is.na(stoich_global_fam$TaxonomicFamily.OrganismStoichiometry),"TaxonomicFamily.OrganismStoichiometry"]<-""

stoich_global_fam$ord_fam<-tolower(paste(stoich_global_fam$TaxonomicOrder.OrganismStoichiometry,stoich_global_fam$TaxonomicFamily.OrganismStoichiometry,sep="_"))
stoich_site_fam$ord_fam<-tolower(paste(stoich_site_fam$TaxonomicOrder.OrganismStoichiometry,stoich_site_fam$TaxonomicFamily.OrganismStoichiometry,sep="_"))


#### biomass per meter squared of inverts

abund2<-merge(abund,benke,by="order") ### we loose a few non-insect taxa here.

abund2$biomass_mg<-abund2$a*abund2$sizeClass**abund2$b*abund2$estimatedTotalCount # benke regression times estimated abundance

abund_summary<- abund2 %>% group_by(siteID,family,order,genus,Date) %>% 
  dplyr::summarise(total_biomass=sum(biomass_mg))

abund_summary<- abund_summary %>% group_by(siteID,family,order,genus) %>% 
  dplyr::summarise(total_biomass_average=mean(total_biomass))

abund_summary$ord_fam_gen<-tolower(paste(abund_summary$order,abund_summary$family,abund_summary$genus,sep="_"))
abund_summary$ord_fam<-tolower(paste(abund_summary$order,abund_summary$family,sep="_"))


########################### selecting only the closest date inverts were collected

#stoich_site_dates<- stoich_raw %>% select(siteID,Date) %>% unique

#abund_summary<-merge(abund_summary,stoich_site_dates,by=c("siteID"))
#abund_summary$date.diff<-abs(abund_summary$Date.x-abund_summary$Date.y)

#abund_summary2<-data.frame()

#for (i in 1:length(unique(abund_summary$siteID))){
#  one_site<-abund_summary[abund_summary$siteID==unique(abund_summary$siteID)[i],]
#  one_date<-one_site[one_site$date.diff==min(one_site$date.diff,na.rm=TRUE),]
#  one_date<-one_date[one_date$Date.x==min(one_date$Date.x),]
  
#  abund_summary2<-rbind(abund_summary2,one_date)
  
#}

#########################


#test<-merge(stoich,abund_summary,by=c"site_date")

################ Tryign to do the merging now
################ merge by order_family_genus
################ merge by order_family
################ merge by order
################ bind all rows and remove duplicates


############# Scenerio 1 matches local first
abund_summary$unique_combo<-paste(abund_summary$siteID,abund_summary$ord_fam_gen)

merge_1<-merge(abund_summary,stoich_site,by=c("siteID","ord_fam_gen"))
merge_1$match_level<-"local_genus"

######### What did we not mannage to keep in this merge

not_merged<-abund_summary[abund_summary$unique_combo %in% merge_1$unique_combo==FALSE,]

merge_2<-merge(not_merged,stoich_global,by=c("ord_fam_gen"))
merge_2$match_level<-"global_genus"

#########

not_merged_2<-not_merged[not_merged$unique_combo %in% merge_2$unique_combo==FALSE,]

merge_3<-merge(not_merged_2,stoich_site_fam,by=c("siteID","ord_fam"))
merge_3$match_level<-"local_family"


##

not_merged_3<-not_merged_2[not_merged_2$unique_combo %in% merge_3$unique_combo,]

##

merge_4<-merge(not_merged_3,stoich_global_fam,by=c("ord_fam"))
merge_4$match_level<-"global_family"


all_stoich<-bind_rows(merge_1,merge_2,merge_3,merge_4)


############# Scenerio 2 only matches global

merge_1_global<-merge(abund_summary,stoich_global,by=c("ord_fam_gen"))
merge_1_global$match_level<-"global_genus"

not_merged<-abund_summary[abund_summary$unique_combo %in% merge_1_global$unique_combo==FALSE,]

merge_2_global<-merge(not_merged,stoich_global_fam,by=c("ord_fam"))
merge_2_global$match_level<-"global_genus"

all_stoich_global<-bind_rows(merge_1_global,merge_2_global)

#test<-left_join(stoich, abund, by = "siteID") %>%
#  mutate(date_diff = abs(Date.x - Date.y)) %>%
#  group_by(siteID) %>%
#  filter(date_diff == min(date_diff)) %>%
#  select(siteID, Date.x,date_diff)

############## Summarizing across sites by taxa

#taxa_stoich_summary <- stoich_raw %>% 
#  group_by(Order_Organism,Family_Organism,Genus_Organism) %>% 
#  dplyr::summarize(np_org=mean(NP_molar_Organism,na.rm=TRUE),cn_org=mean(CN_molar_Organism,na.rm=TRUE),cp_org=mean(CP_molar_Organism,na.rm=TRUE))

#taxa_stoich_summary$ord_fam_gen<-tolower(paste(taxa_stoich_summary$Order_Organism,taxa_stoich_summary$Family_Organism,taxa_stoich_summary$Genus_Organism,sep="_"))

###############


#community_merge<-merge(abund_summary2,taxa_stoich_summary,by="ord_fam_gen",all.x=TRUE)

ffg<-read.csv("./data/vieira_trait_data.csv")

ffg<- ffg %>% select(Family,Genus,ffg) %>% unique()


ffg$fam_gen<-tolower(paste(ffg$Family,ffg$Genus,sep="_"))

all_stoich$fam_gen<-tolower(paste(all_stoich$family,all_stoich$genus,sep="_"))
community_merge_ffg<-merge(all_stoich,ffg,by="fam_gen",all.x=TRUE)

all_stoich_global$fam_gen<-tolower(paste(all_stoich_global$family,all_stoich$genus,sep="_"))
community_merge_ffg_global<-merge(all_stoich_global,ffg,by="fam_gen",all.x=TRUE)
## 450 not merged this way



community_merge_ffg$site_ffg<-paste(community_merge_ffg$siteID,community_merge_ffg$ffg,sep="_")
community_merge_ffg_global$site_ffg<-paste(community_merge_ffg_global$siteID,community_merge_ffg_global$ffg,sep="_")

site_ffg_summary<-data.frame()

for (i in 1:length(unique(community_merge_ffg$site_ffg))){
  one_group<-community_merge_ffg[community_merge_ffg$site_ffg==unique(community_merge_ffg$site_ffg)[i],]
  one_group_global<-community_merge_ffg_global[community_merge_ffg_global$site_ffg==unique(community_merge_ffg$site_ffg)[i],]
  
  
  has_p<-one_group[is.na(one_group$p_content_org)==FALSE,]
  np_site<-sum((has_p$total_biomass_average*(has_p$n_content_org/100)/14))/sum((has_p$total_biomass_average*(has_p$p_content_org/100)/30.974))
  
  cp_site<-sum((has_p$total_biomass_average*(has_p$c_content_org/100)/12))/sum((has_p$total_biomass_average*(has_p$p_content_org/100)/30.974))
  
  has_n<-one_group[is.na(one_group$n_content_org)==FALSE,]
  
  cn_site<-sum((has_p$total_biomass_average*(has_p$c_content_org/100)/12))/sum((has_p$total_biomass_average*(has_p$n_content_org/100)/14))
  
  ## global
  
  has_p<-one_group_global[is.na(one_group_global$p_content_org)==FALSE,]
  np_global<-sum((has_p$total_biomass_average*(has_p$n_content_org/100)/14))/sum((has_p$total_biomass_average*(has_p$p_content_org/100)/30.974))
  
  cp_global<-sum((has_p$total_biomass_average*(has_p$c_content_org/100)/12))/sum((has_p$total_biomass_average*(has_p$p_content_org/100)/30.974))
  
  has_n<-one_group_global[is.na(one_group_global$n_content_org)==FALSE,]
  
  cn_global<-sum((has_p$total_biomass_average*(has_p$c_content_org/100)/12))/sum((has_p$total_biomass_average*(has_p$n_content_org/100)/14))
  
  
  
  output<-data.frame(siteID=one_group$siteID[1],ffg=one_group$ffg[1],np_site,cp_site,cn_site,np_global,cp_global,cn_global)
  output$min_cn_org<-min(one_group$CN_molar_org,na.rm=TRUE)
  output$max_cn_org<-max(one_group$CN_molar_org,na.rm=TRUE)
  output$min_cp_org<-min(one_group$CP_molar_org,na.rm=TRUE)
  output$max_cp_org<-max(one_group$CP_molar_org,na.rm=TRUE)
  output$min_np_org<-min(one_group$NP_molr_org,na.rm=TRUE)
  output$max_np_org<-max(one_group$NP_molr_org,na.rm=TRUE)
                     

  
site_ffg_summary<-rbind(site_ffg_summary,output)
  
}

write.csv(site_ffg_summary,file="./data/community_stoich_summary.csv")


################
stoich_raw$fam_gen<-tolower(paste(stoich_raw$Family_Organism,stoich_raw$Genus_Organism,sep="_"))

taxa_ffg_merge<-merge(stoich_raw,ffg,by="fam_gen",all.x=TRUE)

write.csv(taxa_ffg_merge,file="taxa_ffgs_stoich.csv")



###############

library(Rmisc)

summarySE(community_merge_ffg,groupvars = c("siteID","FFG"),measurevar = "np_org",na.rm=TRUE)


###################