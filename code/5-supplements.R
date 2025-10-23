library(tidyverse)
library(readxl)
library(ggthemes)
library(patchwork)
library(ggpubr)
#import datasets####
NEON_taxa_processed <- read_csv("January 2025 analysis/generated data/NEON_taxa_processed.csv")
NEON_count_sum <- read_csv("January 2025 analysis/generated data/NEON_count_sum.csv")
Invert_stoich_genus_site<- read_csv("January 2025 analysis/generated data/Invert_stoich_genus_site.csv")
Invert_stoich_family_site <- read_csv("January 2025 analysis/generated data/Invert_stoich_family_site.csv")
site_level_average_seston <- read_csv("January 2025 analysis/generated data/site_level_average_seston.csv")
site_level_average_periphyton <- read_csv("January 2025 analysis/generated data/site_level_average_periphyton.csv")
scenarios <- read_csv("January 2025 analysis/generated data/scenarios.csv")
#ffg 
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
#Make list of all sites with invertebrate count data####
NEON_invert_sites<-NEON_count_sum%>%
  left_join(drop_na(taxa_FFG_genus[,c(3,4)]))%>%
  left_join(drop_na(taxa_FFG_family[,c(2,3)]),join_by("family"=="family"))%>%
  mutate(ffg=coalesce(FFG.x, FFG.y))%>%
  select(!c(FFG.x,FFG.y))%>%
  distinct()%>%
  filter(ffg %in%c("HB","CF","CG"))%>%
  group_by(siteID, ffg)%>%
  summarise()%>%
  mutate(NEON_biomass="x")%>%
  pivot_wider(names_from = "ffg", values_from = "NEON_biomass")

Peri_stoich<-data.frame(siteID=unique(site_level_average_periphyton$siteID), peri_stoich="x") #(25)
Ses_stoich<-data.frame(siteID=unique(site_level_average_seston$siteID), ses_stoich="x") #(21)

#Add ffg to invert_stoich_genus and family, filter to ffg of interset and summarise
Inv_stoich<-Invert_stoich_genus_site%>%
  full_join(Invert_stoich_family_site)%>%
  left_join(drop_na(taxa_FFG_genus[,c(3,4)]))%>%
  left_join(drop_na(taxa_FFG_family[,c(2,3)]),join_by("family"=="family"))%>%
  mutate(ffg=coalesce(FFG.x, FFG.y))%>%
  select(!c(FFG.x,FFG.y))%>%
  distinct()%>%
  filter(ffg %in%c("HB","CF","CG"))%>%
  group_by(siteID, ffg)%>%
  summarise()%>%
  mutate(Invert_stoich="x")%>%
  pivot_wider(names_from = "ffg",values_from = "Invert_stoich", values_fill = "")

#make list of sites in each scenario####
scenario_sites<-scenarios%>%
  group_by(scenario, siteID, ffg)%>%
  summarise()%>%
  mutate(present="x")%>%
  group_by(scenario)%>%
  pivot_wider(names_from = "ffg",values_from="present")


#Join all together####
Site_data_list<-Peri_stoich%>%
  full_join(Ses_stoich)%>%
  full_join(NEON_invert_sites)%>%
  full_join(Inv_stoich, join_by(siteID), suffix = c("_biomass","_stoich"))%>%
  full_join(scenario_sites %>% filter(scenario==1), join_by(siteID))%>%
  full_join(scenario_sites %>% filter(scenario==2), join_by(siteID))%>%
  full_join(scenario_sites %>% filter(scenario==3), join_by(siteID))%>%
  full_join(scenario_sites %>% filter(scenario==4), join_by(siteID))%>%
  filter(is.na(peri_stoich)==F | is.na(ses_stoich)==F)

#write to csv
write.csv(Site_data_list,  'January 2025 analysis/generated data/site_table.csv', na="", row.names = F)

#Make supplemental table to show number of taxa in original NEON data within HB, CF and CG, ####
#after dropping higher level IDs and after finding available stoich data
#add lowest id
NEON_taxa_processed<-NEON_taxa_processed%>%
  mutate(sampleID=str_remove(sampleID,"SS."))%>%
  separate(sampleID, c("Site","Date","Sampler","Sample_num","note"))

NEON_taxa_processed<-NEON_taxa_processed%>%
  filter(Sampler %in% c("SURBER","HESS","PONAR"))%>%
  mutate(lowest_id= case_when(taxonRank=="phylum"~ phylum,
                              taxonRank=="class"~class,
                              taxonRank=="subclass"~subclass,
                              taxonRank=="order"~order,
                              taxonRank=="suborder"~suborder,
                              taxonRank=="superfamily"~superfamily,
                              taxonRank=="family"~family,
                              taxonRank=="subfamily"~subfamily,
                              taxonRank=="tribe"~tribe,
                              taxonRank%in% c("genus","subgenus","species","speciesGroup","subspecies")~genus))
#Get dataset with at least a phylum level id
NEON_initial_data<-NEON_taxa_processed%>%
  drop_na(lowest_id)
#Get dataset with orders included in length mass regressions
benke<-read.csv("January 2025 analysis/benke_length_mass.csv")

NEON_initial_biomass<-NEON_initial_data%>%
  left_join(benke)%>%
  drop_na(b)
#Get dataset with everything with at least a family level id
NEON_initial_family_level<-NEON_initial_biomass%>%
  mutate(keep=case_when(is.na(genus)==F~ genus,
                        is.na(genus)==T~family))%>%
  filter(is.na(keep)==F)

#Make table with count of each dataset
NEON_taxa_initial<-NEON_initial_data%>%
  mutate(class_alt=case_when(class=="Insecta"~"Insecta",
                             class!="Insecta"~"Non-insect",
                             is.na(class)==T~"Non-insect"))%>%
  group_by(siteID,class_alt)%>%
  summarise(Initial_count=length(unique(lowest_id)))

NEON_taxa_biomass<-NEON_initial_biomass%>%
  mutate(class_alt=case_when(class=="Insecta"~"Insecta",
                             class!="Insecta"~"Non-insect",
                             is.na(class)==T~"Non-insect"))%>%
  group_by(siteID,class_alt)%>%
  summarise(Biomass_count=length(unique(lowest_id)))

NEON_taxa_family<-NEON_initial_family_level%>%
  mutate(class_alt=case_when(class=="Insecta"~"Insecta",
                             class!="Insecta"~"Non-insect",
                             is.na(class)==T~"Non-insect"))%>%
  group_by(siteID,class_alt)%>%
  summarise(Family_count=length(unique(lowest_id)))

NEON_taxa_all<-NEON_taxa_initial%>%
  left_join(NEON_taxa_biomass)%>%
  left_join(NEON_taxa_family)%>%
  mutate_at(c('Biomass_count','Family_count'), ~replace_na(.,0))

NEON_taxa_all<-NEON_taxa_all%>%
  mutate(biomass_change=Initial_count-Biomass_count,
         family_change=Biomass_count-Family_count)
#write to csv
write.csv(NEON_taxa_all,  'January 2025 analysis/generated data/Taxa_change_list.csv', na="", row.names = F)
#what is lost in the biomass merge
NEON_no_biomass<-NEON_taxa_processed%>%
  left_join(benke)%>%
  filter(is.na(b)==T)

NEON_no_biomass%>%
  group_by(phylum,subphylum,class,order)%>%
  summarise(count=length(unique(lowest_id)))%>%
  print(n=50)
##everything dropped is non-insect worms, mites,isopod, shrimp, collembola, snails/clams, 
#only insect are lepidoptera and neuroptera
NEON_no_biomass%>%
  filter(class=="Insecta")%>%
  reframe(siteID=unique(siteID))%>%
  arrange(siteID)%>%
  print(n=25)

##Explore resource stoich and diversity overall and within each matched FFG####
NEON_FFG_rich<-NEON_count_sum%>%
  left_join(drop_na(taxa_FFG_genus[,c(3,4)]))%>%
  left_join(drop_na(taxa_FFG_family[,c(2,3)]),join_by("family"=="family"))%>%
  mutate(ffg=coalesce(FFG.x, FFG.y))%>%
  select(!c(FFG.x,FFG.y))%>%
  distinct()%>%
  group_by(siteID, ffg)%>%
  summarise(rich=length(unique(lowest_id)))%>%
  filter(ffg%in%c("HB","CF","CG"))%>%
  pivot_wider(names_from = ffg, values_from = rich)

NEON_count_rich<-NEON_count_sum%>%
  group_by(siteID)%>%
  summarise(rich=length(unique(lowest_id)))%>%
  left_join(NEON_FFG_rich)%>%
  left_join(site_level_average_periphyton)%>%
  left_join(site_level_average_seston,keep=T, suffix=c(".per",".ses"), by="siteID")

#convert to long with just means of resources
NEON_count_rich_long<-NEON_count_rich%>%
  pivot_longer(cols = rich:HB, names_to="ffg", values_to = "richness")%>%
  select(siteID.per, mean_CN.per, mean_CP.per, mean_NP.per, mean_CN.ses, mean_CP.ses, mean_NP.ses, ffg, richness)

##periphyton graphs####
###CN####
CN_peri<-ggplot(NEON_count_rich)+
  geom_point(aes(mean_CN.per, rich), color="#661102")+
  geom_point(aes(mean_CN.per, HB), color="#999834")+
  scale_x_log10()+
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50))+
  labs(x='Periphyton C:N (molar)', y="Richness")+
  theme_few();CN_peri

###CP####
CP_peri<-ggplot(NEON_count_rich)+
  geom_point(aes(mean_CP.per, rich), color="#661102")+
  geom_point(aes(mean_CP.per, HB), color="#999834")+
  scale_x_log10()+
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50))+
  labs(x='Periphyton C:P (molar)', y="Richness")+
  theme_few();CP_peri

###NP####
NP_peri<-ggplot(NEON_count_rich)+
  geom_point(aes(mean_NP.per, rich), color="#661102")+
  geom_point(aes(mean_NP.per, HB), color="#999834")+
  scale_x_log10()+
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50))+
  labs(x='Periphyton N:P (molar)', y="Richness")+
  theme_few();NP_peri

##Seston graphs####
###CN####
CN_ses<-ggplot(NEON_count_rich)+
  geom_point(aes(mean_CN.ses, rich), color="#661102")+
  geom_smooth(aes(mean_CN.ses, rich), method="lm",color="#661102",linetype="solid",se=T)+
  geom_point(aes(mean_CN.ses, CF), color="#44AA99")+
  geom_point(aes(mean_CN.ses, CG), color="#DECC77")+
  geom_smooth(aes(mean_CN.ses, CG), method="lm",color="#DECC77",linetype="solid",se=T)+
  scale_x_log10()+
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50))+
  labs(x='Seston C:N (molar)', y="Richness")+
  theme_few();CN_ses

###CP####
CP_ses<-ggplot(NEON_count_rich)+
  geom_point(aes(mean_CP.ses, rich), color="#661102")+
  geom_point(aes(mean_CP.ses, CF), color="#44AA99")+
  geom_point(aes(mean_CP.ses, CG), color="#DECC77")+
  scale_x_log10()+
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50))+
  labs(x='Seston C:P (molar)', y="Richness")+
  theme_few();CP_ses

###NP####
NP_ses<-ggplot(NEON_count_rich)+
  geom_point(aes(mean_NP.ses, rich), color="#661102")+
  geom_point(aes(mean_NP.ses, CF), color="#44AA99")+
  geom_point(aes(mean_NP.ses, CG), color="#DECC77")+
  scale_x_log10()+
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50))+
  labs(x='Seston N:P (molar)', y="Richness")+
  theme_few();NP_ses

#create plot to steal legend from
legend<-ggplot(NEON_count_rich_long, aes(mean_NP.per, richness, color=ffg))+
  geom_point()+
  scale_color_manual(values=c("#44AA99","#DECC77","#999834","#661102"),
                     name="", labels=c("Collector-Filterer","Colector-Gatherer","Herbivore","All"))+
  theme_few(base_size = 14)+
  theme(legend.position = 'bottom');legend
legend<-get_legend(legend, position = "bottom")
#graph all together
plot<-CN_peri+CN_ses+CP_peri+CP_ses+NP_peri+NP_ses+plot_layout(ncol=2, axes='collect')+plot_annotation(tag_levels = 'a', tag_suffix = ")")
plot+legend+theme(plot.tag = element_blank())+plot_layout(heights = c(2,2,2,1))+plot_layout(design=c("AB
                                                                                                     CD
                                                                                                     EF
                                                                                                     GG"))
ggplot2::ggsave("January 2025 analysis/graphs/diversity_plots.jpeg",dpi=800, width=9, height=10)


#Extract slopes and p-values for diversity stoich relationships####
diversity_test<-function(x){
  x_cn_per<-summary(lm(richness~log(mean_CN.per),data=NEON_count_rich_long%>%filter(ffg==x)))
  x_cn_ses<-summary(lm(richness~log(mean_CN.ses),data=NEON_count_rich_long%>%filter(ffg==x)))
  
  x_cp_per<-summary(lm(richness~log(mean_CP.per),data=NEON_count_rich_long%>%filter(ffg==x)))
  x_cp_ses<-summary(lm(richness~log(mean_CP.ses),data=NEON_count_rich_long%>%filter(ffg==x)))
  
  x_np_per<-summary(lm(richness~log(mean_NP.per),data=NEON_count_rich_long%>%filter(ffg==x)))
  x_np_ses<-summary(lm(richness~log(mean_NP.ses),data=NEON_count_rich_long%>%filter(ffg==x)))
  
  output<-data.frame(res=c("peri","ses"),ffg=x,
                     cn_H=c(x_cn_per$coefficients[2,1],x_cn_ses$coefficients[2,1]),
                     cn_se=c(x_cn_per$coefficients[2,2],x_cn_ses$coefficients[2,2]),
                     cn_p=c(x_cn_per$coefficients[2,4],x_cn_ses$coefficients[2,4]),
                     cn_r2=c(x_cn_per$r.squared,x_cn_ses$r.squared),
                     cp_H=c(x_cp_per$coefficients[2,1],x_cp_ses$coefficients[2,1]),
                     cp_se=c(x_cp_per$coefficients[2,2],x_cp_ses$coefficients[2,2]),
                     cp_p=c(x_cp_per$coefficients[2,4],x_cp_ses$coefficients[2,4]),
                     cp_r2=c(x_cp_per$r.squared,x_cp_ses$r.squared),
                     np_H=c(x_np_per$coefficients[2,1],x_np_ses$coefficients[2,1]),
                     np_se=c(x_np_per$coefficients[2,2],x_np_ses$coefficients[2,2]),
                     np_p=c(x_np_per$coefficients[2,4],x_np_ses$coefficients[2,4]),
                     np_r2=c(x_np_per$r.squared,x_np_ses$r.squared))
  
  return(output)
}

all_diversity<-diversity_test("rich")
HB_diversity<-diversity_test("HB")
CF_diversity<-diversity_test("CF")
CG_diversity<-diversity_test("CG")


all_diversity_values<-bind_rows(all_diversity,HB_diversity,CF_diversity,CG_diversity)
all_diversity_values<-all_diversity_values%>%
  pivot_longer(cols=cn_H:np_r2, names_to = "ratio", values_to = "value")%>%
  separate(ratio, c("ratio","test"))%>%
  pivot_wider(names_from="test", values_from = "value")%>%
  mutate(sign=case_when(p<=0.05~"solid",
                        p>0.05~"dotted"))
