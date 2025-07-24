library(tidyverse)
library(ggthemes)
library(patchwork)
library(ggpubr)
library(scales)
library(scatterpie)
library(ggspatial)
library(sf)
library(usmap)
library(USA.state.boundaries)
#import data
scenarios <- read_csv("January 2025 analysis/generated data/scenarios.csv")

#Generate table of resource range and mean for each type####
resource_range<-scenarios%>%
  filter(scenario==1)%>%
  mutate(res_type=case_when(ffg=="HB"~"Peri",
                            ffg!="HB"~"Ses"))%>%
  group_by(res_type)%>%
  summarise(mean_cn=mean(res_CN), min_cn=min(res_CN), max_cn=max(res_CN),
            mean_cp=mean(res_CP), min_cp=min(res_CP), max_cp=max(res_CP),
            mean_np=mean(res_NP), min_np=min(res_NP), max_np=max(res_NP))
#periphyton, herbivore relationships####
##CN####
peri_hb_cn<-scenarios%>%
  filter(ffg=="HB")%>%
  ggplot(aes(res_CN, taxa_CN, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Periphyton C:N (molar)", y="Community C:N (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();peri_hb_cn

#### regressions-full list of model output generated at end for each graph
peri_cn_1<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="HB"&scenario==1)))
peri_cn_2<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="HB"&scenario==2)))
peri_cn_3<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="HB"&scenario==3)))
peri_cn_4<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="HB"&scenario==4)))

##CP####
peri_hb_cp<-scenarios%>%
  filter(ffg=="HB")%>%
  ggplot(aes(res_CP, taxa_CP, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Periphyton C:P (molar)", y="Community C:P (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();peri_hb_cp

#### regressions
peri_cp_1<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="HB"&scenario==1)))
peri_cp_2<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="HB"&scenario==2)))
peri_cp_3<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="HB"&scenario==3)))
peri_cp_4<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="HB"&scenario==4)))

##NP####
peri_hb_np<-scenarios%>%
  filter(ffg=="HB")%>%
  ggplot(aes(res_NP, taxa_NP, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Periphyton N:P (molar)", y="Community N:P (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();peri_hb_np

#### regressions
peri_np_1<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="HB"&scenario==1)))
peri_np_2<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="HB"&scenario==2)))
peri_np_3<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="HB"&scenario==3)))
peri_np_4<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="HB"&scenario==4)))

#plot HB together ####
peri_hb_cn+peri_hb_cp+peri_hb_np+plot_layout(ncol=1, guides="collect")+
  plot_annotation(title="Herbivore", tag_levels = 'a', tag_suffix = ')')& 
  theme(legend.position="bottom")&guides(color = guide_legend(nrow = 2))

ggplot2::ggsave("January 2025 analysis/graphs/HB.jpeg",dpi=800, width=6, height=12)

#seston, collector-filterer relationships####
##CN####
ses_cf_cn<-scenarios%>%
  filter(ffg=="CF")%>%
  ggplot(aes(res_CN, taxa_CN, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Seston C:N (molar)", y="Community C:N (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();ses_cf_cn
#### regressions
ses_cf_cn_1<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="CF"&scenario==1)))
ses_cf_cn_2<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="CF"&scenario==2)))
ses_cf_cn_3<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="CF"&scenario==3)))
ses_cf_cn_4<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="CF"&scenario==4)))

##CP####
ses_cf_cp<-scenarios%>%
  filter(ffg=="CF")%>%
  ggplot(aes(res_CP, taxa_CP, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Seston C:P (molar)", y="Community C:P (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();ses_cf_cp

#### regressions
ses_cf_cp_1<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="CF"&scenario==1)))
ses_cf_cp_2<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="CF"&scenario==2)))
ses_cf_cp_3<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="CF"&scenario==3)))
ses_cf_cp_4<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="CF"&scenario==4)))
##NP####
ses_cf_np<-scenarios%>%
  filter(ffg=="CF")%>%
  ggplot(aes(res_NP, taxa_NP, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Seston N:P (molar)", y="Community N:P (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();ses_cf_np

#### regressions
ses_cf_np_1<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="CF"&scenario==1)))
ses_cf_np_2<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="CF"&scenario==2)))
ses_cf_np_3<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="CF"&scenario==3)))
ses_cf_np_4<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="CF"&scenario==4)))

#seston, collector-gatherer relationships####
##CN####
ses_cg_cn<-scenarios%>%
  filter(ffg=="CG")%>%
  ggplot(aes(res_CN, taxa_CN, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Seston C:N (molar)", y="Community C:N (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();ses_cg_cn

#### regressions
ses_cg_cn_1<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="CG"&scenario==1)))
ses_cg_cn_2<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="CG"&scenario==2)))
ses_cg_cn_3<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="CG"&scenario==3)))
ses_cg_cn_4<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg=="CG"&scenario==4)))
##CP####
ses_cg_cp<-scenarios%>%
  filter(ffg=="CG")%>%
  ggplot(aes(res_CP, taxa_CP, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Seston C:P (molar)", y="Community C:P (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();ses_cg_cp

#### regressions
ses_cg_cp_1<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="CG"&scenario==1)))
ses_cg_cp_2<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="CG"&scenario==2)))
ses_cg_cp_3<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="CG"&scenario==3)))
ses_cg_cp_4<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg=="CG"&scenario==4)))
##NP####
ses_cg_np<-scenarios%>%
  filter(ffg=="CG")%>%
  ggplot(aes(res_NP, taxa_NP, color=factor(scenario)))+
  geom_point()+
  stat_smooth(method="lm")+
  scale_color_manual(values=c("black", "#0072B2", "#E69F00", "#009E73"), name="Scenario",
                     label=c("Observed Local", "Observed Global", "Theoretical Global", "Theoretical Local"))+
  scale_x_continuous(transform = "log", breaks = breaks_log(n=6))+
  scale_y_continuous(transform = "log", breaks=breaks_log(n=6))+
  labs(x="Seston N:P (molar)", y="Community N:P (molar)")+
  geom_abline(slope=1, linetype="dashed")+
  theme_few();ses_cg_np

#### regressions
ses_cg_np_1<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="CG"&scenario==1)))
ses_cg_np_2<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="CG"&scenario==2)))
ses_cg_np_3<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="CG"&scenario==3)))
ses_cg_np_4<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg=="CG"&scenario==4)))

#plot seston/cf and cg on single plot####
p_cf<-ses_cf_cn+ses_cf_cp+ses_cf_np+ plot_layout(ncol=1, guides = 'collect')+
  plot_annotation(title="Collector-Filterer",tag_levels = list(c("a","c","e")), tag_suffix = ')')&
  theme(legend.position = "none")
p_cg<-ses_cg_cn+ses_cg_cp+ses_cg_np+ plot_layout(ncol=1, guides = 'collect')+
  plot_annotation(title="Collector-Gatherer",tag_levels = list(c("b","d","f")), tag_suffix = ')')&
  theme(legend.position = "none")
myLegend <- get_legend(ses_cf_cn +
                         theme(legend.position="bottom")+
                         guides(color = guide_legend(nrow = 2)))

(wrap_elements(p_cf)+wrap_elements(p_cg))/wrap_elements(myLegend)+
  plot_layout( heights = c(1,.1))

ggplot2::ggsave("January 2025 analysis/graphs/CF_CG.jpeg",dpi=800, width=12, height=12)

#Extract slopes and p-values for all scenarios and ratios####
homeostasis_test<-function(x){
  x_cn_1<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg==x &scenario==1)))
  x_cn_2<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg==x &scenario==2)))
  x_cn_3<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg==x &scenario==3)))
  x_cn_4<-summary(lm(log(taxa_CN)~log(res_CN),data=scenarios%>%filter(ffg==x &scenario==4)))
  
  x_cp_1<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg==x &scenario==1)))
  x_cp_2<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg==x &scenario==2)))
  x_cp_3<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg==x &scenario==3)))
  x_cp_4<-summary(lm(log(taxa_CP)~log(res_CP),data=scenarios%>%filter(ffg==x &scenario==4)))
  
  x_np_1<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg==x &scenario==1)))
  x_np_2<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg==x &scenario==2)))
  x_np_3<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg==x &scenario==3)))
  x_np_4<-summary(lm(log(taxa_NP)~log(res_NP),data=scenarios%>%filter(ffg==x &scenario==4)))
  
  output<-data.frame(scenario=c("1","2","3","4"),ffg=x,
                     cn_H=c(x_cn_1$coefficients[2,1],x_cn_2$coefficients[2,1],x_cn_3$coefficients[2,1],x_cn_4$coefficients[2,1]),
                     cn_se=c(x_cn_1$coefficients[2,2],x_cn_2$coefficients[2,2],x_cn_3$coefficients[2,2],x_cn_4$coefficients[2,2]),
                     cn_p=c(x_cn_1$coefficients[2,4],x_cn_2$coefficients[2,4],x_cn_3$coefficients[2,4],x_cn_4$coefficients[2,4]),
                     cn_r2=c(x_cn_1$r.squared,x_cn_2$r.squared, x_cn_3$r.squared, x_cn_4$r.squared),
                     cp_H=c(x_cp_1$coefficients[2,1],x_cp_2$coefficients[2,1],x_cp_3$coefficients[2,1],x_cp_4$coefficients[2,1]),
                     cp_se=c(x_cp_1$coefficients[2,2],x_cp_2$coefficients[2,2],x_cp_3$coefficients[2,2],x_cp_4$coefficients[2,2]),
                     cp_p=c(x_cp_1$coefficients[2,4],x_cp_2$coefficients[2,4],x_cp_3$coefficients[2,4],x_cp_4$coefficients[2,4]),
                     cp_r2=c(x_cp_1$r.squared,x_cp_2$r.squared, x_cp_3$r.squared, x_cp_4$r.squared),
                     np_H=c(x_np_1$coefficients[2,1],x_np_2$coefficients[2,1],x_np_3$coefficients[2,1],x_np_4$coefficients[2,1]),
                     np_se=c(x_np_1$coefficients[2,2],x_np_2$coefficients[2,2],x_np_3$coefficients[2,2],x_np_4$coefficients[2,2]),
                     np_p=c(x_np_1$coefficients[2,4],x_np_2$coefficients[2,4],x_np_3$coefficients[2,4],x_np_4$coefficients[2,4]),
                     np_r2=c(x_np_1$r.squared,x_np_2$r.squared, x_np_3$r.squared, x_np_4$r.squared))
  
  return(output)
}

HB_homeostasis<-homeostasis_test("HB")
CF_homeostasis<-homeostasis_test("CF")
CG_homeostasis<-homeostasis_test("CG")

all_scenario_values<-bind_rows(HB_homeostasis,CF_homeostasis,CG_homeostasis)
all_scenario_values<-all_scenario_values%>%
  pivot_longer(cols=cn_H:np_r2, names_to = "ratio", values_to = "value")%>%
  separate(ratio, c("ratio","test"))%>%
  pivot_wider(names_from="test", values_from = "value")



write.csv(all_scenario_values,file="January 2025 analysis/model output/homeostasis_tests.csv",na="", row.names = F)

#Hypothesis tests among scenarios####
##HB####
hypothesis_tests<-function(x){
  
  hypothesis1cn<-summary(lm(log(taxa_CN)~log(res_CN)*scenario,data=scenarios%>%filter(scenario %in% c(1,2) & ffg==x)))
  hypothesis1cp<-summary(lm(log(taxa_CP)~log(res_CP)*scenario,data=scenarios%>%filter(scenario %in% c(1,2)& ffg==x)))
  hypothesis1np<-summary(lm(log(taxa_NP)~log(res_NP)*scenario,data=scenarios%>%filter(scenario %in% c(1,2)& ffg==x)))
  
  hypothesis2cn<-summary(lm(log(taxa_CN)~log(res_CN)*scenario,data=scenarios%>%filter(scenario %in% c(1,3)& ffg==x)))
  hypothesis2cp<-summary(lm(log(taxa_CP)~log(res_CP)*scenario,data=scenarios%>%filter(scenario %in% c(1,3)& ffg==x)))
  hypothesis2np<-summary(lm(log(taxa_NP)~log(res_NP)*scenario,data=scenarios%>%filter(scenario %in% c(1,3)& ffg==x)))
  
  hypothesis3cn<-summary(lm(log(taxa_CN)~log(res_CN)*scenario,data=scenarios%>%filter(scenario %in% c(1,4)& ffg==x)))
  hypothesis3cp<-summary(lm(log(taxa_CP)~log(res_CP)*scenario,data=scenarios%>%filter(scenario %in% c(1,4)& ffg==x)))
  hypothesis3np<-summary(lm(log(taxa_NP)~log(res_NP)*scenario,data=scenarios%>%filter(scenario %in% c(1,4)& ffg==x)))
  
  hypothesis4cn<-summary(lm(log(taxa_CN)~log(res_CN)*scenario,data=scenarios%>%filter(scenario %in% c(3,4)& ffg==x)))
  hypothesis4cp<-summary(lm(log(taxa_CP)~log(res_CP)*scenario,data=scenarios%>%filter(scenario %in% c(3,4)& ffg==x)))
  hypothesis4np<-summary(lm(log(taxa_NP)~log(res_NP)*scenario,data=scenarios%>%filter(scenario %in% c(3,4)& ffg==x)))
  
  output<-data.frame(nutrient=c("CN","CP","NP"),ffg=x,
                     hypothesis1_p=c(hypothesis1cn$coefficients[4,4],hypothesis1cp$coefficients[4,4],hypothesis1np$coefficients[4,4]),
                     hypothesis2_p=c(hypothesis2cn$coefficients[4,4],hypothesis2cp$coefficients[4,4],hypothesis2np$coefficients[4,4]),
                     hypothesis3_p=c(hypothesis3cn$coefficients[4,4],hypothesis3cp$coefficients[4,4],hypothesis3np$coefficients[4,4]),
                     hypothesis4_p=c(hypothesis4cn$coefficients[4,4],hypothesis4cp$coefficients[4,4],hypothesis4np$coefficients[4,4]))
  
  return(output)
}

HB_hypothesis<-hypothesis_tests( "HB")
CG_hypothesis<-hypothesis_tests( "CG")
CF_hypothesis<-hypothesis_tests( "CF")

all_p_values<-bind_rows(HB_hypothesis,CG_hypothesis,CF_hypothesis)

write.csv(all_p_values,file="January 2025 analysis/model output/hypothesis_tests.csv",na="", row.names = F)

#Make map of all available data####
#pull in site map table for lat long of sites and filter by periphyton list since contains all available sites
#load in NEON site name to code sheet to convert names to 4 letter code
NEON_SiteMap_Table <- read_csv("January 2025 analysis/NEON-SiteMap-Table.csv")
#add siteID column to match NEON invert data (need to make siteNAME in table all lowercase to match STOICH)
NEON_SiteMap_Table$siteName<-tolower(NEON_SiteMap_Table$siteName)
#pull table of data presence (made is 5-supplements******)
data_table <- read_csv("January 2025 analysis/generated data/site_table.csv")
#filter NEON table by data table sites
NEON_SiteMap_data<-NEON_SiteMap_Table%>%
  right_join(data_table, join_by(siteCode==siteID))
#make columns for pie chart of what data is available (periphyton, seston, NEON biomass, stoich HB, CF,CG)
#need to select columns 1:3, 6:8 and convert any X to 30
NEON_SiteMap_data<-NEON_SiteMap_data%>%
  select(siteCode:CF_biomass, CG_stoich:CF_stoich)%>%
  mutate_at(vars(peri_stoich:CF_stoich),~case_when(.=="x"~16.6667,
                                               is.na(.)==T~0))%>%
  rename("Biomass"="CF_biomass")
  
#subset without PR
NEON_SiteMap_data_noPR<-NEON_SiteMap_data%>%
  filter(!stateCode=="PR")
#Convert lat long to match usmap projection and export as dataframe to match up with data
NEON_SiteMap_data_tr<-usmap_transform(NEON_SiteMap_data_noPR %>%rename(lat=latitude, lon=longitude))
NEON_SiteMap_data_tr_df = data.frame(st_coordinates(NEON_SiteMap_data_tr[,1]))
#left join transformed coordinates to point and pie data frames
NEON_SiteMap_data_noPR<-NEON_SiteMap_data_noPR%>%
  bind_cols(NEON_SiteMap_data_tr_df)

##plot with all data at all sites except PR and remove later####
NEON_SiteMap_data_noPR_all<-NEON_SiteMap_data_noPR%>%
  mutate_at(vars(peri_stoich:CF_stoich),~16.6667)

plot_usmap(color="grey60")+
  geom_point(data=NEON_SiteMap_data_noPR_all,aes(x=X, y=Y), size=1)+
  geom_point(data=NEON_SiteMap_data_noPR_all,aes(x=X, y=Y), shape=21,size=11.75)+
  geom_scatterpie(data=NEON_SiteMap_data_noPR_all, aes(x=X, y=Y, r=100000), 
                  cols=colnames(NEON_SiteMap_data_noPR_all[,c(9:14)]), linewidth=.2)+
  scale_fill_manual(values=c("#36753B","#DACD82", "#302383","#97CAEA","#7D2A54","#999945"),name="Data",
                    label=c("Periphyton Stoichiometry","Seston Stoichiometry","Biomass","CF Stoichiometry",
                            "CG Stoichiometry", "HB Stoichiometry"))+
  annotation_north_arrow(location="br",pad_y=unit(0.5, "in"))+
  annotation_scale(location="br")+
  theme_map(base_size=12)+
  theme(legend.position = "right")

ggplot2::ggsave("January 2025 analysis/graphs/map_all.pdf",dpi=800, width=12, height=8)
#then go into pdf editor and delete missing pie segments based on table
##plot just PR sites for inset####
PR<-state_boundaries_wgs84%>%
  filter(STATE_ABBR=="PR")

NEON_SiteMap_data_PR<-NEON_SiteMap_data%>%
  filter(stateCode=="PR")%>%
  mutate_at(vars(peri_stoich:CF_stoich),~16.6667)

ggplot(data=PR)+
  geom_sf(fill=NA,color="grey60", linewidth=0.6)+
  geom_point(data=NEON_SiteMap_data_PR,aes(x=longitude, y=latitude), size=1)+
  geom_point(data=NEON_SiteMap_data_PR,aes(x=longitude, y=latitude), shape=21,size=30.5)+
  geom_scatterpie(data=NEON_SiteMap_data_PR, aes(x=longitude, y=latitude), 
                  cols=colnames(NEON_SiteMap_data_PR[,c(9:14)]), linewidth=.2, pie_scale = 30.5)+
  scale_fill_manual(values=c("#36753B","#DACD82", "#302383","#97CAEA","#7D2A54","#999945"),name="Data",
                    label=c("Periphyton Stoichiometry","Seston Stoichiometry","Biomass","CF Stoichiometry",
                            "CG Stoichiometry", "HB Stoichiometry"))+
  annotation_scale(location="br")+
  theme_map(base_size=12)+
  theme(legend.position = "none")

ggplot2::ggsave("January 2025 analysis/graphs/map_PR.pdf",dpi=800, width=12, height=8)
#then go into pdf editor and delete missing pie segments based on table, combine with previous map
