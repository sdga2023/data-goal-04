# SDG 4 Production
# Written by Brian Stacy
# Aug 23, 2022
# This file generates all the data for the charts in SDG 4 of the 2022 SDG Atlas

# packages
library(tidyverse)
library(haven)
library(here)
library(wbstats)
library(ggbeeswarm)
library(ggthemes)
library(patchwork)
library(readxl)
library(ggrepel)

set.seed(543132)

#set working directory
dir <- here()

#######
#Chart 1 on learning poverty, GNI per capita
#######
#read in the stata data file
covid_closures_df <- read_dta(file=paste0(dir, "/Data/jsw3_uis_oecd_clean.dta")) %>%
  mutate(across(starts_with('aq6_'),~if_else(.==997, as.numeric(NA),as.numeric(.))))


#read in UNESCO data on school closures
unesco_closures_df <- read_excel(path=paste0(dir, "/Data/School-Closures-Database-Final-1.xlsx"),
                                 sheet = 'Country data',
                                 skip=13) %>%
  transmute(
    iso3c=ISO3,
    unicef_full_closed=`Days: Fully closed`,
    unicef_partial_closed=`Days:  Partially closed`,
    unicef_academic_break=`Days: Academic break`,
    unicef_instruction_days=`Instruction Days` ,
    unicef_total_closed=unicef_full_closed+unicef_partial_closed
    
  ) %>%
  filter(!is.na(unicef_full_closed))

unesco_closures_raw <- read_excel(paste0(dir, "/Data/UNESCO_school_closures_database.xlsx")) %>%
  mutate(date=lubridate::ymd(Date)) %>%
  mutate(dow=lubridate::wday(date)) %>%
  filter(!(dow %in% c(1,7)))



unesco_closures_final <- unesco_closures_raw %>%
  group_by(`Country ID`, Status) %>%
  summarise(n=n()) %>% 
  pivot_wider(names_from = Status, values_from=n, values_fill=0) %>%
  transmute(
    iso3c=`Country ID`,
    unicef_total_closed=round(`Closed due to COVID-19`+0.5*`Partially open`,0),
    unicef_full_closed=`Closed due to COVID-19`,
    unicef_partial_closed=`Partially open`,
    unicef_academic_break=`Academic break`,
  )



#also read in some GNP data and population data for plots
wdi_df <- wb_data(
  indicator=c('NY.GNP.PCAP.CD', 'SP.POP.TOTL'),
  country = "All",
  start_date = 2020,
  end_date=2020
)

lpov_df <-read_csv(paste0(dir,"/Data/learning_poverty_region.csv")) %>%
  pivot_longer(
    cols=c("2015",         "2019",         "Optimistic",   "Intermediate", "Pessimistic" ),
    names_to='date',
    values_to='learning_poverty'
  ) %>%
  # filter(Group %in% c(
  #   "East Asia and Pacific", "Europe and Central Asia","Latin America and Caribbean", 
  #   "Middle East and North Africa", "South Asia", "Sub-Saharan Africa"
  # )) #keep just regions
  filter(
    Group %in% c("Low income", "Lower middle income", "Upper middle income", "High income")
  )


lpov_wb_df <- read_excel(paste0(dir, "/Data/lpv_edstats_update2022.xls"), sheet="WDI_indicators") %>%
  select(countrycode,year, indicator, value) %>%
  pivot_wider(
    names_from='indicator',
    values_from='value'
  ) %>%
  rename(iso3c=countrycode,
         year_assessment=year) %>%
  group_by(iso3c) %>%
  arrange(-year_assessment) %>%
  slice(1)%>%
  left_join(wdi_df) %>%
  select(iso3c, country, SE.LPV.PRIM,NY.GNP.PCAP.CD, SP.POP.TOTL ) %>%
  rename(learning_poverty=SE.LPV.PRIM,
         gni_per_capita=NY.GNP.PCAP.CD,
         population=SP.POP.TOTL)

write_excel_csv(lpov_wb_df, paste0(dir, "/output_data/Chart1_learning_poverty_GNI.csv"))


######
# Chart 2 - School closures and GNI per capita
######
covid_income_df <- covid_closures_df %>%
  mutate(primary_closures=if_else(!is.na(aq6_p_typical),aq6_p_typical, aq6_p_total)) %>% #use days closed for typical school if available
  select(countrycode, aq6_p_total, aq6_ls_total, aq6_us_total,primary_closures) %>%
  rename(iso3c=countrycode) 


#join gnp data
covid_income_df <- unesco_closures_final %>%
  left_join(wdi_df) %>%
  left_join(lpov_wb_df) %>%
  left_join(covid_income_df) %>%
  select(iso3c, iso2c, country, aq6_p_total,NY.GNP.PCAP.CD,SP.POP.TOTL, primary_closures, starts_with("unicef_")) %>%
  filter(!is.na(SP.POP.TOTL)) %>%
  rename(instruction_days_lost=primary_closures,
         gni_per_capita=NY.GNP.PCAP.CD,
         population=SP.POP.TOTL)


write_excel_csv(covid_income_df, paste0(dir, "/output_data/Chart2_closures_GNI.csv"))


########
# Internet and Electricity
########
#internet access andd electricity
internet_df <- wb_data(
  indicator=c('NY.GNP.PCAP.CD', 'SP.POP.TOTL', 'IT.NET.USER.ZS', 'EG.ELC.ACCS.ZS'),
  country = "countries_only",
  start_date = 2015,
  end_date=2020
)  %>%
  fill(c('IT.NET.USER.ZS','EG.ELC.ACCS.ZS','SP.POP.TOTL')) %>%
  filter(date==2020) 

internet_df <- internet_df %>%
  mutate(
    internet_pct=case_when(
      between(IT.NET.USER.ZS,0,30) ~ "0-30%",
      between(IT.NET.USER.ZS,30,60) ~ "0-60%",
      between(IT.NET.USER.ZS,60,75) ~ "60-75%",
      between(IT.NET.USER.ZS,75,90) ~ "70-90%",
      between(IT.NET.USER.ZS,90,100) ~ "90-100%"
    ),
    electricity_pct=case_when(
      between(EG.ELC.ACCS.ZS,0,30) ~ "0-30%",
      between(EG.ELC.ACCS.ZS,30,60) ~ "0-60%",
      between(EG.ELC.ACCS.ZS,60,75) ~ "60-75%",
      between(EG.ELC.ACCS.ZS,75,90) ~ "70-90%",
      between(EG.ELC.ACCS.ZS,90,100) ~ "90-100%"
    )
  ) %>%
  rename(
         gni_per_capita=NY.GNP.PCAP.CD,
         population=SP.POP.TOTL)

write_excel_csv(internet_df, paste0(dir, "/output_data/Chart3_internet_electricity.csv"))


########
# Remote Instruction platform
########

platform_df <- covid_closures_df %>%
  mutate(
    onlineplatforms = if_else(( dq1_pp_onlineplatforms ==1| dq1_ls_onlineplatforms ==1| dq1_us_onlineplatforms ==1),1,0),
    television = if_else(( dq1_pp_television ==1| dq1_ls_television ==1| dq1_us_television ==1),1,0),
    mobilephones = if_else(( dq1_pp_mobilephones ==1| dq1_ls_mobilephones ==1| dq1_us_mobilephones ==1),1,0),
    radio =if_else(( dq1_pp_radio ==1| dq1_ls_radio ==1| dq1_us_radio ==1),1,0),
    takehomepackages =if_else(( dq1_pp_takehomepackages ==1| dq1_ls_takehomepackages ==1| dq1_us_takehomepackages ==1),1,0),
    otherdistancelearningm =if_else(( dq1_pp_otherdistancelearningm ==1| dq1_ls_otherdistancelearningm ==1| dq1_us_otherdistancelearningm ==1),1,0)
    
  )

platform_stats <- platform_df %>%
  group_by(incomelevelname) %>%
  summarise(onlineplatforms=mean(onlineplatforms, na.rm=TRUE),
            television=mean(television, na.rm=TRUE),
            mobilephones=mean(mobilephones, na.rm=TRUE),
            radio=mean(radio, na.rm=TRUE),
            takehomepackages=mean(takehomepackages, na.rm=TRUE),
            otherdistancelearningm=mean(otherdistancelearningm, na.rm=TRUE),
            television=mean(television, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(income=factor(incomelevelname, levels=c("High income", "Upper middle income", "Lower middle income", "Low income")))


write_excel_csv(platform_stats, paste0(dir, "/output_data/Chart4_remote_instruction_platforms.csv"))

#########
# COVID Learning Poverty Projections
#########
pov_current <- lpov_df %>% filter(date %in% c('2015','2019'))
lpov_opt <- lpov_df %>% filter(date %in% c('2019',"Optimistic")) %>% mutate(date=if_else(date=="Optimistic","2022",date)) %>% rename(Optimistic=learning_poverty)
lpov_int <- lpov_df %>% filter(date %in% c('2019',"Intermediate")) %>% mutate(date=if_else(date=="Intermediate","2022",date)) %>% rename(Intermediate=learning_poverty)
lpov_pess <- lpov_df %>% filter(date %in% c('2019',"Pessimistic")) %>% mutate(date=if_else(date=="Pessimistic","2022",date)) %>% rename(Pessimistic=learning_poverty)

income_sim_chart <- lpov_int %>%
  left_join(lpov_opt) %>%
  left_join(lpov_pess)

write_excel_csv(income_sim_chart, paste0(dir, "/output_data/Chart5_learning_poverty_simulations.csv"))

######
# COVID Student Learning studies (Patrinos et al)
#######
meta_closures_df <- read_excel(paste0(dir, "/Data/covid_learning_losses.xlsx")) %>%
  mutate(share_closed=`Closure length weeks`/40, #assume 40 weeks of school
         years_lost=`Average learning losses (SD)`/0.33)

write_excel_csv(meta_closures_df, paste0(dir, "/output_data/Chart6_student_learning_impacts.csv"))

#####
# COVID Dropouts
######

meta_dropouts_df <- read_excel(paste0(dir, "/Data/Moscoviz_Evans_dropouts.xlsx")) %>%
  filter(!is.na(`Dropout Rate - Pre`)) 

write_excel_csv(meta_dropouts_df, paste0(dir, "/output_data/Chart7_student_dropout_impacts.csv"))

######
# Mexico Impacts
######

math_mexico_df <- read_excel(paste0(dir, "/Data/hevia_2022_learning_losses_mexico.xlsx"), sheet="math") %>%
  mutate(SES=factor(SES, levels=c("High SES", "Middle High SES", "Middle Low SES", "Low SES")))


#reading
reading_mexico_df <- read_excel(paste0(dir, "/Data/hevia_2022_learning_losses_mexico.xlsx"), sheet="reading") %>%
  mutate(SES=factor(SES, levels=c("High SES", "Middle High SES", "Middle Low SES", "Low SES")))

mexico_df <- math_mexico_df %>%
  left_join(reading_mexico_df) %>%
  select(SES, division_2019, division_2021, comprehension_2019, comprehension_2021)

write_excel_csv(mexico_df, paste0(dir, "/output_data/Chart8_mexico_study.csv"))



######
# LSMS studies
#######

lsms_fig1_data <- read_excel(path=paste0(dir,"/Data/LSMS_data_covid_education.xlsx"), skip=4, sheet='Figure_1') %>%
  mutate(before_covid=after_covid+gap) %>%
  select(Country, before_covid, after_covid) %>%
  pivot_longer(cols=c('before_covid','after_covid'),
               names_to='period',
               values_to='learning_activities') %>%
  pivot_wider(
    names_from='Country',
    values_from='learning_activities'
  ) %>%
  mutate(period=case_when(
    period=="before_covid" ~ "Before Covid",
    period=="after_covid" ~ "April-June 2020"
  )) %>%
  mutate(period=factor(period, levels=c("Before Covid","April-June 2020")))

write_excel_csv(lsms_fig1_data, paste0(dir, "/output_data/Chart9_lsms_learning_activities_change.csv"))

#SES
lsms_fig5_data <- read_excel(path=paste0(dir,"/Data/LSMS_data_covid_education.xlsx"), skip=4, sheet='Figure_5') %>%
  pivot_longer(cols=c('Q1','Q2','Q3','Q4','Q5'),
               names_to='Quintile',
               values_to='learning_activities') %>%
  mutate(Country=if_else(Country=="Burkina_Faso","Burkina Faso", Country)) %>%
  left_join(read_csv(paste0(dir, "/Data/LSMS_data_precovid.csv")))

write_excel_csv(lsms_fig5_data, paste0(dir, "/output_data/Chart10_lsms_learning_activities_inequality.csv"))


enrol_df <- wb_data(
  indicator = c('UIS.ROFST.H.1',
                'UIS.ROFST.H.1.Q1',
                'UIS.ROFST.H.1.Q2',
                'UIS.ROFST.H.1.Q3',
                'UIS.ROFST.H.1.Q4',
                'UIS.ROFST.H.1.Q5'),
  country = c("BFA","ETH","MWI","MLI",
              "NGA","UGA"),
  start_date = 2010,
  end_date = 2021
)

########
# Brazil Study
########

sao_paulo_df <- read_csv(paste0(dir, "/Data/sao_paulo_covid.csv"))

write_excel_csv(sao_paulo_df, paste0(dir, "/output_data/Chart11_sao_paulo_brazil_study.csv"))


