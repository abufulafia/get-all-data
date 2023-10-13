require(tidyverse)
require(GlobalFundr)
require(readxl)

# A. Preliminary steps and background information ####
   # a. ABOUT ####

# creates a full wide format dataset with the most common calculated fields based on PIP and partner lives saved data (incidence and mortality rates)
## HIV incidence rates since Spring 2020 KPI reporting have used spectrum population

## DEFINITIONS ##


# 11 Feb 2020 - added spectrum based incidence rates for use for KPI 1 reporting and to transition to use in CPRs

# dfw$hiv_mor_rate_100k_spec
# dfw$hiv_mor_rate_100k_spec

# population1 = UN population in current year
# population_2 = un population in year t-1

# population2 = spectrum population in current year
# population_3 = spectrum population in year t-1


###  GF lives saved from 2017 onwards

# mal_deaths_averted_CF_WHO_2000
# tb_deaths_averted_hivneg_WHO_2000
# [OLD: deaths_averted_goals_aim_CF_2015_KPI 24 JAN 2020 hiv_deaths_averted_goals_aim_CF_2015_KPI

# GF historic lives saved up to 2016 [used a financial share applied to results]

# Easy reference to GF historic lives saved to 2016, then KPI1 method from 2017 onwards
# ls_gf_hiv
# ls_gf_tb.hivneg
# ls_gf_mal


# GF historic lives saved up to and including 2016 
# ls_hiv_gf_hist,
# ls_tb.hivneg_gf_hist,
# ls_mal_gf_hist


### disbursement data

# disbursement cols labelled as tot have  TB and HIV as split per finance breakdown agreed for results report 2020
# disbursement cols labelled as raw do not
# disbursement cols ending cum are cumulative by year


# b. TO DO ####




# c. Switchboard- Load required packages and set up working directories ####
# rm(list = ls())

# source("~/__SI/_R_scripts_git/richards_functions_June_2019.R")
# loadmypackages()
require(tidyverse)
require(readxl)
require(GlobalFundr)


out <- "C://Users//rgrahn//OneDrive - The Global Fund//Documents//__SI//_R_scripts_out//dfw//"
in_ <- "C://Users//rgrahn//OneDrive - The Global Fund//Documents//__SI//_R_scripts_in//"


# d. Switchboard- Define parameters and load required source files (PIP,meta data, counterfactuals, UNAIDS data)   ####

# set the latest data for partner epi data

latest_hiv_year <- 2022
latest_tb_year <- 2021
latest_mal_year <- 2021

# set start and end date of values to load from PIP (for file size management)
pip_year_start <- 1990 
pip_year_end   <- 2030 

# set to 1 to write a copy of pip for audit / debug purposes. Otherwise set to zero 
save_pip <- 0

# set to 1 to write a copy of full disbursements for further analysis
save_disbursements <- 1

# set eligibility year
eligibility_year <- 2021

# set disbursements start year and end year [for results report for results report set a cut off date of end June of year of publication ]

disb_year_start <- 2000
disb_year_end <- 2022


# AIDS deaths and infections averted ##

# no ART since 2017 constant behavior from 2015
#  from John Stover 28 July 2023
# using 'with dropoff' version
goals_deaths_averted_2015_location <- paste0(in_,"2023_01//cf//Counterfactual 72 countries no ART since 2017 constant behavior from 2015_28jul2023_with Goals dropout sync.xlsx")
file.exists(goals_deaths_averted_2015_location)

# FIRST  version from Yu Teng 10 Aug 2023
# CORRECTED version from Yu Ten 5 Oct 2023 to correct PMTCT data error for BFA
# goals_inf_deaths_averted_2000 file 

goals_inf_deaths_averted_2000_location <- paste0(in_,"2023_01//Counterfactual 77 countries no ART no BC after 2000 28jul23_2022 added_BFA_corr.xlsx")
file.exists(goals_inf_deaths_averted_2000_location)

AIM_deaths_averted_location <- paste0(in_,"2023_01//HIV2023Estimates_GF_17July2023.xlsx")
file.exists(AIM_deaths_averted_location)


AEM_inf_deaths_averted_2000_location <- paste0(in_,"2023_01//Historical impact of HIV programs from AEM 2023.xlsx")
file.exists(AEM_inf_deaths_averted_2000_location)

AEM_inf_deaths_averted_2015_location <- paste0(in_,"2023_01/Strategy impacts from AEM 2023.xlsx")
file.exists(AEM_inf_deaths_averted_2015_location)

UNAIDS_estimates_location <- paste0(in_,"2023_01//HIV2023Estimates_GF_17July2023.xlsx")
file.exists(UNAIDS_estimates_location)


# WHO TB department lives saved  ##
tb_ls_location <- paste0(in_, "2023_01/lives_saved_2023-08-30.csv") # lives saved estimate from WHO TB department Mathieu Bastard
file.exists(tb_ls_location)

# WHO Malaria department lives saved  ##
mal_ls_location <- paste0(in_, "2022_03/wmr2022_gf_indicators_FINAL_20_12_2022.xlsx") # lives saved estimate from WHO malaria department
file.exists(mal_ls_location)

# historical H, T, M lives saved  ##
ls_to2016_location <-  paste0(in_, "2023_01/ls_dashboard.xlsx")
file.exists(ls_to2016_location)

# finance dept.'s split of tb/hiv grants into TB and HIV  ##
tb_hiv_breakdown_location <- paste0(in_, "2023_01/HIV-TB - Disbursement Breakdown 31Dec2022.xlsx")

# read in list of indicators to obtain from PIP
pip_indicators_needed <- readLines("~/__SI/_R_scripts_in/2023_01/pip_indicators_needed.csv", encoding ="UTF-8-BOM")[-1] # drop column name deliberately

lists_location <-paste0(in_,"2023_01//Lists_2023_01.xlsx") # file with disbursement amounts per country, use as results report cohort filter

modelled_kpi_loc_cycle_17_22 <- "~//__SI//_R_scripts_in//2018_11//Copy of allModelledKPIs_3Mar2017.xlsx"
file.exists(modelled_kpi_loc_cycle_17_22)

# read in raw disbursement data from GF public website
# https://data-service.theglobalfund.org/downloads >> to go Data Sets >> Grant Agreement Disbursements
#  set the base URL
disb_location <-
  "https://fetch.theglobalfund.org/v3/odata/VGrantAgreementDisbursements/?$select=geographicAreaCode_ISO3,componentName,disbursementMonth,disbursementYear,disbursementAmount" 

disb_location_multi_country <- 
  "https://fetch.theglobalfund.org/v3/odata/VGrantAgreementDisbursements/?$select=multiCountryName,geographicAreaCode_ISO3,componentName,disbursementMonth,disbursementYear,disbursementAmount" 

# set the location of eligibility data
eligiblity_location <- "~/__SI/_R_scripts_in/2022_01/core_historicaleligibility_database_en.xlsx"

# read in specific lists for results report data production process
rr_map_countries_list_location <- "~/__SI/_R_scripts_in/2020_03/map_countries.csv"
file.exists(rr_map_countries_list_location)

rr_top_10s_location <- paste0(in_,"2021_02/disease_table_cohorts_rr_2021.xlsx")
file.exists(rr_top_10s_location)

# e. Switchboard- SELECT ON LINE OR  OFFLINE VERSION OF PIP and read in PIP ####

# to do switch to use either live pip or an offline processed copy. for now unhash section below to use offline version 
# contains 2021 values for HIV, and 2020 for TB and malaria epi data
# raw_PIP_data  <- read.csv(paste0(in_,"2022_02/PIP_mirror.csv"), stringsAsFactors = FALSE, na.strings = c("NA"))
# for pip mirror for 2022 results report there are stray decimal points in the numerator and denominator columns
# raw_PIP_data <-dplyr::na_if(raw_PIP_data, ".")

# unhash here to extract PIP using GlobalFundr
# 
raw_PIP_data <- 
  GlobalFundr::extractPIP(indicators = pip_indicators_needed) %>% 
  rename_with(tolower) %>% 
  select(iso3codecountry,partnerindicatorvalueinternal,partnerindicatorvaluepublic,partnerindicatorvalueshortname,year) %>% 
  rename(iso3=1,value=2,valuepublic=3,indicatoruniqueidentifier=4,year=5)

# # # # for definitive runs make a copy of PIP
if(save_pip==1) {
  write.csv(raw_PIP_data,file = paste0(out,"raw_PIP_data",Sys.Date(),".csv"),row.names = FALSE)
  print(paste0("A copy of PIP was written to",out,"raw_PIP_data",Sys.Date(),".csv"))
}

# 14 june 2023 HIV133 has duplicates in PIP so remove these using distinct until PIP is updated
dfw <-
  raw_PIP_data %>% select(-(valuepublic)) %>% mutate(id=paste0(iso3,indicatoruniqueidentifier,year)) %>% distinct(id, .keep_all=TRUE) %>% select(-(id))


# f. Set the criteria for inclusion in results report cohort using lists file  ####
lists<-
  read_excel(lists_location, col_names = TRUE) %>%
  rename(country=1) %>%  
  select(
    iso3,iso2,country,`gf_region 10`,ssa,
    `GFalloc1719_HIV/AIDS`,GFalloc1719_TB,GFalloc1719_Malaria,
    `GFalloc2022_HIV/AIDS`,GFalloc2022_TB,GFalloc2022_TB,GFalloc2022_Malaria,
    GF_ST_2022_hivi,GF_ST_2022_tbi,GF_ST_2022_mali,
    `gf_region 10`, who_region,
    AGYW13countries,
    final_hiv_eligibility_17,final_tb_eligibility_17,final_malaria_eligibility_17,
    COE) %>% 
  mutate(`GFalloc1719_HIV/AIDS`=ifelse(is.na(`GFalloc1719_HIV/AIDS`),0,`GFalloc1719_HIV/AIDS`)) %>%  # replace NAs with 0 in 6 columns
  mutate(GFalloc1719_TB=ifelse(is.na(GFalloc1719_TB),0,GFalloc1719_TB)) %>% 
  mutate(GFalloc1719_Malaria=ifelse(is.na(GFalloc1719_Malaria),0,GFalloc1719_Malaria)) %>% 
  mutate(`GFalloc2022_HIV/AIDS`=ifelse(is.na(`GFalloc2022_HIV/AIDS`),0,`GFalloc2022_HIV/AIDS`)) %>% 
  mutate(GFalloc2022_TB=ifelse(is.na(GFalloc2022_TB),0,GFalloc2022_TB)) %>% 
  mutate(GFalloc2022_Malaria=ifelse(is.na(GFalloc2022_Malaria),0,GFalloc2022_Malaria)) %>% 
  # since Russia is only eligible under the NGO rule, if necessary, remove Russia's eligibility for HIV for results reports purposes
  
  mutate(`GFalloc2022_HIV/AIDS`= ifelse(iso3=="RUS",0,`GFalloc2022_HIV/AIDS`)) %>% 
  # create new columns for each disease if elibible in either 17-19 or 20-22 [current definition for results report cohort]
  mutate(`GFalloc1722_HIV/AIDS`=if_else((`GFalloc1719_HIV/AIDS`==1 |`GFalloc2022_HIV/AIDS`==1),1,0)) %>% 
  mutate(GFalloc1722_TB =if_else((GFalloc1719_TB==1 |GFalloc2022_TB==1),1,0)) %>% 
  mutate(GFalloc1722_Malaria= if_else((`GFalloc1719_Malaria`==1 | GFalloc2022_Malaria==1),1,0))  %>% 
  # remove KOS from results report cohort using QNA for KOSOVO
  filter(iso3!="KOS") %>% 
  
  # since QNA is the listed iso3 code in geography reference, use QNA throughout 
  mutate(iso3=ifelse(iso3=="KSV","QNA",iso3)) %>% 
  mutate(`gf_region 10`=ifelse(iso3=="QNA","Eastern Europe and Central Asia",`gf_region 10`))





# note that source file for countries receiving an allocation is C:/Users\rgrahn\OneDrive - The Global Fund\Documents\__SI\_R_scripts_in\2019_08\allocations_17_19_allocations_team.xlsx see Scratch.R for working

# d3. read in list of countries with a non-zero strategy target for LIVES SAVED KPI 1 i [as well as all other reporting cohorts incidence, mortality, deaths and cases ]

# 3 countries have 0 values for the median mix run [ECU,GUY,SSD but per the validated script for KPI 1 Spring 2020 reporting  these are included]
# 100 countries listed in KPI Due diligence Documentation_KPI1_draft_march 2017.pdf 
# 3 countries have a target of zero lives saved but these are still included for lives saved reporting

# cohort for TB i LIVES SAVED
# 99 countries listed countries listed in KPI Due diligence Documentation_KPI1_draft_march 2017.pdf, Copy of allModelledKPIs_3Mar2017.xlsx includes 115 countries but 16 of these have no data [NaN], DMA, GRD, IRQ,KIR, KOS,MHL,FSM,PSE, LCA, VCT,WSM,SYR,TON,TUV, VUT, ZAN. 
# 16 countries have NaN
# No countries have a zero target for the Strategy target
# 99 countries with data

# cohort for mal i LIVES SAVED
# 2 countries that have NaN GNB TLS
# no countries with targets of zero
# 66 countries listed in KPI Due diligence Documentation_KPI1_draft_march 2017.pdf 64 countries have a target that is not NaN


tab_list <- c("HIV KPI 1 c",
              "HIV KPI 1 d",
              "HIV KPI 1 e",
              "HIV KPI 1 b",
              "HIV KPI 1 a",
              "TB KPI 1 c",
              "TB KPI 1 d",
              "TB KPI 1 e",
              "TB KPI 1 b",
              "TB KPI 1 a",
              "Malaria KPI 1 c",
              "Malaria KPI 1 d",
              "Malaria KPI 1 e",
              "Malaria KPI 1 b",
              "Malaria KPI 1 a")

finish <- data.frame(iso3="")
tmp <- data.frame()

# note that in the source excel file NAs are represented by text "NaN"
# these are replaced with real NAs in the excel read in script na="NaN"

for (tab in tab_list) {
  tmp <- read_excel(modelled_kpi_loc_cycle_17_22,sheet=tab, range = "B6:BV150", col_names = FALSE, na="NaN")
  
  colnames(tmp)[1]  <- "iso3"
  colnames(tmp)[65] <- tab  
  colnames(tmp)[73] <- paste0(tab,"_ub")
  colnames(tmp)[57] <- paste0(tab,"_lb")
  tmp <- tmp %>% select(1,73,65,57)
  
  finish <- full_join(tmp,finish)
  finish <- finish %>% filter(iso3!="")
}
# make all columns numeric 
finish[,c(2:ncol(finish))] <- sapply(finish[,c(2:ncol(finish))],as.numeric)

finish <- finish %>% 
  rename_with(tolower) 

colnames(finish) <- tolower(colnames(finish)) 
colnames(finish) <- gsub("malaria", "mal", colnames(finish))
colnames(finish) <- gsub(" ", "_", colnames(finish))
colnames(finish) <- gsub("kpi_1", "", colnames(finish))

# rename the columns to standardise them
colnames(finish) <- gsub("_a", "case_st", colnames(finish))
colnames(finish) <- gsub("_b", "death_st", colnames(finish))
colnames(finish) <- gsub("__c", "_inc_st", colnames(finish))
colnames(finish) <- gsub("__d", "_mor_st", colnames(finish))
colnames(finish) <- gsub("_e", "_l_saved_st", colnames(finish))
colnames(finish) <- gsub("__", "_", colnames(finish))

# exclude countries from the cohort that have NA for a the median mix scenario in 2022
finish <- finish %>% 
  # mal
  mutate(mal_mor_coh_st=ifelse(!is.na(`mal_mor_st`),1,NA)) %>% 
  mutate(mal_inc_coh_st=ifelse(!is.na(`mal_inc_st`),1,NA)) %>% 
  mutate(mal_l_saved_st_coh=ifelse(!is.na(`mal_l_saved_st`),1,NA)) %>% 
  # tb
  mutate(tb_mor_coh_st=ifelse(!is.na(`tb_mor_st`),1,NA)) %>% 
  mutate(tb_inc_coh_st=ifelse(!is.na(`tb_inc_st`),1,NA)) %>% 
  mutate(tb_l_saved_coh_st=ifelse(!is.na(`tb_l_saved_st`),1,NA)) %>%
  # hiv
  mutate(hiv_mor_coh_st=ifelse(!is.na(`hiv_mor_st`),1,NA)) %>% 
  mutate(hiv_inc_coh_st=ifelse(!is.na(`hiv_inc_st`),1,NA)) %>% 
  mutate(hiv_l_saved_coh_st=ifelse(!is.na(`hiv_l_saved_st`),1,NA)) 

# keep only columns defining the cohorts 
finish <- finish %>% select(iso3,contains("coh"))

# values checked against source 27 Nov 2020
KPI1_cohort <- finish

KPI1_cohort <- KPI1_cohort %>% mutate(iso3=ifelse(iso3=="KOS","QNA",iso3)) 

# KPI1_coh %>% summarise_if(is.numeric,sum,na.rm=TRUE)
# KPI1_coh %>% summarise(across(contains("coh"),sum,na.rm=TRUE))



lists <- full_join(lists%>% select(-(contains("GF_ST"))),KPI1_cohort)

# B. Get malaria lives saved and cases averted from WHO [2000 cf]####

# Notes on versions of source file from WHO
# 21 Dec 2022 use file provided by Laura Anderson  wmr2022_gf_indicators_FINAL_20_12_2022.xlsx
# use WHO provided file from GALATAS ANDRADE 20 Dec 2021 Global_Fund_2021_12_20.csv
# 8 Dec 2020 calculate the deaths & infections averted from the source epi data as these have not been provided in time


# other notes on file format etc 
# WHO now provides both cases infections and deaths averted [2000 cf]
# note that the ls and ia prior to 2017 are NOT the historic lives saved (using attributive method up to and including 2016)
# the 'raw' ls and ia have been used in results report for the graphs of incidence and mortality


mal_cfs <-
  read_excel(paste0(mal_ls_location)) %>%
  select(code, year, indicator,value) %>% 
  filter(indicator=="Cases averted per year" | indicator=="Deaths averted per year") %>% 
  filter(nchar(as.character(code))==3) %>% # retain only iso3s - filter out world, regions etc
  rename(iso3=code) %>% 
  rename(indicatoruniqueidentifier=indicator) %>% 
  mutate(indicatoruniqueidentifier=ifelse(
    indicatoruniqueidentifier=="Deaths averted per year","mal_deaths_averted_CF_WHO_2000",indicatoruniqueidentifier)) %>% 
  mutate(indicatoruniqueidentifier=ifelse(
    indicatoruniqueidentifier=="Cases averted per year","mal_cases_averted_CF_WHO_2000", indicatoruniqueidentifier))

dfw <- 
  rbind(dfw,mal_cfs) %>% 
  mutate(year=as.numeric(year)) 

# C. Get TB lives saved estimates from WHO Global TB Program ####

# file from Mathieu Bastard PROVISIONAL lives saved numbers 


ls_tbneg <-
  read.csv(tb_ls_location, stringsAsFactors = FALSE) %>% 
  select(1:3) %>% # keep relevant columns (drop lives saved including hiv +)
  filter(year!="Cumulative") %>% # remove 'cumulative rows'
  na.omit() %>% 
  # mutate(year=as.character(year)) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(value=saved.hivneg*10^3) %>% # each value has to be multiplied by 1,000
  select(-(saved.hivneg)) %>% 
  mutate(indicatoruniqueidentifier="tb_deaths_averted_hivneg_WHO_2000") #name to ensure no confusion with historic GF lives saved reported based on attributive method prior to 2017


# rbind it to pip data
dfw <- rbind(dfw,ls_tbneg) 


# D. HIV CF 1_2015_ deaths averted [2015 CF -Goals and AIM] estimates from Goals and AIM [for KPI1 reporting] ####
#  KPI1 definition of counterfactual: <<<In the case of HIV this counterfactual assumes that any sexual behavior changes established by 2015 would persist but no ART would be available after 2017.>>> cf Results report portfolio impact trend graphs which are based on no ART ever. Overall lives saved number is calculated consistently. AEM estimates are not used for KPI reporting.

# 1. Get GOALS 2015 CF - deaths averted ####

# take GOALS lives saved from file shared by John Stover 20 JUly 2020
# if no GOALS lives saved estimate, use AIM estimates shared by UNAIDS directly
#  for KPI reporting, set behaviour constant at 2015 levels, no ART from 2017

goals_deaths_averted <-
  read_excel(goals_deaths_averted_2015_location, 
                                   sheet=3,col_names= TRUE, range = "A2:AZ79") %>% #check sheet = "Deaths averted" & range includes latest year 
  rename(iso3=`ISO 3166-1 alpha-3`) %>% 
  select(iso3,"2015","2016","2017","2018","2019","2020","2021","2022") %>% #add latest year here 
  gather(key=year,value=value,2:last_col()) %>% 
  rename(ls_goals_unadjusted=value) %>% 
  mutate(year=as.numeric(year))


# read in JS GOALS estimate of DEATHS under the CF of no ART from 2017 
# and behaviour constant at 2015 level  in John Stover file
goals_deaths <-       
  read_excel(paste0(goals_deaths_averted_2015_location),
                                 sheet=7,col_names= TRUE, range = "A2:AZ79") %>% # check sheet  = 2. AIDS Deaths
  rename(iso3=`ISO 3166-1 alpha-3`) %>% 
  select(iso3,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`,`2022`) %>% #add latest year  
  gather(key=year, value = value,2:last_col()) %>% 
  rename(deaths_goals_js=value) %>% 
  select(iso3,year,deaths_goals_js) %>%   
  mutate(year=as.numeric(year))

goals_deaths_averted <- full_join(goals_deaths_averted,goals_deaths)


# also read in AIM estimate of deaths from UNAIDS
UNAIDS_data <- 
  read_excel(UNAIDS_estimates_location,
                          col_names= TRUE,sheet= "HIV raw 2000-2022",
             # , range= "A2:KZ3808"
             skip=1) %>% # update sheet name and range 
  # rename(year="...1") %>% 
  rename_with(tolower)%>% 
  select(iso3,year,"n- aids deaths male+female") %>% 
  filter(nchar(iso3)==3)# remove non country names from the iso3 column

goals_deaths_averted <- left_join(goals_deaths_averted,UNAIDS_data)

goals_deaths_averted <-goals_deaths_averted %>% rename(deaths_aim="n- aids deaths male+female")

# because there are some slight differences between UNAIDS published estimates of actual deaths which appear 
# in John Stover's file [AIM AIDS Deaths] compared to UNAIDS own published estimates redo the scaling using UNAIDS
# published data

# create a column with the ratio of 2015 AIDS deaths UNAIDS / 2015 AIDS deaths goals js and multiply 2015 to 2020
# GOALS deaths averted by this ratio in order to adjust John Stover deaths to match UNAIDS AIM 2015 baseline

goals_deaths_averted <- goals_deaths_averted %>% 
  group_by(iso3) %>% 
  arrange(iso3) %>% 
  mutate(helper=ifelse(year==2015,deaths_aim/deaths_goals_js,NA)) %>% 
  fill(helper,.direction = "updown") %>%
  mutate(deaths_adjusted=helper*deaths_goals_js) %>% 
  # then subtract the deaths_aim from deaths_adjusted
  mutate(ls_goals_adjusted=deaths_adjusted-deaths_aim) %>% 
  select(-(helper)) %>%  
  ungroup() %>% 
  gather(key=indicator,value = value,3:7) %>% 
  select(iso3,year,value,indicator) 


# 2. Get AIM  - deaths averted #### 
# read in UNAIDS AIM estimates of deaths averted in file shared by  DAHER, Juliana . AIM estimates of deaths averted
UNAIDS_data <- 
  read_excel(AIM_deaths_averted_location,
                          col_names= TRUE,sheet= 10, 
             skip=1) %>%  #update range each year
  rename(year="...1") %>% 
  rename_with(tolower)%>%
  select(iso3,year,`n- deaths averted by art male+female`) %>% 
  rename(ls_aim=`n- deaths averted by art male+female`) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(indicator="ls_aim") %>% 
  rename(value=ls_aim) %>% 
  filter(nchar(iso3)==3)# remove non country names from the iso3 column


ls_hiv <- rbind(UNAIDS_data,goals_deaths_averted)

# remove non country names from the iso3 column
ls_hiv <- ls_hiv %>% filter(nchar(ls_hiv$iso3)==3)

ls_hiv <- ls_hiv %>% filter(!is.na(year))
ls_hiv <- ls_hiv %>% filter(!is.na(iso3))

ls_hiv <- spread(ls_hiv,key=indicator,value=value)


# 3. Implement algorithm for 2015 CF - deaths averted ####

# Because  GOALS estimates include lives saved from prevention programs use these as first choice but if not available, then use the AIM estimate (only ART treatment effect)


# rename the adjusted column
ls_hiv <- ls_hiv %>% rename(ls_goals=ls_goals_adjusted)

# since goals includes prevention effect plus treatment GOALS estimate should be higher than AIM which is treatment only therefore since GOALS includes prevention it isn't reasonable for GOALS lives saved to be less than AIM lives saved
ls_hiv$ls_aim <- as.character(ls_hiv$ls_aim)
ls_hiv$ls_goals <- as.character(ls_hiv$ls_goals)


ls_hiv$ls_aim <- as.numeric(ls_hiv$ls_aim)
ls_hiv$ls_goals <- as.numeric(ls_hiv$ls_goals)

ls_hiv <- ls_hiv %>% mutate(ls_goals_aim=pmax(ls_hiv$ls_aim,ls_hiv$ls_goals,na.rm = TRUE))

# write a full version of ls hiv before removing unused indicators
# write.csv(ls_hiv,file=paste0(out,"ls_hiv_full",Sys.Date(),".csv"))

# prepare 4 column structure
ls_hiv <-ls_hiv %>% select(iso3,year,ls_goals_aim,ls_aim,ls_goals) %>% 
  gather(key=indicatoruniqueidentifier,value=value,3:5) 


ls_hiv$indicatoruniqueidentifier <-ifelse(ls_hiv$indicatoruniqueidentifier=="ls_goals_aim",
                                          "hiv_deaths_averted_goals_aim_CF_2015_KPI", 
                                          ls_hiv$indicatoruniqueidentifier)

ls_hiv$indicatoruniqueidentifier <-ifelse(ls_hiv$indicatoruniqueidentifier=="ls_aim",
                                          "hiv_deaths_averted_aim_CF_2015", 
                                          ls_hiv$indicatoruniqueidentifier)

ls_hiv$indicatoruniqueidentifier <-ifelse(ls_hiv$indicatoruniqueidentifier=="ls_goals",
                                          "hiv_deaths_averted_goals_CF_2015", 
                                          ls_hiv$indicatoruniqueidentifier)


# rbind it to pip data
dfw <- rbind(dfw,ls_hiv)
# check for duplicates
# row.names(c)<-(paste(c$iso3,c$indicatoruniqueidentifier,c$year))



# E. HIV CF 2_2000_ Get HIV infections  and deaths averted [2000 CF] from Goals, AEM and UNAIDS [CPR/results report graphics CF for HIV] ####

# read in   second counterfactual for HIV [used for results reporting graphs and CRP graphs. not for official KPI reporting]
# these indicators are all marked rr and only to be used for this purpose

# Counterfactual 77 countries no ART no BC after 2000 28jul23_2022 added.xlsx[John Stover/ Yu Teng ]
# Historical impact of HIV programs from AEM 2023.xlsx [ Tim Brown]
# HIV2023Estimates_GF_17July2023.xlsx  [UNAIDS Julian Daher]

# 1. Get GOALS 2000 CF - infections averted ####

# Goals infections averted 2000 CF
goals_infections_averted <- 
  read_excel(goals_inf_deaths_averted_2000_location,
                                       sheet="Infections averted",col_names= TRUE,
             range= "C2:AD79",
             ) %>% #check latest cols included
  rename(iso3=`ISO 3166-1 alpha-3`) %>% 
  select(iso3,(c(6:last_col()))) %>% 
  gather(key=year,value=value,2:ncol((.))) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(indicatoruniqueidentifier="infections_averted_goals_CF_2000")

# 2. Get GOALS 2000 CF - deaths averted ####
goals_deaths_averted <- 
  read_excel(goals_inf_deaths_averted_2000_location,
                                   sheet="Deaths averted",col_names= TRUE,range= "C2:AD79") %>% #check latest cols included
  rename(iso3=`ISO 3166-1 alpha-3`) %>% 
  select(iso3,(c(6:ncol(.)))) %>% 
  gather(key=year,value=value,2:ncol((.))) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(indicatoruniqueidentifier="deaths_averted_goals_CF_2000")

goals_d_i_averted <- full_join(goals_deaths_averted,goals_infections_averted)

# 3. Get AEM 2000 CF - infections averted ####
# AEM infections averted
AEM_infections_averted <- read_excel(AEM_inf_deaths_averted_2000_location,
                                     col_names= TRUE,range= "B2:Y15",sheet="Infections averted") %>% #check latest cols included
  # rename iso3 and other columns
  rename("iso3"="ISO 3166-1 alpha-3") %>%   
  # rename(2000:2021=(c(2:ncol((.))) 
  gather(key=year,value=value,2:ncol((.))) %>%                                      
  mutate(year=as.numeric(year)) %>% 
  mutate(indicatoruniqueidentifier="infections_averted_AEM_CF_2000")

# 4. Get AEM 2000 CF - deaths averted ####
AEM_deaths_averted <- 
  read_excel(AEM_inf_deaths_averted_2000_location,
                                 col_names= TRUE,range= "B2:Y15",sheet="Deaths averted") %>% 
  rename(iso3=`ISO 3166-1 alpha-3`) %>% 
  gather(key=year,value=value,2:ncol((.))) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(indicatoruniqueidentifier="deaths_averted_AEM_CF_2000")

AEM_d_i_averted <- full_join(AEM_deaths_averted,AEM_infections_averted)

# 5. also get AEM 2015 CF - deaths averted [not for KPI REPORTING]####
AEM_deaths_averted_2015 <- 
  read_excel(AEM_inf_deaths_averted_2015_location,
                                      col_names= TRUE,range= "B2:J15",sheet="Deaths averted") %>% 
  rename(iso3=`ISO 3166-1 alpha-3`) %>% 
  gather(key=year,value=value,2:ncol((.))) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(indicatoruniqueidentifier="deaths_averted_AEM_CF_2015")

AEM_d_i_averted <- full_join(AEM_d_i_averted,AEM_deaths_averted_2015)

# 6. Join  AIM [UNAIDS] 2000 CF - deaths averted ####
d_i_averted <- 
  full_join(AEM_d_i_averted,goals_d_i_averted) %>% 
  mutate(year=as.integer(year)) %>% 
  full_join(ls_hiv) %>% 
  filter(!is.na(iso3)) %>%
  filter(!is.na(year)) %>% 
  spread(key=indicatoruniqueidentifier,value=value,fill = NA, convert = FALSE, sep = NULL, drop=TRUE)

# 7. Implement algorithm for 2000 CF - deaths averted #### 
#  algorithm use AEM, then GOALS, then UNAIDS


# updated in 2019. use Max of GOALS or AIM
d_i_averted <- 
  d_i_averted %>% 
  mutate(deaths_averted_a_g_aim_CF_2000_cpr=
           ifelse(!is.na(deaths_averted_AEM_CF_2000),deaths_averted_AEM_CF_2000,
                  ifelse(!is.na(pmax(hiv_deaths_averted_aim_CF_2015,deaths_averted_goals_CF_2000,na.rm = TRUE)),
                         pmax(hiv_deaths_averted_aim_CF_2015,deaths_averted_goals_CF_2000,na.rm = TRUE),
                         NA)))


# 8. Implement algorithm for 2000 CF - infections averted #### 

# algorithm for incidence only looks for AEM, then goals [AIM has no estimate of infections prevented]. ZWE has goals and it is used.BGD has AEM and it is used. DJI has neither. WORKING CORRECTLY.
# hiv_d_i$infections_averted_a_g_CF_2000_cpr <- ifelse(
#   !is.na(hiv_d_i$infections_averted_AEM_CF_2000),hiv_d_i$infections_averted_AEM_CF_2000,hiv_d_i$infections_averted_goals_CF_2000)

d_i_averted <- 
  d_i_averted %>% 
  mutate(infections_averted_a_g_CF_2000_cpr = 
           ifelse(!is.na(infections_averted_AEM_CF_2000),infections_averted_AEM_CF_2000,infections_averted_goals_CF_2000)) %>%  
  gather(key=indicatoruniqueidentifier,value=value,3:last_col()) %>% 
  full_join(dfw) %>% 
  # for clarity rename AIM deaths averted indicator since it is not the 2015 counterfactual
  mutate(indicatoruniqueidentifier=ifelse(indicatoruniqueidentifier=="hiv_deaths_averted_aim_CF_2015","hiv_deaths_averted_aim_CF",indicatoruniqueidentifier))
# join the 2015 and 2000 counterfactuals into one data frame


# read in UNAIDS estimates of AIDS deaths, new infections and PLHIV

UNAIDS_data <- 
  read_excel(UNAIDS_estimates_location,
                          col_names= TRUE,range= "A2:KZ3981", sheet = 2) %>% 
  # rename(year="...1") %>%  
  rename(country="...5") %>% 
  rename_with(tolower) %>% 
  rename_with(~gsub("n-" , '', .x,fixed=TRUE)) %>% # update the indicator prefix letter each year 
  rename_with(~gsub(" " , '', .x,fixed = TRUE)) %>% 
  select(year,iso3,`aidsdeathsmale+female`,`newhivinfectionsmale+female`,`hivpopulationmale+female`) %>%                                         
  gather(key=indicatoruniqueidentifier,value=value,3:ncol(.)) %>%  
  mutate(year=as.numeric(year)) %>% 
  mutate(value=as.numeric(value)) %>% 
  filter(nchar(iso3)==3) %>% 
  full_join(d_i_averted)


dfw <- 
  full_join(d_i_averted,UNAIDS_data) %>% 
  filter(!(is.na(iso3))) %>%
  filter(year>=1999)

# F. Load historic lives saved to 2016 ####

# updated to Dec 2021 historic partners lives saved master file [C:\Users\rgrahn\OneDrive - The Global Fund\Documents\__SI\_R_scripts_out\KPI1\Master files Jan 2022 KPI reporting\MH calcs\pre_2017estimates\ls_dashboard.xlsx]

# updated to Dec 2020 historic partner lives 

# updated 27 Aug 2020 - master file is on sharepoint at  [https://tgf.sharepoint.com/:f:/r/sites/TSSIN1/PRIE/Project%20folders/Projections/Results%20reports/Results%20report%202020/lives%20saved%20calc%20for%20results%20report/2017-2%20updated_in_2020?csf=1&web=1]
# HIV - historic GF lives saved

# updated 21 Oct 2020 with TB report 2020 data master file at \\gf\Common\Applications\AccessDatabases\richard\fall2020_after_results_report_using final TB data_from_mehran\2017-2 updated_in_2020

# updated aug 2021 for results report using master file at \\gf\Common\Applications\AccessDatabases\richard\fall2021_for_results_report\2017-2 updated_in_2021

# HIV  historic GF lives saved
# read in data
ls_to2016 <-
  read_excel(ls_to2016_location, sheet = "all countries" ,skip=4, col_names = TRUE, range="BD2:BT255")

# read in iso names
iso3 <- read_excel(ls_to2016_location,sheet = "all countries" , col_names = TRUE, range="D2:D255")

ls_to2016 <- cbind(iso3,ls_to2016)

ls_to2016_hiv <- ls_to2016 %>% gather(key=year,value=ls_hiv,2:18)


# TB historic GF lives saved

ls_to2016 <- read_excel(ls_to2016_location,
                        sheet = "all countries" ,skip=4, col_names = TRUE, range="BU2:CK255")

iso3 <- read_excel(ls_to2016_location,
                   sheet = "all countries" , col_names = TRUE, range="D2:D255")

ls_to2016 <- cbind(iso3,ls_to2016)

ls_to2016_tb <- ls_to2016 %>% gather(key=year,value=ls_tb.hivneg,2:18)



# mal historic GF lives saved 
ls_to2016 <- read_excel(ls_to2016_location,sheet = "all countries" ,skip=4, col_names = TRUE, range="CL2:DB255")

iso3 <- read_excel(ls_to2016_location,sheet = "all countries" , col_names = TRUE, range="D2:D255")

ls_to2016 <- cbind(iso3,ls_to2016)

ls_to2016_mal <- ls_to2016 %>% gather(key=year,value=ls_mal,2:18)

ls_to2016 <- full_join(ls_to2016_hiv,ls_to2016_tb)
ls_to2016 <- full_join(ls_to2016,ls_to2016_mal)

# # prepare 4 column structure


ls_to2016 <-ls_to2016 %>%
  mutate(year=as.numeric(year)) %>% 
  gather(key=indicatoruniqueidentifier,value=value,3:5) %>% 
  mutate(indicatoruniqueidentifier=paste0(indicatoruniqueidentifier,"_gf_hist"))# rename indicator to show it is GF data. recall that this uses historic attributive method up to and including 2016

dfw <- full_join(dfw,ls_to2016)

dfw <- dfw %>% spread(key=indicatoruniqueidentifier, value=value)

# take historic ls for up to and including 2016 then take new method from 2017
dfw <- dfw %>% 
  mutate(ls_gf_hiv=ifelse(year<=2016,ls_hiv_gf_hist,hiv_deaths_averted_goals_aim_CF_2015_KPI))  %>%
  mutate(ls_gf_tb.hivneg= ifelse(year<=2016,ls_tb.hivneg_gf_hist,tb_deaths_averted_hivneg_WHO_2000)) %>% 
  mutate(ls_gf_mal=ifelse(year<=2016,ls_mal_gf_hist,mal_deaths_averted_CF_WHO_2000))

dfw <- dfw %>% gather(key=indicatoruniqueidentifier,value = value,3:last_col())

# G. Get disbursements [from GF website] & process ######

# # # get the json file containing disbursements from the data location
`jsondoc` <- jsonlite::fromJSON(disb_location)
disb <- jsondoc$value

# # make a lookup table for regional grants

'multi_country_table' <- jsonlite::fromJSON(disb_location_multi_country)

multi_country_table <- multi_country_table$value

multi_country_table <- 
  multi_country_table %>% 
  filter(!is.na(multiCountryName)) %>%
  distinct(multiCountryName,.keep_all = TRUE) %>%
  rename_with(tolower)


if(save_disbursements==1) {
  write.csv(disb,file = paste0(out,"disbursements_unprocessed",Sys.Date(),".csv"),row.names = FALSE)
  print(paste0("A copy of unprocessed disbursements was written to",out,"disbursements_unprocessed",Sys.Date(),".csv"))
}

#  
# apply filters defined in switchboard for RR disbursements analysis
disb <- disb %>%
  filter(disbursementYear>=disb_year_start
         &disbursementYear<=disb_year_end)

disb <- disb %>%
  group_by(geographicAreaCode_ISO3,componentName,disbursementYear) %>%
  summarise_at(c("disbursementAmount"),sum,na.rm=TRUE) %>% ungroup()
# 
disb <-disb %>%
  rename(iso3=geographicAreaCode_ISO3) %>%
  rename(value=disbursementAmount) %>%
  rename(year=disbursementYear)%>%
  rename(indicatoruniqueidentifier=componentName)
# 
disb <-disb %>%
  select(iso3, year,indicatoruniqueidentifier,value)
# 
# # total disbursements to end of required period
disb %>% summarise_at(vars(value),list(~sum(.)))
# #
disb %>% group_by(indicatoruniqueidentifier) %>%
  summarise_at(vars(value),list(~sum(.))) %>% ungroup()
# # # validated vs finance input file 14 July 2023
# 
# # correct QUA for QMJ multi country western pacific (uses QMJ in Ryan's file)
# disb %>% filter(iso3=="QUA")
# 
disb <- disb %>% mutate(iso3=(ifelse(iso3=="QUA","QMJ",iso3)))

multi_country_table <- 
  multi_country_table %>% 
  mutate(geographicareacode_iso3=(ifelse(geographicareacode_iso3=="QUA","QMJ",geographicareacode_iso3)))
# # values are correct at this point 
# 
# #
# # rename disbursement names with _raw to indicate unadjusted amounts
disb[disb == "HIV"]   <-"disb_hiv_raw"
disb[disb == "Tuberculosis"]   <-"disb_tb_raw"
disb[disb == "Malaria"]   <-"disb_mal_raw"
disb[disb == "RSSH"]   <-"disb_rssh_raw"
disb[disb == "Multicomponent"]   <-"disb_rssh_raw"
disb[disb == "TB/HIV"]   <-"disb_tb_hiv_raw"
# #
# # # # make a df of unadjusted disbursements summed by year
disb <- 
  disb %>% 
  group_by(iso3,indicatoruniqueidentifier,year) %>%
  summarise_at(c("value"),sum,na.rm=TRUE) %>% 
  ungroup() %>%  
  spread(key = indicatoruniqueidentifier,value=value)
# #
disb [is.na(disb)] <-0
# #
# 
disb %>%  summarise_if(vars(is.numeric(.)),list(~sum(.)))
# # disbursement breadkdown of TB/HIV grants 
# # read in file prepared by Ryan Narciso team on 10 July 2023
# #
tb_hiv_breakdown <-
  read_excel(tb_hiv_breakdown_location,
                               sheet = "HIV-TB - Module-Specific %", range = "A7:C67", col_names = FALSE) %>%
  rename("country"=1, "HIV_specific"=2,"TB_specific"=3)
# #
# #
# also read in those countries where an 78/22 split is assumed
tb_hiv_breakdown2 <- 
  read_excel(tb_hiv_breakdown_location,
                                sheet = "HIV-TB - Module-Specific %", range = "A75:C80", col_names = FALSE)%>%
  rename("country"=1, "HIV_specific"=2,"TB_specific"=3)

 
tb_hiv_breakdown <- rbind(tb_hiv_breakdown,tb_hiv_breakdown2)
# # no duplicates at this point
# # # use the old spelling of Cabo Verde to match with lists


tb_hiv_breakdown[tb_hiv_breakdown =="Cabo Verde"] <- "Cape Verde"

# #
tb_hiv_breakdown <- left_join(tb_hiv_breakdown,lists %>% select(iso3,country))
# 
# # add the iso3 codes used for regional grants
tb_hiv_breakdown <-  
  left_join(tb_hiv_breakdown,
  multi_country_table %>% 
  select(geographicareacode_iso3,multicountryname),
  by= c("country"="multicountryname")) %>%
  mutate(iso3=ifelse(is.na(iso3),geographicareacode_iso3,iso3)) %>%
  select(-(geographicareacode_iso3))
# 

disb <-
  left_join(disb,tb_hiv_breakdown)
#
disb <- disb %>% filter(!is.na(iso3))

# disb[is.na(disb)] <-0

disb %>%  summarise_if(vars(is.numeric(.)),list(~sum(.,na.rm = TRUE)))
# matches 10 JUly 2023 finance file

# #### Add proportion indicated of TB/HIV to TB and HIV


# # to hiv first
disb <-
  disb %>% group_by(iso3,year)%>%
  mutate(disb_hiv_tot=
           ifelse(is.na(HIV_specific), disb_hiv_raw, 
           disb_hiv_raw + (disb_tb_hiv_raw*HIV_specific))) %>%
  
  #then to tb
  mutate(disb_tb_tot=
           ifelse(is.na(TB_specific), disb_tb_raw, 
           disb_tb_raw + (disb_tb_hiv_raw*TB_specific))) %>%
  
  # mal is unchanged
  mutate(disb_mal_tot=disb_mal_raw) %>%
  ungroup()

# add cumulative disbursements column for raw disbursements
disb <- disb %>% group_by(iso3) %>% 
  mutate(disb_hiv_raw_cum =cumsum(disb_hiv_raw)) %>%
  mutate(disb_tb_raw_cum =cumsum(disb_tb_raw)) %>%
  mutate(disb_tb_hiv_raw_cum  =cumsum(disb_tb_hiv_raw)) %>%
  mutate(disb_mal_raw_cum  =cumsum(disb_mal_raw))%>%
  mutate(disb_rssh_raw_cum  =cumsum(disb_rssh_raw)) %>%
  
  # and for disbursements with  TB_HIV shared out
  mutate(disb_hiv_tot_cum =cumsum(disb_hiv_tot)) %>%
  mutate(disb_tb_tot_cum =cumsum(disb_tb_tot)) %>%
  mutate(disb_mal_tot_cum  =cumsum(disb_mal_tot)) %>%
  ungroup() %>%
  select(-(contains("specific")))

# 
# # # write just disbursements for checks if needed
 write.csv(disb ,file= paste0(out,"disb_w tbhiv",Sys.Date(),".csv"))
# #
# # # gather and combine with main dataframe
disb <- 
  disb %>% 
  gather(key=indicatoruniqueidentifier,value=value,3:last_col()) %>% 
  mutate(value=as.numeric(value))
# #
# # # check that values is still unchanged from line 875
# # # first for raw values
disb %>% filter(!grepl("cum",indicatoruniqueidentifier))%>%
  filter(grepl("raw",indicatoruniqueidentifier)) %>%
  group_by(indicatoruniqueidentifier) %>%
  summarise_at(vars(value),list(~sum(.)))
# 
disb %>% filter(!grepl("cum",indicatoruniqueidentifier))%>%
  filter(grepl("raw",indicatoruniqueidentifier)) %>%
  summarise_at(vars(value),list(~sum(.)))
# 
# # at the moment TOT values don't match QMJ Multicountry western pacific code is not matching
# # QUA in disbursements file from website but QMJ in RYan's file
# 
# 
# # then for tot values which split tb /hiv  into constituent parts
disb %>% filter(!grepl("cum",indicatoruniqueidentifier))%>%
  filter(grepl("tot",indicatoruniqueidentifier) | grepl("disb_rssh_raw",indicatoruniqueidentifier)) %>%  group_by(indicatoruniqueidentifier) %>%
  summarise_at(vars(value),list(~sum(.,na.rm = TRUE)))

disb %>% filter(!grepl("cum",indicatoruniqueidentifier))%>%
  filter(grepl("tot",indicatoruniqueidentifier) | grepl("disb_rssh_raw",indicatoruniqueidentifier)) %>%
  summarise_at(vars(value),list(~sum(.,na.rm = TRUE)))

# validated vs THi Ly file

# # rbind it to pip data

dfw <- full_join(dfw,disb)
 

# # use row names to check for duplicates
row.names(dfw)<-(paste(dfw$iso3,dfw$indicatoruniqueidentifier,dfw$year))
row.names(dfw) <- NULL
 

# H. Carry out specific calculations within PIP data for incidence rates, inc rates among AGYW and AMYB, also TB cases counterfactuals ####

dfw <-
  dfw %>% 
  pivot_wider(values_from = value, names_from = indicatoruniqueidentifier) %>% 
  # note that investment case modelling used spectrum population for HIV. 
  # first calculation uses UN population division estimates # New infections in year t / (population in t-1 - infected population year **t-1***)
  mutate(hiv_inc_rate_100k= HIV1/(lag(Population1,1)-lag(HIV46,1))*10^5) %>% 
  mutate(hiv_mor_rate_100k= (HIV121/Population1)*10^5) %>% 
  # also use Spectrum population estimates 
  mutate(hiv_inc_rate_100k_spec= HIV1/(lag(Population2,1)-lag(HIV46,1))*10^5) %>% 
  mutate(hiv_mor_rate_100k_spec=(HIV121/Population2)*10^5) %>% 
  # calculate the country incidence rate TB
  mutate(tb_inc_rate_100k = (TB11/ Population1)*10^5) %>% 
  # calculate the country mortality rate TB (excl. HIV+)
  mutate(tb_mor_rate_100k = (TB58/ Population1)*10^5) %>% 
  mutate(tb_inc_rate_100k_who = (TB11/ TB67)*10^5) %>% 
  # calculate the country mortality rate TB (excl. HIV+)
  mutate(tb_mor_rate_100k_who = (TB58/ TB67)*10^5) %>% 
  
  # calculate the malaria country incidence rate per country and year
  mutate(mal_inc_rate_1k=(Malaria1/ (Malaria32)*10^3)) %>% 
  # calculate the country mortality rate malaria
  mutate(mal_mor_rate_100k= (Malaria7/ Malaria32)*10^5) %>% 
  # calculate HIV AGYW incidence rates (UNAIDS data) 
  # mutate(hiv_inc_rate_females_15_24= HIV22/(lag(Population9,1)- lag(HIV67,1))  *10^5) %>% # females
  # mutate(hiv_inc_rate_males_15_24=HIV19/(lag(Population10,1)- lag(HIV64,1))*10^5) %>%  # males
  # Calculate TB infections averted CF 2000 (not provided by WHO) but using WHO population#
  group_by(iso3) %>% 
  mutate(helper=ifelse(year==2000,tb_inc_rate_100k_who,NA)) %>% 
  fill(helper, .direction="updown") %>%  
  mutate(tb_cases_averted_CF_2000_not_WHO=(TB67/10^5* helper)-TB11) %>%
  select(-(helper)) %>% 
  ungroup() %>% 
  # Add specific indicators needed for RR
  # for results report 2020 add malaria populations back calculated from malaria coverage
  # Number of people that slept under an insecticide-treated net (modelled) 
  mutate(Malaria186_no=Malaria186/100*Malaria32) %>% 
  # Number of people with access to an ITN (modelled) 
  mutate(Malaria189_no=Malaria189/100*Malaria32) %>% 
  # for results report 2020 MDR coverage graph add new variables
  mutate(test_cov__newcases= TB522/ TB519 ) %>%
  mutate(test_cov__rtxcases=TB523/TB520) %>% 
  mutate(mdr_tx_cov_det=TB85/TB84) %>% 
  pivot_longer(values_to = "value", names_to = "indicatoruniqueidentifier",3:last_col())      


# 

#### K. Add specific indicators needed for RR
# # for results report 2020 add malaria populations back calculated from malaria coverage
# # Number of people that slept under an insecticide-treated net (modelled) 
# dfw <- dfw %>% mutate(Malaria186_no=Malaria186/100*Malaria32)
# 
# # Number of people with access to an ITN (modelled) 
# dfw <- dfw %>% mutate(Malaria189_no=Malaria189/100*Malaria32)
# 
# # for results report 2020 MDR coverage graph add new variables
# dfw <- dfw %>% mutate(test_cov__newcases= TB522/ TB519 ) %>% mutate(test_cov__rtxcases=TB523/TB520) %>% mutate(mdr_tx_cov_det=TB85/TB84)


# I. Add eligibility data for specified year and eligible ever data ####
# also add the 2021 eligibilty data
eligibility <- read_excel(eligiblity_location,sheet = 3)
colnames(eligibility) <- tolower(colnames(eligibility))

# correct the iso3 code for Zanzibar
eligibility[eligibility=="ZAN"] <- "QNB"

eligibility <- full_join(full_join(
  eligibility %>% filter(`eligibility list year`==eligibility_year) %>%  select(iso3, `disease component`,eligibility) %>% 
    filter(`disease component`=="HIV/AIDS") %>% mutate(hiv_eligibility_2021=ifelse(eligibility!="No",1,NA)) %>% 
    select(-(c(eligibility,`disease component`))),
  
  eligibility %>% filter(`eligibility list year`==eligibility_year) %>%  select(iso3, `disease component`,eligibility) %>% 
    filter(`disease component`=="TB") %>% mutate(tb_eligibility_2021=ifelse(eligibility!="No",1,NA)) %>% 
    select(-(c(eligibility,`disease component`)))
),
eligibility %>% filter(`eligibility list year`==eligibility_year) %>%  select(iso3, `disease component`,eligibility) %>% 
  filter(`disease component`=="Malaria") %>% mutate(mal_eligibility_2021=ifelse(eligibility!="No",1,NA)) %>% 
  select(-(c(eligibility,`disease component`)))
)
eligibility_ever <- read_excel(eligiblity_location, sheet = 3)
colnames(eligibility_ever) <- tolower(colnames(eligibility_ever))

# correct the iso3 code for Zanzibar
eligibility_ever[eligibility_ever=="ZAN"] <- "QNB"

eligibility_ever <- full_join(full_join(
  eligibility_ever %>% filter(`eligibility`!="No") %>% select(iso3,`disease component`,eligibility) %>% 
    filter(`disease component`=="HIV/AIDS" & eligibility!="No") %>% 
    select(-('disease component')) %>%
    distinct(iso3) %>% 
    mutate(hiv_eligible_ever=1),
  eligibility_ever %>% filter(`eligibility`!="No") %>% select(iso3,`disease component`,eligibility) %>% 
    filter(`disease component`=="TB" & eligibility!="No") %>% 
    select(-('disease component')) %>%
    distinct(iso3) %>% 
    mutate(tb_eligible_ever=1)
),
eligibility_ever %>% filter(`eligibility`!="No") %>% select(iso3,`disease component`,eligibility) %>% 
  filter(`disease component`=="Malaria" & eligibility!="No") %>% 
  select(-('disease component')) %>%
  distinct(iso3) %>% 
  mutate(mal_eligible_ever=1)
) %>% 
  mutate(eligible_ever=1)

eligibility <- full_join(eligibility,eligibility_ever)

dfw_all <- 
  dfw %>% 
  pivot_wider(values_from = value, names_from = indicatoruniqueidentifier) %>% 
  full_join(eligibility) %>% 
  select(-(country))

# K. Add lists data 
# add list data
dfw_all <- left_join(dfw_all,lists)

# J. Filtering and outputting steps  ####
# 1. write all indicators, all countries (not filtered for eligibility) [dfw_all] ####

write.csv(dfw_all,file=paste0(out,"dfw_all",Sys.Date(),".csv"),na = "", row.names = FALSE)

# 2. write Results report load file [dfw_rr_inds]####

# first add a marker for the list of countries that are covered by the MAP estimates (Oxford)
dfw  <- left_join(dfw_all, 
                  read.csv(rr_map_countries_list_location, sep=",", stringsAsFactors=FALSE) %>% 
                    rename(iso3=1) %>% 
                    mutate(map_country=1)) %>% 
  # also add id variable
  mutate(id=paste0(iso3,year))

# select relevant indicators and place them in the correct order for the excel template
dfw <- dfw %>% 
  select(iso3,contains("eligible"), contains("alloc1722"),
         contains("alloc2022"),contains("alloc1719"),
         
         # population_2,population_3,
         contains("final"),year,map_country,`gf_region 10`,HIV1,
         #  do not alter the order of any columns before year 
         Population1,id,Population2,
         hiv_l_saved_coh_st, tb_l_saved_coh_st,mal_l_saved_st_coh,
         contains("disb"),contains("deaths"),contains("infections"), contains("rate"), contains("cases"), 
         HIV1,
         
         HIV163, HIV121,HIV46,
         # HIV46_2,
         # hiv_neg_2,
         # hiv_neg_3,
         HIV10,HIV13, HIV130, HIV133, HIV124, # HIV218, not in pip
         # HIV219, not in pip
         HIV168, HIV163, # HIV222,not in pip
               # KPI8,
         # hiv_inc_rate_females_15_24,
         # hiv_inc_rate_males_15_24,
         HIV19, # new infections males 15-24
         HIV22,  # new infections females 15-24                        
         # hiv_mor_rate_males_15_24,
         # hiv_mor_rate_females_15_24,
         # newhivinfections10_19male,
         # newhivinfections10_19female,
         # annualaidsdeaths15_24female,
         HIV139,# annualaidsdeaths15_24male,
         HIV142,# popaged15_24male,
         Population10,# popaged15_24female,
         # Population9,# hivpop15_24male,
         HIV64,# hivpop15_24female,
         HIV67,HIV187,HIV183,HIV186,
         # HIV218,
         # HIV293,
         HIV440,HIV437,TB1,TB30,TB33,
         TB39,TB42,TB29,TB38,TB11,TB50,TB58,TB81,TB85,# TB14,# TB14	e_inc_rr_num is not published by WHO in 2021
         # TB77, e_tbhiv_prct	Estimated HIV in incident TB (percent) is not available in new PIP 14 June 2023
         TB90,TB190,TB97,TB202,TB249,TB269,                 TB285,
         TB273,TB279,TB280, TB281,TB289,TB282,TB288,TB289,TB282,TB283,TB288,TB286,TB293,TB284,
         TB287,TB522,TB519,TB523,TB520,TB269,TB270, TB271, TB272, TB273,
         # TB316, mdr_cmplt	Outcomes for MDR-TB cases: treatment completed is not available in new PIP 14 June 2023
         # TB317, mdr_cur	Outcomes for MDR-TB cases: cured
         TB82,TB83,TB84,TB339,TB67,
         Malaria1,Malaria7,Malaria28, Malaria29,Malaria30,Malaria32,# Malaria36,
         Malaria41,Malaria186,Malaria189,Malaria186_no,Malaria189_no,Malaria192,# Malaria32,
         Malaria41,Malaria39,Malaria45,# GF_ST_2022_hivi,GF_ST_2022_tbi,GF_ST_2022_mali,
         ls_gf_hiv,ls_gf_tb.hivneg,ls_gf_mal)
# year
ifelse(colnames(dfw)[18]=="year","year is OK","year is not in correct column")

# # id
# colnames(dfw)[23]
ifelse(colnames(dfw)[23]=="id","id is OK","id is not in correct column")


#  hardcode the values used for India tb treatment success rate for the results report file ony #####

# Note: cells highlighted yellow for TB India treatment success have been adjusted based on country-published data to replace WHO TB program data which is suspected to include some private sector cases
# the adjustment needs to be made after reviewing the data each year


# 29 June 2023 hardcoding for india stopped as no longer necessary
# NOTE THAT THESE ADJUSTMENTS ARE NOT MADE TO THE DFW ALL FILE

# dfw <- dfw %>% mutate(TB285=ifelse(iso3=="IND"&year==2019, 1708666,TB285)) %>%
#   mutate(TB285=ifelse(iso3=="IND"&year==2018,1566623,TB285)) %>%
#   mutate(TB285=ifelse(iso3=="IND"&year==2017,1381793,TB285)) %>%
#   mutate(TB285=ifelse(iso3=="IND"&year==2016,1446967,TB285)) %>%
#   mutate(TB285=ifelse(iso3=="IND"&year==2015,1410263,TB285)) %>%
#   mutate(TB285=ifelse(iso3=="IND"&year==2014,1267532,TB285)) %>%
#   mutate(TB289=ifelse(iso3=="IND"&year==2019, 1415096,TB289)) %>%
#   mutate(TB289=ifelse(iso3=="IND"&year==2018,1323298,TB289)) %>%
#   mutate(TB289=ifelse(iso3=="IND"&year==2017,1122223,TB289)) %>%
#   mutate(TB289=ifelse(iso3=="IND"&year==2016,1145065,TB289)) %>% 
#   mutate(TB289=ifelse(iso3=="IND"&year==2015,1230938,TB289)) %>%
#   mutate(TB289=ifelse(iso3=="IND"&year==2014,1124503,TB289)) %>%
#   mutate(TB249=ifelse(iso3=="IND"&year==2019,82.8171802,TB249)) %>%
#   mutate(TB249=ifelse(iso3=="IND"&year==2018,84.4681841,TB249)) %>%
#   mutate(TB249=ifelse(iso3=="IND"&year==2017,81.2149866,TB249)) %>%
#   mutate(TB249=ifelse(iso3=="IND"&year==2016,79.1355297,TB249)) %>%
#   mutate(TB249=ifelse(iso3=="IND"&year==2015,87.284308,TB249)) %>%
#   mutate(TB249=ifelse(iso3=="IND"&year==2014,88.7159796,TB249))

# also add in list of countries to feature in top 10 individual country charts and filter to eligible ever

#TB

dfw <- full_join(dfw,
                 read_excel(rr_top_10s_location) %>% 
                   select(TB_10_list) %>%
                   rename(iso3=TB_10_list) %>%
                   mutate(TB_10_list=1)) 

#malaria 
dfw <- full_join(dfw,
                 read_excel(rr_top_10s_location) %>% 
                   select(Malaria_10_list) %>%
                   rename(iso3=Malaria_10_list) %>% 
                   mutate(Malaria_10_list=1))

#HIV/AIDS
dfw <- full_join(dfw,
                 read_excel(rr_top_10s_location) %>% 
                   select(HIV_10_list) %>%
                   rename(iso3=HIV_10_list) %>% 
                   mutate(HIV_10_list=1))


dfw <- dfw %>% 
  filter(eligible_ever==1) %>% 
  filter(!is.na(iso3))

# write.csv(dfw %>% filter(eligible_ever==1),file=paste0(out,"dfw_eligible",Sys.Date(),".csv"),na = "", row.names = FALSE)
write.csv(dfw,file=paste0(out,"dfw_rr_inds",Sys.Date(),".csv"),na = "", row.names = FALSE)


# 3. write load file for allocation qualitative factors adjustment process [optional] ####
# write.csv(dfw %>% select(id,iso3,year,HIV1,HIV121,Malaria1,Malaria32,Malaria7,Population1,TB11,TB50,TB58,population_2,
# HIV46_2,
# hiv_neg_2,
# hiv_inc_rate_100k,hiv_mor_rate_100k,tb_inc_rate_100k,tb_mor_rate_100k,mal_inc_rate_1k,mal_mor_rate_100k),file=paste0(out,"dfw_qa_",Sys.Date(),".csv"),na = "")
# 4. Write load file for Country Results Profile and SSD counterfactuals ####
# # # 
# write.csv(dfw_all %>%
#             filter(eligible_ever==1) %>%
#             select(iso3,year,
#                    infections_averted_a_g_CF_2000_cpr,deaths_averted_a_g_aim_CF_2000_cpr,
#                    tb_cases_averted_CF_2000_not_WHO,tb_deaths_averted_hivneg_WHO_2000,
#                    mal_cases_averted_CF_WHO_2000,mal_deaths_averted_CF_WHO_2000) %>%
#             filter(year>=2000&year<=latest_hiv_year),file=paste0(out,"dfw_CF for SSD",Sys.Date(),".csv"), na="",row.names = FALSE)


##### for results report 2021 produce a file for interactive chart with infections / deaths averted according to results report methodology as well as actuals

# use script rr_2021_interactive_figure_cov.R
