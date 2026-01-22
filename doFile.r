# clear all 
# set more off 
# pause off
# set logtype text
# 
# *************** DESCRIPTION ******************************************
# * Creates replication results for
# * 						Fading Stars 
# * by German Gutierrez and Thomas Philippon, 2019
# * 
# * Version 1.0
# * Last edit: 1/12/2018 
# **********************************************************************
# 
# * Working directory (to be updated by user)
# cd "C:\Users\ggutierr\Dropbox\0. Research\0. Thomas\Stars\AEAPP_Submission\rep\rep\"
library(tidyverse)
library(this.path)
library(readxl)

setwd(dirname(this.path()))
# 
# /* ---------------- */
# /* 	   UTILITIES 	*/
# /* ---------------- */
# 
# capture noisily{
# ssc install rmfiles
# ssc install rangejoin
# ssc install rangestat
# ssc install egenmore
# ssc install _gwtmean   
# ssc install winsor2
# ssc install freduse
# }
# 
# 
# * CREATE DIRECTORIES
# cap mkdir Temp

if (!dir.exists("Temp")) {dir.create("Temp")}  

# * LOAD MAPPING TABLES **** 
# * naics2bea
# import excel 1_Mapping_Files/NAICS2BEA.xlsx, firstrow clear
# rename naics naicsbea
# destring naicsbea, replace force
# drop if naicsbea == .
# save Temp/naics2bea, replace
NAICS2BEA <- read_xlsx("1_Mapping_Files/NAICS2BEA.xlsx") |>
  rename(naicsbea = naics) |>
  mutate(naicsbea = as.numeric(naicsbea)) |>
  filter(naicsbea != "")
  
fwrite(NAICS2BEA, "Temp/naics2bea.csv")

# * aggregation of bea industries + mapping to MNE
# import excel 1_Mapping_Files/BEA_mapping_stars.xlsx, firstrow clear
# keep beacode ind_short sector empsector_indicator mneind_naics
# drop if beacode == ""
# bys beacode ind_short: keep if _n ==1 
# save Temp/bea2industry, replace
BEA2INDUSTRY <- read_xlsx("1_Mapping_Files/BEA_mapping_stars.xlsx") |>
  select(c(beacode, mneind_naics, ind_short, sector, empsector_indicator)) |>
  drop_na(beacode) |>
  distinct(beacode, ind_short, .keep_all = TRUE) |>
  arrange(beacode)

fwrite(BEA2INDUSTRY, "Temp/bea2industry.csv")

# * sic2mne (pre-1998)
# import excel 1_Mapping_Files/BEA_mapping_stars.xlsx, sheet("sic2mne") firstrow clear
# save Temp/sic2mne,replace
# * sic2bea (pre-1998)
# import excel 1_Mapping_Files/BEA_mapping_stars.xlsx, sheet("SIC87_2_BEA") firstrow clear
# rename sic sicbea 
# save Temp/sic87_2_bea,replace
# 
# * clear input/output directories
# rmfiles , folder("0_Inputs/Fred/loaded")
# rmfiles , folder("0_Inputs/US_BEA_Main/loaded")
# rmfiles , folder("0_Inputs/US_BEA_MNE/loaded")
# rmfiles , folder("0_Inputs/US_BEA_SIC/loaded")
# rmfiles , folder("0_Inputs/US_Census_Conc/loaded")
# rmfiles , folder("0_Inputs/NA_Compustat_Annual/loaded") 
# rmfiles , folder("3_Final_Data") 
# rmfiles , folder("4_Figures") 
# 
# 
# ***
# 
# /* -------------------- */
# /* 	   LOAD INPUTS		*/
# /* -------------------- */
# 
# cd "0_Inputs" 
# 
# * macro-data
# do Fred/fred.do
# * industry-level data
# do US_BEA_Main/BEA_dataload.do
# do US_BEA_SIC/BEA_SIC_dataload.do
# do US_BEA_MNE/mne_sic_dataload.do
# do US_BEA_MNE/mne_naics_dataload.do
# do US_Census_Conc/NAICS_dataload.do
# do US_Census_Conc/SIC_dataload.do
# * firm-level data
# do NA_Compustat_Annual/cs_download.do 
# do NA_Compustat_Annual/crsp_mv.do
# do NA_Compustat_Annual/cs_load.do
# 
# 
# ***
# 
# /* -------------------------------- */
# /* 		BUILD ANALYSIS DATASETS	 	*/
# /* -------------------------------- */
# 
# cd ".."
# do 2_Codes/build/BEA_map.do
# do 2_Codes/build/wage_adjustment.do
# do 2_Codes/build/main_datawork_census.do
# do 2_Codes/build/main_datawork.do
# 
# **
# 
# /* ------------------------ */
# /* 		GENERATE RESULTS	*/
# /* ------------------------ */
# 
# do 2_Codes/aWork_Census
# do 2_Codes/aWork_Stars
# !rmdir "Temp" /s /q 
# 
