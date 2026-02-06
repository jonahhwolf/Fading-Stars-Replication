library(tidyverse)
library(this.path)

setwd(dirname(this.path()))
setwd("../../")
# clear all
# set more off 
# pause off
# set logtype text
# 
# *************** DESCRIPTION ******************************************
# * Aggregates BEA dataset to desired granularity and computes key fields
# *
# * Version 1.0
# * Last edit: 1/12/2018 
# *****************************************************************
# 
# * Map segments
# use Temp/bea2industry, clear
# drop mneind_naics
# merge 1:m beacode using "0_Inputs/US_BEA_Main/loaded/BEA_industry_raw"
# drop _m
# Load BEA to industry mapping

bea_mapping <- read_csv("Temp/bea2industry.csv") |>
  select(-mneind_naics)

# Merge with BEA industry raw data
bea_data <- read_csv("0_Inputs/US_BEA_Main/loaded/BEA_industry_raw.csv")

bea_mapped <- bea_mapping |>
  left_join(bea_data, by = "beacode")

# * Aggregate
# ds beacode ind_short sector empsector_indicator year nonov_ind, not
# foreach X of varlist `r(varlist)' {
# 	rename `X' t`X'
# 	egen `X' = sum(t`X'),by(ind_short year) missing
# 	drop t`X'
# }
# bys ind_short year: keep if _n == 1
#
# * Compute fields
# g aa1_pgo = aa1_go/aa1_goq * 100
# 
# order ind_short year 
# sort  ind_short year 
# rename ind_short indcode
# compress
# saveold Temp/BEA_mapped, replace
# 
bea_mapped <- bea_mapped |>
  group_by(ind_short, year) |>
  summarise(
    across(starts_with("aa"), sum),
    across(everything(), first),
    .groups = "drop"
  ) |>
  # * Compute fields
  # g aa1_pgo = aa1_go/aa1_goq * 100
  mutate(aa1_pgo = aa1_go/aa1_goq * 100)
  
# order ind_short year
# sort  ind_short year
# rename ind_short indcode
# compress
# saveold Temp/BEA_mapped, replace
bea_mapped <- bea_mapped |>
  arrange(ind_short, year) |>
  rename(indcode = ind_short)

bea_mapped |>
  fwrite("Temp/BEA_mapped.csv")

# 
# /* --------*/
# /* 	 TEST  */
# /* ------- */
# use Temp/BEA_mapped,clear
# * individual values
# g test1 = (aa1_go - 964.913)    if indcode == "Health_hospitals" & year == 2014
# g test2 = (aa1_ftpt - 2329)     if indcode == "Construction" & year == 1948
# g test3 = (aa1_ftpt - 125)      if indcode == "Min_oil_and_gas" & year == 2000
# g test4 = (aa1_go - 21.683)    	if indcode == "Dur_transp" & year == 1948	// aggregated industry
# g test5 = (aa1_goq - 340.680)   if indcode == "Min_oil_and_gas" & year == 2017	
# * prices
# g test6 = (aa1_pgo - 105.157) if indcode == "Retail_trade" & year == 2012

bea_mapped |>
  filter(indcode == "Health_hospitals" & year == 2014) |>
  select(aa1_go) |>
  first() == 964.913

bea_mapped |>
  filter(indcode == "Dur_transp" & year == 1948) |>
  select(aa1_go) |>
  first() == 21.683

bea_mapped |>
  filter(indcode == "Retail_trade" & year == 2012) |>
  select(aa1_pgo) |>
  first() |>
  round() == 105

rm(bea_mapped)

# egen test7 = max(aa1_pgo - 100) if year == 2009
# * totals
# egen totgo = sum(aa1_go*nonov), by(year )
# g test8 = (22753.957 - totgo)/totgo if year == 2010
# egen totgo_sec = sum(aa1_go*empsector_indicator), by(year )
# g test9 = (22753.957 - totgo_sec)/totgo_sec if year == 2010
# * Emp
# egen totemp = sum(aa1_ftpt*nonov), by(year )
# g test10 = (totemp- 107798)/totemp if year == 1997
# g test11 = (totemp- 125610)/totemp if year == 2016
# su test*
# * Auto BREAK
# collapse (mean) test*
# egen tt = rowtotal(test*)
# if abs(tt[1])>0.001 BREAK
# drop test* 
# pause
