# clear all
# set more off 
# pause off
# set logtype text
# 
# *************** DESCRIPTION **********************************************
# * Maps CRSP to Compustat and computes MV using CRSP for matched firms
# *
# * Version 1.0
# * Last edit: 1/11/2018 
# *************************************************************************   
# 
# /* --------------------------------- */ 
# /* 	  LOAD CRSP AND MAP TO CPSTAT	 */ 
# /* --------------------------------- */ 
# 
# * load crsp
# use "NA_Compustat_Annual/raw/crsp_msf.dta", clear
# keep permno permco date prc shrout
# *isid permno date
# drop if prc == .
# rename permno lpermno
# save tempcrsp.dta, replace
library(data.table)

crsp <- read.csv("raw/crsp_msf.csv") |>
  filter(!is.na(prc)) |>
  rename(lpermno = permno)

fwrite(crsp, "./raw/tempcrsp.csv")
# * map to compustat
# use gvkey linkdt linkenddt lpermno if ~missing(lpermno) using "NA_Compustat_Annual/raw/ccmxpf_linktable.dta", clear
# bys lpermno linkdt (linkenddt): keep if _n == _N
# rangejoin date linkdt linkenddt using tempcrsp, by(lpermno)
# drop if missing(date)
# keep gvkey date lpermno prc shrout
# duplicates drop
# 
# * Generate year and month variables
# gen year = year(date)
# gen month = month(date)
# sort gvkey year month
# 
# * ensure single map per permno
# preserve
# 	keep gvkey lpermno year month 
# 	isid gvkey lpermno year month 
# restore
# save tempcrsp, replace
# 
# **
# 
# /* -------------------- */ 
# /* 		COMPUTE MV		*/ 
# /* -------------------- */ 
# 
# * compute MV
# replace prc = -prc if prc < 0
# g market_value = prc*shrout
# egen me_crsp = sum(market_value), by(gvkey year month) missing
# keep if me_crsp ~= .
# sort gvkey year month
# by gvkey year month: keep if _n == _N
# replace me_crsp = me_crsp/1000
# lab var me_crsp "Market value of Equity (m$, from CRSP)"
# 
# * finalize
# keep gvkey lpermno year month me_crsp 
# order gvkey lpermno year month me_crsp 
# save "NA_Compustat_Annual/loaded/crsp_cpstat_mv.dta", replace
# erase tempcrsp.dta
# 
