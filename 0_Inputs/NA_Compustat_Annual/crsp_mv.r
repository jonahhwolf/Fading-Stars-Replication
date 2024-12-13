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
library(tidyverse)
library(this.path)

setwd(dirname(this.path()))

crsp <- read.csv("raw/crsp_msf.csv") |>
  filter(!is.na(prc)) |>
  rename(lpermno = permno)

anyDuplicated(crsp)

# * map to compustat
# use gvkey linkdt linkenddt lpermno if ~missing(lpermno) using "NA_Compustat_Annual/raw/ccmxpf_linktable.dta", clear
# bys lpermno linkdt (linkenddt): keep if _n == _N
# rangejoin date linkdt linkenddt using tempcrsp, by(lpermno)
# drop if missing(date)
# keep gvkey date lpermno prc shrout
# duplicates drop

linktable <- read.csv("raw/ccmxpf_linktable.csv") |>
  filter(!is.na(lpermno)) |>
  select(gvkey, linkdt, linkenddt, lpermno) |>
  arrange(lpermno, linkdt, desc(linkenddt)) |>
  group_by(lpermno) |>
  slice_tail(n = 1) |>
  ungroup()

crsp_cpstat <- inner_join(crsp, linktable, by = "lpermno") |>
  filter(date >= linkdt & date <= linkenddt) |>
  select(gvkey, date, lpermno, prc, shrout) |>
  distinct()

rm(crsp, linktable)

# * Generate year and month variables
# gen year = year(date)
# gen month = month(date)
# sort gvkey year month

crsp_cpstat <- crsp_cpstat |>
  mutate(year = year(date), month = month(date)) |>
  arrange(gvkey, year, month)

# * ensure single map per permno
# preserve
# 	keep gvkey lpermno year month
# 	isid gvkey lpermno year month
# restore
# save tempcrsp, replace
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
crsp_cpstat |>
  mutate(
    prc = abs(prc),
    market_value = prc * shrout
  ) |>
  group_by(gvkey, year, month) |>
  filter(n() > 1)

crsp_cpstat_mv <- crsp_cpstat |>
  mutate(
    prc = abs(prc),
    market_value = prc * shrout
    ) |>
  group_by(gvkey, year, month) |>
  summarize(me_crsp = sum(market_value, na.rm = TRUE)) |>
  filter(!is.na(me_crsp)) |>
  slice_tail(n = 1) |>
  mutate(me_crsp = me_crsp / 1000) |>
  ungroup()

rm(crsp_cpstat)

# * finalize
# keep gvkey lpermno year month me_crsp 
# order gvkey lpermno year month me_crsp 
# save "NA_Compustat_Annual/loaded/crsp_cpstat_mv.dta", replace
# 
fwrite(crsp_cpstat_mv, "loaded/crsp_cpstat_mv.csv")

rm(crsp_cpstat_mv)

gc()