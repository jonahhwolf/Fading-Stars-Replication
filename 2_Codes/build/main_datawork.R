library(tidyverse)
library(this.path)

setwd(dirname(this.path()))
setwd("../../")

# clear all
# set more off 
# pause off
# set logtype text
# 
# capture program drop fillback
# program define fillback
# g temp = `1' if year == `2'
# egen `1'0 = min(temp),by(`3')
# replace `1' = `1'0 if year < `2' & `1' == .
# drop temp `1'0
# end
# 
# capture program drop fillfwd
# program define fillfwd
# g temp = `1' if year == `2'
# egen `1'0 = min(temp),by(`3')
# replace `1' = `1'0 if year > `2' & `1' == .
# drop temp `1'0
# end
# 
# *************** DESCRIPTION ******************************************
# * Creates firm-level dataset for Star analyses
# **********************************************************************
# 
# /* -------------------------- */
# /* 		PREPARE COMPUSTAT	  */
# /* -------------------------- */
# 
# use "0_Inputs/NA_Compustat_Annual/loaded/NA_compustat", clear
# keep if inrange(year,1955,2017)
# tostring beacode, replace

NA_compustat <- read.csv('0_Inputs/NA_Compustat_Annual/loaded/NA_compustat.csv')

NA_compustat <- NA_compustat |>
  filter(year >= 1955 & year <= 2017) |>
  mutate(beacode = as.character(beacode))

# * BEA NAICS
# merge m:1 beacode using Temp/bea2industry, keep(master matched)
# tab naicsbea _merge if _m == 1 // USPS (491) and other (999)  --> OK
# drop _m

bea2industry <- read.csv("Temp/bea2industry.csv")

cstat_bea <- NA_compustat |>
  left_join(bea2industry, by = "beacode") |>
  filter(!is.na(beacode))

anti_join(NA_compustat, bea2industry, by = "beacode") |>
  filter(!is.na(beacode))

# * fill-in for other
# replace ind_short 	 = "Other" if naicsbea == 999
# replace sector 		 = "Other" if naicsbea == 999
# replace empsector_indicator = 1 if naicsbea == 999
# replace mneind_naics = "All" if naicsbea == 999
# * drop USPS
# drop if naicsbea == 491
# rename ind_short indcode

cstat_bea <- cstat_bea |>
  mutate(
    ind_short = ifelse(naicsbea == 999, "Other", ind_short),
#    sector = ifelse(naicsbea == 999, "Other", sector),
#    empsector_indicator = ifelse(naicsbea == 999, 1, empsector_indicator),
#    mneind_naics = ifelse(naicsbea == 999, "All", mneind_naics)
  ) |>
  filter(naicsbea != 491) |>
  rename(indcode = ind_short)
  
# 
# * SIC to MNE pre-1997
# g sicbea = sic2
# replace sicbea = sic3 if inlist(sic2,37,48)
# merge m:1 sicbea using Temp/sic2mne, keep(master matched)
# tab sicbea if _m == 1	// other (99)
# drop _m
# replace mneind_sic = "All" if sicbea == 99
# replace mneind_sic = "TCU" if inlist(mneind_sic,"Communications","Electric, gas, and sanitary services","Transportation") & year <= 1988
# replace mneind_sic = "CU" if inlist(mneind_sic,"Communications","Electric, gas, and sanitary services") & inrange(year,1989,1993)
# save tempfirm, replace
cstat_bea <- cstat_bea |>
  mutate(sicbea = ifelse(sic2 %in% c(37, 48), sic3, sic2))

# 
# **
# 
# /* ---------------------------- */
# /* 		BEA (PRICES + PROD)		*/
# /* ---------------------------- */
# 
# *industry
# merge m:1 indcode year using "Temp/BEA_mapped", keep(master matched) keepusing(aa1_go* aa1_pgo aa1_ftpt)
# tab indcode if _m == 1
# drop _m
# save tempfirm, replace
# 
# *sector
# use "Temp/BEA_mapped",clear
# keep if empsector_indicator == 1
# keep sector year aa1_goq aa1_ftpt aa1_pgo
# rename aa1_goq aas_goq 
# rename aa1_pgo aas_pgo 
# rename aa1_ftpt aas_ftpt 
# drop if aas_ftpt == .
# merge 1:m sector year using tempfirm, keep(matched using) 
# tab sector if _m == 2
# drop _m
# save tempfirm, replace
# 
# * For "Other" industry, fill in with wtd. average of private industries
# * We don't want to drop because important stars show up here (GE, Berkshire)
# use "Temp/BEA_mapped",clear
# keep if empsector_indicator == 1
# keep sector year aa1_go aa1_goq aa1_ftpt
# drop if aa1_go*aa1_goq*aa1_ftpt == .
# collapse (sum) aa1*,by(year)
# g aa_pgo = 100*aa1_go / aa1_goq 
# rename aa1_goq aa_goq 
# rename aa1_ftpt aa_ftpt 
# keep year aa_*
# merge 1:m year using tempfirm, keep(matched using) 
# drop _m
# replace aas_goq = aa_goq if indcode == "Other"
# replace aa1_pgo = aa_pgo if indcode == "Other" 
# replace aas_pgo = aa_pgo if indcode == "Other" 
# replace aas_ftpt = aa_ftpt if indcode == "Other"
# 
# **
# 
# /* ------------------------ */
# /* 		RELATIVE WAGE 		*/
# /* ------------------------ */
# 
# merge m:1 indcode year using Temp/WageAdj, keep(matched master) nogen
# 
# * Fill forward holding fixed last value
# fillfwd  aa1_wq 2012 indcode
# fillfwd  aa_wq 2012 indcode
# 
# * Set missing = wtd avg ratio 
# egen tt = min(aa_wq),by(year)
# replace aa_wq = tt if aa_wq == .
# replace aa1_wq = aa_wq if aa1_wq == . 
# drop tt 
# 
# * No adjustment before 97
# replace aa1_wq = 1 if year < 1997 
# 
# **
# 
# /* ------------------------ */
# /* 		FOREIGN SALES		*/
# /* ------------------------ */
# 
# * naics
# merge m:1 mneind_naics year using "0_Inputs/US_BEA_MNE/loaded/BEA_mne_naics",  keep(master matched) keepusing(aa1_pctfor)
# * all unmapped are banks. use post-2009 data
# tab mneind_naics  if _m == 1 & inrange(year,1999,2015) 
# drop _m
# rename aa1_pctfor aa1_pctfor_naics
# 
# * sic
# merge m:1 mneind_sic year using "0_Inputs/US_BEA_MNE/loaded/BEA_mne_sic",  keep(master matched)  keepusing(aa1_pctfor)
# tab year if _m == 1 & inrange(year,1983,1998)	 // all mapped
# drop _m
# 
# replace aa1_pctfor_naics = aa1_pctfor if aa1_pctfor_naics  == . 
# drop aa1_pctfor
# rename aa1_pctfor_naics aa1_pctfor
# 
# * fill-in pct foreign
# fillback aa1_pctfor 1983 mneind_sic
# tab mneind_naics if aa1_pctfor == . & year < 2015
# fillback aa1_pctfor 2009 mneind_naics	// banks
# fillfwd aa1_pctfor 2015 mneind_naics
# tab year if aa1_pctfor == .	// all mapped
# 
# **
# 
# /* -------------------- */
# /* 		MACRO-DATA 		*/
# /* -------------------- */
# 
# * FRED
# merge m:1 year using 0_Inputs/Fred/loaded/fred_data, nogen keep(matched master)
# save tempfirm, replace
# 
# * FERNALD TFP
# import excel "0_Inputs/data_quarterly_2018.09.07.xlsx", sheet("annual") firstrow clear case(l)
# drop if date < 1950 | date == .
# rename date year
# keep year dlp dtfp dtfp_util
# rename dlp aa_dlp
# rename dtfp aa_dtfp
# rename dtfp_util aa_dtfp_util
# 
# merge 1:m year using tempfirm, nogen keep(matched using)
# 
# ***
# 
# /* ---------------- */
# /* 		FINALIZE	*/
# /* ---------------- */
# 
# drop if gvkey == . | indcode == ""
# compress
# order gvkey year indcode sale at 
# sort gvkey year
# save  3_Final_Data/main_dataset_firm, replace
# erase tempfirm.dta
