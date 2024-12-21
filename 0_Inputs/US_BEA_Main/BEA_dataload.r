# clear all
# set more off 
# pause off
# set logtype text


library(tidyverse)
library(this.path)
library(readxl)

setwd(dirname(this.path()))
 
# *************** DESCRIPTION ******************************************
# * Loads industry-level datasets from the BEA 
# *
# * Gross output
# * 	- chained and current cost
# *	- value added
# *
# * Employment
# * 
# * The following notation is used:
# *	aa1_ = industry totals
# *
# * 	_X = current cost
# * 	_Xq = chained as of 2009
# *
# * Datasets downloaded from https://www.bea.gov/industry/industry-economic-accounts-information-guide	
# *
# * Version 1.0
# * Last edit: 1/11/2018 
# *****************************************************************
# 
# ***************************************
# capture program drop VALOAD
# program define VALOAD
# rename B va_ind
# drop if va_ind ==""
# g field = "`1'"
# order field
# ds va_ind field, not
# foreach v of var `r(varlist)'{
#    local x : variable label `v'
#    rename `v' y`x'
# }
# destring y*, replace force
# if "`1'" ~= "go" append using temp
# save temp, replace
# end
#
# ***************************************
# 
# **********************************
# ****	DEFINE SEGMENTS		******
# **********************************
# 
# import excel "US_BEA_Main/Mapping_BEA.xlsx", firstrow clear
# isid va_ind
# foreach X in  va_ind emp_ind_pre98 emp_ind_post98{
# 	replace `X' = strltrim(`X')
# }
# save mapping.dta, replace

bea_mapping <- read_xlsx("Mapping_BEA.xlsx")

if (any(duplicated(bea_mapping$va_ind))) {
  stop("va_ind is not a unique identifier")
}

bea_mapping <- bea_mapping %>%
  mutate(across(c(va_ind, emp_ind_pre98, emp_ind_post98), str_trim))

# Save processed data
write_csv(bea_mapping, "mapping.csv")

# **
# 
# **********************************
# ****		LOAD DATA 		******
# **********************************
# 
# /* ------------------------ */
# /* 		GROSS OUTPUT 		*/
# /* ------------------------ */
# 
# * LOAD
# import excel US_BEA_Main/raw/GDPbyInd_GO_1947-2017.xlsx, sheet("GO") cellrange(B6:BU95) firstrow clear
# VALOAD go

gross_output <- read_xlsx("raw/GDPbyInd_GO_1947-2017.xlsx", sheet = "GO", range = "B6:BU95")

gross_output <- gross_output |>
  rename(go = '...1')

# vaload <- function(data, field_value){
#   output <- data |>
#     rename(val_ind = B) |>
#     filter(!is.na(val_ind)) |>
#     mutate(field = field_value) |>
#     relocate(field)
#   
#   cols <- setdiff(names(output), c("val_ind", "field"))
#   
#   for (col in cols){
#     
#     new_name <- paste0("y", col)
#     
#     processed_data <- processed_data |>
#       rename(!!new_name := !!sym(col))
#   }
#   
#   output <- output |>
#     mutate(across(starts_with("y"), ~as.numeric(as.character(.))))
#   
# }

bea_mapping |>
  filter(!is.na(va_ind))

# import excel US_BEA_Main/raw/GDPbyInd_GO_1947-2017.xlsx, sheet("ChainQtyIndexes") cellrange(B6:BU95) firstrow clear
# VALOAD goq

go_q <- read_xlsx("raw/GDPbyInd_GO_1947-2017.xlsx", sheet = "ChainQtyIndexes", range = "B6:BU95")

go_q <- go_q |>
  rename(go_q = "...1")

# **
# 
# * FORMAT 
# replace va_ind = strltrim(va_ind)
# reshape long y, i(va_ind field) j(year)
# reshape wide y, i(va_ind year) j(field, string)
# sort va_ind year


 
# * redefine quantity indices based on 09 USD
# foreach X in go {
# 	g temp = y`X' if year == 2009
# 	egen y`X'09= min(temp), by(va_ind)
# 	replace y`X'q = y`X'q* y`X'09 / 100
# 	drop temp y`X'09
# }
# * rename
# ds va_ind year, not
# foreach X of varlist `r(varlist)' {
#    	replace `X' = `X'/1000 // put data in billions 
# 	local nn = substr("`X'", 2, .)
#    	rename `X' aa1_`nn'
# }
# * Merge industry codes
# merge m:1 va_ind using mapping, keepusing(beacode) 
# egen test1 = sum(_m==3)    // 77 industries x 71 years
# replace test1 = test1-5467
# su test*
# if abs(`r(mean)')>0.001 BREAK
# drop test* _m
# pause
# 
# * clean
# drop if beacode == "" // more granular industries reported in some but not all BEA accounts
# drop if year == . 
# drop if inlist(va_ind,"Hospitals","Nursing and residential care facilities") // keep aggregated because available over longer period
# drop va_ind 
# 
# * aggregate (applies only for 3360)
# ds beacode year, not
# foreach X of varlist `r(varlist)' {
# 	rename `X' t`X'
# 	egen `X' = sum(t`X'), by(beacode year) missing
# 	drop t`X'
# }
# bys beacode year: keep if _n == 1
# sort beacode year
# isid beacode year
# save temp_go, replace
# 
# **
# 
# /* ------------------------ */
# /* 		   EMPLOYMENT 		*/
# /* ------------------------ */
# * NOTE: We are combining two vintages of employment data (A, covering 1948-1997 
# * from BEA; B, covering 1998-2017). There appear to be no jumps
# * We use sectoral employment because it is the only available before 1977
# 
# * PRE-1998 
# import excel US_BEA_Main/raw/GDPbyInd_FTPT_1948-1997.xls,  sheet("1948-97_97NAICS_FTPT Employees") cellrange(B1:AZ83) firstrow clear case(l)
# drop if ind == ""
# ds ind , not
# foreach v of var `r(varlist)'{
#    local x : variable label `v'
#    rename `v' y`x'
# }
# destring y*, replace force
# reshape long y, i(ind ) j(year)
# rename y ftpt
# rename ind emp_ind_pre98
# replace emp = strtrim(emp)
# save temp, replace
# 
# use mapping,clear
# drop if emp_ind_pre98 == ""
# keep beacode emp_ind_pre98 
# merge 1:m emp_ind_pre98 using temp 
# *test
# egen test1 = sum(_m==1)    	// all industries in mapping should map
# su test1					
# if abs(`r(mean)')>0.001 BREAK
# egen test2 = sum(_m==2)    
# su test2
# if abs(`r(mean)'-300) > 0.001 BREAK // 6 aggregated sectors x 50 years don't map (e.g., all industries)
# drop test* 
# pause
# 
# keep if _m == 3
# drop if beacode == ""
# drop emp_ind_pre98 _m
# 
# ds beacode year, not
# foreach X of varlist `r(varlist)' {
# 	rename `X' t`X'
# 	egen `X' = sum(t`X'), by(beacode year) missing
# 	drop t`X'
# }
# bys beacode year: keep if _n == 1
# sort beacode year
# save temp_emp, replace
# 
# ***
# 
# * POST-1998: NIPA table 604 
# * full-time and part-time employees
# import excel US_BEA_Main/raw/Section6All.xlsx,  sheet("T60400D-A") cellrange(B8:W93) firstrow clear case(l)
# rename b emp_ind_post98
# drop if inlist(c,"N4236C","N4237C")	// sub-categories of wholesale trade. Not used and same name as dur/nondur mfg.
# drop c
# ds emp_ind , not
# foreach v of var `r(varlist)'{
#    local x : variable label `v'
#    rename `v' y`x'
# }
# 
# * harmonize industry names
# replace emp = strtrim(emp)
# reshape long y, i(emp_ind) j(year)
# rename y ftpt
# sort emp year
# save temp, replace
# 
# * map industries
# use mapping,clear
# drop if emp_ind_post98 == ""
# keep beacode emp_ind_post98 
# replace emp = strtrim(emp)
# merge 1:m emp_ind_post98 using temp 
# *test
# egen test1 = sum(_m==1)    	// all industries in mapping should map
# su test1					
# if abs(`r(mean)')>0.001 BREAK
# egen test2 = sum(_m==2)    
# su test2
# if abs(`r(mean)'-140) > 0.001 BREAK // 7 granular industries x 20 years don't map (e.g., all industries)
# drop test* 
# 
# keep if _m == 3
# drop emp_ind_ _m
# * aggregate
# ds beacode year, not
# foreach X of varlist `r(varlist)' {
# 	rename `X' t`X'
# 	egen `X' = sum(t`X'), by(beacode year) missing
# 	drop t`X'
# }
# bys beacode year: keep if _n == 1
# sort beacode year
# append using temp_emp
# rename ftpt aa1_ftpt
# isid beacode year
# save temp_emp, replace
# 
# **
# 
# /* --------------------- */
# /* 		FINALIZE 	 	 */
# /* --------------------- */
# 
# use temp_go, clear
# merge 1:1 beacode year using temp_emp, nogen
# save temp, replace
# 
# * fill-in non-overlapping id
# use mapping,clear
# collapse (max) nonov, by(beacode)
# merge 1:m beacode using temp,nogen keep(matched) 
# 
# order beacode year  
# sort  beacode year 
# compress
# saveold US_BEA_Main/loaded/BEA_industry_raw, replace
# erase temp.dta
# erase temp_go.dta
# erase temp_emp.dta
# erase mapping.dta
# 
# ***
# 
# 
# /* ---------------- */
# /* 		TEST		*/
# /* ---------------- */
# * Mean = zero
# use US_BEA_Main/loaded/BEA_industry_raw, clear
# 
# *** individual values
# g test1 = (aa1_go - 964.913)    if beacode == "6220" & year == 2014
# g test2 = (aa1_ftpt - 2329)     if beacode == "2300" & year == 1948
# g test3 = (aa1_ftpt - 125)      if beacode == "2110" & year == 2000
# g test4 = (aa1_go - 21.683)    	if beacode == "3360" & year == 1948	// aggregated industry
# g test5 = (aa1_goq - 340.680)   if beacode == "2110" & year == 2017	
# egen test6 = max(aa1_go - aa1_goq )    if year == 2009
# 
# **** totals
# egen totgo = sum(aa1_go*nonov), by(year )
# g test7 = (22753.957 - totgo)/totgo if year == 2010
# * Emp
# egen totemp = sum(aa1_ftpt*nonov), by(year )
# g test8 = (totemp- 107798)/totemp if year == 1997
# g test9 = (totemp- 125610)/totemp if year == 2016
# su test*
# pause
# * Auto BREAK
# collapse (mean) test*
# egen tt = rowtotal(test*)
# if abs(tt[1])>0.001 BREAK
