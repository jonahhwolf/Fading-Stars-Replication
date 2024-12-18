## Creates Compustat Firm-level dataset
## - loads datasets
## - computes basic firm-level variables
## - maps firms to industry segments
library(tidyverse)
library(this.path)
library(DescTools)
library(data.table)
library(readxl)

setwd(dirname(this.path()))

## DATA LOADING

## FUNDA
# use NA_Compustat_Annual/raw/funda.dta, clear
funda <- read.csv("./raw/funda.csv", nrows = 10000)

## COMPANY

# merge m:1 gvkey using NA_Compustat_Annual/raw/company, nogen keep(matched master) keepusing(loc sic naics)
# duplicates drop gvkey fyear, force
company <- read.csv("./raw/company.csv") |>
  select(c(gvkey, loc, sic, naics))

company_funda <- funda |>
  left_join(company, relationship = "many-to-one", by = "gvkey") |>
  distinct(gvkey, fyear, .keep_all = TRUE)

rm(company, funda)

## CRSP-COMPUSTAT MV
## by december of fiscal year (same as prcc_c in compustat)
# rename fyear year
# g month = 12

company_funda <- company_funda |>
  rename(year = fyear) |>
  mutate(month = 12)

# merge 1:m gvkey year month using NA_Compustat_Annual/loaded/crsp_cpstat_mv, nogen keep(matched master)
crsp_cpstat_mv <- read.csv("loaded/crsp_cpstat_mv.csv")

# rename me_crsp me_crsp_dec
# drop month
tempcpstat <- company_funda |>
  left_join(crsp_cpstat_mv, by = c("gvkey", "year", "month")) |>
  filter(!is.na(gvkey)) |>
  rename(me_crsp_dec = me_crsp) |>
  select(-month)

rm(company_funda, crsp_cpstat_mv)

# # DATA CLEANING
# 
# destring , replace
# g datayear = year(datadate)
# 
# # Apply most basic data filters (additional filters in analysis file)
# drop if year==.
# drop if gvkey==.
# keep if year <= 2017
# 
# drop if gvkey == 4828 & year == 2001 // DELHAIZE AMERICA INC. Severe issue with csho right before exit.
# g test_totct = _N
tempcpstat <- tempcpstat |>
  mutate(datayear = lubridate::year(datadate)) |>
  filter(!is.na(year) & !is.na(gvkey)) |>
  filter(year <= 2017) |>
  filter(!(gvkey == 4828 & year == 2001))

# # FIRM-LEVEL FIELDS
# 
# sort gvkey year
# xtset gvkey year

tempcpstat <- tempcpstat |>
  arrange(gvkey, year)

# # BASIC FINANCIALS
# 
# # MARKET VALUE OF EQUITY: two ways of calculating it
# # 1. crsp-compustat merged (see above)
# # 2. calendar year-end price (note inconsistent timing of shares & price if fyr ~= 12)
# #    prcc_c is the price as of Dec of the FISCAL year. So, for instance, if a    
# #    company's fiscal year end falls b/w jan and may 1990, it's fyear = 1987 and 
# #    prcc_c will be the price as of Dec 1989. However, if the fiscal year end 
# #    falls b/w june and dec 1990, fyear = 1990 and prcc_c is price as of Dec 1990  
# # 	 Csho is ALWAYS  as of fiscal year-end           
# g me_cpstat_dec = csho*prcc_c	 				
# 
# # COMPARE CRSP AND COMPUSTAT 
# # Some differences, although for manu firms the figures are exact in some years, 
# # which suggests the differences are due to raw datasets. This is likely for two reasons: 
# #	a/ 	Compustat reports price for 'main' issue, and we assume it applies for all 
# #		issues (included in csho)
# #	b/ 	Prices differ in some cases, perhaps due to different dates. 
# # Per WRDS team, small impact on results.
# g test_me = me_cpstat_dec/me_crsp_dec-1
# su test_me,det
# drop test_me
# pause

tempcpstat <- tempcpstat |>
  mutate(me_cpstat_dec = csho * prcc_c, test_me = me_cpstat_dec/me_crsp_dec - 1)

summary(tempcpstat$test_me)

tempcpstat <- tempcpstat |>
  select(-test_me)

# # Calendar year-end MVE: Use CRSP if available, else Compustat
# g me = me_crsp_dec 
# replace me = me_cpstat_dec if me == .
# replace me = . if me == 0
# drop me_*
# 
# g mv = me + dltt

tempcpstat <- tempcpstat |>
  mutate(me = ifelse(is.na(me_crsp_dec), me_cpstat_dec, me_crsp_dec), mv = me + dltt) |>
  select(!starts_with("me_"))

# # Profitability
# g ps = oiadp / sale
# replace ps =  -1 if ps < -1
# winsor2 ps, replace cuts(2 98) by(year) 
# 
# label variable me "Market value of equity (Dec of fiscal year)"
# label variable mv "Market Value"
# label variable ps "Profit share (OIADB/SALE)"
# 
# save tempcpstat, replace

tempcpstat <- tempcpstat |>
  group_by(year) |>
  mutate(
    ps = oiadp / sale,
    ps = ifelse(ps < -1, -1, ps),
    ps = Winsorize(ps, val = quantile(ps, probs = c(0.02, 0.98), na.rm = TRUE))) |>
  ungroup()
 
# # ADD SEGMENTATION DIMENSIONS
# 
# # SIC
# 
# tostring sic, g(strsic)
# replace strsic = "0"+ strsic if strlen(strsic) == 3
# forvalues ii = 2(1)4 {
# 	g sic`ii' = substr(strsic,1,`ii')
# }
# destring sic*,replace
# drop strsic

tempcpstat <- tempcpstat |>
  mutate(
    sic2 = sic %/% 100,
    sic3 = sic %/% 10,
    sic4 = sic
    )

# # NAICS
# 
# # Compute NAICS 2-6
# tostring naics, g(naicsstr)
# replace naicsstr = "" if naicsstr == "."
# forvalues X = 2(1)6 {
# 	g naics`X' = substr(naicsstr,1,`X')
# 	replace naics`X' = "" if length(naics`X') ~= `X'
# }
# destring naics*, replace
# drop naicsstr
# compress
# sort gvkey year
# save tempcpstat, replace

tempcpstat <- tempcpstat |>
  mutate(
    naics2 = substr(naics, 1, 2),
    naics3 = substr(naics, 1, 3),
    naics4 = substr(naics, 1 ,4),
    naics5 = substr(naics, 1, 5),
    naics6 = substr(naics, 1, 6),
    across(starts_with("naics"), as.numeric)
  ) |>
  arrange(gvkey, year)

# # MAP NAICS TO NAICS 2007
# # Compustat reports an inconsistent NAICS hierarchy.
# # We map deleted codes to the corresponding, most common 2007 NAICS-4 in the hierarchy
# # We do not map to more granular NAICS because we do not use them, and often the 
# # map is not one-to-one.
# 
# # import concordances 
# # from https://www.census.gov/eos/www/naics/concordances/concordances.html
# import excel "NA_Compustat_Annual/raw/NAICS_Concordances/1997_NAICS_to_2002_NAICS.xls", sheet("Concordance 23 US NoD") firstrow clear case(l) allstring
# keep naics97 naics02
# rename naics97 naics_pre
# rename naics02 naics_post
# drop if naics_pre == ""
# egen constant_code = max(naics_pre == naics_post),by(naics_pre)
# keep if constant_code == 0
# save naics9702,replace

naics9702 <- read_excel("raw/NAICS_Concordances/1997_NAICS_to_2002_NAICS.xls", sheet = "Concordance 23 US NoD")

naics9702 <- naics9702 |>
  select(naics_pre = NAICS97, naics_post = NAICS02) |>
  # Remove rows with empty naics_pre
  filter(naics_pre != "") |>
  # Group by naics_pre and check if any codes are constant
  group_by(naics_pre) |>
  # Keep only rows where the code is not constant
  filter(! all(naics_pre == naics_post)) |>
  # Optional: ungroup to remove grouping
  ungroup()

fwrite(naics9702, "naics9702.csv")

# import excel "NA_Compustat_Annual/raw/NAICS_Concordances/2002_to_2007_NAICS.xls", sheet("02 to 07 NAICS U.S.") cellrange(A3:D1203) firstrow case(l) allstring clear
# keep naicscode c
# rename naicscode  naics_pre
# rename c naics_post
# drop if naics_pre == ""
# egen constant_code = sum(naics_pre == naics_post),by(naics_pre)
# keep if constant_code == 0
# save naics0207, replace

naics0207 <- read_excel("raw/NAICS_Concordances/2002_to_2007_NAICS.xls", sheet = "02 to 07 NAICS U.S.", range = "A3:D1203")

naics0207 <- naics0207 |>
  select(naics_pre = 1, naics_post = 3) |>
  # Remove rows with empty naics_pre
  filter(naics_pre != "") |>
  # Group by naics_pre and check if any codes are constant
  group_by(naics_pre) |>
  # Keep only rows where the code is not constant
  filter(! all(naics_pre == naics_post)) |>
  # Optional: ungroup to remove grouping
  ungroup()
  
fwrite(naics0207, "naics0207.csv")

# import excel "NA_Compustat_Annual/raw/NAICS_Concordances/2012_to_2007_NAICS.xls", sheet("2012 to 2007 NAICS U.S.") cellrange(A3:G1187) firstrow allstring case(l) clear
# keep naicscode c
# rename naicscode  naics_pre 
# rename c naics_post
# drop if naics_pre == ""
# egen constant_code = sum(naics_pre == naics_post),by(naics_pre)
# keep if constant_code == 0
# save naics1207, replace

naics1207 <- read_excel("raw/NAICS_Concordances/2012_to_2007_NAICS.xls", sheet = "2012 to 2007 NAICS U.S.", range = "A3:G1187")

naics1207 <- naics1207 |>
  select(naics_pre = 3, naics_post = 1) |>
  # Remove rows with empty naics_pre
  filter(naics_pre != "") |>
  # Group by naics_pre and check if any codes are constant
  group_by(naics_pre) |>
  # Keep only rows where the code is not constant
  mutate(! all(naics_pre == naics_post)) |>
  # Optional: ungroup to remove grouping
  ungroup()

fwrite(naics1207, "naics1207.csv")

# # Map NAICS-4 when code was retired; else keep prior code
# foreach X in 9702 0207 1207{
# use naics`X',clear
# # select most common map
# g naics4_pre = substr(naics_pre,1,4)
# g naics4_post = substr(naics_post,1,4)
# bys naics4_pre naics4_post: g ct_ni = _N
# bys naics4_pre: g ct_n = _N
# bys naics4_pre naics4_post: keep if _n == 1
# g pctmap = ct_ni/ct_n
# gsort naics4_pre -pctmap naics4_post // for ties, we take lowest NAICS (very rare)
# bys naics4_pre: keep if _n == 1
# keep naics4_pre naics4_post

naics9702 |>
  mutate(
    naics4_pre = substr(naics_pre, 1, 4),
    naics4_post = substr(naics_post, 1, 4)) |>
  group_by(naics4_pre, naics4_post) |>
  mutate(
    ct_ni = n(),  # count of specific pre-post combinations
    ct_n = n_distinct(naics4_pre)  # count of pre codes
  ) |>
  slice(1) |>
  # Calculate mapping percentage
  mutate(pctmap = ct_ni / ct_n) |>
  # Sort to prioritize most common mappings
  arrange(naics4_pre, desc(pctmap), naics4_post) |>
  # Keep only the top mapping for each pre code
  group_by(naics4_pre) |>
  slice(1) |>
  # Select only necessary columns
  select(naics4_pre, naics4_post)
  

# # merge and update
# destring naics*, replace
# rename naics4_pre naics4
# merge 1:m naics4 using tempcpstat, nogen keep(matched using)
# replace naics4 = naics4_post if naics4_post ~= . 
# drop naics4_post 
# save tempcpstat,replace
# sleep 500
# erase naics`X'.dta
# }


# 
# # FILL-IN MISSING NAICS-4 USING SIC
# # Includes all firms that exited before 1985
# # We map to the most common NAICS-4 for a given SIC. This ensures our mapping
# # effectively weighs by the number of firms. Alternatively, we could use the 
# # NAICS-SIC concordances but they rarely provide a one-to-one map 
# 
# # NAICS-4 
# use tempcpstat,clear
# drop if naics4 == .
# bys gvkey sic naics4 : keep if _n == 1
# collapse (count) ctobs = gvkey, by (sic naics4)
# gsort sic -ctobs naics4
# bys sic : keep if _n==1
# keep sic naics4
# rename naics4 naics4_sicmapped
# drop if sic == .
# merge 1:m sic using tempcpstat, nogen
# replace naics4 = naics4_sicmapped if naics4 == .
# drop naics4_sicmapped
# 
# # Adjust NAICS-2 and 3 when mapped (either through SIC or across NAICS)
# tostring naics4, g(strnaics4)
# forvalues ii = 2(1)3 {
# g n`ii' = substr(strnaics4,1,`ii')
# destring n`ii', replace
# replace naics`ii' = n`ii' if n`ii' ~= naics`ii'
# }
# drop n2 n3 strnaics4
# 
# # NAICS TO BEA
# 
# # Map NAICS to BEA segments 
# g naicsbea = naics3
# replace naicsbea = naics4 if inrange(naics4,5411,5419)
# replace naicsbea = naics2 if inlist(naics2,22,23,42,44,45,55,61,81)
# 
# merge m:1 naicsbea using ../Temp/NAICS2BEA
# 
# # test: NAICS 55 (Mgmt, not in cpstat), 513,514,516 (retired tech codes),  
# # 491 (USPS), 521 (Fed), and 999 (Other) don't map. OK.
# tab naicsbea _merge if _m <3
# pause
# drop if _merge == 2
# drop _merge
# 
# # FINALIZE
# 
# order gvkey year 
# sort gvkey year
# bys gvkey year: keep if _n ==1 
# drop if gvkey == .
# xtset gvkey year 
# compress
# save tempcpstat, replace
# drop test*
# save NA_Compustat_Annual/loaded/NA_compustat, replace
# 
# # Test
# use tempcpstat, clear
# egen ct = count(gvkey)
# g test1 =  ct - test_totct 
# su test1
# if abs(`r(mean)')>0.001 BREAK
# erase tempcpstat.dta