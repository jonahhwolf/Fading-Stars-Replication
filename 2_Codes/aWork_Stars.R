# clear all
# set more off 
# pause off
# set logtype text
# 
# *********************************************
# capture program drop DY0
# program define DY0
# g temp = `1' if year==`2'
# egen `1'0 = min(temp),by(gvkey)
# drop temp
# end
# 
# capture program drop truncate
# program define truncate
# replace `1' = `2' if `1'<`2' & `1'~=.
# replace `1' = `3' if `1'>`3' & `1'~=.
# end
# *********************************************
# 
# *************** DESCRIPTION ******************************************
# * Implements decomposition and creates final figures
# * 
# * Version 1.0
# * Last edit: 1/12/2018 
# **********************************************************************
# 
# /* ---------------------- */ 
# /* 		SAMPLE CREATION	  */ 
# /* ---------------------- */ 
# 
# use 3_Final_data/main_dataset_firm, clear
# 
# keep if loc == "USA"
# drop if year==.
# drop if gvkey==.
# drop if at==.
# drop if at<=0
# drop if sale==.
# drop if sale<=0
# drop if emp==.
# drop if emp<=0
# drop if oiadp==.
# drop if aa1_pgo == .	// pre-1963 only
# replace me=0 if me==.
# replace ps = -1 if ps < -1
# 
main_dataset <- read_dta("3_Final_data/main_dataset_firm.dta") |>
  filter(
    loc == "USA",
    !is.na(year),
    !is.na(gvkey),
    !is.na(at),
    at > 0,
    !is.na(sale),
    sale > 0,
    !is.na(emp),
    emp > 0,
    !is.na(oiadp),
    !is.na(aa1_pgo)
  ) |>
  mutate(
    me = ifelse(is.na(me), 0, me),
    ps = ifelse(ps < -1, -1, ps)
  )

# * Real output deflated with industry prices
# g sale09 = sale/(aa1_pgo/100)
# drop if sale09<0.1
# 
# * Year when firm appears in Compustat
# egen year0 = min(year), by(gvkey)
# replace year0 = 1960 if year < 1960
# 
# replace conm="IBM" if conm=="INTL BUSINESS MACHINES CORP"
# replace conm="DUPONT" if conm=="DU PONT (E I) DE NEMOURS"
# replace conm="US STEEL" if conm=="UNITED STATES STEEL CORP" | conm == "USX CORP-CONSOLIDATED"
# 
# sort gvkey year
# xtset gvkey year
# encode(indcode),g(indgr)

main_dataset <- main_dataset %>%
  # Create real sales variable
  mutate(sale09 = sale / (aa1_pgo / 100)) %>%
  # Filter out very small sales
  filter(sale09 >= 0.1) %>%
  # Create year when firm first appears in Compustat
  group_by(gvkey) %>%
  mutate(year0 = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  # Adjust year0 for pre-1960 entries
  mutate(year0 = ifelse(year < 1960, 1960, year0)) %>%
  # Standardize company names
  mutate(
    conm = case_when(
      conm == "INTL BUSINESS MACHINES CORP" ~ "IBM",
      conm == "DU PONT (E I) DE NEMOURS" ~ "DUPONT",
      conm %in% c("UNITED STATES STEEL CORP", "USX CORP-CONSOLIDATED") ~ "US STEEL",
      TRUE ~ conm  # keep original name for all others
    )
  ) %>%
  # Sort by gvkey and year
  arrange(gvkey, year) %>%
  # Create industry group factor variable
  mutate(indgr = as.numeric(as.factor(indcode)))

# 
# *
# 
# ****** EMPLOYMENT AND COSTS ******
# 
# * Total Costs
# g cost = sale - oiadp
# g cost2 = cogs + xsga
# label variable cost "Total Cost: S - OI"
# label variable cost2 "Total cost: COGS+XSGA"
# 
# * Employment
# g logemp = log(emp)
# g dlogemp = d.logemp
# g demp = d.emp
# 
# *
# 
# ****** LABOR PRODUCTIVITY ******
# 
# * Labor productivity 
# g logsale09 = log(sale09)
# g lp = sale09/emp
# drop if lp==.
# g loglp = log(lp)
# g dloglp = d.loglp
# 
# label variable lp "Real Sale per Employee (thou 09 USD)"
# label variable loglp "Log Real Sale per Employee"
# 
# * Labor Quality Adjusted
# g lpw = sale09/(aa1_wq*emp)
# g loglpw = log(lpw)
# g dloglpw = d.loglpw
# 
# label variable lpw "Real Sale per quality-adj. Employees"
# label variable loglpw "Log Real Sale per quality-adj. Employees"
# 
# * truncate
# egen lpmedi = median(lp), by(year indgr)
# g low = lpmedi / 10
# g high = 10 * lpmedi
# truncate lp  low high
# truncate lpw low high
# drop low high
# 
# *
# 
# ****** FOOTPRINT ******
# 
# * shares relative to compustat sample
# egen saletot = sum(sale), by(year)
# egen metot   = sum(me), by(year)
# egen emptot  = sum(emp), by(year)
# 
# g me_share   = me/metot
# g sale_share = sale/saletot
# g emp_share  = emp/emptot
# 
# * shares relative to US economy, with and without export adjustments
# * Units: 
# *	- Compustat: emp = Thousands, sale = Millions, cogs = Millions
# *	- Macro: emp_civil = Millions, GDP = Billions
# g en  = emp/(emp_civil*1e3)
# g sy  = sale/(y*1e3)
# g cy  = cost/(y*1e3)
# g sye = sale*(1-aa1_pctfor)/(y*1e3)
# g cye = cost*(1-aa1_pctfor)/(y*1e3)
# 
# label variable me_share "Share of Mkt Val Equity"
# label variable sale_share "Sale Share of Compustat Sample"
# label variable emp_share "Employment Share of Compustat Sample"
# label variable en "Share of US Civilian Employment"
# label variable sy "Sale over US GDP"
# label variable cy "Total Cost over US GDP"
# label variable sye "Domestic Sale over US GDP"
# label variable cye "Domestic Cost over US GDP"
# 
# *
# 
# ****** INDUSTRY AGGREGATES ******
# egen a_lp = wtmean(lp), by(year) weight(emp)
# egen a1_lp = wtmean(lp), by(year indgr) weight(emp)
# egen a_lpw = wtmean(lpw), by(year) weight(emp)
# egen a1_lpw = wtmean(lpw), by(year indgr) weight(emp)
# egen a1_emp = mean(emp), by(year indgr)
# egen a1_sale09 = mean(sale09), by(year indgr)
# egen a1_ps = wtmean(ps), by(year indgr) weight(sale)
# 
# * BEA sector productivity 
# g aas_lp = 1e3*aas_goq/aas_ftpt
# label variable aas_lp  "GO/Emp (thousand 09 USD)"
# 
# *
# 
# ******* AVERAGING  ******
# * Forward Looking 3-year Growth Rate
# * star status and weights are defined as of year t
# * look at average growth rate over following 3 years
# sort gvkey year
# foreach X in dloglp dloglpw dlogemp demp {
# 	tssmooth ma `X'_ma = `X', w(0 1 2)
# 	replace `X'_ma = (`X' + l.`X')/2 if year == 2017
# }
# drop if dloglp_ma==.
# drop if dlogemp_ma==.
# 
# * truncate
# truncate dloglp_ma  -0.3 0.5
# truncate dloglpw_ma -0.3 0.5
# truncate dlogemp_ma -0.3 0.5
# 
# *
# 
# ****** RELATIVE PRODUCTIVITY ******
# 
# * three measures of relative revenue labor productivity
# g R1lp = (lp - aas_lp)/aas_lp		// lab prod relative to BEA sector
# g R1lpc = (lp - a1_lp)/a1_lp		// lab prod relative to cpstat industry
# g R1lpw = (lpw - aas_lp)/aas_lp		// wage-adjusted lab prod relative to BEA sector 
# g R1loglp = loglp - log(aas_lp)	
# g R1loglpc = loglp - log(a1_lp)
# g R1loglpw = loglpw - log(aas_lp)
# 
# * truncate based on GAFAM values
# truncate R1lp  -0.5 4.5	
# truncate R1lpc -0.5 4.5	
# truncate R1lpw -0.5 4.5
# 
# truncate R1loglp -0.5 1.5		
# truncate R1loglpc -0.5 1.5		
# truncate R1loglpw -0.5 1.5
# 
# 
# **
# 
# 
# /* ----------------------------- */ 
# /* 		DEFINITION OF STARS 	 */ 
# /* ----------------------------- */ 
# 
# *** OVERALL STARS *** 
# gsort year -me
# bys year: g rkme = _n
# replace rkme = . if inlist(me,0,.)
# g star = rkme <=20 & me > 0
# 
# * no oil
# g rkme_exoil = rkme
# replace rkme_exoil = . if inlist(indcode,"Min_oil_and_gas","Nondur_petro")
# sort year rkme_exoil 
# bys year: replace rkme_exoil = _n if ~inlist(indcode,"Min_oil_and_gas","Nondur_petro")
# g star_exoil  = rkme_exoil <= 20 & me > 0
# 
# **
# 
# *** INDUSTRY STARS ***
# gsort year indgr -me
# bys year indgr: g irkme = _n
# replace irkme = . if inlist(me,0,.)
# 
# gsort year indgr -sale
# bys year indgr: g irks = _n
# replace irks = . if inlist(sale,0,.)
# 
# * fill in ME ranks using sales if not enough me-based ranks
# egen nirkme = sum(irkme~=.),by(indcode year)
# replace irkme = irks if nirkme < 4
# 
# g istar = irkme<=4
# g istar_exoil  = istar * ~inlist(indcode,"Min_oil_and_gas","Nondur_petro")
# 
# g all = 1	// for loops
# 
# * Truncate LP at -0.3 for stars (few cases in 1960s)
# foreach X in R1lp R1lpc R1lpw R1loglp R1loglpc R1loglpw {
# 	replace `X' = -0.25 if `X'<-0.25 & `X'~=.  & istar
# }
# 
# **
# 
# * FOOTPRINT OF STARS
# foreach gp in all star istar {
# 	if "`gp'" == "all" local clab = "All"
# 	else if "`gp'" == "star" local clab = "Top 20"
# 	else if "`gp'" == "istar" local clab = "Top 4*Ind"
# 	
# 	egen sys_`gp'  = sum(sy*`gp'), by(year)
# 	egen syes_`gp' = sum(sye*`gp'), by(year)
# 	egen ens_`gp'  = sum(en*`gp'), by(year)
# 	
# 	label variable sys_`gp' "`clab'"
# 	label variable syes_`gp' "`clab'"
# 	label variable ens_`gp' "`clab'"	
# }
# 
# sort gvkey year
# save Temp/tempanalysis_stars, replace
# */
# 
# ***
# 
# /* ------------------------------------ */ 
# /*  	FIGURE 1: FOOTPRINT OF STARS	*/
# /* ------------------------------------ */ 
# 
# use Temp/tempanalysis_stars, clear
# sort year
# 
# keep if star & year >= year0
# bys year: g oo = _n ==1

tempanalysis_stars <- read_dta("Temp/tempanalysis_stars.dta")

processed_data <- tempanalysis_stars |>
  filter(star & year >= year0) |>
  arrange(year) |>
  group_by(year) |>
  mutate(oo = row_number() == 1) |>
  ungroup()

# 
# * EMPLOYMENT
# scatter ens_star ens_istar year if oo ,  c(l l l) ms(th oh i)  leg(r(1)) t1("Employees over Civilian Employment") xti("") 
# graph export 4_Figures/1a_stars_employment.eps, as(eps) mag(150) replace
# graph export 4_Figures/1a_stars_employment.png, as(png) replace
# pause
#  
# label variable syes_star "Top 20, Dom"
# label variable syes_istar "Top 4*Ind, Dom"
# scatter  syes_star sys_star syes_istar sys_istar year if oo ,  c(l l l l) ms(th i oh i) lc(dkgreen dkgreen red red) mc(green green red red) legend(row(2)) t1("Sales over GDP")  xti("")
# graph export 4_Figures/1b_stars_sales_dom.eps, as(eps) mag(150) replace
# graph export 4_Figures/1b_stars_sales_dom.png, as(png) replace
# pause
processed_data |>
  filter(oo) |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = syes_star)) +
  geom_line(aes(y = sys_star)) +
  geom_line(aes(y = syes_istar)) +
  geom_line(aes(y = sys_istar))
# 
# 
# ***
# 
# /* ------------------------------------------------ */ 
# /*  	FIGURE 2: HULTEN CONTRIBUTION OF THE STARS	*/
# /* ------------------------------------------------ */ 
# * m = wtd. avg LP growth among compustat firms
# * s = contribution of compustat firms to aggregate LP growth
# use Temp/tempanalysis_stars, clear
# 
# * COMPUTE CONTRIBUTION
# foreach gp in star_exoil istar_exoil { 
# 	if "`gp'" == "star_exoil" local clab = "Top 20, No Oil"
# 	else if "`gp'" == "istar_exoil" local clab = "Top 4*Ind, No Oil"
# 
# 	* LP
# 	foreach wt in sye cye {
# 		egen dloglp_m_`wt'_`gp' = wtmean(dloglp_ma), by(year) weight(`wt'*`gp')
# 		egen dloglp_s_`wt'_`gp' = sum(dloglp_ma* `gp' * `wt'), by(year)
# 			
# 		egen dloglpw_m_`wt'_`gp' = wtmean(dloglpw_ma), by(year) weight(`wt'*`gp')
# 		egen dloglpw_s_`wt'_`gp' = sum(dloglpw_ma* `gp' *`wt'), by(year)
# 		
# 		label variable dloglp_m_`wt'_`gp' "`clab'"
# 		label variable dloglp_s_`wt'_`gp' "`clab'"
# 		label variable dloglpw_m_`wt'_`gp' "`clab'"
# 		label variable dloglpw_s_`wt'_`gp' "`clab'"
# 	}
# }
# 
# * CREATE FIGURES
# keep if star & year >= year0
# bys year: g oo = _n ==1 
# keep if oo
# tset year
# 
# * Star
# scatter dloglpw_s_sye_star_exoil  dloglpw_s_cye_star_exoil  year, yline(0,lp(dash) lc(gray)) c(l l l) ms(i oh) leg(r(1)) t1("Hulten Contribution, Top 20 No Oil") leg(order(1 "Sales-Weighted" 2 "Cost-Weighted")) yti("") xti("") 
# graph export 4_Figures/2a_stars_Hulten.eps, as(eps) mag(150) replace
# graph export 4_Figures/2a_stars_Hulten.png, as(png) replace
# pause
# * iStar
# scatter dloglpw_s_sye_istar_exoil dloglpw_s_cye_istar_exoil year, c(l l l) yline(0,lp(dash) lc(gray)) ms(i oh) leg(r(1)) t1("Hulten Contribution, Top 4*Ind No Oil") leg(order(1 "Sales-Weighted" 2 "Cost-Weighted")) yti("") xti("") 
# graph export 4_Figures/2b_istars_Hulten.eps, as(eps) mag(150) replace
# graph export 4_Figures/2b_istars_Hulten.png, as(png) replace
# pause
# 
# sort year
# save Temp/Hulten, replace
# */
# 
# **
# 
# /* ---------------------------------------------------- */ 
# /* 		FIGURE 3: REALLOCATION CONTRIBUTION OF STARS	*/
# /* ---------------------------------------------------- */
# * Reallocation contribution to industry lab prod growth
# 
# use Temp/tempanalysis_stars, clear 
# 
# * COMPUTE CONTRIBUTION
# * Three labor productivity benchmarks
# * using BEA
# g reloclp  = dlogemp_ma * R1lp
# g reloclpw = dlogemp_ma * R1lpw
# * using compustat
# g reloclpc = dlogemp_ma * R1lpc
# 
# g wtdreloclp   =  reloclp  *aas_lp * emp /y/1e3  
# g wtdreloclpw  =  reloclpw *aas_lp * emp /y/1e3  
# g wtdreloclpc  =  reloclpc * a1_lp * emp /y/1e3
# 
# * By group
# foreach gp in star_exoil istar_exoil { 
# 	if "`gp'" == "star_exoil" local clab = "Top 20, No Oil"
# 	else if "`gp'" == "istar_exoil" local clab = "Top 4*Ind, No Oil"
# 	
# 	egen Reloclp_s_`gp' = 	sum( wtdreloclp  * `gp' ), by(year)
# 	egen Reloclpw_s_`gp' = 	sum( wtdreloclpw * `gp' ), by(year)
# 	egen Reloclpc_s_`gp' = 	sum( wtdreloclpc * `gp' ), by(year)
# 	
# 	label variable Reloclp_s_`gp' "`clab'"
# 	label variable Reloclpw_s_`gp' "`clab'" 
# 	label variable Reloclpc_s_`gp' "`clab'"	
# }
# 
# *
# 
# * SMOOTH
# keep if star_exoil & year >= year0
# bys year: g oo = _n ==1 
# keep if oo
# tset year
# 
# tssmooth ma aa_dlp_ma = aa_dlp/100, w(2 1 2)
# label variable aa_dlp_ma "Agg. (Fernald)"
# 
# foreach gp in star_exoil istar_exoil { 
# 	if "`gp'" == "all" local clab = "All"
# 	else if "`gp'" == "all_exoil" local clab = "All, No Oil"
# 	else if "`gp'" == "star" local clab = "Top 20"
# 	else if "`gp'" == "star_exoil" local clab = "Top 20, No Oil"
# 	else if "`gp'" == "sstar" local clab = "Top 100, by Sales"
# 	else if "`gp'" == "sstar_exoil" local clab = "Top 100 No Oil, by Sales"
# 	else if "`gp'" == "istar" local clab = "Top 4*Ind"
# 	else if "`gp'" == "istar_exoil" local clab = "Top 4*Ind, No Oil"
# 	else if "`gp'" == "isstar" local clab = "Top 4*Ind, by Sales"
# 	else if "`gp'" == "isstar_exoil" local clab = "Top 4*Ind No Oil, by Sales"
# 		
# 	tssmooth ma Reloclp_s_`gp'_ma = Reloclp_s_`gp', w(1 1 1)
# 	tssmooth ma Reloclpw_s_`gp'_ma = Reloclpw_s_`gp', w(1 1 1)
# 	tssmooth ma Reloclpc_s_`gp'_ma = Reloclpc_s_`gp', w(1 1 1)
# 	
# 	label variable Reloclp_s_`gp'_ma "`clab'"
# 	label variable Reloclpw_s_`gp'_ma "`clab'" 
# 	label variable Reloclpc_s_`gp'_ma "`clab'"	
# }
# 
# * CREATE FIGURES
# gen mreloc_star_exoil =  (Reloclpw_s_star_exoil_ma  + Reloclpc_s_star_exoil_ma)/2
# gen mreloc_istar_exoil = (Reloclpw_s_istar_exoil_ma + Reloclpc_s_istar_exoil_ma)/2
# label variable mreloc_star_exoil "Top 20"
# label variable mreloc_istar_exoil "Top 4*Ind"
# 
# scatter  mreloc_star_exoil  mreloc_istar_exoil  year if oo, c(l l l) ms(th oh sh) leg(r(1)) t1("Productive Reallocation") xti("") 
# graph export 4_Figures/3_stars_reloc_exoil.eps, as(eps) mag(150) replace
# graph export 4_Figures/3_stars_reloc_exoil.png, as(png) replace
# pause
# save Temp/Realloc, replace
# */
# 
# 
# **
# 
# /* -------------------------------------------- */ 
# /* 		FIGURE 4: TOTAL CONTRIBUTIONS OF STARS	*/
# /* -------------------------------------------- */
# 
# use Temp/Realloc, clear
# sort year
# merge 1:1 year using Temp/Hulten, nogen
# merge 1:1 year using Temp/Census, nogen
# 
# g c12m_star_exoil    =  dloglp_s_cye_star_exoil  + mreloc_star_exoil
# g c12m_istar_exoil    =  dloglp_s_cye_istar_exoil  + mreloc_istar_exoil
# 
# egen period = cut(year), at(1950,2001,2020)
# egen mc12star = mean(c12m_star_exoil),by(period)
# egen mc12istar = mean(c12m_istar_exoil),by(period)
# 
# scatter c12m_star_exoil mc12star  c12m_istar_exoil mc12istar c12_star_census year,  c(l l l l l) ms(th i oh i) leg(r(1)) lc(green green dknavy dknavy) t1(Contribution of Stars to Labor Productivity Growth) lp(s dash s dash) leg(order(1 "Top 20, No Oil" 3 "Top4*Ind, No Oil" 5 "Census Top4*Ind"))
# graph export 4_Figures/4_c12_drop_all.eps, as(eps) mag(150) replace
# graph export 4_Figures/4_c12_drop_all.png, as(png) replace
