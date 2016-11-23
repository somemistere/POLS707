*****************************************************************************************
*** This do file creates the database for  												*/
*** Prostitution and Human Trafficking 													*/
*** Seo-Young Cho (Georg-August University Göttingen) 									*/
*** Axel Dreher (Heidelberg University, Goettingen University, KOF, CESifo, IZA)		*/
*** Eric Neumayer (LSE) 																*/
*****************************************************************************************
*** Last update: August 25, 2011 AD
*****************************************************************************************

use "data processed\Cho, Dreher, Neumayer WD replication.dta", clear

* Table 1, column 1
mi estimate, post: oprobit htflowsunodc prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln 	///
		democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa ///
		if inc_low==0, robust 
estimates store c1

* Table 1, column 2
mi estimate, post: oprobit htflowsunodc prostitutionlaw prostitutionbrothel ruleWB_m pop_ln gdp_pc_const_ppp_ln 	///
		democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa  ///
		if inc_low==0, robust 
estimates store c2

* Table 1, column 3
mi estimate, post: oprobit htflowsunodc prostitutionbrothel ruleWB_m pop_ln gdp_pc_const_ppp_ln 	///
		democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa  ///
		if inc_low==0, robust 
estimates store c3

* Table 1, column 4
mi estimate, post: oprobit htflowsunodc prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln 	///
		democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa  ///
		, robust 
estimates store c4

* Table 1, column 5
mi estimate, post: oprobit htflowsunodc prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln 	///
		democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa  ///
		if inc_high_oecd==1 | inc_high==1, robust
estimates store c5

* Table 1, column 6
mi estimate, post: reg htflowsunodc prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln 	///
		democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa  ///
		if inc_low==0, robust
estimates store c6
	
* Table 1, column 7
oprobit htflowsunodc prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa  ///
		if inc_low==0 & _mi_m==0, robust
estimates store c7

* Table 1, column 8
reg htflowsunodc prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa  ///
		if inc_low==0 & _mi_m==0, robust
estimates store c8


estout c1 c2 c3 c4 c5 c6 c7 c8 ///
				using "output tables\table 1.txt", replace label delimiter(_tab) noabbrev  									///
				cells(b(star fmt(%9.3f)) t(par abs fmt(2))) style(fixed) starlevels(* 0.10 ** 0.05 *** 0.01) drop(_cons o.*)	///
				stats(N, labels("Number of countries")  ///
				fmt(0)) order(prostitutionlaw prostitutionbrothel ruleWB_m pop_ln gdp_pc_const_ppp_ln democracy 			///
				stockmigrants1990_ln catholic2 reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa)			///
				mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)")	

		
				
*** Table 2

foreach region in reg_west_europe reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa {
mi estimate, post: oprobit htflowsunodc prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln democracy stockmigrants1990_ln catholic2  ///
		reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa  ///
		if inc_low==0 & `region'==0, robust 
estimates store c_`region'
				
}
				

estout c_reg_west_europe c_reg_east_asia c_reg_europe c_reg_latam c_reg_mideast c_reg_sasia c_reg_ssa ///
				using "output tables\table 2.txt", replace label delimiter(_tab) noabbrev  									///
				cells(b(star fmt(%9.3f)) t(par abs fmt(2))) style(fixed) starlevels(* 0.10 ** 0.05 *** 0.01) drop(_cons)	///
				stats(N, labels("Number of countries")  ///
				fmt(0)) order(prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln democracy 			///
				stockmigrants1990_ln catholic2 reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa o.*) ///
				mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)")					
				

				
*** Extreme Bounds Analysis (EBA), code based on Gassebner, Lamla and Sturm (2011)
use "data processed\Cho, Dreher, Neumayer WD replication EBA.dta", clear
set more off

**define some stuff
local depvar "htflowsunodc"
local basvar "prostitutionlaw ruleWB_m pop_ln gdp_pc_const_ppp_ln democracy stockmigrants1990_ln catholic2 reg_east_asia reg_europe reg_latam reg_mideast reg_sasia reg_ssa"
local impute_con "mediafreedom tourism_in_ln protestantpopulation muslimpopulation urbanization trade_gdp prosecution_m protection_m prevention_m execrlc_right_m alq_m literacy alq_f alq_agri mort_5 visaonothers visaatborder2onothers"  /* continuous X variables */
local impute_bin "englishspeaking frenchspeaking spanishspeaking portuguesespeaking germanspeaking britishlegal socialistlegal frenchlegal germanlegal scandinavianlegal" /* binary X-variables */

local basvar2 "`basvar'"
local level ".95"
local level "3"
local testvar "`impute_con' `impute_bin'"  
local testvar1 "`testvar'"

	
* EBA starts here
***********************************
***********************************
local i "1"
local z "1"
local l "1"
local testvar3 "`testvar1'"
local S : list sizeof testvar1

***********************************
* combinations of ONE
***********************************
forvalues k = 1/`S' {
gettoken test testvar1 : testvar1
local testvar2 "`testvar2'" "`test'"
local var`k' "`test'"
}
local k=`S'+1
local l=`k'

***********************************
* makes combinations of two if level==2
***********************************
if `level'==2 {

local S_nz : list sizeof testvar3

local i "1"

foreach x in `testvar3' {
      local z`i' "`x'"
	local i = `i' + 1
	}
***********************************
* combinations of TWO
***********************************
local l "1"
local i "1"
while "`i'" ~="`S_nz'"{
local j = `i' + 1
while `j'<=`S_nz' {
local var`k' `" `z`i'' `z`j''"'
local k =`k'+1
local j = `j'+ 1
}
local i =`i' + 1
}
local l=`k'
}

***********************************
* makes combinations of three if level==3
***********************************
else if `level'==3 {
local i "1"
local S_nz : list sizeof testvar3

local i "1"

foreach x in `testvar3' {
      local z`i' "`x'"
	local i = `i' + 1
	}
***********************************
* combinations of TWO
***********************************
local l "1"
local i "1"
while "`i'" ~="`S_nz'"{
local j = `i' + 1
while `j'<=`S_nz' {
local var`k' `" `z`i'' `z`j''"'
local k =`k'+1
local j = `j'+ 1
}
local i =`i' + 1
}

***********************************
* combinations of THREE
***********************************
local l=`k'+1
local k "1"
local i "1"
while "`i'" ~="`S_nz'"{
local j = `i' + 1
	while `j'<=`S_nz' {
	local k = `j' + 1
	while `k' <= `S_nz' {
local var`l' `" `z`i'' `z`j'' `z`k''"'
local l = `l'+1
local k = `k' + 1
	}
	local j = `j'+ 1
	}
local i =`i' + 1
}

}
***********************************
* Actual Procedure
************************************

local i "0"
foreach x in `basvar' {
      gen b`i'= 0
	gen se`i'=0
	gen lb`i'=0
	gen ub`i'=0
	gen cdf`i'=0
	gen sig`i'=0
	local i = `i' + 1
	}
gen obs=0
gen ci=0
gen ct=0

***********************************
* Some LOG Files
***********************************
file open ebabaslog using "EBA\ebabaslog-basREG.txt", write replace
*file open notconvergedlog using "EBA\notconverged-basREG.txt", write replace
file open badregressionlog using "EBA\badregression-basREG.txt", write replace

***********************************
* Regression type
***********************************
forvalues k=1/`l'{
 	if "`var`k''"~=""{

		
		capture noisily mi estimate, post: oprobit `depvar' `basvar' `var`k'' if inc_low==0, robust 

		
			
			
		di _column(100) "progress" 100*`k'/`l'

			if _rc==0{
				
				gettoken cond : basvar
                        	if (1-ttail(e(N),(_b[`cond']/_se[`cond'])))~=.{
				quietly replace ci = ci+1
				local h "0"
				quietly replace obs=obs+e(N)
					foreach g in `basvar'{
						quietly replace b`h'= _b[`g'] + b`h'
						quietly replace se`h'= _se[`g'] + se`h'
						if ttail(e(N), abs(_b[`g']/_se[`g'])) <= .05 {
						quietly replace sig`h'=sig`h'+1
						}
						if (lb`h'>(_b[`g']-2*_se[`g'])) {
						quietly replace lb`h'= _b[`g']-2*_se[`g']
						local lbvar`h' `"`var`k''"'
						}
						if (ub`h'<(_b[`g']+2*_se[`g'])) {
						quietly replace ub`h'= _b[`g']+2*_se[`g']
						local ubvar`h' `"`var`k''"'
						}
						quietly replace cdf`h'= (1-ttail(e(N),(_b[`g']/_se[`g'])))+cdf`h'
						local h=`h'+1
		 			
		 		}
				}
				
			}
		 	else {
			file write badregressionlog "`var`k''"  _n
			}

	}
}


***********************************
* Results
***********************************
di "Number of draws" _skip(5) ci
di "AVG Observations" _skip(5) obs/ci
local h "0"
display "Variable" _column(10) "Avg. Beta" _column(22) "Avg.Std.Err" _column(35) "%Sign." _column(47) "CDF-U" _column(60) "lower Bound" _column(95) "upper Bound" _column(130) "combi" _column(140) "Avg. Obs"
file write ebabaslog "Variable" ";" "Avg. Beta" ";" "Avg.Std.Err" ";" "%Sign." ";" "CDF-U" ";" "lower Bound" ";" "lower Bound variables" ";" "upper Bound" ";" "upper Bound variables" ";" "combi" ";" "Avg. Obs" _n

foreach g in `basvar'{
	quietly replace b`h'=b`h'/ci
	quietly replace se`h'=se`h'/ci
	quietly replace sig`h'=sig`h'/ci
	quietly replace cdf`h'=cdf`h'/ci
        quietly replace cdf`h'=max(cdf`h',1-cdf`h')
	display "`g'" _column(10) b`h' _column(22) se`h' _column(35) sig`h' _column(47) cdf`h' _column(60)  lb`h' "`lbvar`h''" _column(95)ub`h' "`ubvar`h''" _column(130) ci _column(140) obs/ci

	file write ebabaslog "`g'" ";" (b`h') ";" (se`h') ";" (sig`h') ";" (cdf`h') ";" (lb`h') ";" "`lbvar`h''" ";" (ub`h') ";" "`ubvar`h''" ";" (ci) ";" (obs/ci)  _n
	local h=`h'+1
}

file close ebabaslog
*file close notconvergedlog
file close badregressionlog
drop b0-ct /* drops results */



