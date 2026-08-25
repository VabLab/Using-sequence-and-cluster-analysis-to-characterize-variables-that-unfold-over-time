*****Data Cleaning for Didactic Paper, Applied Example

*****1) Clean trajectories
use "/Users/lpacca/Library/CloudStorage/Box-Box/01 - Occupation Trajectories/data/trk2018tr_r_MERGED.dta", clear

keep HHID PN BIRTHMO BIRTHYR DEGREE EXDEATHMO EXDEATHYR FIRSTIW GENDER HISPANIC IMMGYEAR RACE USBORN SCHLYRS AAGE BAGE CAGE DAGE EAGE FAGE GAGE HAGE KAGE MAGE NAGE OAGE PAGE QAGE state51_2020-state75_2020 state51_2018-state75_2018 state51_2016-state75_2016 state51_2014-state75_2014 state51_2012-state75_2012 state51_2010-state75_2010 state51_2008-state75_2008 state51_2006-state75_2006 state51_2004-state75_2004 state51_2002-state75_2002 state51_2000-state75_2000 state51_1998-state75_1998 state51_1996-state75_1996 state51_1995-state75_1995 state51_1994-state75_1994 state51_1993-state75_1993 state51_1992-state75_1992    

gen key=HHID+PN
sort key

reshape long state51_ state52_ state53_ state54_ state55_ state56_ state57_ state58_ state59_ state60_ state61_ state62_ state63_ state64_ state65_ state66_ state67_ state68_ state69_ state70_ state71_ state72_ state73_ state74_ state75_, i(key) j(year)

renvars state51_-state75_, postdrop(1)

forval j=51/75 {
replace state`j'="" if state`j'=="."	
}

capture drop mode*
forval j=51/75 {
bysort key: egen mode`j'=mode(state`j')
} //when there is more than one mode, missing values are generated

******
******If there is more than one mode, take the most recent value
forval j=51/75 {
gen year`j'=year if state`j'!=""
} //lists the years when core survey states are available

forval j=51/75{
bysort key: egen max_year`j'=max(year`j')
} //Identify the most recent year

*****change states to numerical
foreach var of varlist state*{
replace `var'="1" if `var'=="work"
replace `var'="2" if `var'=="unemployed"
replace `var'="3" if `var'=="temp_leave"
replace `var'="4" if `var'=="retired"
replace `var'="5" if `var'=="disabled"
replace `var'="6" if `var'=="homemaker"
replace `var'="7" if `var'=="part_time"
destring `var', replace
}

*****change states to numerical
foreach var of varlist mode*{
replace `var'="1" if `var'=="work"
replace `var'="2" if `var'=="unemployed"
replace `var'="3" if `var'=="temp_leave"
replace `var'="4" if `var'=="retired"
replace `var'="5" if `var'=="disabled"
replace `var'="6" if `var'=="homemaker"
replace `var'="7" if `var'=="part_time"
destring `var', replace
}

foreach var of varlist state* mode*{
label define var_label 1 "work" 2 "unemployed" 3 "temp_leave" 4 "retired" 5 "disabled" 6 "homemaker" 7 "part_time", replace
label values `var' var_label 
}

capture drop state*_max state*_max_mean
forval j=51/75 {
gen state`j'_max=state`j' if max_year`j'==year`j'
bysort key: egen state`j'_max_mean=mean(state`j'_max)
} //Identify most recently reported state

//mean
sort key year
forval j=51/75 {
bysort key: egen mean`j'=mean(state`j')
}

//RULE: Replace mode with most recent state value if there is more than one mode
forval j=51/75 {
replace mode`j'=state`j'_max_mean if mode`j'==. & mean`j'!=.
}

edit key year mode*

*******Keep relevant variables only
keep if year==1992
keep key HHID PN BIRTHMO BIRTHYR DEGREE EXDEATHMO EXDEATHYR FIRSTIW GENDER HISPANIC IMMGYEAR RACE USBORN SCHLYRS AAGE BAGE CAGE DAGE EAGE FAGE GAGE HAGE KAGE MAGE NAGE OAGE PAGE QAGE mode51-mode75  

*******
reshape long mode, i(key) j(age)

*******Fill gaps for mode
sort key age
gen mode_nogaps=mode
by key: replace mode_nogaps=mode_nogaps[_n-1] if mode_nogaps[_n-1]==mode_nogaps[_n+1] & mode_nogaps==.
by key: replace mode_nogaps=mode_nogaps[_n-1] if age==75 & mode_nogaps==.
by key: replace mode_nogaps=mode_nogaps[_n+1] if age==51 & mode_nogaps==.
label values mode_nogaps var_label 
save "/Users/lpacca/Library/CloudStorage/Box-Box/01 - Occupation Trajectories/data/finalstates_all.dta", replace

********2) Clean outcomes 
********Self rated health from individual core surveys
//2000-2004
clear
set more off
foreach data in H00B_R H02C_R H04C_R{
global path "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys"
infile using "$path/`data'.dct", using("$path/`data'.DA")
save "$path/`data'.dta", replace
clear
}

//2000
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H00B_R.dta", clear
keep HHID PN G1226
rename G1226 rate_health_2000
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2000.dta", replace

//2002
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H02C_R.dta", clear
keep HHID PN HC001
rename HC001 rate_health_2002
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2002.dta", replace

//2004
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H04C_R.dta", clear
keep HHID PN JC001
rename JC001 rate_health_2004
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2004.dta", replace

//2006
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H06C_R.dta", clear
keep HHID PN KC001
rename KC001 rate_health_2006
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2006.dta", replace

//2008
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H08C_R.dta", clear
keep HHID PN LC001
rename LC001 rate_health_2008
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2008.dta", replace

//2010
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H10C_R.dta", clear
keep HHID PN MC001
rename MC001 rate_health_2010
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2010.dta", replace

//2012
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H12C_R.dta", clear
keep HHID PN NC001
rename NC001 rate_health_2012
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2012.dta", replace

//2014
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H14C_R.dta", clear
keep HHID PN OC001
rename OC001 rate_health_2014
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2014.dta", replace

//2016
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H16C_R.dta", clear
keep HHID PN PC001
rename PC001 rate_health_2016
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2016.dta", replace

//2018
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/h18C_R.dta", clear
keep hhid pn QC001
rename QC001 rate_health_2018
gen key=hhid+pn
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2018.dta", replace

//2020
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/H20C_R.dta", clear
keep HHID PN RC001
rename RC001 rate_health_2020
gen key=HHID+PN
sort key
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2020.dta", replace

use "/Users/lpacca/Library/CloudStorage/Box-Box/04 - Education and BP/trk2018tr_r.dta", clear
capture drop key
gen key=HHID+PN
sort key
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2000.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2002.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2004.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2006.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2008.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2010.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2012.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2014.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2016.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2018.dta"
tab _merge
drop if _merge==2
sort key
drop _merge
merge key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/Health from core surveys/rate_health_2020.dta"

keep HHID PN BIRTHMO BIRTHYR DEGREE EXDEATHYR GENDER HISPANIC RACE SCHLYRS USBORN WTCOHORT rate_health_2000 rate_health_2002 rate_health_2004 rate_health_2006 rate_health_2008 rate_health_2010 rate_health_2012 rate_health_2014 rate_health_2016 rate_health_2018

gen key=HHID+PN

reshape long rate_health_, i(key) j(year)
gen age=year-BIRTHYR
gen rate_health_76=rate_health_ if age==76|age==77
keep if rate_health_76!=. & rate_health_76<=5 //beacuse values of 8 and 9 correspond to non-response
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/self_rated_health.dta", replace
quietly by key:  gen dup = cond(_N==1,0,_n)
tab dup //9,189 unique observations
save, replace

******Merge trajectories with outcome
use "/Users/lpacca/Library/CloudStorage/Box-Box/01 - Occupation Trajectories/data/finalstates_all.dta", clear
sort key
merge m:1 key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/self_rated_health.dta"
keep if _merge==3

gen state=1 if mode_nogaps==1 //work(full time)
replace state=2 if mode_nogaps==7 //work part time
replace state=3 if mode_nogaps==4 //retired
replace state=4 if mode_nogaps==5 //disabled
replace state=5 if mode_nogaps==2|mode_nogaps==3|mode_nogaps==6 //other out of the labor force
replace state=6 if state==. //unreported

gen survey_year=BIRTHYR+age
replace state=. if state==6 & survey_year<=FIRSTIW

sqset state key age
capture drop _merge
//sqindexplot,  color (ebblue eltblue green purple gold gray)

sort key age
trans2subs state, id(key) subs(smat)
matrix list smat

capture drop mode 
capture drop mode_nogaps 
capture drop survey_year
reshape wide state, i(key) j(age)

egen id=group(key)
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/final_sequences.dta", replace
sort id
save, replace

*******Import clusters obtained in R and merge with rest of the data
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7.dta", clear
sort id
gen cluster_7_final=1 if cl_7==9171 //reference group: typical retirement
replace cluster_7_final=2 if cl_7==9184 //early retirement
replace cluster_7_final=3 if cl_7==9099 //full-time to late retirement
replace cluster_7_final=4 if cl_7==8970 //part-time to late retirement
replace cluster_7_final=5 if cl_7==9156 //disability
replace cluster_7_final=6 if cl_7==1190 //typical retirement, initial missingness
replace cluster_7_final=7 if cl_7==9040 //out of work gaps

*******Merge
use "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/final_sequences.dta", clear
sort id
merge id using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7.dta"

reg rate_health_76 BIRTHYR GENDER i.RACE SCHLYRS USBORN i.cluster_7_final

********Sensitivity Analysis with death and lost to follow up as separate states
use "/Users/lpacca/Library/CloudStorage/Box-Box/01 - Occupation Trajectories/data/finalstates_all.dta", clear
sort key
merge m:1 key using "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/self_rated_health.dta"

gen survey_year=BIRTHYR+age
gen too_young=1 if age==75 & survey_year>=2018
bysort key: egen mean_too_young=mean(too_young)
drop if mean_too_young==1

gen state=1 if mode_nogaps==1 //work(full time)
replace state=2 if mode_nogaps==7 //work part time
replace state=3 if mode_nogaps==4 //retired
replace state=4 if mode_nogaps==5 //disabled
replace state=5 if mode_nogaps==2|mode_nogaps==3|mode_nogaps==6 //other out of the labor force
replace state=6 if state==. //unreported

gen death_age=EXDEATHYR-BIRTHYR
replace death_age=. if death_age>1000 //weird individuals reporting birth year=0
replace state=7 if state==6 & death_age<=age //dead

replace state=. if state==6 & survey_year<=FIRSTIW

tab state, mis

capture drop mode 
capture drop mode_nogaps 
capture drop survey_year
capture drop _merge
reshape wide state, i(key) j(age)
save "/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/final_sequences_sensitivity.dta", replace

