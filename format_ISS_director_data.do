************************************************************************
* 	Title: 	ISS Board Data: Director and Firm Level From 
*			Riskmetric's Director Legacy Database in WRDS
*
*	Author: Don Bowen
*
*	Purpose: 
*			Self contained code. Need only be run once.
*			Assembling director level database and a board level database.
*			Cleans some problems with variables.
*			Creates some variables.
*
*   Inputs:
*			directors_legacy_all.dta <-- From riskmetrics, via WRDS. 
*			The entire database, for all firms and years. 
*
*			new_independence_data.dta <-- provided in this repo
*
*   Outputs:
*			dir_level_data.dta 
*			board_level_data.dta
*
*			variables named with ALL_CAPS are firm-year variables. 
*
/*	Other notes:

	Be careful when merging on cusip6 to other datasets. ISS and CCM cusip6 
	need not be the same. 
				
	Not all firm years have CEOs on the BOD, naturally, but some from 1999-2006 
	don't have chairs of the BOD! (in the data)

	Obtains variables:

		identifiers: 			cusip6, fyear, firm_name_in_irrc
		
		dist of board char.:	meanage, medage, sdage,	minage,	maxage
								pct_female
								mean_outside_directorships, medoutside_directorships, sdoutside_directorships,	
								minoutside_directorships, maxoutside_directorships
		
		CEO info:				DUALITY (is the ceo the chairman?), CEO_AGE (avg, if multiple),	CEO_COUNT, CEO_FEMALE 
		
		Committee info:			SIZE_AUDIT, SIZE_COMP, SIZE_NOM
		
		Board structure:		SIZE, SIZE_change
								INDEP#, INDEP_change#
								NOT_INDEP#, NOT_INDEP_change#
								BOARD_IND#, BOARD_IND_change#
								
		Structure changes:		ADD_IND#, ADD_NOT#
								LOSE_IND#, LOSE_NOT# 
								HIRE_IND#, HIRE_NOT#
								SWITCHTO_IND#, SWITCHTO_NOT#	
								LASTYEAR_IND#, LASTYEAR_NOT#
								NONRETURN_IND0, NONRETURN_NOT0
		
	Notes on variables:
		
		# refers to 0, 1, 2, or 3. 
		0 means the variable is based on raw IRRC classification variable
		1 means the classification is switched to indep if 3 years since the director was an employee, no other links to firm
		2 means the classification is switched to indep if 3 years since the director was an employee, as long as not relative or interlocked
		3 means independent if IRRC classification is I or L, or E but 3 yrs or more since last employment
			
		HIRE_* : The HIRE_* variables make a specific assumption: they only count INITIAL hires, the first 
		year for the director at a firm. They only count if the firm already existed, so they omit
		the initial firm year where the whole board is hired (according to the data). However, they
		miss cases were a director is hired, fired, and THEN rehired. I assume that any gaps in 
		director service for a firm is due to data issues. Since the data only covers 10 years, I 
		assume the frequency of multiple, disjoint board terms is low.

		SWITCHTO_* : Directors that switched classifications relative to the previous observation
		for that manager-firm pair.

		LASTYEAR_* : This also applies to the LASTYEAR_* variables: they count when a director is 
		not present the next year for the firm if the firm continues. Any service gaps (and the 
		first termination) are ignored. NOTE THE TIMING OF THIS VARIABLE: it means the directors 
		do not return IN THE NEXT YEAR. 

		The ADD_* variables are relative to the prior year, and ARE NOT TOTAL CHANGE! Change is given by
			ADD_IND - LOSE_IND == INDEP - INDEP[_n-1]
			ADD_NOT - LOSE_NOT == NOT_INDEP - NOT_INDEP[_n-1]

		ADD_* and LOSE_* variables are built on *all* data available, before the years are trimmed,
		which is why firms have switching and adding in the first year of the final dataset. They 
		are relative to the prior year.	

		The director add/leave/switch variables are built on *all* data available, 
		before the years are trimmed, which is why firms have switching and adding
		in the first year of the final dataset.
			
		I fill in gap obervations for a manager firm pairing, assuming that the 
		gaps are data errors most of the time due to the short length of the panel. 
		ISS classification and linkage data are carried forward for the new observations.
	
*/
*************************************************************************

*************************************************************************
* BEGIN: Load presets and the data
*************************************************************************

/*
	* uncomment this section if using this code as standalone
	
	set rmsg on
	set more off

	set matsize 800
	set scrollbufsize 500000		
	capture program drop _all
	clear
	clear matrix

	* define these here if using as standalone, can comment out if this script is called from another do
	global dir_raw           "directors_legacy_all" // defined in main.do	
	global board_lvl_out     "$temp/board_level_data.dta"   
	global dir_lvl_out       "$temp/dir_level_data.dta"     
	
*/
		
*************************************************************************
* BEGIN: Cleaning the data
*************************************************************************
{

	use "$dir_raw", clear
	
*** Meeting date

	format year %ty
	format meetingdate %tdDD/NN/CCYY

	/* manual changes to proxy dates, because additional proxies or early/late proxies cause 2 obs per cusip-year */

	sort cusip year meeting
	by cusip year: g diffmeeting = 1 if meeting[_n-1] != meeting[_n] & _n > 1
	by cusip year: egen prob = sum(diffmeeting)
	gsort -prob cusip year meeting
	count if diffmeeting == 1	
	
	drop 				if cusip == "00204C" & meetingdate == td(03/06/1998) // second obs within 3 months; keep first one - typical proxy date 
	drop 				if cusip == "030954" & meetingdate == td(01/07/1997 ) // two obs within 3 months; keep second one - typical proxy date 
	drop 				if cusip == "089159" & meetingdate == td(23/11/1999) // two obs within 3 months; this one has less information
	drop 				if cusip == "313135" & meetingdate == td(11/03/1997 ) // other date is consistent with laer meeting dates
	drop 				if cusip == "268039" & meetingdate == td(16/10/1997 ) // other date is consistent with laer meeting dates
	drop 				if cusip == "31787A" & meetingdate == td(19/06/2001 ) // only one year for this firm, meetings 5 monhs apart, keep later date
	drop 				if cusip == "247357" & meetingdate == 15146 // second obs for that year an additional filing towards end of fiscal year 
	replace year = 2001 if cusip == "247357" & meetingdate == 14973 // proxy was filed early - fell into earlier calendar year 
	drop 				if cusip == "269246" & meetingdate == 14312 // first obs comes before fyb - change in fye 
	drop 				if cusip == "291525" & meetingdate == 14985 // first obs comes before fyb - late filing for previous year? 
	drop 				if cusip == "448407" & meetingdate == 15818 // second obs within 3 months; keep first one - typical proxy date 
	replace year = 1998 if cusip == "461202" & meetingdate == 14259 // one proxy was filed late - fell into later calendar year 
	replace year = 1997 if cusip == "461202" & meetingdate == td(25/11/1996) // proxy was filed early - fell into earlier calendar year 
	drop				if cusip == "461202" & meetingdate == td(16/01/1998) // filed 1 day after another, less variable info
	drop 				if cusip == "595112" & meetingdate == 14627 // change in fiscal year? - keep later filing as it is consistent with CCM and later years 
	drop 				if cusip == "595112" & meetingdate == td(29/01/1996) // keep later filing as it is consistent with CCM and later years 
	drop 				if cusip == "591695" & meetingdate == td(10/07/1997) // second obs within 3 months; keep first one - typical proxy date 
	replace year = 2001 if cusip == "637657" & meetingdate == 14965 // one proxy was filed early - fell into earlier calendar year 
	replace year = 1998 if cusip == "668367" & meetingdate == td(17/12/1997) // one proxy was filed early - this firm's years are 1 ahead
	replace year = 1997 if cusip == "682680" & meetingdate == td(12/12/1996) // one proxy was filed early - this firm's years are 1 ahead
	drop 				if cusip == "867910" & meetingdate == td(30/04/1996) // other was consistent mnth
	drop 				if cusip == "929903" & meetingdate == 15187 // second obs within 3 months; keep first one - typical proxy date 
	replace year = 1998 if cusip == "918914" & meetingdate == td(03/10/1997 ) // one proxy was filed early - this firm's years are 1 ahead
			
	drop diffmeet prob
	
*** audit committee chair must be on the audit committee
	
	tab audit_chair audit_mem , m
	replace audit_mem = 1 	if audit_chair == 1 
	
*** compensation committee chair must be on the compensation committee
	
	tab comp_chair comp_mem , m
	replace comp_mem = 1 	if comp_chair == 1
	
*** compensation committee chair must be on the compensation committee
	
	tab nomchair nom_membership , m
	replace nom_mem = 1 	if nomchair == 1
	
*** others
	
	tab age
	replace age = . if age < 20
	
	tab year_of_termination, m
	replace year_of_term	= 1999 	if year_of_termination == 99
	replace year_of_term	= 1993 	if year_of_termination == 93	

	tab dirsince, m
	replace dirsince = . 	if dirsince < 1910 // 1 is 1905 and 23 are 0.

	tab  grandfath, m
	replace grandfath = 1 if grandfath == 2	
	
	tab pcnt_ctrl_votingpower, m
	replace pcnt_ctrl_votingpower = 0 	if pcnt_ctrl_votingpower == 8190

	tab insthold
	replace insthold = . 	if insthold > 100
	
	tab class
	replace class = "I" if class == "#"  // manually checked, this director was independent

	*** I spend time and check in the data for different firms within cusip/year ...... 
	* how many cusips year obs have multiple names? These two fixes are necessary:

	replace name = "PCOM" if cusip == "693262"
	replace cusip = "999999" if ticker == "USW"
	
	tab outside_public_boards
	replace  outside_public_boards = 4 if  outside_public_boards == 43 // he has other observations in that year at 4, 4, 3

*** variable formats

	format year %ty
	
	format female  attend_less75_pct -   designated ///
		employment_ceo - gov_comm_mem  interlocking  /// 
		nomchair- nonempchr  other  otherlink  ownless1  prof_services_yn ///
		relative_yn %1.0f
		
*** Director Identification. We need to have a consistent director ID variable.
	
	/* The legacy_dir variable seems to be 1-1 with fullnames (even before 2004 which the documentation indicates is the startdate... backfilled?), but when they are missing for a director, they are missing for all obs for that dir. In these cases, director_detail_id seems to uniquely identify every observation. We'll use this insight.	*/
	
	sort legacy_dir year  director_detail_id
	
	g 	dir_id = legacy_d
	qui sum dir_id
	replace dir_id = `r(max)' + director_d if dir_id == . // prevents ascribing new dir_id's to currently existing ones
		
	/* This half fixes the problem. dir_id cusip year still has duplicates. This is not
	from duplicate directors (i.e. the names are different). This is because some of 
	the problem directors above (with dir_id == .) have the same director_d even though 
	the names are different (not by spelling errors, actually different). So fix this. */
	
	duplicates tag cusip dir_id year, gen(tag)
	replace tag = . if tag == 0
	sort tag full
	egen name_id = group(full) if tag != .
	qui sum dir_id
	replace dir_id = `r(max)' + name_id if tag != .
	drop tag
	
	*** Drop duplicate directorships
	
	sort cusip year full
	duplicates report *
	duplicates drop *, force
	
	sort legacy_pps_id
	duplicates report cusip year full
	duplicates tag cusip year full, gen(tag)
	gsort -tag cusip year full

	drop if fullname == "BUCK A MICKEL" & age == 71 // one observation is duplicated with a bad name

	* the rest of the dups are because 2 firms-years are duplicated with legacy_pps_id duplicated
	* keep the lower valued legacy_pps, which in both cases is consistent with other years for each firm
	
	egen dropgroup = group(tag  legacy_director_id cusip) if tag == 1
	bysort dropgroup (legacy_pps_id): drop if _n > 1 & tag == 1
	
	drop tag dropg // cleanup 
	
*** Fill in missing info (over time and due to inconsistent board meeting info)
	
	/*
	Assume directorship gaps are due to error, and fill in blank intervening years. 
	I.e. someone on the board in 2001 and 2003 should be there in 2002.
		
	Fill in time gaps with stable personal info, assume classification and 
	other linkage variables are constant. Fill forward incrementing variables 
	(like age)
	*/
	
	* first, we need a firm/director pairing id (to tsfill on)
	
	egen dir_firm_id = group(dir_id cusip)	
	order year cusip dir_id dir_firm_id
		
	* tsset the panel based on the firm-dir pairings and time
	
	sort dir_firm_id year
	tsset dir_firm_id year
	tsfill 					// add obs 
	
	*** fill forward "fixed" director traits
	
	g newobs = 1 if cusip ==""
	count if newobs == 1				// 2112 new observations
	
	order  year dir_firm_id meetingdate female indexname
	
	bysort dir_firm_id (year): carryforward cusip - classification  ///
		designated dirsince female grandfath insthold outside_public_boards ///
		priorserv relative_yn former_employee_yn year_of_termination if newobs == 1, replace
	
	*** smartly filling forward for board positions/committees 
	* (only fill in if the prior and next observation agree)
	
	local varfill audit_chair - comp_membership  employment_ceo - employment_vp  gov_comm_mem interlocking nomchair - nonempchr otherlink num_of_shares ownless1 pcnt_ctrl_votingpower prof_services_yn voting year_of_termination year_term_ends
	foreach x of varlist `varfill' {
		bysort dir_firm_id (year): replace `x' = `x'[_n-1] ///
			if `x'[_n-1] == `x'[_n+1] & newobs == 1 	
	}		
	
	*** fill forward incrementing variables (age)
	 
	forval i = 1/10 {
		// just does this replacement multiple times in case a there were multiple gap years
		bysort dir_firm_id (year): replace age = age[_n-1] + 1 if newobs == 1 & age[_n-1] != .
	}
}	
*************************************************************************
* BEGIN: Create director level variables 
*************************************************************************
{
	
*** Committee sizes
	
	bysort cusip year: egen SIZE_COMP = sum(comp_mem)
	bysort cusip year: egen SIZE_NOM = sum(nom_membership )
	bysort cusip year: egen SIZE_AUDIT = sum(audit_membership)
	
*** Director Classification

	/* (See the online appendix doc for more on affliated/linked directors) */

	g dirtype = .
	replace dirtype = 1 if class == "I"
	replace dirtype = 2 if class == "L"
	replace dirtype = 3 if class == "E"
	
	*** Alternative definitions of dirtype, from Guthrie Sokolowsky Wan
	
	gen help1 = (former_emp==1 | business_transa==1 | prof_ser==1 | designated==1 | charity==1 | otherlink==1 | relative==1 | interlock==1) 
	gen help2 = (                business_transa==1 | prof_ser==1 | designated==1 | charity==1 | otherlink==1 | relative==1 | interlock==1) 
	gen help3 = (                business_transa==1 | prof_ser==1 | designated==1 | charity==1 | otherlink==1)                              
	gen help4 = (                                                                                               relative==1 | interlock==1) 
	
	* reclassify all former employees, but with different stringency 
	
	gen diff = year - year_of_termination 
	
	gen dirtype1 		= dirtype 
	replace dirtype1 	= 1 if dirtype1==2 & diff>=3 & diff!=. & help2==0 // most stringent - only reclassify former employees 
	
	gen dirtype2 		= dirtype1
	replace dirtype2	= 1 if dirtype2==2 & help3==1 & help4==0          // least stringent - reclassify all help3-disqualifiers 
	drop help*

	*** Alternative definitions of dirtype, from CG response to GSW
	
	gen dirtype3		= 1 if (dirtype==1 | dirtype==2) & (relative!=1) & (diff>=3)
	replace dirtype3	= 3 if (dirtype==3 | relative==1 | diff<3)	
	
	label variable dirtype  `"From IRRC"'
	label variable dirtype1 `"L to I if 3 yrs only if former emp, not other links"'
	label variable dirtype2 `"L to I if 3 yrs as long as not relative or interlocked"'
	label variable dirtype3 `"I if IRRC says I or L, not relative, 3 or more since emp"'
			
*** director-lvl vars capturing restructing (hire, reclass, lastyear)
	
	*** We need these two vars
	
	sort cusip
	by cusip: egen firstfirmyear = min(year)
	by cusip: egen lastfirmyear = max(year)
	
	sort cusip dir_id year
	by cusip dir_id: egen firstdirfirmyear = min(year)
	by cusip dir_id: egen lastdirfirmyear = max(year)
	
	rename dirtype dirtype0
	
	*** vars at the director level (for each of the different indep defs)
	
	forval i = 0/3 {
	
		g	indep_dir`i' = (dirtype`i' == 1)
		g	notindep_dir`i' = (dirtype`i' != 1)
		
		// switching classification directors	
		
		bysort cusip dir_id (year): g switchto_Ind`i' = 1 if indep_dir`i' == 1 & indep_dir`i'[_n-1] == 0 
		replace switchto_Ind`i' = 0 if year > firstdirfirmyear & switchto_Ind`i' == . 

		by cusip dir_id: g switchto_Not`i' = 1 if indep_dir`i' == 0 & indep_dir`i'[_n-1] == 1 
		replace switchto_Not`i' = 0 if year > firstdirfirmyear & switchto_Not`i' == . 
		
		// Addition of indep and not-indep directors
			
		g 	hire_Ind`i' 		= (indep_dir`i' == 1) if year == firstdirfirmyear & year > firstfirmyear  // . if not the first director firm year and the firm already existed in the data. 1 if indep, 0 if not
		g 	hire_Not`i' 		= 1 - hire_Ind`i'
		
		// Last years of indep and not-indep directors 

		g 	lastyear_Ind`i'		= (indep_dir`i' == 1) if year == lastdirfirmyear & year < lastfirmyear // . if not the last director firm year and the firm exists next year in the data. 1 if indep, 0 if not
		g 	lastyear_Not`i'		= 1 - lastyear_Ind`i'
	}
	
*** board-lvl vars capturing restructing (hire, reclass, lastyear), whose names are CAPITALIZED
		
	forval i = 0/3 {
		
		sort cusip year full
		
		by cusip year: 	egen INDEP`i' = sum(indep_dir`i')
		by cusip year: 	egen NOT_INDEP`i' = sum(notindep_dir`i')
		by cusip year: 	egen SWITCHTO_IND`i' = sum(switchto_Ind`i')
		by cusip year: 	egen SWITCHTO_NOT`i' = sum(switchto_Not`i')
		by cusip year: 	egen HIRE_IND`i' = sum(hire_Ind`i')
		by cusip year: 	egen HIRE_NOT`i' = sum(hire_Not`i')
		by cusip year: 	egen LASTYEAR_IND`i' = sum(lastyear_Ind`i')
		by cusip year: 	egen LASTYEAR_NOT`i' = sum(lastyear_Not`i')
		
		g	ADD_IND`i' 		= SWITCHTO_IND`i' + HIRE_IND`i'
		g	ADD_NOT`i' 		= SWITCHTO_NOT`i' + HIRE_NOT`i'	
		
	}
	
*** check to ensure that each cusip-year is indeed one firm (using ticker and firm name)
	
	sort cusip year
	by cusip year: g temp = 1 if  ticker !=  ticker[_n-1] & _n > 1 & name  !=  name[_n-1] // check on ticker and name to reduce false positives
	by cusip: egen sumdiff = sum(temp)
	
	replace sumdi = . if sumdi == 0
	sum sumdi temp
	keep if sumdi == .
	drop temp sumdi
		
*** adjust variable names to match other datasets 
	
	rename cusip cusip6
	rename year fyear
}	
********************************************************************************
* bring in new independence data, to match exchange dir. classifications 
* note: the new independence data has adtl firm years
********************************************************************************
{
	sort cusip6 fyear dir_id  
	merge 1:1 cusip6 fyear dir_id  using  "$new_indep_data", nogen
	g notindep_dirman = 1-indep_dirman

	*** the new independence data has adtl firm years, so a few variables need to be updated 
	
	drop firstdirfirmyear lastdirfirmyear		// update to reflect new director obs
	bysort cusip dir_id (fyear): egen firstdirfirmyear = min(fyear)
	bysort cusip dir_id (fyear): egen lastdirfirmyear = max(fyear)
	
	drop firstfirmyear lastfirmyear 		// update to reflect new director obs
	bysort cusip (fyear): egen firstfirmyear = min(fyear)
	bysort cusip (fyear): egen lastfirmyear = max(fyear)
	
	*** create all the director level variables	
	
	foreach i in "man" { 

		bysort cusip dir_id (fyear): g switchto_Ind`i' = 1 if indep_dir`i' == 1 & indep_dir`i'[_n-1] == 0  & fyear == fyear[_n-1]+1
		replace switchto_Ind`i' = 0 if fyear > firstdirfirmyear & switchto_Ind`i' == . 

		by cusip dir_id: g switchto_Not`i' = 1 if indep_dir`i' == 0 & indep_dir`i'[_n-1] == 1  & fyear == fyear[_n-1]+1
		replace switchto_Not`i' = 0 if fyear > firstdirfirmyear & switchto_Not`i' == . 
		
		*** Addition of indep and not-indep directors
				
		g 	hire_Ind`i' 		= (indep_dir`i' == 1) if fyear == firstdirfirmyear & fyear > firstfirmyear  // . if not the first director firm year and the firm already existed in the data. 1 if indep, 0 if not
		g 	hire_Not`i' 		= 1 - hire_Ind`i'
		
		*** Last years of indep and not-indep directors (WARNING: This is NOT really firings... this is "Last year but firm doesn't drop from sample...")

		g 	lastyear_Ind`i'		= (indep_dir`i' == 1) if fyear == lastdirfirmyear & fyear < lastfirmyear // . if not the last director firm year and the firm exists next year in the data. 1 if indep, 0 if not
		g 	lastyear_Not`i'		= 1 - lastyear_Ind`i'

	}
		
	*** create firm year aggregates based on  the manual classification
			
	bysort cusip fyear: 	egen SIZE = count(dir_id)

	foreach i in "man" { 

		sort cusip fyear dir_id
		
		by cusip fyear: 	egen INDEP`i' = sum(indep_dir`i')
		by cusip fyear: 	egen NOT_INDEP`i' = sum(notindep_dir`i')
		by cusip fyear: 	egen SWITCHTO_IND`i' = sum(switchto_Ind`i')
		by cusip fyear: 	egen SWITCHTO_NOT`i' = sum(switchto_Not`i')
		by cusip fyear: 	egen HIRE_IND`i' = sum(hire_Ind`i')
		by cusip fyear: 	egen HIRE_NOT`i' = sum(hire_Not`i')
		by cusip fyear: 	egen LASTYEAR_IND`i' = sum(lastyear_Ind`i')
		by cusip fyear: 	egen LASTYEAR_NOT`i' = sum(lastyear_Not`i')
		
		g	ADD_IND`i' 		= SWITCHTO_IND`i' + HIRE_IND`i'
		g	ADD_NOT`i' 		= SWITCHTO_NOT`i' + HIRE_NOT`i'	
				
	}
	
}

********************************************************************************
* BEGIN: output director level data 
********************************************************************************
{	
	sort cusip6 fyear
	
	label variable dir_id 					`"Director ID variable valid over all years"'
	label variable female 					`"Dummy: director is female"'
	label variable outside_public_boards 	`"# of outside_public_boards held by board members"'
	label variable SIZE 					`"Firm Year Level: # of directors on the board"'
	label variable newobs 					`"Filled this observation in for the director-cusip combination"'
	label variable SIZE_COMP				`"Firm Year Level: Size of the compensation committee"'
	label variable SIZE_NOM					`"Firm Year Level: Size of the nominating committee"'
	label variable SIZE_AUDIT				`"Firm Year Level: Size of the audit committee"'  	
		
	foreach i in 0 1 2 3 "man" {
	
		label variable indep_dir`i'   		`"Dummy: director is classified as independent"'
		label variable notindep_dir`i'   	`"Dummy: director is NOT classified as independent"'
		label variable switchto_Ind`i'  	`"director switched classicification to indep from not"'
		label variable switchto_Not`i'  	`"director switched classicification to not indep from indep"'
		label variable hire_Ind`i' 			`"director hired as Independent director"'
		label variable hire_Not`i' 			`"director hired as Not independent director"'
		label variable lastyear_Ind`i' 		`"directors last year and firm will continue, was an Independent director"'
		label variable lastyear_Not`i' 		`"directors last year and firm will continue, was NOT an independent director"'
		label variable INDEP`i'				`"Firm Year Level: # of INDEPendent board members"'
		label variable NOT_INDEP`i' 		`"Firm Year Level: # of NOT_INDEPendent board members"'
		label variable SWITCHTO_IND`i' 		`"Firm Year Level: # of directors that switched classicification to indep from not"'
		label variable SWITCHTO_NOT`i' 		`"Firm Year Level: # of directors that switched classicification to not indep from indep"'
		label variable HIRE_IND`i' 			`"Firm Year Level: # of Independent directors hired"'
		label variable HIRE_NOT`i' 			`"Firm Year Level: # of Not independent directors hired"'
		label variable LASTYEAR_IND`i' 		`"Firm Year Level: # of Independent directors in last year, even though firm exists next year"'
		label variable LASTYEAR_NOT`i' 		`"Firm Year Level: # of Not independent directors in last year, even though firm exists next year"'
		label variable ADD_IND`i' 			`"Firm Year Level: # of Independent directors added since the last year (hires and switched to Not ind)"'
		label variable ADD_NOT`i' 			`"Firm Year Level: # of Not independent directors added since the last year (hires and switched to Ind)"'
	
	}
}
	
	sort cusip6 fyear dir_id	
	
	save "$dir_lvl_out", replace
	
	desc

********************************************************************************
* BEGIN: output board level data 
********************************************************************************
{
	use "$dir_lvl_out", clear
	
	**** Collapse dataset to firm year level

	order *, alpha
	collapse (first) name (mean) outside_public_boards ADD_IND0 - SWITCHTO_NOTman , by(cusip fyear)
			
	**** we have an ADD_* variable. let's also get the LOSE_* variable
	**** and create other variables while we're at it 

	foreach i in "0" "1" "2" "3" "man" {
	
		bysort cusip6 (fyear): g 	LOSE_IND`i'		= LASTYEAR_IND`i'[_n-1] + SWITCHTO_NOT`i'
		bysort cusip6 (fyear): g 	LOSE_NOT`i'		= LASTYEAR_NOT`i'[_n-1] + SWITCHTO_IND`i'
			
		bysort cusip6 (fyear): g 	NONRETURN_IND`i'	= LASTYEAR_IND`i'[_n-1]
		bysort cusip6 (fyear): g 	NONRETURN_NOT`i'	= LASTYEAR_NOT`i'[_n-1] 
		
		g	BOARD_IND`i'		= INDEP`i'/SIZE
		bysort cusip (fyear): g	INDEP_change`i' 		= INDEP`i' - INDEP`i'[_n-1]
		bysort cusip (fyear): g	NOT_INDEP_change`i' 	= NOT_INDEP`i' - NOT_INDEP`i'[_n-1]
		bysort cusip (fyear): g	BOARD_IND_change`i' 	= BOARD_IND`i' - BOARD_IND`i'[_n-1]

	}
			
	**** prep for output
				
	label variable outside_public_boards 	`"mean # of outside_public_boards held by board members"'	
	label variable SIZE 					`"# of directors on the board"'
	label variable name						`"company name"'
	label variable SIZE_AUDIT				`"# of directors on the audit committee"'
	label variable SIZE_COMP				`"# of directors on the compensation committee"'
	label variable SIZE_NOM					`"# of directors on the nominating committee"'
		
	foreach i in "0" "1" "2" "3" "man" {
	
		label variable INDEP`i'				`"# of INDEPendent board members"'
		label variable NOT_INDEP`i' 		`"# of NOT_INDEPendent board members"'
		label variable SWITCHTO_IND`i' 		`"# of directors that switched classicification to indep from not"'
		label variable SWITCHTO_NOT`i' 		`"# of directors that switched classicification to not indep from indep"'
		label variable HIRE_IND`i' 			`"# of Independent directors hired"'
		label variable HIRE_NOT`i' 			`"# of Not independent directors hired"'
		label variable LASTYEAR_IND`i'		`"# of Independent directors in last year, even though firm exists next year"'
		label variable LASTYEAR_NOT`i'		`"# of Not independent directors in last year, even though firm exists next year"'
		label variable ADD_IND`i' 			`"# of Independent directors added since the last year (hires and switched to Not ind)"'
		label variable ADD_NOT`i' 			`"# of Not independent directors added since the last year (hires and switched to Ind)"'
		label variable LOSE_IND`i' 			`"# of Independent directors less since the last year (fired, quit, or switch to Not Ind)"'
		label variable LOSE_NOT`i' 			`"# of Not independent directors less since the last year (fired, quit, or switch to Ind)"'
		label variable NONRETURN_IND`i' 	`"# of Independent directors from last year not on board"'
		label variable NONRETURN_NOT`i' 	`"# of Not independent directors from last year not on board"'
		
		label var BOARD_IND`i' 				"Ratio of IRRC Independent Directors to Total Directors"
		label var INDEP_change`i' 			"INDEP - INDEP [_n-1]"
		label var NOT_INDEP_change`i' 		"NOT_INDEP - NOT_INDEP[_n-1]"
		label var BOARD_IND_change`i'		"BOARD_IND - BOARD_IND[_n-1]"

	}
		
	rename outside_ mean_outside_directorships
	rename name firm_name_in_irrc
	
	order cusip6 fyear firm_name
	sort cusip6 fyear 	
	
	save "$board_lvl_out", replace	
}