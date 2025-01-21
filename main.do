/*

Authors:   Donald Bowen and Jerome Taillard 
Title:     Revisiting Board Independence Mandates: Evidence from Director Reclassifications
		   Review of Finance (2025)
Data:      January 2025 
Versions:  Stata 18.0MP, schemepack v1.4, grc1leg2 v2.26, reghdfe v5.7.3

================================================================================

	Users get a directory:
	
		dir/
			main.do 
			format_ISS_director_data.do
			outputs/
				AnalysisOutput.xlsx 
				log files from running this code
				figures
				Two exhibits as txt files 
			inputs/ 
				It comes with: 
					ISS_cusip6_to_CCM_cusip6.csv
					new_SGA_data.xlsx
					new_independence_data.dta
					psuedo_fy_panel.dta 
					psuedo_cusip_R_t.dta
				
				While running this code, additional files will be created:
					LifeCycles.dta 
					event-level_CAPM.dta 
					permno_dates_for_eventus.txt
			
================================================================================
			
	To run this analysis, users need to 
	
		1. Obtain licensed input files and specify paths to them (see below)
		
		2. If needed, install some stata packages (see top of code below)
		
		3. Table 8 and IA4 download some data directly from WRDS using obdc. 
		   Windows instructions to set up a WRDS-Stata connection with obdc 
		   are here: https://www.stata.com/blog/wrds/Stata_WRDS.pdf
		   
		4. During the run of this code, creating Table IA3 involves a 
		   one-time manual intermediate step, where users go to Event Study
		   by WRDS, upload file, set some options, and download CARs. More 
		   instructions below. 

================================================================================
	
	Inputs users must have licenses for/access to:
	
		ccm                         (ccm_annual_1960_2012_full.dta)
		crsp_monthly                (crsp_monthly_1960_2011_full.dta < to get historical firm exchange listing location)
		ISS/riskmetrics             (directors_legacy_all.dta)
		execucomp                   (execu_1992-2012_full.dta < for Table 12)
		SDC completed mergers       (sdc_acq_completed.dta < for an appendix table)
		
================================================================================
		
	CODE OUTLINE:
	
 		parameters and set up
 	
 		fmt execucomp
	
		create board and dir level data
				
 		define treatment and reclassifier firms
		
 		create firm-year panel
					
		analysis				
		
*/

	log using outputs/log_FormatAndPrepData, text replace 

*************************************************************************
* BEGIN: presets
*************************************************************************
	
	* if needed, install:
	*ssc install reghdfe
	*ssc install schemepack
	*ssc install grc1leg2
	
	* settings
	set rmsg on
	set more off, perm

	set matsize 800
	set scrollbufsize 500000		
	capture program drop _all
	clear
	clear matrix
	
	*************************************************************************
	* BEGIN: USERS NEED TO MODIFY THIS, THE REMAINDER OF FILE WILL RUN
	* Obtain these files from WRDS
	*************************************************************************
	
	/* set using_fake_data equal to 0 or 1
	
	   if using_fake_data = 1, then
			the code will skip all data creation steps (LifeCycles, Execucomp, 
			formatting director data, create firm-year panel)
			and skip making Table 1 (which happens when we create firm-year panel)
			and skip exhibits based on WRDS downloads or data files we didn't 
			create (Tables 8, 9, 10, 11, IA1, IA3, IA4 lpus Figues 1 and IA1)
			
	   if using_fake_data = 0, then
			you must have all of the inputs files obtained, downloaded, and set 
			the paths below to point to the files 
	*/
	global using_fake_data    0          // put 1 or 0 here  
	assert inlist($using_fake_data, 0,1) // leave as-is 
	
	* set input paths for user obtained files 	  
	global ccm_raw            "inputs_noupload/ccm_annual_1960_2012_full"
	global crsp_monthly       "inputs_noupload/crsp_monthly_1960_2011_full"
	global dir_raw            "inputs_noupload/directors_legacy_all"
	global execu_raw          "inputs_noupload/execu_1992-2012_full"
	global completed_mergers  "inputs_noupload/sdc_acq_completed"
		
	*************************************************************************
	* END: USERS NEED TO MODIFY THIS, THE REMAINDER OF FILE WILL RUN
	*************************************************************************
	
	* provided inputs 
	global new_indep_data     "inputs/new_independence_data"
	global iss_ccm_cusip      "inputs/ISS_cusip6_to_CCM_cusip6.csv"
	global sga_handcollection "inputs/new_SGA_data.xlsx"
	
	* set temp folder
	global temp               "temp" // inside cd but can be changed elsewhere easily
	*!rmdir "$temp"  /R /q           // delete entire temp folder and contents for fresh start
	cap mkdir "$temp"                // create temp folder if non-existant 
	
	* derived data (board) will go here, with these file names
	global board_lvl_out      "$temp/board_level_data"   
	global dir_lvl_out        "$temp/dir_level_data"     
	
	* for users running this code without all licensed data 
	if $using_fake_data == 1 {
		copy inputs/psuedo_fy_panel.dta    temp/fy_panel.dta  , replace
		copy inputs/psuedo_cusip_R_t.dta   temp/cusip_R_t.dta , replace
	}
	 
	* set output paths  
	global output_folder      "outputs"
	global excel_filename     "$output_folder/AnalysisOutput.xlsx"
	cap mkdir "$output_folder"  
		
	* any global analysis params 
	
	global 	extra_covariates l.l_a l.td_a l.capx_a l.xrd_a L.SIZE L.BOARD_IND L.incoming_frac L.outgoing_frac
		
*************************************************************************
* BEGIN: function defs 
*************************************************************************
{		
	* Winsor program (winsor varname [1,5])
	capture program drop winsor
	program define winsor
		quiet sum `1', detail
		replace `1' = r(p1)  if `1' ~= . & (`1' < r(p1))  & `2' == 1
		replace `1' = r(p99) if `1' ~= . & (`1' > r(p99)) & `2' == 1
		replace `1' = r(p5)  if `1' ~= . & (`1' < r(p5))  & `2' == 5
		replace `1' = r(p95) if `1' ~= . & (`1' > r(p95)) & `2' == 5
	end	
	
	* to make the tables prettier and more informative	
	cap prog drop myPostEst
	prog def myPostEst
		sum Y if e(sample)
		estadd scalar meanY `r(mean)'		
		foreach v in Rpt_Comm Rt_Comm Rp pt_Comm R p t_Comm {
			count if e(sample) & `v' == 1
			estadd scalar `v' = `r(N)'
		}
	end
	
	* this will define DDD vars flexibly 
	* (e.g. in case R/t/p definitions need to change)
	* e.g. falsification
	cap prog drop define_DDD_vars 	
	prog define define_DDD_vars 	
		*example: define_DDD_vars -5 5 2003
		*what the args do:
		*close = inrange(DIST_2001,`1',`2'), where DIST_2001 equals NOT_INDEP(2001) - INDEP(2001)
		*DIST_2001, and therefore close, are defined for all years in a firm's life
		*p = (fyear >= `3')
		
		g close = inrange(DIST_2001,`1',`2') // inclusive 
		tab DIST_2001 close

		* t_Comm is "NC_i" in paper  ("treatment", defined including committee compliance)

		g p   		        = (fyear >= `3')
		g Rp  		        = R*p
		g pt_Comm  		    = p*(t == 1 | t_Comm == 1)
		g Rt_Comm  		    = R*(t == 1 | t_Comm == 1)
		g Rpt_Comm 		    = R*p*(t == 1 | t_Comm == 1)	
			
		lab var R      		"\$ R_i \$"
		lab var p     		"\$ P_t \$"
		lab var Rp     		"\$ P_t \times R_i \$"
		lab var Rpt	   		"\$ R_i \times P_t \times NC_{i}^{b} \$"
		
		lab var t_Comm      "\$ NC_{i} \$"                        //  "\$ NC_{i}^{b|c} \$"
		lab var pt_Comm     "\$ P_t \times NC_{i}\$"              //  "\$ P_t \times  NC_{i}^{b|c} \$"
		lab var Rt_Comm     "\$ R_i \times NC_{i} \$"             //  "\$ R_i \times NC_{i}^{b|c} \$"
		lab var Rpt_Comm	"\$ P_t \times R_i \times NC_{i} \$"  //  "\$ R_i \times P_t \times NC_{i}^{b|c} \$"
	end	
	
	cap prog drop esttab_to_excel_sheet
	prog def esttab_to_excel_sheet
	syntax using, sheet_name(string) esttab_options(string) temp_csv(string) [notes(string)]
	/*
	DESCRIPTION: 
	A wrapper for esttab that will put the results into a sheet of an
	Excel file. 

	For some reason I'm not spending the time to discover, esttab's option
	addnotes("Note 1" "Note 2" "Note 3") doesn't work properly. Thus, I coded 
	additional functionality (the notes option) to allow the user to add notes. 

	USAGE:
		[run pre- esttab commands]
		esttab_to_excel_sheet using "hey.xlsx", ///
			sheet_name("sheet1") ///
			temp_csv_filename("temp12345.csv") ///
			esttab_options( star(* 0.10 ** 0.05 *** 0.01 ) label ) ///	
			notes(`""Note 1" "Note 2""')
		
	INPUT NOTES:
		using            Must end in ".xlsx"
		sheet_name       String name valid for Excel sheet name
		esttab_options   Any valid esttab options. Do not use addnotes() - weird behavior
		temp_csv         File name. Must end in ".csv". 
		notes            Optional notes. To get multiple notes, encase each in 
						 quotes, and the whole set of notes in compound quotes.
						 For example: `""Note 1" "Note 2""'
	*/
	qui {
		noi esttab, `esttab_options' // show the user the results

		cap erase "`temp_csv'"
		esttab using "`temp_csv'", `esttab_options' // save a temp file

		preserve 
			import delimited using "`temp_csv'" /// open the temp file
			, clear delim(",=",collapse) bindq(strict) stripq(yes)
			
			// these lines add notes in column 1 (which is blank), 
			// because esttab in this setting handles multiple notes weirdly
				tostring v1, replace
				replace v1 = ""
				replace v1 = "NOTES:" in 1
				local i = 2
				foreach part in `notes' {
					replace v1 = "`part'" in `i'
					local i = `i' + 1			
				}		

			cap export excel `using', sheet("`sheet_name'") sheetreplace // save to excel sheet 
		restore
		
		erase "`temp_csv'" // delete temp file
	}
	end
}	
*************************************************************************
* BEGIN: download lifecycle data 
*************************************************************************
	
capture confirm file "inputs/LifeCycles.dta"
if _rc==0 { // make the file only if not made
	di "Skipping, already made"
}
else if $using_fake_data == 0 {
	cd "inputs"

	local url "https://faculty.marshall.usc.edu/Gerard-Hoberg/HobergMaxLifeCycles/idata/LifeCycleDatabase.zip"
	copy `url' LifeCycleDatabase.zip
	unzipfile LifeCycleDatabase.zip

	insheet using "LifeCycleDatabase_ext2021.txt", clear // this is inside the zip we just extracted 
	rename year datadateyear  
	sum
	save "LifeCycles", replace	
	
	/* delete the zip files */

	local datafiles: dir "" files "*.zip"

	foreach datafile of local datafiles {
		di "`datafile'"
		rm `datafile'
	}

	/* delete txt files unless it starts with readme */
	
	/* If you run this after later steps, it might delete txt files we want.
	So let's skip this clean up step...
	local datafiles: dir "" files "*.txt"

	foreach datafile of local datafiles {
		if substr("`datafile'",1,6) != "readme" {
			rm `datafile'			
		}
	}
	*/ 
	cd ../
}
	
*************************************************************************
* BEGIN: execucomp > $temp/gvkey_fyear_ceoinfo, gvkey-fyear level, with info on CEO pay
*************************************************************************

capture confirm file "$temp/gvkey_fyear_ceoinfo.dta"
if _rc==0 { // make the file only if not made
	di "Skipping, already made"
}
else if $using_fake_data == 0 {
**** prep for execumcomp work, we'll want some ccm info

	use gvkey lpermno datadate cusip fyear if fyear > 1993 using "$ccm_raw", clear
	
	destring gvkey, replace
	rename datadate fye
	rename lpermno permno

	**** clean dataset to allow tsset - get rid of multiple obs for gvkey/fyear combo all obs have missing assets anyway 
	
	duplicates report gvkey fyear
	duplicates report gvkey fyear fye
	duplicates drop gvkey fyear fye, force // if the fye date is the same, doesn't matter which I keep
	
	sort gvkey fyear (fye)
	tsset gvkey fyear
	sort gvkey fyear
	
   **** identify gvkey'R fiscal year begin date, b/c it is no longer available in CCM
   * assume it is one day after last year'R fye date 
   * if prior year fye is missing (e.g. first year gvkey is in sample), then assume it is 365 days ago
   
 	g	fyb	=	L1.fye + 1
	replace fyb	=	fye - 364 if L1.fye==.
	
	g	dayb = day(fyb) /* make sure that all fyb begin on first day of month */
	tab dayb
	replace fyb = 	fyb - 1 if dayb==2	
	drop dayb
	
	tempfile ccm_temp_fmt_for
	save `ccm_temp_fmt_for', replace
	
**** execucomp
	
	use "$execu_raw", clear
	
	// WILL BE USING THE LATEST CCM AS WELL TO OBTAIN fiscal year end and fiscal year beginning dates
	
	rename year fyear

	drop cusip-sic
	
	destring gvkey, replace
	destring execid, replace

	keep if (gvkey!=.) & (fyear!=.) & (execid!=.)
	
	sort gvkey fyear	
	
**** bring in some extra info from CCM 

	sort gvkey fyear co_per_rol
	merge m:1 gvkey fyear using `ccm_temp_fmt_for', nogen keep(1 3)	
	g	datadate = fye
	label var datadate "CCM/COMPUSTAT Datadate variable"	
	
*** we want ceo observations... the execucomp FAQ says ceoann is the historical variable of use
	
	*** how many obs are in firm years without ceo obs?
	
	replace ceoann="1" if ceoann=="CEO"
	replace ceoann="." if ceoann=="CFO"
	destring ceoann, replace
	bysort gvkey fyear: egen test=sum(ceoann)
	bysort gvkey fyear: replace test = . if _n > 1
	tab test	
	drop test
	bysort gvkey fyear: egen miss=sum(ceoann)	
	
	*** augment with becameceo variable
	
	gen help1=.
	replace help1 = 1 if becameceo <= fyb & leftofc >= fye /* CEO for full year */
	replace help1 = 2 if becameceo <= fyb & leftofc >= fyb   & leftofc < fye /* CEO at fyb, but not at fye */
	replace help1 = 3 if becameceo > fyb  & becameceo <= fye & leftofc >= fye /* CEO at fye, but start after fyb */
	replace help1 = 4 if becameceo > fyb  & becameceo <= fye & leftofc < fye /* start after fyb, going before fye */
	
	* pick one alternative CEO in firm-years without designated ceoann 
	
	gen help2=. // length of days as ceo
	replace help2=fye-fyb if help1==1
	replace help2=leftofc-fyb if help1==2
	replace help2=fye-becameceo if help1==3
	replace help2=leftofc-becameceo if help1==4
	bysort gvkey fyear (help2): egen help3=rank(help2), field // help3 is a ranking of help2 for firm-year, might be multiple that are coded as 1
	replace ceoann=1 if help3==1 & miss==0	
	
	* break ties of co-CEOs: rank by tdc1, tdc2, co_per_rol - take the highest one
	
	bysort gvkey fyear: egen test=sum(ceoann)
	tab test // 85 years with multiple CEOs
	bysort gvkey fyear (tdc1 tdc2 co_per_rol): egen help4=seq() if test==2 & ceoann==1 & miss==0
	replace ceoann=. if help4==1 // help 4 = 2 is the person with higher salary
	bysort gvkey fyear: egen test2=sum(ceoann)
	tab test2
	drop help* test* miss
		
**** prep to output

	order gvkey fyear execid
	sort gvkey fyear execid
	
	duplicates report gvkey fyear
	duplicates report  gvkey fyear execid
	   
	******************** ONLY SAVE CEO INFO ***************
	keep if ceoann==1 
	duplicates report  gvkey fyear 	
	sort gvkey fyear
	save "$temp/gvkey_fyear_ceoinfo", replace	
}
*************************************************************************
* BEGIN: create board and dir level data > $board_lvl_out
*************************************************************************

capture confirm file "$board_lvl_out.dta"
if _rc==0 { // make the file only if not made
	di "Skipping, already made"
}
else if $using_fake_data == 0 {
	do format_ISS_director_data.do 
}	

*************************************************************************
* BEGIN: define non-compliant and reclassifier firms
*************************************************************************

if $using_fake_data == 0 {

	* first, these are matches where the cusip6 fails (bc cusip6 in CCM and ISS disagree),
	* but firmname and other info confirms a match
	* we will use this lookup table to change the cusip6 in irrc before the merge to CCM
	
	import delim using "$iss_ccm_cusip", clear varn(1)	
	keep cusip*
	rename cusip6_irrc cusip6 
	tempfile ccm_cusip6_lookup
	save `ccm_cusip6_lookup', replace

	* define committee level non-compliance
	
	use "$dir_lvl_out", clear
	rename *man *

	g nom_indep   = indep_dir if nom_membership == 1
	g comp_indep  = indep_dir if comp_membership == 1
	g audit_indep = indep_dir if audit_membership == 1
	g gov_indep = indep_dir   if gov_comm_mem == 1
	
	collapse nom_indep comp_indep audit_indep gov_indep, by(cusip6 fyear)
	
	keep if fyear == 2001 
	keep if aud < 1 | comp < 1| nom < 1 | gov < 1
	keep cusip6
	duplicates drop *, force	
	tempfile firms_with_NC_committee	
	save `firms_with_NC_committee', replace 

	* board level reclass and non-compliance with independence majority
	
	use "$board_lvl_out", clear
	rename *man * // man suffix denotes IRRC variable with manual checks for independence
	
	g R            =  SWITCHTO_IND        if fyear >= 2002 & fyear <= 2006	// sum this over years to get reclassifier
	g DIST_2001    =  NOT_INDEP - INDEP   if fyear == 2001
	g t            =  INDEP < NOT_INDEP   if fyear == 2001
	
	collapse (mean) R DIST_2001 t (lastnm) firm_name_in_irrc, by(cusip6)
	replace R = R > 0 if !missing(R)
	
	merge 1:1 cusip using `firms_with_NC_committee', keep(1 3)
	g t_Comm       = t == 1 | _m == 3 // t_Comm is NC_i in the paper
	drop _m 
	
	* require avail info
	
	egen nmiss = rowmiss(*)
	drop if nmiss>0	
	drop nmiss

	order cusip6 DIST_2001 R t t_Comm
	
	* update cusip6 to match CCM'R 6 digit cusip 
	merge 1:1 cusip6 using `ccm_cusip6_lookup'
	replace cusip6 = cusip6_ccm if _m == 3 & !missing(cusip6_ccm)
	drop cusip6_ccm _m 

	save "$temp/cusip_R_t.dta", replace
}	
*************************************************************************
* BEGIN: create firm-year panel > $temp/fy_panel
* build Table 1 as we go (Sample Selection Criteria aka "waterfall")
*************************************************************************

if $using_fake_data == 0 {
	
	* get gvkey fyear xsga from manual work (doesn't impact conclusions, but is correct data)

	import excel using "$sga_handcollection", clear ///
		 cellrange(A1:P522)
	foreach v of varlist * {
	   local vname = strtoname(`v'[1])
	   rename `v' `vname'
	}
	drop in 1
	keep gvkey fyear xsga2 
	destring *, replace
	tempfile new_sga_data
	save `new_sga_data'	
		
	* start waterfall "log" file 

	cap file close waterfall
	file open waterfall using "$output_folder/T1_waterfall.txt", write text replace

	file write waterfall "\begin{tabular}{lcc}" _n
	file write waterfall "\toprule" _new

	file write waterfall "Step " _column(55) "& " "N " _column(70) "& " "Number of firms \\" _n
	file write waterfall "\midrule" _new

*** load CCM 

{
	// start from CCM
	use if fyear >= 1997 & fyear <= 2006 using "$ccm_raw", clear
	
	// initial panel without  duplicates
	destring gvkey, replace
	duplicates drop gvkey fyear, force 

	distinct gvkey if fyear >= 1999
	file write waterfall "CRSP-Compustat Merged (CCM) fiscal year 1999-2006 " _column(55) "& "  %9.0fc (`r(N)') " & "_column(70)  %9.0fc (`r(ndistinct)') " \\"	 _n

	// drop financial outliers
	drop if (at < 5) | (sale < 5) | (dlc + dltt > at) 
	noi di "low at sale, high lev"	
	noi	distinct gvkey if fyear >= 1999  // low at sale, high lev 
		
	distinct gvkey if fyear >= 1999
	file write waterfall "Assets and Sales \$\geq\$ \\$5m, book leverage \$\leq\$ 1 " _column(55) "& "  %9.0fc (`r(N)') " & "_column(70)  %9.0fc (`r(ndistinct)') " \\"	 _n
	
	// must be NYSE or NASDAQ   
	// CCM exchg is header (current, as of download!) not historical
	// use crsp exchcd instead 

	preserve 
		use permno date exchcd if year(date) >= 1997 & year(date) <= 2006  /// 
			using "$crsp_monthly", clear
				
		g fyear = year(date)	
		rename permno lpermno
		collapse (firstnm) exchcd , by(lp fy)
		tempfile perm_year_exchcd 
		save `perm_year_exchcd'
	restore
		
	bysort lpermno fyear: drop if _N == _n & _N > 1 // drops 2 obs for firms that moved fiscal report form june to dec 1998 for 1998 fyear 
	merge 1:1 lpermno fyear using  `perm_year_exchcd', keep(1 3) nogen
	keep if exchcd == 1 | exchcd == 3
	noi di "nyse / nasdaq firms"			
	noi	distinct gvkey if fyear >= 1999
	// nyse/nasdaq
		
	distinct gvkey if fyear >= 1999 
	file write waterfall "NYSE and NASDAQ listed firm-years " _column(55) "& "  %9.0fc (`r(N)') " & "_column(70)  %9.0fc (`r(ndistinct)') " \\"	 _n	
	
	// require sufficient prof_a observations 
	// count in pre >= 3 and count in post >= 3

	xtset gvkey fyear
	g	prof_a				= oibdp/at
	drop if missing(prof_a)	
	
	g temp1 = fyear if !missing(prof_a) & fyear <= 2002 & fyear >= 1999
	g temp2 = fyear if !missing(prof_a) & fyear >  2002
	egen count1 = count(temp1), by(gvkey)
	egen count2 = count(temp2), by(gvkey)
	keep if count1 >= 3 & count2 >= 3  
	drop temp1 temp2 count1 count2

	noi di "prof_a non-missing requirements" 
	noi	distinct gvkey if fyear >= 1999	
	noi	tab fyear        
		
	distinct gvkey if fyear >= 1999 
	file write waterfall "Firm has at least 3 non-missing ROA in pre and post " _column(55) "& "  %9.0fc (`r(N)') " & "_column(70)  %9.0fc (`r(ndistinct)') " \\"	 _n
	
	// sic restriction
	
	destring sic, replace
	replace sich = sic if sich == . 
	drop sic
	rename sich sic
	g sic3 = floor(sic/10)	
	g sic2 = floor(sic3/10)
	drop if (sic3 >= 490 & sic3 <= 494) | (sic3 >= 600 & sic3 <= 699) 
	noi di "sic req"			
	noi	distinct gvkey if fyear >= 1999 // sic 

	distinct gvkey if fyear >= 1999 
	file write waterfall "Drop SIC 49 and 60-69 " _column(55) "& "  %9.0fc (`r(N)') " & "_column(70)  %9.0fc (`r(ndistinct)') " \\"	 _n
		
	// merge in execucomp vars
	
	merge 1:1 gvkey fyear using "$temp/gvkey_fyear_ceoinfo", keep(1 3) nogen ///
		keepusing(tdc1 tdc2 total_curr option_awards_blk_value)
	
	// update sga data
	
	noi merge 1:1 gvkey fyear using `new_sga_data'
	replace xsga = xsga2 if _m == 3
	drop _m xsga2
	
	// define CCM vars and labels
	
	g   prof_sale           = oibdp / sale	
	g 	l_a                 = log(at)
	g   xrd_a               = xrd  / at 
	replace xrd_a = 0 if xrd_a == .
	g   capx_a              = capx / at
	g	td_a				= (dlc+dltt)/at	
	g	mve					= csho*prcc_f
	g	td_mve				= (dlc+dltt)/(dlc+dltt+mve)
	g	sga_a				= xsga/at // denom: l_sale  in lrz
	g	cash_a 				= che/at
	g	q					= (at - ceq + (csho*prcc_f) - txdb)/at // KZ 1997
 
	* profit margin decomps 
	g 	cogs_sale           = cogs/sale
	g 	sga_sale            = xsga/sale
	g 	xrd_sale            = xrd/sale		
 
	* working cap mgmt 
 	g	fixed_asset_turn    = sale/ppent
	g	asset_turn          = sale/at 
	g	ar_turn             = sale/rect
	g	invt_turn           = cogs/invt 
	g	ap_turn             = cogs/ap
 	g 	dioh    			= invt/(cogs/365)
	g 	dso     			= rect / (sale/365)
	g 	ap_days 			= ap / (cogs/365)
	*ccc defined below after winsorizing dso, dioh, ap_days  

	* employee productivity panel 
 	xtset gvkey fyear
	g 	prof_emp  		 	= oibdp/((emp + L.emp)/2)  
	g 	sale_emp  		 	= sale /((emp + L.emp)/2) 
	g 	l_emp     		 	= log(   (emp + L.emp)/2) 	
	g 	sales_g    		 	= D.sale/L.sale
	foreach v in sale_emp prof_emp {
		replace `v' = `v'/1000 // scale /EMP variables, so that they are $ per emp. 
		// sale, prof, and ppent are in millions, while emp is in thousands
	}
 
	* used in online apx table
	g gdwlip_impairment_bool = !missing(gdwlip)
 
	lab var td_a         		"Book Leverage (DLC + DLTT)/at"
	lab var td_mve       		"Mkt Leverage (DLC+DLTT)/(DLC+DLTT+MVE)"
	lab var l_a          		"Log Book Assets"
	lab var prof_a       		"Profitablity oibdp/at"
	lab var q            		"Q: (at - ceq + (csho*prcc_f) - txdb)/at"
	lab var prof_a           	"ROA"
	lab var prof_sale        	"Profit Margin"
	lab var l_a              	"Log(Assets)"
	lab var td_a             	"Book Leverage"
	lab var xrd_a            	"R&D / Assets"
	lab var capx_a           	"CAPX / Assets"	
	lab var tdc1 	 	     	"TDC1"
	lab var tdc2		     	"TDC2"
	lab var total_curr       	"Salary+Bonus"
	lab var option_awards_blk	"OptionsGranted"
	
	* winsorize CCM based variables 
	
	foreach var in prof_a prof_sale total_curr tdc1 tdc2 option_awards_blk_value /*
	*/ l_a xrd_a capx_a td_a td_mve sga_a cash_a q fixed_asset_turn asset_turn ar_turn invt_turn ap_turn /*
	*/ l_emp sales_g sale_emp prof_emp cogs_sale sga_sale xrd_sale dioh dso ap_days {
		cap winsor `var' 1
	}	
	
	g ccc = dioh + dso - ap_days	
	
	// must be in irrc, with t and R defined 
	// the CUSIPs in the board data do not match to multiple gvkeys, so we can just go ahead and merge
	
	g	cusip6				= substr(cusip,1,6)
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	duplicates report cusip6 fyear // < proves that CUSIPs in the board data do not match to multiple gvkeys
	
	sort gvkey fyear	
	noi di "in irrc and can define t and R and DIST_2001"		
	noi	distinct gvkey if fyear >= 1999	// in irrc (2001 and one of 2002-2006)
	noi	tab fyear       // in irrc (2001 and one of 2002-2006)

	distinct gvkey if fyear >= 1999 
	file write waterfall "In IRRC database and \$R_i\$ and \$NC_i\$ defined " _column(55) "& "  %9.0fc (`r(N)') " & "_column(70)  %9.0fc (`r(ndistinct)') " \\"	 _n

	// merge in board-level vars 
	
	preserve 
		* to update cusip6 to match CCM'R 6 digit cusip 
		import delim using "$iss_ccm_cusip", clear varn(1)	
		rename cusip6_irrc cusip6 
		tempfile ccm_cusip6_lookup
		save `ccm_cusip6_lookup', replace

		* update cusip6 to match CCM'R 6 digit cusip 
		use "$board_lvl_out", clear
		merge m:1 cusip6 using `ccm_cusip6_lookup'
		replace cusip6 = cusip6_ccm if _m == 3 & !missing(cusip6_ccm)
		drop cusip6_ccm _m 
		rename *man *

		tempfile board_stats
		save `board_stats', replace
	restore
	
	merge m:1 cusip6 fyear using `board_stats', keep(1 3) nogen
	
	g incoming_dir = HIRE_IND + HIRE_NOT
	g outgoing_dir = LASTYEAR_IND + LASTYEAR_NOT
	
	foreach v in incoming outgoing {
	g `v'_bool = `v'_dir > 0 if !missing(`v'_dir)
	g `v'_frac = `v'_dir / SIZE 
	}	
	
	// keep enough id info, and created vars 
	
	qui ds
	local last_var = word("`r(varlist)'", `c(k)') // gets name of last var
	keep gvkey-conm  cik        /// ID vars from CCM
	     prof_a-`last_var'   //  our first created var is prof_a 
	
	// add lifecycle vars
	
	g datadateyear = year(datadate)
	merge m:1 gvkey datadateyear using inputs/LifeCycles, keep(1 3) nogen
	sort gvkey fyear	
	
	// drop t R so that you must be explicit every time you load panel
	drop R t t_Comm firm_name_in_irrc
	save "$temp/fy_panel", replace
}

	* finish waterfall table - get to baseline regression

	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 

	define_DDD_vars -5 5 2003 
	distinct gvkey if close  & fyear >= 1999
	file write waterfall "Less than 3 director replacements from 50\% board independence in 2001 " _column(55) "& "  %9.0fc (`r(N)') " & "_column(70)  %9.0fc (`r(ndistinct)') " \\"	 _n

	xtset gvkey fyear	
	noi reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		

	distinct gvkey if e(sample)
	file write waterfall "In baseline regression (due to fixed effects)" _column(55) "& "  %9.0fc (`r(N)') " & "_column(70)  %9.0fc (`r(ndistinct)') " \\"	 _n

	file write waterfall "\end{tabular}" _n

	file close waterfall
}

*************************************************************************
* BEGIN: ANALYSIS
*************************************************************************	

	log close _all 
	log using outputs/log_Analysis, text replace

****************************************************************
* Fig 1: Board stats over time by group 
****************************************************************	
if $using_fake_data == 0 {
	
	use "$dir_lvl_out", clear	
	collapse (sum) former_emp, by(cusip6 fyear)
	tempfile former_emp_count
	save `former_emp_count'
	
	* to update cusip6 to match CCM'R 6 digit cusip (so we can merge in R and t_Comm)
	import delim using "$iss_ccm_cusip", clear varn(1)	
	rename cusip6_irrc cusip6 
	tempfile ccm_cusip6_lookup
	merge 1:m cusip6 using `former_emp_count'
	replace cusip6_ccm = cusip6 if missing(cusip6_ccm)
	drop cusip6 _m firm_name* // the irrc cusip6
	rename cusip6 cusip6
	mdesc
	tempfile former_emp_count
	save `former_emp_count'
		
	* now load firm-year data
	
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	eststo clear
	
	* reduce to main sample
	
	reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey i.fyear)  absorb(gvkey i.fyear#i.sic2)		
	keep if e(sample)
	
	* bring in former_emp_count on board 
	
	merge 1:1 cusip6 fyear using `former_emp_count', keep(1 3)
	mdesc SIZE INDEP former if _m == 1
	rename former former //shorter name 
	format former %9.3g  //display format, will end up as ylabel in graph 

	* turn R and t into a single variable (for collapse/separate/plotting)
	
	egen group = group(R t_Comm)
	tab group R
	tab group t_Comm
	
	lab define groups 1 "NonReclass Comp" ///
	                  2 "NonReclass NonComp" ///
					  3 "Reclass        Comp"  ///
					  4 "Reclass        NonComp", replace
	lab var group ""
	lab values group groups
		
	* when does the former employee ratio become different statistically?
	preserve 
		replace former = former/SIZE
		// via DD within NC: 2002->
		reghdfe former i.fyear##(c.R) if t_Comm == 1, a(gvkey) vce(cluster gvkey)
	restore
			
	* LASTYEAR not valid in 2006  (no 2007 gov data)
	foreach v in LASTYEAR_IND LASTYEAR_NOT {
		replace `v' = . if fyear == 2006 
	}
	
	rename INDEP0 ISS_INDEP
	
	* create remaining variables: # on board now also on board next year
	
	bysort gvkey (fyear): g REMAIN__   = SIZE-LASTYEAR_IND-LASTYEAR_NOT
	bysort gvkey (fyear): g REMAIN_IND = INDEP-LASTYEAR_IND
	bysort gvkey (fyear): g REMAIN_NOT = NOT_INDEP-LASTYEAR_NOT
	
	bysort gvkey (fyear): g NEW__      = HIRE_IND+HIRE_NOT
	
	* set up figs 
	
	local figs SIZE BOARD_IND ISS_INDEP  REMAIN__   REMAIN_IND REMAIN_NOT NEW__ HIRE_IND HIRE_NOT former
	
	* get yearly avgs (by group)
	
	collapse `figs' NOT_INDEP INDEP, by(fyear group)
	lab var fyear "Year"
	
	* sep by group to distinct variables for plotting (eg. line SIZE1 SIZE2 SIZE3 SIZE4 fyear --> 4 lines)
	
	foreach v in `figs' NOT_INDEP INDEP {
	rename `v' temp
	separate temp, g(`v') by(group) short
	drop temp
	}
	
	* convert vars to percentage of (relevant) size
	forval k = 1/4 {
		replace former`k'     = 100*former`k' / SIZE`k'		
		replace REMAIN__`k'   = 100*REMAIN__`k' / SIZE`k'		
		replace REMAIN_IND`k' = 100*REMAIN_IND`k' / INDEP`k'
		replace REMAIN_NOT`k' = 100*REMAIN_NOT`k' / NOT_INDEP`k'
		replace NEW__`k'      = 100*NEW__`k' / SIZE`k'		
		replace HIRE_IND`k'   = 100*HIRE_IND`k' / INDEP`k'
		replace HIRE_NOT`k'   = 100*HIRE_NOT`k' / NOT_INDEP`k'
		replace BOARD_IND`k'  = 100*BOARD_IND`k'
		replace ISS_INDEP`k'  = 100*ISS_INDEP`k'  / SIZE`k'	
	}
		
	* these labels will be the titles of the subgraphs 
	lab var SIZE1       "(1) BoardSize"
	lab var BOARD_IND1  "(2) Exchange Independence"
	lab var ISS_INDEP1  "(3) ISS Independence"
	lab var REMAIN__1   "(5) Retaining Dirs (All)"  
	lab var REMAIN_IND1 "(6) Retaining {it:I}"  
	lab var REMAIN_NOT1 "(7) Retaining {it:N}"
	lab var NEW__1      "(8) Hiring Dirs (All)" 
	lab var HIRE_IND1   "(9) Hiring {it:I}"  
	lab var HIRE_NOT1   "(10) Hiring {it:N}"  
	lab var former1     "(4) Former Employees"
	
	* use val labels for years, because '99 can't be used within xlabel() rules due to syntax parsing 
	label define year_lbl 1999 "'99" 2000 "'00" 2001 "'01" 2002 "'02" 2003 "'03" 2004 "'04" 2005 "'05" 2006 "'06"
	label values fyear year_lbl
	
	* do subgraphs
	local i = 1 
	foreach v in `figs' {
		
		// modify y options (scale, label, title)

		local yopts 
		if "`v'" == "SIZE" {
			local yopts yscale(range(6 10)) ylabel(6(1)10)
		}
		if inlist("`i'","4","5","6") {
			local yopts yscale(range(86 94)) ylabel(86(2)94)			
		}
		if inlist("`i'","7","8","9") {
			local yopts yscale(range(0 16)) ylabel(0(4)16)			
		}
		if inlist("`i'","2","3") {
			local yopts yscale(range(50 80)) ylabel(50(5)80)			
		}
		if inlist("`i'","2","3","4","7", "10") {
			local yopts `yopts' ytitle("% of All Directors", size(medium))
		}
		if inlist("`i'","5","8") {
			local yopts `yopts' ytitle("% of {it:I} Directors", size(medium))
		}
		if inlist("`i'","6","9") {
			local yopts `yopts' ytitle("% of {it:N} Directors", size(medium))
		}		
	
		// modify x options (scale, label, title)
	
		local xopts xscale(range(1999 2006)) xlabel(1999(1)2006, valuelabel) xtitle("")
		
		// plot 
		
		#delimit ;	
			line `v'* fyear if fyear >= 1999, title("`: variable label `v'1'", size(medium)) 
				lc(gs12 red gs12 red) lp(dash dash solid solid) lw(med thick med  thick)
				legend(order(- "{bf:NonCompliant}" 4 "R=1" 2 "R=0"  - "{bf:Compliant}" 3 "R=1"  1 "R=0" ) pos(6) c(2) r(3) color(red red black) colfirst ) 
				`yopts' `xopts'
				name(`v', replace) 
			;
		#delimit cr 	
		
		// edit the legend, only need to do once 
		if `i' == 1 {
			gr_edit .legend.plotregion1.label[1].style.editstyle color(red) editcopy
			gr_edit .legend.plotregion1.label[2].style.editstyle color(red) editcopy
			gr_edit .legend.plotregion1.label[3].style.editstyle color(red) editcopy
			gr_edit .legend.plotregion1.label[4].style.editstyle color(gs6) editcopy
			gr_edit .legend.plotregion1.label[5].style.editstyle color(gs6) editcopy
			gr_edit .legend.plotregion1.label[6].style.editstyle color(gs6) editcopy	
		}
		
		local i = `i' + 1 
	}
	
 	local figs SIZE BOARD_IND ISS_INDEP  former REMAIN__   REMAIN_IND REMAIN_NOT NEW__ HIRE_IND HIRE_NOT 
	grc1leg2 `figs', legendfrom(SIZE) noauto  name(all_groups2, replace) ///
		xsize(6) ysize(7) col(3) holes(2 3) pos(12) ring(0) lxo(18) lyo(-5) ///
		legscale(*.4)

	graph export "$output_folder/Fig_1_BoardStats.png" , replace
	graph export "$output_folder/Fig_1_BoardStats.eps" , replace
	
	clear 	
}	

*************************************************************************
* Table 2: firm summary stats
* controls are lagged, as in the main specification
*************************************************************************
{
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	eststo clear	
		
	sort gvkey fyear

	/* REPORT SUM STATS FOR SAMPLE OF MAIN SPECIFICATION */
	
	reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		
	g keep = e(sample) == 1 
	
	// before we drop to that sample, get the t-1 lagged X vars for the table 
	
	foreach v in l_a td_a  capx_a xrd_a SIZE BOARD_IND incoming_frac outgoing_frac {
		bysort gvkey (fyear): g LAG_`v' = L.`v' if _n > 1
	}
	
	keep if keep 
	
	tabstat prof_a prof_sale   t_Comm R ///
		LAG_* ///
		, c(s) s(n mean sd p5 p50 p95) save
	tabstatmat C, nototal
	mat C = C'
	mat li C
	putexcel set "$excel_filename", modify sheet("T2_raw", replace)
	putexcel A1 = matrix(C), names
}	
*************************************************************************
* Table 3: balance
*************************************************************************
{
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear

	/* get obs from main sample */
	
	reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		
	keep if e(sample) == 1 
		
	/* levels, non-compliant vs not */ 
	
	lab var incoming_frac  "IncomingTurn"
	lab var outgoing_frac  "OutgoingTurn"
	
	eststo clear
	local myvarlist prof_a prof_sale l_a td_a  capx_a xrd_a      SIZE BOARD_IND incoming_frac outgoing_frac
	rename option_awards_ option_awards_ // shorten var name 
		
	foreach v in `myvarlist' {
		local lab: var lab `v'
		di "`lab'"					
		eststo, title("2001, t_Comm vs. c, `lab'"): reghdfe `v' t_Comm if p == 0, cluster(gvkey)  absorb(sic3 )	
	}

	/* average percent annual changes (changes from 1999-2002), non-compliant vs not */

	g t_Comm_perc = t_Comm // puts output in diff row (matters for formatting output)
	foreach v in `myvarlist' {
		local lab: var lab `v'
		di "`lab'"					
		cap drop  perc_ch_`v'
		g perc_ch_`v' = (`v' - l.`v') / l.`v'
		winsor perc_ch_`v' 1		
		noi eststo, title("growth, t_Comm vs. c, `lab'"): reghdfe perc_ch_`v' t_Comm_perc if p == 0, cluster(gvkey)  absorb(sic3 )			
	}

	/* 2001 values, reclass vs. not */

	foreach v in `myvarlist' {
		local lab: var lab `v'
		di "`lab'"					
		eststo, title("2001, reclass vs. not, `lab'"): reghdfe `v' R if p == 0, cluster(gvkey)  absorb(sic3 )	
	}
	
	/* average percent annual changes (changes from 1999-2002), treat vs. control */
	
	g R_perc = R // ensures new row for these reg coefs, which we just do to facilitate formatting tricks in excel 
	foreach v in `myvarlist' {
		local lab: var lab `v'
		di "`lab'"			
		eststo, title("growth, reclass vs. not, `lab'"): reghdfe perc_ch_`v' R_perc if p == 0, cluster(gvkey)  absorb(sic3 )			
	}
				
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T3_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			wide drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  ///
			stats(N r2 r2_a meanY firm_FE firm_FE year_FE, fmt(%9.0fc 2 2 2 0 ) labels("Obs" "R2" "Adj.R2" "MeanY" "firmFE" "firmFE" "yearFE") ) ///
			label mtitles ) ///
		notes(`""reghdfe 	Y R (or T_comm) if  close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')
}
*************************************************************************
* Table 4: main DDD
*************************************************************************
{
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	* std the continuous controls 
	local myvarlist l_a td_a  capx_a xrd_a SIZE BOARD_IND incoming_frac outgoing_frac
	center `myvarlist', s inplace
	sum `myvarlist'
	
	eststo clear	
		
	foreach v in prof_a prof_sale  {
		local lab: var lab `v'
		di "`lab'"			
		rename `v' Y
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		 noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
			
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm  $extra_covariates if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		 noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
						
		rename Y `v'
	}	
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T4_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm   ) ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) ///
			label nomtitles mgroups("ROA" "Profit Margin", pattern(1 0 1))  ///
		)  ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm if close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')
}
****************************************************************
* Fig 2: Plot baseline model TE by year
****************************************************************
		
	foreach v in prof_a prof_sale  {
		
		use "$temp/fy_panel", clear 
		merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
		define_DDD_vars -5 5 2003 
		xtset gvkey fyear

		local lab: var lab `v'
		di "`lab'"			
		rename `v' Y
		g L1_l_a = L1.l_a  // can't interact with lag, apparently, so make lags explicit vars 
		g L1_td_a = L1.td_a 
		g L1_xrd_a = L1.xrd_a 
		g L1_capx_a = L1.capx_a 
		g L1_SIZE = L.SIZE 
		g L1_BOARD_IND = L.BOARD_IND 
		g L1_incoming_frac = L.incoming_frac 
		g L1_outgoing_frac = L.outgoing_frac
		
		noi di "OUTSIDE: `v'"
		foreach x in baseline controls {
		noi di "inside: `v'"
					
			if "`x'" == "baseline" local controls  
			if "`x'" == "controls" local controls L1_l_a L1_td_a L1_xrd_a L1_capx_a L1_SIZE L1_BOARD_IND L1_incoming_frac L1_outgoing_frac
			
			local DDDterms Rt_Comm R t_Comm
			
			* create the variables in reg, enforcing omitted year as 2002
			
			cap drop __*
			foreach somevar in `DDDterms' `controls' {
				forval y = 1999/2006 {
					g __`somevar'_`y' = `somevar' * (fyear == `y')
				}	
			}
			drop __*_2002
						
			eststo clear	
			noi eststo: reghdfe 	Y __*  if close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)  	noconst 		

		noi di "preplot: `v'"
						
			preserve
				coefplot
				
				esttab , keep(*Rt*) star(* 0.10 ** 0.05 *** 0.01 ) label
				mat A = r(coefs)
				noi mat li A
				mat coln A = coef t p
				clear
				svmat2 A ,	names(col) rnames(regvar)
				li
				g stderr = coef / t
				replace stderr = 0 if stderr == .
				g	ub = coef + 1.65 * stderr
				g	lb = coef - 1.65 * stderr
				g	zero = 0
				g   year = substr(regvar,-4,4)
				destring year, replace force 
				tsset year
				tsfill 
				
				* these lines add red lines in the pre and post periods				
				sum coef if year < 2002
				g avg_pre = `r(mean)' if year <= 2002
				sum coef if year > 2002
				g avg_post = `r(mean)' if year >= 2002
				li
				
				* other adjustments for the plot 
				foreach somevar in ub lb zero coef {
					replace `somevar' = 0 if year == 2002
				}

				noi li
				twoway  (rarea ub lb year , pstyle(ci)) ///
						(line coef year, lpattern(solid) lwidth(thick) lcolor(black) ) ///
						(line zero year, lwidth(med) lcolor(black)) ///
(line avg_pre avg_post year, lwidth(med med) lp(dash dash) lc(red red)) /// 
						,  legend(off)  title("")   xlabel(1999(1)2006) xtitle("Year")	/// ylabel(-50(25)75, gmax)
						title( "`lab'") graphregion(color(white) lwidth(medium)) ///
						name("DDD_`v'_`x'", replace) 
			restore
		}
	}

	* make a combined graph and fine tune some things 
	graph combine DDD_prof_a_baseline     DDD_prof_a_controls   ///
		          DDD_prof_sale_baseline  DDD_prof_sale_controls
		
	forval i=1/4{
		gr_edit .plotregion1.graph`i'.title.text = {}
		gr_edit .plotregion1.graph`i'.xaxis1.title.text = {}
	}
	
	gr_edit .plotregion1.graph1.title.text.Arrpush "(1) ROA, no controls"
	gr_edit .plotregion1.graph2.title.text.Arrpush "(2) ROA, with controls"
	gr_edit .plotregion1.graph3.title.text.Arrpush "(3) Profit Margin, no controls"
	gr_edit .plotregion1.graph4.title.text.Arrpush "(4) Profit Margin, with controls"
	
	gr_edit .plotregion1.graph1.yaxis1.reset_rule -.02 .08 .02 , tickset(major) ruletype(range) 
	gr_edit .plotregion1.graph2.yaxis1.reset_rule -.02 .08 .02 , tickset(major) ruletype(range) 
	gr_edit .plotregion1.graph3.yaxis1.reset_rule -.05 .15 .05 , tickset(major) ruletype(range) 
	gr_edit .plotregion1.graph4.yaxis1.reset_rule -.05 .15 .05 , tickset(major) ruletype(range) 
		
	graph export "$output_folder/Fig_2_DDD_all.png", replace
	graph export "$output_folder/Fig_2_DDD_all.eps", replace
	
	clear
	
*************************************************************************
* Table 5: DD by subsamples 
*************************************************************************
{
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	* std the continuous controls 
	local myvarlist l_a td_a  capx_a xrd_a SIZE BOARD_IND incoming_frac outgoing_frac
	center `myvarlist', s inplace
	sum `myvarlist'
			
	eststo clear	
	
	/* dd in reclassifier sample */
	
	foreach v in prof_a prof_sale {
		local lab: var lab `v'
		di "`lab'"			
		rename `v' Y
		eststo, title("`lab'"): reghdfe 	Y pt_Comm    $extra_covariates if R == 1 &  close, cluster(gvkey)  absorb(gvkey fyear)	noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst			
		rename Y `v'
	}	
		
	/* dd in non-reclassifier sample */
	
	foreach v in prof_a prof_sale {
		local lab: var lab `v'
		di "`lab'"			
		rename `v' Y
		eststo, title("`lab'"): reghdfe 	Y pt_Comm    $extra_covariates if R == 0 &  close, cluster(gvkey)  absorb(gvkey fyear)	noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst			
		rename Y `v'
	}	
	
	/* dd in treatment/constrained sample */
		
	foreach v in prof_a prof_sale {
		local lab: var lab `v'
		di "`lab'"			
		rename `v' Y				
		eststo, title("`lab'"): reghdfe 	Y Rp    $extra_covariates if  t_Comm == 1 &  close, cluster(gvkey)  absorb(gvkey fyear)	noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst			
		rename Y `v'
	}	
		
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T5_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  ///
		stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") )   order(pt_Comm Rp) ///
			mgroups("R1_subsample" `"R0_subsample"' "NC1_subsample" "R1_subsample" `"R0_subsample"' "NC1_subsample", pattern(1 0 1 0 1 0 1 0 1 0 1 0)) ///
			label mtitles ) ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm  if  (subsample), cluster(gvkey)  absorb(gvkey fyear#sic2)	""')		
}
*************************************************************************
* Table 6: falsification
*************************************************************************
{
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2001 // close = 1 if firm within 5 of cutoff in 2001, p=fyear>=2001
	xtset gvkey fyear
		
	* std the continuous controls 
	local myvarlist l_a td_a  capx_a xrd_a SIZE BOARD_IND incoming_frac outgoing_frac
	center `myvarlist', s inplace
	sum `myvarlist'

	eststo clear	
		
	foreach v in prof_a prof_sale  {
		local lab: var lab `v'
		di "`lab'"			
		rename `v' Y
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm  if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		nocons
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
			
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm  $extra_covariates if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		 nocons
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
						
		rename Y `v'
	}	
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T6_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm   ) ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) ///
			label nomtitles mgroups("ROA" "Profit Margin", pattern(1 0 0 0 1)) )  ///
		notes(`""reghdfe 	Y Rpt_Comm Rt_Comm Rp pt_Comm R t_Comm  if  close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')
	
	clear 
}
	
*************************************************************************
* Table 7: mechanism
*************************************************************************
{
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
		
	reghdfe prof_a Rpt_Comm Rp pt_Comm  if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		
	g base_samp = e(sample)
		
	* std the continuous controls 
	local myvarlist l_a td_a  capx_a xrd_a SIZE BOARD_IND incoming_frac outgoing_frac
	center `myvarlist', s inplace
	sum `myvarlist'
	
	* panel A: profit margin decomp 
		
	g xrd_sale2 = xrd_sale 
	replace xrd_sale2 = 0 if missing(xrd_sale)
		
	sort gvkey fyear
	
	eststo clear
	foreach v in cogs_sale sga_sale  xrd_sale2     {
		local lab: var lab `v'
		di "`lab'"
		if "`lab'" == "" {
			local lab = "`v'"
		}
		rename `v' Y

		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm  $extra_covariates if base_samp &  close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		 noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
			
		rename Y `v'
	}	
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T7a_decomp_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm   ) ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) ///
			label mtitles	  ///
		)  ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm if close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')		
			
	* panel B: employee productivity
	
	eststo clear	
		
	foreach v in prof_emp	sale_emp  	life1	life2	life3	life4  sales_g  {
		local lab: var lab `v'
		di "`lab'"
		if "`lab'" == "" {
			local lab = "`v'"
		}
		rename `v' Y
			
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm  $extra_covariates if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		 noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
						
		rename Y `v'
	}	
	
	esttab, ///
		drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm ) ///
		stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) keep(Rpt_Comm Rp pt_Comm) ///
		label mtitles		


	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T7b_emp_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm   ) ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) ///
			label mtitles	  ///
		)  ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm if close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')		
					
	* panel C: working cap mgmt - turnover
		
	eststo clear	
		
	foreach v in asset_turn fixed_asset_turn ar_turn invt_turn  ap_turn  ccc  {
		local lab: var lab `v'
		di "`lab'"
		if "`lab'" == "" {
			local lab = "`v'"
		}
		rename `v' Y
			
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm $extra_covariates if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		 noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
						
		rename Y `v'
	}	
	
	esttab, ///
		drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm ) ///
		stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) keep(Rpt_Comm Rp pt_Comm) ///
		label mtitles		
		
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T7c_wcap_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm   ) ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) ///
			label mtitles	  ///
		)  ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm if close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')		
}		
*************************************************************************
* Table 8: CG07 style event study - Table IV, Panel C: FF4 alphas 
*************************************************************************
if $using_fake_data == 0 {
	
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	eststo clear	
	
	* reduce to main sample
	
	reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		
	keep if e(sample)
	
	keep lpermno R t_Comm
	mdesc
	duplicates drop lpermno, force
	distinct *, j
	
	rename lpermno permno 
	tempfile firm_classif
	save `firm_classif'
	
	* permnos for CRSP!
	* to download returns from crsp, need a list of permnos
		
	qui flevelsof permno, local(myvars) // faster than levelsof
	local myvars : subinstr local myvars " " ",", all 
	di length("`myvars'")
	
	* obdc crsp for these permnos 
	
		distinct permno 
		local n_firms = `r(ndistinct)' // for post-check

		* download daily returns + this also grabs delisting codes (irrelevant here due to sample construction)

		clear
				
		odbc load, exec("SELECT a.permno, a.date, a.ret, a.prc, a.shrout, c.dlret, c.dlstcd FROM crsp.dsf a LEFT JOIN crsp.dse c ON a.permno = c.permno AND a.date = c.date where a.permno IN ( `myvars' ) AND a.date BETWEEN '2001-11-01' AND '2002-10-31'")  dsn("wrds-pgdata-64")
		
		sort permno date

		distinct permno
		di "We have return data on " `n_firms' "/" `r(ndistinct)' " firms"
		
		replace ret = ret+dlret if !missing(dlret)
		replace ret = ret - .3 if missing(dlret) & !missing(dlstcd)	 // Shumway (1997)
		drop if missing(ret) 
				
	* merge back in firm groupings
	
	merge m:1 permno using `firm_classif'
	
	* compute mktcap (for value weighting)
	
	g mktcap = abs(prc)*shrout*1000
	sum mktcap prc shrout
	
	* collapse to daily portfolios
	* e.g.: 
	* ret_EW_r0                   equal weighted (EW) return for non-reclass (r=0)
	* ret_EW_r1                   EW return for reclass (r=1) 
	* ret_EW_t0                   EW return for compliant (t=0)
	* ret_EW_t1                   EW return for non-compliant (t=1)
	* ret_EW_r0_t0                EW rets for compliant (t=0) and non-reclass (r=0)
	* ret_EW_r0_t1                EW rets for non-compliant (t=1) and non-reclass (r=0)
	* ret_EW_r1_t0                and so on...
	* ret_EW_r1_t1                and so on...
	* and the same for VW...
	
	* for each needed var, create a ret variable with firm-day returns for applicable obs, missing else
	foreach r in 0 1 {
		foreach t in 0 1 {
			foreach w in E V {
				 cap g ret_`w'W_r`r'_t`t' = ret if R == `r' & t_Comm == `t'
				 cap g ret_`w'W_r`r'      = ret if R == `r'
				 cap g ret_`w'W_t`t'      = ret if            t_Comm == `t'
			}
		}
	}
	
	* which we collapse to daily portfolio returns (here the VWs)
	preserve 
		collapse ret_VW_* [fw=mktcap], by(date)
		tempfile VWs 
		save `VWs'
		sum
	restore
	
	* collapse to daily portfolio returns for the EW
	collapse ret_EW_* , by(date)
	
	merge 1:1 date using `VWs' // merge in the VWs 
	drop _m 
	
	* merge in FF4 factors 
	
	preserve
		clear
		odbc load, exec("SELECT * FROM ff.factors_daily")  dsn("wrds-pgdata-64")
		tempfile FF
		save `FF'
	restore
		
	merge 1:1 date using `FF', keep(1 3)
	drop _m 
	
	* in regs, LHS is ret - rf
	
	foreach v of varlist ret_?W* {
		replace `v' = `v' - rf
	}
	sum ret_?W* rf

	* do some tidying 
	
	g mofd = mofd(date)
	format mofd %tm 
	drop if missing(date)

	* create the long-short ports, and the double sort port

	foreach W in E V {

		// these are the r1-r0 portfolios (within each t subset)
		foreach t in _t1 _t0 {
		g ret_`W'W_r1mr0`t' = ret_`W'W_r1`t' - ret_`W'W_r0`t'
		}

		// the t1-t0 portfolios 
		foreach r in "" _r1 _r0 _r1mr0 {
		g ret_`W'W`r'_t1mt0 = ret_`W'W`r'_t1 - ret_`W'W`r'_t0
		}
	}

	* get ready for portfolio tests 
	
	g t = _n // tsset without gaps, for newey
	tsset t
	local nw_lags 4
	
	* run the tests and save the alphas (formatting the table nicely happens in excel)
	
	eststo clear
	foreach W in E V {
	foreach t in _t1 _t0 _t1mt0 {
	foreach r in "" _r1 _r0 _r1mr0 {
		di "ret_`W'W`r'_t1mt0"
		eststo: newey ret_`W'W`r'`t' mktrf smb hml umd, lag(`nw_lags')
		estadd local W "`W'"
		estadd local t "`t'"
		estadd local r "`r'"
	}	
	}
	}

	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T8_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(4) se(4) compress replace nogaps  order(_cons   ) ///
			stats(N r2 W t r , fmt(0 %9.3f ) labels( "Observations" "R2" "returnWtype" "t_group" "r_group" ) ) ///
			label  ///
		)  ///
		notes(`""FF4, newey lags of `nw_lags'""')
}	
****************************************************************
* Table 9: R/NR director comparisons (Dir_traits_raw)
* Table 10: in/out director comparisons (Dir_turnover_tComm_raw)
****************************************************************	

if $using_fake_data == 0 {
	
*** get common shares as of 10k to fill in missing votecref (ISS stopped collecting after 2003)

	use gvkey lpermno datadate cusip fyear csho if fyear > 1993 using "$ccm_raw", clear
	destring gvkey, replace
	duplicates drop gvkey fyear datadate, force // if the fye date is the same, doesn't matter which we keep
	keep gvkey fyear csho
	distinct g f, j	
	tempfile csho
	save `csho', replace
	
*** get list of sample firms - we just want turnover info at these firm, along with R and t_Comm 

	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
		
	reghdfe 	prof_a Rpt_Comm Rp pt_Comm  if  close, cluster(gvkey)  absorb(gvkey fyear)				
	keep if e(sample) 
	
	collapse (max) R t_Comm (firstnm) cusip6 , by(gvkey)	
	tempfile sample_firm_R_t
	save `sample_firm_R_t'	
	
*** pre-compute some vars of interest at dir_firm_id / fyear level

	use "$dir_lvl_out", clear	
	collapse (min) dirsince, by(dir_id)
	rename dirsince dir_firstYearOnABoard 
	tempfile dir_firstYearOnABoard
	save `dir_firstYearOnABoard'
	
*** load director file, tag directors as incoming and/or outgoing
	
	* to update cusip6 to match CCM'R 6 digit cusip (so we can merge in R and t_Comm)
	import delim using "$iss_ccm_cusip", clear varn(1)	
	rename cusip6_irrc cusip6 
	tempfile ccm_cusip6_lookup
	save `ccm_cusip6_lookup', replace
	
	* load director level, update cusip6 to match CCM'R 6 digit cusip 
	use "$dir_lvl_out", clear
	merge m:1 cusip6 using `ccm_cusip6_lookup'
	replace cusip6 = cusip6_ccm if _m == 3 & !missing(cusip6_ccm)
	drop cusip6_ccm _m 
	
	* add R, t, dir_firstYearOnABoard
	merge m:1 cusip6 using `sample_firm_R_t', keep(3) nogen	
	merge m:1 dir_id using `dir_firstYearOnABoard', keep(1 3) nogen
	
	g YrBoardExperience = fyear - dir_firstYearOnABoard
	
	lab var YrBoardExperience "Director experience"

	* tag incoming/outgoing
	
	egen firm_first_year = min(fyear), by(gvkey)
	egen firm_last_year = max(fyear), by(gvkey)
	
	drop dir_firm_id // some missing vals, reconstitute it
	egen dir_firm_id = group(dir_id cusip6)
	
	bysort dir_firm_id (fyear): g incoming = _n == 1  				& fyear > firm_first_year
	bysort dir_firm_id (fyear): g outgoing = _n == _N 				& fyear < firm_last_year    // CANT BE OUTGOING IN 2006 - no 2007 data 
	bysort dir_firm_id (fyear): g remaining = _n > 1 & _n < _N
			
	drop if fyear < 1999
		
	* define dir_share_pct 
	
	g dir_share_pct = 100*num_of_shares/votecref
	
	merge m:1 gvkey fyear using `csho', keep(1 3) nogen keepusing(csho)	
	pwcorr votecref csho // very similar 
	sum votecref csho if !missing(votecref) // need to scale the same
	replace csho = csho * 1000000
	sum votecref csho if !missing(votecref) 
	
	replace dir_share_pct = 100*num_of_shares/csho if missing(votecref)
	sum dir_share_pct, d
	replace dir_share_pct = . if dir_share_pct > `r(p99)'

	* define other table variables 
	
	g tenure_length = fyear - dirsince		
	
	lab var dir_share_pct                "Fraction of firm shares held"
	lab var outside_public_boards        "Num. of outside board positions"
	lab var attend_less75_pct            "Low attendance"
	lab var audit_membership             "Audit committee"
	lab var comp_membership              "Compensation committee"
	lab var gov_comm_mem                 "Governance committee"
	lab var nom_membership               "Nominating committee"
	lab var indep_dirman                 "Independent"
	lab var tenure_length                "Directorship tenure"
				
	g neg_t_Comm = -t_Comm // just flips the difference on the last test 
	g neg_R = -R           // just flips the difference on the last test 

	g any_committee = audit_membership + comp_membership + gov_comm_mem + nom_membership
	replace any_committee = (any_committee>0) if !missing(any_committee)
	lab var any_committee                "On any of A/C/G/N committees"
		
	// make table
	// could be done also with 	"balancetable" command
		
	local tabvars indep_dirman dir_share_pct former_employee_yn any_committee audit_membership  comp_membership gov_comm_mem nom_membership tenure_length outside_public_boards attend_less75_pct  
	
	local subsample t_Comm == 1 & inrange(fyear, 2003,2006)
	
	eststo clear
	foreach condition in incoming outgoing {	
		estpost ttest `tabvars' if (`condition') & (`subsample'), by(neg_R) unequal
		eststo `condition'
	}		
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T10_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			cells("mu_1(label(R=1) fmt(2)) mu_2(label(R=0) fmt(2)) b(label(Diff) star)") star(* 0.1 ** 0.05 *** 0.01)  mgroups("Incoming" "Outgoing" "Remaining", pattern(1 1 1))	nonumber stats(off)	title("Director Characteristics at Non-Compliant Firms") note("Means and difference of means reported for each subsample of directors, unequal var t-test")	label) ///
		notes(`""Director turnover, non-compliance includes committee shortfalls""')			

	*** tag reclass dir across all of their obs 

	g temp = switchto_Indman if inrange(fyear,2002,2006)
	egen is_reclassifying_dir = max(temp)	, by(gvkey dir_id) // only =1 in the year of reclass for reclass in 2002-2006
	replace is_r = 0 if missing(is_re) 
	drop temp
	
	*** compare directors in the year they reclass to 
		
	g neg_is_reclassifying_dir = - is_reclassifying_dir // just flips the difference on the last test 
	g neg_switchto_Indman = -switchto_Indman
	
	local tabvars indep_dirman dir_share_pct former_employee_yn any_committee audit_membership  comp_membership gov_comm_mem nom_membership tenure_length outside_public_boards attend_less75_pct  
	local subsample inrange(fyear,1999,2006)
	eststo clear
	tabstat  `tabvars'      if (`subsample') , by(switchto_Indman) c(s) s(n mean)
	// compare reclass vs never-reclass
	estpost ttest `tabvars' if (`subsample') & ((switchto_Indman==1) | (!is_reclassifying_dir)),                     by(neg_switchto_Indman) unequal
	eststo 
	// compare reclass vs "never-reclass non-indep"
	estpost ttest `tabvars' if (`subsample') & ((switchto_Indman==1) | (!is_reclassifying_dir & indep_dirman == 0)), by(neg_switchto_Indman) unequal
	eststo 
	// compare reclass vs "never-reclass indep"
	estpost ttest `tabvars' if (`subsample') & ((switchto_Indman==1) | (!is_reclassifying_dir & indep_dirman == 1)), by(neg_switchto_Indman) unequal
	eststo 
	esttab, cells("mu_1(label(R=1) fmt(2)) mu_2(label(R=0) fmt(2)) b(label(Diff) star)") star(* 0.1 ** 0.05 *** 0.01)  mgroups("RvsAll" "RvsNotIndep" "RvsIndep", pattern(1 1 1))	nonumber stats(off)	title("Director Characteristics at Non-Compliant Firms") note("Means and difference of means reported for each subsample of directors, unequal var t-test")	label
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T9_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			cells(`"mu_1(label("rdt=1") fmt(2)) mu_2(label("rdtAlwaysZero") fmt(2)) b(label(Diff) star)"') star(* 0.1 ** 0.05 *** 0.01)  nonumber stats(off)	title("Director characteristics by director reclassification status") note("Means and difference of means reported for each subsample of directors, unequal var t-test")	label) ///
		notes(`""Sample: Dir obs""')				
	
*** What fraction of of Reclassifying NC firms have a reclass director on a key 
*** committee post mandate? (60/96 = 63%)
	
	* is this a reclassifying director?
	
	cap drop temp 
	g temp =switchto_Indman if inrange(fyear,2002,2006)	
	egen R_director = max(temp)	, by(cusip6 dir_id)
	bysort cusip6 dir_id  (fyear): g has_switched = sum(temp) // equals 1
	replace has_switched = (has_switched>0) if !missing(has_switched) // a few directors have stint as non-indep due to consulting, etc
				
	* how many R & NC firms do we have?
	distinct cusip if 	fyear > 2002 ///
						& R == 1  & t_Comm == 1 
	local n_RNC `r(ndistinct)'
	
	* how many of those have a director on a committee who has reclassed?
	distinct cusip if 	fyear > 2002 ///
						& R == 1  & t_Comm == 1 ///
						& (audit_membership == 1 |  comp_membership == 1 |  gov_comm_mem == 1 |  nom_membership == 1 ) ///
						& has_switched == 1
	local n_RNC_with_Rdir_on_comm `r(ndistinct)'			
	
	di `n_RNC_with_Rdir_on_comm'/`n_RNC' // 62.5%
}	

****************************************************************
* Table 11: Board stats at non-compliant firms
****************************************************************

if $using_fake_data == 0 {
		
	* get board level tenure and former employment 
	
	use "$dir_lvl_out", clear
	g tenure_length = fyear - dirsince		
	collapse tenure_length (sum) former_employee_yn, by(cusip6 fyear)
	
	tempfile boardlvl_tenure_employment
	save `boardlvl_tenure_employment', replace
	
	* now load our firm year panel
	
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	merge 1:1 cusip6 fyear using `boardlvl_tenure_employment', keep(1 3) nogen
	
	* keep all obs of the firms in our main test 
	
	sort gvkey fyear	
	reghdfe 	prof_a Rpt_Comm Rp pt_Comm  if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		
	g sample = e(sample)	
	egen ever_in_samp = max(sample), by(gvkey)	
	keep if ever_in_samp
	
	* ad hoc fix needed only for this table 

	replace NONRETURN_NOT = 1 if fyear == 2001 & gvkey == 63422
		
	* set up and tabulate 	
		
	g new_board_members = HIRE_IND + HIRE_NOT 
	g departed_board    = NONRETURN_IND + NONRETURN_NOT

	keep if fyear >= 1999 & fyear <= 2006
	
	separate new_board_members, by(fyear) g(New_)
	separate new_board_members, by(p) g(New_)
	separate departed_board,    by(fyear) g(gone_)
	separate departed_board,    by(p) g(gone_)
	separate tenure_length, by(fyear) g(TENURE_)
	separate tenure_length, by(p) g(TENURE_)
	separate former_employee_yn, by(fyear) g(EMPLOY_)
	separate former_employee_yn, by(p) g(EMPLOY_)
	
	unab tabvars : New_* gone_* TENURE_* EMPLOY_*
	summarize `tabvars' if R == 0 // does it work, yes
	
	cap g negR = -R // (just to make difference value be Reclass-NR)

	* output for tables
	
	eststo clear 	
	
	foreach v in New_ gone_ TENURE_ EMPLOY_ {
	eststo ,    title("R"): 		 estpost summarize `v'* if R == 1 & t_Comm == 1
	eststo ,    title("NR"): 		 estpost summarize `v'* if R == 0 & t_Comm == 1
	eststo  ,   title("diff"):   	 estpost ttest     `v'* if          t_Comm == 1, by(negR) unequal	
		
	}
		
	* a gnarly rename trick for estout
	local rename = ""
	foreach v in New_ gone_ TENURE_ EMPLOY_ {
	ds `v'*
	di "`r(varlist)'"
	forval i = 1/10 {
		local newname = 1998+`i'
		if `newname' > 2006 {
			local int = `newname' - 2007 
			local newname = "post`int'"
		}
		else {
			local newname = "year`newname'"
		}
		local rename = "`rename' "+word("`r(varlist)'",`i') + " `newname'"
	}
	}
	di "`rename'"
		
	*t(pattern(0 0 1 0 0 1 0 0 1) par fmt(2))
	noi	esttab *, cells("mean(pattern(1 1 0 1 1 0 1 1 0 1 1 0) fmt(2))  b(star pattern(0 0 1 0 0 1 0 0 1 0 0 1) fmt(2) label(diff))") ///
		label	 mtitles  mgroups("new" "gone" "tenure" "formeremp", pattern(1 0 0 1 0 0 1 0 0 1)) ///
		star(* 0.10 ** 0.05 *** 0.01 ) rename(`rename')
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T11_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( cells("mean(pattern(1 1 0 1 1 0 1 1 0 1 1 0) fmt(2))  b(star pattern(0 0 1 0 0 1 0 0 1 0 0 1) fmt(2) label(diff))") ///
		label	 mtitles  mgroups("new" "gone" "tenure" "formeremp", pattern(1 0 0 1 0 0 1 0 0 1)) ///
		star(* 0.10 ** 0.05 *** 0.01 ) rename(`rename') ) ///
		notes(`""Boards of non-compliant firms, non-compliance includes committee shortfalls""')		
	
	* get post minus pre for each column
	
	g post = p // having a p and a post allows us to put post coefs in the same row as the interaction term in the three models below
	eststo clear
	foreach v in new_board_members departed_board tenure_length former_employee_yn {
		eststo: reg `v' post 			if R == 1 & t_Comm == 1
		eststo: reg `v' post 			if R == 0 & t_Comm == 1
		eststo: reg `v' c.p##c.R   if  t_Comm == 1		
	}
	
	* notice the "rename()" arg below
	esttab,  order(post   ) rename(c.p#c.R post) keep(post) star(* 0.10 ** 0.05 *** 0.01 ) b(2) compress 
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T11_diffrow_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options(order(post   ) rename(c.p#c.R post) keep(post) star(* 0.10 ** 0.05 *** 0.01 ) b(2) compress  ) ///
		notes(`""Boards of non-compliant firms, non-compliance includes committee shortfalls""')		
}	
	
*************************************************************************
* Table 12: CEO pay (continuation of code, not isolated block)
*************************************************************************
{	
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	* std the continuous controls 
	local myvarlist l_a td_a  capx_a xrd_a SIZE BOARD_IND incoming_frac outgoing_frac
	center `myvarlist', s inplace
	sum `myvarlist'
		
	* Adjust labels for table - the title will clarify they are log(1+Y)
	lab var tdc1 	 				"TDC1"
	lab var tdc2					"TDC2"
	lab var total_curr 				"Salary+Bonus"
	lab var option_awards_blk_value	"OptionsGranted"
	
	eststo clear	
	
	 foreach v in  tdc1 tdc2 total_curr { // 
		local lab: var lab `v'
		di "`v'"		
		cap drop Y
		g Y = log(1+`v')  //  Y vars are log(1+y)
		sum Y
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm   if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)	nocons	
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst			
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm  $extra_covariates if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)	nocons	
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst			
		drop Y 
	}		
			
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("T12_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) order(Rpt_Comm) ///
			label mtitles title("Y vars are log(1+y)") ) ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm  if  close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')
}		


****************************************************************
* Fig 3: leave-some-out analysis 
****************************************************************
/*

This redoes the main results N times.
Each time, it drops 1% of firms from the sample. 
A seed is set before the first run. 
Results are saved and the distribution of beta_DDD and t_DDDD are saved. 

Leave 1 percent out -> L1perc_data.txt is the output

*/
{
	
	if $using_fake_data == 0 {
		local N 10000 // number of trials to run
	}
	else {
		local N 100 // just enough to plot 
	}

	*ssc install schemepack, replace (to get white_tableau)
	set scheme white_tableau 

	local DDDterms Rpt_Comm Rp pt_Comm
	local tvar t_Comm
		
	cap file close L20_data
	file open  L20_data using "$output_folder/L1percO_data.txt", write text replace
	file write L20_data "iteration,beta,tstat" _n
	
	*** prep 

	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	cap g sic2 = floor(sic3/10)
		
	eststo clear	
	
	* get list of gvkeys in main test
	
	preserve
		reghdfe 	prof_a `DDDterms'  if close  & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey fyear)	noconst
		keep if e(sample) 
		keep gvkey
		duplicates drop *, force
		sort gvkey
		tempfile gvkeys
		save `gvkeys'
		count
	restore		
	
	* decide which firms to drop in each iteration of the test
	
	preserve 
		clear
		tempfile dropgvkeys
		save `dropgvkeys', replace emptyok
	restore 
	
	set seed 42 
	preserve 
		qui forval i = 1/`N' {
			use `gvkeys', clear
			sample 5, count
			g iter = `i'
			append using `dropgvkeys', force
			save `dropgvkeys', replace  // obs: iter-gvkey. For each iter, 1% of gvkeys are listed. 
		}
	restore 	
	
	* loop over iterations of dropped gvkeys, within each, run models and save outputs  
		
	set seed 42
	
	local i = 1 // overall iteration counter (across Y and controls)
	qui foreach v in prof_a prof_sale {	
		foreach controls in 0 1 {
						
			forval iter = 1/`N' {
									
						noi _dots `i' 0
						
						// select a random subset of firms to exclude from estimation
						preserve
							use if iter == `iter' using `dropgvkeys', clear
							tempfile drop_these
							save `drop_these'
						restore
						
						cap drop _m
						cap drop dropme
						merge m:1 gvkey using `drop_these', keep(1 3)
						g dropme = _m == 3 
						xtset gvkey fyear
						
						// Run the regression without the current id
						local lab: var lab `v'
						di "`lab'"			
						rename `v' Y
						
						if `controls' == 1 {
							reghdfe 	Y `DDDterms' $extra_covariates if  close & inrange(fyear, 1999,2006) & !dropme , cluster(gvkey i.fyear)  absorb(gvkey i.fyear#i.sic2)	
						}
						else {
							reghdfe 	Y `DDDterms'  if  close & inrange(fyear, 1999,2006) &  !dropme, cluster(gvkey i.fyear)  absorb(gvkey i.fyear#i.sic2)					
						}
										
						// Get the coefficient and t-stat
						matrix b = e(b)
						matrix V = e(V)
						local beta = b[1,1]
						local stderr = sqrt(V[1,1])
						local tstat = `beta' / `stderr'
						
						// Save the results
						file write L20_data "`iter',`beta',`tstat'" _n
			
						// (I used to store extra info in the "blah" column, don't now. Keeping around for back compatibility with code below.)
						
						// rename Y 
						rename Y `v'	
			
			}
		}
	}
	
	file close L20_data
	
	// Convert matrix to dataset
	import delim using "$output_folder/L1percO_data.txt", clear
	
	count 
	g yvar       = "(1) ROA, no controls"    
	replace yvar = "(2) ROA, with controls"             if _n > 1*_N/4
	replace yvar = "(3) Profit Margin, no controls"     if _n > 2*_N/4
	replace yvar = "(4) Profit Margin, with controls"   if _n > 3*_N/4
		
	// this is just to pick the order of columns not by alphabetical (HT nick cox DA LEGEND)
	
	g yvar_order       = 1    
	replace yvar_order = 2 if _n > 1*_N/4
	replace yvar_order = 3 if _n > 2*_N/4
	replace yvar_order = 4 if _n > 3*_N/4
		
	egen yvar2 = group(yvar_order yvar)
	labmask yvar2, values(yvar)			
		
	// graph t-stats 
			   
	if $using_fake_data == 0 {
		local start_tstat 1
		local start_beta  0.01
	}
	else {
		sum tstat 
		local start_tstat = `r(min)'-.5
		sum beta 
		local start_beta  = `r(min)'-.5
	}
	
	twoway (histogram  tstat  ,  percent  start(`start_tstat') bins(30) pstyle( p1bar)  lw(none)  ) ///
		   , ///
		   xline(1.96, lc(red) lp(dash) lstyle(foreground))  /// 
		   xline(1.65, lc(red) lp(dash) lstyle(foreground)) ///
		   by(yvar2, rows(4) title("t({&beta}{subscript:1})", size(large)) note("") subtitle(" ", size(medlarge) margin(l+0 r+0 b-1 t-1)) leg(off) ) ///
		   ytitle("") xtitle("") /// subtitle("")   ///
		   ysize(6) xsize(3)  xlabel(,labsize(medlarge)) ylabel(,labsize(medlarge)) ///
		   name(tstats, replace)
	
	twoway (histogram  beta  ,  percent start(`start_beta') bins(30) pstyle( p1bar)  lw(none) ) ///
		   , ///
		   by(yvar2, rows(4) title("{&beta}{subscript:1}", size(large)) note("") subtitle(" ", size(medlarge)  margin(l+0 r+0 b-1 t-1)) ) ///
		   ytitle("") xtitle("") /// subtitle("")   ///
		   ysize(6) xsize(3)  xlabel(0.01(0.01)0.05,labsize(medlarge)) ylabel(,labsize(medlarge)) ///
		   legend(off label(1 "Non-Reclassifiers") label(2 "Reclassifiers") rows(1)) ///
		   name(betas, replace)
	
	graph combine tstats betas, xsize(6) ysize(6.5) plotregion(margin(0 0 -5 -5)) 
	
	*grc1leg2 tstats betas, xsize(6) ysize(6.5) plotregion(margin(0 0 -5 -5)) hidelegendfrom   noauto

	graph export "$output_folder/Fig_3_L1percO.png" , replace
	graph export "$output_folder/Fig_3_L1percO.eps" , replace
	
	g pabove5  = tstat < 1.96
	g pabove10 = tstat < 1.65
	tabstat pabove* , by(yvar) s(mean count sum)
	
}	

********************************************************************************
* Fig IA1: director propensity to be on committee in event time around reclassification
********************************************************************************

if $using_fake_data == 0 {
	
*** get main sample firms 

	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
		
	reghdfe prof_a Rpt_Comm Rp pt_Comm  if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		
	keep if e(sample) 
	
	collapse (max) R t_Comm (firstnm) cusip6 , by(gvkey)	
	tempfile sample_firm_R_t
	save `sample_firm_R_t'	
	
*** load director file
	
	* we need this to update cusip6 to match CCM'R 6 digit cusip (so we can merge in sample restrict)
	import delim using "$iss_ccm_cusip", clear varn(1)	
	rename cusip6_irrc cusip6 
	tempfile ccm_cusip6_lookup
	save `ccm_cusip6_lookup', replace
	
	* load director level, update cusip6 to match CCM'R 6 digit cusip 
	use "$dir_lvl_out", clear
	merge m:1 cusip6 using `ccm_cusip6_lookup'
	replace cusip6 = cusip6_ccm if _m == 3 & !missing(cusip6_ccm)
	drop cusip6_ccm _m 
	
	* reduce to sample firms
	merge m:1 cusip6 using `sample_firm_R_t', keep(3) nogen	
		
	* reduce to reclassifying directors
	g temp =switchto_Indman if inrange(fyear,2002,2006)	
	egen R_director = max(temp)	, by(cusip6 dir_id)
	keep if R_director

	* get event time indicator around the reclass
	// we will xtset on dir-firm-year. we need a joint dir-firm var, but the existing one is not fully done 
	
	drop dir_firm_id // not fully avail, 
	egen dir_firm_id = group(cusip6 dir_id)
	xtset dir_firm_id fyear
	
	g        reclass_etime = -3 if F3.switchto_Indman == 1
	replace  reclass_etime = -2 if F2.switchto_Indman == 1
	replace  reclass_etime = -1 if F1.switchto_Indman == 1
	replace  reclass_etime = 0  if    switchto_Indman == 1
	replace  reclass_etime = 1  if L1.switchto_Indman == 1
	replace  reclass_etime = 2  if L2.switchto_Indman == 1
	replace  reclass_etime = 3  if L3.switchto_Indman == 1
	
	g dir_on_comm = (audit_membership == 1 |  comp_membership == 1 |  gov_comm_mem == 1 |  nom_membership == 1 )
	
	* get director propensity to be on committee in event time around reclassification

	collapse dir_on_comm audit_membership comp_membership gov_comm_mem nom_membership, by(reclass_etime)
	
	foreach v in dir_on_comm audit_membership comp_membership gov_comm_mem nom_membership {
		replace `v' = `v'*100 // express as %
	}
		 	
	line dir_on_comm audit_membership comp_membership gov_comm_mem nom_membership reclass_etime, ///
		xtitle("Years Relative to Reclassification") ytitle("% of Reclassified Directors") ///
		xlabel(-3(1)3) ///
		text(45.5 3 "Any", place(e)) ///
		text(27 3 "Nominating", place(e) color(red)) ///
		text(25 3 "Audit", place(e)) ///
		text(22.5 3 "Governance", place(e) color(red)) ///
		text(20.7 3 "Compensation", place(e)) ///
		xscale(range(-3 5)) ///
		yscale(range(0(10)50)) ///
		lp(solid dash solid dash solid) ///
		lw(thick med) ///
		lc(black black black red red) ///
    graphregion(margin(small)) ///
    ysize(6) xsize(7) ///
		legend(off)	
	
	graph export "$output_folder/Fig_IA1_reclass_on_committee.png", replace
	graph export "$output_folder/Fig_IA1_reclass_on_committee.eps", replace
}	
			
	
****************************************************************
* Table IA1: Count of NC and R, by independence definition 
****************************************************************

if $using_fake_data == 0 {
	
* load director level data with all variations of definition of Independent
	
	use  "$dir_lvl_out", clear

* get to firm level dataset with R and t_Comm, as in main code
	
	* these definitions:
	*0   --> IRRC raw dir classif
	*1   --> CG2009
	*2   --> Guthruie et al 2012 
	*man --> Our manual examination and supplementation of the Guthruie et al 2012 classif procedure 
	
	foreach def in 0 1 2 man {  
		* to get R and board level t
		g R_`def'            =  SWITCHTO_IND`def'        if fyear >= 2002 & fyear <= 2006	// sum this over years to get reclassifier
		g DIST_2001_`def'    =  NOT_INDEP`def' - INDEP`def'   if fyear == 2001
		g t_`def'            =  INDEP`def' < NOT_INDEP`def'   if fyear == 2001
		
		* to get committee compliance
		g nom_indep_`def'   = indep_dir`def' if nom_membership == 1    & fyear == 2001
		g comp_indep_`def'  = indep_dir`def' if comp_membership == 1   & fyear == 2001
		g audit_indep_`def' = indep_dir`def' if audit_membership == 1  & fyear == 2001
		g gov_indep_`def'   = indep_dir`def' if gov_comm_mem == 1      & fyear == 2001
	}

	* collapse to board level
	collapse (mean) R_0 - gov_indep_man, by(cusip6)

	* R = 1 sum(SWITCHTO ) > 0
	foreach def in 0 1 2 man {
		replace R_`def'  = R_`def'  > 0 if !missing(R_`def' )
	}

	* non-compliance aka treatment ("t") (NC in paper) = 1 if 
	foreach def in 0 1 2  man {
		replace t_`def'  = t_`def' == 1 | nom_indep_`def' < 1 | comp_indep_`def' < 1  | audit_indep_`def' < 1  | gov_indep_`def' < 1 if !missing(t_`def')
	}

	foreach def in 0 1 2  man {
		replace t_`def'  = t_`def' == 1 | nom_indep_`def' < 1 | comp_indep_`def' < 1  | audit_indep_`def' < 1  | gov_indep_`def' < 1 if !missing(t_`def')
	}
	
* find the subset of this that is in the main test

	keep cusip6 R_* t_*
	rename *man *3

	* merge in cusip6_ccm and then on cusip6_ccm fyear of main test
	
	preserve
		import delim using "$iss_ccm_cusip", clear varn(1)	
		keep cusip*
		tempfile ccm_cusip6_lookup
		save `ccm_cusip6_lookup', replace

		* get sample of main test
		
		use "$temp/fy_panel", clear 
		merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
		define_DDD_vars -5 5 2003 
		xtset gvkey fyear						
		reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)	nocons
		keep if e(sample)
		keep cusip6 
		duplicates drop *, force
		count
		rename cusip6 cusip6_ccm
		merge 1:1 cusip6_ccm using `ccm_cusip6_lookup', keep(1 3)
		replace cusip6_ccm = cusip6_irrc if !missing(cusip6_irrc)
		keep cusip6_ccm
		rename cusip6 cusip6
		tempfile cusip6_in_main
		save `cusip6_in_main', replace 
		
	restore 

	merge 1:1 cusip6 using `cusip6_in_main'
	
	g R_4 = R_3 if _merge == 3
	g t_4 = t_3 if _merge == 3
	drop _m
	
	* construct the table 
	
	reshape long R_ t_, i(cusip6) j(def)

	tostring def, replace
	replace def = "manual" if def == "3"
	replace def = "in main test" if def == "4"

	keep if !missing(R_) & !missing(t_)    /* IMPORTANT: SAMPLE RESTRICTION ON THIS TABLE */
	g N_ = 1
	g Rt = R*t
		
	eststo clear
	estpost tabstat N_ R_ t_ Rt if !missing(R_) & !missing(t_), s(sum) by(def) listwise nototal
	esttab, cells("N_(fmt(%9.0fc)) R_ t_ Rt") 
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("IA1_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options(cells("N_(fmt(%9.0fc)) R_ t_ Rt") )
}		

********************************************************************************	
* Table IA2: board summary stats
*************************************************************************
{
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	eststo clear	
	
	* reduce to main sample
	
	reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		
	keep if e(sample)
	
	tabstat gvkey , by(fyear) s(count) save // row 1
	tabstatmat A, nototal
	mat A = A'
				
	tabstat SIZE INDEP NOT_INDEP                /// board comp (panel A)
			HIRE_IND SWITCHTO_IND NONRETURN_NOT /// moves towards indep (panel B)
			HIRE_NOT SWITCHTO_NOT NONRETURN_IND /// moves away from indep (panel C)
			, by(fyear) s(mean) save 
	tabstatmat B, nototal
	mat B = B'
	
	// panel D: formulaic, in excel, based on averages (not director level...)
	
	// panel E
	g	temp1		= SWITCHTO_IND > 0 if SWITCHTO_IND != . 
	g	temp2		= SWITCHTO_NOT > 0 if SWITCHTO_NOT != .
	g	temp3		= temp1 == 1 | temp2 == 1 if SWITCHTO_IND != .
	
	tabstat temp*, s(mean) by(fyear) save
	tabstatmat C, nototal
	mat C = C'

	putexcel set "$excel_filename", modify  sheet("IA2_raw", replace)	
	putexcel A1 = matrix(A), names
	putexcel A5 = matrix(B), names
	putexcel A17 = matrix(C), names
	putexcel save
}	

********************************************************************************	
* Table IA3: merger activity test - propensity, CARs, goodwill impairment 
* involves one-time manual intermediate step to go to event-study-wrds
********************************************************************************

if $using_fake_data == 0 {
	
* get CAR -2,2 for mergers in our sample
* get event study datasets - with ARs, etc. 
* put outputs in merger_CARs folder (key file: merger_CARs/event-level_CAPM.dta)

	use "$completed_mergers" , clear
	destring gvkey, replace
	
	keep if inlist(form,"Acq. Maj. Int.","Acq. of Assets","Merger")
	keep if transvalue > 5 // 5 mil
	
	keep dealnum ayear gvkey adate transvalue 
	rename ayear fyear
	count
	merge m:1 gvkey fyear using "$temp/fy_panel", keep(3) keepusing(lpermno) nogen // reduce to our sample, get permno
	tostring adate, g(date_text2) format(%tdCYND) force
	
	preserve 
	keep  lpermno date_text2
	order lpermno date_text2
	cap export delim using "inputs/permno_dates_for_eventus.txt", novar delim(" ")
	restore 
	
	/*
	Source: https://wrds-www.wharton.upenn.edu/pages/get-data/event-study-wrds/us-daily-event-study-Upload-your-own-events/

	Upload: merger_CARs/permno_dates_for_eventus.txt

	Select all variables. 
	
	Pick a model type and save 2 output files:
		(1) event-eventtime   - contains ARs in eventime for each firm. denoted <event-eventtime-level>
		(2) event level       - contains CARs for each event, denoted <event-level>
	
	Save each file with scheme name of the file:

		<model type><which of the 3 files it is><event window info>
		
	Save to dta filename
	
	Key output file: inputs/event-level_CAPM.dta contains CAR[-2,2] using CAPM model 
		
	*/

	rename (lpermno adate) (permno evtdate)
	merge m:1 permno evtdate using "inputs/event-level_CAPM", 	keep(1 3)
	
	rename (cret car bhar) =_CAPM
	drop nrets* Model

	g negative_car = car_CAPM < 0 if !missing(car_CAPM)	
	
	sort permno evtdate
	
	* sometimes the merger announcement isn't a trading day, and 
	* eventus outputs the event with a date set to the next trading day
	
	foreach v of varlist *_CAPM {
		// move the eventus data back to the announcement date
		bysort permno (evtdate): replace `v' = `v'[_n+1] if _m == 1 & _m[_n+1] == 2 & (abs(evtdate-evtdate[_n+1]) <= 4)
	}
	
	foreach v of varlist *_CAPM {
		// delete the excess lines of data this created 
		drop if _m[_n-1] == 1 & _m == 2 & (abs(evtdate[_n-1]-evtdate) <= 4) & permno == permno[_n-1]
	}

	count
	mdesc
	drop _m
	
	* lets get the right fiscal year  (and gvkey)
	
	drop fyear // to this point, we'd used calendar year (it was good enough to snag permnos)
	distinct permno evtdate, j
	distinct dealnum
	
	preserve
		use lpermno gvkey fyear datadate using "$temp/fy_panel", clear
		tempfile fy_id
		save `fy_id'
	restore
	
	rename permno lpermno
	joinby lpermno using `fy_id'
	distinct dealnum
	
	bysort dealnum (datadate): g maybe_bool = datadate >= evtdate
	bysort dealnum (datadate): g maybe_run = sum(datadate >= evtdate)
	tab maybe_*
	
	distinct dealnum if maybe_bool == maybe_run & maybe_bool == 1
	keep if maybe_bool == maybe_run & maybe_bool == 1
	distinct dealnum
	
	* collapse to firm year 
		
	g frac_valuedestroy = car_CAPM < 0 if !missing(car_CAPM)	
	
	collapse (count) ma_count = dealnum (sum) transvalue ///
		(mean) frac_value* car_CAPM, by(gvkey fyear)
	count
	
	g count_valuedestroy       = ma_count*frac_valuedestroy
	
	* merge to sample 
	
	merge 1:1 gvkey fyear using "$temp/fy_panel"
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	* fill in activity variables as 0 
	
	foreach v in ma_count transvalue count_valuedestroy  {
		replace `v' = 0 if missing(`v')
	}
	
	* run some tests 
			
	eststo clear
	
	// logY
	foreach v in ma_count count_valuedestroy  {
		cap g l_`v' = log(1+`v')

		// log Y+1
		rename l_`v' Y // myPostEst likes Y 
		eststo, title("log `v'"): reghdfe Y Rpt_Comm Rp pt_Comm $extra_covariates  if   close & inrange(fyear, 1999,2006),  cluster(gvkey)  absorb(gvkey i.fyear#i.sic2) nocons
		estadd local firm_FE "Yes"
		estadd local yearInd_FE "Yes"
		myPostEst				
		rename Y l_`v' 
	}	
	
	// booleanY
	foreach v in ma_count count_valuedestroy  {
		cap g b_`v' = `v' > 0
		
		// boolean: y>0
		rename b_`v' Y // myPostEst likes Y 
		eststo, title("0<`v'"): reghdfe Y Rpt_Comm Rp pt_Comm $extra_covariates  if   close & inrange(fyear, 1999,2006),  cluster(gvkey)  absorb(gvkey i.fyear#i.sic2) nocons		
		estadd local firm_FE "Yes"
		estadd local yearInd_FE "Yes"
		myPostEst				
		rename Y b_`v' 
	}	
	
	// y 
	foreach v in gdwlip_impairment_bool car_CAPM frac_valuedestroy   {
		rename `v' Y
		eststo, title("`v'"): reghdfe Y Rpt_Comm Rp pt_Comm $extra_covariates  if   close & inrange(fyear, 1999,2006),  cluster(gvkey)  absorb(gvkey i.fyear#i.sic2) nocons
		estadd local firm_FE "Yes"
		estadd local yearInd_FE "Yes"
		myPostEst				
		rename Y `v'
	}		

	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("IA3_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm   ) ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) ///
			label mtitles   ///
		)  ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm if close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')
}	
********************************************************************************	
* Table IA4: risk taking 
********************************************************************************

if $using_fake_data == 0 {
	
	* point here is to look at risk taking, per Ref #2 point abotu mechanism. 

	* idio using beta loading from prior year 
	* here we account for exact fiscal dates (not cal year)	
	
	clear
			
	odbc load, exec("SELECT * FROM crsp.dsi WHERE date BETWEEN '1996-01-01' AND '2007-09-01'") dsn("wrds-pgdata-64")
	keep date 
	rename date datadate 
	g trading_date = 1
	tempfile trading_dates
	save `trading_dates'
	
	* get permnos in main test
	
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
		
	reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		
	keep if e(sample)
	
	/* get firm-date for filings. adjust it to be trading dates */
	
	preserve
	
		// a sledgehammer approach:
		keep lpermno datadate
		g sample_datadate_raw = datadate // will need this to merge back to our compustat sample 
		format sample_datadate_raw %td
		merge m:1 datadate using `trading_dates' , keep(1 3)
		g found_nearest_trading_date = _m == 3
		replace datadate = datadate - 1 if !found_nearest_trading_date
		tab found_nearest_trading_date
		drop _m
		
		merge m:1 datadate using `trading_dates' , keep(1 3)
		g update = _m ==3 & found_nearest_trading_date == 0 // 
		replace found_nearest_trading_date = 1 if update == 1
		replace datadate = datadate - 1 if !found_nearest_trading_date
		tab found_nearest_trading_date
		drop _m update
		
		merge m:1 datadate using `trading_dates' , keep(1 3)
		g update = _m ==3 & found_nearest_trading_date == 0 // 
		replace found_nearest_trading_date = 1 if update == 1
		replace datadate = datadate - 1 if !found_nearest_trading_date
		tab found_nearest_trading_date
		drop _m update
		
		merge m:1 datadate using `trading_dates' , keep(1 3)
		g update = _m ==3 & found_nearest_trading_date == 0 // 
		replace found_nearest_trading_date = 1 if update == 1
		replace datadate = datadate - 1 if !found_nearest_trading_date
		tab found_nearest_trading_date
			
		keep lpermno datadate sample_datadate_raw
		rename (lpermno datadate) (permno date)
		tempfile sample_dates
		save `sample_dates'
	restore
	
	keep lpermno
	duplicates drop lpermno, force
	rename lpermno permno 
	
	* permnos for CRSP!
	* to download returns from crsp, need a list of permnos
		
	qui flevelsof permno, local(myvars) // faster than levelsof
	local myvars : subinstr local myvars " " ",", all 
	di length("`myvars'")
	
	* obdc crsp for these permnos 
	
		distinct permno 
		local n_firms = `r(ndistinct)' // for post-check

		* download daily returns + this also grabs delisting codes (irrelevant here due to sample construction)

		clear
				
		odbc load, exec("SELECT a.permno, a.date, a.ret, b.vwretd, a.prc, a.shrout, c.dlret, c.dlstcd FROM crsp.dsf a LEFT JOIN crsp.dse c ON a.permno = c.permno AND a.date = c.date LEFT JOIN crsp.dsi b ON a.date = b.date WHERE a.permno IN ( `myvars' ) AND a.date BETWEEN '1996-01-01' AND '2007-09-01'") dsn("wrds-pgdata-64")
		
		sort permno date

		distinct permno
		di "We have return data on " `n_firms' "/" `r(ndistinct)' " firms"
		
		replace ret = ret+dlret if !missing(dlret)
		replace ret = ret - .3 if missing(dlret) & !missing(dlstcd)	 // Shumway (1997)
		drop if missing(ret) // these are the first obs of firm. ie if ipo is Sept 20, first monthly ret is Oct 31 (msf puts rets at month end)
		
	* fill out the panel with all dates so that our sample dates are definitely avail
	
	duplicates drop permno date ret, force 
	bysort permno date: drop if _N > 1 & missing(dlret) // when dlret is found, the firm-date obs is duplicated (once with and without dlret)			
			
	xtset permno date
*tsfill, full
	
	* get the sample filing dates
	
	merge 1:1 permno date using `sample_dates'
	distinct permno date if _m == 3, j
	di "We have tagged filing dates for " `r(ndistinct)' " obs"
			
	* estimate capm beta for year t	via rangestat	
			
*gen low  = cond(_m == 3, date-251*1+1, 1)
	gen low  = cond(_m == 3, date-365*1+1, 1)
	gen high = cond(_m == 3, date,   0)
	
	count if low < high // rangestat runs on these regs 
	
	rangestat (reg) ret vwretd, interval(date low high) by(permno)
	rename b_vwretd capm_beta_this_year 
	order capm_beta_this_year
	drop reg_nobs-se_cons
	
	rangestat (sd) ret, interval(date low high) by(permno)
	lab var ret_sd "SD(rawreturns)"

	* estimate capm for year-1 on these dates (so we can use the loadings in year t)
	
	drop low high
	gen low  = cond(_m == 3, date-365*2+1, 1)
    gen high = cond(_m == 3, date-365*1,   0)
	
	count if low < high // rangestat runs on these regs 
	
	rangestat (reg) ret vwretd, interval(date low high) by(permno)
		
	preserve 
		keep permno date ret vwretd 
		tempfile lil_crsp
		save `lil_crsp'
	restore 
		
	* reduce to filing dates, and for each, build all the dates from the fiscal year
	
	keep if !missing(b_vwretd) 
	keep if reg_nobs >= 100
	count 
	keep permno date b_* capm_beta_this_year ret_sd sample_datadate_raw
	expand 365
	rename date datadate
	bysort permno datadate: g date = datadate + 1 - _n
		
	* merge in the return data for that year
	
	merge m:1 permno date using `lil_crsp', keep(1 3)
	mdesc
	
	* get capm resid in year t (with beta and alpha from t-1)
	
	g resid = ret - (b_cons + b_vwretd*vwretd)
		
	* collapse to firm-year 
	
	drop datadate // this is the nearest trading date at this point, not the filing date
	rename sample_datadate_raw datadate
	
	gcollapse (sd) ret resid (mean) capm_beta_this_year, by(permno datadate)
	rename (ret resid) sd_=
	
	* merge in sample 
	
	rename permno lpermno
	merge 1:1 lpermno datadate using "$temp/fy_panel", keep(2 3) nogen
	
	* now run main test
	
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	eststo clear
	
	* run main test to verify sample and outputs
	
	reghdfe prof_a Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)	
	sum sd_ret sd_resid if e(sample)
	
	* now run tests:

	global 	extra_covariates_noLEV l.l_a  l.capx_a l.xrd_a L.SIZE L.BOARD_IND L.incoming_frac L.outgoing_frac  // we shouldn't control for lag leverage in firm FE models with leverage on LHS
	
	eststo clear
	
	foreach v in sd_ret capm_beta_this_year sd_resid    {
		rename `v' Y
		eststo, title("`v'"): reghdfe Y Rpt_Comm Rp pt_Comm $extra_covariates  if   close & inrange(fyear, 1999,2006),  cluster(gvkey)  absorb(gvkey i.fyear#i.sic2) nocons
		estadd local firm_FE "Yes"
		estadd local yearInd_FE "Yes"
		myPostEst				
		rename Y `v'
	}			
	
	foreach v in  td_a td_mve  cash_a {
		rename `v' Y
		eststo, title("`v'"): reghdfe Y Rpt_Comm Rp pt_Comm $extra_covariates_noLEV  if   close & inrange(fyear, 1999,2006),  cluster(gvkey)  absorb(gvkey i.fyear#i.sic2) nocons
		estadd local firm_FE "Yes"
		estadd local yearInd_FE "Yes"
		myPostEst				
		rename Y `v'
	}		
		
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("IA4_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm   ) ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) ///
			label mtitles   ///
		)  ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm if close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')
}
	
*************************************************************************
* Table IA5: other Y   
*************************************************************************
{
	use "$temp/fy_panel", clear 
	merge m:1 cusip6 using "$temp/cusip_R_t.dta", keep(3) nogen 
	define_DDD_vars -5 5 2003 
	xtset gvkey fyear
	
	* std the continuous controls 
	local myvarlist l_a td_a  capx_a xrd_a SIZE BOARD_IND incoming_frac outgoing_frac
	center `myvarlist', s inplace
	sum `myvarlist'
	
	eststo clear	
		
	foreach v in l_emp q  {
		local lab: var lab `v'
		di "`lab'"			
		rename `v' Y
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		 noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
			
		eststo, title("`lab'"): reghdfe 	Y Rpt_Comm Rp pt_Comm  $extra_covariates if   close & inrange(fyear, 1999,2006), cluster(gvkey)  absorb(gvkey i.fyear#i.sic2)		 noconst
			estadd local firm_FE "Yes"
			estadd local yearInd_FE "Yes"
			myPostEst	
						
		rename Y `v'
	}	
	
	esttab_to_excel_sheet using "$excel_filename", ///
		sheet_name("IA5_raw") ///
		temp_csv("temp12345.csv") ///
		esttab_options( ///
			drop( ) star(* 0.10 ** 0.05 *** 0.01 ) b(3) compress replace nogaps  order(Rpt_Comm   ) ///
			stats(firm_FE yearInd_FE N r2 r2_a meanY , fmt(0 0 %9.0fc 2 2 2 ) labels( "firmFE" "yearIndFE" "Obs" "R2" "Adj.R2" "MeanY") ) ///
			label nomtitles mgroups("Log(L)" "Q", pattern(1 0 1))  ///
		)  ///
		notes(`""reghdfe 	Y Rpt_Comm Rp pt_Comm if close, cluster(gvkey)  absorb(gvkey fyear#sic2)	""')
}
		
	log close _all 
				