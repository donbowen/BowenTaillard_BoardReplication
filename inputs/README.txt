ISS_cusip6_to_CCM_cusip6.CSV

	If you replace cusip6 in the ISS database with cusip6_ccm, you can merge to CCM. 
	This file only contains observations where the change needs to be made. 
	
new_independence_data.dta

	firm-director-year dataset with independence variable 
	that matchs exchange classification rule
	
	See the Online Appendix for details. 

	dir_id is an amolgolation of ISS's legacy_dir and director_detail_id variables. See format_ISS_director_data.do for details. 
		
new_SGA_data.xlsx

	cogs2 and xsga2 are versions of cogs and xsga that are harmonized across observations of a firm so 
	that changes to accounting treatment during the sample period do not drive estimations. This data 
	collection process was initiated to fill in missing SGA data. 