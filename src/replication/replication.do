/*=====================================================================================================================================
Project:		OECD Legal Needs replication
Author(s):		Natalia Rodriguez
Dependencies:  	World Justice Project
Creation Date:	August 2025
=======================================================================================================================================*/

clear all
cls


/*=====================================================================================================================================
					0. Settings and Data Loading
=====================================================================================================================================*/


*--- Required packages:
* NONE


*--- Defining directories paths:
// First, we define the path to the SharePoint in your local computer (This is different for EACH USER):

*------ (a) Natalia Rodriguez:
if (inlist("`c(username)'", "nrodriguez")) {
	global path2SP "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic"
}

*------ (b) Any other user: PLEASE INPUT YOUR PERSONAL PATH TO THE SHAREPOINT DIRECTORY:
else {
	global path2SP ""
}

// Second, we define the path to the data and reference do-files (This is the same for ALL USERS):
// No need to change

global doFiles ""
global data "${path2SP}\EU Subnational\EU-S Data\eu-gpp\1. Data\3. Merge"
global exp "${path2SP}\Data Analytics\6. Country Reports\OECD-EULNS\data\replication"

*------ Loading Data - region names and weights

import excel "${path2SP}\EU Subnational\EU-S Data\reports\eu-thematic-reports\data-viz\inputs\region_labels.xlsx", sheet("Hoja1") firstrow clear

save "${path2SP}\EU Subnational\EU-S Data\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta", replace


*------ Loading Data - EU_GPP merged
use "$data\\EU_GPP_2024.dta", clear


/*=====================================================================================================================================
					1. Recoding variables
=====================================================================================================================================*/

keep if country_name_ltn=="Italy" | country_name_ltn=="Malta"


*------ A2J variables

*** Problem prevalence

recode AJP_A1_bin AJP_A2_bin AJP_A3_bin AJP_B1_bin AJP_B2_bin AJP_B3_bin AJP_B4_bin AJP_C1_bin AJP_C2_bin AJP_C3_bin AJP_C4_bin AJP_D1_bin AJP_D2_bin AJP_D3_bin AJP_D4_bin AJP_D5_bin AJP_D6_bin AJP_E1_bin AJP_E2_bin AJP_E3_bin AJP_F1_bin AJP_F2_bin AJP_G1_bin AJP_G2_bin AJP_G3_bin AJP_H1_bin AJP_H2_bin AJP_H3_bin AJP_I1_bin AJP_J1_bin AJP_J2_bin AJP_J3_bin AJP_J4_bin AJP_K1_bin AJP_K2_bin AJP_K3_bin AJP_L1_bin AJP_L2_bin (2=0)

egen prevalence11=rowtotal(AJP_A1_bin AJP_A2_bin AJP_A3_bin AJP_B1_bin AJP_B2_bin AJP_B3_bin AJP_B4_bin AJP_C1_bin AJP_C2_bin AJP_C3_bin AJP_C4_bin AJP_D1_bin AJP_D2_bin AJP_D3_bin AJP_D4_bin AJP_D5_bin AJP_D6_bin AJP_E1_bin AJP_E2_bin AJP_E3_bin AJP_F1_bin AJP_F2_bin AJP_G1_bin AJP_G2_bin AJP_G3_bin AJP_H1_bin AJP_H2_bin AJP_H3_bin AJP_I1_bin AJP_J1_bin AJP_J2_bin AJP_J3_bin AJP_J4_bin AJP_K1_bin AJP_K2_bin AJP_K3_bin AJP_L1_bin AJP_L2_bin)

gen prevalence=(prevalence11>0)

*Defining problems with high severity (>=4)
foreach v in A1 A2 A3 B1 B2 B3 B4 C1 C2 C3 C4 D1 D2 D3 D4 D5 D6 E1 E2 E3 F1 F2 G1 G2 G3 H1 H2 H3 I1 J1 J2 J3 J4 K1 K2 K3 L1 L2 {
	gen AJP_`v'_bin_4=AJP_`v'_bin if AJP_`v'_sev>3 & AJP_`v'_sev<98
	replace AJP_`v'_bin_4=0 if AJP_`v'_sev<=3
	replace AJP_`v'_bin_4=. if AJP_`v'_sev>=98
}

*Defining prevalence for high-severity problems
egen prevalence1=rowtotal(AJP_A1_bin_4 AJP_A2_bin_4 AJP_A3_bin_4 AJP_B1_bin_4 AJP_B2_bin_4 AJP_B3_bin_4 AJP_B4_bin_4 AJP_C1_bin_4 AJP_C2_bin_4 AJP_C3_bin_4 AJP_C4_bin_4 AJP_D1_bin_4 AJP_D2_bin_4 AJP_D3_bin_4 AJP_D4_bin_4 AJP_D5_bin_4 AJP_D6_bin_4 AJP_E1_bin_4 AJP_E2_bin_4 AJP_E3_bin_4 AJP_F1_bin_4 AJP_F2_bin_4 AJP_G1_bin_4 AJP_G2_bin_4 AJP_G3_bin_4 AJP_H1_bin_4 AJP_H2_bin_4 AJP_H3_bin_4 AJP_I1_bin_4 AJP_J1_bin_4 AJP_J2_bin_4 AJP_J3_bin_4 AJP_J4_bin_4 AJP_K1_bin_4 AJP_K2_bin_4 AJP_K3_bin_4 AJP_L1_bin_4 AJP_L2_bin_4)

gen prevalence2=(prevalence1>0)


*** Types of problems

	* Accidental Illness & Injury *
		*Coded as 0 if F1 and F2 are answered as no, 1 otherwise
		gen Injury=0
		foreach x in F1 F2 {
			replace Injury=1 if AJP_`x'_bin_4==1
			}

	* Citizenship & ID *
		*Coded as 0 if J1, J2, J3 are answered as no, 1 otherwise. 
		gen Govt_other=0
		foreach x in J1 J2 J3 {
			replace Govt_other=1 if AJP_`x'_bin_4==1
			}

	* Community * 
		*Coded as 0 if C3 and E3 are answered as no, 1 otherwise (at least one yes).
		gen Community=0
		foreach x in  E3 C3 {
			replace Community=1 if AJP_`x'_bin_4==1
			}

	*Consumer problems, coded as 0 if A1, A2 and A3 are answered as no, 1 otherwise (at least one yes). Once again a reminder that all 37 variables in q34 are coded as 1=Yes, 0=No
	gen Consumer=0
	foreach x in A1 A2 A3 {
	replace Consumer=1 if AJP_`x'_bin_4==1
	}

	*Employment problems, coded as 0 if G1, G2 and G3 are answered as no, 1 otherwise (at least one yes).
	gen Employment=0
	foreach x in G1 G2 G3 {
	replace Employment=1 if AJP_`x'_bin_4==1
	}

	*Education problems, coded as 0 if E1 and E2  are answered as no, 1 otherwise (at least one yes).
	gen Education=0
	foreach x in E1 E2 {
	replace Education=1 if AJP_`x'_bin_4==1
	}

	*Family problems, coded as 0 if D1, D2, D3, D4, D5 and D6 are answered as no, 1 otherwise (at least one yes). 
	gen Family=0
	foreach x in D1 D2 D3 D4 D5 D6 {
	replace Family=1 if AJP_`x'_bin_4==1
	}

	*Housing problems, coded as 0 if C1, C2, C3 and C4 are answered as no, 1 otherwise (at least one yes).
	/* GH Note - Updated this on 21 Feb 2023 to omit C3, mirroring Alex's updated categories. */
	gen Housing=0
	foreach x in C1 C2 C4 {
	replace Housing=1 if AJP_`x'_bin_4==1
	}

	*Land problems, coded as 0 if B1, B2, B3 and B4 are answered as no, 1 otherwise (at least one yes). 
	gen Land=0
	foreach x in B1 B2 B3 B4 {
	replace Land=1 if AJP_`x'_bin_4==1
	}

	*Law enforcement problems, for this one we only have q19_I1	 (Being beaten up or arrested without justification by a member of the police or the military.)
	gen law_enf=0
	replace law_enf=1 if AJP_I1_bin_4==1

	*Money & Debt problems, coded as 0 if L1 and L2 are answered as no, 1 otherwise (at least one yes). /* GH Note - 21 Feb 2023 - updated the code so that the full variable name "MoneyDebt" is included in the 'replace' command. Original just said "Money".*/
	gen MoneyDebt=0
	foreach x in L1 L2 K1 K2 K3 {
	replace MoneyDebt=1 if AJP_`x'_bin_4==1
	}

	*Public Services, coded as 0 if H1 and H2 are answered as no, 1 otherwise
	/* GH Note - Updated this on 21 Feb 2023 to include H3, mirroring Alex's updated categories. */
	gen Govt_payment=0
	foreach x in H1 H2 H3 J4 {
	replace Govt_payment=1 if AJP_`x'_bin_4==1
	}
	
	label var Injury "Accidental Illness & Injury" 
	label var Govt_other "Citizenship & ID" 
	label var Community "Community" 
	label var Consumer "Consumer" 
	label var Employment "Employment" 
	label var Education "Education" 
	label var Family "Family" 
	label var Housing "Housing" 
	label var Land "Land and Property" 
	label var law_enf "Law Enforcement" 
	label var MoneyDebt "Money and Debt" 
	label var Govt_payment "Public Services" 

*- Severity for selected problem
gen sev_selected=. 

foreach x in A1 A2 A3 B1 B2 B3 B4 C1 C2 C3 C4 D1 D2 D3 D4 D5 D6 E1 E2 E3 F1 F2 G1 G2 G3 H1 H2 H3 I1 J1 J2 J3 J4 K1 K2 K3 L1 L2 {
		replace sev_selected=AJP_`x'_sev if AJP_problem=="`x'"
	}
	
gen sev_selected_4=sev_selected if sev_selected>=4 & sev_selected<98 & sev_selected!=.

gen non_trivial=.
replace non_trivial=1 if sev_selected>=4 & sev_selected<98 & sev_selected!=.
replace non_trivial=0 if sev_selected<=3 & sev_selected!=. 
 

*** Services

*- Adequate assistance and representation: Run from end to beginning for R validation (case_when replication).
gen access2rep=.
replace access2rep=0 if AJD_inst_advice==98 

replace access2rep=0 if AJD_inst_advice==2  & (AJD_noadvice_reason==4 | AJD_noadvice_reason==5 | AJD_noadvice_reason==6 | AJD_noadvice_reason==7 | AJD_noadvice_reason==8 | AJD_noadvice_reason==9 | AJD_noadvice_reason==10 | AJD_noadvice_reason==98) 

replace access2rep=1 if AJD_inst_advice == 2 & (AJD_noadvice_reason==1 | AJD_noadvice_reason==2 | AJD_noadvice_reason==3)

replace access2rep=0 if AJD_inst_advice==1 & (AJD_adviser_1==1 | AJD_adviser_9==1 | AJD_adviser_98==1)

replace access2rep=1 if AJD_inst_advice==1 & AJD_adviser_1==1 & AJD_expert_adviser==1

replace access2rep=1 if AJD_inst_advice==1 & (AJD_adviser_2 == 1 | AJD_adviser_3 ==1 | AJD_adviser_4 == 1 | AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1) 

replace access2rep=. if non_trivial==.
replace access2rep=. if non_trivial==0

*- Type of advisor 

//Appropriate advice (only the people that went to 2-8 advisers)
gen adv=. 
replace adv=1 if AJD_inst_advice==1 & AJD_adviser_1==1 & AJD_expert_adviser==1
replace adv=1 if AJD_inst_advice==1 & (AJD_adviser_2 == 1 | AJD_adviser_3 ==1 | AJD_adviser_4 == 1 | AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1) 

gen friendfamily=.
replace friendfamily=1 if AJD_adviser_1==1 & AJD_expert_adviser==1
replace friendfamily=0 if AJD_adviser_1==1 & AJD_expert_adviser==2
replace friendfamily=0 if AJD_adviser_1==1 & AJD_expert_adviser==98
replace friendfamily=0 if AJD_adviser_1==1 & AJD_expert_adviser==99
replace friendfamily=0 if AJD_adviser_1==1 & AJD_expert_adviser==.
replace friendfamily=0 if AJD_adviser_1==2 
replace friendfamily=. if adv!=1 | (non_trivial==0 | non_trivial==.)


gen lawyer=.
replace lawyer=1 if AJD_adviser_2==1
replace lawyer=0 if AJD_adviser_2==2
replace lawyer=. if adv!=1
replace lawyer=. if non_trivial==0 | non_trivial==.

gen govlegalaid=. 
replace govlegalaid=1 if AJD_adviser_3==1
replace govlegalaid=0 if AJD_adviser_3==2
replace govlegalaid=. if adv!=1
replace govlegalaid=. if non_trivial==0 | non_trivial==.

gen courtgovbody=.
replace courtgovbody=1 if AJD_adviser_4==1
replace courtgovbody=0 if AJD_adviser_4==2 
replace courtgovbody=. if adv!=1
replace courtgovbody=. if non_trivial==0 | non_trivial==.

gen healthprof=.
replace healthprof=1 if AJD_adviser_5==1
replace healthprof=0 if AJD_adviser_5==2
replace healthprof=. if adv!=1
replace healthprof=. if non_trivial==0 | non_trivial==.

gen tradeunion=.
replace tradeunion=1 if AJD_adviser_6==1 
replace tradeunion=0 if AJD_adviser_6==2
replace tradeunion=. if adv!=1
replace tradeunion=. if non_trivial==0 | non_trivial==.

gen religious=.
replace religious=1 if AJD_adviser_7==1
replace religious=0 if AJD_adviser_7==2
replace religious=. if adv!=1
replace religious=. if non_trivial==0 | non_trivial==. 

gen civilsoc=.
replace civilsoc=1 if AJD_adviser_8==1
replace civilsoc=0 if AJD_adviser_8==2
replace civilsoc=. if adv!=1
replace civilsoc=. if non_trivial==0 | non_trivial==. 

gen otherorg=.
replace otherorg=1 if AJD_adviser_9==1
replace otherorg=0 if AJD_adviser_9==2 
replace otherorg=. if non_trivial==0 | non_trivial==. 

*- Good information and advice
gen access2info=.
replace access2info=1 if AJE_infosource==1 | AJE_infosource==2
replace access2info=0 if AJE_infosource>2 & AJE_infosource<=98
replace access2info=. if non_trivial==.
replace access2info=. if non_trivial==0


*- DRM
gen access2drm=.
replace access2drm=1 if AJR_resolution==1
replace access2drm=0 if AJR_resolution==2
replace access2drm=. if AJR_resolution==98
replace access2drm=. if AJR_resolution==99
replace access2drm=. if non_trivial==.
replace access2drm=. if non_trivial==0


*- No access needed
gen no_access=.

replace no_access=1 if AJR_resolution==2 & (AJR_noresol_reason==3 | AJR_noresol_reason==5 | AJR_noresol_reason==6 | AJR_noresol_reason==7 | AJR_noresol_reason==8)

replace no_access=0 if AJR_resolution==2 & (AJR_noresol_reason==1 | AJR_noresol_reason==2 | AJR_noresol_reason==4 | AJR_noresol_reason==9 | AJR_noresol_reason==10 | AJR_noresol_reason==11)

replace no_access=0 if AJR_resolution==2 & AJR_noresol_reason==98

replace no_access=. if non_trivial==.
replace no_access=. if non_trivial==0

*- Don't know (no access)
gen no_access_dk=.
replace no_access_dk=1 if AJR_resolution==2 & AJR_noresol_reason==98
replace no_access_dk=0 if AJR_resolution==2 & AJR_noresol_reason<98 & AJR_noresol_reason!=. 
replace no_access_dk=. if non_trivial==.
replace no_access_dk=. if non_trivial==0

*- No access no needed
gen no_access_v2=.

replace no_access_v2=1 if AJR_resolution==2 & (AJR_noresol_reason==1 | AJR_noresol_reason==2 | AJR_noresol_reason==4 | AJR_noresol_reason==9 | AJR_noresol_reason==10 | AJR_noresol_reason==11)

replace no_access_v2=0 if AJR_resolution==2 & (AJR_noresol_reason==3 | AJR_noresol_reason==5 | AJR_noresol_reason==6 | AJR_noresol_reason==7 | AJR_noresol_reason==8)

replace no_access_v2=0 if AJR_resolution==2 & AJR_noresol_reason==98
replace no_access_v2=. if non_trivial==.
replace no_access_v2=. if non_trivial==0


*** Status

*- Done, fully resolved
gen done_fully=.
replace done_fully=0 if non_trivial==1
replace done_fully=1 if AJR_state_noresol==4
replace done_fully=1 if AJR_state_resol==4
replace done_fully=. if non_trivial==.
replace done_fully=. if non_trivial==0


*- Done partially
gen done_partially=.
replace done_partially=0 if non_trivial==1
replace done_partially=1 if AJR_state_noresol==3
replace done_partially=1 if AJR_state_resol==3
replace done_partially=. if non_trivial==.
replace done_partially=. if non_trivial==0

*- Too early to say
gen too_early=. 
replace too_early=0 if non_trivial==1
replace too_early=1 if AJR_state_noresol==2
replace too_early=1 if AJR_state_resol==2
replace too_early=. if non_trivial==.
replace too_early=. if non_trivial==0

*- Don't know - No Answer
gen done_dk=.
replace done_dk=0 if non_trivial==1
replace done_dk=1 if AJR_state_noresol==98 | AJR_state_noresol==99
replace done_dk=1 if AJR_state_resol==98 | AJR_state_resol==99
replace done_dk=. if non_trivial==.
replace done_dk=. if non_trivial==0

*- Ongoing
gen ongoing=.
replace ongoing=0 if non_trivial==1
replace ongoing=1 if AJR_state_noresol==1
replace ongoing=1 if AJR_state_resol==1
replace ongoing=. if non_trivial==.
replace ongoing=. if non_trivial==0

*- Outcome (for checks)
gen rp_outcome=. 
replace rp_outcome=1 if AJR_state_noresol==4
replace rp_outcome=0 if AJR_state_noresol==3  
replace rp_outcome=1 if AJR_state_resol==4
replace rp_outcome=0 if AJR_state_resol==3 
replace rp_outcome=. if non_trivial==.
replace rp_outcome=. if non_trivial==0 


*** Process

*- Fair
gen rp_fair=.
replace rp_fair=0 if AJR_fair==2 | AJR_fair==98
replace rp_fair=1 if AJR_fair==1
replace rp_fair=. if AJR_state_noresol<3 & AJR_state_noresol>=98 &  AJR_state_noresol!=. 
replace rp_fair=. if AJR_state_resol<3 & AJR_state_resol>=98 & AJR_state_resol!=.
replace rp_fair=. if non_trivial==.
replace rp_fair=. if non_trivial==0

*- Slow
gen slow=.
replace slow=0 if AJR_slow==2 | AJR_slow==98
replace slow=1 if AJR_slow==1
replace slow=. if AJR_state_noresol<3 & AJR_state_noresol>=98 &  AJR_state_noresol!=. 
replace slow=. if AJR_state_resol<3 & AJR_state_resol>=98 & AJR_state_resol!=.
replace slow=. if non_trivial==.
replace slow=. if non_trivial==0

*- Expensive
gen expensive=.
replace expensive=0 if AJR_expensive==2 | AJR_expensive==98
replace expensive=1 if AJR_expensive==1 
replace expensive=. if AJR_state_noresol<3 & AJR_state_noresol>=98 &  AJR_state_noresol!=. 
replace expensive=. if AJR_state_resol<3 & AJR_state_resol>=98 & AJR_state_resol!=.
replace expensive=. if non_trivial==.
replace expensive=. if non_trivial==0

*- Satisfaction 
gen satifaction=.
replace satifaction=0 if AJR_satis_outcome==3 | AJR_satis_outcome==4 | AJR_satis_outcome==98
replace satifaction=1 if AJR_satis_outcome==1 | AJR_satis_outcome==2
replace satifaction=. if AJR_state_noresol<3 & AJR_state_noresol>=98 &  AJR_state_noresol!=. 
replace satifaction=. if AJR_state_resol<3 & AJR_state_resol>=98 & AJR_state_resol!=.
replace satifaction=. if non_trivial==.
replace satifaction=. if non_trivial==0

*- Solving time
gen rp_time=.
replace rp_time=1 if AJR_solvingtime>=0 & AJR_solvingtime<=12 & AJR_solvingtime!=.
replace rp_time=0 if AJR_solvingtime>12 & AJR_solvingtime!=.
replace rp_time=0 if AJR_solvingtime==-8888
replace rp_time=. if AJR_solvingtime==-9999
replace rp_time=. if AJR_state_noresol<3 & AJR_state_noresol>=98 &  AJR_state_noresol!=. 
replace rp_time=. if AJR_state_resol<3 & AJR_state_resol>=98 & AJR_state_resol!=.
replace rp_time=. if non_trivial==0
replace rp_time=. if non_trivial==.

*- Months
gen months=AJR_solvingtime
recode months (-9999 -8888 =.)
replace months=. if non_trivial==0
replace months=. if non_trivial==.

*- Cost
gen rp_cost=.
replace rp_cost=1 if AJR_solvingcosts==1 & (AJR_costdiff==1 | AJR_costdiff==2 )
replace rp_cost=0 if AJR_solvingcosts==1 & (AJR_costdiff==3 | AJR_costdiff==4 | AJR_costdiff==98 )
replace rp_cost=1 if AJR_solvingcosts==2
replace rp_cost=. if AJR_state_noresol<3 & AJR_state_noresol>=98 &  AJR_state_noresol!=. 
replace rp_cost=. if AJR_state_resol<3 & AJR_state_resol>=98 & AJR_state_resol!=.
replace rp_cost=. if non_trivial==0
replace rp_cost=. if non_trivial==.


*** Hardship

*Hardship
foreach v in AJE_health AJE_emotional AJE_income AJE_drugs {
	gen `v'_n=`v'
	recode `v'_n (2 =0) (98 =0) (99=.)
}

egen hardship_n=rowtotal(AJE_health_n AJE_emotional_n AJE_income_n AJE_drugs_n)
gen hardship=(hardship_n>0)
replace hardship=. if hardship_n==0 & prevalence==0 
gen hardship2=hardship
replace hardship=. if non_trivial==0
replace hardship=. if non_trivial==.

*Health
gen health=AJE_health_n
replace health=. if non_trivial==0
replace health=. if non_trivial==.

*econ
gen econ=AJE_income_n
replace econ=. if non_trivial==0
replace econ=. if non_trivial==.

*relation
gen relation=AJE_emotional_n
replace relation=. if non_trivial==0
replace relation=. if non_trivial==.

*drugs
gen drugs=AJE_drugs_n
replace drugs=. if non_trivial==0
replace drugs=. if non_trivial==.

*------ Problem description
recode AJE_description_1 AJE_description_2 AJE_description_3 AJE_description_4 AJE_description_5 AJE_description_6 AJE_description_7 (2=0)

*------ Perceptions

recode JSE_access2info JSE_access2assis AJE_legalrights AJE_advice (1 2=1) (3 4 98=0) ( 99=.)

*------ Legal empowerment
recode A5_1 A5_2 A5b (2=0) (98 99=.)
egen legal_emp1=rowtotal(A5_1 A5_2 A5b)
gen legal_emp=(legal_emp1>0)

*------ Trust
recode TRT_judges TRT_prosecutors TRT_pda (1 2=1) (3 4 98=0) ( 99=.)

*------ Demographics

*Age
gen age_g=.
replace age_g=1 if age>=18 & age<=24 & age!=.
replace age_g=2 if age>=25 & age<=34 & age!=.
replace age_g=3 if age>=35 & age<=44 & age!=.
replace age_g=4 if age>=45 & age<=54 & age!=.
replace age_g=5 if age>=55 & age<=64 & age!=.
replace age_g=6 if age>=65 & age!=.

*Fin
gen fin2=fin
recode fin2 (1 2=1) (3 4 5=2) (98 99=.)

*Gender
gen gend2=gend
recode gend2 (3 4 =.)



/*=====================================================================================================================================
					2. Export data
=====================================================================================================================================*/

*----- Country level 

#delimit ;
global a2j "prevalence2 
Consumer Family Employment law_enf MoneyDebt Community 
Land Housing Education Govt_payment Govt_other Injury prevalence1
access2info JSE_access2info
access2rep 
friendfamily lawyer courtgovbody tradeunion healthprof civilsoc govlegalaid religious 
JSE_access2assis
access2drm no_access no_access_dk no_access_v2 
done_fully done_partially too_early done_dk ongoing rp_outcome
rp_fair slow expensive
satifaction rp_time months rp_cost
hardship health econ relation drugs AJE_legalrights AJE_description_1 AJE_description_2 AJE_description_3 AJE_description_4 AJE_description_5 AJE_description_6 AJE_description_7 AJE_advice legal_emp TRT_judges TRT_prosecutors TRT_pda" ;
;
#delimit cr

* Total sample counts - Country
preserve

gen cont=1

collapse (sum) cont, by(country_name_ltn)

save "$exp\counts_a2j_path.dta", replace

restore


*Region

preserve

collapse (mean) $a2j, by(country_name_ltn nuts_id)

foreach v in $a2j {
	format %10.0g `v'
}

save "$exp\regional_a2j_path.dta", replace

*Country
merge 1:1 nuts_id using "${path2SP}\EU Subnational\EU-S Data\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
keep if _merge==3
drop _merge

collapse (mean) $a2j [pw=regionpoppct], by(country_name_ltn)

merge 1:1 country_name_ltn using "$exp\counts_a2j_path.dta"
drop _merge

export excel using "$exp\reports_replication.xlsx", replace firstrow(var) sheet("Overall")
putexcel set "$exp\reports_replication.xlsx", sheet("Overall") modify
putexcel B2:X28, overwri nformat("0.0%") 
*putexcel Q2:Q28, overwri nformat(number) 
putexcel A1:Y1, overwri bold hcenter txtwrap

restore


*----- Age group
preserve

collapse (mean) $a2j, by(country_name_ltn nuts_id age_g)

foreach v in $a2j {
	format %10.0g `v'
}

*Country
merge m:1 nuts_id using "${path2SP}\EU Subnational\EU-S Data\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
keep if _merge==3
drop _merge

collapse (mean) $a2j [pw=regionpoppct], by(country_name_ltn age_g)

keep country age_g prevalence2 prevalence1 access2info JSE_access2info access2rep JSE_access2assis access2drm rp_time slow rp_fair rp_cost expensive rp_outcome satifaction hardship AJE_legalrights AJE_description_1 AJE_description_2 AJE_description_3 AJE_description_4 AJE_description_5 AJE_description_6 AJE_description_7 AJE_advice legal_emp TRT_judges TRT_prosecutors TRT_pda

label define age 1 "18-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "+65"
label values age_g age	

export excel using "$exp\reports_replication.xlsx", firstrow(var) sheet("Age") 
putexcel set "$exp\reports_replication.xlsx", sheet("Age") modify
putexcel B2:X28, overwri nformat("0.0%") 
*putexcel Q2:Q28, overwri nformat(number) 
putexcel A1:Y1, overwri bold hcenter txtwrap

restore

*----- Urban
preserve

collapse (mean) $a2j, by(country_name_ltn nuts_id urban)

foreach v in $a2j {
	format %10.0g `v'
}

*Country
merge m:1 nuts_id using "${path2SP}\EU Subnational\EU-S Data\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
keep if _merge==3
drop _merge

collapse (mean) $a2j [pw=regionpoppct], by(country_name_ltn urban)

keep country urban prevalence2 prevalence1 access2info JSE_access2info access2rep JSE_access2assis access2drm rp_time slow rp_fair rp_cost expensive rp_outcome satifaction hardship AJE_legalrights AJE_description_1 AJE_description_2 AJE_description_3 AJE_description_4 AJE_description_5 AJE_description_6 AJE_description_7 AJE_advice legal_emp TRT_judges TRT_prosecutors TRT_pda

export excel using "$exp\reports_replication.xlsx", firstrow(var) sheet("Urban") 
putexcel set "$exp\reports_replication.xlsx", sheet("Urban") modify
putexcel B2:X28, overwri nformat("0.0%") 
*putexcel Q2:Q28, overwri nformat(number) 
putexcel A1:Y1, overwri bold hcenter txtwrap

restore

*----- Fin
preserve

collapse (mean) $a2j, by(country_name_ltn nuts_id fin2)

foreach v in $a2j {
	format %10.0g `v'
}

*Country
merge m:1 nuts_id using "${path2SP}\EU Subnational\EU-S Data\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
keep if _merge==3
drop _merge

collapse (mean) $a2j [pw=regionpoppct], by(country_name_ltn fin2)

drop if fin2==.

keep country fin2 prevalence2 prevalence1 access2info JSE_access2info access2rep JSE_access2assis access2drm rp_time slow rp_fair rp_cost expensive rp_outcome satifaction hardship AJE_legalrights AJE_description_1 AJE_description_2 AJE_description_3 AJE_description_4 AJE_description_5 AJE_description_6 AJE_description_7 AJE_advice legal_emp TRT_judges TRT_prosecutors TRT_pda
 
label define fin2 1 "Constrained" 2 "Unconstrained"
label values fin2 fin2

export excel using "$exp\reports_replication.xlsx", firstrow(var) sheet("Fin situation") 
putexcel set "$exp\reports_replication.xlsx", sheet("Fin situation") modify
putexcel B2:X28, overwri nformat("0.0%") 
*putexcel Q2:Q28, overwri nformat(number) 
putexcel A1:Y1, overwri bold hcenter txtwrap

restore


*----- Gender
preserve

collapse (mean) $a2j, by(country_name_ltn nuts_id gend2)

foreach v in $a2j {
	format %10.0g `v'
}

*Country
merge m:1 nuts_id using "${path2SP}\EU Subnational\EU-S Data\reports\eu-thematic-reports\data-viz\inputs\region_labels.dta"
keep if _merge==3
drop _merge

collapse (mean) $a2j [pw=regionpoppct], by(country_name_ltn gend2)

drop if gend2==.

keep country gend2 prevalence2 prevalence1 access2info JSE_access2info access2rep JSE_access2assis access2drm rp_time slow rp_fair rp_cost expensive rp_outcome satifaction hardship AJE_legalrights AJE_description_1 AJE_description_2 AJE_description_3 AJE_description_4 AJE_description_5 AJE_description_6 AJE_description_7 AJE_advice legal_emp TRT_judges TRT_prosecutors TRT_pda

label define gend2 1 "Male" 2 "Female"
label values gend2 gend2

export excel using "$exp\reports_replication.xlsx", firstrow(var) sheet("Gender") 
putexcel set "$exp\reports_replication.xlsx", sheet("Gender") modify
putexcel B2:X28, overwri nformat("0.0%") 
*putexcel Q2:Q28, overwri nformat(number) 
putexcel A1:Y1, overwri bold hcenter txtwrap

restore






