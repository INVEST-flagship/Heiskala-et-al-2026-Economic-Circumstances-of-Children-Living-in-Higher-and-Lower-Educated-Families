****************************************************************************************************************************

	* Heiskala, Tuominen, Erola, Kilpi-Jakonen (2025): 
	* Economic Circumstances of Children Living in Higher and Lower-Educated Families and the Contribution of Household Structure: A Cross-Country Comparison with a Child's Perspective
	
	* Analyses, part I (excluding Figure 6)
	* Note: The confidence intervals shown in Figure 5 were calculated manually in Excel based on the analyses below. All figures were visualized using Excel.
		
****************************************************************************************************************************

	* some descriptives with the whole sample
	
	* open the data
		use GGS_final.dta, clear // this dataset is created in "Heiskala_Tuominen_Erola_Kilpi-Jakonen_2025_data"
	
	* keep all the countries first
	
******************************************************************************** 
***************************** CHILD PERSPECTIVE ******************************** 
******************************************************************************** 
	
	* individual id for each respondent (now arid not unique, gets same values in different countries)
		gen id_new = _n
	
	* CREATING THE SAMPLE: CHILD PERSPECTIVE
		* expand rows according to number of residents in the household
		expand ahhsize, gen(new)			
		
		* sort by 'family' id
		sort id_new		
		
		* id for each individual in this sample = each person in the data/households		
		gen id_child = _n
		
		* sort and create a seq variable by 'family'
		sort id_new	id_child		
		bysort id_new: gen obsnum = _n
		sort id_new	id_child		

		* obsnum 1 is hh number 1, obsnum 2 is hh number 2 etc...
		* relation of this individual to the respondent
		gen position = .
	
		forvalues i = 1(1)17 {
		replace position = ahg3_`i' if obsnum == `i'
		}

		* age of this individual
		gen aged = .

		forvalues i = 1(1)17 {
		replace aged = ahg6y_`i' if obsnum == `i'
		}
		
		replace aged = ayear-aged

		* keep only children (relation to the respondent 2-6)
		keep if position >= 2 & position <=6
		
		* keep only children aged less than 18
		keep if aged < 18

	
******************************************************************************** 
******************************    TABLE 1     ********************************** 
******************************************************************************** 
		* Sample by countries: Sample population
		ta acountry
	
******************************************************************************** 
******************************  TABLE 2   **************************************
******************************************************************************** 		
		bysort acountry: ta paredu [aw=aweight]
		ta paredu [aw=aweight]
		
******************************************************************************** 
******************************  FIGURES 2 & 3   ********************************
******************************************************************************** 
		
		* FIG 2
		foreach c in 11 12 13 15 18 19 20 23 25 26 28 {
		mean partner [aw=aweight] if acountry == `c', over(paredu)
		}
		
		* FIG 3
		foreach c in 11 12 13 15 18 19 20 23 25 26 28 {
		mean numres [aw=aweight] if acountry == `c', over(paredu)
		}
		
********************************************************************************		
***************************** IMPUTATION ANALYSES ****************************** 
******************************************************************************** 	
		

	
	* loop over countries
	clear
	
	foreach c in 11 12 13 15 18 19 20 23 25 26 28 {

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == `c'
	
******************************************************************************** 
***************************** CHILD PERSPECTIVE ******************************** 
******************************************************************************** 
	
	* individual id for each respondent (now arid not unique, gets same values in different countries)
		gen id_new = _n
	
	* CREATING THE SAMPLE: CHILD PERSPECTIVE
		* expand rows according to number of residents in the household
		expand ahhsize, gen(new)			
		
		* sort by 'family' id
		sort id_new		
		
		* id for each individual in this sample = each child		
		gen id_child = _n
		
		* sort and create a seq variable by 'family'
		sort id_new	id_child		
		bysort id_new: gen obsnum = _n
		sort id_new	id_child		

		* obsnum 1 is hh number 1, obsnum 2 is hh number 2 etc...
		* relation of this individual to the respondent
		gen position = .
	
		forvalues i = 1(1)17 {
		replace position = ahg3_`i' if obsnum == `i'
		}

		* age of this individual
		gen aged = .

		forvalues i = 1(1)17 {
		replace aged = ahg6y_`i' if obsnum == `i'
		}
		
		replace aged = ayear-aged

		* keep only children (relation to the respondent 2-6)
		keep if position >= 2 & position <=6
		
		* keep only children aged less than 18
		keep if aged < 18

	
******************************************************************************** 
***************************** MULTIPLE IMPUTATION ****************************** 
******************************************************************************** 
	
	misstable summarize own_income partner_income aage partner age_childborn numres asex acountry paredu paredu8cat aweight
	
	* preparation
	mi set mlong
	
	* register the variable that we want to have imputed
	mi register imputed own_income partner_income age_childborn

	* register all the other variables
	mi register regular numres aage female aweight partner paredu
	
	* impute and include all the variables we have in the analytical model (let's do 20 datasets, and rseed can be any number)
	mi impute chained ///
		(regress) age_childborn own_income partner_income ///
		= numres aage female paredu partner, ///
		add(20) rseed(1253)
	
	* Passive variables

	mi passive: gen hh_income2 = own_income + partner_income if partner == 1
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income (only few hundred changes made, so partner's income imputed quite correctly)
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	

	* last edits to variables, mean centering
		
		summarize age_childborn
		local mean_age1 = r(mean)
		mi passive: gen agechildborn_c = age_childborn - `mean_age1'

		summarize aage
		local mean_age2 = r(mean)
		mi passive: gen age_c = aage - `mean_age2'
		
******************************************************************************** 
********************    DESCS WITH IMPUTED DATA: FIG 1 & 4    ****************** 
******************************************************************************** 		
	
	log using country_`c'_desc.log, text replace
			
		mi estimate: mean hh_eq_rank [aw=aweight], over(paredu)
		mi estimate: mean age_childborn [aw=aweight], over(paredu)
	
	* Close the log for this country
		log close	
		
******************************************************************************** 
***************************** FIGURE 5.         ******************************** 
******************************************************************************** 

	* Loop over imputations
	* Start logging (name includes country code)
    log using country_`c'_figure5.log, text replace
	
			forvalues m = 1/20 {
				mi xeq `m': oaxaca hh_eq_rank ///
					(family: partner numres agechildborn_c) ///
					(controls: female age_c) ///
					[pweight=aweight], ///
					by(paredu) pooled swap vce(cluster id_new)

					}
	* Close the log for this country
    log close				
	
******************************************************************************** 
***************************** APPENDIX.         ******************************** 
******************************************************************************** 

	* drop single-hh fathers and those with highly educated fathers
	* in other words, keep single-mothers with low and high edu, and two parent households with high educated mothers -> the only thing that can vary in parental education is then mother's edu

	log using country_`c'_appendix.log, text replace
	drop if paredu8cat >= 6 | paredu8cat == 2
	
			* Loop over imputations
			forvalues m = 1/20 {
				mi xeq `m': oaxaca hh_eq_rank ///
					(family: partner numres agechildborn_c) ///
					(controls: female age_c) ///
					[pweight=aweight], ///
					by(paredu) pooled swap vce(cluster id_new)

					}
		* Close the log for this country
    log close					
		
		
	}
		

		
		