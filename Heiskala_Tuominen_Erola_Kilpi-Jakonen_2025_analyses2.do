****************************************************************************************************************************

	* Heiskala, Tuominen, Erola, Kilpi-Jakonen (2025): 
	* Economic Circumstances of Children Living in Higher and Lower-Educated Families and the Contribution of Household Structure: A Cross-Country Comparison with a Child's Perspective
	
	* Analyses, part II (Figure 6)
	* Note: Visualized using Excel.
		
****************************************************************************************************************************
	
	
******************************************************************************** 
*****************   FIGURE 6: PREPARATION       ******************************** 
******************************************************************************** 

* manually over countries 11 12 13 15 18 19 (no 20) 23 25 26 28

******************************************************************************** 
*   11

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 11
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable	
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 43
		local diff_total = 28
		local diff_unexplained = 23
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_11_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close					

******************************************************************************** 
*   12

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 12
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 43
		local diff_total = 16
		local diff_unexplained = 12
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_12_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close					
******************************************************************************** 
*   13

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 13
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income 
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 42
		local diff_total = 20
		local diff_unexplained = 19
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_13_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close	

******************************************************************************** 
*   15

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 15
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 41
		local diff_total = 28
		local diff_unexplained = 24
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_15_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close			

******************************************************************************** 
*   18

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 18
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 45
		local diff_total = 15
		local diff_unexplained = 12
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_18_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close						

******************************************************************************** 
*   19

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 19
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 44
		local diff_total = 35
		local diff_unexplained = 28
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_19_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income high-base: `first_diff'"
		di "Income counter-base: `secondfirst_diff'"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close				

******************************************************************************** 
*   23

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 23
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income 
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 36
		local diff_total = 27
		local diff_unexplained = 22
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_23_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close	
******************************************************************************** 
*   25

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 25
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 44
		local diff_total = 23
		local diff_unexplained = 19
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_25_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close	
		
******************************************************************************** 
*   26

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 26
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 46
		local diff_total = 16
		local diff_unexplained = 13
		
		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_26_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close	
		

******************************************************************************** 
*   28

	* open the data
		use GGS_final.dta, clear
	
	* restrict to one country
		keep if acountry == 28
		  	
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
	mi passive: replace hh_income2 = own_income if partner == 0 // making sure that only partners who are resident are considered contributing to the household income
	mi passive: gen eq_income = hh_income2 / sqrt(ahhsize)

	
	* percentile rank from equivalenced household annual net income (for Norway: gross from registers)	
		sort eq_income
		mi passive: egen rank = rank(eq_income)
		mi passive: egen count = count(eq_income)
		mi passive: gen hh_eq_rank = 100 * (rank / count)	
		
		* round income rank variable
		mi passive: replace hh_eq_rank = round(hh_eq_rank)
	
********************************************************************************
*****************   FIGURE 6: CALCULATION (MI DATA)   **************************
********************************************************************************
		* Parameters
		local base = 46
		local diff_total = 25
		local diff_unexplained = 21

		* Mean income among those without a HE degree
		mean eq_income if hh_eq_rank == `base'
		local income_base = r(table)[1,1]
		
		* Mean income among those who have a HE degree (base + difference)
		mean eq_income if hh_eq_rank == (`base' + `diff_total')
		local income_high = r(table)[1,1]

		* "unexplained" income in €
		mean eq_income if hh_eq_rank == (`base' + `diff_unexplained')
		local counterfactual_diff = r(table)[1,1]

		* Median income in €
		centile eq_income, centile(50)
		local income_median = r(c_1)

		* Differences
		local first_diff = `income_high'-`income_base'
		local second_diff = `counterfactual_diff'-`income_base'
		
		* Relation to the median
		local rel_first_diff = `first_diff' / `income_median'
		local rel_second_diff = `second_diff' / `income_median'

		log using country_28_fig6.log, text replace
		
		* Print
		di "---------------------------------------------"
		di "Income (first diff) / median: `rel_first_diff'"
		di "Income (second diff) / median: `rel_second_diff'"
		di "---------------------------------------------"

		log close	
