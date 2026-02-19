
****************************************************************************************************************************

	* Heiskala, Tuominen, Erola, Kilpi-Jakonen (2025): 
	* Economic Circumstances of Children Living in Higher and Lower-Educated Families and the Contribution of Household Structure: A Cross-Country Comparison with a Child's Perspective
	
	* Data and sample	
		
****************************************************************************************************************************
	
	* Open GGS1/Wave 1 dataset
		use GGS_Wave1.dta, clear
	
	* Append to Norway
		append using GGS_Wave1_Norway_V.4.3.dta
		
	* Drop few countries due to missings in outcomes (no partner's income, income only in ranges/bands, etc.)
		* Japan and Australia not included in this dataset
			
			ta acountry
			ta acountry, nolabel
			drop if acountry == 21 | acountry == 22 | acountry == 17 | acountry == 14 | acountry == 29 | acountry == 16
			
			ta acountry
			ta acountry, nolabel // 11 countries altogether
							
****************************************************************************************************************************
	
	* Modify variables
	
****************************************************************************************************************************
	* Variables to be included:
		* Outcome: household equivalenced annual income -> percentiles for each country 
		* Independent variables: education, partner or not, number of children, age when first child born
		* Controls + weights + other important: arid acountry aregion asex aage ahhsize aweight 
		
		* Partner status (0 not living with partner 1 living with partner)
			gen partner = .
			replace partner = 1 if aparstat == 1
			replace partner = 0 if aparstat == 2 | aparstat == 3
			ta partner aparstat, m	
			
		* Education level (categorical)
					
			* focal person sex: asex
			* focal person edu: aeduc, a148
			
			* partner sex: a384
			* partner edu: a379
			
			* HE degree or not (isced 5 or 6)
			gen he_degree = .
			replace he_degree = 0 if aeduc <5 & aeduc!=.
			replace he_degree = 1 if aeduc == 5 | aeduc == 6
	
			* For France (aeduc missing for France):
			replace he_degree = 0 if acountry == 15 & a148 >= 1501 & a148 <= 1505
			replace he_degree = 1 if acountry == 15 & a148 == 1506 | a148 == 1507
			
			* For Bulgaria (a148 with an extra category):
			replace he_degree = 0 if acountry == 11 & a148 == 1101
			
			ta aeduc he_degree, m
			ta a148 he_degree, m
			
			* For the partner
			gen he_degree_partner = .
			replace he_degree_partner = 0 if a379 <5 & a379!=. & partner == 1
			replace he_degree_partner = 1 if (a379 == 5 | a379 == 6) & partner == 1
			
			* For France (a379 with different values for France):
			replace he_degree_partner = 0 if acountry == 15 & (a379 >= 1501 & a379 <= 1505) & partner == 1
			replace he_degree_partner = 1 if acountry == 15 & (a379 == 1506 | a379 == 1507) & partner == 1
			
			* For Bulgaria (a379 with an extra category):
			replace he_degree_partner = 0 if acountry == 11 & a379 == 1101 & partner == 1
		
			ta a379 he_degree_partner, m
			
			* take the maximum education from both adults in the hh (0 neither have a HE degree 1 either of the adult has a HE degree)
			* based on the 'current partner'
				gen paredu = .
				replace paredu = 0 if he_degree == 0 | he_degree_partner == 0
				replace paredu = 1 if he_degree == 1 | he_degree_partner == 1

			* Sex-specific he_degree
			gen he_degree_male = .
			gen he_degree_female = .

			* Assign education to male variable: use focal's if focal is male, otherwise partner's
			replace he_degree_male = he_degree if asex == 1
			replace he_degree_male = he_degree_partner if a384 == 1

			* Assign education to female variable: use focal's if focal is female, otherwise partner's
			replace he_degree_female = he_degree if asex == 2
			replace he_degree_female = he_degree_partner if a384 == 2
								
			* Parental education, eight categories
			gen paredu8cat = .

			* 0 = both low
			replace paredu8cat = 0 if he_degree_female == 0 & he_degree_male == 0 & partner == 1

			* 1 = female high, male low
			replace paredu8cat = 1 if he_degree_female == 1 & he_degree_male == 0 & partner == 1

			* 2 = female low, male high
			replace paredu8cat = 2 if he_degree_female == 0 & he_degree_male == 1 & partner == 1

			* 3 = both high
			replace paredu8cat = 3 if he_degree_female == 1 & he_degree_male == 1 & partner == 1

			* 4 = only female, low
			replace paredu8cat = 4 if he_degree_female == 0 & he_degree_male == . & partner == 0
			
			* 5 = only female, high
			replace paredu8cat = 5 if he_degree_female == 1 & he_degree_male == . & partner == 0
			
			* 6 = only male, low
			replace paredu8cat = 6 if he_degree_male == 0 & he_degree_female == . & partner == 0
						
			* 7 = only male, high
			replace paredu8cat = 7 if he_degree_male == 1 & he_degree_female == . & partner == 0
			
			
		* Age when first child born
			* age of respondent - age of oldest child
			gen age_childborn = .
			replace age_childborn = aage - ageoldest 
		
			* which do we see as misreporting:
				* at least those when age is -something,
				* and those who have been younger than 13 when the firs child has been born
				* replace those cases as missing
			
			replace age_childborn = . if age_childborn < 13
			
			ta age_childborn
			
			
		****************************************************************************************************************************

		* Household equivalenced income
			* net amount of payment type income X: a866_1 - a866_13 / a938_1 - a938_13
			* income range of payment type income X: a867_1-a867_13 / a939_1-a939_13 
				* (Poland has its own range variables and Norway doesn't have range incomes as income values come from registers)
			* frequency/number of payments recieved last 12 months type income X: a865_1 - a865_13 / a937_1 - a937_13
			
			* The logic:
			
				* 1) calculate what is the median value in income ranges (among those who have reported income) in every country and every income range
				* 2) replace income values with income range values if specific income is missing
				* 3) do the same (use the same median income calculated in part 1) for partner's income 

				* 4) multiply income amount by the frequency of these payments in a year 
					* (this is done for all except Norway and Poland, they have annual incomes already)
				
				* 5) sum all income types (own and partner's) together and then altogether own and partner's income together
				* 6) equivalence scaling 
				* + remember exceptions Poland and Norway
			
				* 7) country-specific percentile from equivalenced household annual net income 
					* (for Norway: gross from registers)

	****************************************************************************************************************************

				* 1) calculate what is the median value in income ranges (among those who have reported income) in every country and every income range
					* take the median from type 1 income (!)

					* create a value for net income which first gets values from specific incomes
					forvalues i = 1(1)13 {
					gen net_income_`i' = a866_`i'
					}
					
					* replace that as missing if it gets value .a
					forvalues i = 1(1)13 {
					replace net_income_`i' = . if net_income_`i' == .a
					}
						
****************************************************************************************************************************
						
					* Bulgaria 11
						ta a867_1 if acountry == 11
						ta a867_1 if acountry == 11, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 11 & a866_1 <= 26, d // 18
						sum a866_1 if acountry == 11 & a866_1 >= 26 & a866_1 < 61, d // 42
						sum a866_1 if acountry == 11 & a866_1 >= 61 & a866_1 < 102, d // 72
						sum a866_1 if acountry == 11 & a866_1 >= 102 & a866_1 < 154, d // 128
						sum a866_1 if acountry == 11 & a866_1 >= 154 & a866_1 < 205, d // 179
						sum a866_1 if acountry == 11 & a866_1 >= 205 & a866_1 < 307, d // 230
						sum a866_1 if acountry == 11 & a866_1 >= 307 & a866_1 < 409, d // 307
						sum a866_1 if acountry == 11 & a866_1 >= 409 & a866_1 < 511, d // 409
						sum a866_1 if acountry == 11 & a866_1 >= 511, d // 767
						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' = 18 if net_income_`i' == . & a867_`i' == 1101 & acountry == 11
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 42 if net_income_`i' == . & a867_`i' == 1102 & acountry == 11
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 72 if net_income_`i' == . & a867_`i' == 1103 & acountry == 11
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 128 if net_income_`i' == . & a867_`i' == 1104 & acountry == 11
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 179 if net_income_`i' == . & a867_`i' == 1105 & acountry == 11
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 230 if net_income_`i' == . & a867_`i' == 1106 & acountry == 11
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 307 if net_income_`i' == . & a867_`i' == 1107 & acountry == 11
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 409 if net_income_`i' == . & a867_`i' == 1108 & acountry == 11
						}		
										
						forvalues i = 1(1)13 {
						replace net_income_`i' = 767 if net_income_`i' == . & a867_`i' == 1109 & acountry == 11
						}		
										
					* Russia 12
						ta a867_1 if acountry == 12
						ta a867_1 if acountry == 12, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 12 & a866_1 < 14, d // 4
						sum a866_1 if acountry == 12 & a866_1 >= 14 & a866_1 < 28, d // 20
						sum a866_1 if acountry == 12 & a866_1 >= 28 & a866_1 < 42, d // 34
						sum a866_1 if acountry == 12 & a866_1 >= 42 & a866_1 < 56 , d // 48
						sum a866_1 if acountry == 12 & a866_1 >= 56 & a866_1 < 71, d // 60
						sum a866_1 if acountry == 12 & a866_1 >= 71 & a866_1 < 85 , d // 71
						sum a866_1 if acountry == 12 & a866_1 >= 85 & a866_1 < 142, d // 100
						sum a866_1 if acountry == 12 & a866_1 >= 142 & a866_1 < 213, d // 156
						sum a866_1 if acountry == 12 & a866_1 >= 213 & a866_1 < 284, d // 228
						sum a866_1 if acountry == 12 & a866_1 >= 284 & a866_1 < 426, d // 284
						sum a866_1 if acountry == 12 & a866_1 >= 426 & a866_1 < 568, d // 427
						sum a866_1 if acountry == 12 & a866_1 >= 568, d // 683
						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' = 4 if net_income_`i' == . & a867_`i' == 1201 & acountry == 12
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 20 if net_income_`i' == . & a867_`i' == 1202 & acountry == 12
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 34 if net_income_`i' == . & a867_`i' == 1203 & acountry == 12
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 48 if net_income_`i' == . & a867_`i' == 1204 & acountry == 12
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 60 if net_income_`i' == . & a867_`i' == 1205 & acountry == 12
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 71 if net_income_`i' == . & a867_`i' == 1206 & acountry == 12
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 100 if net_income_`i' == . & a867_`i' == 1207 & acountry == 12
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 156 if net_income_`i' == . & a867_`i' == 1208 & acountry == 12
						}		
										
						forvalues i = 1(1)13 {
						replace net_income_`i' = 228 if net_income_`i' == . & a867_`i' == 1209 & acountry == 12
						}		
												
						forvalues i = 1(1)13 {
						replace net_income_`i' = 284 if net_income_`i' == . & a867_`i' == 1210 & acountry == 12
						}		
											
						forvalues i = 1(1)13 {
						replace net_income_`i' = 427 if net_income_`i' == . & a867_`i' == 1211 & acountry == 12
						}		
											
						forvalues i = 1(1)13 {
						replace net_income_`i' = 683 if net_income_`i' == . & a867_`i' == 1212 & acountry == 12
						}		
					
					
					* Georgia 13
						ta a867_1 if acountry == 13
						ta a867_1 if acountry == 13, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 13 & a866_1 < 22, d // 12
						sum a866_1 if acountry == 13 & a866_1 >= 22 & a866_1 < 44, d // 28
						sum a866_1 if acountry == 13 & a866_1 >= 44 & a866_1 < 66, d // 46
						sum a866_1 if acountry == 13 & a866_1 >= 66 & a866_1 <  88, d // 66
						sum a866_1 if acountry == 13 & a866_1 >= 88 & a866_1 < 132, d // 105
						sum a866_1 if acountry == 13 & a866_1 >= 132 & a866_1 <  175, d // 153
						sum a866_1 if acountry == 13 & a866_1 >= 175 & a866_1 < 219, d // 175
						sum a866_1 if acountry == 13 & a866_1 >= 219 & a866_1 < 438, d // 234.5
						sum a866_1 if acountry == 13 & a866_1 >= 438, d // 526

						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' =  12 if net_income_`i' == . & a867_`i' == 1301 & acountry == 13
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 28 if net_income_`i' == . & a867_`i' == 1302 & acountry == 13
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 46 if net_income_`i' == . & a867_`i' == 1303 & acountry == 13
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 66 if net_income_`i' == . & a867_`i' == 1304 & acountry == 13
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 105 if net_income_`i' == . & a867_`i' == 1305 & acountry == 13
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 153 if net_income_`i' == . & a867_`i' == 1306 & acountry == 13
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 175 if net_income_`i' == . & a867_`i' == 1307 & acountry == 13
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 234.5 if net_income_`i' == . & a867_`i' == 1308 & acountry == 13
						}		
										
						forvalues i = 1(1)13 {
						replace net_income_`i' = 526 if net_income_`i' == . & a867_`i' == 1309 & acountry == 13
						}		
	
					
					* France 15
						ta a867_1 if acountry == 15
						ta a867_1 if acountry == 15, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 15 & a866_1 < 500, d // 305
						sum a866_1 if acountry == 15 & a866_1 >= 500 & a866_1 < 1000, d // 750
						sum a866_1 if acountry == 15 & a866_1 >= 1000 & a866_1 < 1500, d // 1200
						sum a866_1 if acountry == 15 & a866_1 >= 1500 & a866_1 < 2000, d // 1600
						sum a866_1 if acountry == 15 & a866_1 >= 2000 & a866_1 < 2500, d // 2100
						sum a866_1 if acountry == 15 & a866_1 >= 2500 & a866_1 < 3000, d // 2600
						sum a866_1 if acountry == 15 & a866_1 >= 3000 & a866_1 < 5000, d // 3500
						sum a866_1 if acountry == 15 & a866_1 >= 5000, d // 6500

						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' = 305  if net_income_`i' == . & a867_`i' == 1 & acountry == 15
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 750 if net_income_`i' == . & a867_`i' == 2 & acountry == 15
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1200 if net_income_`i' == . & a867_`i' == 3 & acountry == 15
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 1600 if net_income_`i' == . & a867_`i' == 4 & acountry == 15
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2100 if net_income_`i' == . & a867_`i' == 5 & acountry == 15
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2600 if net_income_`i' == . & a867_`i' == 6 & acountry == 15
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 3500 if net_income_`i' == . & a867_`i' == 7 & acountry == 15
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 6500 if net_income_`i' == . & a867_`i' == 8 & acountry == 15
						}		

					* Netherlands 18
						ta a867_1 if acountry == 18
						ta a867_1 if acountry == 18, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 18 & a866_1 < 550, d // added directly to below
						sum a866_1 if acountry == 18 & a866_1 >= 550 & a866_1 < 750, d  
						sum a866_1 if acountry == 18 & a866_1 >= 750 & a866_1 < 950, d  
						sum a866_1 if acountry == 18 & a866_1 >= 950 & a866_1 < 1150, d  
						sum a866_1 if acountry == 18 & a866_1 >= 1150 & a866_1 < 1350, d  
						sum a866_1 if acountry == 18 & a866_1 >= 1350 & a866_1 < 1550, d  
						sum a866_1 if acountry == 18 & a866_1 >= 1550 & a866_1 < 1750, d  
						sum a866_1 if acountry == 18 & a866_1 >= 1750 & a866_1 < 1950, d  
						sum a866_1 if acountry == 18 & a866_1 >= 1950 & a866_1 < 2150, d  
						sum a866_1 if acountry == 18 & a866_1 >= 2150 & a866_1 < 2350, d  
						sum a866_1 if acountry == 18 & a866_1 >= 2350 & a866_1 < 2550, d  
						sum a866_1 if acountry == 18 & a866_1 >= 2550 & a866_1 < 2750, d  
						sum a866_1 if acountry == 18 & a866_1 >= 2750 & a866_1 < 2950, d  
						sum a866_1 if acountry == 18 & a866_1 >= 2950 & a866_1 < 3150, d  
						sum a866_1 if acountry == 18 & a866_1 >= 3150 & a866_1 < 3350, d  
						sum a866_1 if acountry == 18 & a866_1 >= 3350 & a866_1 < 3550, d  
						sum a866_1 if acountry == 18 & a866_1 >= 3550, d  

						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' =  250 if net_income_`i' == . & a867_`i' == 1801 & acountry == 18
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 628 if net_income_`i' == . & a867_`i' == 1802 & acountry == 18
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 809 if net_income_`i' == . & a867_`i' == 1803 & acountry == 18
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 1000 if net_income_`i' == . & a867_`i' == 1804 & acountry == 18
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1250 if net_income_`i' == . & a867_`i' == 1805 & acountry == 18
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1450 if net_income_`i' == . & a867_`i' == 1806 & acountry == 18
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1600 if net_income_`i' == . & a867_`i' == 1807 & acountry == 18
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1800 if net_income_`i' == . & a867_`i' == 1808 & acountry == 18
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2000 if net_income_`i' == . & a867_`i' == 1809 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2200 if net_income_`i' == . & a867_`i' == 1810 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2500 if net_income_`i' == . & a867_`i' == 1811 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2600 if net_income_`i' == . & a867_`i' == 1812 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2800 if net_income_`i' == . & a867_`i' == 1813 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace net_income_`i' = 3000 if net_income_`i' == . & a867_`i' == 1814 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace net_income_`i' = 3200 if net_income_`i' == . & a867_`i' == 1815 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace net_income_`i' = 3500 if net_income_`i' == . & a867_`i' == 1816 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace net_income_`i' = 4500 if net_income_`i' == . & a867_`i' == 1817 & acountry == 18
						}	
					
					* Romania 19
						ta a867_1 if acountry == 19
						ta a867_1 if acountry == 19, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 19 & a866_1 < 77, d // added directly to below
						sum a866_1 if acountry == 19 & a866_1 >= 77 & a866_1 < 137, d  
						sum a866_1 if acountry == 19 & a866_1 >= 137 & a866_1 < 330, d  
						sum a866_1 if acountry == 19 & a866_1 >= 330 & a866_1 < 523, d  
						sum a866_1 if acountry == 19 & a866_1 >= 523 & a866_1 < 716, d  
						sum a866_1 if acountry == 19 & a866_1 >= 716 & a866_1 < 908, d  
						sum a866_1 if acountry == 19 & a866_1 >= 908 & a866_1 < 1101, d  
						sum a866_1 if acountry == 19 & a866_1 >= 1101 & a866_1 < 1290, d  
						sum a866_1 if acountry == 19 & a866_1 >= 1290 & a866_1 < 1487, d  
						sum a866_1 if acountry == 19 & a866_1 >= 1487, d  

						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' =  44 if net_income_`i' == . & a867_`i' == 1901 & acountry == 19
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 101 if net_income_`i' == . & a867_`i' == 1902 & acountry == 19
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 186 if net_income_`i' == . & a867_`i' == 1903 & acountry == 19
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 401.5 if net_income_`i' == . & a867_`i' == 1904 & acountry == 19
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 573 if net_income_`i' == . & a867_`i' == 1905 & acountry == 19
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 819 if net_income_`i' == . & a867_`i' == 1906 & acountry == 19
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 992 if net_income_`i' == . & a867_`i' == 1907 & acountry == 19
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1147 if net_income_`i' == . & a867_`i' == 1908 & acountry == 19
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1354 if net_income_`i' == . & a867_`i' == 1909 & acountry == 19
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2260.5 if net_income_`i' == . & a867_`i' == 1910 & acountry == 19
						}
						
					* Belgium 23
						ta a867_1 if acountry == 23
						ta a867_1 if acountry == 23, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 23 & a866_1 < 500, d // added directly to below
						sum a866_1 if acountry == 23 & a866_1 >= 500 & a866_1 < 1000, d  
						sum a866_1 if acountry == 23 & a866_1 >= 1000 & a866_1 < 1500, d  
						sum a866_1 if acountry == 23 & a866_1 >= 1500 & a866_1 < 2000, d  
						sum a866_1 if acountry == 23 & a866_1 >= 2000 & a866_1 < 2500, d  
						sum a866_1 if acountry == 23 & a866_1 >= 2500 & a866_1 < 3000, d  
						sum a866_1 if acountry == 23 & a866_1 >= 3000 & a866_1 < 5000, d  
						sum a866_1 if acountry == 23 & a866_1 >= 5000, d 

						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' =  300 if net_income_`i' == . & a867_`i' == 1 & acountry == 23
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 823.5 if net_income_`i' == . & a867_`i' == 2 & acountry == 23
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1200 if net_income_`i' == . & a867_`i' == 3 & acountry == 23
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 1600 if net_income_`i' == . & a867_`i' == 4 & acountry == 23
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2100 if net_income_`i' == . & a867_`i' == 5 & acountry == 23
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2500 if net_income_`i' == . & a867_`i' == 6 & acountry == 23
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 3300 if net_income_`i' == . & a867_`i' == 7 & acountry == 23
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 6375 if net_income_`i' == . & a867_`i' ==  8& acountry == 23
						}	
						
						
						
					* Lithuania 25
					 	ta a867_1 if acountry == 25
						ta a867_1 if acountry == 25, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 25 & a866_1 < 58, d 
						sum a866_1 if acountry == 25 & a866_1 >= 58 & a866_1 < 115, d 
						sum a866_1 if acountry == 25 & a866_1 >= 115 & a866_1 < 173, d 
						sum a866_1 if acountry == 25 & a866_1 >= 173 & a866_1 < 231, d 
						sum a866_1 if acountry == 25 & a866_1 >= 231 & a866_1 < 289, d 
						sum a866_1 if acountry == 25 & a866_1 >= 289 & a866_1 < 347, d 
						sum a866_1 if acountry == 25 & a866_1 >= 347 & a866_1 < 405, d  
						sum a866_1 if acountry == 25 & a866_1 >= 405 & a866_1 < 463, d  
						sum a866_1 if acountry == 25 & a866_1 >= 463 & a866_1 < 521, d  
						sum a866_1 if acountry == 25 & a866_1 >= 521 & a866_1 < 579, d  
						sum a866_1 if acountry == 25 & a866_1 >= 579 & a866_1 < 724, d  
						sum a866_1 if acountry == 25 & a866_1 >= 724 & a866_1 < 868, d  
						sum a866_1 if acountry == 25 & a866_1 >= 868 & a866_1 < 1013, d  
						sum a866_1 if acountry == 25 & a866_1 >= 1013 & a866_1 < 1158, d  
						sum a866_1 if acountry == 25 & a866_1 >= 1158 & a866_1 < 1303, d  
						sum a866_1 if acountry == 25 & a866_1 >= 1303 & a866_1 < 1448, d  
						sum a866_1 if acountry == 25 & a866_1 >= 1448 & a866_1 < 2027, d  
						sum a866_1 if acountry == 25 & a866_1 >= 2027 & a866_1 < 2896, d  
						sum a866_1 if acountry == 25 & a866_1 >= 2896, d  

						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' = 35  if net_income_`i' == . & a867_`i' == 2501 & acountry == 25
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 93 if net_income_`i' == . & a867_`i' == 2502 & acountry == 25
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 139 if net_income_`i' == . & a867_`i' == 2503 & acountry == 25
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 188 if net_income_`i' == . & a867_`i' == 2504 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 231 if net_income_`i' == . & a867_`i' == 2505 & acountry == 25
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 289 if net_income_`i' == . & a867_`i' == 2506 & acountry == 25
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 347 if net_income_`i' == . & a867_`i' == 2507 & acountry == 25
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 434 if net_income_`i' == . & a867_`i' == 2508 & acountry == 25
						}
															
						forvalues i = 1(1)13 {
						replace net_income_`i' = 463 if net_income_`i' == . & a867_`i' == 2509 & acountry == 25
						}	
															
						forvalues i = 1(1)13 {
						replace net_income_`i' = 521 if net_income_`i' == . & a867_`i' == 2510 & acountry == 25
						}	
														
						forvalues i = 1(1)13 {
						replace net_income_`i' = 579 if net_income_`i' == . & a867_`i' == 2511 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 781 if net_income_`i' == . & a867_`i' == 2512 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 868 if net_income_`i' == . & a867_`i' == 2513 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1157 if net_income_`i' == . & a867_`i' == 2514 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1302 if net_income_`i' == . & a867_`i' == 2515 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1447 if net_income_`i' == . & a867_`i' == 2516 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1736 if net_income_`i' == . & a867_`i' == 2517 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 2604 if net_income_`i' == . & a867_`i' == 2518 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 3732 if net_income_`i' == . & a867_`i' == 2519 & acountry == 25
						}	
						
					* Czech Republic 28
						/* 
						Using convertion values from Forbes 10.1.2024
						
						4900 Kc or less  -> - 199€
                        5000 - 6999 Kc   -> 200€ -  284€
                        7000 - 8999 Kc   -> 285€ - 366€
                        9000 - 11999 Kc  -> 367€ - 488€
                        12000 - 14999 Kc -> 489€ - 610€
                        15000 - 16999 Kc -> 611€ - 692€
                        17000 - 19999 Kc -> 693€ - 814€
                        20000 - 24999 Kc -> 815€ - 1017€
                        25000 - 29999 Kc -> 1018€ - 1221€
                        30000+ Kc        -> 1222€ -
						*/
					
						ta a867_1 if acountry == 28
						ta a867_1 if acountry == 28, nolabel
						
						* take the median for all the country-specific income range groups
						sum a866_1 if acountry == 28 & a866_1 < 200, d // added directly to below
						sum a866_1 if acountry == 28 & a866_1 >= 200 & a866_1 < 285, d 
						sum a866_1 if acountry == 28 & a866_1 >= 285 & a866_1 < 366, d 
						sum a866_1 if acountry == 28 & a866_1 >= 366& a866_1 < 488, d 
						sum a866_1 if acountry == 28 & a866_1 >= 488 & a866_1 < 610, d 
						sum a866_1 if acountry == 28 & a866_1 >= 610 & a866_1 < 692, d 
						sum a866_1 if acountry == 28 & a866_1 >= 692 & a866_1 < 814, d 
						sum a866_1 if acountry == 28 & a866_1 >= 814 & a866_1 < 1017, d 
						sum a866_1 if acountry == 28 & a866_1 >= 1017 & a866_1 < 1221, d 
						sum a866_1 if acountry == 28 & a866_1 >= 1221, d

						 
				* 2) replace income values with income range values if specific income is missing
						forvalues i = 1(1)13 {
						replace net_income_`i' =  118.57 if net_income_`i' == . & a867_`i' == 2801 & acountry == 28
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 240.48 if net_income_`i' == . & a867_`i' == 2802 & acountry == 28
						}							
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 320.64 if net_income_`i' == . & a867_`i' == 2803 & acountry == 28
						}							 

						forvalues i = 1(1)13 {
						replace net_income_`i' = 400.8 if net_income_`i' == . & a867_`i' == 2804 & acountry == 28
						}	
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 534.4 if net_income_`i' == . & a867_`i' == 2805 & acountry == 28
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 668 if net_income_`i' == . & a867_`i' == 2806 & acountry == 28
						}		
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 734.8 if net_income_`i' == . & a867_`i' == 2807 & acountry == 28
						}		
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 868.4 if net_income_`i' == . & a867_`i' == 2808 & acountry == 28
						}	
									
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1169 if net_income_`i' == . & a867_`i' == 2809 & acountry == 28
						}										
						
						forvalues i = 1(1)13 {
						replace net_income_`i' = 1536.4 if net_income_`i' == . & a867_`i' == 2810 & acountry == 28
						}										


****************************************************************************************************************************
				* 3) do the same (use the same median income done in part 1) for partner's income 
					* country-specific medians for range variable: a939_1-a939_13 
					* replace with range-values by medians of these ranges (using the medians from persons not partners) (except for Norway and Poland)

					* create a value for net income which first gets values from specific incomes
					forvalues i = 1(1)13 {
					gen par_net_income_`i' = a938_`i'
					}
					
					* replace that as missing if it gets value .a
					forvalues i = 1(1)13 {
					replace par_net_income_`i' = . if par_net_income_`i' == .a
					}
			
****************************************************************************************************************************
					* Bulgaria 11
	
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 18 if par_net_income_`i' == . & a939_`i' == 1101 & acountry == 11
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 42 if par_net_income_`i' == . & a939_`i' == 1102 & acountry == 11
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 72 if par_net_income_`i' == . & a939_`i' == 1103 & acountry == 11
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 128 if par_net_income_`i' == . & a939_`i' == 1104 & acountry == 11
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 179 if par_net_income_`i' == . & a939_`i' == 1105 & acountry == 11
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 230 if par_net_income_`i' == . & a939_`i' == 1106 & acountry == 11
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 307 if par_net_income_`i' == . & a939_`i' == 1107 & acountry == 11
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 409 if par_net_income_`i' == . & a939_`i' == 1108 & acountry == 11
						}		
										
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 767 if par_net_income_`i' == . & a939_`i' == 1109 & acountry == 11
						}		
										
					* Russia 12
						
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 4 if par_net_income_`i' == . & a939_`i' == 1201 & acountry == 12
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 20 if par_net_income_`i' == . & a939_`i' == 1202 & acountry == 12
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 34 if par_net_income_`i' == . & a939_`i' == 1203 & acountry == 12
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 48 if par_net_income_`i' == . & a939_`i' == 1204 & acountry == 12
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 60 if par_net_income_`i' == . & a939_`i' == 1205 & acountry == 12
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 71 if par_net_income_`i' == . & a939_`i' == 1206 & acountry == 12
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 100 if par_net_income_`i' == . & a939_`i' == 1207 & acountry == 12
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 156 if par_net_income_`i' == . & a939_`i' == 1208 & acountry == 12
						}		
										
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 228 if par_net_income_`i' == . & a939_`i' == 1209 & acountry == 12
						}		
												
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 284 if par_net_income_`i' == . & a939_`i' == 1210 & acountry == 12
						}		
											
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 427 if par_net_income_`i' == . & a939_`i' == 1211 & acountry == 12
						}		
											
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 683 if par_net_income_`i' == . & a939_`i' == 1212 & acountry == 12
						}		
					
					
					* Georgia 13
						 
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' =  12 if par_net_income_`i' == . & a939_`i' == 1301 & acountry == 13
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 28 if par_net_income_`i' == . & a939_`i' == 1302 & acountry == 13
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 46 if par_net_income_`i' == . & a939_`i' == 1303 & acountry == 13
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 66 if par_net_income_`i' == . & a939_`i' == 1304 & acountry == 13
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 105 if par_net_income_`i' == . & a939_`i' == 1305 & acountry == 13
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 153 if par_net_income_`i' == . & a939_`i' == 1306 & acountry == 13
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 175 if par_net_income_`i' == . & a939_`i' == 1307 & acountry == 13
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 234.5 if par_net_income_`i' == . & a939_`i' == 1308 & acountry == 13
						}		
										
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 526 if par_net_income_`i' == . & a939_`i' == 1309 & acountry == 13
						}		
	
					
					* France 15
						 
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 305  if par_net_income_`i' == . & a939_`i' == 1 & acountry == 15
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 750 if par_net_income_`i' == . & a939_`i' == 2 & acountry == 15
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1200 if par_net_income_`i' == . & a939_`i' == 3 & acountry == 15
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1600 if par_net_income_`i' == . & a939_`i' == 4 & acountry == 15
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2100 if par_net_income_`i' == . & a939_`i' == 5 & acountry == 15
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2600 if par_net_income_`i' == . & a939_`i' == 6 & acountry == 15
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 3500 if par_net_income_`i' == . & a939_`i' == 7 & acountry == 15
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 6500 if par_net_income_`i' == . & a939_`i' == 8 & acountry == 15
						}		

					* Netherlands 18
						
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' =  250 if par_net_income_`i' == . & a939_`i' == 1801 & acountry == 18
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 628 if par_net_income_`i' == . & a939_`i' == 1802 & acountry == 18
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 809 if par_net_income_`i' == . & a939_`i' == 1803 & acountry == 18
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1000 if par_net_income_`i' == . & a939_`i' == 1804 & acountry == 18
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1250 if par_net_income_`i' == . & a939_`i' == 1805 & acountry == 18
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1450 if par_net_income_`i' == . & a939_`i' == 1806 & acountry == 18
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1600 if par_net_income_`i' == . & a939_`i' == 1807 & acountry == 18
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1800 if par_net_income_`i' == . & a939_`i' == 1808 & acountry == 18
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2000 if par_net_income_`i' == . & a939_`i' == 1809 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2200 if par_net_income_`i' == . & a939_`i' == 1810 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2500 if par_net_income_`i' == . & a939_`i' == 1811 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2600 if par_net_income_`i' == . & a939_`i' == 1812 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2800 if par_net_income_`i' == . & a939_`i' == 1813 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 3000 if par_net_income_`i' == . & a939_`i' == 1814 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 3200 if par_net_income_`i' == . & a939_`i' == 1815 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 3500 if par_net_income_`i' == . & a939_`i' == 1816 & acountry == 18
						}	
					
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 4500 if par_net_income_`i' == . & a939_`i' == 1817 & acountry == 18
						}	
					
					* Romania 19
						 
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' =  44 if par_net_income_`i' == . & a939_`i' == 1901 & acountry == 19
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 101 if par_net_income_`i' == . & a939_`i' == 1902 & acountry == 19
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 186 if par_net_income_`i' == . & a939_`i' == 1903 & acountry == 19
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 401.5 if par_net_income_`i' == . & a939_`i' == 1904 & acountry == 19
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 573 if par_net_income_`i' == . & a939_`i' == 1905 & acountry == 19
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 819 if par_net_income_`i' == . & a939_`i' == 1906 & acountry == 19
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 992 if par_net_income_`i' == . & a939_`i' == 1907 & acountry == 19
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1147 if par_net_income_`i' == . & a939_`i' == 1908 & acountry == 19
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1354 if par_net_income_`i' == . & a939_`i' == 1909 & acountry == 19
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2260.5 if par_net_income_`i' == . & a939_`i' == 1910 & acountry == 19
						}
						
					* Belgium 23
						 
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' =  300 if par_net_income_`i' == . & a939_`i' == 1 & acountry == 23
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 823.5 if par_net_income_`i' == . & a939_`i' == 2 & acountry == 23
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1200 if par_net_income_`i' == . & a939_`i' == 3 & acountry == 23
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1600 if par_net_income_`i' == . & a939_`i' == 4 & acountry == 23
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2100 if par_net_income_`i' == . & a939_`i' == 5 & acountry == 23
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2500 if par_net_income_`i' == . & a939_`i' == 6 & acountry == 23
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 3300 if par_net_income_`i' == . & a939_`i' == 7 & acountry == 23
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 6375 if par_net_income_`i' == . & a939_`i' ==  8& acountry == 23
						}	
						
						
						
					* Lithuania 25
					
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 35  if par_net_income_`i' == . & a939_`i' == 2501 & acountry == 25
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 93 if par_net_income_`i' == . & a939_`i' == 2502 & acountry == 25
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 139 if par_net_income_`i' == . & a939_`i' == 2503 & acountry == 25
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 188 if par_net_income_`i' == . & a939_`i' == 2504 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 231 if par_net_income_`i' == . & a939_`i' == 2505 & acountry == 25
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 289 if par_net_income_`i' == . & a939_`i' == 2506 & acountry == 25
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 347 if par_net_income_`i' == . & a939_`i' == 2507 & acountry == 25
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 434 if par_net_income_`i' == . & a939_`i' == 2508 & acountry == 25
						}
															
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 463 if par_net_income_`i' == . & a939_`i' == 2509 & acountry == 25
						}	
															
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 521 if par_net_income_`i' == . & a939_`i' == 2510 & acountry == 25
						}	
														
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 579 if par_net_income_`i' == . & a939_`i' == 2511 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 781 if par_net_income_`i' == . & a939_`i' == 2512 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 868 if par_net_income_`i' == . & a939_`i' == 2513 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1157 if par_net_income_`i' == . & a939_`i' == 2514 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1302 if par_net_income_`i' == . & a939_`i' == 2515 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1447 if par_net_income_`i' == . & a939_`i' == 2516 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1736 if par_net_income_`i' == . & a939_`i' == 2517 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 2604 if par_net_income_`i' == . & a939_`i' == 2518 & acountry == 25
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 3732 if par_net_income_`i' == . & a939_`i' == 2519 & acountry == 25
						}	
						
					* Czech Republic 28
						/* 
						Using convertion values from Forbes 10.1.2024
						
						4900 Kc or less  -> - 199€
                        5000 - 6999 Kc   -> 200€ -  284€
                        7000 - 8999 Kc   -> 285€ - 366€
                        9000 - 11999 Kc  -> 367€ - 488€
                        12000 - 14999 Kc -> 489€ - 610€
                        15000 - 16999 Kc -> 611€ - 692€
                        17000 - 19999 Kc -> 693€ - 814€
                        20000 - 24999 Kc -> 815€ - 1017€
                        25000 - 29999 Kc -> 1018€ - 1221€
                        30000+ Kc        -> 1222€ -
						*/
					
						
						* if net income is missing (1-13 income types), replace it with the median from that income range
						forvalues i = 1(1)13 {
						replace par_net_income_`i' =  118.57 if par_net_income_`i' == . & a939_`i' == 2801 & acountry == 28
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 240.48 if par_net_income_`i' == . & a939_`i' == 2802 & acountry == 28
						}							
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 320.64 if par_net_income_`i' == . & a939_`i' == 2803 & acountry == 28
						}							 

						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 400.8 if par_net_income_`i' == . & a939_`i' == 2804 & acountry == 28
						}	
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 534.4 if par_net_income_`i' == . & a939_`i' == 2805 & acountry == 28
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 668 if par_net_income_`i' == . & a939_`i' == 2806 & acountry == 28
						}		
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 734.8 if par_net_income_`i' == . & a939_`i' == 2807 & acountry == 28
						}		
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 868.4 if par_net_income_`i' == . & a939_`i' == 2808 & acountry == 28
						}	
									
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1169 if par_net_income_`i' == . & a939_`i' == 2809 & acountry == 28
						}										
						
						forvalues i = 1(1)13 {
						replace par_net_income_`i' = 1536.4 if par_net_income_`i' == . & a939_`i' == 2810 & acountry == 28
						}										
	
****************************************************************************************************************************
			* 4) multiply income amount by frequency in a year
				
				* own income
				forvalues i = 1(1)13 {
				gen sumnet_income_`i' = a865_`i' * net_income_`i'
				}
	
	
				* partner's income
				forvalues i = 1(1)13 {
				gen sumpar_net_income_`i' = a937_`i' * par_net_income_`i'
				}
				
****************************************************************************************************************************
								
			* 5) sum all income types (own and partner's) together and then altogether own and partner's income together
				
				* sum income together (own and partner's)
				* NOTE! after this missing values get value zero!
					egen own_income = rowtotal(sumnet_income_1 sumnet_income_2 sumnet_income_3 sumnet_income_4 sumnet_income_5 sumnet_income_6 sumnet_income_7 sumnet_income_8 sumnet_income_9 sumnet_income_10 sumnet_income_11 sumnet_income_12 sumnet_income_13)
					
					egen partner_income = rowtotal(sumpar_net_income_1 sumpar_net_income_2 sumpar_net_income_3 sumpar_net_income_4 sumpar_net_income_5 sumpar_net_income_6 sumpar_net_income_7 sumpar_net_income_8 sumpar_net_income_9 sumpar_net_income_10 sumpar_net_income_11 sumpar_net_income_12 sumpar_net_income_13)

				* and then sum own income and partner's income together = household annual net income summed
						
					gen hh_income = own_income + partner_income
****************************************************************************************************************************
				
			* + remember exceptions Poland and Norway

				* Norway (exception: from registers, no range variables, annual gross income)
					egen own_incomeNOR = rowtotal(a866_1_2000 a866_2_2000 a866_3_2000 a866_4_2000 a866_5_2000)
					egen partner_incomeNOR = rowtotal(a938_1_2000 a938_2_2000 a938_3_2000 a938_4_2000)
					gen hh_incomeNOR = own_incomeNOR + own_incomeNOR
					
					replace hh_income = hh_incomeNOR if acountry == 20

****************************************************************************************************************************
			* + remember exceptions Poland and Norway
		
				* Poland (exception: annual income summed)
					* first replace with income range if income is only reported with band (only for respondents)
					* Poland 26
					* Poland has for some reason two types of classifications for these income ranges -> combination of these two (more based on the EUR one with more observations)
						ta a867_2600
						ta a867_2600, nolabel
												
						* take the median for all the country-specific income range groups
						sum a866_2600 if a866_2600 < 500, d // added directly to below
						sum a866_2600 if a866_2600 >= 500 & a866_2600 < 1000, d 
						sum a866_2600 if a866_2600 >= 1000 & a866_2600 < 1500, d
						sum a866_2600 if a866_2600 >= 1500 & a866_2600 <  2000, d
						sum a866_2600 if a866_2600 >= 2000 & a866_2600 < 2500, d
						sum a866_2600 if a866_2600 >= 2500 & a866_2600 <  3000, d
						sum a866_2600 if a866_2600 >= 3000 & a866_2600 <  5000, d
						sum a866_2600 if a866_2600 >= 5000, d
						 
						* if net income is missing (no need to do with a loop for 1-13 income types as Poland reports summed income), replace it with the median from that income range
						* first make sure that all missings are reported as missings
						replace a866_2600 = . if a866_2600 == .a
						
						replace a866_2600 = 251 if a866_2600 == . & (a867_2600 == 1 | a867_2600 == 9 | a867_2600 == 10) & acountry == 26
						replace a866_2600 = 629 if a866_2600 == . & (a867_2600 == 2 | a867_2600 == 11) & acountry == 26
						replace a866_2600 = 1108 if a866_2600 == . & (a867_2600 == 3 | a867_2600 == 12) & acountry == 26
						replace a866_2600 = 1637 if a866_2600 == . & (a867_2600 == 4 | a867_2600 == 13) & acountry == 26
						replace a866_2600 = 2015 if a866_2600 == . & a867_2600 == 5  & acountry == 26
						replace a866_2600 = 2519 if a866_2600 == . & a867_2600 == 6  & acountry == 26
						replace a866_2600 = 3463 if a866_2600 == . & a867_2600 == 7  & acountry == 26
						replace a866_2600 = 5038 if a866_2600 == . & a867_2600 == 8 & acountry == 26
						
					
					egen own_incomePOL = rowtotal(a866_2600)
					egen partner_incomePOL = rowtotal(a938_2600)					

					gen hh_incomePOL = own_incomePOL + partner_incomePOL
				
					replace hh_income = hh_incomePOL if acountry == 26
					
****************************************************************************************************************************
					
			* Now put household income to missing if it gets value 0
				* this is done because we created the 0 values by ourselves with rowtotal
				replace hh_income = . if hh_income == 0

****************************************************************************************************************************
				
			* 6) equivalence scaling
				gen hh_e = hh_income/sqrt(ahhsize)
				
			* 7) country-specific percentile from equivalenced household annual net income (for Norway: gross from registers)
				sort acountry hh_e
				bysort acountry: egen rank = rank(hh_e)
				bysort acountry: egen count = count(hh_e)
				gen hh_incomerank = 100 * (rank / count)

****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************		

* Save the final dataset to be used
		save GGS_final.dta, replace
		
* Full sample: 126,417 individuals nested in 11 countries, income missing for around 14 000 individuals
	* Sample statistics: adult population (Table 1)
		ta acountry


****************************************************************************************************************************	
****************************************************************************************************************************
****************************************************************************************************************************
