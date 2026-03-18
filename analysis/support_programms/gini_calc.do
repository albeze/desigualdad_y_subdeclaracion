capture program drop gini 
program define gini, rclass

syntax varlist(max=1) [if] [iweight]

quietly {
preserve
* touse = 1 -> observacion si cumple if & !=.
* touse = 0 -> observacion no cumple if | ==.
	marksample touse 
	keep if `touse' == 1

	local wt : word 2 of `exp'
		if "`wt'"=="" {
	local wt = 1
}
summ `varlist' [`weight'`exp']
* poblacion de referencia 
local obs=r(sum _w)
* media ingreso 
local media=r(mean)

sort `varlist'

tempvar each i aux

gen `aux' = sum(`wt')
gen `i' = (2*`aux'-`wt'+1) /2
gen `each' = `varlist'*(`obs'-`i'+1)
summ `each' [`weight'`exp']
local gini = 1 + (1/`obs') - (2/(`media'*`obs'^2)) * r(sum)

return scalar gini = `gini'
restore

}


display as text "Gini `varlist' = " as result `gini'
end
