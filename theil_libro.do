capture program drop theil
program define theil, rclass

syntax varlist(max=1) [if] [iweight]

quietly {
	preserve
  
	marksample touse 
	keep if `touse' == 1

	local wt : word 2 of `exp'
	if "`wt'"=="" {
		local wt = 1
	}
	
tempvar each
* Calcular la media ponderada
	summ `varlist' [`weight'`exp']
	local media = r(mean)

	* Generar componente del Theil
	gen `each' = (`varlist'/`media') * ln(`varlist'/`media')

        * Calcular índice Theil
	summ `each' [`weight'`exp']
	local theil = r(sum) / r(sum_w)

* Devolver resultado
	return scalar theil = `theil'

	restore
}

end
