set more off
clear all

capture cd "/Users/alfonsoberriosz/Documents/Stata/Monografia"

use "EPH_T3_2024", clear

sort codusu nro_hogar trimestre 
egen id = group(codusu nro_hogar trimestre) 

drop if ipcf == . | ipcf < 0
* Generar percentiles de ingreso
sort ipcf, stable

egen poblacion_total 	= total(pondih)
gen poblacion_acumulada = sum(pondih)
gen share_poblacion 	= poblacion_acumulada / poblacion_total
gen p_ipcf 				= 0
gen d_ipcf				= 0
gen double pp_ipcf  = poblacion_acumulada[_N]/100
gen double pd_ipcf  = poblacion_acumulada[_N]/10

gen ingreso_acumulado   = sum(pondih * ipcf)
gen ingreso_pond        = ipcf * pondih
egen ingreso_total      = total(pondih * ipcf)
gen shrinc              = ingreso_acumulado / ingreso_total

*for-loop para crear percentiles de ingreso
/*
forvalues i=1(1)100 {
 replace p_ipcf = `i' if share_poblacion > (`i'-1)/100 & ///
	share_poblacion < `i'/100
}
*/
forvalues i = 0(1)99    {
	replace p_ipcf=`i'+1 if (poblacion_acumulada>pp_ipcf*`i' & poblacion_acumulada<=pp_ipcf*(`i'+1))
	}

//version 16: table p_ipcf, c(max shrinc)
	
*for-loop para crear deciles de ingreso
/*
forvalues i=1(1)10{
	replace d_ipcf = `i' if share_poblacion > (`i'-1)/10 & share_poblacion <= `i'/10
}
*/

forvalues i = 0(1)9    {
	replace d_ipcf=`i'+1 if (poblacion_acumulada>pd_ipcf*`i' & poblacion_acumulada<=pd_ipcf*(`i'+1))
}

/*table p_ipcf [w=pondih], stat(mean ipcf)
table d_ipcf [w=pondih], stat(mean ipcf)

* Ingreso acumulado para generar curva de Lorenz
table p_ipcf [w=pondih], stat(max shrinc)
table d_ipcf [w=pondih], stat(max shrinc)
*/

* Exportar resultados a Excel

preserve 
	/*version 16: */
	table p_ipcf [w=pondih], c(mean ipcf) replace 
	export excel using "percentiles_ajus.xlsx", sheetmodify sheet("T3") cell(E5)
restore 	

preserve 
	/*version 16:*/ 
	table p_ipcf, c(max shrinc) replace 
	export excel using "percentiles_ajus.xlsx", sheetmodify sheet("T3") cell(K5)
restore 	

preserve 
	/*version 16:*/ 
	table d_ipcf [w=pondih], c(mean ipcf) replace 
	export excel using "percentiles_ajus.xlsx", sheetmodify sheet("T3") cell(A5)
restore 	

* Programa para calcular Gini en Gasparini (2012)
run "ginilibro.do"
display "El gini del T3 2024 en Argentina es:"
gini ipcf [w=pondih] 

* Programa para calcular Theil en Gasparini (2012)
run "theil_libro.do"
theil ipcf [w=pondih] if ipcf>0
display "El theil del T3 2024 en Argentina es:" r(theil)

foreach num of numlist 01 40 41 42 43 44{
	display "El gini del T3 2024 en la región `num' es:" 
	gini ipcf [w=pondih] if region ==`num' & ipcf>0
}

foreach numb of numlist 02 32 25{
	display "El gini del T3 2024 en la región `numb' es:" 
	gini ipcf [w=pondih] if aglomerado ==`numb' & ipcf>0
}


--
******************************************************************
******* Acumulación de ingresos en los cuantiles más ricos *******
******************************************************************

/*
* Elegí el valor de x (porcentaje más rico que querés analizar)
local x = 0.01  // ejemplo: 0.1 para top 0.1%, 1 para top 1%, 5 para top 5%

* Crear variable que marca el top x% más rico (de ingreso per cápita)
gen top_x = share_poblacion >= (1 - `x'/100)

* Calcular ingreso ponderado por hogar (ya existe)
* gen ingreso_pond = ipcf * pondih   // ya creada arriba

* Calcular ingreso total acumulado del top x%
gen ingreso_top_x = ingreso_pond * top_x

* Sumar ingreso del top x%
egen ingreso_top_total = total(ingreso_top_x)

* Calcular porcentaje del ingreso total que acumula ese grupo
gen pct_ingreso_top_x = ingreso_top_total / ingreso_total

* Mostrar el resultado
display "El `x'% más rico acumula el " pct_ingreso_top_x[1]*100 "% del ingreso total."

*OPCION 2*
* Top 5%
sum ingreso_pond if share_pob >= 0.95, meanonly
scalar top5 = (r(sum)/ingreso_total)*100

* Top 1%
sum ingreso_pond if share_pob >= 0.99, meanonly
scalar top1 = (r(sum)/ingreso_total)*100

* Top 0.1%
sum ingreso_pond if share_pob >= 0.999, meanonly
scalar top01 = (r(sum)/ingreso_total)*100

* Top 0.01%
sum ingreso_pond if share_pob >= 0.9999, meanonly
scalar top001 = (r(sum)/ingreso_total)*100

* 5. Presentar resultados
display _n "PARTICIPACIÓN EN EL INGRESO DE LOS MÁS RICOS - EPH 2024"
display "5% más rico: " %6.2f top5 "%"
display "1% más rico: " %6.2f top1 "%"
display "0.1% más rico: " %6.2f top01 "%"
display "0.01% más rico: " %6.2f top001 "%"
*/

* For-loop para iterar sobre el x% más rico 
* Creado de esta manera porque no se puede nombrar decimales (e.g. 0.1) como variable
local lista "0_01 0_1 0_5 1 5 10"
local valores "0.01 0.1 0.5 1 5 10"

local i = 1
foreach sufijo in `lista' {
	local x : word `i' of `valores'

	* Marcar el top x% más rico
	gen top_`sufijo' = share_poblacion >= (1 - `x'/100)

	* Ingreso acumulado del top x%
	gen ingreso_top_`sufijo' = ingreso_pond * top_`sufijo'
	egen ingreso_total_`sufijo' = total(ingreso_top_`sufijo')

	* Porcentaje del ingreso total
	gen pct_ingreso_`sufijo' = ingreso_total_`sufijo' / ingreso_total

	* Mostrar el resultado
	display "El top `x'% más rico acumula el " ///
		pct_ingreso_`sufijo'[1]*100 "% del ingreso total."

	local ++i
}

****************************************************************************
******* Ajuste de ingresos por subdeclaración en Albina et al.(2024) *******
****************************************************************************

* Importar el archivo xls con los coeficientes de ajuste
preserve
global ruta_coef "/Users/alfonsoberriosz/Documents/Stata/"
import excel "$ruta_coef/coef_ajuste_v2.xls", sheet("coef_p") cellrange(A5:F105) firstrow clear
rename Percentil p_ipcf
rename Coef coef_p
save "$ruta_coef/coef_ajuste", replace
restore

* Unir los datos con los coeficientes de ajuste
merge m:1 p_ipcf using "$ruta_coef/coef_ajuste.dta"
drop _merge

* Ajustar el ipcf por los coeficientes de ajuste
gen ipcf_ajustado_p = ipcf * coef_p // Ajuste por percentil
sort ipcf_ajustado_p, stable
drop if ipcf_ajustado_p == . | ipcf_ajustado_p < 0

* Definición de variables
egen tot_pop                = total(pondih)
gen cum_pop                 = sum(pondih)
gen share_pop 	            = cum_pop / tot_pop
gen percentile 				= 0
gen decile 					= 0
gen double pppercentile     = poblacion_acumulada[_N]/100
gen double ppdecile         = poblacion_acumulada[_N]/10

* Definición de variables para crear Curva de Lorenz
gen inc_acumulado           = sum(pondih * ipcf_ajustado_p)
gen inc_pond                = ipcf_ajustado_p * pondih
egen inc_total              = total(pondih * ipcf_ajustado_p)
gen shrincm                 = inc_acumulado / inc_total

*For-loop para crear percentiles de ingreso postajuste
forvalues i = 0(1)99 {
	replace percentile=`i'+1 if (cum_pop>pppercentile*`i' & cum_pop<=pppercentile*(`i'+1))
}
//table percentile, stat(max shrinc)

* Exporta a Excel
preserve 
	/*version 16: */
	table percentile [w=pondih], c(mean ipcf_ajustado_p) replace 
	export excel using "percentiles_ajus.xlsx", sheetmodify sheet("T3") cell(G5)
restore 	

preserve 
	/*version 16:*/
	table percentile, c(max shrincm) replace 
	export excel using "percentiles_ajus.xlsx", sheetmodify sheet("T3") cell(M5)
restore 	

* Programa para calcular Gini en Gasparini (2012)
display "El gini del T3 2024 en Argentina después del ajuste es:"
gini ipcf_ajustado_p [w=pondih]

* Programa para calcular Theil en Gasparini (2012)
theil ipcf_ajustado_p [w=pondih] if ipcf_ajustado_p>0
display "El theil ajustado del T3 2024 en Argentina es:" r(theil)



*******************************************************************************
******* Acumulación de ingresos en los cuantiles más ricos (POSTAJUSTE) *******
*******************************************************************************
/*
local y = 0.01   // ejemplo: 0.1 para top 0.1%, 1 para top 1%, 5 para top 5%

* Crear variable que marca el top x% más rico (de ingreso per cápita)
gen top_y = share_pop >= (1 - `y'/100)

* Calcular ingreso total acumulado del top x%
gen ingreso_top_y = inc_pond * top_y

* Sumar ingreso del top x%
egen inc_top_total = total(ingreso_top_y)

* Calcular porcentaje del ingreso total que acumula ese grupo
gen pct_ingreso_top_y = inc_top_total / inc_total

* Mostrar el resultado
display "El `y'% más rico acumula el " pct_ingreso_top_y[1]*100 "% del ingreso total."
*/

local lista_aj "0001 001 005 01 05 10"
local valores_aj "0.01 0.1 0.5 1 5 10"

local j = 1
foreach sufx in `lista' {
	local z : word `j' of `valores_aj'

	* Marcar el top z% más rico
	gen topp_`sufx' = share_poblacion >= (1 - `z'/100)

	* Ingreso acumulado del top z%
	gen ingreso_topp_`sufx' = inc_pond * topp_`sufx'
	egen inc_total_`sufx' = total(ingreso_topp_`sufx')

	* Porcentaje del ingreso total
	gen pct_inc_`sufx' = inc_total_`sufx' / inc_total

	* Mostrar el resultado
	display "El top `z'% más rico acumula el " pct_inc_`sufx'[1]*100 "% del ingreso total (POSTAJUSTE)."

	local ++j
}

*****************************************************************************************
******* Ajuste de ingresos (postajuste) por subdeclaración en Albina et al.(2024) *******
*****************************************************************************************

preserve
global ruta_coef "/Users/alfonsoberriosz/Documents/Stata/"
import excel "$ruta_coef/coef_ajuste_v2.xls", sheet("coef_d") cellrange(A5:F15) firstrow clear
rename Decil d_ipcf
rename Coef coef_d
save "$ruta_coef/coef_ajuste", replace
restore

* Unir los datos con los coeficientes de ajuste
merge m:1 d_ipcf using "$ruta_coef/coef_ajuste.dta"
drop _merge
* Ajustar el ipcf por los coeficientes de ajuste
gen ipcf_ajustado_d = ipcf * coef_d // Ajuste por percentil
sort ipcf_ajustado_d, stable

* Creación de deciles postajuste
forvalues i = 0(1)9    {
	replace decile=`i'+1 if (cum_pop>ppdecile*`i' & cum_pop<=ppdecile*(`i'+1))
}

* Exportar a Excel
preserve 
	/*version 16: */
	table decile [w=pondih], c(mean ipcf_ajustado_d) replace 
	export excel using "percentiles_ajus.xlsx", sheetmodify sheet("T3") cell(C5)
restore 	
