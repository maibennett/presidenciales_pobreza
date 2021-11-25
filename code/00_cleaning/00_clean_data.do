*** Analisis de Datos 1era Vuelta Presidencial 2021 - Chile ***
*** Autora: Magdalena Bennett ***

global main_dir "C:\Users\mc72574\Dropbox\UT\UT Research\Presidenciales\data\"

* Load data (downloaded directly from: https://oficial.servel.cl/resultados-preliminares-elecciones-presidenciales-parlamentarias-y-cores-2021/
import delimited "${main_dir}Servel_20211121_PRESIDENCIALES\Servel_20211121_PRESIDENCIALES_CHILE.csv", delimiter(";") clear

* Transform votes to numeric
gen votos_preliminar = regexs(2) if regexm(votos_preliminar_string, "^([^0-9]*)([0-9]+)([^0-9]*)$")

destring votos_preliminar, replace

replace candidato = strtrim(candidato)
replace region_nombre = strtrim(region_nombre)
replace comuna_nombre = strtrim(comuna_nombre)

* Create added variables by county

collapse (sum) votos_preliminar, by(region_id region_nombre comuna_id comuna_nombre candidato)

bysort region_id region_nombre comuna_id comuna_nombre: egen total_votos = sum(votos_preliminar)

gen p_votos = votos_preliminar/total_votos

export delimited using "${main_dir}working\servel_county_clean.csv", replace


**** CASEN *****

* Load data (downloaded directly from: http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2017)
use "${main_dir}casen_2017_stata\Casen 2017.dta", clear

gen pobreza_d = pobreza<=2
replace pobreza_d = . if pobreza==.

gen pobreza_ext_d = pobreza<=1
replace pobreza_ext_d = . if pobreza==.


collapse (mean) pobreza_d pobreza_ext_d pobreza_multi_4d pobreza_multi_5d [fw = expc], by(comuna)

export delimited using "${main_dir}working\casen_county_clean.csv", nolabel replace
