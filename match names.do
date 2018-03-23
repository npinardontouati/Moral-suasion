global path_election "/Users/npinardontouati/Dropbox/Election_data"
global temp "03_Temp"

use "$path_election/$temp/panel_MN.dta", clear

keep prenom nom
duplicates drop
drop if nom=="" 

gen idusing=_n
save "/Users/npinardontouati/Documents/Data_boards_mutualistes/03_Temp/namesMN.dta", replace


use "$path_election/$temp/panel_LG.dta", clear

keep prenom_n nom_n
duplicates drop
drop if nom=="" 

gen idusing=_n
save "/Users/npinardontouati/Documents/Data_boards_mutualistes/03_Temp/namesLG.dta", replace
