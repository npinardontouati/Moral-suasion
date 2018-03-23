global path "/Users/npinardontouati/Documents/Data_boards_mutualistes/03_Temp"


***************************************************************************************************
********************************* WIDE FORMAT *****************************************************
***************************************************************************************************


import delimited using "$path/wide_CA.csv", varnames(1) delimiters(";") clear

order v1 v2

keep v1 v2
duplicates drop
rename v2 name

replace name = regexr(name,"François Xavier","François-Xavier") if regexm(name,"François Xavier")

split name,parse(" ")
gen i=_n

rename name name_original
reshape long name, i(i name_original v1) j(nb)
drop if name==""

gen flag=1 if name=="de"|name=="des"|name=="le"|name=="les"
replace name= name[_n-1]+" " + name if flag[_n-1]==1
replace name= "GOUZE de SAINT-MARTIN" if name=="GOUZE"
drop if name=="de SAINT-MARTIN"
replace name= "de ST EXUPERY" if name=="de ST"
drop if name=="EXUPERY"
replace name= "LE MORVAN" if name=="MORVAN"  
drop if name=="LE" & i==35
replace name= "LE BOT" if name=="BOT"  
drop if name=="LE" & (i==40|i==41)

replace name= "LE DREN" if name=="DREN"  
drop if name=="LE" & (i==40|i==41)
replace name= "LE BARS" if name=="BARS"  
drop if name=="LE" & i==41
drop if flag==1

sort i nb
bysort i: gen id=_n

gen surname= name[_n-1] if (int(id/2)*2)==id

gen number_cons_missings=1 if surname=="" & surname[_n-1]=="" // OK

drop if surname==""

keep v1 name surname

save "$path/wide_CA.dta", replace


***************************************************************************************************
********************************* LONG FORMAT *****************************************************
***************************************************************************************************


use "$path/long_CA.dta", clear

duplicates drop

foreach var of varlist v1-v3 {
replace `var'="" if `var'=="EMPTY"
replace `var'="" if `var'=="NA"
}

drop if v3 !="" & v2=="" & v1==""
drop if regexm(v1,"Admin") & v2=="" & v3==""


gen id=_n
replace v2=v3 if i==51
replace v3="" if i==51
replace v3=v1 if i==3
replace v1=v2 if i==3
replace v2=v3 if i==3
replace v3="" if i==3

replace v1=v1[_n-1] if v1=="" & v2!="" & regexm(v2,"[0-9]")==0 & v3=="" & i==i[_n-1]

gen clean=0
replace clean=1 if regexm(v1,"Admin|ADMIN|PRES|Pres|pres|compleme|TRESO|Secret|SECRET|PReS|membres |admin |Vice |Membres|Direc |DIRECTEUR|autre") & v2!="" & regexm(v2,"[0-9]")==0 & v3==""

sort clean id

replace v2=v1 if i==53
replace v1="" if i==53
replace clean=1 if i==53



drop if v3=="" & i==38
replace v1="" if i==38
replace v2=v2+" "+v3 if i==38
replace v3="" if i==38

drop if regexm(v2,"[0-9]") & v1=="" & v3==""
drop if id==2

replace v3="" if regexm(v3,"[0-9]")
replace v2="" if regexm(v2,"[0-9]")

replace v1 = v1+" "+v1[_n+1] if v2!="" & v2[_n+1]=="" & (i==59|i==60)
drop if v2=="" & v2[_n-1]!="" & (i==59|i==60)
replace v3=v2 if (i==59|i==60)
replace v2=v1 if (i==59|i==60)
replace v1=v3 if (i==59|i==60)
replace v3="" if (i==59|i==60)

replace v1=subinstr(v1,"Les Vice-Presidents","",.)
replace v1=subinstr(v1,"Les Administrateurs","",.)
replace v1=subinstr(v1,"Le President","",.)
replace v1=subinstr(v1,"LA PRESIDENTE","",.)

replace v2=v1 if v2==""
replace v1="" if v1==v2
drop v3

rename v2 name


split name if ((regexm(name,"LES VICE") & i==49)), parse(" ")
drop name1-name3
drop name5 name7 name10 name11
replace name4= name4 + " " +name6 
replace name8= name8 + " " +name9
replace name12= name12 + " " +name13
drop name6 name9 name13
replace name4=trim(name4)
replace name8=trim(name8)
replace name12=trim(name12)
replace name4=name if name8==""
rename name name_o
reshape long name,i(id) j(nb)
drop if name==""
drop name_o


replace name=name +" "+name[_n+1] if id==9
replace name=name +" "+name[_n+1] if id==11
replace name=name +" "+name[_n+1] if id==13
drop if id==14|id==10|id==12
replace name=subinstr(name," -","-",.)

split name if (id>=9 & id<=13 ), parse(" ")
drop name3 name6 name9 name12 name15 name18
replace name1=name1+ " " +name2
replace name4=name4+ " " +name5
replace name7=name7+ " " +name8
replace name10=name10+ " " +name11
replace name13=name13+ " " +name14
replace name16=name16+ " " +name17
replace name19=name19+ " " +name20
drop name2 name5 name8 name11 name14 name17 name20
foreach i in "1" "4" "7" "10" "13" "16" "19" {
replace name`i'=trim(name`i')
}
replace name1=name if name4==""

rename name name_o
replace id=_n
drop nb
reshape long name,i(id) j(nb)
drop if name==""
drop name_o
drop id nb
drop clean i

replace name=subinstr(name,"M. ","",.)
replace name=subinstr(name,"M ","",.)
replace name=subinstr(name,"  "," ",.)
replace name=subinstr(name,"Marie Pierre","Marie-Pierre",.)
replace name=subinstr(name,"Jean Claude","Jean-Claude",.)

replace name=subinstr(name,"SAINT MARTIN","SAINT-MARTIN",.)
replace name=subinstr(name,"(","",.)
replace name=subinstr(name,")","",.)
replace name=subinstr(name,"31/12/2016 ","",.)
replace name=subinstr(name,"31/12/2013 ","",.)

keep name
duplicates drop

split name, parse(" ")

rename name name_o

gen flag=1 if regexm(name1,"[A-Z][A-Z]") | name1=="de" | name1=="De"
gen surname=name1
replace surname=name2 if flag==1
replace surname=name3 if flag==1 & name3!=""

gen name=name2
replace name= name2+ " " +name3+ " "+ name4+ " " +name5+ " "+ name6 if name6!=""
replace name= name2+ " " +name3+ " "+ name4 if name4!="" & name6==""
replace name= name2+ " " +name3 if name3!="" & name4=="" & flag!=1
replace name= name1+ " " +name2 if name3!="" & name4=="" & flag==1
replace name= name2+ " " +name1 if name3!="" & name4=="" & flag==1 & name2=="de"

keep name surname

append using "$path/wide_CA.dta"
drop v1
duplicates drop

rename name nom
rename surname prenom

replace nom=upper(nom)
replace nom=regexr(nom,"- ","-")
replace nom=regexr(nom," -","-")
replace nom=regexr(nom," - ","-")
replace nom= subinstr(nom, "É", "E", .) 
replace nom= subinstr(nom, "È", "E", .) 
replace nom= subinstr(nom, "Ç", "C", .) 
replace nom= subinstr(nom, "Ï", "I", .) 

gen nom_new=nom
replace nom_new = subinstr(nom_new, "-", "", .) 
replace nom_new = subinstr(nom_new, "'", "", .) 
replace nom_new = subinstr(nom_new, " ", "", .) 
replace nom_new = subinstr(nom_new, "*", "", .) 

gen prenom_new=prenom
replace prenom_new=lower(prenom_new)
replace prenom_new = subinstr(prenom_new, "É", "e", .) 
replace prenom_new = subinstr(prenom_new, "Ï", "i", .) 
replace prenom_new = subinstr(prenom_new, "é", "e", .) 
replace prenom_new = subinstr(prenom_new, "è", "e", .) 
replace prenom_new = subinstr(prenom_new, "ë", "e", .) 
replace prenom_new = subinstr(prenom_new, "ç", "c", .) 
replace prenom_new = subinstr(prenom_new, "ô", "o", .) 
replace prenom_new = subinstr(prenom_new, "ï", "i", .) 
replace prenom_new = subinstr(prenom_new, "î", "i", .) 
replace prenom_new = subinstr(prenom_new, "j.", "jean-", .)
replace prenom_new = subinstr(prenom_new, "jean.", "jean-", .)  
replace prenom_new = subinstr(prenom_new, "j-", "jean-", .) 
replace prenom_new = subinstr(prenom_new, "p.", "pierre-", .) 
replace prenom_new = subinstr(prenom_new, "p-", "pierre-", .) 
replace prenom_new = subinstr(prenom_new, "m.", "marie-", .) 
replace prenom_new = subinstr(prenom_new, "c.", "claude-", .) 
replace prenom_new = subinstr(prenom_new, "h.", "henri-", .) 
replace prenom_new = subinstr(prenom_new, "f.", "francois-", .) 
replace prenom_new = subinstr(prenom_new, "a.", "anne-", .) 
replace prenom_new = subinstr(prenom_new, "l.", "louis-", .) 
replace prenom_new = subinstr(prenom_new, "y.", "yves-", .) 
replace prenom_new = subinstr(prenom_new, "-", "", .) 
replace prenom_new = subinstr(prenom_new, " ", "", .)

gen idmaster=_n

reclink nom_new prenom_new using "$path/namesMN.dta", idmaster(idmaster) idusing(idusing) gen(newvar) wmatch(20 1)
gsort -newvar

keep nom_new prenom_new newvar
duplicates drop


replace newvar=0 if newvar<1
replace newvar=0 if nom=="LEFEBVRE" & prenom=="dominique" // https://fr.wikipedia.org/wiki/Dominique_Lefebvre_(banquier)
replace newvar=0 if nom=="GIRAUD" & prenom=="claude" // CA sud rhone alpes vs maire dans la loire. https://www.le-pays.fr/montrond-les-bains/politique/2014/04/03/claude-giraud-entame-son-5e-mandat_1946354.html
replace newvar=0 if nom=="LEROY" & prenom=="philippe" // CA val de france vs maire Normandie. 
replace newvar=0 if nom=="ANDRE" & prenom=="pierre" // CA languedoc vs maire Aisne. 



rename newvar match_mayor

gen idmaster=_n
reclink nom_new prenom_new using "$path/namesLG.dta", idmaster(idmaster) idusing(idusing) gen(newvar) wmatch(20 1)
gsort -newvar
replace newvar=0 if newvar<1
replace newvar=0 if nom=="DAVID" & prenom=="dominique" // https://www.francebleu.fr/infos/politique/legislatives-qui-est-dominique-david-la-nouvelle-deputee-rem-de-bordeaux-1-1497541971
replace newvar=0 if nom=="LEFEBVRE" & prenom=="dominique" // https://fr.wikipedia.org/wiki/Dominique_Lefebvre_(banquier)
rename newvar match_MP

gsort -match_mayor

keep nom_new prenom_new match*
replace match_m=0 if match_m==.
replace match_M=0 if match_M==.
save "$path/all_CA.dta", replace


