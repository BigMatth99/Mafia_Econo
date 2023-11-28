*The entire code is ours, we just use the same controls as in the initial study

*DIFFS IN DIFFS

*TABLES 1, 2, 3, 4

use "ADD_Mafia_municipality.dta", clear
*We cut Sicily vertically (West-East) using the longitude as a dummy variable, 1 are Western municipalities and 0 are Eastern municipalities. 
*to get the median
summarize longitude, detail
replace longitude = 1 if longitude<=14.43556
replace longitude = 0 if longitude>14.43556
tab longitude

*The treatment is the drought. The Western municipalities are the treatment group with the drought and the Eastern municipalities are the control group without  the drought. 
rename longitude treatment
reshape long Mafia, i(comune1853) j(year)
drop if year==1987

*to check if the groups we made represent really a "control" and a "treatment" group I check the mean average relative rainfall in Spring 1893 of the two groups and I can reajust the longitude if needed.
 sum sp3m1893_n30c if treatment == 1
  sum sp3m1893_n30c if treatment == 0
sum sp3m1893_n30 if treatment == 1
  sum sp3m1893_n30 if treatment == 0
  *Results: small standard deviations!! But control group has a mean of 0,82 which is 12% lower than usual and has a commune with a average relative rainfall of 6% which is terribly low. But, globally we assume that the two groups are well representative of a treatment and a control group.

gen time = (year>=1894) & !missing(year)
gen did = time*treatment
reg Mafia time treatment did, r
*To evaluate what the closest to the reality the impact of the drought on the treatment group we have to add controls to the did regression
*First I add geographic controls.
*I just verify sometimes the control variables, to see if their is a huge difference between the two groups.
 sum agricola_rel if treatment ==1
 sum agricola_rel if treatment ==0
reg Mafia time treatment did Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves agricola_rel, r

*If I add to it more geographic control variables.
reg Mafia time treatment did lnpop1861 lnsurface centreheight maxheight slope2 pa_pdist1856 port2_pdist1856 roads1799 ave_temp var_sp3m_n30 sp3m_ave_n30 Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves, r

*the graph on the DID with controls
collapse (mean) Mafia, by(time treatment)

*we change manually time==0 with time==1885 and time==1 and time==1900
*we change manually the dataset to add the time effect.
set obs 5
replace treatment = 2 in 5
replace time = 0 in 5
replace Mafia = 0.9212121 in 5
set obs 6
replace treatment = 2 in 6
replace time = 1 in 6
replace Mafia = 1.3707195 in 6

twoway (line Mafia time if treatment==1) (line Mafia time if treatment==0) (line Mafia time if treatment==2),legend(label(1 Treatment group) label(2 Control group) label(3 time effect))

*INSTRUMENTAL VARIABLES

*To see the correlation
corr peasants_fasci hhi1865 Citrus_groves seminatoritot_rel Vineyards Rural_rent Urban_rent infant1869 dev_spending1884 doctors_pop1885



*RESET THE DATA




clear

use "ADD_Mafia_municipality.dta", clear

*TABLE 5

*TAXATION

*1 instrument

*First stage
reg peasants_fasci ind_dir_ratio1884 if Mafia1900!=.
*Fasci controls
reg peasants_fasci ind_dir_ratio1884 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel seminatoritot_rel if Mafia1900!=.
*Mafia controls
reg peasants_fasci ind_dir_ratio1884 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel seminatoritot_rel Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves Mafia1885 if Mafia1900!=.
*Geographical controls
reg peasants_fasci ind_dir_ratio1884 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel seminatoritot_rel Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves Mafia1885 lnpop1861 lnsurface centreheight maxheight slope2 pa_pdist1856 port2_pdist1856 roads1799 ave_temp var_sp3m_n30 sp3m_ave_n30 if Mafia1900!=.

*2 instruments

*First stage
reg peasants_fasci ind_dir_ratio1884 sp3m1893_n30 if Mafia1900!=.
*Fasci controls
reg peasants_fasci ind_dir_ratio1884 sp3m1893_n30 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel seminatoritot_rel if Mafia1900!=.
*Mafia controls
reg peasants_fasci ind_dir_ratio1884 sp3m1893_n30 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel seminatoritot_rel Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves Mafia1885 if Mafia1900!=.
*Geographical controls
reg peasants_fasci ind_dir_ratio1884 sp3m1893_n30 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel seminatoritot_rel Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves Mafia1885 lnpop1861 lnsurface centreheight maxheight slope2 pa_pdist1856 port2_pdist1856 roads1799 ave_temp var_sp3m_n30 sp3m_ave_n30 if Mafia1900!=.

*WE GIVE UP TAXATION

*TABLE 6

*LET'S TRY SHARE GRAINS AS AN INSTRUMENTAL VARIABLE

*2 instruments

*First stage
reg peasants_fasci seminatoritot_rel sp3m1893_n30 if Mafia1900!=.
*Fasci controls
reg peasants_fasci seminatoritot_rel sp3m1893_n30 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel if Mafia1900!=.
*Mafia controls
reg peasants_fasci seminatoritot_rel sp3m1893_n30 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves Mafia1885 if Mafia1900!=.
*Geographical controls
reg peasants_fasci seminatoritot_rel sp3m1893_n30 predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves Mafia1885 lnpop1861 lnsurface centreheight maxheight slope2 pa_pdist1856 port2_pdist1856 roads1799 ave_temp var_sp3m_n30 sp3m_ave_n30 if Mafia1900!=.

*1 instrument 

*First stage
reg peasants_fasci seminatoritot_rel if Mafia1900!=.
predict peasants_fasci_grains0
*Fasci controls
reg peasants_fasci seminatoritot_rel predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel if Mafia1900!=.
predict peasants_fasci_grains1
*Mafia controls
reg peasants_fasci seminatoritot_rel predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves Mafia1885 if Mafia1900!=.
predict peasants_fasci_grains2
*Geographical controls
reg peasants_fasci seminatoritot_rel predr_peas_fasci ruralcentre1861 Rural_rent Urban_rent agricola_rel Citrus_groves sulfurproduction1868_70 Vineyards Olives_groves Mafia1885 lnpop1861 lnsurface centreheight maxheight slope2 pa_pdist1856 port2_pdist1856 roads1799 ave_temp var_sp3m_n30 sp3m_ave_n30 if Mafia1900!=.
predict peasants_fasci_grains3


*Probabilities Mafia/Fasci FIGURE 3

ologit Mafia1900 i.peasants_fasci
margins peasants_fasci, post
margins, at(peasants_fasci=(0/1)) predict(outcome(0/1)) atmeans



*SECOND STAGE WITH ORDERED LOGIT

*TABLE7

* Ordered Logit with Peasant Fasci binary

global ylist Mafia1900 
global xlist peasants_fasci sp3m1893_n30

describe $ylist $xlist
summarize $ylist $xlist

tabulate $ylist

ologit $ylist $xlist

mfx, predict (outcome(0))
mfx, predict (outcome(1))
mfx, predict (outcome(2))
mfx, predict (outcome(3))

*Ordered Logit with instrumental variable without control variables

global ylist Mafia1900 
global xlist peasants_fasci_grains0 sp3m1893_n30

describe $ylist $xlist
summarize $ylist $xlist

tabulate $ylist

ologit $ylist $xlist

mfx, predict (outcome(0))
mfx, predict (outcome(1))
mfx, predict (outcome(2))
mfx, predict (outcome(3))


*Ordered Logit with instrumental variable with 1 control

global ylist Mafia1900 
global xlist peasants_fasci_grains1 sp3m1893_n30

describe $ylist $xlist
summarize $ylist $xlist

tabulate $ylist

ologit $ylist $xlist

mfx, predict (outcome(0))
mfx, predict (outcome(1))
mfx, predict (outcome(2))
mfx, predict (outcome(3))


*Ordered Logit with instrumental variable with 2 controls
¨
global ylist Mafia1900 
global xlist peasants_fasci_grains2 sp3m1893_n30

describe $ylist $xlist
summarize $ylist $xlist

tabulate $ylist

ologit $ylist $xlist

mfx, predict (outcome(0))
mfx, predict (outcome(1))
mfx, predict (outcome(2))
mfx, predict (outcome(3))


*Ordered Logit with instrumental variable with 3 controls

global ylist Mafia1900 
global xlist peasants_fasci_grains3 sp3m1893_n30

describe $ylist $xlist
summarize $ylist $xlist

tabulate $ylist

ologit $ylist $xlist

mfx, predict (outcome(0))
mfx, predict (outcome(1))
mfx, predict (outcome(2))
mfx, predict (outcome(3))

*CLOGLOG

*TABLES 8, 9, 10, 11

gen Mafia1900bin = Mafia1900

recode Mafia1900bin 1=0
recode Mafia1900bin 2=1
recode Mafia1900bin 3=1

probit Mafia1900bin peasants_fasci lnpop1861 lnsurface centreheight maxheight slope2 pa_pdist1856 port2_pdist1856 roads1799 ave_temp var_sp3m_n30 sp3m_ave_n30
estat ic

gen newMafia1900bin = (Mafia1900bin==0)
cloglog newMafia1900bin peasants_fasci lnpop1861 lnsurface centreheight maxheight slope2 pa_pdist1856 port2_pdist1856 roads1799 ave_temp var_sp3m_n30 sp3m_ave_n30
estat ic

cloglog Mafia1900bin peasants_fasci lnpop1861 lnsurface centreheight maxheight slope2 pa_pdist1856 port2_pdist1856 roads1799 ave_temp var_sp3m_n30 sp3m_ave_n30
estat ic
margins, dydx(Mafia1900bin )


*Codes that we didn't use 

*Let's try IV with Logit

*Logit
generate Mafia1900dummy = 0 
replace Mafia1900dummy = 1 if Mafia1900>=2
replace Mafia1900dummy = . if missing(Mafia1900)
logit Mafia1900dummy peasants_fasci_hat1 
margins
logit Mafia1900dummy peasants_fasci_hat2 
margins

*Ordered logit regression
label define Mafia1900 0 "No mafia" 1 "Low Mafia" 2 "High Mafia" 3 "Very High Mafia"
label values Mafia1900 Mafia1900
numlabel, add
tab Mafia1900
ologit Mafia1900 peasants_fasci ind_dir_ratio1884 sp3m1893_n30
margins
marginsplot
margins, at(peasants_fasci=(0/1)) predict(outcome(0)) atmeans
margins, at(peasants_fasci=(0/1)) predict(outcome(1)) atmeans
margins, at(peasants_fasci=(0/1)) predict(outcome(2)) atmeans
margins, at(peasants_fasci=(0/1)) predict(outcome(3)) atmeans
*odds ratio
ologit Mafia1900 peasants_fasci ind_dir_ratio1884 sp3m1893_n30, or

*With one IV
ologit Mafia1900 peasants_fasci ind_dir_ratio1884 Citrus_groves seminatoritot_rel Vineyards Olives_groves Rural_rent Urban_rent slope2 sulfurproduction1868_70 lnpop1861 lnsurface ruralcentre1861 Mafia_n1885 Mafia_n1885max doctors_pop1885 draftrj_height_rate1885 draftrj_ill_rate1885 vaccine_pop1885 ind_pc_tax_capacity1884 ind_dir_ratio1884 affittanza mezzadria ownfarming army1875_pop eff_civ1 eff_civ2 if sp3m1893_n30!=.
margins, dydx(*) atmeans
marginsplot
margins, at(peasants_fasci=(0/1)) predict(outcome(0)) atmeans
margins, at(peasants_fasci=(0/1)) predict(outcome(1)) atmeans
margins, at(peasants_fasci=(0/1)) predict(outcome(2)) atmeans
margins, at(peasants_fasci=(0/1)) predict(outcome(3)) atmeans
