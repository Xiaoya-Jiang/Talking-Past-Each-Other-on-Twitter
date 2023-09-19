****** Part 3. VAR modeing
*** Section 1. read and process data
import delimited "expression_polarization_ts.csv", varnames(1) clear
ren date olddate //rename date to olddate
split olddate, parse(-) gen(newdate) // parse and generate variable newdate. in mac, the code is: split olddate, parse(/) gen(newdate)
destring newdate*, replace // destring

ren newdate1 year // generate year, month, date variable
ren newdate2 month
ren newdate3 day

gen date = mdy(month, day, year) //change to timer series format
format date %td
tsset date
drop olddate year month day

save polarizationRR.dta, replace

destring liberal, gen(newlib) force // destring liberal, general newlib
ipolate newlib date, gen(new_liberal) // replace missing value in newlib, generate new_liberal
ren liberal old_liberal // rename the liberal that has missing value to old_liberal
ren new_liberal liberal // rename the liberal ts that has no missing to liberal
summ liberal // summary of the liberal variable

destring conservative, gen(newcon) force
ipolate newcon date, gen(new_cons)
ren conservative old_cons
ren new_cons conservative

destring indeterminate, gen(newind) force
ipolate newind date, gen(new_ind)
ren newind indetermite_old
ren new_ind indetermite
compress

*** Section 2. VAR modeling
varsoc liberal conservative indetermite, exog(borderwall_restrict borderwall_allow travelban_restrict travelban_allow visas_restrict visas_allow sanctuary_restrict sanctuary_allow family_sep_restrict  dacadapa_restrict  dacadapa_allow  ice_restrict  ice_allow refugee_restrict  immi_pro_events  immi_p_crime  imm_v_crime  immi_hardships  trump_related trump_tweet)
// check for lag, best lag is 4

var liberal conservative indetermite, exog(borderwall_restrict borderwall_allow travelban_restrict travelban_allow visas_restrict visas_allow sanctuary_restrict sanctuary_allow family_sep_restrict  dacadapa_restrict  dacadapa_allow  ice_restrict  ice_allow refugee_restrict  immi_pro_events  immi_p_crime  imm_v_crime  immi_hardships  trump_related trump_tweet) lags(1/4)
/// liberal, conservative, indeterminate variables are endogenous variables; event features are exogenous variables
// lag one through four
// difference between VAR model here and the Prais-Winsten estimation is that in VAR, we are controlling for conservative and indeterminate/holding them constant, when examining events' impact on liberal ts

vargranger

*Impulse
irf create impulse, set(immigration, replace)
irf graph irf, irf(impulse) impulse(liberal) response(conservative) level(90)
irf graph irf, irf(impulse) impulse(conservative) response(liberal) level(90)