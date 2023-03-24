
*BSAS analysis - predictors of weight-stigmatizing attitudes

clear all
cd "BSAS_project"
use "stata11\bsa15_to_ukds_final.dta"

*******************************************
*VERSION is paramount: weight stigma questions only in versions A & D, mental health questions only in versions B & C
*SEP and political attitudes across all version.
fre ABCVer
*******************************************

*demographics:hhinc
*gender: 1=male, 2=female
fre Rsex
*age: need to tidy up:
fre Rage
recode Rage 98=. 99=.
fre Rage
summ Rage
*7 missing values - 1 don't know and 6 refusals. can impute back

*make a centered version
gen cRage=.
summ Rage, det
return list
replace cRage=Rage-r(mean)
summ cRage

*make a squared version of that:
gen cRagesq=cRage*cRage

*update: do this in categories.
*roughly 10-year bands, but need to merge the last because very few people in it in the relevant half of the sample:
egen Rage_cat=cut(Rage), at(0 18 30 40 50 60 70 80 100)
fre Rage_cat
tab Rage Rage_cat
*recode and label:
recode Rage_cat 18=1 30=2 40=3 50=4 60=5 70=6 80=7 
label define Rage_cat 1"18-29" 2"30-39" 3"40-49" 4"50-59" 5"60-69" 6"70-79" 7"80+"
label values Rage_cat Rage_cat
tab Rage Rage_cat, mi
*all good


*ethnicity?
fre RaceOri3
*make one with fewer groups:
gen RaceOri3_v2=RaceOri3
*don't know/refused:
recode RaceOri3_v2 98=. 99=.
*merge smaller Black and South Asian groups:
recode RaceOri3_v2 1/3=2 4/6=3 7/8=3 11=4 10=4
*largest group to baseline:
recode RaceOri3_v2 9=1
label define RaceOri3_v2 1"white" 2"Black" 3"Asian" 4"other, mixed"
label values RaceOri3_v2 RaceOri3_v2
fre RaceOri3_v2
tab RaceOri3 RaceOri3_v2

*SEP:
*education
fre HEdQual HEdQual2 HEdQual3
*start with HEdQual3
*however, foreign quals group has been merged with the 9 DK/refused/NA.
*separate that out:
gen HEdQual4=HEdQual3
recode HEdQual4 8=5
replace HEdQual4=. if HEdQual==8
label define HEdQual4 1"Degree" 2"Higher education below degree/A level" 3"O level or equiv/CSE" 4"No qualifications" 5"Foreign quals"
label values HEdQual4 HEdQual4
fre HEdQual4

*and a 3-group one - degree, quals below degree, no quals:
gen HEdQual_3grp=.
replace HEdQual_3grp=1 if HEdQual4==1
replace HEdQual_3grp=2 if HEdQual4==2 | HEdQual4==3 | HEdQual4==5
replace HEdQual_3grp=3 if HEdQual4==4
label define HEdQual_3grp 1"degree" 2"quals below degree" 3"no qualifications"
label values HEdQual_3grp HEdQual_3grp
fre HEdQual_3grp

*objective income:

*personal earnings:
fre REarn REarnD REarnQ
*this is tricky as doesn't apply to the ~1/2 of people not in work. leave for now

*household income:
fre HHincome HHIncD HHIncQ 
*what's with the 264 not applicable?
*not to do with single people:
fre Househld if HHIncD==-1
*also a lot of refusals (~10%) and a few don't knows.
*code both to missing:
foreach var in HHincome HHIncD HHIncQ {
recode `var' -1=. 
recode `var' 97=. 98=.
fre `var'
}
*best bet is deciles as continuous?
*no, because assumes linearity.
fre HHIncQ

*make tertiles.
fre HHincome
gen HHIncT=.
replace HHIncT=1 if HHincome<8
replace HHIncT=2 if HHincome>=8 & HHincome<15
replace HHIncT=3 if HHincome>=15 & HHincome!=.
fre HHIncT
tab HHincome HHIncT 

*subjective income:
*asked of A, C, D:
fre SRInc
*code to misisng if not asked:
recode SRInc -1=. 
*a few don't know/refusals:
recode SRInc 8=. 9=.
fre SRInc
*flip this for bigger baseline group:
gen SRInc_REV=4-SRInc
label define SRInc_REV 1"low income" 2"middle income" 3"high income"
label values SRInc_REV SRInc_REV
fre SRInc_REV

*employment status:
capture drop REconAct_v2
fre REconAct
gen REconAct_v2=REconAct
*only 3 missing
recode REconAct_v2 99=.
recode REconAct_v2 1=21 2=22
recode REconAct_v2 3/4=1 5/7=2 8=3 9=4 10=5 11=5 21=5 22=5
label define REconAct_v2 1"employed" 2"unemployed" 3"sick/disab" 4"retired" 5"looking after home/family, other"
label values REconAct_v2 REconAct_v2
fre REconAct_v2
tab REconAct REconAct_v2
*try that. might not work in imputation models, so make a simpler one:

capture drop REconAct_v3
fre REconAct
gen REconAct_v3=REconAct
*only 3 missing
recode REconAct_v3 99=.
recode REconAct_v3 1=21 2=22
recode REconAct_v3 3/4=1 5/7=2 8=3 9/22=4
label define REconAct_v3 1"employed" 2"unemployed" 3"sick/disab" 4"other economically inactive"
label values REconAct_v3 REconAct_v3
fre REconAct_v3
tab REconAct REconAct_v3

*and a 2-group one:

*for whether in labour market:
capture drop REconAct_v4
fre REconAct
gen REconAct_v4=REconAct
*only 3 missing
recode REconAct_v4 99=.
recode REconAct_v4 1=21 2=22
recode REconAct_v4 3/4=1 5/7=1 8/22=2
*need to make this 0/1 for imputation:
replace REconAct_v4=REconAct_v4-1
label define REconAct_v4 0"in labour market" 1"economically inactive"
label values REconAct_v4 REconAct_v4
fre REconAct_v4
tab REconAct REconAct_v4

*for employed vs anything else:
capture drop REconAct_v5
fre REconAct
gen REconAct_v5=REconAct
*only 3 missing
recode REconAct_v5 99=.
recode REconAct_v5 1=21 2=22
recode REconAct_v5 3/4=1 5/22=2
*need to make this 0/1 for imputation:
replace REconAct_v5=REconAct_v5-1
label define REconAct_v5 0"employed" 1"anything else"
label values REconAct_v5 REconAct_v5
fre REconAct_v5
tab REconAct REconAct_v5

*occupational social class
fre RNSSECG
*a few refusals:
recode RNSSECG -9=. 
*a few not classifiable (=8). Keep separate.
fre RNSSECG

*merge some groups:
*standard 8-group version:
gen RNSSECG_8grp=RNSSECG
recode RNSSECG_8grp 1.1=1 1.2=1
label define RNSSECG_8grp 1"1.1 and 1.2" 2"Lower managerial and professional" 3"Intermediate occupations" 4"Small employers/own account" 5"Lower supervisory and technical" 6"Semi-routine occupations" 7"Routine occupations" 8"not classifiable"
label values RNSSECG_8grp RNSSECG_8grp 
fre RNSSECG_8grp

*also make the standard 5-group version:
*https://www.ons.gov.uk/methodology/classificationsandstandards/otherclassifications/thenationalstatisticssocioeconomicclassificationnssecrebasedonsoc2010
*5-group merges 1&2 and 6&7.
gen RNSSECG_5grp=RNSSECG
recode RNSSECG_5grp 1.1=1 1.2=1 2=1 3=2 4=3 5=4 6/7=5 8=6
label define RNSSECG_5grp 1"Higher managerial, administrative and professional occupations" 2"Intermediate occupations" 3"Small employers/own account" 4"Lower supervisory/technical" 5"Semi-routine/routine occupations" 6"not classifiable"
label values RNSSECG_5grp RNSSECG_5grp 
fre RNSSECG_5grp

*housing tenure
gen Tenure_3grp=Tenure2
recode Tenure_3grp 2/3=2 4/5=3 9=.
label define Tenure_3grp 1"Owned/being bought" 2"Rented: LA, HA, etc" 3"Private rented, other"
label values Tenure_3grp Tenure_3grp
fre Tenure_3grp
tab Tenure7 Tenure_3grp

*imd: quntiles and deciles, already cleaned
fre qimd dimd
*ooh, quite a lot of missingness

*******************************************

*health-related:
*general health

*limiting long-term mental or physical health condition/disability?
fre DisNew2
*recode a few don't know/refused:
recode DisNew2 8=. 9=.
*rebase to be 0/1, needed for imputation:
recode DisNew2 2=0
fre DisNew2

*own bmi
fre EstHt EstWt

*views around own weight:
fre viewwt happywt
*NB: appears to be a mislabelling issue here: top group should be very unhappy, not very happy!
*in questionnaire, the last categ for participants was very unhappy.
*this was the one before can't choose, suggesting it is this categ which is mislabelled, rather than the other four.

*code to missing for those not asked:
foreach var in EstHt EstWt viewwt happywt {
replace `var'=. if ABCVer==2 | ABCVer==3
fre `var'
}
*even for those administered, some didn't complete it:
foreach var in EstHt EstWt viewwt happywt {
recode `var' -1=.
fre `var'
}
*for these two, also have a small group of can't choose/not answered.
*how to treat these? unlikely to MAR! if imputing, can impute back. 
foreach var in viewwt happywt {
recode `var' 8=. 9=.
fre `var'
}

*for viewwt, need to merge tiny categories:
gen viewwt_v2=viewwt
recode viewwt_v2 2=1 3=2 4=3 5=4
label define viewwt_v2 1"A bit underweight/very underweight" 2"About the right weight" 3"A bit overweight" 4"Very overweight"
label values viewwt_v2 viewwt_v2
fre viewwt_v2

*for regression models, need to rearrrange so a decent-sized middle group is baseline:
gen viewwt_v3=viewwt_v2
recode viewwt_v3 2=0 3=2 4=3
label define viewwt_v3 0"About the right weight" 1"A bit underweight/very underweight" 2"A bit overweight" 3"Very overweight"
label values viewwt_v3 viewwt_v3
fre viewwt_v3

*for happywt, more intuitive to flip so that this measures happiness not unhappiness.
fre happywt
gen happywtr=6-happywt
label define happywtr 1"very unhappy" 2"unhappy" 3"neither happy nor unhappy" 4"happy" 5"very happy"
label values happywtr happywtr
fre happywtr
tab happywt happywtr, nol

*make BMI:
*check outliers in height and weight:
summ EstHt EstWt, det
fre EstHt 
fre EstWt
*some definite errors. 

/*trim at +/- 4SD? nope, doesn't catch the low-end implausible values:
foreach var in EstHt EstWt {
summ `var', det
return list
replace `var'=. if `var'<r(mean)-(4*r(sd))
replace `var'=. if `var'>r(mean)+(4*r(sd))
summ `var', det
}
*/

*do manually:
*height:
list HowTall* if EstHt<130 
*ok, looks like the four tiny values are cases of people stating metres instead of centimetres. convert those:
replace EstHt=EstHt*100 if EstHt<120

*weight:
list MuchWgh MuchWghP MuchWghS MuchWghS MuchWghK Rsex if EstWt<35
*three of these appear to be people confusing stone with pounds or kilos.
*where stones given in place of pounds:
replace EstWt=EstWt*14 if MuchWghP==17 | MuchWghP==18
replace EstWt=EstWt*6.35029 if MuchWghK==8
*remaining person probably dropped a zero but can't know for sure, so code to missing
replace EstWt=. if EstWt<35

*now make bmi:
capture drop bmi
gen bmi=EstWt/((EstHt/100)^2)
fre bmi
*great

*******************************************

*political and social attitudes:

*left-right (derived)
summ leftrigh 
*libertarian-authoriatarian (derived)
summ libauth 

*remove those not asked:
foreach var in leftrigh libauth {
summ `var'
recode `var' -2=.
*and those with missing values. 
*if imputing, can maybe gain some back by allowing 10% missingness in this - will need to check individual items. leave for now
recode `var' 9=.
summ `var'
}

*rename to be slightly shorter - will need this later for saving coefficients from paramed:
rename leftrigh leftr
rename libauth liba

*attitudes to mental illness: can't look at overlap with weight stigma as asked in B & C only, but can look at whether predictors are similar.
*vignettes with Andy and Stephen: psychosis and depression respectively:
*psychosis:
describe MHV1MvNx MHV1Soc MHV1Fri MHV1Col MHV1Mar MHV1ChC
fre MHV1MvNx MHV1Soc MHV1Fri MHV1Col MHV1Mar MHV1ChC
*depression
describe MHV2MvNx MHV2Soc MHV2Fri MHV2Col MHV2Mar MHV2ChC
fre MHV2MvNx MHV2Soc MHV2Fri MHV2Col MHV2Mar MHV2ChC

*code to missing: not asked, also small groups of don't know/refused:
foreach var in MHV1MvNx MHV1Soc MHV1Fri MHV1Col MHV1Mar MHV1ChC MHV2MvNx MHV2Soc MHV2Fri MHV2Col MHV2Mar MHV2ChC {
    recode `var' -1=.
	recode `var' 8=. 9=.
	fre `var'
}

*nothing needs reverse-coding:
*psychosis:
fre MHV1MvNx MHV1Soc MHV1Fri MHV1Col MHV1Mar MHV1ChC
*depression
fre MHV2MvNx MHV2Soc MHV2Fri MHV2Col MHV2Mar MHV2ChC

*so can just add up.
gen psychosisstimgatizing=MHV1MvNx+MHV1Soc+MHV1Fri+MHV1Col+MHV1Mar+MHV1ChC
gen depressionstigmatizing=MHV2MvNx+MHV2Soc+MHV2Fri+MHV2Col+MHV2Mar+MHV2ChC
*can't check this against zwtstig attitudes but can see if relationships with other things look the same.

*attitudes to welfare recipients: follow DM:
/*four items:
unempjob        byte    %8.0g      unempjob * Around here, most unemployed people could find a job if they really wanted one:
sochelp         byte    %8.0g      sochelp  * Many people who get social security dont really deserve any help: Versions A, C,
dolefidl        byte    %8.0g      dolefidl   Most people on the dole are fiddling in one way or another: Versions A, C, D
welffeet        byte    %8.0g      welffeet * If welfare benefits werent so generous, people would learn to stand on own feet:
*/
fre unempjob sochelp dolefidl welffeet

*check alpha:
*NB: can't do this in imputed data because alpha not supported by mi estimate
alpha unempjob sochelp dolefidl welffeet
*reliability: 0.84


*set to missing for those not asked, and a small number of not answered:
foreach var in unempjob sochelp dolefidl welffeet {
recode `var' -2=. -1=.
recode `var' 9=.
fre `var'
}

*make a simple index - add up, but taking account of fact that all are reverse-coded:
capture drop cc_welfst
gen cc_welfst=(6-unempjob)+(6-sochelp)+(6-dolefidl)+(6-welffeet)
*rebase to start at 0:
replace cc_welfst=cc_welfst-4
label variable cc_welfst "index of welfare-stigmatizing attitudes"
fre cc_welfst
*check coding:
list cc_welfst unempjob sochelp dolefidl welffeet in 1/10
list cc_welfst unempjob sochelp dolefidl welffeet in 1/10, nol
*looks correct.

********************************************************************
*ATTITUDES TOWARDS OBESITY (VERSIONS A & D ONLY):

fre MaleObes FemObes OwnShape 
fre owtmarr owtlazy owtlose owtcare owtnhs 
*also these:
fre WorrOWt-TaxFuel

*all of these only asked of people in versions A and D. set to missing for other participants:
foreach var of varlist MaleObes FemObes OwnShape  WorrOWt-TaxFuel owtmarr owtlazy owtlose owtcare owtnhs {
replace `var'=. if ABCVer==2 | ABCVer==3
fre `var'
}

*for those in self-completion questionnaire, others missing if they didn't complete it:
foreach var of varlist MaleObes FemObes OwnShape owtmarr owtlazy owtlose owtcare owtnhs {
replace `var'=. if `var'==-1
fre `var'
}

*now deal with the "don't know/refused" and "can't choose" people. how to treat these? unlikely to MAR! if imputing, can impute back. 
*different codes for different variables:
foreach var of varlist MaleObes FemObes OwnShape {
replace `var'=. if `var'==98 | `var'==99
fre `var'
}

foreach var of varlist owtmarr owtlazy owtlose owtcare owtnhs {
tab `var'
}

*make a version of owtmarr to treat as ordered 3-group categorical
fre owtmarr 
gen owtmarr_3grp=.
replace owtmarr_3grp=1 if owtmarr==2
replace owtmarr_3grp=2 if owtmarr==8
replace owtmarr_3grp=3 if owtmarr==1
label define owtmarr_3grp 1"wouldn't affect how I felt" 2"can't choose" 3"would affect how I felt"
label values owtmarr_3grp owtmarr_3grp

tab owtmarr_3grp owtmarr, mi

*for others, set to missing.
foreach var of varlist WorrOWt-TaxFuel owtmarr owtlazy owtlose owtcare owtnhs {
replace `var'=. if `var'==8 | `var'==9
fre `var'
}

****************************
*MAIN OUTCOMES:

*make an index of weight-stigmatizing attitudes out of four of these:
fre owtlazy owtlose owtcare owtnhs

*care needed: two are reverse-coded with respect to the others, so need to flip two:
*make a cc_ version for now, as will impute:
gen cc_wtstig=(6-owtlazy)+(6-owtlose)+owtcare+owtnhs
*rebase to start at 0
replace cc_wtstig=cc_wtstig-4
label variable cc_wtstig "index of weight-stigmatizing attitudes"
fre cc_wtstig
*check it:
list cc_wtstig owtlazy owtlose owtcare owtnhs in 1/10
list cc_wtstig owtlazy owtlose owtcare owtnhs in 1/10, nol
*looks correct.

*check alpha:
*NB: can't do this in imputed data because alpha not supported by mi estimate
*automatically reverses the latter two
alpha owtlazy owtlose owtcare owtnhs, item
*reliability: 0.65

*the binary one will need to be looked at on its own:
fre owtmarr
*although, will need to flip to be 0/1 for imputation.
recode owtmarr 2=0
fre owtmarr

*update: using 3-group version!
fre owtmarr_3grp

**********************************
*others good for imputation - not all of them though, and might need a bit more prep?
foreach var of varlist WorrOWt-TaxFuel {
fre `var'
}
*come back to this as needed

************************************

*beliefs about causes of obesity:

*biological vs personal responsibility:
describe WtInher WtMetab WtEat WtExer
/*WtInher         byte    %8.0g      WtInher    Agree/disagree: being overweight is inherited from parents: Version A, D
WtMetab         byte    %8.0g      WtMetab  * Agree/disagree: most overweight people put on weight due to low metabolism: Vers
WtEat           byte    %8.0g      WtEat    * Agree/disagree: most overweight people put on weight due to eating too much: Ver
WtExer          byte    %8.0g      WtExer   * Agree/disagree: most overweight people put on weight due to lack of exercise: Ve
*/
fre WtInher WtMetab WtEat WtExer

*ok, first two capture periceved importance of biological factors, next two capture perceieved importance of diet and exercise.
*can add the four together for an index of biological determinants ---> choices?
*actually, do this the other way around so that it's consistent with the measure of structural factors: easier to interpret.
gen cc_bio=(6-WtInher)+(6-WtMetab)+WtEat+WtEat
*rebase to start at 0
replace cc_bio=cc_bio-4
label variable cc_bio "index: how important is metabolism/inheritance (vs diet+excercise) in obesity"
fre cc_bio
*check it:
list cc_bio WtInher WtMetab WtEat WtExer in 1/10
list cc_bio WtInher WtMetab WtEat WtExer in 1/10, nol
list WtInher   WtMetab   WtEat   WtExer if cc_bio==0
list WtInher   WtMetab   WtEat   WtExer if cc_bio==14
*ok

*importance of structural factors:
describe HFExpen HFTime PhysTime EDLSit SafeWkC FFCheap 
/*HFExpen         byte    %8.0g      HFExpen    Agree/disagree: healthy food too expensive for most people: Version A, D
HFTime          byte    %8.0g      HFTime     Agree/disagree: most people lack time to make healthy meals: Version A, D
PhysTime        byte    %8.0g      PhysTime   Agree/disagree: most people lack time to be physically active: Version A, D
EDLSit          byte    %8.0g      EDLSit   * Agree/disagree: everyday life means people spend too much time sitting down: Ver
SafeWkC         byte    %8.0g      SafeWkC    Agree/disagree: not enough safe places to walk/cycle: Version A, D
FFCheap         byte    %8.0g      FFCheap    Agree/disagree: cheap fast food too easily available: Version A, D
*/
fre HFExpen HFTime PhysTime EDLSit SafeWkC FFCheap 

*can add up the six into an index. none reverse-coded.

gen cc_struc=(6-HFExpen)+(6-HFTime)+(6-PhysTime)+(6-EDLSit)+(6-SafeWkC)+(6-FFCheap)
*rebase to start at 0
replace cc_struc=cc_struc-6
label variable cc_struc "index: how important are structural factors in obesity"
fre cc_struc
*check it:
list cc_struc HFExpen HFTime PhysTime SafeWkC FFCheap EDLSit in 1/10
list cc_struc HFExpen HFTime PhysTime SafeWkC FFCheap EDLSit in 1/10, nol
*looks correct.

*as for the other indexes, impute individual items then add up with mi passive


**************************************************
*beliefs about health consequences of obesity.

*these ask about whether people with obesity more likely to suffer from a range of things:
fre ObHlthC1 ObHlthC2 ObHlthC3 ObHlthC4 ObHlthC5 ObHlthC6 ObHlthC7 ObHlthC8 ObHlthC9 ObHlthC10 ObHlthC11
*these two are different: all of them, or none of them, so can't be added, need to be incorporated differently
fre ObHlthC12 ObHlthC13

*make an index:
gen ObHlth_cons=0
*code to missing for people not asked:
replace ObHlth_cons=. if ABCVer==2 | ABCVer==3

*then add up:
foreach var in ObHlthC1 ObHlthC2 ObHlthC3 ObHlthC4 ObHlthC5 ObHlthC6 ObHlthC7 ObHlthC8 ObHlthC9 ObHlthC10 ObHlthC11 {
replace ObHlth_cons=ObHlth_cons+1 if `var'==1
}
*how do these compare to the other two?
fre ObHlth_cons if ObHlthC12==1
fre ObHlth_cons if ObHlthC13==1
*check individual responses:
list ObHlthC1 ObHlthC2 ObHlthC3 ObHlthC4 ObHlthC5 ObHlthC6 ObHlthC7 ObHlthC8 ObHlthC9 ObHlthC10 ObHlthC11 if ObHlthC12==1
list ObHlthC1 ObHlthC2 ObHlthC3 ObHlthC4 ObHlthC5 ObHlthC6 ObHlthC7 ObHlthC8 ObHlthC9 ObHlthC10 ObHlthC11 if ObHlthC13==1
*ok, were alternative responses! so need to incorporate:
replace ObHlth_cons=11 if ObHlthC12==1
replace ObHlth_cons=0 if ObHlthC13==1

*what to do about people who were in this version but didn't answer any items?
fre ObHlthC1 ObHlthC2 ObHlthC3 ObHlthC4 ObHlthC5 ObHlthC6 ObHlthC7 ObHlthC8 ObHlthC9 ObHlthC10 ObHlthC11 if ABCVer==1 | ABCVer==4
*looks like these are all the same ones
foreach var in ObHlthC1 ObHlthC2 ObHlthC3 ObHlthC4 ObHlthC5 ObHlthC6 ObHlthC7 ObHlthC8 ObHlthC9 ObHlthC10 ObHlthC11 {
replace ObHlth_cons=. if `var'==. & (ABCVer==1 | ABCVer==4)
}
*yep - just 9 so can lose them
*check
label variable ObHlth_cons "more likely if overweight: index"
fre ObHlth_cons

************************************

*own peception of prevalence of weight stigma in job market:
*make binary, and flip it so that largest group is biggest:
gen WtAffJb_v2=WtAffJb
recode WtAffJb_v2 2=0 
recode WtAffJb_v2 1/4=1
label define WtAffJb_v2 0"Healthy weight more likely" 1"other answer"
label values WtAffJb_v2 WtAffJb_v2
label variable WtAffJb_v2 "Healthy weight more likely to be offered job than very overweight?"
fre WtAffJb_v2

****************************************************************************************************************************************

****COMPLETE CASE DESCRIPTIVES:

capture log close
log using "complete-case descriptives.log", replace

*RESTRICT TO ANALYTIC SAMPLE:
keep if ABCVer==1 | ABCVer==4
keep if Rage!=.
count
*2,186

*check variables:
summ Rage cRage cRagesq bmi cc_* leftr liba

tab1 Rsex Rage_cat RaceOri3_v2 HEdQual_3grp SRInc_REV RNSSECG_5grp REconAct_v3 ///
DisNew2 ///
viewwt_v3 happywtr ///
owtmarr_3grp ///
WtAffJb_v2

log close

*how many people have full data, and how are they different?
capture drop cc_full
gen cc_full=1
foreach var in RaceOri3_v2 HEdQual_3grp HHIncT SRInc_REV RNSSECG_5grp REconAct_v3 bmi happywtr viewwt_v3 DisNew2 cc_welfst cc_wtstig cc_bio cc_struc ObHlth_cons leftr liba cc_welfst owtmarr_3grp {
replace cc_full=0 if `var'==.
}
fre cc_full


*how are they different? 
foreach var in Rage bmi happywtr cc_wtstig cc_bio cc_struc ObHlth_cons leftr liba cc_welfst {
display "`var''"
sdtest `var', by(cc_full)
}
*equal variance:
foreach var in happywtr cc_bio cc_struc leftr liba cc_welfst {
display "`var''"
ttest `var', by(cc_full)
}

*unequal variance for age, bmi, cc_wtstig, ObHlth_cons
foreach var in Rage bmi cc_wtstig ObHlth_cons {
display "`var''"
ttest `var', by(cc_full) unequal
}

foreach var in Rsex RaceOri3_v2 HEdQual_3grp HHIncT SRInc_REV RNSSECG_5grp REconAct_v3 viewwt_v3 DisNew2 owtmarr_3grp {
tab `var' cc_full, chi col
}

**************************************************************************
***COMPLETE-CASE ANALYSIS:

*RESTRICT TO ANALYTIC SAMPLE:
keep if ABCVer==1 | ABCVer==4
keep if Rage!=.
count
*2,186

********************************************
*MAIN OUTCOME: CONTINUOUS INDEX:

*base model: age anad sex only
regress cc_wtstig cRage Rsex 
*women less stigmatizing
*no clear age effects, but might not be linear

regress cc_wtstig i.Rage_cat Rsex 
*ok, definitely NOT linear

*ETHNICITY:
regress cc_wtstig i.Rage_cat Rsex i.RaceOri3_v2

*SEP measures in turn:
foreach var in i.HEdQual_3grp i.HHIncT i.SRInc_REV i.RNSSECG_5grp i.REconAct_v2 i.REconAct_v3 {
regress cc_wtstig i.Rage_cat Rsex `var'
}

*long-term health condition/disability?
regress cc_wtstig i.Rage_cat Rsex DisNew2

*own bmi?
regress cc_wtstig i.Rage_cat Rsex bmi

*own perception of shape?
regress cc_wtstig i.Rage_cat Rsex OwnShape

*own attitudes towards own weight?
fre viewwt_v2 happywtr
regress cc_wtstig i.Rage_cat Rsex i.viewwt_v3

regress cc_wtstig i.Rage_cat Rsex happywtr
regress cc_wtstig i.Rage_cat Rsex i.happywtr

*beliefs: consequences
fre ObHlth_cons
regress cc_wtstig i.Rage_cat Rsex ObHlth_cons

**political/social:
regress cc_wtstig i.Rage_cat Rsex leftr
regress cc_wtstig i.Rage_cat Rsex libauth
regress cc_wtstig i.Rage_cat Rsex cc_welfst

**************************************************
*SECONDARY OUTCOME:

*base model: age anad sex only
regress owtmarr_3grp i.Rage_cat Rsex
*women less stigmatizing
*linear age effects, positively related

*ETHNICITY:
regress owtmarr_3grp i.Rage_cat Rsex i.RaceOri3_v2

*SEP measures in turn:
foreach var in i.HEdQual_3grp i.HHIncT i.SRInc_REV i.RNSSECG_5grp i.REconAct_v2 i.REconAct_v3 {
regress owtmarr_3grp i.Rage_cat Rsex `var'
}

*long-term health condition/disability?
regress owtmarr_3grp i.Rage_cat Rsex DisNew2

*own bmi?
regress owtmarr_3grp i.Rage_cat Rsex bmi

*own perception of shape?
regress owtmarr_3grp i.Rage_cat Rsex OwnShape

*own attitudes towards own weight?
fre viewwt_v2 happywtr
regress owtmarr_3grp i.Rage_cat Rsex i.viewwt_v3

regress owtmarr_3grp i.Rage_cat Rsex happywtr

**political/social:
regress owtmarr_3grp i.Rage_cat Rsex leftr
regress owtmarr_3grp i.Rage_cat Rsex libauth
regress owtmarr_3grp i.Rage_cat Rsex cc_welfst

save "bsas_pre_imp.dta", replace

*****************************************************************************************************************************************

*ENTRY POINT: BUILD AN IMPUTATION MODEL.

clear all
cd "BSAS_project"
use "bsas_pre_imp.dta"

*RESTRICT TO THE RELEVANT HALF OF THE SURVEY:
keep if ABCVer==1 | ABCVer==4
count
*2188

*******************************************
*how to take account of nonlinearity with age?

*apparently, the best approach is JAR: "just another variable". specify the squared term first and put this in imputation models.
*https://www.ssc.wisc.edu/sscc/pubs/stata_mi_models.htm#Transformations
*https://www.ssc.wisc.edu/sscc/pubs/stata_mi_ex.htm#Non-Linearity

*a complication is that with this approach, can end up having combinations of the linear and squared term which are inconsistent.
*not if none of them are imputed because both on the right-hand side!

*restrict to people with age and sex:
keep if Rage!=.
*2 observations dropped
keep if Rsex!=.
*0 dropped

******************************************
mi set wide

*use flipped versions of variables with larger groups as baseline, if possible.

*check variables:
fre Rage_cat RaceOri3_v2 HEdQual_3grp HHIncT SRInc_REV RNSSECG_5grp REconAct_v3 Tenure_3grp ///
DisNew2 ///
bmi viewwt_v3 happywtr ///
leftr liba ///
unempjob sochelp dolefidl welffeet ///
MaleObes FemObes OwnShape ///
owtlazy owtlose owtcare owtnhs owtmarr_3grp  ///
WtInher WtMetab WtEat WtExer ///
HFExpen HFTime PhysTime EDLSit SafeWkC FFCheap ///
WtAffJb_v2 ObHlth_cons

*register variables:
*nb: educational quals var has missingness so can't go on RHS 
mi register regular Rsex Rage_cat

mi register imputed RaceOri3_v2 HEdQual_3grp HHIncT SRInc_REV RNSSECG_5grp REconAct_v3 Tenure_3grp ///
DisNew2 ///
bmi viewwt_v3 happywtr ///
leftr liba ///
unempjob sochelp dolefidl welffeet ///
MaleObes FemObes OwnShape ///
owtlazy owtlose owtcare owtnhs owtmarr_3grp ///
WtInher WtMetab WtEat WtExer ///
HFExpen HFTime PhysTime EDLSit SafeWkC FFCheap ///
WtAffJb_v2 ObHlth_cons

mi impute chained ///
(mlogit) RaceOri3_v2 ///
(mlogit) HEdQual_3grp ///
(mlogit) REconAct_v3 ///
(mlogit) RNSSECG_5grp ///
(mlogit) HHIncT ///
(mlogit) SRInc_REV ///
(mlogit) Tenure_3grp ///
(truncreg, ll(12.99) ul(53.64)) bmi ///
(mlogit) viewwt_v3 ///
(pmm, knn(10)) happywtr ///
(pmm, knn(10)) ObHlth_cons ///
(logit) DisNew2 ///
(truncreg, ll(1) ul(11)) MaleObes FemObes OwnShape ///
(pmm, knn(10)) owtlazy owtlose owtcare owtnhs ///
(ologit) owtmarr_3grp ///
(pmm, knn(10)) HFExpen HFTime PhysTime EDLSit SafeWkC FFCheap ///
(pmm, knn(10)) WtInher WtMetab WtEat WtExer ///
(logit) WtAffJb_v2 ///
(truncreg, ll(1) ul(5)) leftr liba ///
(pmm, knn(10)) unempjob sochelp dolefidl welffeet ///
=Rsex i.Rage_cat, rseed(99) dots add(50) augment
*nb: requires augment option, otherwise pefect prediction

**prep in mi passive:

*wtstig attitudes:
mi passive: gen wtstig=(6-owtlazy)+(6-owtlose)+owtcare+owtnhs
*rebase to start at 0
mi passive: replace wtstig=wtstig-4
label variable wtstig "index of weight-stigmatizing attitudes"
mi estimate: mean wtstig

*welfst attitudes:
mi passive: gen welfst=(6-unempjob)+(6-sochelp)+(6-dolefidl)+(6-welffeet)
*rebase to start at 0:
mi passive: replace welfst=welfst-4
label variable welfst "index of welfare-stigmatizing attitudes"
mi estimate: prop welfst

*beliefs about obesity: biology vs choices
mi passive: gen bio=(6-WtInher)+(6-WtMetab)+WtEat+WtEat
*rebase to start at 0
mi passive: replace bio=bio-4
label variable bio "index: how important is metabolism/inheritance (vs diet+excercise) in obesity"
mi estimate: prop bio

*beliefs about obesity: structural factors
mi passive: gen struc=(6-HFExpen)+(6-HFTime)+(6-PhysTime)+(6-EDLSit)+(6-SafeWkC)+(6-FFCheap)
*rebase to start at 0
mi passive: replace struc=struc-6
label variable struc "index: how important are structural factors in obesity"
*mi estimate: prop struct
*don't panic: varies between because range is different in m=27. this variable is treated as continuous in later regressions so that's ok

*make a binary variable for whether has a degree?
fre HEdQual_3grp
mi passive: gen degree=.
mi passive: replace degree=1 if HEdQual_3grp==1
mi passive: replace degree=0 if HEdQual_3grp==2 | HEdQual_3grp==3
mi estimate: prop degree

*merge small groups for subjective income:
mi passive: gen SRInc_2grp=SRInc_REV
mi passive: replace SRInc_2grp=2 if SRInc_2grp==3
label define SRInc_2grp 1"low" 2"middle or high"
label values SRInc_2grp SRInc_2grp
label variable SRInc_2grp "subjective income, binary"
mi estimate: prop SRInc_2grp

*make standardized versions of all the belief/attitudinal scales:
foreach var in wtstig bio struc ObHlth_cons welfst leftr liba {
mi passive: egen z`var'=std(`var')
}

*and get bmi on a less ridiculous scale:
*what's the S.D. in the unimputed data?
summ bmi
*5.2kg/m2
*so makes sense to do this on the scale of per 5kg/m2
mi passive: gen rescale_bmi=bmi/5
label variable rescale_bmi "BMI, for coefficients per 5kg/m2"
summ rescale_bmi

save "bsas_50imps.dta", replace

***********************************************************************************

*ENTRY POINT: post-imputation

clear all
cd "BSAS_project"
use "bsas_50imps.dta", clear

*****************************************************************
*DESCRIPTIVES:

capture ssc install misum
capture ssc install putexcel

mi convert flong

capture replace "results\imputed_Table1.xlsx"
putexcel set "results\imputed_Table1.xlsx", replace

local k=3
putexcel A`k'="Table 1: Descriptive Characteristics of Analytic Samplea", bold 
local k=`k'+1
putexcel B`k'="mean" C`k'="SD" D`k'="min" E`k'="max" , bold border(top bottom)
***ssc install misum***
*Means for contin ones
local k=`k'+1
foreach var in Rage bmi wtstig bio struc ObHlth_cons leftr liba welfst {
misum `var' 
putexcel A`k'="`var'"
matrix `var'_mean=r(`var'_mean)
putexcel B`k'=matrix(`var'_mean)
matrix `var'_sd=r(`var'_sd)
putexcel C`k'=matrix(`var'_sd)
matrix `var'_min=r(`var'_min)
putexcel D`k'=matrix(`var'_min)
matrix `var'_max=r(`var'_max)
putexcel E`k'=matrix(`var'_max)
local k=`k'+1
}

*Categ ones:
***ssc install distinct***
local k=17
putexcel D`k'="%" , bold
local k=`k'+1
foreach var in Rsex RaceOri3_v2 HEdQual_3grp HHIncT SRInc_REV RNSSECG_5grp REconAct_v3 DisNew2 viewwt_v3 happywtr owtmarr_3grp {
*Get proportions, tranpose,fill
mi estimate: proportion `var' 
matlist e(b_mi)
matrix `var'=e(b_mi)'
putexcel A`k'="`var'" D`k'=matrix(`var') 
*get number of rows needed before the next one
capture drop counter
by `var', sort: gen counter=1 if _n==1
replace counter=sum(counter)
local k=`k'+counter[_N]
display `k'
}

*****************************************************************

*regression models: index outcome

capture log close
log using "regressionresults_zwtstig.log", replace

*regression models:

eststo clear

*base model: age and sex only
eststo base: mi estimate, post: regress zwtstig i.Rage_cat Rsex 
*women less stigmatizing
*no clear age effects, but might not be linear

*ETHNICITY:
eststo ethnicity: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.RaceOri3_v2

*SEP measures in turn:
eststo HEdQual_3grp: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HEdQual_3grp
eststo HHIncT: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HHIncT
eststo SRInc_REV: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.SRInc_REV 
eststo RNSSECG_5grp: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.RNSSECG_5grp
eststo REconAct_v3: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.REconAct_v3

*own rescale_bmi?
eststo rescale_bmi: mi estimate, post: regress zwtstig i.Rage_cat Rsex rescale_bmi

*own attitudes towards own weight?
eststo viewwt_v3: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.viewwt_v3

*happiness with own weight
eststo happywtr: mi estimate, post: regress zwtstig i.Rage_cat Rsex happywtr

*long-term health condition/disability?
eststo DisNew2: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.DisNew2

*beliefs about causes:
*biology vs choice:
eststo zbio: mi estimate, post: regress zwtstig i.Rage_cat Rsex zbio
*structural factors important:
eststo zstruc: mi estimate, post: regress zwtstig i.Rage_cat Rsex zstruc
*and consequences:
eststo zObHlth_cons: mi estimate, post: regress zwtstig i.Rage_cat Rsex zObHlth_cons

**political/social:
eststo zleftr: mi estimate, post: regress zwtstig i.Rage_cat Rsex zleftr
eststo zliba: mi estimate, post: regress zwtstig i.Rage_cat Rsex zliba
eststo zwelfst: mi estimate, post: regress zwtstig i.Rage_cat Rsex zwelfst


*export in one file:
*do this in the order you want it to be in the table! makes life much easier with formatting:
capture erase "results/imputed_main_effects_zwtstig.xls"
capture erase "results/imputed_main_effects_zwtstig.txt"
*include reference groups, as useful to have in table in manuscript
estout using "results/imputed_main_effects_zwtstig.xls", cells ("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") ///
keep(*Rage* *Rsex *RaceOri3_v2 *HEdQual_3grp *HHIncT *SRInc_REV *RNSSECG_5grp *REconAct_v3 rescale_bmi *viewwt_v3 happywtr *DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst) ///
replace title(Associations of demographic, socioeconomic, health-related and attitudinal factors with weight-stigmatizing attitudes) ///
note("all models include age and gender") 

*for formatting in excel: formula for CI in single column is =CONCAT(C4&","&D4)

log close

*******************************

*GRAPH THIS.

*relabel everything as you want it to appear on the graph:
label variable Rage_cat "age categories"
*label variable cRage "age (years)"
*label variable cRagesq "age (years), squared"
label variable Rsex "gender: women vs men"
label variable RaceOri3_v2 "ethnic group (ref: white)"
label variable rescale_bmi "own BMI (per 5kg/m2)"
label variable DisNew2 "has long-standing illness/disability"
label variable viewwt_v3 "perception of own weight"
label variable happywtr "happiness with own weight"
label variable zbio "importance of inheritance/metabolism index"
label variable zstruc "importance of structural factors index"
label variable zObHlth_cons "health consequences of obesity index"
label variable zleftr "left-right index (higher values: rightwing)"
label variable zliba "libertarian-authoritarian index (higher values: authoritarian)"
label variable zwelfst "welfare-stigmatizing index"

*demographic:
coefplot base, /// 
keep (*Rage* Rsex) xline(0) ///
rename (1.Rage_cat =  "age: 18-29" 2.Rage_cat = "age: 30-39" 3.Rage_cat = "age: 40-49" 4.Rage_cat = "50-59" 5.Rage_cat = "60-69" 6.Rage_cat = "70-79" 7.Rage_cat = "80+") ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 1: Demographic differences: weight-stigmatizing index*", pos(11) size(small)) ///
note("*All models include age and gender. Reference age group: 18-29. Indexes are standardized.", size(vsmall)) 
graph save "results/graphs/coefplot_demographic_differences_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_demographic_differences_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_demographic_differences_zwtstig.tif, replace width(800)

*socioeconomic differences:
coefplot HEdQual_3grp HHIncT SRInc_REV RNSSECG_5grp REconAct_v3, /// 
keep (*HEdQual_3grp* *HHIncT* *SRInc_REV* *RNSSECG_5grp* *REconAct_v3*) xline(0) ///
rename (1.HEdQual_3grp =  "university degree" 2.HEdQual_3grp = "qualifications below degree" 3.HEdQual_3grp = "no qualifications" ///
1.HHIncT = "household income: lowest tercile" 2.HHIncT = "household income: middle tercile" 3.HHIncT = "household income: highest tercile" ///
1.SRInc_REV = "subjective income: low" 2.SRInc_REV = "subjective income: middle" 3.SRInc_REV = "subjective income: high" ///
1.RNSSECG_5grp = "NSSEC: Higher managerial, administrative and professional occupations" 2.RNSSECG_5grp = "NSSEC: Intermediate occupations" 3.RNSSECG_5grp = "NSSEC: Small employers/own account" 4.RNSSECG_5grp ="NSSEC: Lower supervisory/technical" 5.RNSSECG_5grp ="NSSEC: Semi-routine/routine" 6.RNSSECG_5grp ="NSSEC: not classifiable" ///
1.REconAct_v3 = "Employment status: employed" 2.REconAct_v3 ="Employment status: unemployed" 3.REconAct_v3="Employment status: Permanently sick/disabled" 4.REconAct_v3="Employment status: other") ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 2: Socioeconomic differences: weight-stigmatizing index*", pos(11) size(small)) ///
note("*All models include age and gender. Reference group, education: has a degree." "Reference, household income tercile: lowest. Reference, subjective income: low." "Reference, NS-SEC: 1&2. Reference, employment status: employed/self-employed." "Indexes are standardized.", size(vsmall)) 
graph save "results/graphs/coefplot_socioeconomic_differences_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_socioeconomic_differences_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_socioeconomic_differences_zwtstig.tif, replace width(800)

*own bmi and health:
coefplot rescale_bmi DisNew2 happywtr viewwt_v3, /// 
keep (rescale_bmi *DisNew2* happywtr *viewwt_v3*) xline(0) ///
rename(1.DisNew2 = "has longterm illness/disability (yes vs. no)" ///
0.viewwt_v3 =  "self-perception: about right weight" 1.viewwt_v3 = "self-perception: underweight" 2.viewwt_v3 = "self-perception: a bit overweight" 3.viewwt_v3 = "self-perception: very overweight") ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 3: Own weight and health: differences in weight-stigmatizing index*", pos(11) size(small)) ///
note("*All models include age and gender. Reference group, for perception of own weight: about" "the right weight. Indexes are standardized.", size(vsmall)) 
graph save "results/graphs/coefplot_ownbmihealth_differences_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_ownbmihealth_differences_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_ownbmihealth_differences_zwtstig.tif, replace width(800)

*attitudinal differences:
coefplot zbio zstruc zObHlth_cons zleftr zliba zwelfst, ///
keep (*zbio* *zstruc* *zObHlth_cons* *zleftr* *zliba* *zwelfst*) xline(0) ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 4: Beliefs and attitudes: differences in weight-stigmatizing index*", pos(11) size(small)) ///
note("*All models include age and gender. Indexes are standardized.", size(vsmall)) 
graph save "results/graphs/coefplot_attitudinal_differences_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_attitudinal_differences_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_attitudinal_differences_zwtstig.tif, replace width(800)

***************************************************************************************************************

*ENTRY POINT: MEDIATION of the continuous outcome. 

clear all
cd "BSAS_project"
use "bsas_50imps.dta", clear

capture log close
log using "attenuationresults_zwtstig.log", replace

*attenuation models:

*age differences:
*unadjusted difference:
mi estimate: regress zwtstig i.Rage_cat Rsex
*then observe attenuation:
foreach var in rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
mi estimate: regress zwtstig i.Rage_cat Rsex `var'
}
*own bmi is important
*all together:
mi estimate: regress zwtstig i.Rage_cat Rsex rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst

*sex differences:
*unadjusted difference:
mi estimate: regress zwtstig i.Rage_cat Rsex
*then observe attenuation:
foreach var in rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
mi estimate: regress zwtstig i.Rage_cat Rsex `var'
}
*nothing really explains the sex difference individually, but biggest point attenuation is for happywtr and struc
*all together:
mi estimate: regress zwtstig i.Rage_cat Rsex rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst
*still not reduced!

*sociodemographic differences:
*education:
*unadjusted difference:
mi estimate: regress zwtstig i.Rage_cat Rsex i.HEdQual_3grp
*then observe attenuation:
foreach var in rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
display "`var'"
mi estimate: regress zwtstig i.Rage_cat Rsex i.HEdQual_3grp `var'
}
mi estimate: regress zwtstig i.Rage_cat Rsex i.HEdQual_3grp rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst


*objective income:
*unadjusted difference:
mi estimate: regress zwtstig i.Rage_cat Rsex i.HHIncT
*then observe attenuation:
foreach var in rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
display "`var'"
mi estimate: regress zwtstig i.Rage_cat Rsex i.HHIncT `var'
}
mi estimate: regress zwtstig i.Rage_cat Rsex i.HHIncT rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst

*subjective income:
*unadjusted difference:
mi estimate: regress zwtstig i.Rage_cat Rsex i.SRInc_REV
*then observe attenuation:
foreach var in rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
display "`var'"
mi estimate: regress zwtstig i.Rage_cat Rsex i.SRInc_REV `var'
}
mi estimate: regress zwtstig i.Rage_cat Rsex i.SRInc_REV rescale_bmi i.viewwt_v3 happywtr i.DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst

log close

**************
*GRAPH THIS.

capture ssc install eststo
capture ssc install coeflpot

*label all exposures and mediators as you want them on the graph:
label variable Rage_cat "Age categories"
*label variable cRage "age (years)"
*label variable cRagesq "age (years), squared"
label variable Rsex "gender: women vs men"
label variable RaceOri3_v2 "ethnic group (ref: white)"
label variable HEdQual_3grp "educational qualifications"
label variable SRInc_REV "subjective income"
label variable bmi "own BMI (per 5kg/m2)"
label variable DisNew2 "has long-standing illness/disability"
label variable viewwt_v3 "perception of own weight"
label variable happywtr "happiness with own weight"
label variable zbio "importance of inheritance/metabolism"
label variable zstruc "importance of structural factors"
label variable zObHlth_cons "health consequences of obesity"
label variable zleftr "left-right index (higher values: rightwing)"
label variable zliba "libertarian-authoritarian index (higher values: authoritarian)"
label variable zwelfst "welfare-stigmatizing index"


*AGE AND GENDER: from same model 
eststo clear

*unadjusted difference:
eststo dem_base: mi estimate, post: regress zwtstig i.Rage_cat Rsex
estimates store dem_base
*then observe attenuation:
foreach var in rescale_bmi happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
display "`var'"
eststo dem_`var': mi estimate, post: regress zwtstig i.Rage_cat Rsex `var'
estimates store dem_`var'
}
foreach var in viewwt_v3 {
display "`var'"
eststo dem_`var': mi estimate, post: regress zwtstig i.Rage_cat Rsex i.`var'
estimates store dem_`var'
}
*and then one adjusted for everything:
eststo dem_all: mi estimate, post: regress zwtstig i.Rage_cat Rsex rescale_bmi i.viewwt_v3 happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst
estimates store dem_all

*PLOTS: two for age, as so many categs
coefplot dem_base dem_rescale_bmi dem_viewwt_v3 dem_happywtr dem_DisNew2 dem_zbio dem_zstruc dem_zObHlth_cons dem_zleftr dem_zliba dem_zwelfst dem_all, ///
keep (2.Rage_cat 3.Rage_cat 4.Rage_cat) xline(0) asequation swapnames ///
rename (1.Rage_cat =  "18-29" 2.Rage_cat = "30-39" 3.Rage_cat = "40-49" 4.Rage_cat = "50-59" 5.Rage_cat = "60-69" 6.Rage_cat = "70-79" 7.Rage_cat = "80+") ///
eqrename (dem_base = "Adjusted for: gender" ///
dem_rescale_bmi = "+ own BMI" ///
dem_viewwt_v3 ="+ view of own weight" ///
dem_happywtr = "+ happiness with own weight" ///
dem_DisNew2 = "+ longstanding illness/disability" ///
dem_zbio = "+ importance of inheritance/metabolism" ///
dem_zstruc = "+ importance of structural factors" ///
dem_zObHlth_cons = "+ perceived health consequences" ///
dem_zleftr = "+ left-right" ///
dem_zliba = "+ libertarian-authoritarian" ///
dem_zwelfst = "+ welfare-stigmatizing" ///
dem_all = "+ full adjustment", regex) ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 5a: Age differences in weight-stigmatizing index*", pos(11) size(small)) ///
note ("*Coefficients show differences by age group (reference: 18-29). Indexes are standardized.", size(vsmall))
*save it
graph save "results/graphs/coefplot_age_pt1_mediation_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_age_pt1_mediation_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_age_pt1_mediation_zwtstig.tif, replace width(800)

coefplot dem_base dem_rescale_bmi dem_viewwt_v3 dem_happywtr dem_DisNew2 dem_zbio dem_zstruc dem_zObHlth_cons dem_zleftr dem_zliba dem_zwelfst dem_all, ///
keep (5.Rage_cat 6.Rage_cat 7.Rage_cat) xline(0) asequation swapnames ///
rename (1.Rage_cat =  "18-29" 2.Rage_cat = "30-39" 3.Rage_cat = "40-49" 4.Rage_cat = "50-59" 5.Rage_cat = "60-69" 6.Rage_cat = "70-79" 7.Rage_cat = "80+") ///
eqrename (dem_base = "Adjusted for: gender" ///
dem_rescale_bmi = "+ own BMI" ///
dem_viewwt_v3 ="+ view of own weight" ///
dem_happywtr = "+ happiness with own weight" ///
dem_DisNew2 = "+ longstanding illness/disability" ///
dem_zbio = "+ importance of inheritance/metabolism" ///
dem_zstruc = "+ importance of structural factors" ///
dem_zObHlth_cons = "+ perceived health consequences" ///
dem_zleftr = "+ left-right" ///
dem_zliba = "+ libertarian-authoritarian" ///
dem_zwelfst = "+ welfare-stigmatizing" ///
dem_all = "+ full adjustment", regex) ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 5b: Age differences in weight-stigmatizing index*", pos(11) size(small)) ///
note ("*Coefficients show differences by age group (reference: 18-29). Indexes are standardized.", size(vsmall))
*save it
graph save "results/graphs/coefplot_age_pt2_mediation_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_age_pt2_mediation_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_age_pt2_mediation_zwtstig.tif, replace width(800)

*one for gender:
coefplot dem_base dem_rescale_bmi dem_viewwt_v3 dem_happywtr dem_DisNew2 dem_zbio dem_zstruc dem_zObHlth_cons dem_zleftr dem_zliba dem_zwelfst dem_all, ///
keep (*Rsex*) xline(0) asequation swapnames ///
rename(1.Rsex = "women vs men") ///
eqrename (dem_base = "Adjusted for: age" ///
dem_rescale_bmi = "+ own BMI" ///
dem_viewwt_v3 ="+ view of own weight" ///
dem_happywtr = "+ happiness with own weight" ///
dem_DisNew2 = "+ longstanding illness/disability" ///
dem_zbio = "+ importance of inheritance/metabolism" ///
dem_zstruc = "+ importance of structural factors" ///
dem_zObHlth_cons = "+ perceived health consequences" ///
dem_zleftr = "+ left-right" ///
dem_zliba = "+ libertarian-authoritarian" ///
dem_zwelfst = "+ welfare-stigmatizing" ///
dem_all = "+ full adjustment", regex) ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 6: Gender differences in weight-stigmatizing index*", pos(11) size(small)) ///
note ("*Coefficients show differences by gender (women vs men). Indexes are standardized.", size(vsmall))
*save it
graph save "results/graphs/coefplot_gender_mediation_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_gender_mediation_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_gender_mediation_zwtstig.tif, replace width(800)

******************************************************
*export results: age and gender from the same model!
capture erase "results/imputed_demographic_attenuation_zwtstig.xls"
capture erase "results/imputed_demographic_attenuation_zwtstig.txt"
*include reference groups, as useful to have in table in manuscript
estout using "results/imputed_demographic_attenuation_zwtstig.xls", cells ("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") ///
keep(*Rage* *Rsex) ///
replace title(Demographic associations with index: attenuation with adjustment) ///
note("All models include age and gender") 
*will need to transpose....
*******************************************************************************

*EDUCATION:
eststo clear

*unadjusted difference:
eststo educ_base: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HEdQual_3grp
estimates store educ_base
*then observe attenuation:
foreach var in rescale_bmi happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
display "`var'"
eststo educ_`var': mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HEdQual_3grp `var'
estimates store educ_`var'
}
foreach var in viewwt_v3 {
display "`var'"
eststo educ_`var': mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HEdQual_3grp i.`var'
estimates store educ_`var'
}
*and then one adjusted for everything:
eststo educ_all: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HEdQual_3grp rescale_bmi i.viewwt_v3 happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst
estimates store educ_all

*PLOT:
coefplot educ_base educ_rescale_bmi educ_viewwt_v3 educ_happywtr educ_DisNew2 educ_zbio educ_zstruc educ_zObHlth_cons educ_zleftr educ_zliba educ_zwelfst educ_all, ///
keep (*HEdQual_3grp*) xline(0) asequation swapnames ///
rename(2.HEdQual_3grp = "quals below degree" 3.HEdQual_3grp = "no quals") ///
eqrename (educ_base = "Adjusted for: age, gender" ///
educ_rescale_bmi = "+ own BMI" ///
educ_viewwt_v3 ="+ view of own weight" ///
educ_happywtr = "+ happiness with own weight" ///
educ_DisNew2 = "+ longstanding illness/disability" ///
educ_zbio = "+ importance of inheritance/metabolism" ///
educ_zstruc = "+ importance of structural factors" ///
educ_zObHlth_cons = "+ perceived health consequences" ///
educ_zleftr = "+ left-right" ///
educ_zliba = "+ libertarian-authoritarian" ///
educ_zwelfst = "+ welfare-stigmatizing" ///
educ_all = "+ full adjustment", regex) ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 7: Educational differences in weight-stigmatizing index*", pos(11) size(small)) ///
note ("*Reference category: has a university degree. Indexes are standardized.", size(vsmall))
*save it
graph save "results/graphs/coefplot_educ_mediation_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_educ_mediation_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_educ_mediation_zwtstig.tif, replace width(800)

******************************************************
*export results: education only
capture erase "results/imputed_education_attenuation_zwtstig.xls"
capture erase "results/imputed_education_attenuation_zwtstig.txt"
*include reference groups, as useful to have in table in manuscript
estout using "results/imputed_education_attenuation_zwtstig.xls", cells ("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") ///
keep(*HEdQual_3grp*) ///
replace title(Education associations with index: attenuation with adjustment) ///
note("All models include age and gender") 
*will need to transpose....

*********************************************************************************************************************************
*OBJECTIVE INCOME TERCILES:

eststo clear

*unadjusted difference:
eststo objinc_base: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HHIncT
estimates store objinc_base
*then observe attenuation:
foreach var in rescale_bmi happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
display "`var'"
eststo objinc_`var': mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HHIncT `var'
estimates store objinc_`var'
}
foreach var in viewwt_v3 {
display "`var'"
eststo objinc_`var': mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HHIncT i.`var'
estimates store objinc_`var'
}
*and then one adjusted for everything:
eststo objinc_all: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.HHIncT rescale_bmi i.viewwt_v3 happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst
estimates store objinc_all

coefplot objinc_base objinc_rescale_bmi objinc_viewwt_v3 objinc_happywtr objinc_DisNew2 objinc_zbio objinc_zstruc objinc_zObHlth_cons objinc_zleftr objinc_zliba objinc_zwelfst objinc_all, ///
keep (*HHIncT*) xline(0) asequation swapnames ///
rename(1.HHIncT = "lowest" 2.HHIncT ="middle" 3.HHIncT ="highest") ///
eqrename (objinc_base = "Adjusted for: age, gender" ///
objinc_rescale_bmi = "+ own BMI" ///
objinc_viewwt_v3 ="+ view of own weight" ///
objinc_happywtr = "+ happiness with own weight" ///
objinc_DisNew2 = "+ longstanding illness/disability" ///
objinc_zbio = "+ importance of inheritance/metabolism" ///
objinc_zstruc = "+ importance of structural factors" ///
objinc_zObHlth_cons = "+ perceived health consequences" ///
objinc_zleftr = "+ left-right" ///
objinc_zliba = "+ libertarian-authoritarian" ///
objinc_zwelfst = "+ welfare-stigmatizing" ///
objinc_all = "+ full adjustment", regex) ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 8: Weight-stigmatizing attitudes by household income tercile*", pos(11) size(small)) ///
note ("*reference category: lowest tercile. Indexes are standardized.", size(vsmall))
*save it
graph save "results/graphs/coefplot_objinc_mediation_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_objinc_mediation_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_objinc_mediation_zwtstig.tif, replace width(800)


******************************************************
*export results: obj income only
capture erase "results/imputed_objinc_attenuation_zwtstig.xls"
capture erase "results/imputed_objinc_attenuation_zwtstig.txt"
*include reference groups, as useful to have in table in manuscript
estout using "results/imputed_objinc_attenuation_zwtstig.xls", cells ("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") ///
keep(*HHIncT*) ///
replace title(Household income tercile associations with index: attenuation with adjustment) ///
note("All models include age and gender") 
*will need to transpose....

********************************************************************************************************************************************
*SUBJECTIVE INCOME:

eststo clear

*unadjusted difference:
eststo subjinc_base: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.SRInc_REV
estimates store subjinc_base
*then observe attenuation:
foreach var in rescale_bmi happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
display "`var'"
eststo subjinc_`var': mi estimate, post: regress zwtstig i.Rage_cat Rsex i.SRInc_REV `var'
estimates store subjinc_`var'
}
foreach var in viewwt_v3 {
display "`var'"
eststo subjinc_`var': mi estimate, post: regress zwtstig i.Rage_cat Rsex i.SRInc_REV i.`var'
estimates store subjinc_`var'
}
*and then one adjusted for everything:
eststo subjinc_all: mi estimate, post: regress zwtstig i.Rage_cat Rsex i.SRInc_REV rescale_bmi i.viewwt_v3 happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst
estimates store subjinc_all

coefplot subjinc_base subjinc_rescale_bmi subjinc_viewwt_v3 subjinc_happywtr subjinc_DisNew2 subjinc_zbio subjinc_zstruc subjinc_zObHlth_cons subjinc_zleftr subjinc_zliba subjinc_zwelfst subjinc_all, ///
keep (*SRInc_REV*) xline(0) asequation swapnames ///
rename(1.SRInc_REV = "low" 2.SRInc_REV ="middle" 3.SRInc_REV ="high") ///
eqrename (subjinc_base = "Adjusted for: age, gender" ///
subjinc_rescale_bmi = "+ own BMI" ///
subjinc_viewwt_v3 ="+ view of own weight" ///
subjinc_happywtr = "+ happiness with own weight" ///
subjinc_DisNew2 = "+ longstanding illness/disability" ///
subjinc_zbio = "+ importance of inheritance/metabolism" ///
subjinc_zstruc = "+ importance of structural factors" ///
subjinc_zObHlth_cons = "+ perceived health consequences" ///
subjinc_zleftr = "+ left-right" ///
subjinc_zliba = "+ libertarian-authoritarian" ///
subjinc_zwelfst = "+ welfare-stigmatizing" ///
subjinc_all = "+ full adjustment", regex) ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend(off) grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig 9: Weight-stigmatizing attitudes by subjective income*", pos(11) size(small)) ///
note ("*reference category: 'low' subjective income. Indexes are standardized", size(vsmall))
*save it
graph save "results/graphs/coefplot_subjinc_mediation_zwtstig.gph", replace
*capture graph export results/graphs/coefplot_subjinc_mediation_zwtstig.tif, replace width(1200)
capture graph export results/graphs/coefplot_subjinc_mediation_zwtstig.tif, replace width(800)


******************************************************
*export results: subj income only
capture erase "results/imputed_subjinc_attenuation_zwtstig.xls"
capture erase "results/imputed_subjinc_attenuation_zwtstig.txt"
*include reference groups, as useful to have in table in manuscript
estout using "results/imputed_subjinc_attenuation_zwtstig.xls", cells ("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") ///
keep(*SRInc_REV*) ///
replace title(Subjective income tercile associations with index: attenuation with adjustment) ///
note("All models include age and gender") 
*will need to transpose....

*******************************************************************************************************

*SECONDARY OUTCOME: ologit regression models.

capture log close
log using "regressionresults_owtmarr.log", replace

*regression models:

eststo clear

*base model: age and sex only
eststo base: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex 
*women more stigmatizing!
*here, linear age effect, positive association

*ETHNICITY:
eststo ethnicity: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex i.RaceOri3_v2
*trend towards differences

*SEP measures in turn:
eststo HEdQual_3grp: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex i.HEdQual_3grp
eststo HHIncT: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex i.HHIncT
eststo SRInc_REV: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex i.SRInc_REV 
eststo RNSSECG_5grp: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex i.RNSSECG_5grp
eststo REconAct_v3: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex i.REconAct_v3

*own bmi?
eststo rescale_bmi: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex rescale_bmi

*own attitudes towards own weight?
eststo viewwt_v3: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex i.viewwt_v3

eststo happywtr: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex happywtr

*long-term health condition/disability?
eststo DisNew2: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex i.DisNew2

*beliefs about causes:
*biology vs choice:
eststo zbio: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex zbio
*structural factors important:
eststo zstruc: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex zstruc
*consequences:
eststo zObHlth_cons: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex zObHlth_cons

**political/social:
eststo zleftr: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex zleftr
eststo zliba: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex zliba
eststo zwelfst: mi estimate, post or: ologit owtmarr_3grp i.Rage_cat Rsex zwelfst

*export in one file:
capture erase "results/imputed_main_effects_owtmarr.xls"
capture erase "results/imputed_main_effects_owtmarr.txt"
*include reference groups, as useful to have in table in manuscript
estout using "results/imputed_main_effects_owtmarr.xls", eform cells ("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") ///
keep(*Rage* *Rsex *RaceOri3_v2 *HEdQual_3grp *HHIncT *SRInc_REV *RNSSECG_5grp /*no 2.RNSSECG_5grp group, as flipped over to be baseline*/ *REconAct_v3 rescale_bmi *viewwt_v3 happywtr *DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst) ///
replace title(Associations of demographic, socioeconomic, health-related and attitudinal factors with secondary outcome) ///
note("Odds ratios, from ordered logistic regression. All models include age and gender") 

log close

***************************************************************************************************************************

*ENTRY POINT: sensitivity analysis: each item in the index

clear all
cd "BSAS_project"
use "bsas_50imps.dta", clear

*because of the coding, you need to reverse the two which aren't intuitive:
mi passive: gen owtlazy_REV=6-owtlazy 
mi passive: gen owtlose_REV=6-owtlose 

*then standardize all index items:
foreach var in owtlazy_REV owtlose_REV owtnhs owtcare {
mi passive: egen z`var'=std(`var')
}

*relabel everything as you want it to appear on the graph:
label variable Rage_cat "Age categories"
*label variable cRage "age (years)"
*label variable cRagesq "age (years), squared"
label variable Rsex "gender: women vs men"
label variable RaceOri3_v2 "ethnic group (ref: white)"
label variable HEdQual_3grp "educational qualifications"
label variable SRInc_REV "subjective income"
label variable rescale_bmi "own BMI (per 5kg/m2)"
label variable DisNew2 "has long-standing illness/disability"
label variable viewwt_v3 "perception of own weight"
label variable happywtr "happiness with own weight"
label variable zbio "importance of inheritance/metabolism"
label variable zstruc "importance of structural factors"
label variable zObHlth_cons "health consequences of obesity"
label variable zleftr "left-right index (higher values: rightwing)"
label variable zliba "libertarian-authoritarian index (higher values: authoritarian)"
label variable zwelfst "welfare-stigmatizing index"

*and the outcomes:
label variable zowtlazy_REV "most obese people are lazy" 
label variable zowtlose_REV "most obese people could lose weight if they tried" 
label variable zowtnhs "obese people should have same rights to expensive NHS treatment" 
label variable zowtcare "obese people care about appearance as much as anyone else"

eststo clear

*base model: age and sex only
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_base: mi estimate, post: regress `out' i.Rage_cat Rsex 
}
*graph it:
coefplot *_base, /// 
keep (*Rage* Rsex) xline(0) ///
eqrename (zowtlazy_REV_base = "most obese people lazy"  ///
zowtlose_REV_base = "could lose weight if tried" ///
zowtnhs_base = "same rights to NHS treatment" ///
zowtcare_base = "care about appearance") ///
coeflabels (,wrap(40) labsize(vsmall)) ///
legend( label (2 "most obese people lazy") label (4 "could lose weight if tried") label (6 "same rights to NHS treatment") label (8 "care about appearance") size(vsmall)) ///
grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig S1: Demographic differences in index items*", pos(11) size(small)) ///
note ("*All models include age and gender. Outcomes and indexes are standardized", size(vsmall))
graph save "results/graphs/coefplot_demographic_index_items.gph", replace
*capture graph export results/graphs/coefplot_demographic_index_items.tif, replace width(1200)
capture graph export results/graphs/coefplot_demographic_index_items.tif, replace width(800)

*then for the rest, loop through predictors, in each case looking at the outcome items in turn.
*models have slight differences, but graph part can be in a single loop.

*ethnicity:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_RaceOri3_v2: mi estimate, post: regress `out' i.Rage_cat Rsex i.RaceOri3_v2
}
*education:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_HEdQual_3grp: mi estimate, post: regress `out' i.Rage_cat Rsex i.HEdQual_3grp
}
*objective income terciles:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_HHIncT: mi estimate, post: regress `out' i.Rage_cat Rsex i.HHIncT
}
*subjective income:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_SRInc_REV: mi estimate, post: regress `out' i.Rage_cat Rsex i.SRInc_REV 
}
*NS-SEC
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_RNSSECG_5grp: mi estimate, post: regress `out' i.Rage_cat Rsex i.RNSSECG_5grp
}
*empstat:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_REconAct_v3: mi estimate, post: regress `out' i.Rage_cat Rsex i.REconAct_v3
}
*own rescale_bmi:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_rescale_bmi: mi estimate, post: regress `out' i.Rage_cat Rsex rescale_bmi
}
*viewwt_v3:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_viewwt_v3: mi estimate, post: regress `out' i.Rage_cat Rsex i.viewwt_v3
}
*happywtr:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_happywtr: mi estimate, post: regress `out' i.Rage_cat Rsex happywtr
}
*long-term illness/disability:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_DisNew2: mi estimate, post: regress `out' i.Rage_cat Rsex i.DisNew2
}
*biology vs choice:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_zbio: mi estimate, post: regress `out' i.Rage_cat Rsex zbio
}
*structural factors:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_zstruc: mi estimate, post: regress `out' i.Rage_cat Rsex zstruc
}
*health consequences:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_zObHlth_cons: mi estimate, post: regress `out' i.Rage_cat Rsex zObHlth_cons
}
*left-right:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_zleftr: mi estimate, post: regress `out' i.Rage_cat Rsex zleftr
}
*lib-auth:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_zliba: mi estimate, post: regress `out' i.Rage_cat Rsex zliba
}
*welfst:
foreach out in zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
eststo `out'_zwelfst: mi estimate, post: regress `out' i.Rage_cat Rsex zwelfst
}

*Now graph them all individually.
*Repeat for each outcome, pulling graph titles from macros defined here:
global  RaceOri3_v2_title = "Ethnicity:"
global HEdQual_3grp_title = "Education:"
global HHIncT_title = "Objective household income:"
global SRInc_REV_title = "Subjective income:"
global RNSSECG_5grp_title = "NSSSEC:"
global REconAct_v3_title = "Employment status:"
global rescale_bmi_title = "Own BMI (per 5kg/m2):"
global happywtr_title = "Happiness with own weight:"
global DisNew2_title = "Long-term illness/disability:"
global viewwt_v3_title = "Perception of own weight:"
global zbio_title "Importance of inheritance/metabolism:"
global zstruc_title "Importance of structural factors:"
global zObHlth_cons_title "Perceived health consequences:"
global zleftr_title "Left-right index:"
global zliba_title "Libertarian-authoritarian index:"
global zwelfst_title "Welfare-stigmatizing index:"

*graph it:
local k=1
foreach var in RaceOri3_v2 HEdQual_3grp HHIncT SRInc_REV RNSSECG_5grp REconAct_v3 rescale_bmi viewwt_v3 happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst  {
local k=`k'+1
coefplot *`var'*, /// 
keep (*`var'*) xline(0) ///
asequation swapnames ///
rename (1.RaceOri3_v2 =  "white" 2.RaceOri3_v2 = "Black" 3.RaceOri3_v2 = "Asian" 4.RaceOri3_v2 = "mixed/other" ///
rename(1.HEdQual_3grp "university degree" 2.HEdQual_3grp ="quals below degree" 3.HEdQual_3grp = "no quals") ///
rename(1.HHIncT = "lowest" 2.HHIncT ="middle" 3.HHIncT ="highest") ///
rename(1.SRInc_REV = "low" 2.SRInc_REV ="middle" 3.SRInc_REV ="high") ///
rename(1.RNSSECG_5grp = "NSSEC: 1 & 2" 2.RNSSECG_5grp = "NSSEC 3" 3.RNSSECG_5grp = "4" 4.RNSSECG_5grp ="5" 5.RNSSECG_5grp ="6 & 7" 6.RNSSECG_5grp ="unclassifiable") ///
rename(1.REconAct_v3 = "employed" 2.REconAct_v3 ="unemployed" 3.REconAct_v3="sick/disabled" 4.REconAct_v3="other") ///
0.viewwt_v3 =  "about right weight" 1.viewwt_v3 = "underweight" 2.viewwt_v3 = "a bit overweight" 3.viewwt_v3 = "very overweight") ///
eqrename (zowtlazy_REV_`var' = "most obese people lazy"  ///
zowtlose_REV_`var' = "could lose weight if tried" ///
zowtnhs_`var' = "same rights to NHS treatment" ///
zowtcare_`var' = "care about appearance") ///
coeflabels (,wrap(30) labsize(vsmall)) /// 
legend(off) ///
grid(none) scheme (s1mono) graphregion (color(white)) ///
title ("{bf}Fig S`k': $`var'_title differences in index items", pos(11) size(small))  ///
note ("*All models include age and gender. Outcomes and indexes are standardized", size(vsmall))
graph save "results/graphs/coefplot_`var'_index_items.gph", replace
capture graph export results/graphs/coefplot_`var'_index_items.tif, replace width(1200)
}

*NB: code for a legend if needed is:
*legend( label (2 "most obese people lazy") label (4 "could lose weight if tried") label (6 "same rights to NHS treatment") label (8 "care about appearance") size(vsmall)) ///

*EXPORT COEFFICIENTS:
*individual items:
*export the coefficients in one file:
*do this in the order you want it to be in the table! makes life much easier with formatting:
capture erase "results/imputed_index_items.xls"
capture erase "results/imputed_index_items.txt"
*include reference groups, as useful to have in table in manuscript
estout using "results/imputed_index_items.xls", cells ("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") ///
keep(*Rage* *Rsex *RaceOri3_v2 *HEdQual_3grp *HHIncT *SRInc_REV *RNSSECG_5grp *REconAct_v3 rescale_bmi *viewwt_v3 happywtr *DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst) ///
replace title(Associations of demographic, socioeconomic, health-related and attitudinal factors with individual index items) ///
note ("*All models include age and gender. Outcomes and indexes are standardized", size(vsmall))

*******************************************************************************************

*ENTRY POINT: HEATMAP ATTEMPT for each item in the index

clear all
cd "BSAS_project"
use "bsas_50imps.dta", clear

*because of the coding, you need to reverse the two which aren't intuitive:
mi passive: gen owtlazy_REV=6-owtlazy 
mi passive: gen owtlose_REV=6-owtlose 

*then standardize all index items:
foreach var in owtlazy_REV owtlose_REV owtnhs owtcare {
mi passive: egen z`var'=std(`var')
}

*save regression coeffiients in a matrix:
matrix itemcoeffs = J(36,5,.)
matrix list itemcoeffs

*age:
*define starting row and column:
local r=1
local c=1
local s=`r'+1
local t=`r'+2
local u=`r'+3
local v=`r'+4
local w=`r'+5
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex 
matrix itemcoeffs [`r',`c']=_b[2.Rage_cat]
matrix itemcoeffs [`s',`c']=_b[3.Rage_cat]
matrix itemcoeffs [`t',`c']=_b[4.Rage_cat]
matrix itemcoeffs [`u',`c']=_b[5.Rage_cat]
matrix itemcoeffs [`v',`c']=_b[6.Rage_cat]
matrix itemcoeffs [`w',`c']=_b[7.Rage_cat]
local c=`c'+1
}

*gender:
*define starting row and column:
local r=7
local c=1
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex 
matrix itemcoeffs [`r',`c']=_b[Rsex]
local c=`c'+1
}

*ethnicity:
*define starting row and column, other relevant rows:
local r=8
local c=1
local s=`r'+1
local t=`r'+2
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex i.RaceOri3_v2
matrix itemcoeffs [`r',`c']=_b[2.RaceOri3_v2]
matrix itemcoeffs [`s',`c']=_b[3.RaceOri3_v2]
matrix itemcoeffs [`t',`c']=_b[4.RaceOri3_v2]
local c=`c'+1
}

*loop through 3-group socieoconomic vars.
*define starting row for this block
local r=11
foreach var in HEdQual_3grp HHIncT SRInc_REV {
*reset to left-hand side, pick out second relevant rows:
local c=1
local s=`r'+1
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex i.`var'
matrix itemcoeffs [`r',`c']=_b[2.`var']
matrix itemcoeffs [`s',`c']=_b[3.`var']
local c=`c'+1
}
local r=`r'+2
}
*NS-SEC
*define starting row for this block
local r=17
local c=1
local s=`r'+1
local t=`r'+2
local u=`r'+3
local v=`r'+4
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex i.RNSSECG_5grp
matrix itemcoeffs [`r',`c']=_b[2.RNSSECG_5grp]
matrix itemcoeffs [`s',`c']=_b[3.RNSSECG_5grp]
matrix itemcoeffs [`t',`c']=_b[4.RNSSECG_5grp]
matrix itemcoeffs [`u',`c']=_b[5.RNSSECG_5grp]
matrix itemcoeffs [`v',`c']=_b[6.RNSSECG_5grp]
local c=`c'+1
}

*empstat:
*define starting row for this block
local r=22
local c=1
local s=`r'+1
local t=`r'+2
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex i.REconAct_v3
matrix itemcoeffs [`r',`c']=_b[2.REconAct_v3]
matrix itemcoeffs [`s',`c']=_b[3.REconAct_v3]
matrix itemcoeffs [`t',`c']=_b[4.REconAct_v3]
local c=`c'+1
}

*own rescale_bmi:
*define starting row for this block
local r=25
local c=1
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex rescale_bmi
matrix itemcoeffs [`r',`c']=_b[rescale_bmi]
local c=`c'+1
}
*viewwt_v3: NB: this one coded differently! categs start at 0
*define starting row for this block
local r=26
local c=1
local s=`r'+1
local t=`r'+2
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex i.viewwt_v3
matrix itemcoeffs [`r',`c']=_b[1.viewwt_v3]
matrix itemcoeffs [`s',`c']=_b[2.viewwt_v3]
matrix itemcoeffs [`t',`c']=_b[3.viewwt_v3]
local c=`c'+1
}

*the rest can be looped through:
*define starting row for the block:
local r=29
foreach var in happywtr DisNew2 zbio zstruc zObHlth_cons zleftr zliba zwelfst {
local c=1
foreach out in zwtstig zowtlazy_REV zowtlose_REV zowtnhs zowtcare {
mi estimate, post: regress `out' i.Rage_cat Rsex `var'
matrix itemcoeffs [`r',`c']=_b[`var']
local c=`c'+1
}
local r=`r'+1
}
*check
matrix list itemcoeffs

*label the columns and rows
matrix colnames itemcoeffs = index lazy lose nhs care
matrix rownames itemcoeffs = "Age - 30-39" "Age - 40-49" "Age - 50-59" "Age - 60-69" "Age - 70-79" "Age - 80+" ///
"Gender - women" "Ethnicity - Black" "Ethnicity - Asian" "Ethnicity - mixed/other" ///
"Education - quals below degree" "Education - no qualifications" ///
"Objective income - medium" "Objective income - high" ///
"Subjective income - medium" "Subjective income - high" ///
"RNSSEC - 2" "RNSSEC - 3" "RNSSEC - 4" "RNSSEC - 5" "RNSSEC - unclassifiable" ///
"Employment - unemployed" "Employment - sick/disabled" "Employment - other" ///
"Own BMI (per 5kg/m2)" ///
"Sees self as underweight" "Sees self as a bit overweight" "Sees self as very overweight" ///
"Happiness with own weight, 1-5" ///
"Has long-term illness/disability" ///
"Importance of inheritance" "Importance of structural factors" "Health consequences of obesity" "Left-right values" "Libertarian-authoritarian values" "Welfare-stigmatizing attitudes"
*check
matrix list itemcoeffs

*heatplot:
heatplot itemcoeffs, values(format(%9.2f)) ylabel(, labsize(vsmall)) xlabel(, labsize(small)) xscale(alt) colors(plasma) ///
aspectratio(1) legend(off) graphregion (color(white)) ///
title ("{bf}Figure 10: Associations with the index and its individual items", pos(11) size(small)) ///
note("*All models include age and gender. Indexes are standardized", size(vsmall)) 

graph save "results/graphs/heatplot_index_items.gph", replace
*graph export results/graphs/heatplot_index_items.tif, replace width(1200)
graph export results/graphs/heatplot_index_items.tif, replace width(800)
