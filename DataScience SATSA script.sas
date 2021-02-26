*Frequency documentation and recoding/rescaling sleep variables for harmonization;
*Author(s): Tina Vo:
*Created on: 21Aug2020;
*Notes:  
1-Checking SATSA 21aug2020;

PROC IMPORT DATAFILE = '\\138.23.174.169\FileShare\GxE\Work\GE in-progress\Sleep\Data\Derived\SATSAsleep_21aug2020_3' OUT = SATSAs
DBMS = sav REPLACE;
RUN;

PROC CONTENTS DATA = SATSAs VARNUM;
RUN;
/*****************************************
***********SATSA*************************
******************************************/

/*****converting SATSA sleep duration variable***/
title 'frequency for SATSA sleep duration variables';
PROC Freq data=SATSAs;
tables ASLEEPN_S /*I go to sleep at*/ ASLEEPM_S /*wake at*/ BSLEEPN	BSLEEPN_T /*I got to sleep at*/	BSLEEPM	BSLEEPM_T /*wake at*/
ZSLEEPN ZSLEEPM;
run;

proc contents data=SATSAs;
run;

/*example code
week_num=scan(UniqueID,2,'_'); change underscore to hyphen. hours, minutes.

full_id=cats(ID, "_",week_num);

data baseline_renamed2; set baseline_renamed;
Array_var_list{*} LsrAct;
Do _i=1 to dim (_var_list);
-var_list{_i} =tranwrd(_var_list{_i}, "Ahat", " ");
end;
drop _i;
run;*/
/*-------------------------------------------------------------------------------*/
/*example code found online 
data want;
  set have;
  new_time=input(cats(char_time,"00"),hhmmss.);
  format new_time time5.;
run;



/*set string to numeric for ASLEEP and ZSLEEP, BSLEEP already numeric*/
title 'String to Numeric';
data SATSA_num; set SATSAs;
Abedtime= input (ASLEEPN_Src, time10.);
Awaketime= input (ASLEEPM_Src, time10.);
Zbedtime= input (ZSLEEPNrc, time10.);
Zwaketime= input (ZSLEEPMrc, time10.);
run;

proc contents data= SATSA_num;
run;


title 'Renaming BSLEEP, convert to seconds';
/*renaming B sleep variables to be consistent with A and Z sleep items*/
data SATSA_num1; set SATSA_num;
Bbedtime=BSLEEPN_T;
Bwaketime=BSLEEPM_T;
if Bbedtime=0 then Bbedtime=86400;
run;

title 'Calculating sleep duration based on bedtime and waketime';
/*calculate sleep duration based on waketime and bedtime*/
data SATSA_num2; set SATSA_num1;
A_hrs_slept = mod(24+(Awaketime-Abedtime)/3600,24);
B_hrs_slept = mod(24+(Bwaketime-Bbedtime)/3600,24);
Z_hrs_slept = mod(24+(Zwaketime-Zbedtime)/3600,24);
run;

title 'frequency check for sleep duration';
proc freq data=SATSA_num2;
tables A_hrs_slept B_hrs_slept Z_hrs_slept;
run;

title 'SLEEPRX 0=no endorsement, 1=some endorsement';
/*rescaling sleepRX 0=no endorsement, 1=yes/some endorsement*/
data SATSA_3; set SATSA_num2;
if GSLEEPRX=1 then GSLEEPRX=0;
if GSLEEPRX=2 then GSLEEPRX=1;
if HSLEEPRX=1 then HSLEEPRX=0;
if HSLEEPRX=2 then HSLEEPRX=1;
if QSLEEPRX=1 then QSLEEPRX=0;
if QSLEEPRX=2 then QSLEEPRX=1;
if XSLEEPRX=1 then XSLEEPRX=0;
if XSLEEPRX=2 then XSLEEPRX=1;
if USLEEPRX=1 then USLEEPRX=0;
if USLEEPRX=2 then USLEEPRX=1;
if WSLEEPRX=1 then WSLEEPRX=0;
if WSLEEPRX=2 then WSLEEPRX=1;
if SSLEEPRX=2 then SSLEEPRX=.;
if FSLEEPRX=1 then FSLEEPRX=0;
if FSLEEPRX=2 then FSLEEPRX=1;
IPT1SLEEPRX=CLASSX;
run;

title 'SLEEPRX Means';
proc means data=SATSA_3;
vars SLEEPRX VSLEEPRX GSLEEPRX HSLEEPRX JSLEEPRX QSLEEPRX 
XSLEEPRX RSLEEPRX SSLEEPRX USLEEPRX WSLEEPRX SLEEPRX FSLEEPRX IPT1SLEEPRX;
run;

title 'Sleep Latency "no endorse=0/some endorse=1"';
/*SATSA sleep latency original 1=no endorsement (coded as zero), original 2-5=some endorsement (coded as 1)*/
data SATSA_4; set SATSA_3;
if ASLEEP5=1 then ASLEEP5_h=0;
if ASLEEP5 ge 2 then ASLEEP5_h=1;
/*creating a new variable RC for rescaling. 1&2=0 2-5=1*/
if 1<=ASLEEP5<=2 then ASLEEP5_RC=0;
if ASLEEP5 ge 3 then ASLEEP5_RC=1;

if BSLEEP5=1 then BSLEEP5_h=0;
if BSLEEP5 ge 2 then BSLEEP5_h=1;
if 1<=BSLEEP5<=2 then BSLEEP5_RC=0;
if BSLEEP5 ge 3 then BSLEEP5_RC=1;

if ZSLEEP5=1 then ZSLEEP5_h=0;
if ZSLEEP5 ge 2 then ZSLEEP5_h=1;
if 1<=ZSLEEP5<=2 then ZSLEEP5_RC=0;
if ZSLEEP5 ge 3 then ZSLEEP5_RC=1;

if E60=2 then E60=0;
if E60=3 then E60=.;
run;

title 'Sleep latency means';
PROC MEANS data=SATSA_4;
vars ASLEEP5_h ASLEEP5_RC BSLEEP5_h BSLEEP5_RC ZSLEEP5_h ZSLEEP5_RC E60;
run;
PROC freq data=SATSA_4;
tables ASLEEP5_h ASLEEP5_RC BSLEEP5_h BSLEEP5_RC ZSLEEP5_h ZSLEEP5_RC E60;
run;


title 'creating sleep categories 0=short, 1=ref, 2=long for SATSA';
data SATSA_sleepcat; set SATSA_4;

if A_hrs_slept lt 6 and A_hrs_slept gt 0 then slp_ave_cat1=0;
if A_hrs_slept gt 10 then slp_ave_cat1=2;
if 6<=A_hrs_slept<=10 then slp_ave_cat1=1;


if B_hrs_slept lt 6 and B_hrs_slept gt 0 then slp_ave_cat2=0;
if B_hrs_slept gt 10 then slp_ave_cat2=2;
if 6<=B_hrs_slept<=10 then slp_ave_cat2=1;


if Z_hrs_slept lt 6 and Z_hrs_slept gt 0 then slp_ave_cat3=0;
if Z_hrs_slept gt 10 then slp_ave_cat3=2;
if 6<=Z_hrs_slept<=10 then slp_ave_cat3=1;
run;

title 'frequency for SATSA sleep categories';
proc freq data=SATSA_sleepcat;
tables slp_ave_cat1 slp_ave_cat2 slp_ave_cat3;
run;

title 'sleep disturbance items 0=no endorsement, 1=some endorsement';
proc freq data=SATSA_sleepcat;
tables VCESDK GCESDK HCESDK JCESDK KCESDK LCESDK ACESDK
BCESDK QCESDK XCESDK FCESDK RCESDK SCESDK UCESDK WCESDK 
YCESDK ZCESDK ICESDK ASLEEP3 ASLEEP1 ASLEEP4 ASLEEP8
ASLEEP10 QM6 ZSLEEP1 ZSLEEP3 ZSLEEP4 ZSLEEP8 ZSLEEP10
M6 VM6 GM6;
run;

title 'means for SATSA sleep disturbance items';
proc means data=SATSA_sleepcat;
vars VCESDK GCESDK HCESDK JCESDK KCESDK LCESDK ACESDK
BCESDK QCESDK XCESDK FCESDK RCESDK SCESDK UCESDK WCESDK
YCESDK ZCESDK ICESDK ASLEEP3 ASLEEP1 ASLEEP4 ASLEEP8
ASLEEP10 QM6 ZSLEEP1 ZSLEEP3 ZSLEEP4 ZSLEEP8 ZSLEEP10
BSLEEP1 BSLEEP3 BSLEEP4 BSLEEP8 BSLEEP10
M6 VM6 GM6;
run;

data SATSA_sleepcat3; set SATSA_sleepcat;
/*made another variable with a rescaling (vars w/ RC instead of h) instead of 1=0 & 2-4=1, changed it to 1&2=0 "never and rarely" & 3-4=1*/
if VCESDK=1 then VCESDK_h=0; /*I did not sleep well*/
if VCESDK ge 2 then VCESDK_h=1;
if 1<=VCESDK<=2 then VCESDK_RC=0;
if VCESDK ge 3 then VCESDK_RC=1;


if GCESDK=1 then GCESDK_h=0;
if GCESDK ge 2 then GCESDK_h=1;
if 1<=GCESDK<=2 then GCESDK_RC=0;
if GCESDK ge 3 then GCESDK_RC=1;

if HCESDK=1 then HCESDK_h=0;
if HCESDK ge 2 then HCESDK_h=1;
if 1<=HCESDK<=2 then HCESDK_RC=0;
if HCESDK ge 3 then HCESDK_RC=1;


if JCESDK=1 then JCESDK_h=0;
if JCESDK ge 2 then JCESDK_h=1;
if 1<=JCESDK<=2 then JCESDK_RC=0;
if JCESDK ge 3 then JCESDK_RC=1;


if KCESDK=1 then KCESDK_h=0;
if KCESDK ge 2 then KCESDK_h=1;
if 1<=KCESDK<=2 then KCESDK_RC=0;
if KCESDK ge 3 then KCESDK_RC=1;


if LCESDK=1 then LCESDK_h=0;
if LCESDK ge 2 then LCESDK_h=1;
if 1<=LCESDK<=2 then LCESDK_RC=0;
if LCESDK ge 3 then LCESDK_RC=1;


if ACESDK=1 then ACESDK_h=0;
if ACESDK ge 2 then ACESDK_h=1;
if 1<=ACESDK<=2 then ACESDK_RC=0;
if ACESDK ge 3 then ACESDK_RC=1;


if BCESDK=1 then BCESDK_h=0;
if BCESDK ge 2 then BCESDK_h=1;
if 1<=BCESDK<=2 then BCESDK_RC=0;
if BCESDK ge 3 then BCESDK_RC=1;


if QCESDK=1 then QCESDK_h=0;
if QCESDK ge 2 then QCESDK_h=1;
if 1<=QCESDK<=2 then QCESDK_RC=0;
if QCESDK ge 3 then QCESDK_RC=1;


if XCESDK=1 then XCESDK_h=0;
if XCESDK ge 2 then XCESDK_h=1;
if 1<=XCESDK<=2 then XCESDK_RC=0;
if XCESDK ge 3 then XCESDK_RC=1;


if FCESDK=1 then FCESDK_h=0;
if FCESDK ge 2 then FCESDK_h=1;
if 1<=FCESDK<=2 then FCESDK_RC=0;
if FCESDK ge 3 then FCESDK_RC=1;

if RCESDK=1 then RCESDK_h=0;
if RCESDK ge 2 then RCESDK_h=1;
if 1<=RCESDK<=2 then RCESDK_RC=0;
if RCESDK ge 3 then RCESDK_RC=1;


if SCESDK=1 then SCESDK_h=0;
if SCESDK ge 2 then SCESDK_h=1;
if 1<=SCESDK<=2 then SCESDK_RC=0;
if SCESDK ge 3 then SCESDK_RC=1;


if UCESDK=1 then UCESDK_h=0;
if UCESDK ge 2 then UCESDK_h=1;
if 1<=UCESDK<=2 then UCESDK_RC=0;
if UCESDK ge 3 then UCESDK_RC=1;

if WCESDK=1 then WCESDK_h=0;
if WCESDK ge 2 then WCESDK_h=1;
if 1<=WCESDK<=2 then WCESDK_RC=0;
if WCESDK ge 3 then WCESDK_RC=1;


if YCESDK=1 then YCESDK_h=0;
if YCESDK ge 2 then YCESDK_h=1;
if 1<=YCESDK<=2 then YCESDK_RC=0;
if YCESDK ge 3 then YCESDK_RC=1;


if ZCESDK=1 then ZCESDK_h=0;
if ZCESDK ge 2 then ZCESDK_h=1;
if 1<=ZCESDK<=2 then ZCESDK_RC=0;
if ZCESDK ge 3 then ZCESDK_RC=1;


if ICESDK=1 then ICESDK_h=0;
if ICESDK ge 2 then ICESDK_h=1;
if 1<=ICESDK<=2 then ICESDK_RC=0;
if ICESDK ge 3 then ICESDK_RC=1;

/*made another variable with a rescaling (vars w/ RC instead of h) instead of 1=0 & 2-5=1, changed it to 1&2=0 "never and rarely" & 3-5=1*/
if ASLEEP3=1 then ASLEEP3_h=0;
if ASLEEP3 ge 2 then ASLEEP3_h=1;
if 1<=ASLEEP3<=2 then ASLEEP3_RC=0;
if ASLEEP3 ge 3 then ASLEEP3_RC=1;

if ASLEEP1=1 then ASLEEP1_h=0;
if ASLEEP1 ge 2 then ASLEEP1_h=1;
if 1<=ASLEEP1<=2 then ASLEEP1_RC=0;
if ASLEEP1 ge 3 then ASLEEP1_RC=1;


if ASLEEP4=1 then ASLEEP4_h=0;
if ASLEEP4 ge 2 then ASLEEP4_h=1;
if 1<=ASLEEP4<=2 then ASLEEP4_RC=0;
if ASLEEP4 ge 3 then ASLEEP4_RC=1;


if ASLEEP8=1 then ASLEEP8_h=0;
if ASLEEP8 ge 2 then ASLEEP8_h=1;
if 1<=ASLEEP8<=2 then ASLEEP8_RC=0;
if ASLEEP8 ge 3 then ASLEEP8_RC=1;


if ASLEEP10=1 then ASLEEP10_h=0;
if ASLEEP10 ge 2 then ASLEEP10_h=1;
if 1<=ASLEEP10<=2 then ASLEEP10_RC=0;
if ASLEEP10 ge 3 then ASLEEP10_RC=1;

if ZSLEEP1=1 then ZSLEEP1_h=0;
if ZSLEEP1 ge 2 then ZSLEEP1_h=1;
if 1<=ZSLEEP1<=2 then ZSLEEP1_RC=0;
if ZSLEEP1 ge 3 then ZSLEEP1_RC=1;


if ZSLEEP3=1 then ZSLEEP3_h=0;
if ZSLEEP3 ge 2 then ZSLEEP3_h=1;
if 1<=ZSLEEP3<=2 then ZSLEEP3_RC=0;
if ZSLEEP3 ge 3 then ZSLEEP3_RC=1;

if ZSLEEP4=1 then ZSLEEP4_h=0;
if ZSLEEP4 ge 2 then ZSLEEP4_h=1;
if 1<=ZSLEEP4<=2 then ZSLEEP4_RC=0;
if ZSLEEP4 ge 3 then ZSLEEP4_RC=1;

if ZSLEEP8=1 then ZSLEEP8_h=0;
if ZSLEEP8 ge 2 then ZSLEEP8_h=1;
if 1<=ZSLEEP8<=2 then ZSLEEP8_RC=0;
if ZSLEEP8 ge 3 then ZSLEEP8_RC=1;

if ZSLEEP10=1 then ZSLEEP10_h=0;
if ZSLEEP10 ge 2 then ZSLEEP10_h=1;
if 1<=ZSLEEP10<=2 then ZSLEEP10_RC=0;
if ZSLEEP10 ge 3 then ZSLEEP10_RC=1;

if BSLEEP1=1 then BSLEEP1_h=0;
if BSLEEP1 ge 2 then BSLEEP1_h=1;
if 1<=BSLEEP1<=2 then BSLEEP1_RC=0;
if BSLEEP1 ge 3 then BSLEEP1_RC=1;

if BSLEEP3=1 then BSLEEP3_h=0;
if BSLEEP3 ge 2 then BSLEEP3_h=1;
if 1<=BSLEEP3<=2 then BSLEEP3_RC=0;
if BSLEEP3 ge 3 then BSLEEP3_RC=1;

if BSLEEP4=1 then BSLEEP4_h=0;
if BSLEEP4 ge 2 then BSLEEP4_h=1;
if 1<=BSLEEP4<=2 then BSLEEP4_RC=0;
if BSLEEP4 ge 3 then BSLEEP4_RC=1;


if BSLEEP8=1 then BSLEEP8_h=0;
if BSLEEP8 ge 2 then BSLEEP8_h=1;
if 1<=BSLEEP8<=2 then BSLEEP8_RC=0;
if BSLEEP8 ge 3 then BSLEEP8_RC=1;


if BSLEEP10=1 then BSLEEP10_h=0;
if BSLEEP10 ge 2 then BSLEEP10_h=1;
if 1<=BSLEEP10<=2 then BSLEEP10_RC=0;
if BSLEEP10 ge 3 then BSLEEP10_RC=1;

if QM6=3 then QM6_h=.;
if QM6=2 then QM6_h=0;
if QM6=1 then QM6_h=1;
if M6=3 then M6_h=.;
if M6=2 then M6_h=0;
if M6=1 then M6_h=1;
if VM6=3 then VM6_h=.;
if VM6=0 then VM6_h=0;
if VM6=1 then VM6_h=1;
if GM6=3 then GM6_h=.;
if GM6=2 then GM6_h=0;
if GM6=1 then GM6_h=1;
run;

title 'means for SATSA sleep dis items';
proc freq data=SATSA_sleepcat3;
tables VCESDK_h VCESDK_rc GCESDK_h GCESDK_rc HCESDK_h HCESDK_rc JCESDK_h JCESDK_rc KCESDK_h KCESDK_rc LCESDK_h LCESDK_rc ACESDK_h ACESDK_rc
BCESDK_h BCESDK_rc QCESDK_h QCESDK_rc XCESDK_h XCESDK_RC FCESDK_h FCESDK_RC RCESDK_h RCESDK_RC SCESDK_h SCESDK_RC UCESDK_h UCESDK_RC WCESDK_h WCESDK_RC
YCESDK_h YCESDK_RC ZCESDK_h ZCESDK_RC ICESDK_h ICESDK_RC ASLEEP3_h ASLEEP3_RC ASLEEP1_h ASLEEP1_RC ASLEEP4_h ASLEEP4_RC ASLEEP8_h ASLEEP8_RC
ASLEEP10_h ASLEEP10_RC QM6_h ZSLEEP1_h ZSLEEP1_RC ZSLEEP3_h ZSLEEP3_RC ZSLEEP4_h ZSLEEP4_RC ZSLEEP8_h ZSLEEP8_RC ZSLEEP10_h ZSLEEP10_RC
BSLEEP1_h BSLEEP1_RC BSLEEP3_h BSLEEP3_RC BSLEEP4_h BSLEEP4_RC BSLEEP8_h BSLEEP8_RC BSLEEP10_h BSLEEP10_RC
M6_h VM6_h GM6_h;
run;

/*create datafile with the naming conventions*/
data SATSA_sleep_names; set SATSA_sleepcat3;
slp_dur_fu16=A_hrs_slept;
slp_dur_fu18=B_hrs_slept;
slp_dur_fu17=Z_hrs_slept;
slpcat_fu16=slp_ave_cat1;
slpcat_fu18=slp_ave_cat2;
slpcat_fu17=slp_ave_cat3;
latency_fu16=ASLEEP5_h;
latency_fu18=BSLEEP5_h;
latency_fu17=ZSLEEP5_h;
RClatency_fu16=ASLEEP5_RC;
RClatency_fu18=BSLEEP5_RC;
RClatency_fu17=ZSLEEP5_RC;
latency_in=E60;
slpRX_in=SLEEPRX;
slpRX_fu1=IPT1SLEEPRX;
slpRX_fu2=VSLEEPRX;
slpRX_fu3=QSLEEPRX;
slpRX_fu4=GSLEEPRX;
slpRX_fu5=XSLEEPRX;
slpRX_fu6=HSLEEPRX;
slpRX_fu7=FSLEEPRX;
slpRX_fu8=RSLEEPRX;
slpRX_fu9=SSLEEPRX;
slpRX_fu10=JSLEEPRX;
slpRX_fu11=USLEEPRX;
slpRX_fu13=WSLEEPRX;
wake_earlyfu16=ASLEEP1_h;
wake_earlyfu18=BSLEEP1_h;
wake_earlyfu17=ZSLEEP1_h;
RCwake_earlyfu16=ASLEEP1_RC;
RCwake_earlyfu18=BSLEEP1_RC;
RCwake_earlyfu17=ZSLEEP1_RC;

wake_nightfu16=ASLEEP4_h;
wake_nightfu18=BSLEEP4_h;
wake_nightfu17=ZSLEEP4_h;
RCwake_nightfu16=ASLEEP4_RC;
RCwake_nightfu18=BSLEEP4_RC;
RCwake_nightfu17=ZSLEEP4_RC;

snorefu16=ASLEEP10_h;
snorefu18=BSLEEP10_h;
snorefu17=ZSLEEP10_h;
RCsnorefu16=ASLEEP10_RC;
RCsnorefu18=BSLEEP10_RC;
RCsnorefu17=ZSLEEP10_RC;

restless_in=M6_h;
restless_fu2=VM6_h;
restless_fu3=QM6_h;
restless_fu4=GM6_h;
zygos=PRZYGUP;
study=1;
run;
