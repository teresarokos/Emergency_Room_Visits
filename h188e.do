/************************************************************************************************/
/* Stata User File for H188E Data                                                                 */
/*                                                                                              */
/* This file contains information and a sample Stata program to create a permanent              */
/* Stata dataset for users who want to use Stata in processing the MEPS data provided           */
/* in this PUF release.  Stata (StataCorp) has the capability to produce                        */
/* appropriate standard errors for estimates from a survey with a complex sample                */
/* design such as the Medical Expenditure Panel Survey (MEPS).                                  */
/* The input file for creating a permanent Stata dataset is the ASCII data file                 */
/* (H188E.DAT) supplied in this PUF release, which in turn can be extracted from the              */
/* .EXE file. After entering the Stata interactive environment access the Stata DO-File         */
/* editor by clicking on the appropriate icon in the command line at the top of the             */
/* screen.  Copy and paste the following Stata commands into the editor and save as a           */
/* DO file.  A DO file is a Stata program which may then be executed using the DO command.      */
/* For example, if the DO file is named H188E.DO and is located in the directory                  */
/* C:\MEPS\PROG, then the file may be executed by typing the following command into             */
/* the Stata command line:                                                                      */
/*                         do C:\MEPS\PROG\H188E.DO                                               */
/* The program below will output the Stata dataset H188E.DTA                                      */
/************************************************************************************************/


#delimit ;
cd "/Users/teresarokos/Desktop/GOV 1005/GOV1005-Final_Project";
log using H188E.log, replace;
clear;

* INPUT ALL VARIABLES;
infix
  long   DUID 1-5
  int    PID 6-8
  str    DUPERSID 9-16
  str    EVNTIDX 17-28
  byte   EVENTRN 29-29
  str    ERHEVIDX 30-41
  str    FFEEIDX 42-53
  byte   PANEL 54-55
  byte   MPCDATA 56-56
  int    ERDATEYR 57-60
  byte   ERDATEMM 61-62
  byte   VSTCTGRY 63-64
  byte   VSTRELCN 65-66
  byte   LABTEST 67-68
  byte   SONOGRAM 69-70
  byte   XRAYS 71-72
  byte   MAMMOG 73-74
  byte   MRI 75-76
  byte   EKG 77-78
  byte   EEG 79-80
  byte   RCVVAC 81-82
  byte   ANESTH 83-84
  byte   THRTSWAB 85-86
  byte   OTHSVCE 87-88
  byte   SURGPROC 89-90
  byte   MEDPRESC 91-92
  str    ERCCC1X 93-95
  str    ERCCC2X 96-98
  str    ERCCC3X 99-101
  str    ERCCC4X 102-104
  byte   FFERTYPE 105-106
  byte   FFBEF16 107-108
  double ERXP16X 109-117
  double ERTC16X 118-126
  double ERFSF16X 127-134
  double ERFMR16X 135-142
  double ERFMD16X 143-150
  double ERFPV16X 151-159
  double ERFVA16X 160-167
  double ERFTR16X 168-174
  double ERFOF16X 175-181
  double ERFSL16X 182-188
  double ERFWC16X 189-195
  double ERFOR16X 196-203
  double ERFOU16X 204-210
  double ERFOT16X 211-218
  double ERFXP16X 219-227
  double ERFTC16X 228-236
  double ERDSF16X 237-243
  double ERDMR16X 244-250
  double ERDMD16X 251-257
  double ERDPV16X 258-264
  double ERDVA16X 265-271
  double ERDTR16X 272-277
  double ERDOF16X 278-281
  double ERDSL16X 282-287
  double ERDWC16X 288-294
  double ERDOR16X 295-301
  double ERDOU16X 302-307
  double ERDOT16X 308-314
  double ERDXP16X 315-321
  double ERDTC16X 322-329
  byte   IMPFLAG 330-330
  double PERWT16F 331-342
  int    VARSTR 343-346
  byte   VARPSU 347-347
using H188E.dat;

*DEFINE VARIABLE LABELS;
label variable DUID "DWELLING UNIT ID";
label variable PID "PERSON NUMBER";
label variable DUPERSID "PERSON ID (DUID + PID)";
label variable EVNTIDX "EVENT ID";
label variable EVENTRN "EVENT ROUND NUMBER";
label variable ERHEVIDX "EVENT ID FOR CORRESPONDING HOSPITAL STAY";
label variable FFEEIDX "FLAT FEE ID";
label variable PANEL "PANEL NUMBER";
label variable MPCDATA "MPC DATA FLAG";
label variable ERDATEYR "EVENT DATE - YEAR";
label variable ERDATEMM "EVENT DATE - MONTH";
label variable VSTCTGRY "BEST CATEGORY FOR CARE P RECV ON VST DT";
label variable VSTRELCN "THIS VST RELATED TO SPEC CONDITION";
label variable LABTEST "THIS VISIT DID P HAVE LAB TESTS";
label variable SONOGRAM "THIS VISIT DID P HAVE SONOGRAM OR ULTRSD";
label variable XRAYS "THIS VISIT DID P HAVE X-RAYS";
label variable MAMMOG "THIS VISIT DID P HAVE A MAMMOGRAM";
label variable MRI "THIS VISIT DID P HAVE AN MRI/CATSCAN";
label variable EKG "THIS VISIT DID P HAVE AN EKG OR ECG";
label variable EEG "THIS VISIT DID P HAVE AN EEG";
label variable RCVVAC "THIS VISIT DID P RECEIVE A VACCINATION";
label variable ANESTH "THIS VISIT DID P RECEIVE ANESTHESIA";
label variable THRTSWAB "THIS VISIT DID P HAVE A THROAT SWAB";
label variable OTHSVCE "THIS VISIT DID P HAVE OTH DIAG TEST/EXAM";
label variable SURGPROC "WAS SURG PROC PERFORMED ON P THIS VISIT";
label variable MEDPRESC "ANY MEDICINE PRESCRIBED FOR P THIS VISIT";
label variable ERCCC1X "MODIFIED CLINICAL CLASSIFICATION CODE";
label variable ERCCC2X "MODIFIED CLINICAL CLASSIFICATION CODE";
label variable ERCCC3X "MODIFIED CLINICAL CLASSIFICATION CODE";
label variable ERCCC4X "MODIFIED CLINICAL CLASSIFICATION CODE";
label variable FFERTYPE "FLAT FEE BUNDLE";
label variable FFBEF16 "TOTAL # OF VISITS IN FF BEFORE 2016";
label variable ERXP16X "TOT EXP FOR EVENT (ERFXP16X + ERDXP16X)";
label variable ERTC16X "TOTAL CHG FOR EVENT (ERFTC16X+ERDTC16X)";
label variable ERFSF16X "FACILITY AMT PD, FAMILY (IMPUTED)";
label variable ERFMR16X "FACILITY AMT PD, MEDICARE (IMPUTED)";
label variable ERFMD16X "FACILITY AMT PD, MEDICAID (IMPUTED)";
label variable ERFPV16X "FACILITY AMT PD, PRIV INSUR (IMPUTED)";
label variable ERFVA16X "FAC AMT PD,VETERANS/CHAMPVA(IMPUTED)";
label variable ERFTR16X "FACILITY AMT PD,TRICARE(IMPUTED)";
label variable ERFOF16X "FACILITY AMT PD, OTH FEDERAL (IMPUTED)";
label variable ERFSL16X "FACILITY AMT PD, STATE/LOC GOV (IMPUTED)";
label variable ERFWC16X "FACILITY AMT PD, WORKERS COMP (IMPUTED)";
label variable ERFOR16X "FACILITY AMT PD, OTH PRIV (IMPUTED)";
label variable ERFOU16X "FACILITY AMT PD, OTH PUB (IMPUTED)";
label variable ERFOT16X "FACILITY AMT PD, OTH INSUR (IMPUTED)";
label variable ERFXP16X "FACILITY SUM PAYMENTS ERFSF16X-ERFOT16X";
label variable ERFTC16X "TOTAL FACILITY CHARGE (IMPUTED)";
label variable ERDSF16X "DOCTOR AMOUNT PAID, FAMILY (IMPUTED)";
label variable ERDMR16X "DOCTOR AMOUNT PD, MEDICARE (IMPUTED)";
label variable ERDMD16X "DOCTOR AMOUNT PAID, MEDICAID (IMPUTED)";
label variable ERDPV16X "DOCTOR AMT PD, PRIV INSUR (IMPUTED)";
label variable ERDVA16X "DR AMT PD,VETERANS/CHAMPVA(IMPUTED)";
label variable ERDTR16X "DOCTOR AMT PD,TRICARE(IMPUTED)";
label variable ERDOF16X "DOCTOR AMT PAID, OTH FEDERAL (IMPUTED)";
label variable ERDSL16X "DOCTOR AMT PD, STATE/LOC GOV (IMPUTED)";
label variable ERDWC16X "DOCTOR AMOUNT PD, WORKERS COMP (IMPUTED)";
label variable ERDOR16X "DOCTOR AMT PD, OTH PRIVATE (IMPUTED)";
label variable ERDOU16X "DOCTOR AMT PD, OTH PUB (IMPUTED)";
label variable ERDOT16X "DOCTOR AMT PD, OTH INSUR (IMPUTED)";
label variable ERDXP16X "DOCTOR SUM PAYMENTS ERDSF16X - ERDOT16X";
label variable ERDTC16X "TOTAL DOCTOR CHARGE (IMPUTED)";
label variable IMPFLAG "IMPUTATION STATUS";
label variable PERWT16F "EXPENDITURE FILE PERSON WEIGHT, 2016";
label variable VARSTR "VARIANCE ESTIMATION STRATUM, 2016";
label variable VARPSU "VARIANCE ESTIMATION PSU, 2016";


*DEFINE VALUE LABELS FOR REPORTS;
label define H188E0001X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0002X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0003X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0004X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED" ;

label define H188E0005X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         2016 "2016" ;

label define H188E0006X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0007X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0008X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0009X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0010X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0011X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0012X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0013X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0014X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0015X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0016X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0017X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0018X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0019X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0020X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0021X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0022X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0023X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0024X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0025X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0026X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0027X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0028X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0029X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0030X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0031X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0032X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0033X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0034X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0035X
         -1 "-1 INAPPLICABLE"
         -9 "-9 NOT ASCERTAINED"
         0 "0" ;

label define H188E0036X
         1 "ROUND 1"
         2 "ROUND 2"
         3 "ROUND 3"
         4 "ROUND 4"
         5 "ROUND 5" ;

label define H188E0037X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         0 "0"
         2 "1 - 2" ;

label define H188E0038X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 FLAT FEE STEM"
         2 "2 FLAT FEE LEAF" ;

label define H188E0039X
         0 "0 NOT ELIGIBLE FOR IMPUTATION"
         1 "1 COMPLETE HC DATA"
         2 "2 COMPLETE MPC DATA"
         3 "3 FULLY IMPUTED"
         4 "4 PARTIALLY IMPUTED"
         5 "5 CAPITATION IMPUTATION" ;

label define H188E0040X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0041X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0042X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO" ;

label define H188E0043X
         1 "1 HAS MPC DATA"
         2 "2 NO MPC DATA" ;

label define H188E0044X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0045X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0046X
         20 "PANEL 20"
         21 "PANEL 21" ;

label define H188E0047X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0048X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0049X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0050X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

label define H188E0051X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 DIAGNOSIS OR TREATMENT"
         2 "2 EMERGENCY (E.G., ACCIDENT OR INJURY)"
         3 "3 PSYCHOTHERAPY/MENTAL HEALTH COUNSELING"
         4 "4 FOLLOW-UP OR POST-OPERATIVE VISIT"
         5 "5 IMMUNIZATIONS OR SHOTS"
         6 "6 PREGNANCY-RELATED (INC PRENATAL/ DELV)"
         91 "91 OTHER" ;

label define H188E0052X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO" ;

label define H188E0053X
         -1 "-1 INAPPLICABLE"
         -7 "-7 REFUSED"
         -8 "-8 DK"
         -9 "-9 NOT ASCERTAINED"
         1 "1 YES"
         2 "2 NO"
         95 "95 NO SERVICES RECEIVED" ;

* ASSOCIATE VARIABLES WITH VALUE LABEL DEFINITIONS;
label value ANESTH H188E0001X;
label value EEG H188E0002X;
label value EKG H188E0003X;
label value ERDATEMM H188E0004X;
label value ERDATEYR H188E0005X;
label value ERDMD16X H188E0006X;
label value ERDMR16X H188E0007X;
label value ERDOF16X H188E0008X;
label value ERDOR16X H188E0009X;
label value ERDOT16X H188E0010X;
label value ERDOU16X H188E0011X;
label value ERDPV16X H188E0012X;
label value ERDSF16X H188E0013X;
label value ERDSL16X H188E0014X;
label value ERDTC16X H188E0015X;
label value ERDTR16X H188E0016X;
label value ERDVA16X H188E0017X;
label value ERDWC16X H188E0018X;
label value ERDXP16X H188E0019X;
label value ERFMD16X H188E0020X;
label value ERFMR16X H188E0021X;
label value ERFOF16X H188E0022X;
label value ERFOR16X H188E0023X;
label value ERFOT16X H188E0024X;
label value ERFOU16X H188E0025X;
label value ERFPV16X H188E0026X;
label value ERFSF16X H188E0027X;
label value ERFSL16X H188E0028X;
label value ERFTC16X H188E0029X;
label value ERFTR16X H188E0030X;
label value ERFVA16X H188E0031X;
label value ERFWC16X H188E0032X;
label value ERFXP16X H188E0033X;
label value ERTC16X H188E0034X;
label value ERXP16X H188E0035X;
label value EVENTRN H188E0036X;
label value FFBEF16 H188E0037X;
label value FFERTYPE H188E0038X;
label value IMPFLAG H188E0039X;
label value LABTEST H188E0040X;
label value MAMMOG H188E0041X;
label value MEDPRESC H188E0042X;
label value MPCDATA H188E0043X;
label value MRI H188E0044X;
label value OTHSVCE H188E0045X;
label value PANEL H188E0046X;
label value RCVVAC H188E0047X;
label value SONOGRAM H188E0048X;
label value SURGPROC H188E0049X;
label value THRTSWAB H188E0050X;
label value VSTCTGRY H188E0051X;
label value VSTRELCN H188E0052X;
label value XRAYS H188E0053X;

*DISPLAY A DESCRIPTION OF STATA FILE;
describe;

*LIST FIRST 20 OBSERVATIONS IN THE FILE;
list in 1/20;

save H188E, replace;

#delimit cr

* data file is stored in H188E.dta
* log  file is stored in H188E.log

log close

/************************************************************************************************
 NOTES:                                                                                          
                                                                                                 
 1. This program has been tested on Stata Version 10 (for Windows).                              
                                                                                                 
 2. This program will create a permanent Stata dataset.  All additional analyses                 
    can be run using this dataset.  In addition to the dataset, this program creates             
    a log file named H188E.LOG and a data file named H188E.DTA.  If these files (H188E.DTA and H188E.LOG)
    already exist in the working directory, they will be replaced when this program is executed. 
                                                                                                 
 3. If the program ends prematurely, the log file will remain open.  Before running this         
    program again, the user should enter the following Stata command: log close                  
                                                                                                 
 4. The cd command assigns C:\MEPS\DATA as the working directory and location of the input       
    ASCII and output .DTA and .LOG files and can be modified by the user as necessary.           
                                                                                                 
 5. Stata commands end with a carriage return by default. The command                            
    #delimit ;                                                                                   
    temporarily changes the command ending delimiter from a carriage return to a semicolon.      
                                                                                                 
 6. The infix command assumes that the input variables are numeric unless the variable name      
    is prefaced by str.  For example, DUPERSID is the a string (or character) variable.          
                                                                                                 
************************************************************************************************/
