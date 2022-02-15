; File name  ...\DOSE_Response\a425WASO_FINAL_new.txt
$PROB WASO_FINAL_new Exp Resid Error/Correlation BSL/ED50

$INPUT STUD ID AGE WGT HT=DROP GEN HORM=DROP RACE=DROP DRUG
DOSE WEEK NITE ESTM=DROP LTPS=DROP LOTM=DROP LREM=DROP LSLP=DROP
NARS=DROP NAWK=DROP NSHF=DROP TMIB=DROP ST1=DROP ST2=DROP ST34=DROP
NREM=DROP STRM=DROP TOTS=DROP WASO=DV SEFF=DROP ELAP=DROP QLSP=DROP
NHRS=DROP NMNS=DROP NUMA=DROP FALL=DROP QUAL=DROP WAKE=DROP MBHV=DROP
CLCR MDV1=MDV MDV2=DROP MDV3=DROP MDV4=DROP MDV5=DROP MDV6=DROP

; STUD: Study number
; ID:   Subject Number
; AGE:  age in yrs
; WGT:  Weight in kg
; HT:   Height in cm
; GEN:  Gender (1=Male, 0=Female)
; HORM: Hormonal Status (0=Male, 1=Premeno, 2=Postmeno)
; RACE: Race (1=White 2=Black 3=Hispanic,4=Asian or Pacific Islander, 
;       5=American Indian or Alaskan Native, 6=Other) 
; DRUG: Indicator Variable (=0 for PCB and PD 0200390 and =1 for Zolpidem)
; DOSE: Dose (0, 25, 50, 75, 100 mg or 10 mg for Zolpidem)
; WEEK: Week of Study (1-4 for Study A4251003 and 1-5 for Study A4251004)
; NITE: Night (1 or 2)
; ESTM:	End Time of Sleep (HR:MIN:SEC)
; LTPS:	Latency to Persistent Sleep (min)
; LOTM:	Light Out Time (HR:MIN:SEC)
; LREM:	Latency to REM (min)
; LSLP:	Latency to Sleep (min)
; NARS:	Number of Arousals 
; NAWK:	Number of Awakenings(PSG)
; NSHF:	Number of Shifts to Stage 1
; TMIB:	Time in Bed (min)
; ST1:	Stage 1 Sleep Minutes
; ST2:	Stage 2 Sleep Minutes
; ST34:	Stage 3-4 Sleep Minutes
; NREM:	Non-REM State Sleep Minutes
; STRM:	REM State Sleep Minutes
; TOTS:	Total Sleep Time (min)
; WASO:	Wake After Sleep Onset (min)
; SEFF:	Sleep Efficiency (%)
; ELAP: Elapsed minutes to Fall Asleep
; QLSP:	Quality of Sleep
; NHRS:	Number of Hours of Sleep
; NMNS:	Number of Minutes of Sleep
; NUMA:	Number of Awalenings(SSQ)
; FALL: Ease of Getting to Sleep (Leeds)
; WAKE: Ease of Awakening from Sleep (Leeds)
; QUAL: Quality of Sleep (Leeds)
; MBHV: Early Morning Behavior (Leeds)
; CLCR: Creatinine Clearance in mL/min
; MDV1: Missing Data Value PSG Data
; MDV2: Missing Data Value Subjective Sleep Data
; MDV3: Missing Data Value for FALL Data
; MDV4: Missing Data Value for WAKE Data
; MDV5: Missing Data Value for QUAL Data
; MDV6: Missing Data Value for MBHV Data

$DATA wasodata.csv IGNORE=#

; Data from Night 1 (NOT shifted) for Study A4251004 Excluded
; Data from Subject 10011004 Period 2 considered outlier
    
 $PRED

;******************VERBATIM CODE TO GENERATE SUMMARY DOCUMENT**********
;**********************************************************************

"      FIRST
"*     BY Bill and Arnab 
"*     USER CONTROL BLOCK
"*     one and only one of the next two lines should be active
"*           
"      INCLUDE 'c:\nmv\nm\nsizes'
"*     INCLUDE '/prod/nonmem/nonmemva11.kod/nm/NSIZES'
"       
"      COMMON /ROCM6/ THETAF(lth),OMEGAF(lvr,lvr),SIGMAF(lvr,lvr)
"      COMMON /ROCM7/ SETH(lth), SEOM(lvr,lvr),SESIG(lvr,lvr)
"      COMMON /ROCM9/ IERE,IERC
"      COMMON /CM18/ SPEC
"      COMMON /ROCM8/ OBJECT
"      INTEGER unitp,i1a,j1a,k1a,l1a,IERC,t1a,o1a,s1a,test1a,IERE
"      DOUBLE PRECISION  THETAF, OMEGAF, SIGMAF, OBJECT
"      REAL SETH, SEOM, SESIG, fudge,carmel,ll1a,ul1a,c1a,m1a,corr1a
"      CHARACTER cisym * 40
"      CHARACTER sammy * 60
"      CHARACTER lloyd * 90
"      unitp=42
"*
"*
"*      USER CONTROL BLOCK
"*      m1a = standard errors
"      m1a=1.96
"*      cisym='fill in here' CALL YOUR FILE
"      cisym='a425_FINAL.txt'
"*      t1a=lenght of theta vector
"      t1a=12
"*      o1a=rows in omega^2 matrix
"      o1a=5 
"*      s1a=rows in sigma^2 matrix
"      s1a=2
"      IF (ICALL.EQ.1) then
"         OPEN (42,FILE='FCON',ACCESS='sequential')
"         call files (unitp)
"         READ (42,200) lloyd
"         READ (42,200) lloyd
"         CLOSE (unitp)
"         call files (unitp)
"         OPEN (42,FILE=cisym,ACCESS='append')    
"         call files(unitp)
"         write(42,200)' '
"         write(42,200) lloyd
"         write(42,200)' '
"         CLOSE(unitp)
"         call files(unitp)
"      endif
"
"      IF (ICALL.EQ.3.AND.IERE.EQ.0) then
"         OPEN (42,FILE=cisym,ACCESS='append')    
"         call files(unitp)
"         write(42,200) ' '
"         write(42,204) ' Final objective value = ', OBJECT
"         write(42,200) ' '
"         write(42,200) ' Final estimates, fixation and upper and lower bounds'
"         write(42,200) ' for confidence intervals constructed as estimate'
"         write(42,203) ' +/- ',m1a,'* standard error of estimate'
"         write(42,200) ' '
"         sammy = '   Effect           Estimate   Fixed      Lower       Upper'
"         write(42,200) sammy
"         write(42,200) ' '
"         DO 60 i1a=1,t1a 
"            fudge=THETAF(i1a)-m1a*SETH(i1a)
"            carmel=THETAF(i1a)+m1a*SETH(i1a)
"            test1a=0
"            if(SETH(i1a).EQ.0) test1a = -1
"            if(SETH(i1a).GT.10000) test1a=1 
"                 IF (test1a.eq.1.or.test1a.eq.-1) THEN
"                    fudge=0
"                    carmel=0
"                 ENDIF    
"            write (42,102) ' THETA[', i1a,']',THETAF(i1a),test1a,fudge,carmel
"  60     CONTINUE
"         DO 70 j1a=1,o1a
"            DO 65 l1a=1,j1a
"               IF(j1a.NE.l1a) THEN
"                 test1a=0
"                 if(SEOM(j1a,l1a).EQ.0) test1a = -1
"                 if(SEOM(j1a,l1a).GT.10000) test1a=1 
"                 IF(OMEGAF(j1a,j1a).GT.0.AND.OMEGAF(l1a,l1a).GT.0) THEN
"         corr1a=OMEGAF(j1a,l1a)/(OMEGAF(j1a,j1a)**(0.5)*OMEGAF(l1a,l1a)**(0.5))
"                 ENDIF 
"                    ll1a=0
"                    ul1a=0
"          write(42,103)' CORREL[',j1a,',',l1a,']',corr1a,test1a,ll1a,ul1a
"               ENDIF
"               IF(j1a.EQ.l1a) THEN
"                 test1a=0
"                 if(SEOM(j1a,l1a).EQ.0) test1a = -1
"                 if(SEOM(j1a,l1a).GT.10000) test1a=1   
"                 c1a=(OMEGAF(j1a,l1a))**(0.5)
"                 ll1a=(MAX(0.0,OMEGAF(j1a,l1a)-m1a*SEOM(j1a,l1a)))**(0.5)
"                 ul1a=(OMEGAF(j1a,l1a)+m1a*SEOM(j1a,l1a))**(0.5)
"                 IF (test1a.eq.1.or.test1a.eq.-1) THEN
"                    ll1a=0
"                    ul1a=0
"                 ENDIF
"            write(42,101)' OMEGA^(1/2)[ ',j1a,',',l1a,']',c1a,test1a,ll1a,ul1a
"               ENDIF
"  65        continue        
"  70     continue 
"         do 80 k1a=1,s1a
"            do 75 l1a=1,k1a
"               IF (k1a.NE.l1a) THEN
"                  test1a=0
"                  if(SESIG(k1a,l1a).EQ.0) test1a = -1
"                  if(SESIG(k1a,l1a).GT.10000) test1a=1 
"                  IF (SIGMAF(k1a,k1a).GT.0.AND.SIGMAF(l1a,l1a).GT.0) THEN   
"         corr1a=SIGMAF(k1a,l1a)/(SIGMAF(k1a,k1a)**(0.5)*SIGMAF(l1a,l1a)**(0.5))
"                  ENDIF
"                    ll1a=0
"                    ul1a=0
"        write(42,103)' CORREL[',k1a,',',l1a,']',corr1a,test1a,ll1a,ul1a
"               ENDIF
"               IF (k1a.EQ.l1a) THEN
"                  test1a=0
"                  if(SESIG(k1a,l1a).EQ.0) test1a = -1
"                  if(SESIG(k1a,l1a).GT.10000) test1a=1   
"                  c1a=(SIGMAF(k1a,l1a))**(0.5)
"                  ll1a=(MAX(0.0,SIGMAF(k1a,l1a)-m1a*SESIG(k1a,l1a)))**(0.5)
"                  ul1a=(SIGMAF(k1a,l1a)+m1a*SESIG(k1a,l1a))**(0.5)
"                  IF (test1a.eq.1.or.test1a.eq.-1) THEN
"                    ll1a=0
"                    ul1a=0
"                  ENDIF
"          write(42,101)' SIGMA^(1/2)[ ',k1a,',',l1a,']',c1a,test1a,ll1a,ul1a
"               ENDIF
"  75        continue     
"  80     continue             
"         CLOSE(unitp)
"         call files(unitp)
"       endif
"       IF (ICALL.EQ.3.AND.IERC.NE.0) then
"          OPEN (42,FILE=cisym,ACCESS='append')    
"          call files(unitp)
"          write(42,200) 'Sorry, $COV did not run'
"          CLOSE(unitp)
"          call files(unitp)
"       endif
"       IF (ICALL.EQ.3.AND.IERE.NE.0) then
"          OPEN (42,FILE=cisym,ACCESS='append')    
"          call files(unitp)
"          write(42,205) 'Sorry, $EST did not run, termination code= ',IERE
"          CLOSE(unitp)
"          call files(unitp)
"       endif
" 100   format(A,I2,A1,2X, F10.4) 
" 101   format(A,I1,A1,I1,A1,1X, E11.4,2X,I2,7X,E11.4,4X,E11.4)
" 102   format(A,I2,A1,9X, E11.4,2X,I2,7X,E11.4,4X,E11.4) 
" 103   format(A,I1,A1,I1,A1,7X, E11.4,2X,I2,7X,E11.4,4X,E11.4)
" 200   format(A)
" 201   format(A60) 
" 202   format(I3, F18.4)
" 203   format(A,F4.1,A30) 
" 204   format(A25, F12.4) 
" 205   format(A,I9)

;****************************PHARMACODYNAMIC MODEL*********************
;**********************************************************************
 
 ; Define STUDY Factor

   STUF=0                   ; For STUDY A4251004
   IF (STUD.EQ.3) STUF=1    ; For STUDY A4251003

   ; Define Equation for Drug Effect

    PCB = THETA(1)*EXP(ETA(1))
    EMAX = THETA(2) 
    EC50 = THETA(3)*EXP(ETA(2))
    ZOLP = THETA(4)

 
    F=PCB-((PCB*EMAX*DOSE/(EC50+DOSE))*(1-DRUG))-(PCB*ZOLP*DRUG)

   IPRED=F         ;INDIVIDUAL PREDICTION
   IRES=DV-IPRED   ;INDIVIDUAL RESIDUAL
   W=1            ; 1=Additive and F (or IPRED)=Constant CV
   IWRES = IRES/W
 
   Y = F+F*EPS(1)+EPS(2)
    
 $THETA (0, 90, 480)   ;  PCB
 $THETA (1 FIXED)    ; EMAX
 $THETA (0, 50, 500)   ; EC50
 $THETA (0, 0.5, 1)         ; ZOLPIDEM

 $OMEGA BLOCK(2) 0.25 0.1 0.25

 $SIGMA 0.3 (0 FIXED)

 $EST  METHOD=1 INTERACTION SIGDIGITS=3 MAXEVAL=9999 PRINT=5 
  POSTHOC NOABORT MSFO=MSF1
 $COV
 $TABLE  ONEHEADER ID STUD WEEK NITE DRUG DOSE PCB EMAX EC50 ZOLP
  ETA(1) ETA(2) MDV WRES IWRES IPRED NOPRINT FILE=WASO.tab
 