proc print data=infile; run;

proc glm data=infile;
  class dose;
  model RESP=dose;
  lsmeans dose /CL;
  ods output LSMeanCL(MATCH_ALL)=LSM;
  run;quit;

data outfile (KEEP=D MEAN SE LOWER UPPER N);
set LSM;
D = dose;
MEAN = LSMean;
LOWER = LowerCL;
UPPER = UpperCL;
SE = 1;
N = 1;
run;

data outfile(KEEP=DOSE MEAN SE LOWER UPPER N); set outfile; 
DOSE = D;
run;
