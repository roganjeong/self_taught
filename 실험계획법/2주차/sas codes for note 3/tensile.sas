options ls=75 ps=60 nocenter;

data one;
 infile 'e:\course\SKKU\Experimental_Design\My_Course\2014_Spring\SAS_codes\tensile.dat';
 input percent strength time;
 run;

title1 ' example';
proc print data=one; 
run;

symbol1 v=circle i=none;
title1 'Plot of Strength vs Percent Blend';
proc gplot data=one;  
plot strength*percent/frame; 
run;

proc boxplot;
 plot strength*percent/boxstyle=skeletal pctldef=4;
 run;

proc glm;
 class percent; 
 model strength=percent;
 output out=oneres p=pred r=res; 
run;

proc sort; by pred;
symbol1 v=circle i=sm50; 
title1 'Residual Plot';
proc gplot;  
plot res*pred/frame; 
run;

proc univariate data=oneres pctldef=4 normal;
 var res;  
 qqplot res / normal (L=1 mu=est sigma=est);
 histogram res / normal; 
run;

symbol1 v=circle i=none;
title1 'Plot of residuals vs time';
proc gplot; 
plot res*time / vref=0 vaxis=-6 to 6 by 1;
run;


/* check constant variance */

proc glm data=one;
class percent;
model strength=percent;
means percent / hovtest=bartlett hovtest=levene;
output out=diag p=pred r=res;
run;
