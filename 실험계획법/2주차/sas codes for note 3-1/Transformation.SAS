data two; 
 infile 'D:\course\SKKU\Experimental_Design\My_Course\2015-Spring\SAS_codes\boxcox.dat'; 
 input trt resp; 
run; 

proc glm data=two; 
 class trt; 
 model resp=trt; 
 output out=three p=pred r=res; 
run; 

title1 'Residual Plot'; 
proc sgplot data=three; 
scatter y=res x=pred; 
run;

proc print data=three;
run;

proc transreg data=three; 
 model boxcox(resp/convenient lambda=-2.0 to 2.0 by 0.1)=class(trt); 
run;
