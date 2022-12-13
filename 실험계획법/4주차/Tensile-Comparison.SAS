data one;
 infile 'd:\course\SKKU\Experimental_Design\My_Course\2014_Spring\SAS_codes\tensile.dat';
 input percent strength time;
run;

proc print data=one;
run;

proc glm data=one; 
 class percent; 
 model strength=percent; 
 /* Construct CI for Treatment Means*/ 
 means percent /alpha=.05 clm lsd; 
 means percent /alpha=.05 clm bon; 
 means percent /alpha=.05 clm scheffe; 
 /* Pairwise Comparison*/ 
 means percent /alpha=.05 lines lsd; 
 means percent /alpha=.05 lines bon; 
 means percent /alpha=.05 lines tukey; 
 means percent /alpha=.05 dunnett ('15'); 
run;
