data one;
 infile 'e:\course\SKKU\Experimental_Design\My_Course\2017_Spring\SAS_codes\tensile.dat';
 input percent strength time;
run;

proc glm data=one;
 class percent;
 model strength=percent;
 contrast 'C1' percent 0 0 0 1 -1;
 contrast 'C2' percent 1 0 1 -1 -1;
 contrast 'C3' percent 1 0 -1 0 0;
 contrast 'C4' percent 1 -4 1 1 1;
 contrast 'C5' percent -2 -1 0 1 2;
run;
