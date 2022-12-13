
/* BIBD */

options nocenter ps=60 ls=75;
data example;
input trt block resp @@;
datalines;
1 1 73 1 2 74 1 4 71 2 2 75 2 3 67 2 4 72
3 1 73 3 2 75 3 3 68 4 1 75 4 3 72 4 4 75
;

proc print data=example;
run;

proc glm;
class block trt;
model resp = block trt ;
lsmeans trt / tdiff pdiff adjust=bon stderr;
lsmeans trt / tdiff pdiff adjust=tukey;
contrast 'a' trt 1 -1 0 0;
estimate 'b' trt  0 0 1 -1 ;
output out=myout r=res p=pred;
run;

/* Model adequacy checking */
proc sgplot data=myout;
scatter y=res x=pred;
refline 0;
run;

proc univariate data=myout normal;
var res;
qqplot res/normal(mu=est sigma=est
color=red L=1);
run;