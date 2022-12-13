options nocenter ps=65 ls=80;
data new;
input strain nitrogen @@;
cards;
1 2.80 1 7.04 1 0.41 1 1.73 1 0.18 
2 0.60 2 1.14 2 0.14 2 0.16 2 1.40 
3 0.05 3 1.07 3 1.68 3 0.46 3 4.87 
4 1.20 4 0.89 4 3.22 4 0.77 4 1.24 
5 0.74 5 0.20 5 1.62 5 0.09 5 2.27 
6 1.26 6 0.26 6 0.47 6 0.46 6 3.26
;

proc print data=new;
run;

proc npar1way;
class strain;
var nitrogen;
run;
