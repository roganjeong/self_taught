data lambs;
input diet wtgain@@;
cards;
1 8 1 13 1 9 2 12 2 16 2 21 2 17 2 18 3 11 3 10 3 17 3 6
;
run;

proc print;
run;

proc glm;
 class diet;
 model wtgain=diet;
 means diet;
 estimate 'L1' intercept 6 diet 1 2 3;
run;
