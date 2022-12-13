/* Partitioning Chi-squared*/
data jobsatis;
input income satis count @@;
cards;
3 1 2 3 2 4 3 3 13 3 4 3
10 1 2 10 2 6 10 3 22 10 4 4
20 1 0 20 2 1 20 3 15 20 4 8
30 1 0 30 2 3 30 3 13 30 4 8
;
run;

proc print data=jobsatis;
run;

proc freq data=jobsatis;
weight count;
tables income*satis/relrisk chisq expected nopercent norow nocol;
exact fisher or /alpha=0.05;
run;

data collapse1;
input income satis count @@;
cards;
3 1 2 3 2 4 3 3 13 3 4 3
10 1 2 10 2 6 10 3 22 10 4 4
;
run;
data collapse2;
input income satis count @@;
cards;
20 1 0 20 2 1 20 3 15 20 4 8
30 1 0 30 2 3 30 3 13 30 4 8
;
run;
data collapse3;
input income $ satis count @@;
cards;
<15 1 4 <15 2 10 <15 3 35 <15 4 7
>15 1 0 >15 2 4 >15 3 28 >15 4 16
;
run;

proc freq data=collapse1;
weight count;
tables income*satis/chisq expected nopercent norow nocol;
run;
proc freq data=collapse2;
weight count;
tables income*satis/chisq expected nopercent norow nocol;
run;
proc freq data=collapse3;
weight count;
tables income*satis/chisq expected nopercent norow nocol;
run;
