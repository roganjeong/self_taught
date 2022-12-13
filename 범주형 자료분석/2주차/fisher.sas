options ls=100 ps=200;

data fisher;
 input poured guess count;
 cards;
 1 1 3
 1 2 1
 2 1 1
 2 2 3
 ;

proc freq data=fisher;
  weight count;
  table poured*guess / relrisk chisq;
  exact fisher or / alpha=.05;
run;
