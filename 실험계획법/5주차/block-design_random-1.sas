options nocenter ls=75;

data wash;
  input stain soap y @@;
  cards;
  1 1 45 1 2 47 1 3 48 1 4 42
  2 1 43 2 2 46 2 3 50 2 4 37
  3 1 51 3 2 52 3 3 55 3 4 49
  run;

proc glm data=wash;
    class stain soap;
	model y = stain soap;
	random stain;
	means soap / alpha=0.05 tukey lines;
run;

proc mixed data=wash;
    class stain soap;
	model y = soap;
	random stain;
	lsmeans soap / alpha=0.05 adjust=tukey tdiff;
run;


