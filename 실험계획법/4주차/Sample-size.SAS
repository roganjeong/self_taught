proc power; 
 onewayanova groupmeans = 575 | 600 | 650 | 675 
 stddev = 25 
 alpha = 0.01 
 npergroup = . 
 power = .9; 
run;