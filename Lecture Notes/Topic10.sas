*    TOPIC 10 SIMULATION;

*   GENERATING RANDOM NUMBER FOLLOWS UNIFORM DISTRIBUTION   ;
data Ugen;
call streaminit(1234); /* 1234 is used as the seed, similar as set.seed(1234) in R, which helps the genarated random nummbers reproducible */
do i = 1 to 10; * here we use n = 10;
 x = rand('uniform', 2, 3); * here, we use a = 2, b = 3; 
 output;
end;
keep x;
run;

proc print data=Ugen;
var x;
run;

* https://support.sas.com/documentation/cdl/en/lefunctionsref/63354/HTML/default/viewer.htm#p0fpeei0opypg8n1b06qe4r040lv.htm  ;

* https://go.documentation.sas.com/doc/en/pgmsascdc/9.4_3.2/lefunctionsref/p026ygl6toz3tgn14lt4iu6cl5bb.htm ;


*   GENERATING RANDOM NUMBERS FOLLOW EXPONENTIAL DISTRIBUTION   ;
data Egen;
call streaminit(1234); 
do i = 1 to 10; * here we use n = 10;
 x = rand('exponential', 1/5); *rate lambda = 5; 
 output;
end;
keep x;
run;

proc print data=Egen;
var x;
run;

*   GENERATING RANDOM NUMBERS FOLLOW WEIBULL DISTRIBUTION   ;
data Weibullgen;
call streaminit(1234);
n = 10; 
do i = 1 to n;
x = rand('weibull',4); *shape alpha = 4;
output; end;keep x;run;





*   GENERATING RANDOM NUMBERS FOLLOW NORMAL DISTRIBUTION   ;
data Normalgen;
call streaminit(1234);
n = 10; mu = 5; sigma = 1.5;
do i = 1 to n;
x = rand('normal',mu,sigma);
output; end;keep x;run;




*   GENERATING RANDOM NUMBERS FOLLOW CHISQUARE DISTRIBUTION   ;
data Chisqgen;
call streaminit(1234);
n = 10; df = 3;
do i = 1 to n;
x = rand('chisq',df);
output; end;keep x;run;


*   GENERATING RANDOM NUMBERS FOLLOW BINOMIAL DISTRIBUTION   ;
data Bingen;
call streaminit(1234);
p = 0.3; n = 100; *n = number of trials of the distribution;
do i = 1 to 10; *10 is the number of the observations that we want to generate;
x = rand('binom',p,n);
output; end;keep x;run;


*   GENERATING RANDOM NUMBERS FOLLOW POISSON DISTRIBUTION   ;
data Poisgen;
call streaminit(1234);
do i = 1 to 10; *n = 10; 
x = rand('poisson',3); *lambda = 3;
output; end;keep x;run;




** CHECKING SIZE OF T-TEST  ; 

* Generate M normal random samples;
* "mcrep" (MC replicates) is the index for the m-th random sample;
data simu;
seed=123;
M=10000; n=20; mu=170; sigma=10;
call streaminit(seed);
do mcrep = 1 to M;
do i = 1 to n;
x = rand("normal",mu,sigma);
output;
end;
end;
keep mcrep x;
run;
proc sort data=simu;
by mcrep;
run;
/*
Perform the t-test for each sample for Ho: mu = 170
Output p-value "probt" in "outtest"
*/
proc univariate data=simu noprint mu0 = 170;
by mcrep;
var x;
output out = outtest probt = p; 
run;

* Count how many samples have p-value "probt" < 0.05 (reject Ho);
data outtest1; set outtest;
reject = (p<0.05);
run;
/*in the dataset OUTPUT1, the last column "reject" indicate if p-value <0.05
Hence the mean of column "reject" is the rejection rate.
*/

proc means data=outtest1 n mean;
var reject;
run;

*the mean of "reject" should be close to 0.05; 











