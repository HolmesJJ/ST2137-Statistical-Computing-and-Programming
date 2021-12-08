* IMPORTING DATA FROM A TEXT/CSV FILE:;
FILENAME REFFILE '/home/u59061977/tablets.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.newdata;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

proc print data=newdata;
run;

/*             ANOVA         */
PROC ANOVA data = newdata; 
class lab;
model amount = lab;
means lab;
run;

* OR ANOVA can be derived by: ;

PROC NPAR1WAY ANOVA data = newdata;
class lab;
var amount;
run;

* the code below also can help to create ANOVA;
PROC GLM data=newdata;
title2 'Proc glm Analysis';
/* same as 'proc anova' except 'glm' allows residual plots but gives more junk output */
class lab;
model amount = lab;
output  out=amountfit p=yhat r=resid;
/* store fitted values and fitted residuals in dataset called 'amountfit' for later use */


* More about Anova: https://documentation.sas.com/?docsetId=statug&docsetTarget=statug_anova_syntax01.htm&docsetVersion=15.1&locale=en ;


*      Kruskal-Wallis Test;
proc npar1way data= newdata wilcoxon dscf;
class lab;
var amount;
run;


/* Multiple Comparisons: Tukey */

PROC ANOVA data =newdata;
class lab;
model amount = lab;
means lab / tukey cldiff alpha=0.05;
run;

/* Multiple Comparisons: Bonferroni */

PROC ANOVA data =newdata;
class lab;
model amount = lab;
means lab / Bon cldiff alpha=0.05;
run;


* Test for equal variances: Levene test ;
PROC ANOVA data =newdata;
class lab;
model amount = lab;
means lab / hovtest = levene alpha=0.05;
run;

* Test for equal variances: Bartlett test ;
PROC ANOVA data =newdata;
class lab;
model amount = lab;
means lab / HOVTEST=BARTLETT alpha=0.05;
run;

*Test for normality for variable amount;
proc univariate data=newdata normal ;
var amount;
histogram amount /normal;
qqplot /normal (mu=est sigma=est);
run;


*Test for normality of residuals;
proc univariate   data=amountfit plot normal;
var resid;
/* plot qq-plot of residuals and ;
/* Shapiro test of normality, which has p-value = 0.0099, similar as in R */

































 