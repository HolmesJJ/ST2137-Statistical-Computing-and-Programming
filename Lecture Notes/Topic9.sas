*Topic 9;

* IMPORTING DATA ;
FILENAME REFFILE '/home/u59061977/ex10_1.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.example1;
	DELIMITER=" ";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

proc print data=example1;
run;

*give variable gender an indicator (dummy) variable;
data example1;
set example1;
if gender = "M" then gen= 1; *choose male = 1;
if gender = "F" then gen= 0; *and choose female = 0;
run;
*when fitting a model, variable "gen" should be used instead of "gender";



* Correlation values;
proc corr data=example1 nosimple; 
title "Example of a correlation matrix";
var height weight age;
run;
*nosimple is used to suppress the descriptive statistics;


* Scatter plot of height vs weight;
proc sgscatter  data = example1;
   plot height * weight;
run;

* Scatter plot of height vs weight classified by gender;
proc sgscatter  data = example1;
   plot height * weight
   / datalabel = gender group = gender;
run;


* A source that is easy to read about proc reg: https://stats.idre.ucla.edu/sas/library/sas-libraryoverview-of-sas-proc-reg/   ;



*Simple model: weight~height;
proc reg data=example1;
  model weight = height;
  output out=analysis P =yhat R =residual STUDENT = resid cookd= cooks H = leverage; 
*a dataset named "analysis" is stored with all information about data and the above variables;
*P = fitted, R = raw residuals, student is standardized residuals ;
run;
quit;

* Save the results as a new dataset so in the next part we can use these results (using the coefficients for example);
*More details: https://documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=9.4_3.4&docsetId=statug&docsetTarget=statug_reg_details52.htm&locale=en    ;
*using the ODS: https://documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=9.4_3.4&docsetId=statug&docsetTarget=statug_ods_toc.htm&locale=en   ;



* Multiple model: weight~height + age;
proc reg data=example1;
  model weight = height age/SS1;
run;
quit;
*SS1 is the sequential SS_R (Type I SS) as in anova table in R;


* Multiple model: weight~height + age + gen;
proc reg data=example1;
  model weight = height age gen/SS1; 
run;
quit;
*SS1 is the sequential SS_R (Type I SS) as in anova table in R;

*** Model with interaction term;
* we need to create a variable for the interaction term first;
data example1;
set example1;
height_gen = height*gen;
run;
* now can fit a model with the created interaction term above;
proc reg data=example1;
  model weight = height age gen height_gen;
run;
quit;




*Test for normality of the standardized residuals;
*we use model of weight~height where the results of model is stored in dataset "analysis";
proc univariate data=analysis normal ;
var resid;
histogram resid /normal;
qqplot /normal (mu=est sigma=est);
run;


***********Prediction of NEW OBSERVATIONS, using the simple model weight ~ height;
*For SAS, add a line to the data set with response weight being omitted, let height =64 while
other regressors missing. That is add the line . 64 . . into the original data;

*to add another line of . 64 . . to the current dataset:;
data example1; 
set example1 end=last;
output;
if last then do;
    gender = . ; *information for gender is missed;
    height = 64; *height is given, here height = 64 inches;
    weight = .;
    age =. ;
    output;
end;
run;

*Using the simple model weight ~height to predict the weight when height = 64, output is saved in table "predict";
proc reg data=example1 alpha = 0.01;
  model weight = height;
output out=predict(where=(weight=.)) p=predicted uclm=UCL_Pred lclm=LCL_Pred;
run;
quit;
*if alpha is not given, then a default alpha = 0.05 is chosen;
* Use lclm - uclm: lower and upper bounds of the confidence interval of Mean response;
* use lcl and ulc: lower and uppper bounds of the prediction interval of response. 
*ulc and lcl are the upper and lower of a (1-alpha)%CI for the response when height is given = 64;



































 