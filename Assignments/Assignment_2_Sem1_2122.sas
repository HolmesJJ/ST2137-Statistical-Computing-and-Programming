* Assignment 2, Sem1-2122; 

* IMPORTING DATA ;
FILENAME REFFILE '/home/u59061977/house_selling_prices_OR.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.house;
	DELIMITER=",";
	GETNAMES=N; * do not get the name given in the data file;
	DATAROW=2;
RUN;

data house;
  set house(rename=  (var1 = HP var2 = price var3 = housesize var4 =acres 
  var5 = lotsize var6 = bedroom var7 = bath var8 = age var9 = garage 
  var10 = condition  var11 = age_cat ));
run;

* garage is categorical with 0, 1, we use it as indicator for garage = yes (1);
* condition is categorical with 0, 1, we use it as indicator for condition = 1 (NOT good);

* ###########  EXPLORING VARIABLE PRICE;

proc univariate data = house;
var price;
histogram price/normal;
run;
* # very skewed;


* ######### CREATE NEW VARIABLE = SQRT(PRICE) ;
data house;
  set house ;
  Sprice = sqrt(price);
run;
* get a check on the symmetriicity of variable sqrt(price);
proc univariate data = house;
var Sprice;
histogram Sprice/normal;
run;
* symmetric, but the tails looks heavier than normal ;
* should use sqrt(price) as response, it will be better; 
* if student uses price as the reponse, the part on model adequacy checking 
 later will show the violation on normality; 


*# CHECK ASSOCIATION BETWEEN CATEGORICAL VARIABLES AND RESPONSE (SQRT(PRICE));
* by boxplot ;

proc sgplot data=house;
title 'Boxplot of SQRT(price) by status of GARAGE';
vbox Sprice /category=garage;
run;


proc sgplot data=house;
title 'Boxplot of SQRT(price) by status of CONDITION';
vbox Sprice /category=condition;
run;


proc sgplot data=house;
title 'Boxplot of SQRT(price) by status of AGE';
vbox Sprice /category=Age_cat;
run;


*# CHECK ASSOCIATION BETWEEN QUANTITATIVE VARIABLES AND RESPONSE
* # CORRELATION MATRIX ;

proc corr data=house nosimple; 
title "Correlation Matrix";
var Sprice housesize acres lotsize bedroom bath age;
run;

* # comments:  
# lotsize and acres has correlation = 1. Hence we can choose one of them to add into model only.
# house size and the response has largest correlation value, 0.6887809.
# from scatter plot, it's clearest of a stronger/clearer association with the response than other variable.
# the correlation between bath and bedroom is large, we may consider to use one of the two;

* ############## MODELING
# INITIAL MODEL: conconsider few models to be the initial one: no interaction term, with interaction term then compare.
# it seems the interaction term(s) like: housesize*bath and/or bath*bedroom not helpful ;


* INITIAL MODEl M1;
proc reg data=house;
  model Sprice = housesize lotsize bedroom bath age garage condition/SS1 SS2;
run;
quit;
* variable CONDITION is highly INSIGNIFICANT, can drop it to fit new model;

proc reg data=house;
  model Sprice = housesize lotsize bedroom bath age garage/SS1 SS2;
run;
quit;
* lotsize now is the most insignificant, can drop it to fit new model;


proc reg data=house;
  model Sprice = housesize bedroom bath age garage/SS1 SS2;
run;
quit;
*# age now the most insignificant. It's our own decision to decide 
to simplify the model further or not.
this is model M3: Multiple R-squared:  0.6009,    Adjusted R-squared:  0.5906 ;

*####################  RESIDUAL ANALYSIS ;
proc reg data=house;
  model Sprice = housesize bedroom bath age garage/SS1 SS2;
  output out=analysis P =yhat STUDENT = SR cookd= cooks; 
run;
quit;


* QQ plot of SR;
proc univariate data=analysis normal ;
var SR;
histogram SR /normal;
qqplot /normal (mu=est sigma=est);
run;

*Scatter plot of SR vs fitted values;
proc sgscatter data = analysis;
   plot SR*yhat;
run;

*# COMMENTS ON RESIDUAL PLOTS:
# linearity: ok
# constant variance: ok
# normality: lightly violated. This is expected since the histogram 
 of response is symmetric but not very normal.
# Without the outliers, the normaility assumption could be flexibly acceptable.

#NOTE: one may try to use age_cat instead of age, the fitting does not improve much, 
 but model is more complex with more coefficients ;
 


*#####################  USING PRICE AS RESPONSE: everything (checking association) 
 can be done similarly as above ;
 
 









 
