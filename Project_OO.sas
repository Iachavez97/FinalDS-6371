*To import test data;
FILENAME REFFILE "C:\Users\owola\Documents\MY_COURSES\FALL_2023\DS_6371_Stats_Foundations\Project\house-prices-advanced-regression-techniques\test.csv";
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV
OUT=testData;
GETNAMES=YES;
RUN;

proc print data = testData;
run;

*To import train data;
FILENAME REFFILE "C:\Users\owola\Documents\MY_COURSES\FALL_2023\DS_6371_Stats_Foundations\Project\house-prices-advanced-regression-techniques\train.csv";
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV
OUT=trainData;
GETNAMES=YES;
RUN;

proc print data = trainData;
run;

*Getting the log data for trainData;
data trainData;
set trainData;
log_GrLivArea = log(GrLivArea);
log_SalePrice = log(SalePrice);
log_overallQual = log(overallQual);
log_FullBath = log(FullBath);
run;

*Plotting the single linear regression for the sales price and living area;
*Linear-Linear plot;
proc corr; run;
symbol c=blue v= dot;
proc sgscatter data = trainData;
matrix SalePrice GrLivArea;

*Creating the Sinmple Linear Regression;
*Finding the right variable;
/*
proc sgscatter data = trainData;
matrix SalePrice MSSubClass LotArea OverallQual OverallCond MasVnrArea BsmtFinSF1 BsmtFinSF2 BsmtUnfSF TotalBsmtSF LowQualFinSF GrLivArea BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr KitchenAbvGr TotRmsAbvGrd GarageCars GarageArea	GarageQual WoodDeckSF	OpenPorchSF	EnclosedPorch 3SsnPorch ScreenPorch	PoolArea MiscVal MoSold YrSold;
*/

proc reg data = trainData;
model SalePrice = GrLivArea;
run;

*Getting the labelled cooks data and leverages;
proc reg data=trainData  plots(only label) =(CooksD RStudentByLeverage);
   model SalePrice = GrLivArea; /* can also use INFLUENCE option */
run;

data NewtrainData;
set trainData;
if _n_ = 1299 then delete;
if _n_ = 524 then delete;
run;

proc print data = NewtrainData;
run;

proc reg data = NewtrainData;
model SalePrice = GrLivArea;
run;

*Getting the labelled cooks data and leverages;
proc reg data = NewtrainData  plots(only label) =(CooksD RStudentByLeverage);
   model SalePrice = GrLivArea; /* can also use INFLUENCE option */
run;

*Running the model selection with the train/test split;
*running the forward selection;
proc glmselect data = NewtrainData plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea /selection = Forward(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

*running the Backward selection;
proc glmselect data = NewtrainData plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea /selection = Backward(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

*running the Stepwise selection;
proc glmselect data = NewtrainData plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea /selection = Stepwise(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

/*
*Running the model selection with train/test split, same seed;
*running the forward selection;
proc glmselect data = NewtrainData plots = all seed = 206145000;
partition fraction(test= 0.2);
model SalePrice = GrLivArea /selection = Forward(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

*running the Backward selection;
proc glmselect data = NewtrainData plots = all seed = 206145000;
partition fraction(test= 0.2);
model SalePrice = GrLivArea /selection = Backward(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

*running the Stepwise selection;
proc glmselect data = NewtrainData plots = all seed = 206145000;
partition fraction(test= 0.2);
model SalePrice = GrLivArea /selection = Stepwise(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;
*/

/*proc glm data = trainData;
model SalePrice = GrLivArea / solution clparm;
run;

*log-Linear plot;
proc glm data = trainData;
model log_SalePrice = GrLivArea / solution clparm;
run;

*Linear-Log plot;
proc glm data = trainData;
model SalePrice = log_GrLivArea / solution clparm;
run;

*Log-Log plot;
proc glm data = trainData;
model log_SalePrice = log_GrLivArea / solution clparm;
run;
*/



*Multiple Linear Regression;
/*proc glm data = trainData;
model SalePrice = GrLivArea FullBath / solution clparm;
run;*/

*Plotting the single linear regression for the sales price and living area;
*Linear-Linear plot;
proc corr; run;
symbol c=blue v= dot;
proc sgscatter data = trainData;
matrix SalePrice GrLivArea FullBath;

*Creating the Sinmple Linear Regression;
*Finding the right variable;
/*
proc sgscatter data = trainData;
matrix SalePrice MSSubClass LotArea OverallQual OverallCond MasVnrArea BsmtFinSF1 BsmtFinSF2 BsmtUnfSF TotalBsmtSF LowQualFinSF GrLivArea BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr KitchenAbvGr TotRmsAbvGrd GarageCars GarageArea	GarageQual WoodDeckSF	OpenPorchSF	EnclosedPorch 3SsnPorch ScreenPorch	PoolArea MiscVal MoSold YrSold;
*/

proc reg data = trainData;
model SalePrice = GrLivArea FullBath;
run;

*Getting the labelled cooks data and leverages;
proc reg data=trainData  plots(only label) =(CooksD RStudentByLeverage);
   model SalePrice = GrLivArea FullBath; /* can also use INFLUENCE option */
run;

data NewtrainData2;
set trainData;
if _n_ = 1299 then delete;
if _n_ = 524 then delete;
run;

proc print data = NewtrainData2;
run;

proc reg data = NewtrainData2;
model SalePrice = GrLivArea FullBath;
run;

*Getting the labelled cooks data and leverages;
proc reg data = NewtrainData2  plots(only label) =(CooksD RStudentByLeverage);
   model SalePrice = GrLivArea FullBath; /* can also use INFLUENCE option */
run;

*running the forward selection;
proc glmselect data = NewtrainData2 plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea FullBath /selection = Forward(select=CV choose=CV stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

*running the Backward selection;
proc glmselect data = NewtrainData2 plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea FullBath /selection = Backward(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

*running the Stepwise selection;
proc glmselect data = NewtrainData2 plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea FullBath /selection = Stepwise(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;



*Additional Multiple Linear Regression (SaalePrice ~ GrLivArea + OverallQual);
proc corr; run;
symbol c=blue v= dot;
proc sgscatter data = trainData;
matrix SalePrice GrLivArea OverallQual;

proc reg data = trainData;
model SalePrice = GrLivArea OverallQual;
run;

*Getting the labelled cooks data and leverages;
proc reg data=trainData  plots(only label) =(CooksD RStudentByLeverage);
   model SalePrice = GrLivArea OverallQual; /* can also use INFLUENCE option */
run;

data NewtrainData3;
set trainData;
if _n_ = 1299 then delete;
if _n_ = 524 then delete;
run;

proc print data = NewtrainData3;
run;

proc reg data = NewtrainData3;
model SalePrice = GrLivArea OverallQual;
run;

*Getting the labelled cooks data and leverages;
proc reg data = NewtrainData3  plots(only label) =(CooksD RStudentByLeverage);
   model SalePrice = GrLivArea OverallQual; /* can also use INFLUENCE option */
run;

*running the forward selection;
proc glmselect data = NewtrainData3 plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea OverallQual /selection = Forward(select=CV choose=CV stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

*running the Backward selection;
proc glmselect data = NewtrainData3 plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea OverallQual /selection = Backward(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;

*running the Stepwise selection;
proc glmselect data = NewtrainData3 plots = all;
partition fraction(test= 0.2);
model SalePrice = GrLivArea OverallQual /selection = Stepwise(stop=CV) cvmethod=random(5) stats = adjrsq CVDETAILS;
run;
