library(ggplot2)
#library package to get the mode
library(modeest)
climate <- read.csv("C:\\Users\\bartha4\\Documents\\Data Analytics/API_19_DS2_en_csv_v2_3733206.csv", sep = "\t")
View(climate)
summary(climate)
counts <- table(climate$Indicator.Code)
print(counts)
plot(counts, main="Indicator.Code(s)", xlab="Number of each indicator code")
df <- read.csv("C:\\Users\\bartha4\\Documents\\Data Analytics/API_19_DS2_en_csv_v2_3733206.csv", sep = "\t")
urbanPopYears <- table(climate$X2016, exclude = NULL)
print(urbanPopYears)
str(climate$X2016)
c2016 <- as.numeric(climate$X2016)
#the col has NA so you need to include na.rm=true
median(c2016, na.rm=TRUE)
mean(c2016, na.rm=TRUE)
#mode
mlv(c2016, method="mfv", na.rm=TRUE)
#range
range(c2016, na.rm = TRUE)
#histogram for 2016 for all the countries
hist(c2016, xlim=c(0,3.5e+09),
     main="Histogram for 2016 variable")
str(climate$X2020)
c2020 <- as.numeric(climate$X2020)
#the col has NA so you need to include na.rm=true
median(c2020, na.rm=TRUE)
mean(c2020, na.rm=TRUE)
#mode
mlv(c2020, method="mfv", na.rm=TRUE)
#range
range(c2020, na.rm = TRUE)
#histogram for 2020 for all the countries
hist(c2020,xlim=c(0,7e+09),
     main="Histogram for 2020 variable")
str(climate$X1960)
c1960 <- as.numeric(climate$X1960)
#the col has NA so you need to include na.rm=true
median(c1960, na.rm=TRUE)
mean(c1960, na.rm=TRUE)
#mode
mlv(c1960, method="mfv", na.rm=TRUE)
#range
range(c1960, na.rm = TRUE)
#histogram for 2020 for all the countries
hist(c1960,xlim=c(0,3.5e+09),
     main="Histogram for 1960 variable")
hist(c1960,xlim=c(0,1e+09),
     main="Histogram for 1960 variable")
str(climate$X1985)
c1985 <- as.numeric(climate$X1985)
#the col has NA so you need to include na.rm=true
median(c1985, na.rm=TRUE)
mean(c1985, na.rm=TRUE)
#mode
mlv(c1985, method="mfv", na.rm=TRUE)
#range
range(c1985, na.rm = TRUE)
#histogram for 2020 for all the countries
hist(c1985,xlim=c(0,2e+10),
     main="Histogram for 1985 variable")
str(climate$X2000)
c2000 <- as.numeric(climate$X2000)
#the col has NA so you need to include na.rm=true
median(c2000, na.rm=TRUE)
mean(c2000, na.rm=TRUE)
#mode
mlv(c2000, method="mfv", na.rm=TRUE)
#range
range(c2000, na.rm = TRUE)
#histogram for 2020 for all the countries
hist(c2000,xlim=c(0,6.2e+10),
     main="Histogram for 2000 variable")
boxplot(c1960,c1985,c2000,c2016,c2020, main="range in values for all the country and indicator names", names = c("1960", "1985", "2000", "2016", "2020") )
boxplot(climate[19085,35:65], main="Mortality Rate since 1990")
#boxplot(climate[19090,35:65], main="Ease of doing business ranking since 1990")
#################
#################
#################
##modeling data##
#################
#################
library(dplyr)
climate$iType <- as.factor(ifelse(climate$Indicator.Code == 'SP.URB.TOTL.IN.ZS', 1,                       
                          ifelse(climate$Indicator.Code == 'SP.URB.TOTL', 2,
                          ifelse(climate$Indicator.Code== 'SP.URB.GROW',3,
                          ifelse(climate$Indicator.Code == 'SP.POP.TOTL',4,
                          ifelse(climate$Indicator.Code == 'SP.POP.GROW',5,
                          ifelse(climate$Indicator.Code == 'SI.POV.DDAY',6,
                          ifelse(climate$Indicator.Code == 'SH.STA.MALN.ZS',7,
                          ifelse(climate$Indicator.Code == 'SH.MED.CMHW.P3',8,
                          ifelse(climate$Indicator.Code == 'SH.DYN.MORT',9,
                          ifelse(climate$Indicator.Code == 'SE.PRM.CMPT.ZS',10,
                          ifelse(climate$Indicator.Code == 'SE.ENR.PRSC.FM.ZS',11,
                          ifelse(climate$Indicator.Code == 'NV.AGR.TOTL.ZS',12,
                          ifelse(climate$Indicator.Code == 'IQ.CPA.PUBS.XQ',13,
                          ifelse(climate$Indicator.Code == 'IC.BUS.EASE.XQ',14,
                          ifelse(climate$Indicator.Code == 'ER.PTD.TOTL.ZS',15,
                          ifelse(climate$Indicator.Code == 'ER.MRN.PTMR.ZS',16,
                          ifelse(climate$Indicator.Code == 'ER.LND.PTLD.ZS',17,
                          ifelse(climate$Indicator.Code == 'ER.H2O.FWTL.ZS',18,
                          ifelse(climate$Indicator.Code == 'ER.H2O.FWTL.K3',19,
                          ifelse(climate$Indicator.Code == 'EN.URB.MCTY.TL.ZS',20,
                          ifelse(climate$Indicator.Code == 'EN.POP.EL5M.ZS',21,
                          ifelse(climate$Indicator.Code == 'EN.POP.EL5M.UR.ZS',22,
                          ifelse(climate$Indicator.Code == 'EN.POP.EL5M.RU.ZS',23,
                          ifelse(climate$Indicator.Code == 'EN.CLC.MDAT.ZS',24,
                          ifelse(climate$Indicator.Code == 'EN.CLC.GHGR.MT.CE',25,
                          ifelse(climate$Indicator.Code == 'EN.CLC.DRSK.XQ',26,
                          ifelse(climate$Indicator.Code == 'EN.ATM.SF6G.KT.CE',27,
                          ifelse(climate$Indicator.Code == 'EN.ATM.PFCG.KT.CE',28,
                          ifelse(climate$Indicator.Code == 'EN.ATM.NOXE.ZG',29,
                          ifelse(climate$Indicator.Code == 'EN.ATM.NOXE.KT.CE',30,
                          ifelse(climate$Indicator.Code == 'EN.ATM.METH.ZG',31,
                          ifelse(climate$Indicator.Code == 'EN.ATM.METH.KT.CE',32,
                          ifelse(climate$Indicator.Code == 'EN.ATM.HFCG.KT.CE',33,
                          ifelse(climate$Indicator.Code == 'EN.ATM.GHGT.ZG',34,
                          ifelse(climate$Indicator.Code == 'EN.ATM.GHGT.KT.CE',35,
                          ifelse(climate$Indicator.Code == 'EN.ATM.GHGO.ZG',36,
                          ifelse(climate$Indicator.Code == 'EN.ATM.GHGO.KT.CE',37,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.SF.ZS',38,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.SF.KT',39,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.SF.KT',40,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.PP.GD.KD',41,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.PP.GD',42,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.PC',43,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.LF.ZS',44,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.LF.KT',45,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.KT',46,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.KD.GD',47,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.GF.ZS',48,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.GF.KT',49,
                          ifelse(climate$Indicator.Code == 'EN.ATM.CO2E.EG.ZS',50,
                          ifelse(climate$Indicator.Code == 'EG.USE.PCAP.KG.OE',51,
                          ifelse(climate$Indicator.Code == 'EG.USE.ELEC.KH.PC',52,
                          ifelse(climate$Indicator.Code == 'EG.USE.COMM.GD.PP.KD',53,
                          ifelse(climate$Indicator.Code == 'EG.FEC.RNEW.ZS',54,
                          ifelse(climate$Indicator.Code == 'EG.ELC.RNWX.ZS',55,
                          ifelse(climate$Indicator.Code == 'EG.ELC.RNEW.ZS',56,
                          ifelse(climate$Indicator.Code == 'EG.ELC.PETR.ZS',57,
                          ifelse(climate$Indicator.Code == 'EG.ELC.NUCL.ZS',58,
                          ifelse(climate$Indicator.Code == 'EG.ELC.NGAS.ZS',59,
                          ifelse(climate$Indicator.Code == 'EG.ELC.HYRO.ZS',60,
                          ifelse(climate$Indicator.Code == 'EG.ELC.COAL.ZS',61,
                          ifelse(climate$Indicator.Code == 'EG.ELC.ACCS.ZS',62,
                          ifelse(climate$Indicator.Code == 'BX.KLT.DINV.WD.GD.ZS',63,
                          ifelse(climate$Indicator.Code == 'AG.YLD.CREL.KG',64,
                          ifelse(climate$Indicator.Code == 'AG.LND.PRCP.MM',65,
                          ifelse(climate$Indicator.Code == 'AG.LND.IRIG.AG.ZS',66,
                          ifelse(climate$Indicator.Code == 'AG.LND.FRST.ZS',67,
                          ifelse(climate$Indicator.Code == 'AG.LND.FRST.K2',68,
                          ifelse(climate$Indicator.Code == 'AG.LND.EL5M.ZS',69,
                          ifelse(climate$Indicator.Code == 'AG.LND.EL5M.UR.ZS',70,
                          ifelse(climate$Indicator.Code == 'AG.LND.EL5M.UR.K2',71,
                          ifelse(climate$Indicator.Code == 'AG.LND.EL5M.RU.ZS',72,
                          ifelse(climate$Indicator.Code == 'AG.LND.EL5M.RU.K2',73,
                          ifelse(climate$Indicator.Code == 'AG.LND.ARBL.ZS',74,
                          ifelse(climate$Indicator.Code == 'AG.LND.AGRI.ZS',75,
                          ifelse(climate$Indicator.Code == 'AG.LND.AGRI.K2',76,0
                          )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

cor(c2020, c2016, use = "complete.obs")
cor(c1960, c2020, use = "complete.obs")
cor(c1960, c1985, use = "complete.obs")                           
cor(c1960, c2000, use = "complete.obs") 
scatter.smooth(x=c1960, y= c1985, main="1960 ~ 1985")
scatter.smooth(x=c2016, y= c2020, main="2016 ~ 2020")
scatter.smooth(x=c1985, y= c2000, main="1985 ~ 2000")
#data cleaning
df <- data.frame(
  years = 1960:2020,
  population = c(180671000,	183691000,	186538000,189242000,191889000,194303000,196560000,198712000,200706000,202677000,205052000,207661000,209896000,	211909000,	213854000,	215973000,	218035000,	220239000,	222585000,	225055000,	227225000,	229466000,	231664000,	233792000,	235825000,	237924000,	240133000,	242289000,	244499000,	246819000,	249623000,	252981000,	256514000,	259919000,	263126000,	266278000,	269394000,	272657000,	275854000,	279040000,	282162411,	284968955,	287625193,	290107933,	292805298,	295516599,	298379912,	301231207,	304093966,	306771529,	309327143,	311583481,	313877662,	316059947,	318386329,	320738994,	323071755,	325122128,	326838199,	328329953,	329484123),
  greenhouse = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5400503.5,	5440420.62,	5704297.25,	5912971.38,	5756864.1, 5539255.42,	5868459.65,	6048233.1,	6054464.7,	6135462.9,	5896082.8,	5830249.4,	5534118,	5524777.8,	5761217.3,	5776897.6,	5731026.3,	5743632.7,	6185748,	6221391.6,	5960280,	5918730,	6000620,	6108080,	6199140,	6260710,	6426070,	6675670,	6709430,	6706400,	6861150,	6806970,	6646960,	6706390,	6787570,	6801820,	6707430,	6797860,	6619980,	6194450,	6442580,	6214430,	5968740,	6108670,	6129040,	6003650,	5907270,	5842800,	6023620, NA, NA),
  investment = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.113667809,	0.066102932,	0.099287786,	0.135402869,	0.229090182,	0.137099799,	0.155331555,	0.140261482,	0.233883413,	0.306394355,	0.585866342,	0.800738126,	0.634908483,	0.316452387,	0.624874152,	0.221941613,	0.675731298,	1.302414002,	1.086807482,	1.343240723,	1.194504107,	0.561047032,	0.464853987,	0.732369584,	0.767643589,	0.904218188,	1.209693103,	1.424065573,	2.329849229,	3.244314135,	3.405318336,	1.630116343,	1.015460357,	1.022023979,	1.749187307,	1.091876415,	2.160487842,	2.398397384,	2.318328109,	1.114843565,	1.761193112,	1.695323307,	1.545625032,	1.716613576,	1.436946698,	2.80417574,	2.53073398,	1.948643533,	1.039765408,	1.409955825,	1.008436489),
  co2 = c(2890696.1,	2880505.507,	2987207.873,	3119230.874,	3255995.306,	3390922.571,	3561878.111,	3695708.943,	3831354.94,	4024748.853,	4328904.501,	4356770.034,	4564952.958,	4770194.948,	4598487.673,	4406329.539,	4613100.668,	4742292.745,	4890861.25,	4901796.244,	4723209.677,	4535800.308,	4306748.487,	4341878.347,	4475192.132,	4492555.377,	4495463.308,	4688373.177,	4892526.068,	4955081.421,	4844990,	4808090,	4880200,	5000350,	5073230,	5126900,	5283110,	5547990,	5590700,	5610300,	5776410,	5749250,	5594160,	5659630,	5740030, 5756080,	5656580,	5740270,	5563340,	5159550,	5392870,	5172100,	4950210,	5089500,	5102580,	4982790,	4888640,	4813720,	4981300, NA, NA)
)
df
#multilinear regression
set.seed(100)
new <- data.frame(years = 2021)
trainingIndex <- sample(1:nrow(df), 0.8*nrow(df))  # row indices for training data
trainingData <- df[trainingIndex, ]  # model training data
testData  <- df[-trainingIndex, ]   # test data
lmMod <- lm(investment~years+population+greenhouse+co2, data = trainingData)
#predict
popPred <- predict(lmMod, newdata=testData)
summary(lmMod)
actuals_preds <- data.frame(cbind(actuals=testData$investment, predicteds=popPred))
head(actuals_preds)
#testing the accuracy of the multilinear regression model
correlation_accuracy <- cor(actuals_preds, use="complete.obs")
print(correlation_accuracy)
#get rid of the NA values in the actual_preds dataframe
actuals_preds_noNA <- na.omit(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds_noNA, 1, min) / apply(actuals_preds_noNA, 1, max))  
mape <- mean(abs((actuals_preds_noNA$predicteds - actuals_preds_noNA$actuals))/actuals_preds_noNA$actuals)  
print(mape)
library(MLmetrics)
mse <- MSE(y_pred=actuals_preds_noNA$predicteds,y_true=actuals_preds_noNA$actuals)
print(mse)
#linear regression
df2 <- data.frame(
  years = 1960:2020,
  population = c(180671000,	183691000,	186538000,189242000,191889000,194303000,196560000,198712000,200706000,202677000,205052000,207661000,209896000,	211909000,	213854000,	215973000,	218035000,	220239000,	222585000,	225055000,	227225000,	229466000,	231664000,	233792000,	235825000,	237924000,	240133000,	242289000,	244499000,	246819000,	249623000,	252981000,	256514000,	259919000,	263126000,	266278000,	269394000,	272657000,	275854000,	279040000,	282162411,	284968955,	287625193,	290107933,	292805298,	295516599,	298379912,	301231207,	304093966,	306771529,	309327143,	311583481,	313877662,	316059947,	318386329,	320738994,	323071755,	325122128,	326838199,	328329953,	329484123),
  greenhouse = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5400503.5,	5440420.62,	5704297.25,	5912971.38,	5756864.1, 5539255.42,	5868459.65,	6048233.1,	6054464.7,	6135462.9,	5896082.8,	5830249.4,	5534118,	5524777.8,	5761217.3,	5776897.6,	5731026.3,	5743632.7,	6185748,	6221391.6,	5960280,	5918730,	6000620,	6108080,	6199140,	6260710,	6426070,	6675670,	6709430,	6706400,	6861150,	6806970,	6646960,	6706390,	6787570,	6801820,	6707430,	6797860,	6619980,	6194450,	6442580,	6214430,	5968740,	6108670,	6129040,	6003650,	5907270,	5842800,	6023620, NA, NA),
  investment = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.113667809,	0.066102932,	0.099287786,	0.135402869,	0.229090182,	0.137099799,	0.155331555,	0.140261482,	0.233883413,	0.306394355,	0.585866342,	0.800738126,	0.634908483,	0.316452387,	0.624874152,	0.221941613,	0.675731298,	1.302414002,	1.086807482,	1.343240723,	1.194504107,	0.561047032,	0.464853987,	0.732369584,	0.767643589,	0.904218188,	1.209693103,	1.424065573,	2.329849229,	3.244314135,	3.405318336,	1.630116343,	1.015460357,	1.022023979,	1.749187307,	1.091876415,	2.160487842,	2.398397384,	2.318328109,	1.114843565,	1.761193112,	1.695323307,	1.545625032,	1.716613576,	1.436946698,	2.80417574,	2.53073398,	1.948643533,	1.039765408,	1.409955825,	1.008436489),
  co2 = c(2890696.1,	2880505.507,	2987207.873,	3119230.874,	3255995.306,	3390922.571,	3561878.111,	3695708.943,	3831354.94,	4024748.853,	4328904.501,	4356770.034,	4564952.958,	4770194.948,	4598487.673,	4406329.539,	4613100.668,	4742292.745,	4890861.25,	4901796.244,	4723209.677,	4535800.308,	4306748.487,	4341878.347,	4475192.132,	4492555.377,	4495463.308,	4688373.177,	4892526.068,	4955081.421,	4844990,	4808090,	4880200,	5000350,	5073230,	5126900,	5283110,	5547990,	5590700,	5610300,	5776410,	5749250,	5594160,	5659630,	5740030, 5756080,	5656580,	5740270,	5563340,	5159550,	5392870,	5172100,	4950210,	5089500,	5102580,	4982790,	4888640,	4813720,	4981300, NA, NA)
)
df2
lmMod <- lm(investment~years, data = df2)
#predict investment
resultInvest <- predict(lmMod, newdata = new)
print(resultInvest)
plot(df2$investment, df2$years)
#predict greenhouse
lmModGreenhouse <- lm(greenhouse~years, data = df2)
resultGreenhouse <- predict(lmModGreenhouse, newdata = new)
print(resultGreenhouse)
plot(df2$greenhouse, df2$years)
#predict co2
lmModco2 <- lm(co2~years, data = df2)
resultco2 <- predict(lmModco2, newdata = new)
print(resultco2)
plot(df2$co2, df2$years)
#predict population
lmModPop <- lm(population~years, data = df2)
resultPop <- predict(lmModPop, newdata = new)
print(resultPop)
plot(df2$population, df2$years)
df2[nrow(df2) + 1,] <- c(2021, resultPop,resultGreenhouse,  resultInvest, resultco2)
df2
plot(df2$greenhouse, df2$years)
plot(df2$investment, df2$years)
plot(df2$population, df2$years)
plot(df2$co2, df2$years)
# kmeans clustering
set.seed(100)
k.max <- 10
df_noNA <- na.omit(df)
# kmeans using greenhouse, investment, and co2 variables
# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss<- sapply(1:k.max,function(k){kmeans(df_noNA[,2:5],k,nstart = 10,iter.max = 10)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(df_noNA[,2:5],3,nstart = 10)
table(icluster$cluster,df_noNA$greenhouse, df_noNA$investment, df_noNA$co2)
# kmeans using investment variable
# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss<- sapply(1:k.max,function(k){kmeans(df_noNA[,3],k,nstart = 5,iter.max = 5)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(df_noNA[,3],1,nstart = 5)
table(icluster$cluster,df_noNA$greenhouse)
