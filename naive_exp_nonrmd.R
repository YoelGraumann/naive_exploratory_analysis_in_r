---
  title: "Exploratory Data Analysis in R"
author: "Yoel G"
date: "10 11 2020"
output:
  html_document: default
pdf_document: default
---
  Super Simple Exploratory Data Analysis:
  
  This R file contains many different functions which might be useful.

I think this file might be useful for statisticians especially. 
Consider this a super simple, super naive exploratory data analysis.
Hopefully you find this useful.

```{r}
shock.data.tab.96 <- read.csv("C:/Users/Yoel/Desktop/Generalized Linear Models/Shockdata/shock.data.tab.96.txt", sep="")
### new workable data
shockdata=cbind(shock.data.tab.96)
shockdata$survival[shockdata$survival==1]<-'S'
shockdata$survival[shockdata$survival==3]<-'D'
shockdata$sex[shockdata$sex==1]<-"M"
shockdata$sex[shockdata$sex==2]<-"F"
shockdata$shocktp[shockdata$shocktp==2]<-"Nonshock"
shockdata$shocktp[shockdata$shocktp==3]<-"Hypovolemic"
shockdata$shocktp[shockdata$shocktp==4]<-"Cardiogenic"
shockdata$shocktp[shockdata$shocktp==5]<-"Bacterial"
shockdata$shocktp[shockdata$shocktp==6]<-"Neurogenic"
shockdata$shocktp[shockdata$shocktp==7]<-"Other"
shockdata.factored<-cbind(shock.data.tab.96)
```


```{r}
xtabs(~ survival+sex, data = shockdata)
```

```{r}
xtabs(~survival+shocktp, data=shockdata)
```







#########################################################################################################################################################################################################################################################################################################################################################

Let's do Logistic regression. Since the response variable is Survival which is either Survived or Died.

Let's start with a super simple model. We will try to predict survival of he patient using only the gender of each patient

```{r}
shockdata$sex<-as.factor(shockdata$sex)
shockdata$survival<-as.factor(shockdata$survival)
shockdata$shocktp<-as.factor(shockdata$shocktp)
shockdata$id<-NULL
```


```{r}

glm(survival ~ .,data=shockdata,family='binomial')

```




Let's take a look at the coefficients part of the output. 
They correspond to the following model:


$Survival= -0.04445+0.82721x(Male)$

When the variable "Male" is equal to 0 when the patient is female and 1 when the patient is male.




######################################################################################################################################################################################################################################





##########################################################################################################################################################################################################
analysis for categorical variables
```{r}
### sex, survival, shocktp
sd.tab<-xtabs(~ survival+sex, data = shockdata)
round(prop.table(sd.tab),2)
```

```{r}
## EXACTLY WHAT I NEEDED. cross table 1:
#### survival and shocktype
library(gmodels)
CrossTable(shockdata$survival, shockdata$shocktp, digits=2, expected=FALSE, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, sresid=FALSE, format=c("SPSS"), dnn = c("Survival, D=died, S=survived","Shock Type"))
#xtabs(~ survival+shocktp, data = shockdata)
#xtabs(~ shocktp+sex, data = shockdata)
```




```{r}

### survival and sex

CrossTable(shockdata$survival, shockdata$sex, digits=2, expected=FALSE, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, sresid=FALSE, format=c("SPSS"), dnn = c("Survival, D=died, S=survived","Sex"))

```




```{r}

### shock type and sex

CrossTable(shockdata$sex, shockdata$shocktp, digits=2, expected=FALSE, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, sresid=FALSE, format=c("SPSS"), dnn = c("Sex, M=Male, F=Female","Shock Type"))

```






################ frequency tables:
```{r}
survival_prop=table(shock.data.tab.96$survival)/96
barplot(survival_prop,ylim=c(0,0.75),col = 'purple',xlim = c(0,7), xlab="survival")
title('1=Survived, 3=Died',cex.main=0.95,adj=0.1)

```





```{r}

survival_prop=table(shock.data.tab.96$sex)/96
barplot(survival_prop,ylim=c(0,0.65),col = 'green',xlim = c(0,7), xlab="sex")
title('1=Male,2=Female',cex.main=0.95,adj=0.1)


```

```{r}


survival_prop=table(shock.data.tab.96$shocktp)/96
barplot(survival_prop,ylim=c(0,0.65),col = 'cyan',xlim = c(0,7), xlab="Shock Type")
title('2=nonshock, 3=hypovolemic, 4=cardiogenic, 5=bacterial, 6=neurogenic, 7=other',cex.main=0.95,adj=0.1)




```




```{r}
print(table(shock.data.tab.96$survival)/96)

```







```{r}
continuousdata<-cbind(shock.data.tab.96)
continuousdata[ , c("id",'sex','survival','shocktp')]<- list(NULL)
summary(continuousdata,sd=TRUE)

```








```{r}
library(RcmdrMisc)
data(continuousdata)
options(digits=4)
numSummary(continuousdata, statistics=c("sd"))


```







```{r}

heatmap(cor(continuousdata))
cor(continuousdata)
```






```{r}

library(GGally)
library(ggplot2)
ggpairs(continuousdata)

```










```{r}



library(plyr)
library(psych)
multi.hist(continuousdata)
```





### normal corplot
```{r}

library(corrplot)
corrplot(cor(continuousdata), method='number',number.cex=0.55)



```
### corplot no hct, THIS IS THE BEST THING TO DO!
```{r}

COPY<-data.frame(continuousdata)
COPY$hgb<-NULL
COPY$sp<-NULL
COPY$dp<-NULL
COPY$at<-NULL
COPY$bsa<-NULL
COPY$ci<-NULL
library(corrplot)
corrplot(cor(COPY), method='number',number.cex=0.55)
##### Now we calculate the eigen ratio
mycorr_copy=cor(COPY)
max((eigen(mycorr_copy)$values))/min(eigen(mycorr_copy)$values)
```


### corplot no hgb
```{r}
COPY_2<-data.frame(continuousdata)
COPY_2$hgb<-NULL
library(corrplot)
corrplot(cor(COPY_2), method='number',number.cex=0.55)
##### Now we calculate the eigen ratio
mycorr_copy_2=cor(COPY_2)
max((eigen(mycorr_copy_2)$values))/min(eigen(mycorr_copy_2)$values)


```



```{r}
library(ggplot2)
library(GGally)
myvars_part_one <- c("map", "sp", "dp","height")
myvars_part_two<-c("bsa","mct","at","hct","hgb")
myvars<-c("map","sp","dp","mct","at","hct","hgb")
correlation_data_one <- shockdata[myvars_part_one]
correlation_data_two<-shockdata[myvars_part_two]
correlation_data<-shockdata[myvars]
ggscatmat(correlation_data)

```



```{r,fig.width=15,fig.height=7}

pairs(shockdata)
```

```{r}

mycorr=cor(continuousdata)
kappa(mycorr,CI=FALSEe)
max((eigen(mycorr)$values))/min(eigen(mycorr)$values)
```



```{r}
for (i in shock.data.tab.96){
  boxplot(i~shocktp,data=shock.data.tab.96,ylab =colnames(shock.data.tab.96[i,0]))}

```









```{r}
#shockdata_for_glm<-shock.data.tab.96.deepcopy()






```





######################################################################################################




SHOCK DATA FACTORED



```{r}

shockdata.factored$id<-NULL
```





```{r}
shock.data<-cbind(shock.data.tab.96)
shock.data$id<-NULL
model1<-lm(survival~.,data=shock.data)
summary(model1)
```




















#### BOXPLOTS



making a boxplot for every possibility, then we will see if i can put them all in, or not
`try to find interesting relationships.



Survival and continuous variables let's see what we get
```{r}

boxplot(age~survival,data=shock.data.tab.96,ylab="age",xlab="survival")
#we can see that there is a relation ship between a preson's age and their survival, the older they are the more likely they are to die
boxplot(height~survival,data=shock.data.tab.96,ylab="height",xlab="survival")
#seems like taller people are more likely to have survived
boxplot(sp~survival,data=shock.data.tab.96,ylab="sp",xlab="survival")
#seems like higher systolic pressure is related to survival
boxplot(map~survival,data=shock.data.tab.96,ylab="map",xlab="survival")
#same as systolic pressure, higher blood pressure is correlated with survival
boxplot(hr~survival,data=shock.data.tab.96,ylab="hr",xlab="survival")
#weird heart rate outliers for survival
boxplot(dp~survival,data=shock.data.tab.96,ylab="height",xlab="survival")
#diastolic blood pressure, like the rest of blood pressures, correlated with survived
boxplot(mvp~survival,data=shock.data.tab.96,ylab="mvp",xlab="survival")
#lower mean venous pressure correlated with survival, maybe the outliers cause the oher pressures to show a positive correlation?
boxplot(bsa~survival,data=shock.data.tab.96,ylab="bsa",xlab="survival")
#higher body surface area correlated with better survival, some outliers
boxplot(ci~survival,data=shock.data.tab.96,ylab="ci",xlab="survival")
#not really correlated with survival, maybe with the kind of shock?
boxplot(at~survival,data=shock.data.tab.96,ylab="at",xlab="survival")
#lower values correlated with survival as expected, some outliers
boxplot(mct~survival,data=shock.data.tab.96,ylab="mct",xlab="survival")
#smaller values are better, as expected.
boxplot(uo~survival,data=shock.data.tab.96,ylab="uo",xlab="survival")
#higher values are better A LOT of outliers
boxplot(vi~survival,data=shock.data.tab.96,ylab="vi",xlab="survival")
#higher vi means more survival
boxplot(rci~survival,data=shock.data.tab.96,ylab="rci",xlab="survival")
# seems like its pretty much the same for survival
boxplot(hgb~survival,data=shock.data.tab.96,ylab="hgb",xlab="survival")
# a lot more variance for survival with hgb, could e high and low
boxplot(hct~survival,data=shock.data.tab.96,ylab="hct",xlab="survival")
#higher variance for survival... can't really correlate that with survival right?
#maybe both hgb and hct should be dropped ?
#hematocrit doesnt seem to be related to survival
```




```{r}
boxplot(age~sex,data=shock.data.tab.96,ylab="age",xlab="sex")
#There's more variance in age in the women than in the men
boxplot(height~sex,data=shock.data.tab.96,ylab="height",xlab="sex")
#women are shorter, we already know that lol
boxplot(sp~sex,data=shock.data.tab.96,ylab="sp",xlab="sex")
#more variance in med than in women for the systolic pressure
boxplot(map~sex,data=shock.data.tab.96,ylab="map",xlab="sex")
#about the same for men and women, no big difference
boxplot(hr~sex,data=shock.data.tab.96,ylab="hr",xlab="sex")
#women are more likely to have higher heart right, some outliers for both sexes
boxplot(dp~sex,data=shock.data.tab.96,ylab="dp",xlab="sex")
#diastolic blood pressure, men are more likely to have a higher dp and they have more variance between themselves.
boxplot(mvp~sex,data=shock.data.tab.96,ylab="mvp",xlab="sex")
#women have higher mvp and more variance between themselves.
boxplot(bsa~sex,data=shock.data.tab.96,ylab="bsa",xlab="sex")
#higher body surface area, obviously connected to height in some way, men have higher bsa than women
boxplot(ci~sex,data=shock.data.tab.96,ylab="ci",xlab="sex")
#men have a higher cI, also more variance.
boxplot(at~sex,data=shock.data.tab.96,ylab="at",xlab="sex")
#kind of the same for men and women
boxplot(mct~sex,data=shock.data.tab.96,ylab="mct",xlab="sex")
#women have higher median mct, and higher variance
boxplot(uo~sex,data=shock.data.tab.96,ylab="uo",xlab="sex")
#more variance of uo for men than women, a lot of outliers.
boxplot(vi~sex,data=shock.data.tab.96,ylab="vi",xlab="sex")
#men have more vi but also more variance
boxplot(rci~sex,data=shock.data.tab.96,ylab="rci",xlab="sex")
# men have higher, but with more variance
boxplot(hgb~sex,data=shock.data.tab.96,ylab="hgb",xlab="sex")
# about the same for men and women, more variance for men
boxplot(hct~sex,data=shock.data.tab.96,ylab="hct",xlab="sex")
# about the same for men and women, more variance for men
```





## finally shock type and the continous variables

```{r}


boxplot(age~shocktp,data=shock.data.tab.96,ylab="age",xlab="shocktp")
#There's more variance in age in the women than in the men
boxplot(height~shocktp,data=shock.data.tab.96,ylab="height",xlab="shocktp")
#women are shorter, we already know that lol
boxplot(sp~shocktp,data=shock.data.tab.96,ylab="sp",xlab="shocktp")
#more variance in med than in women for the systolic pressure
boxplot(map~shocktp,data=shock.data.tab.96,ylab="map",xlab="shocktp")
#about the same for men and women, no big difference
boxplot(hr~shocktp,data=shock.data.tab.96,ylab="hr",xlab="shocktp")
#women are more likely to have higher heart right, some outliers for both sexes
boxplot(dp~shocktp,data=shock.data.tab.96,ylab="dp",xlab="shocktp")
#diastolic blood pressure, men are more likely to have a higher dp and they have more variance between themselves.
boxplot(mvp~shocktp,data=shock.data.tab.96,ylab="mvp",xlab="shocktp")
#women have higher mvp and more variance between themselves.
boxplot(bsa~shocktp,data=shock.data.tab.96,ylab="bsa",xlab="shocktp")
#higher body surface area, obviously connected to height in some way, men have higher bsa than women
boxplot(ci~shocktp,data=shock.data.tab.96,ylab="ci",xlab="shocktp")
#men have a higher cI, also more variance.
boxplot(at~shocktp,data=shock.data.tab.96,ylab="at",xlab="shocktp")
#kind of the same for men and women
boxplot(mct~shocktp,data=shock.data.tab.96,ylab="mct",xlab="shocktp")
#women have higher median mct, and higher variance
boxplot(uo~shocktp,data=shock.data.tab.96,ylab="uo",xlab="shocktp")
#more variance of uo for men than women, a lot of outliers.
boxplot(vi~shocktp,data=shock.data.tab.96,ylab="vi",xlab="shocktp")
#men have more vi but also more variance
boxplot(rci~shocktp,data=shock.data.tab.96,ylab="rci",xlab="shocktp")
# men have higher, but with more variance
boxplot(hgb~shocktp,data=shock.data.tab.96,ylab="hgb",xlab="shocktp")
# about the same for men and women, more variance for men
boxplot(hct~shocktp,data=shock.data.tab.96,ylab="hct",xlab="shocktp")
# about the same for men and women, more variance for men
```





#### FORMULATION OF THE GLIM





```{r}

###making the data correct
shockdata$id<-NULL
shockdata$sex<-as.factor(shockdata$sex)
shockdata$survival<-as.factor(shockdata$survival)
shockdata$shocktp<-as.factor(shockdata$shocktp)
```


### naive moodel
```{r}

shock_model<-glm(survival ~ .,family = binomial(link = 'logit'),data = shockdata)
summary(shock_model)
```


#### omitting dp,sp and hgb from the model
```{r}
test_data<-data.frame(shockdata)
#test_data$dp<-NULL
#test_data$sp<-NULL
#test_data$hgb<-NULL
#test_data$hr<-NULL
#test_data$shocktp<-NULL
#test_data$height<-NULL
#test_data$ci<-NULL
#test_data$at<-NULL
#test_data$age<-NULL
#test_data$sex<-NULL
#test_data$hct<-NULL
#test_data$mct<-NULL

shocktp_asdata<-data.frame(model.matrix(~shocktp-1,test_data))
test_data$shocktp<-NULL
sex_asdata<-data.frame(model.matrix(~sex-1,test_data))

### working on new data
new_data<-cbind(test_data,shocktp_asdata)
new_data$sex<-NULL
new_data<-cbind(new_data,sex_asdata)
#new_data$shocktpOther<-NULL
new_data$sexM<-NULL

## this is because of multicolinearity
new_data$hgb<-NULL
new_data$sp<-NULL
new_data$dp<-NULL



new_data$ci<-NULL
new_data$at<-NULL
new_data$height<-NULL
new_data$hr<-NULL
#new_data$age<-NULL
new_data$sexF<-NULL
#new_data$shocktpOther<-NULL

#new_data$shocktpHypovolemic<-NULL
#new_data$shocktpNeurogenic<-NULL
#new_data$shocktpCardiogenic<-NULL
#new_data$shocktpBacterial<-NULL
new_data$shocktpNonshock<-NULL
new_data$mct<-NULL
new_data$hct<-NULL
#new_data$rci<-NULL
new_data$vi<-NULL
new_data$uo<-NULL
new_data$map<-NULL
#new_data$bsa<-NULL
#new_data$mvp<-NULL

### new model with better omitted data

test_model<-glm(survival ~.,family = binomial(link = 'logit'),data = new_data)
summary(test_model)
```
##  diagnostics plots

```{r}
plot(test_model)
```
```{r}
# get mu.hat and y - mu.hat
mu.hat =test_model$fitted.values	# estimated mu
response.resid = residuals(test_model, type="response")
# check if var(y) = const * [E(y)]^2
par(mfrow=c(1,2))
plot(mu.hat, abs(response.resid))
plot(mu.hat, response.resid) 
par(mfrow=c(1,1))
```



```{r}
dev.resid = residuals(test_model, type="deviance")
pearson.resid = residuals(test_model, type="pearson")

par(mfrow=c(1,2))
plot(log.mu.hat, pearson.resid)
identify(log.mu.hat, pearson.resid)
plot(log.mu.hat, dev.resid)
identify(log.mu.hat, dev.resid)
par(mfrow=c(1,1))



h.ii = hatvalues(test_model)
D.ii = cooks.distance(test_model)
plot(h.ii, D.ii,main="Cook's Distance",cex=0.7)
identify(h.ii, D.ii)
plot(test_model,4)


# now try to obtain added variable plots, using av.plots{car}
#avPlot(test_model, mismatch )
#avPlot(test_model, ask=FALSE, one.page=TRUE) #for all of them (on one page) 
# added-variable plots for dist.  click on graph with left mouse button to identify pts.  when finished, click on graph with rt mouse button 
# neither of these works for a glm; they only work for a lm.  instead, get partial residual plots using cr.plot{car}.
#for a lm, we can identify pts interactively; not now
#Using cr.plot{car}.  For a lm, we can identify pts interactively; not now.
```

Added Variable Plots.


```{r}
library(car)
#windows(7,7)
(avPlots(test_model,cex=0.8))

```


#### removing outliers observations
```{r}
clean_data<-data.frame(new_data)
clean_data<-clean_data[-c(88), ]
clean_model<-glm(survival ~.,family = binomial(link = 'logit'),data = clean_data)
summary(clean_model)

```





