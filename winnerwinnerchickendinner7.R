data=read.csv("CaseStudy.csv")
data=data[,-1]  ## remove ID
dim(data)
view(data)

##data=data[-(8820:8844),]###just ignore this if you only have 8819 rows
dim(data)
## Step 1 -Missing Data-Mice for out of sample data,NA.omit for in sample data
y=data$CKD
class(data)
summary(data)
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status
summary(data_in)
data_in=na.omit(data_in)
dim(data_in)
summary(data_in)


model=glm(CKD ~ Age+Female+Racegrp+Activity+PVD+CVD+Hypertension+Anemia+Diabetes, family="binomial",data=data_in)
summary(model)

####model=glm(CKD ~ Age+Female+Racegrp+Activity+PVD+CVD+Hypertension+Anemia+Diabetes, family="binomial",data=data_in)
summary(model)###CHF is not significant,do we need to keep CHF? 
#Final variables based on correlation and research
#3 Categories (Characteristics, Heart Health, Conditions)
#Age,Female,Racegrp,Activity -> Characteristics
#PVD,CVD,CHF,Hypertension -> Heart Health
#Anemia,Diabetes >-Conditions
with(model, null.deviance - deviance) 
##df
with(model, df.null - df.residual)
## pvalue of difference
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# if <.05, then model is significant from a null model (model with no variables)
# note that you can do this incrementally by adding one variable at a time.
## Step 5 - Alternate. Ho:  Model Fits the Data, Ha: Model does not Fit, Definition 5-2
## devniance
-2*logLik(model)### LogLiK- the larger the better, but there is a "-2".
              ## Thus, the smaller, the better!
## test
with(model, pchisq(deviance, df.residual, lower.tail = FALSE))##1 for good


#Step 3 Test model
phatnew=predict(model, newdata = data_in, type = "response")
phatnew


classify=ifelse(phatnew>.08,1,0)
summary(classify)

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}

cc=c_accuracy(data_in$CKD,classify)
round(cc,5)   ## Make accuracy and TPR as closer as possible##

acc=c_accuracy(data_in$CKD,classify)
money=acc[7]*1300-acc[9]*(100)

money



#Last Step - Screen Out_data
library(mice)
summary(data_out)
dim(data_out)
View(data_out)
?mice
comp_data=mice(data_out)
data_out<- complete(comp_data)
data_out=data_out[,-c(7)]
summary(data_out)
View(data_out)
write.csv(data_out,"data_out.csv")


phatnew1=predict(model, newdata = data_out, type = "response")
phatnew1
write.csv(phatnew1, "Probabilities2.csv")
#Probability column for final

classify1=ifelse(phatnew1>.08,1,0)
classify1
write.csv(classify1, "Predictions2.csv")
#prediction column for final
