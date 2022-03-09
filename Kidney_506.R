library(corrplot)
library(dplyr)
library(psych)
library(ROCR)
library(car)

ch <- read.csv('/')
View(ch)
summary(ch)

# Dividing the test and train data as suggested in case study
ch_train <- ch[1:6000,]
ch_test <- ch[6001:8844,]

summary(ch_train)

#------------------------- Data Preparation--------------#

#Deleting NA's in dataset - Using LD Method for simplicity
ch_train_imputed <- ch_train[complete.cases(ch_train),] #or can use ** na.omit(dataframe) **
ch_train_imputed <- subset(ch_train_imputed, ch_train_imputed$ï..ID != 5934)
ch_train_imputed <- ch_train_imputed[-1]

str(ch_train_imputed)

attach(ch_train_imputed)

#Exploratory Analysis

# col_name <- colnames(ch_train_imputed)
#f = vector()
#n = vector()
#for (i in col_name)
# {
# if (is.factor(col_name[i]))
#{
# f[i] <- col_name[i]
# print(f[i])
# }

#else
#{
#n[i] <- col_name[i]
#print(n[i])
#}
#}

#a =lapply(ch_train_imputed,is.factor)
#unlist(a)


# Finding colnames which are factor and numeric
#f <- names(Filter(is.factor, ch_train_imputed))
#n <- names(Filter(is.numeric,ch_train_imputed))

#Subsetting data into numeric and factors

ch_train_fac <- ch_train_imputed %>% select(Female,Educ,Unmarried,Income,Insured,Obese,Dyslipidemia                                                  ,PVD,PoorVision,Smoker,Hypertension,Fam.Hypertension,Diabetes                                          ,Fam.Diabetes,Stroke,CVD,Fam.CVD,CHF,Anemia,Racegrp,CareSource)

ch_train_num <- ch_train_imputed %>% select(Age,Weight, Height, BMI, Waist, SBP, DBP,HDL,LDL,Total.Chol                                                   ,Activity)
target <- as.factor(ch_train_imputed$CKD)


# Creating Bins for Age
ch_train_num$agegroup <- cut(ch_train_num$Age, breaks = c(20,40,60,75,100),right = FALSE,labels = FALSE)

test1 <- subset(ch_train_num,select = c(Age, agegroup)) # for testing purpose

ch_train_fac <- as.data.frame(ch_train_fac)
ch_train_num <- as.data.frame(ch_train_num)


#-----------Exploratory Analysis-------#
# Exploring Relationships 

#Between Age and Hypertension
tbl <- table(Hypertension,ch_train_num$agegroup)
chisq.test(tbl) # Highly Related

#Between Age and Diabetes
tbl1 <- table(Diabetes,ch_train_num$agegroup)
chisq.test(tbl1)

#Between Hypertension and Activity
tbl2 <- table(Hypertension,Activity)
chisq.test(tbl2) #Significant Relationship but comparitively very less


#Between Hypertension and Smokers
tbl3 <- table(Hypertension,Smoker)
chisq.test(tbl3) # Not good enough r-squared

tbl4 <- table(Hypertension,Diabetes)
chisq.test(tbl4)

# Finding out correlation between numeric factors

M = cor(ch_train_num)
corrplot(M, method = 'number')
#corrplot(M, method = 'color')

#Dummy coding the variables

#dummy_race <- C(ch_train_fac$Racegrp,treatment)

#Race
for (i in sort(unique(ch_train_fac$Racegrp))){
  ch_train_fac[i] <-ifelse(ch_train_fac$Racegrp == i, 1,0)
  colnames(ch_train_fac)[which(names(ch_train_fac) == i)] <- paste("RaceGrp_",i,sep = "")
}

#Care Source
for (i in sort(unique(ch_train_fac$CareSource))){
  ch_train_fac[i] <-ifelse(ch_train_fac$CareSource == i, 1,0)
  colnames(ch_train_fac)[which(names(ch_train_fac) == i)] <- paste("CareSrc_",i,sep = "")
}

#Removing Additional Column of Race and Care Source
ch_train_fac <- ch_train_fac[-c(20,21,25,27)] # Removing columns care source, racegrp and making ref cat for dummy coding (white and Dr/HMO)


#Removing Age column from num dataframe
ch_train_num <- ch_train_num[-1]

#Changing categorical variables into factors

ch_train_fac_final <-as.data.frame(apply(ch_train_fac, 2, factor))

#Combining numerical and categorical data
ch_train_final <- cbind(ch_train_fac_final,ch_train_num,target)


#dummy_care <- dummy.code(ch_train_fac$CareSource)

# ------------------ Building Model on all variables ---------------------#
attach(ch_train_final)

# Not including: Female + Total Chol + Obese + Dyslipidemia + Fam.Diabetes+ BMI
formula <- target ~  Educ + Unmarried + Income + Insured +PVD + PoorVision + Smoker + Hypertension + 
  Fam.Hypertension + Diabetes + Stroke + CVD + Fam.CVD + CHF + Anemia + RaceGrp_black                      + RaceGrp_hispa + RaceGrp_other + CareSrc_clinic+ CareSrc_noplace + CareSrc_other +                      Height + Weight + Waist + SBP + DBP + HDL + LDL + Activity + agegroup

#produces model with all independent variables 
full=glm(formula, data = ch_train_final,family=binomial(logit))	
summary(full)

detach(ch_train_final)
# ------------------Model Evaluation-------------------------#
score <- predict(full,ch_train_final,type="response")
pred <- ifelse(score > 0.083 , 1,0)

#Confusion Matrix
cnf<-table(ch_train_final$target,pred)
cnf

#Accuracy
accuracy <- sum(diag(cnf))/sum(cnf) *100
print (accuracy)

# Recall
recall <- cnf[2,2]/sum(cnf[2,]) *100
print(recall)

#Precision
precision <- cnf[2,2]/sum(cnf[,2]) *100
print(precision)

#f-score
f_score <- 2*precision*recall/(precision+recall)
print(f_score)

#Check for Multi-collinearity
vif(full) # variance inflation factors
sqrt(vif(full)) > 2 # problem?

#confint(full)

#---------------------------ROCR Curves-------------------------#

#ROC Curve
pred_roc = prediction(score, target)
perf = performance(pred_roc,"tpr","fpr")
# Plotting the ROC curve
plot(perf, col = 'black', lty = 3, lwd = 3, main ="ROC Curve", colorize = T)
abline(a = 0, b = 1)


#PR Curve
pr_curve <- performance(pred_roc,"prec","rec")
plot(pr_curve, col = 'black', lty = 3, lwd = 3, main ="PR Curve", colorize = T)

#Optimal Threshold with FN cost 10 times FP cost
cost.perf <- performance(pred_roc,"cost",cost.fp = 1, cost.fn = 10)
pred_roc@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

b <- table(ch_train_imputed$Diabetes,ch_train_imputed$Hypertension,ch_train_imputed$CKD)
barplot(b)