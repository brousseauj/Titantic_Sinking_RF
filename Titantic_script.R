df.train = read.csv('train.csv')
df.test = read.csv('test.csv')

df.test$Survived= NA
all.df = rbind(df.train,df.test)

summary(df.train)
#Looking at the data we have some NA's to deal with
#and the Cabin factor is weird as well. 
#there are also some columns we can drop too. 

# The Cabin is to far gone to recover. But Age we can probably fix. 
to.drop = c('Cabin','Ticket')

all.df =all.df[,!(names(all.df) %in% to.drop)]

all.df$Survived=as.factor(ifelse(all.df$Survived==1,'Survived','Died'))
all.df$Pclass = as.factor(all.df$Pclass)
# View missing data visually
library(Amelia)
missmap(all.df,main='Missing Data',
        legend=FALSE,col = c('red','white'),)
# To recover age we have a couple of options. 
#1.) Use a Decison tree
#2.) Use median


#Method 1
set.seed(100)
all.df$treeAge = all.df$Age
library(rpart)
agefit<- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=all.df[!is.na(all.df$Age),], method="anova")
all.df$treeAge[is.na(all.df$treeAge)]=predict(agefit,all.df[is.na(all.df$treeAge),])

#Method 2
medianage = median(all.df$Age,na.rm = TRUE)


all.df$Age[is.na(all.df$Age)]=median(all.df$Age,na.rm = TRUE)

#The ages are slightly different meaning that we could see better models with one or the other. 


# Lets add an age grop category

for (i in 1:length(all.df$treeAge)){
  if(all.df$treeAge[i]<15){
    all.df$ageGroup[i] ='Under 15'
  }
  if(all.df$treeAge[i] >= 15&  all.df$treeAge[i] < 18){
    all.df$ageGroup[i] = '15-18' 
  }
  if(all.df$treeAge[i] >= 18 & all.df$treeAge[i] < 30){
    all.df$ageGroup[i] = '18-29'}
  if(all.df$treeAge[i] >=30 & all.df$treeAge[i] <50){
    all.df$ageGroup[i] = '30-49'
  }
  else(all.df$ageGroup[i] = 'Over 50')
}

all.df$ageGroup=as.factor(all.df$ageGroup)

# Embarked has 2 entries that are missing a field. 
#They are filled in as '', so we will fill these in with 'S'
#S has the highest number of passengers. 

all.df$Embarked[all.df$Embarked=='']='S'


## Pclass is a realtive estimate of socioeconomic class. 
# We know that women and childen were saved first based on historical records. 
# By multiplying Pclass and Age, we can get older-poor people and younger-rich people. 
# Could play a nice role. 

all.df$SocioFeat = as.integer(all.df$Pclass)*all.df$Age
# Higher number indicits poorer and older. 


# Family size could also play a role, where smaller families saved or bigger families?


all.df$familySize = all.df$SibSp + all.df$Parch + 1 
# 1 Family size of one means they were traveling alone. 

# Looking at the name column, we see there are different titles.
# Mr, Mrs, Ms, Don, etc.
# Let's pull those out as factors

all.df$title = as.factor(regmatches(all.df$Name, regexpr("(?<=, ).+?(?=\\.)", all.df$Name,perl = TRUE)))
all.df$Name = NULL # No longer need the name column 

all.df$Fare[is.na(all.df$Fare)]=median(all.df$Fare,na.rm = TRUE)

## Lets visualize the survivors based on these new features.
library(ggplot2)
library(ggthemes)

train.df = all.df[1:801,]


sex_plot=
  ggplot(train.df,aes(Sex))+geom_bar()+facet_grid(~Survived)+ theme_fivethirtyeight()+
  theme(axis.title=element_text(size=12))+ylab('Count')+ggtitle('Number of Males vs Females')
ggsave(filename = 'sex_plot.png',plot = sex_plot,dpi = 300)

age_plot=
  ggplot(train.df,aes(Sex,Age))+geom_boxplot()+
  facet_grid(~Survived)+theme_fivethirtyeight()+theme_fivethirtyeight()+
  xlab('')+ylab("Age")+ggtitle('Age of Passenger')+theme(axis.title=element_text(size=12))
ggsave('age_plot.png',plot=age_plot,dpi=300)

fare_plot=
  ggplot(train.df,aes(Sex,Fare))+geom_boxplot()+
  facet_grid(~Survived)+theme_fivethirtyeight()+
  xlab('')+ylab("Fare")+ggtitle('Fare of Passenger')+theme(axis.title=element_text(size=12))
ggsave('fare_plot.png',plot=fare_plot,dpi=300)
## Looks funny. Plot count of Pclass

class_count = 
  ggplot(train.df,aes(Pclass))+geom_bar()+
  theme_fivethirtyeight()+ ggtitle('Class\n 3 = Lower')+ylab("Count")+xlab('')+ facet_grid(~Survived)+
  theme(axis.title=element_text(size=12))
ggsave('class_count.png',plot=class_count,dpi = 400)


embarked_plot = 
  ggplot(train.df,aes(Embarked))+geom_bar()+facet_grid(~Survived)+theme_fivethirtyeight()+
  ggtitle('Port of Embarkation\n(C = Cherbourg; Q = Queenstown; S = Southampton)')+theme(axis.title=element_text(size=12))
ggsave('plcass_plot.png',plot=pclass_plot,dpi=300)

title_plot = 
  ggplot(train.df,aes(title))+geom_bar()+facet_grid(~Survived)+theme_fivethirtyeight()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle('Title of Passenger')
ggsave('title_plot.png',plot=title_plot,dpi=300)

familysize_plot = 
  ggplot(train.df,aes(Sex,familySize))+geom_boxplot()+
  facet_grid(~Survived)+theme_fivethirtyeight()+
  xlab('')+ylab("Size")+ggtitle('Family Size of Passenger')+theme(axis.title=element_text(size=12))
ggsave('familysize_plot.png',familysize_plot,dpi=300)

socioEconFeat_plot = 
  ggplot(train.df,aes(Sex,SocioFeat))+geom_boxplot()+
  facet_grid(~Survived)+theme_fivethirtyeight()+ggtitle('Socioeconomic Feature')+
  xlab('')+ylab("Socioeconomic Rating")+theme(axis.title=element_text(size=12))
ggsave('sociofeat.png',socioEconFeat_plot,dpi=300)

# Plot difference in tree age vs median fill age

tempdf =as.data.frame(rbind(cbind(all.df$Age,'Filled_with_Median'),cbind(all.df$treeAge,'Filled_with_Decision_Tree')))
tempdf$V1=as.integer(tempdf$V1)

agediff_plot=ggplot(tempdf,aes(x=V2,y=V1))+geom_boxplot()+theme_fivethirtyeight()
ggsave(filename = 'agediff_plot.png',agediff_plot,dpi=300)

## Build models.

#Split data back up

train = all.df[1:891,]
test = all.df[892:1309,]

# Model 1

library(caret)
set.seed(100)
cntrl = trainControl(method='oob',
                     repeats=3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
model1 <- train(Survived ~ .,
                 data = train,
                 method = "rf",
                 trControl=cntrl,
                 prox=TRUE,allowParallel=TRUE
                )
model1
plot(varImp(model1))
print(model1$finalModel)




#Looking at the plot, we can see that treeAge is a better predictor then Age, so let's remove Age
#We can also remove Parch, Embarked,Sibsp

#Model 2
model2 = randomForest(Survived~
                        Sex+Fare+SocioFeat+treeAge+Pclass+familySize+
                        SibSp+Parch+Embarked+ageGroup,
                      data = train,
                      importance=TRUE,
                      ntree=1000)
model2
varImpPlot(model2)

set.seed(100)
cntrl = trainControl(method='repeatedcv',
                     number=10,
                     repeats = 5
                     )
model2 <- train(Survived ~  
                  Sex+title+Fare+SocioFeat+treeAge+Pclass+familySize+
                  SibSp+Parch+Embarked+ageGroup,
                data = train,
                method = "rf",
                trControl=cntrl,
                allowParalell=T)
model2
plot(varImp(model2))
print(model2$finalModel)
model2$modelInfo

fit1=predict(object = model2,test)

confusionMatrix(data = test,test$Survived)

library(randomForest)

fit1a = randomForest((Survived ~  
                        Sex+title+Fare+SocioFeat+treeAge+Pclass+familySize+
                        SibSp+Parch+Embarked+ageGroup,
                      data = train)

# #model 3
# set.seed(100)
# model3 = randomForest(as.factor(Survived)~
#                         Sex+title+Fare+SocioFeat+treeAge+Pclass+familySize,
#                       data = train,
#                       importance=TRUE,
#                       ntree=2000,mtry=3)
# model3
# varImpPlot(model3)
# 
# ##Submit Model 2 to kaggle
# 
# Prediction = predict(model1,test)
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# submit$Survived = as.integer(submit$Survived)
# submit$Survived = ifelse(submit$Survived==1,0,1)
# 
# write.csv(submit, file = "model1.csv", row.names = FALSE)
# 
# ## these models perform just as poorly as a gender based model. 
# 
# # Let's try and add some new features
# 
# 
# ## model 4
# 
# set.seed(100)
# model4 = randomForest(as.factor(Survived)~
#                         Sex+title+Fare+SocioFeat+treeAge+Pclass+familySize+ageGroup,
#                       data = train,
#                       importance=TRUE,
#                       ntree=2000,mtry=3)
# model4
# varImpPlot(model4)
# 
# ##Model 5
# 
# # Let's try a different method
# 
# #PLSDA (Partial least squares discriminat analysis)
# library(caret)
# cntrl = trainControl(method='repeatedcv',
#                      repeats=3,
#                      classProbs = TRUE,
#                      summaryFunction = twoClassSummary)
# plsFit <- train(Survived ~  
#                   Sex+title+Fare+SocioFeat+treeAge+Pclass+familySize+
#                   SibSp+Parch+Embarked+ageGroup,data = train,
#                 method = "pls",
#                 metric='ROC',
#                trControl=cntrl
#                 )
# plsFit
# plot(plsFit)
# confusionMatrix(plsFit)
# 
# 
# model5_predict = predict(plsFit,test)
# plot(model5_predict)
# confusionMatrix(plsFit,test$Classes)
# 
# submit <- data.frame(PassengerId = test$PassengerId, Survived = model5_predict)
# submit$Survived = as.integer(submit$Survived)
# submit$Survived = ifelse(submit$Survived==1,0,1)
# 
# write.csv(submit, file = "model5.csv", row.names = FALSE)
# 
# 
# ## Model 6
# ## Lets use the best predictors from RF with a PLSDA
# 
# library(caret)
# cntrl = trainControl(method='oob',
#                      repeats=3,
#                      classProbs = TRUE,
#                      summaryFunction = twoClassSummary)
# plsFit2 <- train(Survived ~ Sex+title+Fare+SocioFeat+treeAge+familySize,
#                 data = train,
#                 method = "cforest",
#                 tuneLength=15,
#                 trControl=cntrl,
#                 metric='Accuracy',
#                 preProc = c('center','scale'))
# plsFit2
# plot(plsFit2)
# 
# model6_predict = predict(plsFit2,test,type = 'prob')
# confusionMatrix(plsFit2,test$Classes)
# 
# 
# submit <- data.frame(PassengerId = test$PassengerId, Survived = model6_predict)
# submit$Survived = as.integer(submit$Survived)
# submit$Survived = ifelse(submit$Survived==1,0,1)
# 
# write.csv(submit, file = "model6.csv", row.names = FALSE)
# 
# ## Model 7 GLM
# 
# 
# 
# cntrl = trainControl(method='repeatedcv',
#                      repeats=5,
#                      classProbs = TRUE,
#                      summaryFunction = twoClassSummary)
# plsFit3 <- train(Survived ~ .,data = train,
#                  method = "gbm",
#                  tuneLength=10,
#                  trControl=cntrl,
#                  metric='Kappa',
#                 number=20,
#                  preProc = c('center','scale'))
# plsFit3
# summary(plsFit3)
# 
# model7_predict = predict(plsFit3,test)
# confusionMatrix(plsFit3,test$Classes)
# 
# submit <- data.frame(PassengerId = test$PassengerId, Survived = model7_predict)
# submit$Survived = as.integer(submit$Survived)
# submit$Survived = ifelse(submit$Survived==1,0,1)
# 
# write.csv(submit, file = "model7.csv", row.names = FALSE)
# 
# ## Model 8
# 
# 
# 
# library(caret)
# cntrl = trainControl(method='oob',
#                      repeats=3,
#                      classProbs = TRUE,
#                      summaryFunction = twoClassSummary)
# cforest1 <- train(Survived ~  
#                    Sex+title+Fare+SocioFeat+treeAge+Pclass+familySize+
#                    SibSp+Parch+Embarked+ageGroup,
#                  data = train,
#                  method = "cforest",
#                  trControl=cntrl)
#         plot(cforest1)      
# summary(predict(cforest1,newdata=test))
# 
# 
# 
# 
# 
# model8_predict = predict(plsFit2,test)
# confusionMatrix(plsFit2,test$Classes)
# 
# 
# submit <- data.frame(PassengerId = test$PassengerId, Survived = model8_predict)
# submit$Survived = as.integer(submit$Survived)
# submit$Survived = ifelse(submit$Survived==1,0,1)
# 
# write.csv(submit, file = "model8.csv", row.names = FALSE)
# 
# ## Model 9
# 
# 
# 
# library(caret)
# cntrl = trainControl(method='oob',
#                      repeats=3,
#                      classProbs = TRUE,
#                      summaryFunction = twoClassSummary)
# plsFit2 <- train(Survived ~ Sex+title+Fare+SocioFeat+treeAge+familySize,
#                  data = train,
#                  method = "cforest",
#                  tuneLength=15,
#                  trControl=cntrl,
#                  metric='Accuracy',
#                  preProc = c('center','scale'))
# plsFit2
# plot(plsFit2)
# 
# model_predict = predict(plsFit2,test)
# confusionMatrix(plsFit2,test$Classes)
# 
# 
# submit <- data.frame(PassengerId = test$PassengerId, Survived = model9_predict)
# submit$Survived = as.integer(submit$Survived)
# submit$Survived = ifelse(submit$Survived==1,0,1)
# 
# write.csv(submit, file = "model9.csv", row.names = FALSE)
# 
# 
# ##Model 10
# 
# 
# 
# library(caret)
# cntrl = trainControl(method='repeatedcv',
#                      repeats=3,
#                      classProbs = TRUE,
#                      summaryFunction = twoClassSummary)
# plsFit10 <- train(Survived ~ Sex+title+Fare+SocioFeat+treeAge+familySize,
#                  data = train,
#                  method = "cforest",
#                  tuneLength=15,
#                  trControl=cntrl,
#                  metric='Accuracy',
#                  preProc = c('center','scale'))
# plsFit10
# plot(plsFit10)
# 
# model_predict10 = predict(plsFit10,test,type = 'raw')
# confusionMatrix(plsFit10,test$Classes)
# 
# 
# submit <- data.frame(PassengerId = test$PassengerId, Survived = model_predict10)
# submit$Survived = as.integer(submit$Survived)
# submit$Survived = ifelse(submit$Survived==1,0,1)
# 
# write.csv(submit, file = "model10.csv", row.names = FALSE)
# 
# 
# ##Model 11
# 
# library(caret)
# cntrl = trainControl(method='cv',
#                      repeats=3,
#                      classProbs = TRUE,
#                      savePredictions = T,
#                      summaryFunction = twoClassSummary)
# plsFit11 <- train(Survived ~ Sex+title+Fare+SocioFeat+treeAge+familySize,
#                   data = train,
#                   method = "knn",
#                   tuneLength=15,
#                   trControl=cntrl,
#                   metric='Accuracy',
#                   preProc = c('center','scale'))
# plsFit11
# plot(plsFit11)
# 
# model_predict11 = predict(plsFit11,test,type = 'raw')
# confusionMatrix(plsFit11,test$Classes)
# 
# 
# submit <- data.frame(PassengerId = test$PassengerId, Survived = model_predict11)
# submit$Survived = as.integer(submit$Survived)
# submit$Survived = ifelse(submit$Survived==1,0,1)
# 
# write.csv(submit, file = "model11.csv", row.names = FALSE)
