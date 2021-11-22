# The CDR is based on a scale of 0-3: no dementia (CDR = 0), 
# questionable dementia (CDR = 0.5), MCI (CDR = 1), 
# moderate cognitive impairment (CDR = 2), 
# and severe cognitive impairment (CDR = 3).

df<-read.csv('oasis.csv')

library(utils)
library(dplyr)
library(stats)

library(tidyr)

library(plyr)
library(caret)

#We can see that 14 columns out of 22 columns have null values
NullCols<-as.data.frame(cbind(lapply(lapply(df, is.na), sum)))
print(rownames(subset(NullCols, NullCols$V1 != 0)))

names(df)
#The names of some columns may cause some problem , so we change them to more meaningful names

#Creating a copy of dataframe
new_df<-data.frame(df)
View(new_df)
#Rename columns to have more meaningful names, used Camelcasing here for easy reading
colnames(new_df)<-c('id','gender','dominantHand','Age','educLvl','ecoStatus','MiniMentalStateExamination','ClinicalDementiaRating','EstimatedTotalIntracranialVolume',
                    'NormalizeWholeBrainVolume','AtlasScalingFactor','Delay')
View(new_df)
new_df$Delay<-as.numeric(new_df$Delay)
new_df<-subset(new_df,select=-c(id,Delay,dominantHand))

new_df$ClinicalDementiaRating<-as.factor(new_df$ClinicalDementiaRating)


#It would be a good estimate to compute ecoStat of missing rows with mean of other values
ecoStat<-new_df$ecoStatus
ecoStat[is.na(ecoStat)]<-mean(ecoStat,na.rm=TRUE)
#Computed mean value,which was replaced in place of NA
print(mean(ecoStat))
ecoStat->new_df$ecoStatus

new_df<-new_df[complete.cases(new_df$educLvl),]

library(randomForest)

# Create the forest.
output.forest <- randomForest(ClinicalDementiaRating ~ ., 
                              data = new_df)
pred = predict(output.forest, newdata=new_df[3,])


x<-c(121,114,56,47,37,200,247,255,16,12,169,43,5,7,251)
mean(x)
sd(x)
