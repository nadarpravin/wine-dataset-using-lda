set.seed(123)

setwd("C:\\Users\\krishnan\\Documents\\Imarticus\\Project4WineClassification\\Dataset")

library(GGally)
library(repr)
library(scales)
library(klaR)
library(caret)
library(ggplot2)
library(moments)
library(Amelia)
library(MASS)  #for lda function
library(tidyverse)
library(ggpubr)
library(memisc)
library(psych)
library(GGally)
library(caTools)

wine<-read.csv("wine.data.txt")

View(wine)


#no header in our dataset
# gave names


colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash','Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids','Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')

view(wine)


table(wine$Type)
#checking prior probability

table(wine$Type)/nrow(wine)


str(wine)

colSums(is.na(wine))


options(repr.plot.width = 4,repr.plot.height = 3)
ggplot(aes(x=Type),data = wine)+geom_bar(fill="red",color="orange")



#creating a function to plot histogram, boxplot and scatter plot together to analysis variables.

wine_attribute <- function(attribute, varName = '', bins = 30) {
  ## Building the histogram:
  histogram <- ggplot(data = wine) +
    geom_histogram(aes(x=attribute), bins = bins,
                   fill = 'steelblue', colour='darkgrey', alpha= 0.8) +
    labs(x = varName)
  ## Histogram scaling y_log10:
  histYlog <- histogram + scale_y_log10() +
    labs(y = 'log10(count)', x= varName)
  
  ## Histogram scaling x_log10:
  histXlog <- histogram + scale_x_log10() + 
    labs(x = paste('log10(', varName,')'))
  
  ## Building the boxplot highlighting the outliers:
  outliers <- ggplot(wine, aes(x = 1, y = attribute)) + 
    geom_jitter(alpha = 0.1 ) +
    geom_boxplot(alpha = 0.2, color = 'red') + 
    labs(x ='distance from mean', y= varName)
  
  ## Arranging all the plots:
  histPlots <- ggarrange(histogram, histXlog, histYlog, ncol=1, nrow=3)
  ggarrange(outliers, histPlots,ncol=2, widths = c(1,1.5))
}


#malic acid
## plot size to 7.5 x 3 
options(repr.plot.width=7.5, repr.plot.height=3)
## How the "alcohol" attribute is distributed?
wine_attribute(wine$Malic, varName = 'Malic (mg/L)')
#Alcohol

options(repr.plot.width=7.5, repr.plot.height=3)

wine_attribute(wine$Alcohol,varName = 'alchol')

#ash
options(repr.plot.width=7.5, repr.plot.height=3)
wine_attribute(wine$Ash,varName = 'ash')  

#alcalanity
wine_attribute(wine$Alcalinity,varName = 'alcalanity')  

#magnesium
wine_attribute(wine$Magnesium,varName = 'mg')  

#phenols
wine_attribute(wine$Phenols,varName = 'phenols')  

##Flavanoids vs Type
options(repr.plot.width=6, repr.plot.height=4)  #Setting the plot size
ggplot(aes(x= factor(Type), y= Nonflavanoids), data = wine) +
  geom_jitter( alpha = .2) +
  geom_boxplot( alpha = .5,color = 'blue')+
  stat_summary(fun.y = "mean", geom = "point", color = "darkblue", 
               shape = 4, size = 4) +
  labs(x= 'Type',
       y= 'Nonflavanoids',
       title= 'Nonflavanoids Vs. Type')

pairs.panels(wine,gap=0,bg=c("red","blue","green")[wine$Type],pch = 21)  #pairsplot

wine$Type<-as.factor(wine$Type)
#splittin0
df<-sample.split(wine$Type,SplitRatio = 0.8)

df_train<-wine[df==TRUE,]  
df_test<-wine[df==FALSE,]  

dim(df_test)  
dim(df_train)  

names(df_train)
mod1<-lda(Type~.,data=df_train)
mod1
attributes(mod1)
mod1$prior
mod1$counts
mod1$means
mod1$scaling


plot(mod1,pch=10,col=c("red","blue","green"))
attributes(mod1)


pred<-predict(mod1,newdata = df_test)
pred

table(pred$class,df_test$Type)


library(devtools)


partimat(Type~.,data=df_train[,c(1,2,3,4,5,6,7)],method="lda")


pred_test<-predict(mod1,newdata = df_test)
pred_test
pred_test$class

a<-table(pred_test$class,df_test$Type)
accuracy=sum(diag(a))/sum(a)
accuracy
