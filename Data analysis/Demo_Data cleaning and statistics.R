
# a simple R demo by Yun Wen 
# supervised by Dr Walter van Heuven & Dr Ruth Filik

rm(list=ls())

# intall packages for mixed effect model and bayes factor
install.packages("lme4", repos = "http://cran.us.r-project.org")
install.packages("BayesFactor", repos = "http://cran.us.r-project.org")

# set working directory
data_path ="/Users/User/desktop/data"
setwd(data_path)

# Activate libraries
library(Matrix)
library(lme4)
library(languageR)
library(plyr)
library(coda)
library(BayesFactor)

#################################
######## import data ###########
# there are 32 participants in this experiement
# data collected from psychopy with all file names combining participant number and "_LDT.csv"
# e.g., participant 1: 1_LDT.csv
# this is a lexical decision exp with 2 conditions: word frequency (low vs. high) and cognate status (cognate vs. non-cognate)

subjList = as.factor(c(1:32))

mydata = data.frame()

for (i in 1:length(subjList) ) {
  dataname = paste(subjList[i], "_LDT.csv", sep = "") 
  # read individual dataset
  sub_data = read.csv(dataname)
  # remove practice data
  sub_data = subset(sub_data,sub_data$trials_2.thisIndex != "")
  
  # select useful columns
  sub_data = subset(sub_data, select=c("cognate","frequency","stim",
                                       "response.keys","response.corr","response.rt",
                                       "participant","gender","age"))
  
  # check data: print number of rows
  cat(paste("S", subjList[i], sep = ""), "has", nrow(sub_data), "rows\n" )
  
  mydata =   rbind(mydata,sub_data)
  rm(sub_data)
}

##############################################
# data cleaning and merging

# force int to factor
mydata$participant= as.factor(mydata$participant)

# rename columns
mydata = rename(mydata, c("participant"="subjnr","response.corr"="ACC"))

#transform RT
mydata$RT = mydata$response.rt*1000

# merge subject data(lextale)
subdata = read.table("subj.txt", header=TRUE,sep="\t")
alldata = merge(mydata,subdata,by="subjnr")

# error percentage per subject
err_sj = with(mydata,aggregate(ACC,list(subjnr),mean))
colnames(err_sj) = c("subjnr","err")
mydata = merge(mydata,err_sj,by="subjnr")

# remove errors
corr_data = subset(mydata,mydata$ACC > 0)

# remove outliers
n1 = nrow(corr_data)
clean_data = subset(corr_data,corr_data$RT > 300 & corr_data$RT < 2500)
n2 =nrow(clean_data)
# outlier percentage 
out_perc = (n1 - n2)/n1

# means per condition
m = with(clean_data,aggregate(RT,list(cognate,frequency),mean))
round(m,0)
print(m)

#################################################
# anova F1 analysis (RT)

f1_RT = with(clean_data,aggregate(RT,list(frequency,cognate,subjnr),mean))
colnames(f1_RT) = c("frequency","cognate","subjnr","RT")

# delete subject
de_sub = c("28","29","30")

for (i in 1:length(de_sub) ) {
  f1_RT = subset(f1_RT,f1_RT$subjnr !=  de_sub[i])
  
}

f1_RT.aov = aov(RT ~ (frequency*cognate) + Error(subjnr/(frequency*cognate)),f1_RT)
summary(f1_RT.aov)
print(model.tables(f1_RT.aov,"means"),digits=3)

#bayes factor
bf_f1RT = anovaBF(RT ~ frequency*cognate + subjnr, data = f1_RT, 
                  whichRandom ="subjnr")
summary(bf_f1RT)

#################################################
# mixed effect model
model.null = lmer(RT ~ 1+ (1|subjnr) + (1|stim), clean_data )

model.1 = lmer(RT ~ frequency*cognate + (1|subjnr) + (1|stim),clean_data )
summary(model.1)
R_demo_yun.txt
