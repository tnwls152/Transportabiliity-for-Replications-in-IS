# Section 5: Real-world application: FB deactivation effects on users' new knowledge (26.01.20)
rm(list=ls())

# 1) Preprocess datasets
# 1-1) Load experimental data (Allcott et al. 2020)
library(haven)
RawData <- read_dta("Use.dta file; name: Experimental data")
RawData <- as.data.frame(RawData)

# 1-2) Extract impact evaluation sample in the Allcott et al. (2020) (n=1661)
impactEvalSmpl <- RawData[(RawData$sample_main==1),]

# 1-3) Load the observational data (Asimovic et al. 2021)
Asimovic_complete_data <- read.csv("Use .csv file; name: Replicated experiment data", stringsAsFactors = FALSE) # it includes the individuals who dropped out during the experiment
final_data <- Asimovic_complete_data[Asimovic_complete_data$attrition=="0",] # The individuals in this sample are all ones who finished the endline survey
options(digits=3) # round the digits to three decimal points

# 1-4) Follow the Asimovic et al. (2021)'s pre-processing process
final_data$ethn_t <- NA
final_data[which(final_data$ethn=="1" | final_data$ethn=="-1"),]$ethn_t <- 1 #Bosniak
final_data[which(final_data$ethn=="2"|final_data$ethn=="-4"),]$ethn_t <- 2 #Serb
final_data[which(final_data$ethn=="3"|final_data$ethn=="-5"),]$ethn_t <- 3 #Croat
final_data[which(final_data$ethn=="0"),]$ethn_t <- 4 #Bosnian
final_data[which(final_data$ethn=="-3"|final_data$ethn=="-2"),]$ethn_t <- 5 #Other/Do not want to say

c_news <- NA
for (i in 1:nrow(final_data)){
  final_data$c_news[i] <- final_data$fbnews_st1[i]+final_data$fbnews_st2[i]+final_data$fbnews_st3[i]+final_data$fbnews_st4[i]+final_data$fbnews_st5[i]+final_data$fbnews_st6[i]+final_data$fbnews_st7[i]+final_data$fbnews_st8[i]
}

# 1-5) Extract necessary variables from the initial dataset
Asimovic_ObsData <- final_data[, c('c_news','treatment','freq_usage','freq_fbnews','ethn_t','gender', 'age', 'educ', 'employ1', 'imp_ethn', 'imp_cntry','politics_int')]

# 1-6) Convert some variables following Allcott's data definition
medage <- median(Asimovic_ObsData$age)
age_dummy <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if(Asimovic_ObsData$age[i] > medage) {
    Asimovic_ObsData$age_dummy[i] <- 1
  } else {
    Asimovic_ObsData$age_dummy[i] <- 0
  }
}

news_fb <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if(Asimovic_ObsData$freq_fbnews[i] == 2 | Asimovic_ObsData$freq_fbnews[i] == 3) {
    Asimovic_ObsData$news_fb[i] <- 1
  } else {
    Asimovic_ObsData$news_fb[i] <- 0
  }
}

minutes_fb <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if(Asimovic_ObsData$freq_usage[i] == 4) {
    Asimovic_ObsData$minutes_fb[i] <- 1
  } else {
    Asimovic_ObsData$minutes_fb[i] <- 0
  }
}

# 2) Table 6: Candidates for W and their means
# 2-1) Compare "Education" between Source and Target
library(fastDummies)
impactEvalSmpl_dummies <- dummy_cols(impactEvalSmpl, select_columns = 'stratum', remove_first_dummy = TRUE)
Target_Asimovic <- Asimovic_ObsData
Source_Alloctt <- impactEvalSmpl_dummies

College <- NA
for (i in 1:nrow(Source_Alloctt)){
  if(Source_Alloctt$educyears[i] >= 16) {
    Source_Alloctt$College[i] <- 1
  } else {
    Source_Alloctt$College[i] <- 0
  }
}

College <- NA
for (i in 1:nrow(Target_Asimovic)){
  if(Target_Asimovic$educ[i] == 4 | Target_Asimovic$educ[i] == 5) {
    Target_Asimovic$College[i] <- 1
  } else {
    Target_Asimovic$College[i] <- 0
  }
}

mean(Target_Asimovic$College)
mean(Source_Alloctt$College)

# 2-1-2) Change data structure for t-test
Temp1 <- Source_Alloctt["College"]
Temp2 <- Target_Asimovic["College"]
DF_College <- rbind(Temp1, Temp2)
DF_College$Group[1:2014] <- 1 
DF_College$Group[1662:2014] <- 2
rm(Temp1)
rm(Temp2)
t.test(College ~ Group, data=DF_College)

# 2-2) Compare "Age" between source and target
mean(Source_Alloctt$age_dummy)
mean(Target_Asimovic$age_dummy)

# 2-2-1) Change data structure for t-test
Temp1 <- Source_Alloctt["age_dummy"]
Temp2 <- Target_Asimovic["age_dummy"]
DF_AgeDummy <- rbind(Temp1, Temp2)
DF_AgeDummy$Group[1:2014] <- 1 
DF_AgeDummy$Group[1662:2014] <- 2
rm(Temp1)
rm(Temp2)
t.test(age_dummy ~ Group, data=DF_AgeDummy)

# 2-3) Compare "Gender" between source and target
Target_Asimovic_subset <- subset(Target_Asimovic, Target_Asimovic$gender!=0)

male <- NA
for (i in 1:nrow(Target_Asimovic_subset)){
  if(Target_Asimovic_subset$gender[i] == 1) {
    Target_Asimovic_subset$male[i] <- 1
  } else {
    Target_Asimovic_subset$male[i] <- 0
  }
}

mean(Source_Alloctt$male)
mean(Target_Asimovic_subset$male)

# 2-3-1) Change the data structure for t-test
Temp1 <- Source_Alloctt["male"]
Temp2 <- Target_Asimovic_subset["male"]
DF_Gender <- rbind(Temp1, Temp2)
DF_Gender$Group[1:2012] <- 1 
DF_Gender$Group[1662:2012] <- 2
rm(Temp1)
rm(Temp2)
t.test(male ~ Group, data=DF_Gender)

# 2-4) Compare "Frequency of FB usage" between source and target
mean(Source_Alloctt$minutes_fb)
mean(Target_Asimovic$minutes_fb)

Temp1 <- Source_Alloctt["minutes_fb"]
Temp2 <- Target_Asimovic["minutes_fb"]
DF_FBusage <- rbind(Temp1, Temp2)
DF_FBusage$Group[1:2014] <- 1 
DF_FBusage$Group[1662:2014] <- 2
rm(Temp1)
rm(Temp2)
t.test(minutes_fb ~ Group, data=DF_FBusage)


# 5) Compare "Frequency of reading news on FB" between source and target
mean(Source_Alloctt$news_fb)
mean(Target_Asimovic$news_fb)

Temp1 <- Source_Alloctt["news_fb"]
Temp2 <- Target_Asimovic["news_fb"]
DF_FBNews <- rbind(Temp1, Temp2)
DF_FBNews$Group[1:2014] <- 1 
DF_FBNews$Group[1662:2014] <- 2
rm(Temp1)
rm(Temp2)
t.test(news_fb ~ Group, data=DF_FBNews)


################################################################
# 3) Examine transportability (Table 7)
Allcott_ExpData <- impactEvalSmpl

# Note: the candidates for W is {age, gender, education, FB usage, and FB usage for getting news} 
# 3-1) Transportability; When W is frequent usage of FB. The transport formula is E(Y|do(X), W)P*(W)
# 3-1-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w0 <- aggregate(index_news ~ T + minutes_fb, data = Allcott_ExpData, mean)[1,3]
E_y_x1_w0 <- aggregate(index_news ~ T + minutes_fb, data = Allcott_ExpData, mean)[2,3]
E_y_x0_w1 <- aggregate(index_news ~ T + minutes_fb, data = Allcott_ExpData, mean)[3,3]
E_y_x1_w1 <- aggregate(index_news ~ T + minutes_fb, data = Allcott_ExpData, mean)[4,3]

# 3-1-2) Compute P*(W) from Asimovic's data
P_w0 <- table(Asimovic_ObsData$minutes_fb)[1]/nrow(Asimovic_ObsData)
P_w1 <- table(Asimovic_ObsData$minutes_fb)[2]/nrow(Asimovic_ObsData)

# 3-1-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w0*P_w0 + E_y_x0_w1*P_w1
E_Y_x1_Asimovic <- E_y_x1_w0*P_w0 + E_y_x1_w1*P_w1
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-2) Transportability; When W is education
# 3-2-1) Compute E(Y|do(X), W) from the source, Allcott's data.
table(Allcott_ExpData$educyears)
college <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if(Allcott_ExpData$educyears[i] == 16|Allcott_ExpData$educyears[i] == 19) {
    Allcott_ExpData$college[i] <- 1
  } else {
    Allcott_ExpData$college[i] <- 0
  }
}

E_y_x0_w0 <- aggregate(index_news ~ T + college, data = Allcott_ExpData, mean)[1,3]
E_y_x1_w0 <- aggregate(index_news ~ T + college, data = Allcott_ExpData, mean)[2,3]
E_y_x0_w1 <- aggregate(index_news ~ T + college, data = Allcott_ExpData, mean)[3,3]
E_y_x1_w1 <- aggregate(index_news ~ T + college, data = Allcott_ExpData, mean)[4,3]

# 3-2-2) Compute P*(W) from Asimovic's data
table(Asimovic_ObsData$educ)
college <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if(Asimovic_ObsData$educ[i] == 4|Asimovic_ObsData$educ[i] == 5) {
    Asimovic_ObsData$college[i] <- 1
  } else {
    Asimovic_ObsData$college[i] <- 0
  }
}

P_w0 <- table(Asimovic_ObsData$college)[1]/nrow(Asimovic_ObsData)
P_w1 <- table(Asimovic_ObsData$college)[2]/nrow(Asimovic_ObsData)

# 3-2-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w0*P_w0 + E_y_x0_w1*P_w1
E_Y_x1_Asimovic <- E_y_x1_w0*P_w0 + E_y_x1_w1*P_w1
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-3) Transportability; When W is the FB usage for news
# 3-3-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w0 <- aggregate(index_news ~ T + news_fb, data = Allcott_ExpData, mean)[1,3]
E_y_x1_w0 <- aggregate(index_news ~ T + news_fb, data = Allcott_ExpData, mean)[2,3]
E_y_x0_w1 <- aggregate(index_news ~ T + news_fb, data = Allcott_ExpData, mean)[3,3]
E_y_x1_w1 <- aggregate(index_news ~ T + news_fb, data = Allcott_ExpData, mean)[4,3]

# 3-3-2) Compute P*(W) from Asimovic's data
P_w0 <- table(Asimovic_ObsData$news_fb)[1]/nrow(Asimovic_ObsData)
P_w1 <- table(Asimovic_ObsData$news_fb)[2]/nrow(Asimovic_ObsData)

# 3-3-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w0*P_w0 + E_y_x0_w1*P_w1
E_Y_x1_Asimovic <- E_y_x1_w0*P_w0 + E_y_x1_w1*P_w1
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-4) When W is age
# 3-4-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w0 <- aggregate(index_news ~ T + age_dummy, data = Allcott_ExpData, mean)[1,3]
E_y_x1_w0 <- aggregate(index_news ~ T + age_dummy, data = Allcott_ExpData, mean)[2,3]
E_y_x0_w1 <- aggregate(index_news ~ T + age_dummy, data = Allcott_ExpData, mean)[3,3]
E_y_x1_w1 <- aggregate(index_news ~ T + age_dummy, data = Allcott_ExpData, mean)[4,3]

# 3-4-2) Compute P*(W) from Asimovic's data
P_w0 <- table(Asimovic_ObsData$age_dummy)[1]/nrow(Asimovic_ObsData)
P_w1 <- table(Asimovic_ObsData$age_dummy)[2]/nrow(Asimovic_ObsData)

# 3-4-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w0*P_w0 + E_y_x0_w1*P_w1
E_Y_x1_Asimovic <- E_y_x1_w0*P_w0 + E_y_x1_w1*P_w1
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-5) When W is gender
# 3-5-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w0 <- aggregate(index_news ~ T + male, data = Allcott_ExpData, mean)[1,3]
E_y_x1_w0 <- aggregate(index_news ~ T + male, data = Allcott_ExpData, mean)[2,3]
E_y_x0_w1 <- aggregate(index_news ~ T + male, data = Allcott_ExpData, mean)[3,3]
E_y_x1_w1 <- aggregate(index_news ~ T + male, data = Allcott_ExpData, mean)[4,3]

# 3-5-2)  Compute P*(W) from Asimovic's data
table(Asimovic_ObsData$gender)
male <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if(Asimovic_ObsData$gender[i] == 1) {
    Asimovic_ObsData$male[i] <- 1
  } else if (Asimovic_ObsData$gender[i] == 2){
    Asimovic_ObsData$male[i] <- 0
  } else {
    Asimovic_ObsData$male[i] <- 99
  }
}

Asimovic_ObsData_GenderNA <- Asimovic_ObsData[(Asimovic_ObsData$male!=99),]
P_w0 <- table(Asimovic_ObsData_GenderNA$male)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w1 <- table(Asimovic_ObsData_GenderNA$male)[2]/nrow(Asimovic_ObsData_GenderNA)

# 3-5-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w0*P_w0 + E_y_x0_w1*P_w1
E_Y_x1_Asimovic <- E_y_x1_w0*P_w0 + E_y_x1_w1*P_w1
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic

################## W is any combination of two variables ###################

# 3-6) Transportability; When W is the FB usage for news & frequent usage of FB
# 3-6-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + news_fb + minutes_fb, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + news_fb + minutes_fb, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + news_fb + minutes_fb, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + news_fb + minutes_fb, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + news_fb + minutes_fb, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + news_fb + minutes_fb, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + news_fb + minutes_fb, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + news_fb + minutes_fb, data = Allcott_ExpData, mean)[8,4]

# 3-6-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$minutes_fb)[1,1]/nrow(Asimovic_ObsData)
P_w01 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$minutes_fb)[1,2]/nrow(Asimovic_ObsData)
P_w10 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$minutes_fb)[2,1]/nrow(Asimovic_ObsData)
P_w11 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$minutes_fb)[2,2]/nrow(Asimovic_ObsData)

# 3-6-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-7) Transportability; When W is the FB usage for news & education
# 3-7-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + news_fb + college, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + news_fb + college, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + news_fb + college, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + news_fb + college, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + news_fb + college, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + news_fb + college, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + news_fb + college, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + news_fb + college, data = Allcott_ExpData, mean)[8,4]

# 3-7-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$college)[1,1]/nrow(Asimovic_ObsData)
P_w01 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$college)[1,2]/nrow(Asimovic_ObsData)
P_w10 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$college)[2,1]/nrow(Asimovic_ObsData)
P_w11 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$college)[2,2]/nrow(Asimovic_ObsData)

# 3-7-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-8) Transportability; When W is the frequent usage of FB & education
# 3-8-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + minutes_fb + college, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + minutes_fb + college, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + minutes_fb + college, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + minutes_fb + college, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + minutes_fb + college, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + minutes_fb + college, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + minutes_fb + college, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + minutes_fb + college, data = Allcott_ExpData, mean)[8,4]

# 3-8-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData$minutes_fb, Asimovic_ObsData$college)[1,1]/nrow(Asimovic_ObsData)
P_w01 <- table(Asimovic_ObsData$minutes_fb, Asimovic_ObsData$college)[1,2]/nrow(Asimovic_ObsData)
P_w10 <- table(Asimovic_ObsData$minutes_fb, Asimovic_ObsData$college)[2,1]/nrow(Asimovic_ObsData)
P_w11 <- table(Asimovic_ObsData$minutes_fb, Asimovic_ObsData$college)[2,2]/nrow(Asimovic_ObsData)

# 3-8-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-9) Transportability; When W is the frequent usage of FB & Age
# 3-9-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + minutes_fb + age_dummy, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + minutes_fb + age_dummy, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + minutes_fb + age_dummy, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + minutes_fb + age_dummy, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + minutes_fb + age_dummy, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + minutes_fb + age_dummy, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + minutes_fb + age_dummy, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + minutes_fb + age_dummy, data = Allcott_ExpData, mean)[8,4]

# 3-9-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData$minutes_fb, Asimovic_ObsData$age_dummy)[1,1]/nrow(Asimovic_ObsData)
P_w01 <- table(Asimovic_ObsData$minutes_fb, Asimovic_ObsData$age_dummy)[1,2]/nrow(Asimovic_ObsData)
P_w10 <- table(Asimovic_ObsData$minutes_fb, Asimovic_ObsData$age_dummy)[2,1]/nrow(Asimovic_ObsData)
P_w11 <- table(Asimovic_ObsData$minutes_fb, Asimovic_ObsData$age_dummy)[2,2]/nrow(Asimovic_ObsData)

# 3-9-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-10) Transportability; When W is the FB usage for News & Age
# 3-10-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + news_fb + age_dummy, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + news_fb + age_dummy, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + news_fb + age_dummy, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + news_fb + age_dummy, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + news_fb + age_dummy, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + news_fb + age_dummy, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + news_fb + age_dummy, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + news_fb + age_dummy, data = Allcott_ExpData, mean)[8,4]

# 3-10-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$age_dummy)[1,1]/nrow(Asimovic_ObsData)
P_w01 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$age_dummy)[1,2]/nrow(Asimovic_ObsData)
P_w10 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$age_dummy)[2,1]/nrow(Asimovic_ObsData)
P_w11 <- table(Asimovic_ObsData$news_fb, Asimovic_ObsData$age_dummy)[2,2]/nrow(Asimovic_ObsData)

# 3-10-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-11) Transportability; When W is Education & Age
# 3-11-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + college + age_dummy, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + college + age_dummy, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + college + age_dummy, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + college + age_dummy, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + college + age_dummy, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + college + age_dummy, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + college + age_dummy, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + college + age_dummy, data = Allcott_ExpData, mean)[8,4]

# 3-11-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData$college, Asimovic_ObsData$age_dummy)[1,1]/nrow(Asimovic_ObsData)
P_w01 <- table(Asimovic_ObsData$college, Asimovic_ObsData$age_dummy)[1,2]/nrow(Asimovic_ObsData)
P_w10 <- table(Asimovic_ObsData$college, Asimovic_ObsData$age_dummy)[2,1]/nrow(Asimovic_ObsData)
P_w11 <- table(Asimovic_ObsData$college, Asimovic_ObsData$age_dummy)[2,2]/nrow(Asimovic_ObsData)

# 3-11-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic



# 3-12) Transportability; When W is age & gender
# 3-12-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + age_dummy + male, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + age_dummy + male, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + age_dummy + male, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + age_dummy + male, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + age_dummy + male, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + age_dummy + male, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + age_dummy + male, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + age_dummy + male, data = Allcott_ExpData, mean)[8,4]

# 3-12-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData_GenderNA$age_dummy, Asimovic_ObsData_GenderNA$male)[1,1]/nrow(Asimovic_ObsData_GenderNA)
P_w01 <- table(Asimovic_ObsData_GenderNA$age_dummy, Asimovic_ObsData_GenderNA$male)[1,2]/nrow(Asimovic_ObsData_GenderNA)
P_w10 <- table(Asimovic_ObsData_GenderNA$age_dummy, Asimovic_ObsData_GenderNA$male)[2,1]/nrow(Asimovic_ObsData_GenderNA)
P_w11 <- table(Asimovic_ObsData_GenderNA$age_dummy, Asimovic_ObsData_GenderNA$male)[2,2]/nrow(Asimovic_ObsData_GenderNA)

# 3-12-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-13) Transportability; When W is FB usage for news & gender
# 3-13-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + news_fb + male, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + news_fb + male, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + news_fb + male, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + news_fb + male, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + news_fb + male, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + news_fb + male, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + news_fb + male, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + news_fb + male, data = Allcott_ExpData, mean)[8,4]

# 3-13-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData_GenderNA$news_fb, Asimovic_ObsData_GenderNA$male)[1,1]/nrow(Asimovic_ObsData_GenderNA)
P_w01 <- table(Asimovic_ObsData_GenderNA$news_fb, Asimovic_ObsData_GenderNA$male)[1,2]/nrow(Asimovic_ObsData_GenderNA)
P_w10 <- table(Asimovic_ObsData_GenderNA$news_fb, Asimovic_ObsData_GenderNA$male)[2,1]/nrow(Asimovic_ObsData_GenderNA)
P_w11 <- table(Asimovic_ObsData_GenderNA$news_fb, Asimovic_ObsData_GenderNA$male)[2,2]/nrow(Asimovic_ObsData_GenderNA)

# 3-13-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-14) Transportability; When W is edu & gender
# 3-14-1) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w00 <- aggregate(index_news ~ T + college + male, data = Allcott_ExpData, mean)[1,4]
E_y_x1_w00 <- aggregate(index_news ~ T + college + male, data = Allcott_ExpData, mean)[2,4]
E_y_x0_w10 <- aggregate(index_news ~ T + college + male, data = Allcott_ExpData, mean)[3,4]
E_y_x1_w10 <- aggregate(index_news ~ T + college + male, data = Allcott_ExpData, mean)[4,4]
E_y_x0_w01 <- aggregate(index_news ~ T + college + male, data = Allcott_ExpData, mean)[5,4]
E_y_x1_w01 <- aggregate(index_news ~ T + college + male, data = Allcott_ExpData, mean)[6,4]
E_y_x0_w11 <- aggregate(index_news ~ T + college + male, data = Allcott_ExpData, mean)[7,4]
E_y_x1_w11 <- aggregate(index_news ~ T + college + male, data = Allcott_ExpData, mean)[8,4]

# 3-14-2) Compute P*(W) from Asimovic's data
P_w00 <- table(Asimovic_ObsData_GenderNA$college, Asimovic_ObsData_GenderNA$male)[1,1]/nrow(Asimovic_ObsData_GenderNA)
P_w01 <- table(Asimovic_ObsData_GenderNA$college, Asimovic_ObsData_GenderNA$male)[1,2]/nrow(Asimovic_ObsData_GenderNA)
P_w10 <- table(Asimovic_ObsData_GenderNA$college, Asimovic_ObsData_GenderNA$male)[2,1]/nrow(Asimovic_ObsData_GenderNA)
P_w11 <- table(Asimovic_ObsData_GenderNA$college, Asimovic_ObsData_GenderNA$male)[2,2]/nrow(Asimovic_ObsData_GenderNA)

# 3-14-3) Transportability
E_Y_x0_Asimovic <- E_y_x0_w00*P_w00 + E_y_x0_w10*P_w10 + E_y_x0_w01*P_w01 + E_y_x0_w11*P_w11
E_Y_x1_Asimovic <- E_y_x1_w00*P_w00 + E_y_x1_w10*P_w10 + E_y_x1_w01*P_w01 + E_y_x1_w11*P_w11
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic

########### W is any combination of three variables################

# 3-15) Transportability; When W is Age, FB usage, and FB news usage
# 3-15-1) Create a strata based on the three variables in W

stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}


# 3-15-2) Compute E(Y|do(X), W) from the source, Allcott's data.
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-15-3) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$news_fb[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$news_fb[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$news_fb[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$news_fb[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$news_fb[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$news_fb[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$news_fb[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$news_fb[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData$stratum_defined)[1]/nrow(Asimovic_ObsData)
P_w2 <- table(Asimovic_ObsData$stratum_defined)[2]/nrow(Asimovic_ObsData)
P_w3 <- table(Asimovic_ObsData$stratum_defined)[3]/nrow(Asimovic_ObsData)
P_w4 <- table(Asimovic_ObsData$stratum_defined)[4]/nrow(Asimovic_ObsData)
P_w5 <- table(Asimovic_ObsData$stratum_defined)[5]/nrow(Asimovic_ObsData)
P_w6 <- table(Asimovic_ObsData$stratum_defined)[6]/nrow(Asimovic_ObsData)
P_w7 <- table(Asimovic_ObsData$stratum_defined)[7]/nrow(Asimovic_ObsData)
P_w8 <- table(Asimovic_ObsData$stratum_defined)[8]/nrow(Asimovic_ObsData)

# 3-15-4) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-16) When W is education, the frequent usage of FB, and FB for news
# 3-16-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-16-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData$stratum_defined)[1]/nrow(Asimovic_ObsData)
P_w2 <- table(Asimovic_ObsData$stratum_defined)[2]/nrow(Asimovic_ObsData)
P_w3 <- table(Asimovic_ObsData$stratum_defined)[3]/nrow(Asimovic_ObsData)
P_w4 <- table(Asimovic_ObsData$stratum_defined)[4]/nrow(Asimovic_ObsData)
P_w5 <- table(Asimovic_ObsData$stratum_defined)[5]/nrow(Asimovic_ObsData)
P_w6 <- table(Asimovic_ObsData$stratum_defined)[6]/nrow(Asimovic_ObsData)
P_w7 <- table(Asimovic_ObsData$stratum_defined)[7]/nrow(Asimovic_ObsData)
P_w8 <- table(Asimovic_ObsData$stratum_defined)[8]/nrow(Asimovic_ObsData)

# 3-16-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic



# 3-17) Transportability; When W is edu & gender & FB frequent usage
# 3-17-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-17-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)

# 3-17-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-18) W is gender & edu & age
# 3-18-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-18-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)

# 3-18-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic



# 3-19) W is gender, edu, and FB news
# 3-19-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-19-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)

# 3-19-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-20) W is gender, Age, and FB usage
# 3-20-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-20-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)

# 3-20-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-21) W is gender, Age, and FB news
# 3-21-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-21-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)

# 3-21-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-22) W is gender, FB Usage, and FB news
# 3-22-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$minutes_fb[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$minutes_fb[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$minutes_fb[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$minutes_fb[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$minutes_fb[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$minutes_fb[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$minutes_fb[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$minutes_fb[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-22-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$minutes_fb[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$minutes_fb[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$minutes_fb[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$minutes_fb[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$minutes_fb[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$minutes_fb[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$minutes_fb[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$minutes_fb[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)

# 3-22-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-23) When W is education, the frequent usage of FB, and age
# 3-23-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-23-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData$stratum_defined)[1]/nrow(Asimovic_ObsData)
P_w2 <- table(Asimovic_ObsData$stratum_defined)[2]/nrow(Asimovic_ObsData)
P_w3 <- table(Asimovic_ObsData$stratum_defined)[3]/nrow(Asimovic_ObsData)
P_w4 <- table(Asimovic_ObsData$stratum_defined)[4]/nrow(Asimovic_ObsData)
P_w5 <- table(Asimovic_ObsData$stratum_defined)[5]/nrow(Asimovic_ObsData)
P_w6 <- table(Asimovic_ObsData$stratum_defined)[6]/nrow(Asimovic_ObsData)
P_w7 <- table(Asimovic_ObsData$stratum_defined)[7]/nrow(Asimovic_ObsData)
P_w8 <- table(Asimovic_ObsData$stratum_defined)[8]/nrow(Asimovic_ObsData)

# 3-23-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-24) When W is education, FB News, and age
# 3-24-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
}
E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]

# 3-24-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$news_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$news_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$news_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$news_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$news_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$news_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$news_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$news_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 8
  }
}

P_w1 <- table(Asimovic_ObsData$stratum_defined)[1]/nrow(Asimovic_ObsData)
P_w2 <- table(Asimovic_ObsData$stratum_defined)[2]/nrow(Asimovic_ObsData)
P_w3 <- table(Asimovic_ObsData$stratum_defined)[3]/nrow(Asimovic_ObsData)
P_w4 <- table(Asimovic_ObsData$stratum_defined)[4]/nrow(Asimovic_ObsData)
P_w5 <- table(Asimovic_ObsData$stratum_defined)[5]/nrow(Asimovic_ObsData)
P_w6 <- table(Asimovic_ObsData$stratum_defined)[6]/nrow(Asimovic_ObsData)
P_w7 <- table(Asimovic_ObsData$stratum_defined)[7]/nrow(Asimovic_ObsData)
P_w8 <- table(Asimovic_ObsData$stratum_defined)[8]/nrow(Asimovic_ObsData)

# 3-24-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic

############ W is any combination of four variables################

# 3-25) W is gender, edu, age and FB news
# 3-25-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 9
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 10
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 11
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 12
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 13
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 14
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 15
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 16
  }
}

E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]
E_y_x0_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[17,3]
E_y_x0_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[19,3]
E_y_x0_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[21,3]
E_y_x0_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[23,3]
E_y_x0_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[25,3]
E_y_x0_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[27,3]
E_y_x0_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[29,3]
E_y_x0_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[31,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]
E_y_x1_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[18,3]
E_y_x1_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[20,3]
E_y_x1_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[22,3]
E_y_x1_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[24,3]
E_y_x1_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[26,3]
E_y_x1_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[28,3]
E_y_x1_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[30,3]
E_y_x1_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[32,3]

# 3-25-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 9
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 10
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 11
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 12
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 13
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 14
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 15
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 16
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)
P_w9 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[9]/nrow(Asimovic_ObsData_GenderNA)
P_w10 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[10]/nrow(Asimovic_ObsData_GenderNA)
P_w11 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[11]/nrow(Asimovic_ObsData_GenderNA)
P_w12 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[12]/nrow(Asimovic_ObsData_GenderNA)
P_w13 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[13]/nrow(Asimovic_ObsData_GenderNA)
P_w14 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[14]/nrow(Asimovic_ObsData_GenderNA)
P_w15 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[15]/nrow(Asimovic_ObsData_GenderNA)
P_w16 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[16]/nrow(Asimovic_ObsData_GenderNA)

# 3-25-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8 +
  E_y_x0_w9*P_w9 + E_y_x0_w10*P_w10 + E_y_x0_w11*P_w11 + E_y_x0_w12*P_w12 + E_y_x0_w13*P_w13 + E_y_x0_w14*P_w14 + E_y_x0_w15*P_w15 + E_y_x0_w16*P_w16
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8 +
  E_y_x1_w9*P_w9 + E_y_x1_w10*P_w10 + E_y_x1_w11*P_w11 + E_y_x1_w12*P_w12 + E_y_x1_w13*P_w13 + E_y_x1_w14*P_w14 + E_y_x1_w15*P_w15 + E_y_x1_w16*P_w16 
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic



# 3-26) W is gender, edu, age and FB usage
# 3-26-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 9
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 10
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 11
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 12
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 13
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 14
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 15
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 16
  }
}

E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]
E_y_x0_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[17,3]
E_y_x0_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[19,3]
E_y_x0_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[21,3]
E_y_x0_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[23,3]
E_y_x0_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[25,3]
E_y_x0_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[27,3]
E_y_x0_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[29,3]
E_y_x0_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[31,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]
E_y_x1_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[18,3]
E_y_x1_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[20,3]
E_y_x1_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[22,3]
E_y_x1_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[24,3]
E_y_x1_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[26,3]
E_y_x1_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[28,3]
E_y_x1_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[30,3]
E_y_x1_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[32,3]

# 3-26-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 9
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 10
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 11
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 12
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 13
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 14
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 15
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 16
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)
P_w9 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[9]/nrow(Asimovic_ObsData_GenderNA)
P_w10 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[10]/nrow(Asimovic_ObsData_GenderNA)
P_w11 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[11]/nrow(Asimovic_ObsData_GenderNA)
P_w12 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[12]/nrow(Asimovic_ObsData_GenderNA)
P_w13 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[13]/nrow(Asimovic_ObsData_GenderNA)
P_w14 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[14]/nrow(Asimovic_ObsData_GenderNA)
P_w15 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[15]/nrow(Asimovic_ObsData_GenderNA)
P_w16 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[16]/nrow(Asimovic_ObsData_GenderNA)

# 3-26-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8 +
  E_y_x0_w9*P_w9 + E_y_x0_w10*P_w10 + E_y_x0_w11*P_w11 + E_y_x0_w12*P_w12 + E_y_x0_w13*P_w13 + E_y_x0_w14*P_w14 + E_y_x0_w15*P_w15 + E_y_x0_w16*P_w16
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8 +
  E_y_x1_w9*P_w9 + E_y_x1_w10*P_w10 + E_y_x1_w11*P_w11 + E_y_x1_w12*P_w12 + E_y_x1_w13*P_w13 + E_y_x1_w14*P_w14 + E_y_x1_w15*P_w15 + E_y_x1_w16*P_w16 
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-27) W is gender, edu, FB news and FB usage
# 3-27-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 9
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 10
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 11
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 12
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 13
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 14
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 15
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 16
  }
}

E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]
E_y_x0_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[17,3]
E_y_x0_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[19,3]
E_y_x0_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[21,3]
E_y_x0_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[23,3]
E_y_x0_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[25,3]
E_y_x0_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[27,3]
E_y_x0_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[29,3]
E_y_x0_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[31,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]
E_y_x1_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[18,3]
E_y_x1_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[20,3]
E_y_x1_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[22,3]
E_y_x1_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[24,3]
E_y_x1_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[26,3]
E_y_x1_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[28,3]
E_y_x1_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[30,3]
E_y_x1_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[32,3]

# 3-27-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 9
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 10
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 11
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 12
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 13
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 14
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 15
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 16
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)
P_w9 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[9]/nrow(Asimovic_ObsData_GenderNA)
P_w10 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[10]/nrow(Asimovic_ObsData_GenderNA)
P_w11 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[11]/nrow(Asimovic_ObsData_GenderNA)
P_w12 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[12]/nrow(Asimovic_ObsData_GenderNA)
P_w13 <- 0
P_w14 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[13]/nrow(Asimovic_ObsData_GenderNA)
P_w15 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[14]/nrow(Asimovic_ObsData_GenderNA)
P_w16 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[15]/nrow(Asimovic_ObsData_GenderNA)

# 3-27-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8 +
  E_y_x0_w9*P_w9 + E_y_x0_w10*P_w10 + E_y_x0_w11*P_w11 + E_y_x0_w12*P_w12 + E_y_x0_w13*P_w13 + E_y_x0_w14*P_w14 + E_y_x0_w15*P_w15 + E_y_x0_w16*P_w16
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8 +
  E_y_x1_w9*P_w9 + E_y_x1_w10*P_w10 + E_y_x1_w11*P_w11 + E_y_x1_w12*P_w12 + E_y_x1_w13*P_w13 + E_y_x1_w14*P_w14 + E_y_x1_w15*P_w15 + E_y_x1_w16*P_w16 
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic


# 3-28) W is gender, age, FB news and FB usage
# 3-28-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 9
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 10
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 11
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 12
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 13
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 14
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 15
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 16
  }
}

E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]
E_y_x0_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[17,3]
E_y_x0_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[19,3]
E_y_x0_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[21,3]
E_y_x0_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[23,3]
E_y_x0_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[25,3]
E_y_x0_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[27,3]
E_y_x0_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[29,3]
E_y_x0_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[31,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]
E_y_x1_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[18,3]
E_y_x1_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[20,3]
E_y_x1_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[22,3]
E_y_x1_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[24,3]
E_y_x1_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[26,3]
E_y_x1_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[28,3]
E_y_x1_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[30,3]
E_y_x1_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[32,3]

# 3-28-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 9
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 10
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 11
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 12
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 13
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 14
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 15
  }
  else if((Asimovic_ObsData_GenderNA$age_dummy[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$news_fb[i]==1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 16
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)
P_w9 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[9]/nrow(Asimovic_ObsData_GenderNA)
P_w10 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[10]/nrow(Asimovic_ObsData_GenderNA)
P_w11 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[11]/nrow(Asimovic_ObsData_GenderNA)
P_w12 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[12]/nrow(Asimovic_ObsData_GenderNA)
P_w13 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[13]/nrow(Asimovic_ObsData_GenderNA)
P_w14 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[14]/nrow(Asimovic_ObsData_GenderNA)
P_w15 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[15]/nrow(Asimovic_ObsData_GenderNA)
P_w16 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[16]/nrow(Asimovic_ObsData_GenderNA)

# 3-28-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8 +
  E_y_x0_w9*P_w9 + E_y_x0_w10*P_w10 + E_y_x0_w11*P_w11 + E_y_x0_w12*P_w12 + E_y_x0_w13*P_w13 + E_y_x0_w14*P_w14 + E_y_x0_w15*P_w15 + E_y_x0_w16*P_w16
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8 +
  E_y_x1_w9*P_w9 + E_y_x1_w10*P_w10 + E_y_x1_w11*P_w11 + E_y_x1_w12*P_w12 + E_y_x1_w13*P_w13 + E_y_x1_w14*P_w14 + E_y_x1_w15*P_w15 + E_y_x1_w16*P_w16 
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic



# 3_29) W is edu, age, FB news and FB usage
# 3-29-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$age_dummy[i] == 0) & (Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 9
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 10
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 11
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 12
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 13
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i]==0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 14
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 15
  }
  else if((Allcott_ExpData$age_dummy[i] == 1) & (Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$news_fb[i]==1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 16
  }
}

E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]
E_y_x0_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[17,3]
E_y_x0_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[19,3]
E_y_x0_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[21,3]
E_y_x0_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[23,3]
E_y_x0_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[25,3]
E_y_x0_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[27,3]
E_y_x0_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[29,3]
E_y_x0_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[31,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]
E_y_x1_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[18,3]
E_y_x1_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[20,3]
E_y_x1_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[22,3]
E_y_x1_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[24,3]
E_y_x1_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[26,3]
E_y_x1_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[28,3]
E_y_x1_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[30,3]
E_y_x1_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[32,3]

# 3-29-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData)){
  if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i]==0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i]==0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i]==1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i]==1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i]==0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i]==0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i]==1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData$age_dummy[i] == 0) & (Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i]==1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 8
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i]==0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 9
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i]==0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 10
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i]==1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 11
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$college[i] == 0) & (Asimovic_ObsData$news_fb[i]==1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 12
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i]==0) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 13
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i]==0) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 14
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i]==1) & (Asimovic_ObsData$minutes_fb[i] == 0)) {
    Asimovic_ObsData$stratum_defined[i] <- 15
  }
  else if((Asimovic_ObsData$age_dummy[i] == 1) & (Asimovic_ObsData$college[i] == 1) & (Asimovic_ObsData$news_fb[i]==1) & (Asimovic_ObsData$minutes_fb[i] == 1)) {
    Asimovic_ObsData$stratum_defined[i] <- 16
  }
}

P_w1 <- table(Asimovic_ObsData$stratum_defined)[1]/nrow(Asimovic_ObsData)
P_w2 <- table(Asimovic_ObsData$stratum_defined)[2]/nrow(Asimovic_ObsData)
P_w3 <- table(Asimovic_ObsData$stratum_defined)[3]/nrow(Asimovic_ObsData)
P_w4 <- table(Asimovic_ObsData$stratum_defined)[4]/nrow(Asimovic_ObsData)
P_w5 <- table(Asimovic_ObsData$stratum_defined)[5]/nrow(Asimovic_ObsData)
P_w6 <- table(Asimovic_ObsData$stratum_defined)[6]/nrow(Asimovic_ObsData)
P_w7 <- table(Asimovic_ObsData$stratum_defined)[7]/nrow(Asimovic_ObsData)
P_w8 <- table(Asimovic_ObsData$stratum_defined)[8]/nrow(Asimovic_ObsData)
P_w9 <- table(Asimovic_ObsData$stratum_defined)[9]/nrow(Asimovic_ObsData)
P_w10 <- table(Asimovic_ObsData$stratum_defined)[10]/nrow(Asimovic_ObsData)
P_w11 <- table(Asimovic_ObsData$stratum_defined)[11]/nrow(Asimovic_ObsData)
P_w12 <- table(Asimovic_ObsData$stratum_defined)[12]/nrow(Asimovic_ObsData)
P_w13 <- 0
P_w14 <- table(Asimovic_ObsData$stratum_defined)[13]/nrow(Asimovic_ObsData)
P_w15 <- table(Asimovic_ObsData$stratum_defined)[14]/nrow(Asimovic_ObsData)
P_w16 <- table(Asimovic_ObsData$stratum_defined)[15]/nrow(Asimovic_ObsData)

# 3-29-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8 +
  E_y_x0_w9*P_w9 + E_y_x0_w10*P_w10 + E_y_x0_w11*P_w11 + E_y_x0_w12*P_w12 + E_y_x0_w13*P_w13 + E_y_x0_w14*P_w14 + E_y_x0_w15*P_w15 + E_y_x0_w16*P_w16
E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8 +
  E_y_x1_w9*P_w9 + E_y_x1_w10*P_w10 + E_y_x1_w11*P_w11 + E_y_x1_w12*P_w12 + E_y_x1_w13*P_w13 + E_y_x1_w14*P_w14 + E_y_x1_w15*P_w15 + E_y_x1_w16*P_w16 
Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic



################# W is a combination of five variables #############
# 3-30) W is gender, edu, age, FB news, and Freq usage of FB
# 3-30-1) Compute E(Y|do(X), W) from the source, Allcott's data.
stratum_defined <- NA
for (i in 1:nrow(Allcott_ExpData)){
  if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 1
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 2
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 3
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 4
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 5
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 6
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 7
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 8
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 9
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 10
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 11
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 12
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 13
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 14
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 15
  }
  else if((Allcott_ExpData$college[i] == 0) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 16
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 17
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 18
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 19
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 20
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 21
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 22
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 23
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 0) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 24
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 25
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 26
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 27
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==0) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 28
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 29
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 0) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 30
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 0)) {
    Allcott_ExpData$stratum_defined[i] <- 31
  }
  else if((Allcott_ExpData$college[i] == 1) & (Allcott_ExpData$male[i] == 1) & (Allcott_ExpData$age_dummy[i]==1) & (Allcott_ExpData$news_fb[i] == 1) & (Allcott_ExpData$minutes_fb[i] == 1)) {
    Allcott_ExpData$stratum_defined[i] <- 32
  }
}

E_y_x0_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[1,3]
E_y_x0_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[3,3]
E_y_x0_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[5,3]
E_y_x0_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[7,3]
E_y_x0_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[9,3]
E_y_x0_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[11,3]
E_y_x0_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[13,3]
E_y_x0_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[15,3]
E_y_x0_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[17,3]
E_y_x0_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[19,3]
E_y_x0_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[21,3]
E_y_x0_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[23,3]
E_y_x0_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[25,3]
E_y_x0_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[27,3]
E_y_x0_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[29,3]
E_y_x0_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[31,3]
E_y_x0_w17 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[33,3]
E_y_x0_w18 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[35,3]
E_y_x0_w19 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[37,3]
E_y_x0_w20 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[39,3]
E_y_x0_w21 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[41,3]
E_y_x0_w22 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[43,3]
E_y_x0_w23 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[45,3]
E_y_x0_w24 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[47,3]
E_y_x0_w25 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[49,3]
E_y_x0_w26 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[51,3]
E_y_x0_w27 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[53,3]
E_y_x0_w28 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[55,3]
E_y_x0_w29 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[57,3]
E_y_x0_w30 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[59,3]
E_y_x0_w31 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[61,3]
E_y_x0_w32 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[63,3]

E_y_x1_w1 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[2,3]
E_y_x1_w2 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[4,3]
E_y_x1_w3 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[6,3]
E_y_x1_w4 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[8,3]
E_y_x1_w5 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[10,3]
E_y_x1_w6 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[12,3]
E_y_x1_w7 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[14,3]
E_y_x1_w8 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[16,3]
E_y_x1_w9 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[18,3]
E_y_x1_w10 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[20,3]
E_y_x1_w11 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[22,3]
E_y_x1_w12 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[24,3]
E_y_x1_w13 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[26,3]
E_y_x1_w14 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[28,3]
E_y_x1_w15 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[30,3]
E_y_x1_w16 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[32,3]
E_y_x1_w17 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[34,3]
E_y_x1_w18 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[36,3]
E_y_x1_w19 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[38,3]
E_y_x1_w20 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[40,3]
E_y_x1_w21 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[42,3]
E_y_x1_w22 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[44,3]
E_y_x1_w23 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[46,3]
E_y_x1_w24 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[48,3]
E_y_x1_w25 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[50,3]
E_y_x1_w26 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[52,3]
E_y_x1_w27 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[54,3]
E_y_x1_w28 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[56,3]
E_y_x1_w29 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[58,3]
E_y_x1_w30 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[60,3]
E_y_x1_w31 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[62,3]
E_y_x1_w32 <- aggregate(index_news ~ T + stratum_defined, data = Allcott_ExpData, mean)[64,3]

# 3-30-2) Compute P*(W) from the target, Asimovic's data
stratum_defined <- NA
for (i in 1:nrow(Asimovic_ObsData_GenderNA)){
  if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 1
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 2
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 3
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 4
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 5
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 6
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 7
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 8
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 9
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 10
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 11
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 12
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 13
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 14
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 15
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 0) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 16
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 17
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 18
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 19
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 20
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 21
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 22
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 23
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 0) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 24
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 25
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 26
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 27
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==0) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 28
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 29
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 0) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 30
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 0)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 31
  }
  else if((Asimovic_ObsData_GenderNA$college[i] == 1) & (Asimovic_ObsData_GenderNA$male[i] == 1) & (Asimovic_ObsData_GenderNA$age_dummy[i]==1) & (Asimovic_ObsData_GenderNA$news_fb[i] == 1) & (Asimovic_ObsData_GenderNA$minutes_fb[i] == 1)) {
    Asimovic_ObsData_GenderNA$stratum_defined[i] <- 32
  }
}

P_w1 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[1]/nrow(Asimovic_ObsData_GenderNA)
P_w2 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[2]/nrow(Asimovic_ObsData_GenderNA)
P_w3 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[3]/nrow(Asimovic_ObsData_GenderNA)
P_w4 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[4]/nrow(Asimovic_ObsData_GenderNA)
P_w5 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[5]/nrow(Asimovic_ObsData_GenderNA)
P_w6 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[6]/nrow(Asimovic_ObsData_GenderNA)
P_w7 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[7]/nrow(Asimovic_ObsData_GenderNA)
P_w8 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[8]/nrow(Asimovic_ObsData_GenderNA)
P_w9 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[9]/nrow(Asimovic_ObsData_GenderNA)
P_w10 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[10]/nrow(Asimovic_ObsData_GenderNA)
P_w11 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[11]/nrow(Asimovic_ObsData_GenderNA)
P_w12 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[12]/nrow(Asimovic_ObsData_GenderNA)
P_w13 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[13]/nrow(Asimovic_ObsData_GenderNA)
P_w14 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[14]/nrow(Asimovic_ObsData_GenderNA)
P_w15 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[15]/nrow(Asimovic_ObsData_GenderNA)
P_w16 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[16]/nrow(Asimovic_ObsData_GenderNA)
P_w17 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[17]/nrow(Asimovic_ObsData_GenderNA)
P_w18 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[18]/nrow(Asimovic_ObsData_GenderNA)
P_w19 <- 0
P_w20 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[19]/nrow(Asimovic_ObsData_GenderNA)
P_w21 <- 0
P_w22 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[20]/nrow(Asimovic_ObsData_GenderNA)
P_w23 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[21]/nrow(Asimovic_ObsData_GenderNA)
P_w24 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[22]/nrow(Asimovic_ObsData_GenderNA)
P_w25 <- 0
P_w26 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[23]/nrow(Asimovic_ObsData_GenderNA)
P_w27 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[24]/nrow(Asimovic_ObsData_GenderNA)
P_w28 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[25]/nrow(Asimovic_ObsData_GenderNA)
P_w29 <- 0
P_w30 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[26]/nrow(Asimovic_ObsData_GenderNA)
P_w31 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[27]/nrow(Asimovic_ObsData_GenderNA)
P_w32 <- table(Asimovic_ObsData_GenderNA$stratum_defined)[28]/nrow(Asimovic_ObsData_GenderNA)


# 3-30-3) Calculate transportability
E_Y_x0_Asimovic <- E_y_x0_w1*P_w1 + E_y_x0_w2*P_w2 + E_y_x0_w3*P_w3 + E_y_x0_w4*P_w4 + E_y_x0_w5*P_w5 + E_y_x0_w6*P_w6 + E_y_x0_w7*P_w7 + E_y_x0_w8*P_w8 +
  E_y_x0_w9*P_w9 + E_y_x0_w10*P_w10 + E_y_x0_w11*P_w11 + E_y_x0_w12*P_w12 + E_y_x0_w13*P_w13 + E_y_x0_w14*P_w14 + E_y_x0_w15*P_w15 + E_y_x0_w16*P_w16 + 
  E_y_x0_w17*P_w17 + E_y_x0_w18*P_w18 + E_y_x0_w19*P_w19 + E_y_x0_w20*P_w20 + E_y_x0_w21*P_w21 + E_y_x0_w22*P_w22 + E_y_x0_w23*P_w23 + E_y_x0_w24*P_w24 +
  E_y_x0_w25*P_w25 + E_y_x0_w26*P_w26 + E_y_x0_w27*P_w27 + E_y_x0_w28*P_w28 + E_y_x0_w29*P_w29 + E_y_x0_w30*P_w30 + E_y_x0_w31*P_w31 + E_y_x0_w32*P_w32

E_Y_x1_Asimovic <- E_y_x1_w1*P_w1 + E_y_x1_w2*P_w2 + E_y_x1_w3*P_w3 + E_y_x1_w4*P_w4 + E_y_x1_w5*P_w5 + E_y_x1_w6*P_w6 + E_y_x1_w7*P_w7 + E_y_x1_w8*P_w8 +
  E_y_x1_w9*P_w9 + E_y_x1_w10*P_w10 + E_y_x1_w11*P_w11 + E_y_x1_w12*P_w12 + E_y_x1_w13*P_w13 + E_y_x1_w14*P_w14 + E_y_x1_w15*P_w15 + E_y_x1_w16*P_w16 + 
  E_y_x1_w17*P_w17 + E_y_x1_w18*P_w18 + E_y_x1_w19*P_w19 + E_y_x1_w20*P_w20 + E_y_x1_w21*P_w21 + E_y_x1_w22*P_w22 + E_y_x1_w23*P_w23 + E_y_x1_w24*P_w24 + 
  E_y_x1_w25*P_w25 + E_y_x1_w26*P_w26 + E_y_x1_w27*P_w27 + E_y_x1_w28*P_w28 + E_y_x1_w29*P_w29 + E_y_x1_w30*P_w30 + E_y_x1_w31*P_w31 + E_y_x1_w32*P_w32 

Causal_effect_Asimovic <- E_Y_x1_Asimovic-E_Y_x0_Asimovic









