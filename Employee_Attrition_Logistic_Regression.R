setwd("C:/Users/Aruntamil/Documents/R/R_WorkingDirectory/Logistic Regression/HRAssignment")
#setwd("D:/One Drive/OneDrive - Tata Interactive Systems/Projects/UpGradDataAnalytics/Course 3/Logistic Regression/Case Study")
#Logistic regression model to analyse attrition of XYZ company
#Include libraries that are required 
library(MASS)
library(dplyr)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(ggplot2)
library(caTools)
library(ROCR)

#Read CSV files into dataframes
survey.employee.data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general.employee.data <- read.csv("general_data.csv", stringsAsFactors = F)
in.time.employee.data <- read.csv("in_time.csv", stringsAsFactors = F)
survey.manager.data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out.time.employee.data <- read.csv("out_time.csv", stringsAsFactors = F)

#Verify Employee ID is same across all dataset
setdiff(general.employee.data$EmployeeID,in.time.employee.data$X) 
setdiff(general.employee.data$EmployeeID,out.time.employee.data$X) 
setdiff(general.employee.data$EmployeeID,survey.employee.data$EmployeeID) 
setdiff(general.employee.data$EmployeeID,survey.manager.data$EmployeeID) 

#Convert intime and outtime into Posixct seconds
in.time.employee.data[,-1] <- lapply(in.time.employee.data[,-1], function(x) {as.POSIXct(x,format = "%Y-%m-%d %H:%M:%S",origin="1970-01-01")})
out.time.employee.data[,-1] <- lapply(out.time.employee.data[,-1], function(x) {as.POSIXct(x,format = "%Y-%m-%d %H:%M:%S",origin="1970-01-01")})

#Calculate mean time in office by subtracting intime and outtime for each employee
daily.hours.spent <-data.frame(lapply(out.time.employee.data[,-1] - in.time.employee.data[,-1],function(x) {as.numeric(x)}))
daily.average.hours.spent <- data.frame(apply(daily.hours.spent,1,function(x) {mean(x,na.rm = TRUE)}))
daily.average.hours.spent <- cbind(out.time.employee.data[,1],daily.average.hours.spent)
colnames(daily.average.hours.spent)<-c("EmployeeID","MeanTimeInOffice")
daily.average.hours.spent$MeanTimeInOffice<-round(daily.average.hours.spent$MeanTimeInOffice,digit=2)

#Merge all datasets into a single master dataset with all columns
employee.hr.data <- merge(general.employee.data,survey.employee.data,by="EmployeeID",all=F)
employee.hr.data <- merge(employee.hr.data,survey.manager.data,by="EmployeeID",all = F)
employee.hr.data <- merge(employee.hr.data,daily.average.hours.spent,by="EmployeeID",all=F)

attrition.percentage<-length(which(employee.hr.data$Attrition=="Yes"))/nrow(employee.hr.data)

#Data Preparation
#Check for NA values
sapply(employee.hr.data, function(x) sum(is.na(x)))

#Remove records with NA values
employee.hr.data <- na.omit(employee.hr.data)
sapply(employee.hr.data, function(x) sum(is.na(x)))

#Replace levels with categorical values
employee.hr.data$Education<-recode(employee.hr.data$Education,"1='Below College';2='College';3='Bachelor';4='Master';5='Doctor'")
employee.hr.data$EnvironmentSatisfaction<-recode(employee.hr.data$EnvironmentSatisfaction,"1='Low';2='Medium';3='High';4='Very High'")
employee.hr.data$JobInvolvement<-recode(employee.hr.data$JobInvolvement,"1='Low';2='Medium';3='High';4='Very High'")
employee.hr.data$JobSatisfaction<-recode(employee.hr.data$JobSatisfaction,"1='Low';2='Medium';3='High';4='Very High'")
employee.hr.data$PerformanceRating<-recode(employee.hr.data$PerformanceRating,"1='Low';2='Good';3='Excellent';4='Outstanding'")
employee.hr.data$WorkLifeBalance<-recode(employee.hr.data$WorkLifeBalance,"1='Bad';2='Good';3='Better';4='Best'")

#Remove columns that have only one unique value. THis removes three columns - EmployeeCount,Over18, StandardHours
employee.hr.data<-Filter(function(x)(length(unique(x))>1), employee.hr.data)


#Exploratory data analysis
#Stacked barchart for categorical values

univariate_categorical <- function(dataset,var,var_name,var_fill){
  
  dataset %>% ggplot(aes(x = as.factor(var),fill=var_fill )) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 1) +
    scale_y_continuous(labels=scales::percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)
    ) 
}


bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(employee.hr.data, aes(x=Gender,fill=Attrition))+ geom_bar(), 
          ggplot(employee.hr.data, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

univariate_categorical(employee.hr.data,employee.hr.data$Attrition,"Retention and Attrition %",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$Education,"Education Distribution",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$EducationField,"Education Field Distribution",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$Department,"Department Distribution",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$JobRole,"Job Role Distribution",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))

#R&D has higher attrition
#Employees with bachelors and master have similar attrition levels
#Research scientisits, lab technicians, and sales executives have similar but high attrition level
#These observaions are consistent with low job satisfaction levels (see charts below)

plot_grid(ggplot(employee.hr.data, aes(x=Gender,fill=JobSatisfaction))+ geom_bar(), 
          ggplot(employee.hr.data, aes(x=Education,fill=JobSatisfaction))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=EducationField,fill=JobSatisfaction))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=Department,fill=JobSatisfaction))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=JobRole,fill=JobSatisfaction))+ geom_bar()+bar_theme1,
          align = "h") 

plot_grid(ggplot(employee.hr.data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(employee.hr.data, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

univariate_categorical(employee.hr.data,employee.hr.data$Gender,"Gender Distribution",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$JobSatisfaction,"Job Satisfaction Distribution",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$WorkLifeBalance,"WorkLife Balance",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$JobInvolvement,"Job Involvement",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$PerformanceRating,"Performance Rating",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$BusinessTravel,"Business Travel",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))
univariate_categorical(employee.hr.data,employee.hr.data$MaritalStatus,"Marital Status",employee.hr.data$Attrition)+guides(fill=guide_legend(title="Attrition"))

#The organization is genearlly very high performing. All ratings are excellent and outstanding. 
#Nobody has low or good rating.
#Among the Employees having "better" work life balance attrition is the highest
##Curiously employees with "high" job involvement also quit their job
#job satisfaction levels do not reduce attrition. Same across all job satisfaction levels
#People who rarely get to travel also have the highest attrition


#Numeric variables
# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(employee.hr.data, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(employee.hr.data, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#The number of employess in 20 to 40 years of age is high

plot_grid(ggplot(employee.hr.data, aes(MonthlyIncome))+ geom_histogram(binwidth = 20000),
          ggplot(employee.hr.data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Number of employees with salary below 60000 is high

quantile(employee.hr.data$MonthlyIncome,probs = seq(0, 1, 0.01), na.rm = FALSE)

plot_grid(ggplot(employee.hr.data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 5),
          ggplot(employee.hr.data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(employee.hr.data, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 1),
          ggplot(employee.hr.data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(employee.hr.data, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(employee.hr.data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

quantile(employee.hr.data$YearsAtCompany,probs = seq(0, 1, 0.01), na.rm = FALSE)

#big jump in YearsAtCompany after 22 years. Cap at 97 percentile with value as 22
#196 people with more than 20 years at the company

employee.hr.data$YearsAtCompany[which(employee.hr.data$YearsAtCompany>22)]<-22
 
plot_grid(ggplot(employee.hr.data, aes(MeanTimeInOffice))+ geom_histogram(binwidth = 1),
          ggplot(employee.hr.data, aes(x="",y=MeanTimeInOffice))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Majority of the people spend less than 9 hours in office

plot_grid(ggplot(employee.hr.data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 2),
          ggplot(employee.hr.data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
quantile(employee.hr.data$YearsSinceLastPromotion,probs = seq(0, 1, 0.01), na.rm = FALSE)
#A whooping 60% employees haven't received promotion in 7 years
#Cap YearsSinceLastPromotion at 92 percentile which is greater than 9 year
employee.hr.data$YearsSinceLastPromotion[which(employee.hr.data$YearsSinceLastPromotion>7)]<-7

plot_grid(ggplot(employee.hr.data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 2),
          ggplot(employee.hr.data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
quantile(employee.hr.data$YearsWithCurrManager,probs = seq(0, 1, 0.01), na.rm = FALSE)
#Cap YearsWithCurrManager at 98% percentile which is greater than 14 year
employee.hr.data$YearsWithCurrManager[which(employee.hr.data$YearsWithCurrManager>14)]<-14

plot_grid(ggplot(employee.hr.data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5),
          ggplot(employee.hr.data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
quantile(employee.hr.data$TotalWorkingYears,probs = seq(0, 1, 0.01), na.rm = FALSE)
#Cap TotalWorkingYears at 96 percentile which is greater than 29 years
employee.hr.data$TotalWorkingYears[which(employee.hr.data$TotalWorkingYears>29)]<-29


plot_grid(ggplot(employee.hr.data, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(employee.hr.data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Boxplots of numeric variables relative to attrition status
plot_grid(ggplot(employee.hr.data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee.hr.data, aes(x=Attrition,y=MeanTimeInOffice, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee.hr.data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee.hr.data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee.hr.data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#median time spent in office of people attriting is ~8.5 hours as compared to people staying ~7.5 hours

#Scale numeric variables and create dummy variables for categorical fields
str(employee.hr.data)
#Convert Job level to factor
employee.hr.data$JobLevel<-as.factor(employee.hr.data$JobLevel)

num.col.names <- sapply(employee.hr.data, is.numeric)

#creating a dataframe of continuous variables
employee.hr.data.num<- employee.hr.data[,num.col.names]
#Scale the continuous variables and remove EmployeeID column
employee.hr.data.num.scale<-lapply(employee.hr.data.num[,-1],function(x) scale(x))

# creating a dataframe of categorical features
employee.hr.data.chr<- employee.hr.data[,!num.col.names]

# converting categorical attributes to factor
employee.hr.data.factor<- data.frame(sapply(employee.hr.data.chr, function(x) factor(x)))
str(employee.hr.data.factor)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee.hr.data.factor, function(x) data.frame(model.matrix(~x-1,data =employee.hr.data.factor))[,-1]))

# Final dataset
employee.hr.data.final<- cbind(employee.hr.data.num.scale,dummies)
View(employee.hr.data.final) #4300 obs. of  54 variables


# splitting the data between train and test
set.seed(100)

indices = sample.split(employee.hr.data.final$Attrition, SplitRatio = 0.7)

employee.hr.data.final.train = employee.hr.data.final[indices,]

employee.hr.data.final.test = employee.hr.data.final[!(indices),]

#Logistic regression model
employee.attrition.model.1 = glm(Attrition ~ ., data = employee.hr.data.final.train, family = "binomial")
summary(employee.attrition.model.1)

#Step AIC to reduce number of variables
employee.attrition.model.2=stepAIC(employee.attrition.model.1,direction="both")
summary(employee.attrition.model.2)
vif(employee.attrition.model.2)

#AIC=2101.1
#EducationField.xLife.Sciences has high VIF value0f 16.8 and high p value of 0.11 so removing it
employee.attrition.model.3=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
      TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
      MeanTimeInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
      Department.xResearch...Development + Department.xSales + 
      Education.xDoctor +  EducationField.xMarketing + 
      EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
      JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
      JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
      JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
      EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
      WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
      JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.3)
vif(employee.attrition.model.3)
#AIC=2101.4
#VIF values of Department.xSales and Department.xResearch...Development are 4 but their Pvalues are low

#Department.xSales has high VIF value of 4.49 so removing it
employee.attrition.model.4=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                 MeanTimeInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                                 Department.xResearch...Development + 
                                 Education.xDoctor +  EducationField.xMarketing + 
                                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                 JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                                 JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.4)
vif(employee.attrition.model.4)
#WorkLifeBalance.xBetter has a high VIF of 3.59 so removing it
employee.attrition.model.5=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                 MeanTimeInOffice + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                                 Department.xResearch...Development + 
                                 Education.xDoctor +  EducationField.xMarketing + 
                                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                 JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                                 JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.5)
vif(employee.attrition.model.5)
# BusinessTravel.xTravel_Frequently has high VIF of 3.51 so removing it
employee.attrition.model.6=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                 MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                 Department.xResearch...Development + 
                                 Education.xDoctor +  EducationField.xMarketing + 
                                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                 JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                                 JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.6)
vif(employee.attrition.model.6)
#VIF values are low for all variables
#Pvalue of WorkLifeBalance.xBest is 0.95 so removing it
employee.attrition.model.7=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                 MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                 Department.xResearch...Development + 
                                 Education.xDoctor +  EducationField.xMarketing + 
                                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                 JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                 JobSatisfaction.xVery.High +  
                                 WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                                 JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.7)
#Pvalue of WorkLifeBalance.xGood is 0.44 so removing it
employee.attrition.model.8=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                 MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                 Department.xResearch...Development + 
                                 Education.xDoctor +  EducationField.xMarketing + 
                                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                 JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                 JobSatisfaction.xVery.High +  
                                 JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                                 JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.8)
#Pvalue of EducationField.xMedical is 0.30 so removing it
employee.attrition.model.9=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                 MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                 Department.xResearch...Development + 
                                 Education.xDoctor +  EducationField.xMarketing + 
                                 EducationField.xOther + EducationField.xTechnical.Degree + 
                                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                 JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                 JobSatisfaction.xVery.High +  
                                 JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                                 JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.9)
#Pvalue of EducationField.xMarketing is 0.20 so removing it
employee.attrition.model.10=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                 MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                 Department.xResearch...Development + 
                                 Education.xDoctor + 
                                 EducationField.xOther + EducationField.xTechnical.Degree + 
                                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                 JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                 JobSatisfaction.xVery.High +  
                                 JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                                 JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.10)
#Pvalue of EducationField.xOther is 0.19 so removing it
employee.attrition.model.11=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Department.xResearch...Development + 
                                  Education.xDoctor + 
                                  EducationField.xTechnical.Degree + 
                                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High +  
                                  JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                                  JobInvolvement.xLow, family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.11)
#Pvalue of JobInvolvement.xLow is 0.15 so removing it
employee.attrition.model.12=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Department.xResearch...Development + 
                                  Education.xDoctor + 
                                  EducationField.xTechnical.Degree + 
                                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High +  
                                  JobInvolvement.xMedium + JobInvolvement.xVery.High 
                                  , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.12)
#Pvalue of JobInvolvement.xVery.High  is 0.18 so removing it
employee.attrition.model.13=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Department.xResearch...Development + 
                                  Education.xDoctor + 
                                  EducationField.xTechnical.Degree + 
                                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High +  
                                  JobInvolvement.xMedium 
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.13)
#Pvalue of JobInvolvement.xMedium   is 0.19 so removing it
employee.attrition.model.14=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Department.xResearch...Development + 
                                  Education.xDoctor + 
                                  EducationField.xTechnical.Degree + 
                                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.14)
#Pvalue of EducationField.xTechnical.Degree is 0.14 so removing it
employee.attrition.model.15=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Department.xResearch...Development + 
                                  Education.xDoctor + 
                                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.15)
#Pvalue of JobRole.xHuman.Resources is 0.09 so removing it
employee.attrition.model.16=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Department.xResearch...Development + 
                                  Education.xDoctor + 
                                  JobLevel.x2 + JobLevel.x5 + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.16)
#Pvalue of JobLevel.x5 is 0.09 so removing it
employee.attrition.model.17=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Department.xResearch...Development + 
                                  Education.xDoctor + 
                                  JobLevel.x2 + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.17)
#Pvalue of Department.xResearch...Development is 0.05 so removing it
employee.attrition.model.18=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Education.xDoctor + 
                                  JobLevel.x2 + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.18)
#Pvalue of EnvironmentSatisfaction.xVery.High is 0.05 so removing it
employee.attrition.model.19=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + BusinessTravel.xTravel_Rarely + 
                                  Education.xDoctor + 
                                  JobLevel.x2 + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.19)
#Pvalue of BusinessTravel.xTravel_Rarely is 0.05 so removing it
employee.attrition.model.20=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  Education.xDoctor + 
                                  JobLevel.x2 + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.20)
#Pvalue of JobRole.xSales.Executive is 0.02 so removing it
employee.attrition.model.21=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  Education.xDoctor + 
                                  JobLevel.x2 + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.21)
#Pvalue of JobRole.xResearch.Director is 0.04 so removing it
employee.attrition.model.22=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  Education.xDoctor + 
                                  JobLevel.x2 + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.22)
#Pvalue of Education.xDoctor is 0.01 so removing it
employee.attrition.model.23=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  JobLevel.x2 + JobRole.xManager + 
                                  JobRole.xManufacturing.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.23)
#Pvalue of JobLevel.x2 is 0.01 so removing it
employee.attrition.model.24=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  JobRole.xManager + 
                                  JobRole.xManufacturing.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.24)
#Pvalue of JobRole.xManager is 0.004 so removing it
employee.attrition.model.25=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  JobRole.xManufacturing.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.25)
#Pvalue of JobRole.xManager is 0.004 so removing it
employee.attrition.model.25=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  JobRole.xManufacturing.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.25)
#Pvalue of TrainingTimesLastYear is 0.000347 so removing it
employee.attrition.model.26=glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                                  YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  JobRole.xManufacturing.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.26)
#Pvalue of Age is 0.000293 so removing it
employee.attrition.model.27=glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
                                  YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  JobRole.xManufacturing.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow + 
                                  JobSatisfaction.xVery.High
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.27)
#Pvalue of JobSatisfaction.xVery.High is 0.000217 so removing it
employee.attrition.model.28=glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
                                  YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  JobRole.xManufacturing.Director + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow 
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.28)

#Pvalue of JobRole.xManufacturing.Director is 0.000131 so removing it
employee.attrition.model.29=glm(formula = Attrition ~  NumCompaniesWorked + TotalWorkingYears + 
                                  YearsSinceLastPromotion + YearsWithCurrManager + 
                                  MeanTimeInOffice + 
                                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                                  JobSatisfaction.xLow 
                                , family = "binomial", data = employee.hr.data.final.train)
summary(employee.attrition.model.29)


########################################################################
# With 8 significant variables in the model
#1.NumCompaniesWorked
#2.TotalWorkingYears
#3.YearsSinceLastPromotion
#4.YearsWithCurrManager
#5.MeanTimeInOffice
#6.MaritalStatus.xSingle
#7.EnvironmentSatisfaction.xLow
#8.JobSatisfaction.xLow

final_model<- employee.attrition.model.29


#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

employee.hr.data.final.test.pred = predict(final_model, type = "response", 
                    newdata = employee.hr.data.final.test[,-which(colnames(employee.hr.data.final.test)=="Attrition")])
summary(employee.hr.data.final.test.pred)
#Append predicticted probability to test data frame
employee.hr.data.final.test$prob <- employee.hr.data.final.test.pred
View(employee.hr.data.final.test)
#Check confusion matrix for 50%
employee.hr.data.final.test.pred.attrition <- factor(ifelse(employee.hr.data.final.test.pred >= 0.50, "Yes", "No"))
employee.hr.data.final.test.actual.attrition <- factor(ifelse(employee.hr.data.final.test$Attrition==1,"Yes","No"))
employee.hr.data.final.test.confusion.matrix.50 <- confusionMatrix(employee.hr.data.final.test.pred.attrition, employee.hr.data.final.test.actual.attrition, positive = "Yes")
employee.hr.data.final.test.confusion.matrix.50
#At 0.5 probability Sensitivity=0.2105 Specificity=0.9870 Accuracy=0.8612
#Check confusion matrix for 40%
employee.hr.data.final.test.pred.attrition <- factor(ifelse(employee.hr.data.final.test.pred >= 0.40, "Yes", "No"))
employee.hr.data.final.test.confusion.matrix.40 <- confusionMatrix(employee.hr.data.final.test.pred.attrition, employee.hr.data.final.test.actual.attrition, positive = "Yes")
employee.hr.data.final.test.confusion.matrix.40
#At 0.4 probability Sensitivity=0.3540 Specificity=0.9519 Accuracy=0.855

#Calculate optimal probalility cutoff 

calc_sens_spec_accu_fn <- function(cutoff) 
{
  employee.hr.data.final.test.pred.attrition <- factor(ifelse(employee.hr.data.final.test.pred >= cutoff, "Yes", "No"))
  conf.matrix <- confusionMatrix(employee.hr.data.final.test.pred.attrition, employee.hr.data.final.test.actual.attrition, positive = "Yes")
  acc <- conf.matrix$overall[1]
  sens <- conf.matrix$byClass[1]
  spec <- conf.matrix$byClass[2]
  matrix.out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(matrix.out) <- c("sensitivity", "specificity", "accuracy")
  return(matrix.out)
}

# Summary of test probability
summary(employee.hr.data.final.test.pred)
#Min=0.0019 Max=0.825 
#Creating cutoff values from 0.001919 to 0.82500 for plotting and initiallizing a matrix of 100 X 3.
prob.cutoff.seq = seq(0.001919,.8250,length=100)
employee.attrition.model.sens.spec.accu = matrix(0,100,3)
#calculate sensitivity, specificity and accuracy for all probabilities
for(i in 1:100)
{
  employee.attrition.model.sens.spec.accu[i,] = calc_sens_spec_accu_fn(prob.cutoff.seq[i])
} 

#Plot sensitivity, specificity and accurac
plot(prob.cutoff.seq, employee.attrition.model.sens.spec.accu[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(prob.cutoff.seq,employee.attrition.model.sens.spec.accu[,2],col="darkgreen",lwd=2)
lines(prob.cutoff.seq,employee.attrition.model.sens.spec.accu[,3],col=4,lwd=2)
box()
legend('right',col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#Determining optimal cutoff
prob.optimal.cutoff <- prob.cutoff.seq[which(abs(employee.attrition.model.sens.spec.accu[,1]-employee.attrition.model.sens.spec.accu[,2])<0.02)]
#prob.optimal.cutoff=0.1681

#Create confusion matrix for cut off value of 0.1681
employee.hr.data.final.test.cutoff.attrition <- factor(ifelse(employee.hr.data.final.test.pred >=0.168198, "Yes", "No"))
employee.hr.data.final.test.conf.mat.final <- confusionMatrix(employee.hr.data.final.test.cutoff.attrition, employee.hr.data.final.test.actual.attrition, positive = "Yes")
employee.hr.data.final.test.conf.mat.final
employee.attrition.model.accu <- employee.hr.data.final.test.conf.mat.final$overall[1]
employee.attrition.model.accu
#accuracy=0.7395
employee.attrition.model.sens <- employee.hr.data.final.test.conf.mat.final$byClass[1]
employee.attrition.model.sens
#sensitivity=0.7512
employee.attrition.model.spec <- employee.hr.data.final.test.conf.mat.final$byClass[2]
employee.attrition.model.spec
#Specificity=0.7373

View(employee.hr.data.final.test)
#Calculate KS -statistic
#Convert yes -> 1 and no ->0
employee.hr.data.final.test.cutoff.attrition <- ifelse(employee.hr.data.final.test.cutoff.attrition=="Yes",1,0)
employee.hr.data.final.test.actual.attrition <- ifelse(employee.hr.data.final.test.actual.attrition=="Yes",1,0)

#on testing  data
employee.attrition.model.pred.object.test<- prediction(employee.hr.data.final.test.cutoff.attrition, employee.hr.data.final.test.actual.attrition)

employee.attrition.model.performance.measures.test<- performance(employee.attrition.model.pred.object.test, "tpr", "fpr")
#ROC Curve
plot(employee.attrition.model.performance.measures.test,col="green")
abline(a=0,b=1)
employee.attrition.model.performance.measures.rocr.auc.test<- performance(employee.attrition.model.pred.object.test, measure="auc")
employee.attrition.model.performance.measures.rocr.auc.test@y.values
#Area under the curve=0.74

employee.attrition.model.ks.table.test <- attr(employee.attrition.model.performance.measures.test, "y.values")[[1]] - 
  (attr(employee.attrition.model.performance.measures.test, "x.values")[[1]])

max(employee.attrition.model.ks.table.test)

#KS Statistics=0.4884

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

calc.lift.fn <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)),
          Gain.Randommodel=bucket*10,
          Lift.Randommodel=1.0)
  return(gaintable)
}

employee.attrition.decile = calc.lift.fn(employee.hr.data.final.test.actual.attrition, employee.hr.data.final.test.pred, groups = 10)
View(employee.attrition.decile)
employee.attrition.decile
#Gain Chart
ggplot(employee.attrition.decile,aes(bucket))+
  geom_line(aes(y=Gain),colour="#000099")+geom_point(aes(y=Gain),size=3, colour="#000099")+
  geom_line(aes(y=Gain.Randommodel),colour="#CC0000")+geom_point(aes(y=Gain.Randommodel),size=3, colour="#CC0000")+
  geom_text(aes(label = round(Gain,digits = 2),y=Gain), size = 3,vjust=-2)+
  geom_text(aes(label = round(Gain.Randommodel,digits = 2),y=Gain.Randommodel), size = 3,vjust=-2)
  
     
#Lift Chart
ggplot(employee.attrition.decile,aes(bucket))+
  geom_line(aes(y=Cumlift),colour="#000099")+geom_point(aes(y=Cumlift),size=3, colour="#000099")+
  geom_line(aes(y=Lift.Randommodel),colour="#CC0000")+geom_point(aes(y=Lift.Randommodel),size=3, colour="#CC0000")+
  geom_text(aes(label = round(Cumlift,digits = 2),y=Cumlift), size = 3,vjust=-2)+
  geom_text(aes(label = round(Lift.Randommodel,digits = 2),y=Lift.Randommodel), size = 3,vjust=-2)

#graph for ppt

plot_grid(ggplot(employee.hr.data, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(), 
          ggplot(employee.hr.data, aes(x=TotalWorkingYears,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=MeanTimeInOffice,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee.hr.data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

