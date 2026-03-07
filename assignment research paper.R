df1=Assignment_data
coutry_code=df1$v000
coutry_code
df=data.frame(df1$v002,df1$v012,df1$v106,df1$v106,df1$v190,df1$v367,df1$v701,df1$v714,df1$v744a,df1$v744b,df1$v744c,df1$v744e,df1$p8)
df
missing_wanted_last_child=sum(is.na(df$df1.v367))
missing_wanted_last_child
missing_wife_goes_out=sum(is.na(df$df1.v744a))
missing_wife_goes_out
missing_neglects_child=sum(is.na(df$df1.v744b))
missing_neglects_child
missing_wife_argues=sum(is.na(df$df1.v744c))
missing_wife_argues
missing_burns_food=sum(is.na(df$df1.v744e))
missing_burns_food
wealth_index=sum()
wealth_index
wealth_index=sum(df$df1.v190==1)
wealth_index
missing_wanted_last_child
wanted_last_child=sum(df$df1.v367==1)
wanted_last_child
wanted_child=df$df1.v367
wanted_child
sum(wanted_child==3,na.rm = TRUE)
husband_education_level=df$df1.v701
husband_education_level
sum(husband_education_level==8,na.rm= TRUE)
wealth_index=df$df1.v190
wealth_index
education_level=df$df1.v106
education_level
sum(education_level==3,na.rm = TRUE)
currently_working=df$df1.v714
currently_working
sum(currently_working==1,na.rm = TRUE)
countery=Assignment_data$v000
countery
cluster_number=Assignment_data$v001
cluster_number
line_number=Assignment_data$v003
line_number
weight=Assignment_data$v005
weight
resident=Assignment_data$v025
resident
sum(resident==2,na.rm = T)
division=Assignment_data$v024
division
sum(division==8,na.rm = T)
tv=Assignment_data$v121
tv
sum(tv==7,na.rm = T)
wealth_index=Assignment_data$v190
wealth_index
wanted_last_child_child=Assignment_data$v367
wanted_last_child
health_insurance=Assignment_data$v481
health_insurance
sum(health_insurance==1,na.rm = T)
marital_status=Assignment_data$v501
marital_status
partner_education=Assignment_data$v701
partner_education
goes_without_telling=Assignment_data$v744a
goes_without_telling
sum(goes_without_telling==8,na.rm = T)
neglects_children=Assignment_data$v744b
neglects_children
sum(neglects_children==8,na.rm = T)
wife_argues=Assignment_data$v744c
wife_argues
sum(wife_argues==8,na.rm = T)
burns_food=Assignment_data$v744e
burns_food
sum(current_marital_status==0,na.rm = T)
victim=rep(1,73239)
victim
model1=glm(victim~wealth_index+education_level+currently_working+husband_education_level+resident+division+tv+health_insurance)
summary(model1)
anova(model1,test="Chisq")
str(wealth_index)
wealth_index=as.factor(wealth_index)
education_level=as.factor(education_level)
currently_working=as.factor(currently_working)
husband_education_level=as.factor(husband_education_level)
resident=as.factor(resident)
division=as.factor(division)
tv=as.factor(tv)
health_insurance=as.factor(health_insurance)
victim=as.factor(victim)
xtabs(~victim+wealth_index)
xtabs(~victim+education_level)
xtabs(~victim+currently_working)
xtabs(~victim+husband_education_level)
xtabs(~victim+resident)
xtabs(~victim+division)
xtabs(~victim+tv)
xtabs(~victim+health_insurance)
model1=glm(victim~wealth_index+education_level+husband_education_level,family = "binomial")
cleaned=data.frame(victim,wealth_index,education_level,currently_working,husband_education_level,resident,division,tv,health_insurance)
cleaned
clean_data=na.omit(cleaned)
clean_data
model2=glm(clean_data$victim~clean_data$wealth_index+clean_data$education_level+clean_data$currently_working+clean_data$husband_education_level+clean_data$resident+clean_data$division+clean_data$tv+clean_data$health_insurance,family = "binomial")
summary(model2)
model3=glm(clean_data$victim~clean_data$wealth_index)

clean_data$wealth_index
wealth_index
hist(current_marital_status,main = "Current marital status",xlab ="Categories",ylab = "Frequency",col = "cadetblue3",border = "black")
wealth_index1=as.numeric(wealth_index)
education_level1=as.numeric(education_level)
currently_working1=as.numeric(currently_working)
# Your values
values <- c(805,47978,131,24325)
names <- c("Justified", "Not justified","Not sure","Did not answer")

# Calculate percentages
percentages <- round(100 * values / sum(values), 1)

# Create labels
labels <- paste(names, ":", percentages, "%")

# Create pie chart
pie(values,
    labels = labels,
    main = "Whether domestic abuse is justified if wife burns food ",
    col = c("red", "darkorange","yellow","cyan3"))
husband_education_level1=as.numeric(husband_education_level)
library(nnet)
model1=multinom(clean_data$victim~clean_data$wealth_index+clean_data$husband_education_level)
library(VGAM)
install.packages("VGAM")
library(VGAM)
model1=vglm(victim1~education_level1+husband_education_level1, family = multinomial)
victim1=as.factor(victim)
division1=as.numeric(division)
division1
# Load the library
library(ggplot2)

# Suppose your data frame is named df and the variable is 'division'
# Make sure 'division' is a factor
division1 <- as.factor(current_marital_status)

# Create the bar plot
ggplot(df, aes(x = division1)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Division where the respondent resides in",
       x = "Division", y = "Frequency") +
  theme_minimal()
current_marital_status=Assignment_data$v501 

current_marital_status
labels.default(current_marital_status)
current_marital_status1=as.factor(current_marital_status)
str(current_marital_status)
attr(current_marital_status,"labels")
library(haven)
sum(current_marital_status==5,na.rm = T)
last_child=Assignment_data$v367
last_child
sum(last_child==1,na.rm = T)
anik=Assignment_data$v744d
anik
sum(anik==1,na.rm = T)

installed.packages("performance")
library(performance)
vif(model1)
cor(clean_data$education_level,clean_data$husband_education_level)
