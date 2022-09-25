library(tidyverse)
library(RColorBrewer)
library(dlookr)
library(ggcorrplot)
library(plyr) 
library(dplyr)
library(cowplot)

diabetes = read.csv("diabetic_data.csv", stringsAsFactors=F)

dim(diabetes)

str(diabetes)

## 1
#########################################################################
## Checking the distribution of Target Class
table(diabetes$readmitted)

table(diabetes$readmitted)/nrow(diabetes)*100

ggplot(diabetes, aes(x = readmitted,fill = readmitted)) +
  geom_bar() +
  ggtitle("Distribution of Target Class") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_brewer(palette="Pastel1")

## 2
#########################################################################
## encounter_id and patient_nbr
count(diabetes)
n_distinct(diabetes$encounter_id)
n_distinct(diabetes$patient_nbr)

## 3
#########################################################################
## Distribution of Race
ggplot(diabetes, aes(x = race,fill = race)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of Race") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_brewer(palette="Pastel1")


## Chi-square Test for Race
c_test <- chisq.test(table(diabetes$readmitted, diabetes$race))
c_test
c_test$p.value

## 4
#########################################################################
## Distribution of Gender
ggplot(diabetes, aes(x = gender,fill = gender)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_brewer(palette="Pastel1")


## Chi-square Test for Gender
daibetes_gen_readm <- filter(diabetes, gender != "Unknown/Invalid" ) %>%
  select(gender, readmitted) 

c_test <- chisq.test(table(daibetes_gen_readm$readmitted, daibetes_gen_readm$gender))
c_test
c_test$p.value

## 5
#########################################################################
## Distribution of Age

library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(10)

ggplot(diabetes, aes(x = age,fill = age)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_manual(values = mycolors)

## Chi-square Test for Age
c_test <- chisq.test(table(diabetes$readmitted, diabetes$age))
c_test
c_test$p.value


## 6
##################################################################
# weight
count(filter(diabetes, weight == "?"))/count(diabetes)*100

## 7
##################################################################
# admission_type_id
new_admission_id <- transform(diabetes, admission_type_id = as.character(admission_type_id)) %>%
  select(admission_type_id,readmitted)

ggplot(new_admission_id, aes(x = admission_type_id, fill = admission_type_id)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of Admission ID") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_brewer(palette="Pastel1")

## Mapping "NULL" and "Not Mapped" to "Not Available"  
# new_admission_id$admission_type_id[new_admission_id$admission_type_id == "6"] <- "5"
# new_admission_id$admission_type_id[new_admission_id$admission_type_id == "8"] <- "5"

## Chi-square Test for Age
c_test <- chisq.test(table(new_admission_id$readmitted, new_admission_id$admission_type_id))
c_test
c_test$p.value


## 8
##################################################################
# discharge_disposition_id
diabetes_dispo_id <- transform(diabetes, discharge_disposition_id = as.character(discharge_disposition_id)) %>%
  select(discharge_disposition_id,readmitted)

library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(30)

ggplot(diabetes_dispo_id, aes(x = discharge_disposition_id, fill = discharge_disposition_id)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of Discharge_Disposition_Id") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = mycolors)

c_test <- chisq.test(table(diabetes_dispo_id$readmitted, diabetes_dispo_id$discharge_disposition_id))
c_test
c_test$p.value

## 9
##################################################################
# admission_source_id
library(tidyverse)
library(dplyr)

diabetes_adm_source <- transform(diabetes, adm_source_id = as.character(admission_source_id)) %>%
  select(adm_source_id,readmitted)

library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(21)

ggplot(diabetes_adm_source, aes(x = adm_source_id, fill = adm_source_id)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of Admission_Source_Id") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_manual(values = mycolors)

c_test <- chisq.test(table(diabetes_adm_source$readmitted, diabetes_adm_source$adm_source_id))
c_test
c_test$p.value



round(table(diabetes_adm_source$adm_source_id)/nrow(diabetes_adm_source)*100, 2) >5

## 10
##################################################################
# payer_code
count(filter(diabetes, payer_code == "?"))/count(diabetes)*100

mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(24)

ggplot(diabetes, aes(x = payer_code, fill = payer_code)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of Payer_Code") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_manual(values = mycolors)

c_test <- chisq.test(table(diabetes$readmitted, diabetes$payer_code))
c_test
c_test$p.value

## 11
##################################################################
# medical_specialty
count(filter(diabetes, medical_specialty == "?"))/count(diabetes)*100

mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(90)

ggplot(diabetes, aes(x = medical_specialty, fill = medical_specialty)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of medical_specialty") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = mycolors) +
  coord_flip()

c_test <- chisq.test(table(diabetes$readmitted, diabetes$medical_specialty))
c_test
c_test$p.value

## 12
##################################################################
# numerical_variables

numeric = c("time_in_hospital",
            "num_lab_procedures",
            "num_procedures",
            "num_medications",
            "number_outpatient",
            "number_emergency",
            "number_inpatient",
            "number_diagnoses")

diabetes_numeric <- select(diabetes, numeric)

correlation_matrix <- round(cor(diabetes_numeric),2)

ggcorrplot(correlation_matrix, hc.order = TRUE, lab = TRUE,
           colors = c("#6D9EC1", "white", "#E46726"))

## 13
##################################################################
# Relational Plots

## num_medications and time_in_hospital
ggplot(data = diabetes, mapping = aes(x = num_medications, y = time_in_hospital)) + 
  geom_point(mapping = aes(color=readmitted)) + 
  geom_smooth(se = FALSE) 


## num_medications and num_procedures
ggplot(data = diabetes, mapping = aes(x = num_medications, y = num_procedures)) + 
  geom_point(mapping = aes(color=readmitted)) + 
  geom_smooth(se = FALSE)

## num_lab_procedures and time_in_hospital
ggplot(data = diabetes, mapping = aes(x = num_lab_procedures, y = time_in_hospital)) + 
  geom_point(mapping = aes(color=readmitted)) + 
  geom_smooth(se = FALSE)


## number_emergency and number_inpatient
ggplot(data = diabetes, mapping = aes(x = number_emergency, y = number_inpatient)) + 
  geom_point(mapping = aes(color=readmitted)) + 
  geom_smooth(se = FALSE)

## 14
##################################################################
# time_in_hospital

## Checking the null counts
sum(is.na(diabetes_numeric$time_in_hospital))

## Histogrm Plot
ggplot(diabetes, mapping = aes(time_in_hospital)) +
  geom_histogram(fill="turquoise2", color="black") +
  ggtitle("Distribution of time_in_hospital") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


## Plotting the Normality
plot_normality(diabetes, time_in_hospital)

## Testing Normality


skewness(diabetes$time_in_hospital)

## Shapiro-Wilk normality test 
normality(diabetes, time_in_hospital)

#box plot
ggplot(data = diabetes, mapping = aes(x = readmitted, y = time_in_hospital)) + 
  geom_boxplot(fill="springgreen1")+
  ggtitle("Box Plot of Time_In_Hospital with Readmitted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## 15
##################################################################
# num_lab_procedures

## Checking the null counts
sum(is.na(diabetes_numeric$num_lab_procedures))

## Histogrm Plot
ggplot(diabetes, mapping = aes(num_lab_procedures)) +
  geom_histogram(fill="turquoise2", color="black") +
  ggtitle("Distribution of num_lab_procedures") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


## Plotting the Normality
plot_normality(diabetes, num_lab_procedures)

## Testing Normality
skewness(diabetes$num_lab_procedures)

## Shapiro-Wilk normality test 
normality(diabetes, num_lab_procedures)

#box plot
ggplot(data = diabetes, mapping = aes(x = readmitted, y = num_lab_procedures)) + 
  geom_boxplot(fill="springgreen1")+
  ggtitle("Box Plot of num_lab_procedures with Readmitted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


## 16
##################################################################
# num_procedures

## Checking the null counts
sum(is.na(diabetes_numeric$num_procedures))

## Histogrm Plot
ggplot(diabetes, mapping = aes(num_procedures)) +
  geom_histogram(fill="turquoise2", color="black") +
  ggtitle("Distribution of num_procedures") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


## Plotting the Normality
plot_normality(diabetes, num_procedures)

## Testing Normality
skewness(diabetes$num_procedures)

## Shapiro-Wilk normality test 
normality(diabetes, num_procedures)

#box plot
ggplot(data = diabetes, mapping = aes(x = readmitted, y = num_procedures)) + 
  geom_boxplot(fill="springgreen1")+
  ggtitle("Box Plot of num_procedures with Readmitted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## 17
##################################################################
# num_medications

## Checking the null counts
sum(is.na(diabetes_numeric$num_medications))

## Histogrm Plot
ggplot(diabetes, mapping = aes(num_medications)) +
  geom_histogram(fill="turquoise2", color="black") +
  ggtitle("Distribution of num_medications") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


## Plotting the Normality
plot_normality(diabetes, num_medications)

## Testing Normality
skewness(diabetes$num_medications)

## Shapiro-Wilk normality test 
normality(diabetes, num_medications)

#box plot
ggplot(data = diabetes, mapping = aes(x = readmitted, y = num_medications)) + 
  geom_boxplot(fill="springgreen1")+
  ggtitle("Box Plot of num_medications with Readmitted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



## 18
##################################################################
# number_outpatient

## Checking the null counts
sum(is.na(diabetes_numeric$number_outpatient))

## Histogrm Plot
ggplot(diabetes, mapping = aes(number_outpatient)) +
  geom_histogram(fill="turquoise2", color="black") +
  ggtitle("Distribution of number_outpatient") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


## Plotting the Normality
plot_normality(diabetes, number_outpatient)

## Testing Normality
skewness(diabetes$number_outpatient)

## Shapiro-Wilk normality test 
normality(diabetes, number_outpatient)

#box plot
ggplot(data = diabetes, mapping = aes(x = readmitted, y = number_outpatient)) + 
  geom_boxplot(fill="springgreen1")+
  ggtitle("Box Plot of number_outpatient with Readmitted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


## 19
##################################################################
# number_emergency

## Checking the null counts
sum(is.na(diabetes_numeric$number_emergency))

## Histogrm Plot
ggplot(diabetes, mapping = aes(number_emergency)) +
  geom_histogram(fill="turquoise2", color="black") +
  ggtitle("Distribution of number_emergency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#box plot
ggplot(data = diabetes, mapping = aes(x = readmitted, y = number_emergency)) + 
  geom_boxplot(fill="springgreen1")+
  ggtitle("Box Plot of number_emergency with Readmitted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


## 20
##################################################################
# number_inpatient

## Checking the null counts
sum(is.na(diabetes_numeric$number_inpatient))

## Histogrm Plot
ggplot(diabetes, mapping = aes(number_inpatient)) +
  geom_histogram(fill="turquoise2", color="black") +
  ggtitle("Distribution of number_inpatient") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

## 21
##################################################################
# number_diagnoses

## Checking the null counts
sum(is.na(diabetes_numeric$number_diagnoses))

## Histogrm Plot
ggplot(diabetes, mapping = aes(number_diagnoses)) +
  geom_histogram(fill="turquoise2", color="black") +
  ggtitle("Distribution of number_diagnoses") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

## 22
##################################################################
# max_glu_serum

ggplot(diabetes, aes(x = max_glu_serum, fill = max_glu_serum)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of max_glu_serum") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_brewer(palette="Pastel1")

## 23
##################################################################
# A1Cresult

ggplot(diabetes, aes(x = A1Cresult, fill = A1Cresult)) +
  geom_bar(show.legend = FALSE) +
  ggtitle("Distribution of A1Cresult") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.25) +
  scale_fill_brewer(palette="Pastel1")

## 24
##################################################################
# Medical Features Part 1

## Function to plot distribution of a categorical variable
cat_distribution <- function(df, atr) {
  title <- paste("Distribution of",atr, sep=" ")
  plt <- ggplot(df, aes_string(x = atr, fill = atr)) +
    geom_bar(show.legend = FALSE) +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(label = ..count..), stat = "count", vjust=-0.25, size=2) +
    scale_fill_brewer(palette="Pastel1")
  plt
}

## First 9 Features
metformin_plot <- cat_distribution(diabetes, "metformin")
repaglinide_plot <- cat_distribution(diabetes, "repaglinide")
nateglinide_plot <- cat_distribution(diabetes, "nateglinide")
chlorpropamide_plot <- cat_distribution(diabetes, "chlorpropamide")
glimepiride_plot <- cat_distribution(diabetes, "glimepiride")
acetohexamide_plot <- cat_distribution(diabetes, "acetohexamide")
glipizide_plot <- cat_distribution(diabetes, "glipizide")
glyburide_plot <- cat_distribution(diabetes, "glyburide")
tolbutamide_plot <- cat_distribution(diabetes, "tolbutamide")

library(cowplot)
## Making a grid
plot_grid(metformin_plot,
          repaglinide_plot,
          nateglinide_plot,
          chlorpropamide_plot,
          glimepiride_plot,
          acetohexamide_plot,
          glipizide_plot,
          glyburide_plot,
          tolbutamide_plot,
          ncol = 3, labels = "AUTO")


##################################################################
# Medical Features Part 2

## Next 6 Features
pioglitazone_plot <- cat_distribution(diabetes, "pioglitazone")
rosiglitazone_plot <- cat_distribution(diabetes, "rosiglitazone")
acarbose_plot <- cat_distribution(diabetes, "acarbose")
miglitol_plot <- cat_distribution(diabetes, "miglitol")
troglitazone_plot <- cat_distribution(diabetes, "troglitazone")
tolazamide_plot <- cat_distribution(diabetes, "tolazamide")

## Making a grid
plot_grid(pioglitazone_plot,
          rosiglitazone_plot,
          acarbose_plot,
          miglitol_plot,
          troglitazone_plot,
          tolazamide_plot,
          ncol = 3, labels = "AUTO")



##################################################################
# Medical Features Part 3

## Next 8 Features
examide_plot <- cat_distribution(diabetes, "examide")
citoglipton_plot <- cat_distribution(diabetes, "citoglipton")
insulin_plot <- cat_distribution(diabetes, "insulin")
glyburide_metformin_plot <- cat_distribution(diabetes, "glyburide.metformin")
glipizide_metformin_plot <- cat_distribution(diabetes, "glipizide.metformin")
glimepiride_pioglitazone_plot <- cat_distribution(diabetes, "glimepiride.pioglitazone")
metformin_rosiglitazone_plot <- cat_distribution(diabetes, "metformin.rosiglitazone")
metformin_pioglitazone_plot <- cat_distribution(diabetes, "metformin.pioglitazone")

## Making a grid
plot_grid(examide_plot,
          citoglipton_plot,
          insulin_plot,
          glyburide_metformin_plot,
          glipizide_metformin_plot,
          glimepiride_pioglitazone_plot,
          metformin_rosiglitazone_plot,
          metformin_pioglitazone_plot,
          ncol = 2, labels = "AUTO")

## 25
##################################################################
## Chi-Squared Test of Medical Features

## Function Calculate Chi-Square Test Results
perform_chisq <- function(df, attr_list, target, significance_threshold = 0.05) {
  
  attr_names <- c()
  X_sqr_stat <- c()
  p_value <- c()
  significant <- c()
  
  for(attrs in attr_list)
  {
    c_test <- chisq.test(table(df[,target], df[,attrs]))
    
    attr_names <- append(attr_names, attrs)
    X_sqr_stat <- append(X_sqr_stat, c_test$statistic)
    p_value <- append(p_value, c_test$p.value)
    significant <- append(significant, (c_test$p.value < significance_threshold))
  }
  
  data.frame(attr_names,X_sqr_stat,p_value,significant)
}


## Medical Features List
medical_features <- list("metformin","repaglinide","nateglinide","chlorpropamide","glimepiride",
                         "acetohexamide","glipizide","glyburide","tolbutamide","pioglitazone",
                         "rosiglitazone","acarbose","miglitol","troglitazone","tolazamide",
                         "examide","citoglipton","insulin","glyburide.metformin","glipizide.metformin",
                         "glimepiride.pioglitazone","metformin.rosiglitazone",
                         "metformin.pioglitazone")



## Performing Chi-Squared Test on above list taking threshold as 5%

chi_sq_df <- perform_chisq(diabetes, medical_features, "readmitted", significance_threshold = 0.05)
chi_sq_df



## 26
##################################################################
# change (change in medication)

change_med_plot <- cat_distribution(diabetes, "change")

change_med_plot

# Chi-Squared Test
perform_chisq(diabetes, list("change"), "readmitted", significance_threshold = 0.05)



## 27
##################################################################
# diabetesMed (Diabetes medication)

diabetesMed_plot <- cat_distribution(diabetes, "diabetesMed")

diabetesMed_plot

# Chi-Squared Test
perform_chisq(diabetes, list("diabetesMed"), "readmitted", significance_threshold = 0.05)



## 28
##################################################################
# ANOVA Test of Numeric Attributes

## Function Calculate One Way Anova Test Results
perform_anova <- function(df, attr_list, target, significance_threshold = 0.05) {
  
  attr_names <- c()
  F_stat <- c()
  p_value <- c()
  significant <- c()
  
  for(attrs in attr_list)
  {
    anova_test <- aov(df[,attrs]~df[,target], data = df)
    
    p <- summary(anova_test)[[1]][["Pr(>F)"]][1]
    f <- summary(anova_test)[[1]][["F value"]][1]
    
    attr_names <- append(attr_names, attrs)
    F_stat <- append(F_stat, f)
    p_value <- append(p_value, p)
    significant <- append(significant, (p < significance_threshold))
  }
  
  data.frame(attr_names,F_stat,p_value,significant)
}


## Numeric Features List
numeric_features <- list("time_in_hospital","num_lab_procedures","num_procedures","num_medications",
                         "number_outpatient","number_emergency","number_inpatient","number_diagnoses")



## Performing Anova Test on above list taking threshold as 5%

anova_df <- perform_anova(diabetes, numeric_features, "readmitted", significance_threshold = 0.05)
anova_df








