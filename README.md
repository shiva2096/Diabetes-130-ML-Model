# Analysis & Modeling of Diabetes 130-US Dataset

## About the Dataset
Dataset Used - [uci-link](https://archive.ics.uci.edu/ml/datasets/diabetes+130-us+hospitals+for+years+1999-2008)

The dataset represents 10 years (1999-2008) of clinical care at 130 US hospitals and integrated delivery networks. It includes over 50 features representing patient and hospital outcomes. Information was extracted from the database for encounters that satisfied the following criteria.

- (1) It is an inpatient encounter (a hospital admission).
- (2) It is a diabetic encounter, that is, one during which any kind of diabetes was entered to the system as a diagnosis.
- (3) The length of stay was at least 1 day and at most 14 days.
- (4) Laboratory tests were performed during the encounter.
- (5) Medications were administered during the encounter.

The data contains such attributes as patient number, race, gender, age, admission type, time in hospital, medical specialty of admitting physician, number of lab test performed, HbA1c test result, diagnosis, number of medication, diabetic medications, number of outpatient, inpatient, and emergency visits in the year before the hospitalization, etc.

**Target Variable** - Readmitted (whether the patient was readmitted within 30 days, or after 30 days, or did not get readmitted)

## [Exploratory Data Analysis](https://github.com/shiva2096/Diabetes-130-ML-Model/blob/main/EDA%20Diabetes130.pdf)

For each attribute in the dataset, checking the distribution, performing hypothesis test with chi-squared and anova test, checking correlation and normality.

![alt text](https://github.com/shiva2096/Diabetes-130-ML-Model/blob/main/Images/01%20-%20T_Class%20dist%20bar.png?raw=True)

## [Working with Classifiers](https://github.com/shiva2096/Diabetes-130-ML-Model/blob/main/Modeling-1.pdf)

Using Logistic Regression, NaiveBayes Classifier, SVM, Decision Trees and Nearest Neighbour algorithm for classifying the dataset in 3 classes.

![alt txt](https://github.com/shiva2096/Diabetes-130-ML-Model/blob/main/Images/Results-1.png?raw=True)
