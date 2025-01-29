# Load packages for data manipulation
library(tidyr)       # Data tidying
library(dplyr)       # Data manipulation
library(purrr)       # Functional programming
library(stringr)     # String manipulation
library(magrittr)    # Pipes (%>%)
library(lubridate)   # Date and time manipulation
library(readxl)      # Read Excel files
library(tibble)      # Enhanced data frames

# Load packages for statistical analysis
library(MASS)        # Statistical functions
library(mice)        # Multiple imputations
library(rms)         # Regression modeling
library(MuMIn)       # Model selection
library(boot)        # Bootstrap functions
library(AICcmodavg)  # AIC model comparison
library(caret)       # Machine learning
library(gam)         # Generalized additive models
library(rstatix)     # Statistical tests
library(metafor)

# Load packages for survival analysis
library(survival)        # Survival analysis
library(survivalAnalysis) # Advanced survival plots
library(ggsurvfit)       # ggplot2 extensions for survival plots
library(tidycmprsk)      # Competing risks analysis

# Load packages for visualization
library(ggplot2)         # Data visualization
library(ggpubr)          # Publication-ready plots
library(ggvenn)          # Venn diagrams
library(cowplot)         # Plot grid layouts
library(pheatmap)        # Heatmaps
library(ComplexHeatmap)  # Advanced heatmaps
library(circlize)        # Circular plots
library(scales)          # Scaling functions
library(forplo)
library(patchwork)

# Load packages for reporting and tables
library(gtsummary)       # Summary tables
library(tableone)        # Create Table 1
library(table1)          # Advanced Table 1
library(Gmisc, quietly = TRUE)  # Table summaries
library(glue)            # String interpolation
library(knitr)           # Dynamic reports

# Load packages for model selection
#library(glmulti)         # Multi-model inference
library(leaps)           # Subset regression

# Other utilities
#library(rJava)           # Java integration
library(writexl)         # Write to Excel

citation() 
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PRELIMINARY PART 
#Import the DATA from the file 

##Selecting the working directory
setwd("/Users/macbookpro/Documents/École/Doctorat en recherche/Projets/ORACLE/Project_Interaction_analysis")

## Importing the original data (will be used to create table 1)
col_types <- c("guess", "guess", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "numeric", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric")
data_original_imported <- suppressWarnings(read_excel("data_ORACLE_original_20240429.xlsx", col_types = col_types))
data_original <-data_original_imported
## Importing the imputated data without the systematically missing data (d)= Only imputation of of all missing data
col_types <-c("guess", "text" ,"text","text", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text" )
Data_imputated_all_imported <- suppressWarnings(read_excel("data_ORACLE_imp_20250102_joined.xlsx", col_types = col_types))
data_imputated<-Data_imputated_all_imported 
## Importing the imputated data with the systematically missing data not replaced (NR) = Only imputation of non-systematically missing
col_types <-c("guess","text","text","text","text","text","numeric","text","numeric","text","text","text","text","numeric","numeric","text","text","numeric","text","text","text","text","text","text","text","text","numeric","numeric","numeric","text","numeric","text","text","text","text","numeric","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","numeric","text","numeric","numeric","text","numeric","numeric","text","text","text","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text")
Data_imputated_without_systematically_missing_imported<-suppressWarnings(read_excel("data_ORACLE_imp_sysREMOVED_20250102_joined.xlsx", col_types = col_types))
data_imputated_without_systematically_missing<- Data_imputated_without_systematically_missing_imported

currentpath<-"/Users/macbookpro/Documents/École/Doctorat en recherche/Projets/ORACLE/Interaction_lung_function_and_inflammation"
#======================================================================================================================================
#TRANSFORMATION of Values
#Putting NA for the missing value
data_original[data_original == "NA"] <- NA
data_imputated[data_imputated== "NA"] <- NA
data_imputated_without_systematically_missing[data_imputated_without_systematically_missing == "NA"] <- NA

#Transform values to put them in the the good units
#Modify the values of the stratos trials to put the reversibility in % for all the studies instead of decimal
data_original<- data_original %>% 
  mutate(FEV1_reversibility_percent_postBD_real = case_when(
    Enrolled_Trial_name=="STRATOS_1" ~ FEV1_PCT_reversibility_postBD*100,
    Enrolled_Trial_name=="STRATOS_2" ~ FEV1_PCT_reversibility_postBD*100,
    TRUE ~ FEV1_PCT_reversibility_postBD))
#Modify the values of the Captain study to put adherence in trial in percentage everywhere
data_original<- data_original %>% 
  mutate(Adherence_InTrial_quantity_real = case_when(
    Enrolled_Trial_name=="CAPTAIN" ~ Adherence_InTrial_quantity*100,
    TRUE ~ Adherence_InTrial_quantity))
#======================================================================================================================================
#Creation of a unique dataframe which contain original value, imputated and imputated_only_not_systematically_missing

data_original_main<-data_original[,c("Sequential_number","Enrolled_Trial_name","Treatment_arm","Age","Gender_0Female_1Male","BMI","Ethnicity","Country","Region","Treatment_step","Any_severe_attack_previous_12m_0no_1yes","Any_attack_or_hospitalization_previous_12_months","Number_severe_attack_previous_12m","Number_hospitalisations_for_asthma_previous_12_months","Number_hospitalizations_previous_12m_1Yes_0No","Previous_ICU_0no_1yes_9999notknown","Previous_Intubation_0no_1yes_9999notknown","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Psychiatric_disease_0no_1yes_9999notknown","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","ICS_DOSE_CLASS","LABA_prescribed_0no_1yes","LAMA_prescribed__0no_1yes","maintenance_OCS_prescribed__0no_1yes","Theophylline_prescribed__0no_1yes","Intranasal_seroid_prescribed__0no_1yes","FEV1_predicted_L","FVC_predicted_L","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FVC_postBD_PCT_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","ACT_baseline_score","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE","Follow_up_duration_days_nozero","Number_severe_asthma_attacks_during_followup","Time_to_First_attack","Time_to_2n_attack","Time_to_3n_attack","Time_to_4n_attack","Time_to_5n_attack","End_FollowUp_Reason","FEV1PREBD_L_52W","FEV1PREBD_PCT_52W","FEV1POSTBD_L_52W","FEV1POSTBD_PCT_52W","FEV1_reversibility_percent_postBD_real")]

data_imputated_all<-data_imputated[,c("Sequential_number",".imp","Age","Gender_0Female_1Male","BMI","Any_severe_attack_previous_12m_0no_1yes","Number_severe_attack_previous_12m_con","Number_hospitalisations_for_asthma_previous_12_months_con","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE")]
colnames(data_imputated_all)[-c(1:2)] <- paste0(colnames(data_imputated_all)[-c(1:2)], "_imputated")
colnames(data_imputated_all) <- make.unique(colnames(data_imputated_all))

data_imputated_no_systematically_missing<-data_imputated_without_systematically_missing[,c("Sequential_number",".imp","Age","Gender_0Female_1Male","BMI","Any_severe_attack_previous_12m_0no_1yes","Number_severe_attack_previous_12m_con","Number_hospitalisations_for_asthma_previous_12_months_con","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE")]
colnames(data_imputated_no_systematically_missing)[-c(1:2)] <- paste0(colnames(data_imputated_no_systematically_missing)[-c(1:2)], "_imputated_no_systematically_missing")

#combining the datasets
merged_data_imputated <- merge(data_imputated_all, data_imputated_no_systematically_missing, 
                               by = c("Sequential_number", ".imp"), 
                               all = TRUE)

All_data <- merge(data_original, merged_data_imputated, 
                               by = c("Sequential_number"), 
                               all = TRUE)
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PART A : CREATION of TABLE 1  

##Selecting the data_original which contain the value without any imputation
data_original_table<-All_data

###Labeling the differents variable with a clear name for table 1 and their respective units
####Age
label(data_original_table$Age) <- "Age"
####Sex
data_original_table$Gender_0Female_1Male <- factor(data_original_table$Gender_0Female_1Male, levels=c(0,1), labels=c("Female", "Male"))
label(data_original_table$Gender_0Female_1Male) <- "Sex, n(%)"
####Ethnicity
data_original_table$Ethnicity <- factor(data_original_table$Ethnicity, levels=c("American_Indian_or_Alaska_Native","Asian","Black_or_African_American","Maori","Multiple","Native_Hawaiian_or_other_Pacific_Islander","Other","White"))
label(data_original_table$Ethnicity) <- "Ethnicity, n(%)"
####Region
data_original_table$Region<-factor(data_original$Region,levels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"))
data_original_table$Region <- factor(data_original_table$Region, levels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"), labels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"))
label(data_original_table$Region) <- "Region, n(%)"
####BMI
label(data_original_table$BMI) <- "Body Mass Index"
units(data_original_table$BMI) <- "kg/m2"
####Treatment step
label(data_original_table$Treatment_step) <- "Treatment step, n(%)"
####Blood eosinophils (BEC)
label(data_original_table$Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced) <- "Blood eosinophils"
units(data_original_table$Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced) <- "x10^9 cells/L"
####FENO
label(data_original_table$FeNO_baseline_ppb) <- "FeNO"
units(data_original_table$FeNO_baseline_ppb) <- "ppb"
####IgE
label(data_original_table$Total_IgE) <- "Total IgE"
units(data_original_table$Total_IgE) <- "ng/mL"
####FEV1
label(data_original_table$FEV1_preBD_PCT_Baseline) <- "FEV1"
units(data_original_table$FEV1_preBD_PCT_Baseline) <- "% of predicted"
####FEV1/FVC
label(data_original_table$FEV1_FVC_ratio) <- "FEV1/FVC"
####Previous ICU or intubation
data_original_table$Previous_ICU_or_intubation_0no_1yes <- factor(data_original_table$Previous_ICU_or_intubation_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_ICU_or_intubation_0no_1yes) <- "Previous ICU or intubation, n(%)"
####Previous ICU
data_original_table$Previous_ICU_0no_1yes_9999notknown <- factor(data_original_table$Previous_ICU_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_ICU_0no_1yes_9999notknown) <- "Previous ICU, n(%)"
####Previous intubation
data_original_table$Previous_Intubation_0no_1yes_9999notknown <- factor(data_original_table$Previous_Intubation_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_Intubation_0no_1yes_9999notknown) <- "Previous intubation, n(%)"
####Atopy history
data_original_table$Atopy_history_0no_1yes_9999notknown <- factor(data_original_table$Atopy_history_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Atopy_history_0no_1yes_9999notknown) <- "Atopy history, n(%)"
####Allergy testing positive
data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown <- factor(data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown) <- "Allergy testing positive, n(%)"
####Eczema
data_original_table$Eczema_0no_1yes_9999notknown <- factor(data_original_table$Eczema_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Eczema_0no_1yes_9999notknown) <- "Eczema, n(%)"
####Allergic rhinitis
data_original_table$AllergicRhinitis__0no_1yes_9999notknown <- factor(data_original_table$AllergicRhinitis__0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$AllergicRhinitis__0no_1yes_9999notknown) <- "Allergic rhinitis, n(%)"
####Chronic rhinosinusitis
data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown <- factor(data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown) <- "Chronic rhinosinusitis, n(%)"
####Nasal polyposis
data_original_table$Nasal_polyposis_0no_1yes_9999notknown <- factor(data_original_table$Nasal_polyposis_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Nasal_polyposis_0no_1yes_9999notknown) <- "Nasal polyposis, n(%)"
####Previous nasal polypectomy
data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown <- factor(data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown) <- "Previous nasal polypectomy, n(%)"
####ACQ-5
label(data_original_table$ACQ_baseline_score_mean) <- "ACQ-5"
####Psychiatric disease
data_original_table$Psychiatric_disease_0no_1yes_9999notknown <- factor(data_original_table$Psychiatric_disease_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Psychiatric_disease_0no_1yes_9999notknown) <- "Psychiatric disease, n(%)"
####Smoking history
data_original_table$Smoking_0never_1ex_2current <- factor(data_original_table$Smoking_0never_1ex_2current, levels=c(0,1,2), labels=c("Never smoked","Ex-smoker", "Current smoker"))
label(data_original_table$Smoking_0never_1ex_2current) <- "Smoking history, n(%)"
####On ICS
data_original_table$Any_ICS_prescribed_0no_1yes <- factor(data_original$Any_ICS_prescribed_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Any_ICS_prescribed_0no_1yes) <- "On ICS, n(%)"
####ICS Dose
data_original_table$ICS_DOSE_CLASS <- factor(data_original_table$ICS_DOSE_CLASS, levels=c("0","Low","Medium","High"))
label(data_original_table$ICS_DOSE_CLASS) <- "ICS Dose, n(%)"
####On SABA
data_original_table$SABA_prescribed__0no_1yes <- factor(data_original_table$SABA_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$SABA_prescribed__0no_1yes) <- "On SABA, n(%)"
####On SABA actuation
label(data_original_table$SABA_actuations_per_day_average_PreTrial) <- "SABA actuations per day pre trial"
label(data_original_table$SABA_actuations_per_day_average_InTrial) <- "SABA actuations per day in trial"
####On mOCS
data_original_table$maintenance_OCS_prescribed__0no_1yes <- factor(data_original_table$maintenance_OCS_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$maintenance_OCS_prescribed__0no_1yes) <- "On mOCS, n(%)"
####On intranasal ICS
data_original_table$Intranasal_seroid_prescribed__0no_1yes <- factor(data_original_table$Intranasal_seroid_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Intranasal_seroid_prescribed__0no_1yes) <- "On intranasal ICS, n(%)"
####On LABA
data_original_table$LABA_prescribed_0no_1yes <- factor(data_original_table$LABA_prescribed_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$LABA_prescribed_0no_1yes) <- "On LABA, n(%)"
####On Montelukast
data_original_table$Montelukast_prescribed__0no_1yes <- factor(data_original_table$Montelukast_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Montelukast_prescribed__0no_1yes) <- "On Montelukast, n(%)"
####On LAMA
data_original_table$LAMA_prescribed__0no_1yes <- factor(data_original_table$LAMA_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$LAMA_prescribed__0no_1yes) <- "On LAMA, n(%)"
####Adherence
label(data_original_table$Adherence_PreTrial_quantity) <- "Adherence pre-trial"
label(data_original_table$Adherence_InTrial_quantity) <- "Adherence in trial"
####Attack history: severe exacerbation or hospitalisation in past 12 months
data_original_table$Any_attack_or_hospitalization_previous_12_months <- factor(data_original_table$Any_attack_or_hospitalization_previous_12_months, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Any_attack_or_hospitalization_previous_12_months) <- "Attack history: severe exacerbation or hospitalisation in past 12 months, n(%)"
####Attack history: Number of severe exacerbations in past 12 months
label(data_original_table$Number_severe_attack_previous_12m) <- "Attack history: Number of severe exacerbations in past 12 months, n(%)"
####Attack history: Number of hospitalisation in past 12 months
label(data_original_table$Number_hospitalisations_for_asthma_previous_12_months) <- "Attack history: Number of hospitalisation in past 12 months"
####Follow-up duration (days)
label(data_original_table$Follow_up_duration_days_nozero) <- "Follow-up duration (days)"

##Identify the data which the only available data is in categories (for age and BMI)
data_original_table<- data_original_table %>% 
  mutate(Age_format = case_when(
    Age_cat=="NA" ~ NA,
    !is.na(Age) ~ "Continuous",
    TRUE ~ "In categories"))
data_original_table$Age_format <- factor(data_original_table$Age_format, levels=c("Continuous","In categories"))

data_original_table<- data_original_table %>% 
  mutate(BMI_format = case_when(
    BMI_cat=="NA" ~ NA,
    !is.na(BMI) ~ "Continuous",
    TRUE ~ "In categories"))
data_original_table$BMI_format <- factor(data_original_table$BMI_format, levels=c("Continuous","In categories"))

##Create the groups for the data analyzed by group (FEV1, ACQ-5, In trial severe exacerbations)
###Group of FEV1
data_original_table$FEV1_PCT_reversibility_postBD_by_group<-cut(data_original_table$FEV1_reversibility_percent_postBD_real,breaks = c(-1000,12,1000),labels=c("<12%","⩾12%"))
label(data_original_table$FEV1_PCT_reversibility_postBD_by_group) <- "FEV1 reversibility (by group)"
###Group of ACQ-5
data_original_table$ACQ_baseline_score_mean_by_group<-cut(data_original_table$ACQ_baseline_score_mean,breaks = c(-1000,1.5,1000),labels=c("<1.5","⩾1.5%"))
label(data_original_table$ACQ_baseline_score_mean_by_group) <- "ACQ-5 (by group)"

###Group of In trial severe exacerbations
data_original_table$Exacerbations_during_follow_up_by_group<-cut(data_original_table$Number_severe_asthma_attacks_during_followup,breaks = c(-1000,0.9,1000),labels=c("0","⩾1"))
label(data_original_table$Exacerbations_during_follow_up_by_group) <- "In trial severe exacerbations, n(%)"

#CREATING of CRSwNP
data_original_table<- data_original_table %>% 
  mutate(CRSwNP=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_0no_1yes_9999notknown=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_0no_1yes_9999notknown=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_0no_1yes_9999notknown) ~ NA,
    TRUE ~ NA
  ))
data_original_table$CRSwNP <- factor(data_original_table$CRSwNP, levels=c("Yes","No"), labels=c("Yes","No"))

##CRSsNP (No CRSwNP + No history of nasal polypectomy + CRS=="Yes")
data_original_table<- data_original_table %>% 
  mutate(CRSsNP=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_Rhinosinusitis_0no_1yes_9999notknown) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_Rhinosinusitis_0no_1yes_9999notknown=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_Rhinosinusitis_0no_1yes_9999notknown=="Yes" ~ "Yes",
    TRUE ~ NA
  ))
data_original_table$CRSsNP <- factor(data_original_table$CRSsNP, levels=c("Yes","No"), labels=c("Yes","No"))


# Create function for continuous and categorical variables
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x, ), digits = 3), c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD), "Median (IQR)"=sprintf(paste("%s (%s - %s)"), MEDIAN, Q1, Q3),"Geo. mean (GSD)"=sprintf("%s (&plusmn; %s)", GMEAN, GSD),"Geo. mean (IQR)"=sprintf(paste("%s (%s - %s)"), GMEAN, Q1, Q3), "Range"=sprintf("%s - %s", MIN, MAX)))}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))}
colnames(data_original_table)
# Create the tables 1 by trial
Table1_by_trial <- table1(~ Age+ Age_format + Gender_0Female_1Male +BMI+BMI_imputated+BMI_imputated_no_systematically_missing+BMI_format +Smoking_0never_1ex_2current+ Ethnicity +Region +Atopy_history_0no_1yes_9999notknown+Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown+Eczema_0no_1yes_9999notknown+AllergicRhinitis__0no_1yes_9999notknown+Chronic_Rhinosinusitis_0no_1yes_9999notknown+Nasal_polyposis_0no_1yes_9999notknown+Previous_nasal_polypectomy_0no_1yes_9999notknown+CRSsNP+CRSwNP+Psychiatric_disease_0no_1yes_9999notknown+Any_ICS_prescribed_0no_1yes+ICS_DOSE_CLASS+SABA_prescribed__0no_1yes+SABA_actuations_per_day_average_PreTrial+SABA_actuations_per_day_average_InTrial+maintenance_OCS_prescribed__0no_1yes+Intranasal_seroid_prescribed__0no_1yes+LABA_prescribed_0no_1yes+LAMA_prescribed__0no_1yes+Montelukast_prescribed__0no_1yes+Adherence_PreTrial_quantity+Adherence_InTrial_quantity_real+ Treatment_step+ACQ_baseline_score_mean+ACQ_baseline_score_mean_by_group+Any_attack_or_hospitalization_previous_12_months+Number_severe_attack_previous_12m+Number_hospitalisations_for_asthma_previous_12_months+Previous_ICU_or_intubation_0no_1yes+Previous_ICU_0no_1yes_9999notknown+Previous_Intubation_0no_1yes_9999notknown+FEV1_preBD_PCT_Baseline+FEV1_FVC_ratio+FEV1_reversibility_percent_postBD_real+FEV1_PCT_reversibility_postBD_by_group+Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced+FeNO_baseline_ppb+Total_IgE+Total_IgE_imputated+Total_IgE_imputated_no_systematically_missing+Follow_up_duration_days_nozero+Exacerbations_during_follow_up_by_group  # add the variables you want
                          | Enrolled_Trial_name, # the | means  "by"
                          overall=c(left="Total"), # add overall= if you want the total number to be included
                          data=subset(data_original_table,.imp==1),
                          render.continuous=my.render.cont, 
                          render.categorical=my.render.cat)

# Other information for required Table 1:follow up duration and number asthma attack total and by study, 
## Total follow-up and number of severe asthma attack
table_total <- data.frame(c(sum(data_original_table$Follow_up_duration_years_nozero, na.rm=TRUE),sum(data_original_table$Number_severe_asthma_attacks_during_followup, na.rm=TRUE)))
rownames(table_total)<-c("Follow up duration (year)","Nb of asthma attack during follow-up")
colnames(table_total)<-"Total"

##Follow-up duration in year
sum(data_original$Follow_up_duration_years_nozero, na.rm=TRUE)
table_sum_follow_up<- aggregate(data_original_table$Follow_up_duration_years_nozero, by=list(Category=data_original_table$Enrolled_Trial_name), FUN=sum)
table_sum_follow_up<- t(table_sum_follow_up)
colnames(table_sum_follow_up)<-table_sum_follow_up[1,]
table_sum_follow_up<-table_sum_follow_up[2,]

##Number of severe asthma attack
table_sum_asthma_attack<- aggregate(data_original_table$Number_severe_asthma_attacks_during_followup, by=list(Category=data_original_table$Enrolled_Trial_name), FUN=sum)
table_sum_asthma_attack<- t(table_sum_asthma_attack)
colnames(table_sum_asthma_attack)<-table_sum_asthma_attack[1,]
table_sum_asthma_attack<-table_sum_asthma_attack[2,]
##Table with the total information
table_sum_follow_up_and_asthma_attack <- rbind(table_sum_follow_up,table_sum_asthma_attack)
rownames(table_sum_follow_up_and_asthma_attack)<- c("Follow up duration (year)","Nb of asthma attack during follow-up")
table_sum_follow_up_and_asthma_attack <- cbind(table_total,table_sum_follow_up_and_asthma_attack)
table_sum_follow_up_and_asthma_attack
#Export the table for follow-up duration and asthma attack sum
write_xlsx(table_sum_follow_up_and_asthma_attack,"table_sum_follow_up_and_attack.xlsx")
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PART C = PREPARING THE IMPUTATED DATASET (identification of variable, calculation of variables and creation of category)
## Identify the categorical data
Data_Oracle<-All_data %>% 
  #Gender
  mutate(Gender_0Female_1Male= case_when(Gender_0Female_1Male ==0 ~ "Female", Gender_0Female_1Male ==1 ~ "Male",TRUE ~ NA)) %>% 
  mutate(Gender_0Female_1Male_imputated= case_when(Gender_0Female_1Male_imputated ==0 ~ "Female", Gender_0Female_1Male_imputated ==1 ~ "Male",TRUE ~ NA)) %>% 
  mutate(Gender_0Female_1Male_imputated_no_systematically_missing= case_when(Gender_0Female_1Male_imputated_no_systematically_missing ==0 ~ "Female", Gender_0Female_1Male_imputated_no_systematically_missing ==1 ~ "Male",TRUE ~ NA)) %>% 
  #Smoking
  mutate(Smoking_0never_1ex_2current= case_when(Smoking_0never_1ex_2current ==0 ~ "Never", Smoking_0never_1ex_2current ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  mutate(Smoking_0never_1ex_2current_imputated= case_when(Smoking_0never_1ex_2current_imputated ==0 ~ "Never", Smoking_0never_1ex_2current_imputated ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current_imputated ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  mutate(Smoking_0never_1ex_2current_imputated_no_systematically_missing= case_when(Smoking_0never_1ex_2current_imputated_no_systematically_missing ==0 ~ "Never", Smoking_0never_1ex_2current_imputated_no_systematically_missing ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current_imputated_no_systematically_missing ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  #Atopy
  mutate(Atopy_history_0no_1yes_9999notknown= case_when(Atopy_history_0no_1yes_9999notknown ==0 ~ "No", Atopy_history_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Atopy_history_0no_1yes_9999notknown_imputated= case_when(Atopy_history_0no_1yes_9999notknown_imputated ==0 ~ "No", Atopy_history_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>%
  #Airborne_allergen_sensitisation
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Eczema
  mutate(Eczema_0no_1yes_9999notknown= case_when(Eczema_0no_1yes_9999notknown ==0 ~ "No", Eczema_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Eczema_0no_1yes_9999notknown_imputated= case_when(Eczema_0no_1yes_9999notknown_imputated ==0 ~ "No", Eczema_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>%
  #Allergic rhinitis
  mutate(AllergicRhinitis__0no_1yes_9999notknown= case_when(AllergicRhinitis__0no_1yes_9999notknown ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(AllergicRhinitis__0no_1yes_9999notknown_imputated= case_when(AllergicRhinitis__0no_1yes_9999notknown_imputated ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Chronic rhinosinusitis
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Nasal polyposis
  mutate(Nasal_polyposis_0no_1yes_9999notknown= case_when(Nasal_polyposis_0no_1yes_9999notknown ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Nasal_polyposis_0no_1yes_9999notknown_imputated= case_when(Nasal_polyposis_0no_1yes_9999notknown_imputated ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) 


colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male" )]<- "Gender"
Data_Oracle$Gender<-as.factor(Data_Oracle$Gender)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male_imputated" )]<- "Gender_imputated"
Data_Oracle$Gender_imputated<-as.factor(Data_Oracle$Gender_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male_imputated_no_systematically_missing" )]<- "Gender_imputated_no_systematically_missing"
Data_Oracle$Gender_imputated_no_systematically_missing<-as.factor(Data_Oracle$Gender_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current" )]<- "Smoking_Statut"
Data_Oracle$Smoking_Statut<-as.factor(Data_Oracle$Smoking_Statut)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current_imputated" )]<- "Smoking_Statut_imputated"
Data_Oracle$Smoking_Statut_imputated<-as.factor(Data_Oracle$Smoking_Statut_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current_imputated_no_systematically_missing" )]<- "Smoking_Statut_imputated_no_systematically_missing"
Data_Oracle$Smoking_Statut_imputated_no_systematically_missing<-as.factor(Data_Oracle$Smoking_Statut_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown" )]<- "Atopy_history"
Data_Oracle$Atopy_history<-as.factor(Data_Oracle$Atopy_history)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown_imputated" )]<- "Atopy_history_imputated"
Data_Oracle$Atopy_history_imputated<-as.factor(Data_Oracle$Atopy_history_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Atopy_history_imputated_no_systematically_missing"
Data_Oracle$Atopy_history_imputated_no_systematically_missing<-as.factor(Data_Oracle$Atopy_history_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown" )]<- "Airborne_allergen_sensibilisation"
Data_Oracle$Airborne_allergen_sensibilisation<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated" )]<- "Airborne_allergen_sensibilisation_imputated"
Data_Oracle$Airborne_allergen_sensibilisation_imputated<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Airborne_allergen_sensibilisation_imputated_no_systematically_missing"
Data_Oracle$Airborne_allergen_sensibilisation_imputated_no_systematically_missing<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown" )]<- "Eczema"
Data_Oracle$Eczema<-as.factor(Data_Oracle$Eczema)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown_imputated" )]<- "Eczema_imputated"
Data_Oracle$Eczema_imputated<-as.factor(Data_Oracle$Eczema_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Eczema_imputated_no_systematically_missing"
Data_Oracle$Eczema_imputated_no_systematically_missing<-as.factor(Data_Oracle$Eczema_imputated_no_systematically_missing)

colnames(Data_Oracle)[which(colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown" )]<- "Allergic_rhinitis"
Data_Oracle$Allergic_rhinitis<-as.factor(Data_Oracle$Allergic_rhinitis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown_imputated" )]<- "Allergic_rhinitis_imputated"
Data_Oracle$Allergic_rhinitis_imputated<-as.factor(Data_Oracle$Allergic_rhinitis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Allergic_rhinitis_imputated_no_systematically_missing"
Data_Oracle$Allergic_rhinitis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Allergic_rhinitis_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown" )]<- "Chronic_rhinosinusitis"
Data_Oracle$Chronic_rhinosinusitis<-as.factor(Data_Oracle$Chronic_rhinosinusitis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated" )]<- "Chronic_rhinosinusitis_imputated"
Data_Oracle$Chronic_rhinosinusitis_imputated<-as.factor(Data_Oracle$Chronic_rhinosinusitis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Chronic_rhinosinusitis_imputated_no_systematically_missing"
Data_Oracle$Chronic_rhinosinusitis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Chronic_rhinosinusitis_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown" )]<- "Nasal_polyposis"
Data_Oracle$Nasal_polyposis<-as.factor(Data_Oracle$Nasal_polyposis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown_imputated" )]<- "Nasal_polyposis_imputated"
Data_Oracle$Nasal_polyposis_imputated<-as.factor(Data_Oracle$Nasal_polyposis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Nasal_polyposis_imputated_no_systematically_missing"
Data_Oracle$Nasal_polyposis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Nasal_polyposis_imputated_no_systematically_missing)

## Modify the name of columns for clear names
Data_Oracle<-Data_Oracle %>% 
  mutate(Eosinophils_Log=log10(Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced_imputated" )]<- "Eosinophils_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced_imputated_no_systematically_missing" )]<- "Eosinophils_Log_imputated_no_systematically_missing"

Data_Oracle<-Data_Oracle %>% 
  mutate(FeNO_Log=log10(FeNO_baseline_ppb))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="FeNO_baseline_ppb_imputated" )]<- "FeNO_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="FeNO_baseline_ppb_imputated_no_systematically_missing" )]<- "FeNO_Log_imputated_no_systematically_missing"

Data_Oracle<-Data_Oracle %>% 
  mutate(IgE_Log=log10(Total_IgE))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Total_IgE_imputated")]<- "IgE_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Total_IgE_imputated_no_systematically_missing")]<- "IgE_Log_imputated_no_systematically_missing"

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con"  )]<- "Attack_12mo_Nb"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con_imputated"  )]<- "Attack_12mo_Nb_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con_imputated_no_systematically_missing"  )]<- "Attack_12mo_Nb_imputated_no_systematically_missing"

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con"  )]<- "Hospitalisations_12mo_Nb"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con_imputated"  )]<- "Hospitalisations_12mo_Nb_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con_imputated_no_systematically_missing"  )]<- "Hospitalisations_12mo_Nb_imputated_no_systematically_missing"
#======================================================================================================================================
#Calculation of the continuous variable 

##Calculation of predicted spirometric parameters according to %FEV1 or %FVC when the FEV1_predicted_L was not given
Data_Oracle<-Data_Oracle%>% 
  mutate(FEV1_predicted_L= case_when(
    is.na(FEV1_predicted_L) & !is.na(FEV1_preBD_L_Baseline) & !is.na(FEV1_preBD_PCT_Baseline) ~ (100*FEV1_preBD_L_Baseline)/FEV1_preBD_PCT_Baseline,
    is.na(FEV1_predicted_L) & !is.na(FEV1_postBD_L_Baseline) & !is.na(FEV1_postBD_PCT_Baseline) ~ (100*FEV1_postBD_L_Baseline)/FEV1_postBD_PCT_Baseline,
    TRUE ~ FEV1_predicted_L
  )) %>% 
  mutate(FVC_predicted_L= case_when(
    is.na(FVC_predicted_L) & !is.na(FVC_preBD_L_Baseline) & !is.na(FVC_preBD_PCT_Baseline) ~ (100*FVC_preBD_L_Baseline)/FVC_preBD_PCT_Baseline,
    is.na(FVC_predicted_L) & !is.na(FVC_postBD_L_Baseline) & !is.na(FVC_preBD_PCT_Baseline) ~ (100*FVC_postBD_L_Baseline)/FVC_preBD_PCT_Baseline,
    TRUE ~ FVC_predicted_L
  )) 

## Calculation FEV1/FVC
Data_Oracle<-Data_Oracle%>% 
  mutate(Tiffeneau=FEV1_preBD_L_Baseline/FVC_preBD_L_Baseline)%>% 
  mutate(Tiffeneau_imputated=FEV1_preBD_L_Baseline_imputated/FVC_preBD_L_Baseline_imputated)%>% 
  mutate(Tiffeneau_imputated_no_systematically_missing=FEV1_preBD_L_Baseline_imputated_no_systematically_missing/FVC_preBD_L_Baseline_imputated_no_systematically_missing)

## Calculate the value in absolute for eosinophils, FeNO and IgE
Data_Oracle<-Data_Oracle %>% 
  mutate(Blood_Eos_baseline_x10_9_cells_per_L_imputated=10^Eosinophils_Log_imputated) %>% 
  mutate(Blood_Eos_baseline_x10_9_cells_per_L_imputated_no_systematically_missing=10^Eosinophils_Log_imputated_no_systematically_missing) %>% 
  
  mutate(FeNO_baseline_ppb_imputated=10^FeNO_Log_imputated)%>% 
  mutate(FeNO_baseline_ppb_imputated_no_systematically_missing=10^FeNO_Log_imputated_no_systematically_missing)%>% 
  
  mutate(Total_IgE_imputated=10^IgE_Log_imputated) %>% 
  mutate(Total_IgE_imputated_no_systematically_missing=10^IgE_Log_imputated_no_systematically_missing)

##Put the Follow up duration in days
Data_Oracle<-Data_Oracle %>% 
  mutate(Follow_up_duration_days=Data_Oracle$Follow_up_duration_days_nozero)
#======================================================================================================================================
## Calculation of the parameters by a definite change
Data_Oracle<-Data_Oracle%>% 
  #Age per 10 year increase
  mutate(Age_per_10=Age/10) %>%
  mutate(Age_per_10_imputated=Age_imputated/10) %>%
  mutate(Age_per_10_imputated_no_systematically_missing=Age_imputated_no_systematically_missing/10) %>%
  #BMI per 5 increase
  mutate(BMI_per_5=BMI/5) %>% 
  mutate(BMI_per_5_imputated=BMI_imputated/5) %>% 
  mutate(BMI_per_5_imputated_no_systematically_missing=BMI_imputated_no_systematically_missing/5) %>% 
  #FEV1 per 10% decrease
  mutate(FEV1_preBD_per10_Baseline=-FEV1_preBD_PCT_Baseline/10) %>%
  mutate(FEV1_preBD_per10_Baseline_imputated=-FEV1_preBD_PCT_Baseline_imputated/10) %>%
  mutate(FEV1_preBD_per10_Baseline_imputated_no_systematically_missing=-FEV1_preBD_PCT_Baseline_imputated_no_systematically_missing/10) %>%
  #Reversibility per 10%
  mutate(FEV1_per10_reversibilityBD= FEV1_PCT_reversibility_postBD/10) %>% 
  mutate(FEV1_per10_reversibilityBD_imputated= FEV1_PCT_reversibility_postBD_imputated/10) %>% 
  mutate(FEV1_per10_reversibilityBD_imputated_no_systematically_missing= FEV1_PCT_reversibility_postBD_imputated_no_systematically_missing/10)

### Calculate the value standardize by percentile 25 and 75 
FeNO_delta_75_25<- quantile(Data_Oracle$FeNO_Log_imputated)[4]-quantile(Data_Oracle$FeNO_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset
BEC_delta_75_25<- quantile(Data_Oracle$Eosinophils_Log_imputated)[4]-quantile(Data_Oracle$Eosinophils_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset


### Scaling FeNO based on the 25th and 75th percentiles (method by Prof. Frank Harrell)
Data_Oracle <- Data_Oracle %>%
  mutate(FeNO_p_imputated = FeNO_Log_imputated / FeNO_delta_75_25)  # Scale log FeNO by dividing by 0.47 (between the 25th and 75th percentiles)
Data_Oracle <- Data_Oracle %>%
  mutate(BEC_p_imputated = Eosinophils_Log_imputated / BEC_delta_75_25)  # Scale log FeNO by dividing by 0.47 (between the 25th and 75th percentiles)


# Calculate the mean FeNO, mean BEC and mean IgE of the 10 imputations per patient in the dataset
patient_mean_FeNO <- Data_Oracle %>%
  dplyr::group_by(Sequential_number) %>%
  dplyr::summarize(mean_FeNO = mean(FeNO_baseline_ppb_imputated, na.rm = TRUE))
Data_Oracle <- 
  left_join(Data_Oracle, patient_mean_FeNO, by = "Sequential_number")

patient_mean_BEC <- Data_Oracle %>%
  dplyr::group_by(Sequential_number) %>%
  dplyr::summarize(mean_BEC = mean(Blood_Eos_baseline_x10_9_cells_per_L_imputated, na.rm = TRUE))
Data_Oracle <- 
  left_join(Data_Oracle, patient_mean_BEC, by = "Sequential_number")
Data_Oracle$Total_IgE_imputated

patient_mean_IgE <- Data_Oracle %>%
  dplyr::group_by(Sequential_number) %>%
  dplyr::summarize(mean_IgE = mean(Total_IgE_imputated, na.rm = TRUE))
Data_Oracle <- 
  left_join(Data_Oracle, patient_mean_IgE, by = "Sequential_number")

#======================================================================================================================================
#Creation of new categoricals data
##Create the CRsNP factor (CRS without NP)
#CREATING of CRSwNP

Data_Oracle <- Data_Oracle  %>% 
  mutate(CRSwNP=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis) ~ NA,
    TRUE ~ NA
  )) %>% 
  mutate(CRSwNP_imputated=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_imputated=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown_imputated =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_imputated=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_imputated) ~ NA,
    TRUE ~ NA
  )) %>% 
  mutate(CRSwNP_imputated_no_systematically_missing=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_imputated_no_systematically_missing=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown_imputated_no_systematically_missing =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_imputated_no_systematically_missing=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_imputated_no_systematically_missing) ~ NA,
    TRUE ~ NA
  ))
  
Data_Oracle$CRSwNP <- factor(Data_Oracle$CRSwNP, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSwNP_imputated <- factor(Data_Oracle$CRSwNP_imputated, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSwNP_imputated_no_systematically_missing <- factor(Data_Oracle$CRSwNP_imputated_no_systematically_missing, levels=c("No","Yes"), labels=c("No","Yes"))
##CRSsNP (No CRSwNP + No history of nasal polypectomy + CRS=="Yes")
Data_Oracle<- Data_Oracle %>% 
  mutate(CRSsNP=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis=="Yes" ~ "Yes",
    TRUE ~ NA
  ))%>% 
  mutate(CRSsNP_imputated=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP_imputated)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis_imputated) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP_imputated=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated=="Yes" ~ "Yes",
    TRUE ~ NA
  ))%>% 
  mutate(CRSsNP_imputated_no_systematically_missing=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP_imputated_no_systematically_missing)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis_imputated_no_systematically_missing) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP_imputated_no_systematically_missing=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated_no_systematically_missing=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated_no_systematically_missing=="Yes" ~ "Yes",
    TRUE ~ NA
  ))
Data_Oracle$CRSsNP <- factor(Data_Oracle$CRSsNP, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSsNP_imputated <- factor(Data_Oracle$CRSsNP_imputated, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSsNP_imputated_no_systematically_missing <- factor(Data_Oracle$CRSsNP_imputated_no_systematically_missing, levels=c("No","Yes"), labels=c("No","Yes"))


Table1_test_by_trial <- table1(~ Chronic_rhinosinusitis+Chronic_rhinosinusitis_imputated_no_systematically_missing+Nasal_polyposis+Nasal_polyposis_imputated_no_systematically_missing+CRSsNP+CRSsNP_imputated_no_systematically_missing+CRSwNP+CRSwNP_imputated_no_systematically_missing
                               | Enrolled_Trial_name, # the | means  "by"
                               overall=c(left="Total"), # add overall= if you want the total number to be included
                               data=subset(Data_Oracle,.imp==1),
                               render.continuous=my.render.cont, 
                               render.categorical=my.render.cat)

##Category by inflammatory marker
Data_Oracle$Eosinophils_by_group <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)
Data_Oracle$Eosinophils_by_group_imputated <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L_imputated,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)

Data_Oracle$Eosinophils_by_group_imputated_no_systematically_missing <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L_imputated_no_systematically_missing,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)


Data_Oracle$FeNO_baseline_by_group <- cut(
  Data_Oracle$FeNO_baseline_ppb,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)
Data_Oracle$FeNO_baseline_by_group_imputated <- cut(
  Data_Oracle$FeNO_baseline_ppb_imputated,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)

Data_Oracle$FeNO_baseline_by_group_imputated_no_systematically_missing <- cut(
  Data_Oracle$FeNO_baseline_ppb_imputated_no_systematically_missing,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)


Data_Oracle$IgE_by_group<-cut(Data_Oracle$Total_IgE,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))
Data_Oracle$IgE_by_group_imputated<-cut(Data_Oracle$Total_IgE_imputated,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))
Data_Oracle$IgE_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$Total_IgE_imputated_no_systematically_missing,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))


##Category by lung function
Data_Oracle$FEV1_preBD_Baseline_by_group<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline,breaks = c(0,50,60,70,100000),right = FALSE,labels=c('<50%',"50-<60%",'60-<70%',">=70%"))

Data_Oracle$FEV1_preBD_Baseline_by_group_imputated<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline_imputated,breaks = c(0,50,60,70,100000),labels=c('<50%',"50-<60%",'60-<70%',">=70%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline_imputated_no_systematically_missing,breaks = c(0,50,60,70,100000),labels=c('<50%',"50-<60%",'60-<70%',">=70%"))

Data_Oracle$FEV1_preBD_Baseline_by_group <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group, levels = c(">=70%","60-<70%","50-<60%","<50%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated, levels = c(">=70%","60-<70%","50-<60%","<50%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing, levels = c(">=70%","60-<70%","50-<60%","<50%"))

##Category by GINA_treatment_step
Data_Oracle<-Data_Oracle %>% 
  mutate(Treatment_step= case_when(Treatment_step=="1"~"Step 1",Treatment_step=="2"~"Step 2",Treatment_step=="3"~"Step 3",Treatment_step=="4"~"Step 4",Treatment_step=="5"~"Step 5",TRUE~Treatment_step))

Data_Oracle<-Data_Oracle %>% 
  mutate(Treatment_step_1and2= case_when(Treatment_step=="Step 1"~"Step 1-2",Treatment_step=="Step 2"~"Step 1-2",TRUE~Treatment_step))
Data_Oracle$Treatment_step_1and2<-as.factor(Data_Oracle$Treatment_step_1and2)

Data_Oracle$Treatment_step <- factor(Data_Oracle$Treatment_step, levels = c("Step 3","Step 1","Step 2","Step 4","Step 5"))
Data_Oracle$Treatment_step_1and2 <- factor(Data_Oracle$Treatment_step_1and2, levels = c("Step 4","Step 1-2","Step 3","Step 5"))

##Create a treatment step_1-2_vs_3-4-5
Data_Oracle <- Data_Oracle %>%
  mutate(Treatment_step_1_2vs3_5 = factor(case_when(
    Treatment_step %in% c("Step 1", "Step 2") ~ "Step 1-2",
    Treatment_step %in% c("Step 3", "Step 4", "Step 5") ~ "Step 3-5",
    TRUE ~ Treatment_step  # Keep other values as they are
  )))


##Category by ACQ-5
Data_Oracle$ACQ5_by_group<-cut(Data_Oracle$ACQ_baseline_score_mean,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))
Data_Oracle$ACQ5_by_group_imputated<-cut(Data_Oracle$ACQ_baseline_score_mean_imputated,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))
Data_Oracle$ACQ5_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$ACQ_baseline_score_mean_imputated_no_systematically_missing,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))

##Category by BMI
Data_Oracle$BMI_by_group<-cut(Data_Oracle$BMI,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))
Data_Oracle$BMI_by_group_imputated<-cut(Data_Oracle$BMI_imputated,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))
Data_Oracle$BMI_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$BMI_imputated_no_systematically_missing,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))

##Category by Age group
Data_Oracle$Age_by_group_imputated<-cut(Data_Oracle$Age_imputated,breaks = c(-10,40,50,60,100000),labels=c("<40","40-50","50-60",">60"))

#Category of ICS DOSE_CLASS
Data_Oracle$ICS_DOSE_CLASS <- factor(Data_Oracle$ICS_DOSE_CLASS, 
                                     levels = c("0", "Low", "Medium", "High"))
Data_Oracle$ICS_DOSE_CLASS <- relevel(Data_Oracle$ICS_DOSE_CLASS, ref = "High")
Data_Oracle$ICS_DOSE_NUMERIC <- as.numeric(factor(Data_Oracle$ICS_DOSE_CLASS, 
                                                  levels = c("0", "Low", "Medium", "High"))) - 1

Data_Oracle$ICS_DOSE_CLASS_0_or_Low_combine <- factor(ifelse(Data_Oracle$ICS_DOSE_CLASS %in% c("0", "Low"), 
                                                              "0 or Low", 
                                                              as.character(Data_Oracle$ICS_DOSE_CLASS)),
                                                       levels = c("0 or Low", "Medium", "High"))
#======================================================================================================================================
#======================================================================================================================================
# PART D = INTERACTION ANALYSIS between inflammatory biomarkers and others treatable traits
### Mutivariate (no interaction)) for asthma attack
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+ offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
Results_multivariate_analysis<-res_pool[2:12,c(1,2,7,8,6)]
res_pool
#======================================================================================================================================
# Interactions analysis for asthma attack risk (Inflammatory biomarkers X others main risks factors)
##Create the loop to calculate all the interactions of inflammatory biomarkers
variable_1<- c("Eosinophils_Log","FeNO_Log")
variable_2<- c("Treatment_step_1and2","ACQ_baseline_score_mean","Any_severe_attack_previous_12m_0no_1yes","FEV1_preBD_per10_Baseline")
compilation_interaction<- data.frame()
Data_Oracle
summary(Data_Oracle$Treatment_step_1and2)

summary(Data_Oracle$Treatment_step_1and2)
AICC_values<-c()
for (v1 in variable_1) {
  for (v2 in variable_2) {
    res_comb = NULL
    if (v2 == "Treatment_step_1and2") {
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"_imputated*",v2,"+Age_imputated+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      } 
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[(nrow(res_pool)-2):nrow(res_pool),]
      interaction
    } else {
      valid_trials <- unique(Data_Oracle$Enrolled_Trial_name[!is.na(Data_Oracle[[v2]])])
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"_imputated*",v2,"_imputated+Age_imputated+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i & Enrolled_Trial_name %in% valid_trials))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      }
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[nrow(res_pool),]
    }
    compilation_interaction<- rbind(compilation_interaction,interaction)
  }
}
compilation_interaction_attack_Total<-compilation_interaction[,c("term","estimate","p.value","2.5 %","97.5 %")]
colnames(compilation_interaction_attack_Total)<-c("term","estimate","p_value","PCT_2_5","PCT_97_5")
compilation_interaction_attack_Total

##Remove the columns that are not interactions terms
Table_interaction_multivariate_Inflamm_X_Riskfactors  <- compilation_interaction_attack_Total %>%
  filter(!grepl("as.factor\\(Enrolled_Trial_name\\)", term))
Table_interaction_multivariate_Inflamm_X_Riskfactors 
##Adding the confidence interval in this format [X.XX;X.XX]
Table_interaction_multivariate_Inflamm_X_Riskfactors <- Table_interaction_multivariate_Inflamm_X_Riskfactors %>% 
  mutate(confidence_interval=paste(format(round(estimate, digits=2), nsmall = 2)," (",format(round(PCT_2_5, digits=2), nsmall = 2) ,"-",format(round(PCT_97_5, digits=2), nsmall = 2) ,")",sep=""))
Table_interaction_multivariate_Inflamm_X_Riskfactors


##Calculation of False discovery rate (FDR)
FDR_Inflamm_X_Riskfactors<-data.frame(p.adjust(Table_interaction_multivariate_Inflamm_X_Riskfactors$p_value,method= "fdr"))
colnames(FDR_Inflamm_X_Riskfactors)<- "FDR"
Table_interaction_multivariate_Inflamm_X_Riskfactors<-cbind(Table_interaction_multivariate_Inflamm_X_Riskfactors,FDR_Inflamm_X_Riskfactors)
Table_interaction_multivariate_Inflamm_X_Riskfactors

## Formatting p-value and FDR in a format with a number of digit adequate
Table_interaction_multivariate_Inflamm_X_Riskfactors <- Table_interaction_multivariate_Inflamm_X_Riskfactors %>% 
  mutate(p_value_format=paste("",format.pval(pv = p_value, digits = 4,nsmall = 3),"",sep="")) %>% 
  mutate(FDR_format= paste("",format.pval(pv = FDR, digits = 2,nsmall = 1),"",sep="")) 
Table_interaction_multivariate_Inflamm_X_Riskfactors

#Save the table in xlsx format
write_xlsx(Table_interaction_multivariate_Inflamm_X_Riskfactors,"Table_interaction_multivariate_Inflamm_X_Riskfactors.xlsx")
#======================================================================================================================================
#Evaluation of interaction Inflammation X GINA 
###GROUP GINA 1 or 2

Data_Oracle$Treatment_step
ORACLE_GINA_1_2<- subset(Data_Oracle,Treatment_step == "Step 1"|Treatment_step == "Step 2")

ORACLE_GINA_1_2$Enrolled_Trial_name
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) #+ as.factor(Enrolled_Trial_name)
                         , data = subset(ORACLE_GINA_1_2, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
Feno_GINA_1_2<-res_pool[8,]
BEC_GINA_1_2<-res_pool[9,]

###GROUP GINA 3
summary(Data_Oracle$Treatment_step)
ORACLE_GINA_3<- subset(Data_Oracle,Treatment_step == "Step 3")
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) #+ as.factor(Enrolled_Trial_name)
                         , data = subset(ORACLE_GINA_3, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
Feno_GINA_3<-res_pool[8,]
BEC_GINA_3<-res_pool[9,]

###GROUP GINA 4
summary(Data_Oracle$Treatment_step_1and2)
ORACLE_GINA_4<- subset(Data_Oracle,Treatment_step == "Step 4")
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) #+ as.factor(Enrolled_Trial_name)
                         , data = subset(ORACLE_GINA_4, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
Feno_GINA_4<-res_pool[8,]
BEC_GINA_4<-res_pool[9,]

###GROUP GINA 5
summary(Data_Oracle$Treatment_step_1and2)
ORACLE_GINA_5<- subset(Data_Oracle,Treatment_step == "Step 5")
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) #+ as.factor(Enrolled_Trial_name)
                         , data = subset(ORACLE_GINA_5, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
Feno_GINA_5<-res_pool[8,]
BEC_GINA_5<-res_pool[9,]

#Merging the data in dataframes
FeNO_GINA<-rbind(Feno_GINA_1_2,Feno_GINA_3,Feno_GINA_4,Feno_GINA_5)
BEC_GINA<-rbind(BEC_GINA_1_2,BEC_GINA_3,BEC_GINA_4,BEC_GINA_5)


#Identifying the row to the dose of ICS and numbers of patients
rownames(FeNO_GINA)<-c(paste("GINA step 1 or 2\n(N=",nrow(ORACLE_GINA_1_2)/10,")",sep=""),
                           paste("GINA step 3\n(N=",nrow(ORACLE_GINA_3)/10,")",sep=""),
                       paste("GINA step 4\n(N=",nrow(ORACLE_GINA_4)/10,")",sep=""),
                       paste("GINA step 5\n(N=",nrow(ORACLE_GINA_5)/10,")",sep=""))
rownames(BEC_GINA)<-c(paste("GINA step 1 or 2\n(N=",nrow(ORACLE_GINA_1_2)/10,")",sep=""),
                          paste("GINA step 3\n(N=",nrow(ORACLE_GINA_3)/10,")",sep=""),
                          paste("GINA step 4\n(N=",nrow(ORACLE_GINA_4)/10,")",sep=""),
                          paste("GINA step 5\n(N=",nrow(ORACLE_GINA_5)/10,")",sep=""))
FeNO_BEC_GINA<-rbind(FeNO_GINA,BEC_GINA)
FeNO_BEC_GINA
forplo(as.data.frame(FeNO_GINA[,c("estimate","2.5 %","97.5 %")]),
       row.labels = rownames(FeNO_GINA), 
       em="aRR",
       left.align=FALSE,
       xlim= c(0.2,14),
       shade.every=1,
       shade.col='gray',
       left.bar= FALSE,
       margin.left=10,
       margin.right=10,
       add.columns=p_round(FeNO_GINA[,"p.value"], digits = 2),
       add.colnames=c('p-value'),
       #groups=c(rep(1,4)),
       #grouplabs=c('GINA step'),
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=7
)

forplo(as.data.frame(FeNO_BEC_GINA[,c("estimate","2.5 %","97.5 %")]),
       row.labels = rownames(FeNO_BEC_GINA), 
       em="aRR",
       left.align=FALSE,
       xlim= c(0.2,14),
       shade.every=1,
       shade.col='gray',
       left.bar= FALSE,
       margin.left=10,
       margin.right=10,
       add.columns=p_round(FeNO_BEC_GINA[,"p.value"], digits = 2),
       add.colnames=c('p-value'),
       groups=c(rep(1,4),rep(2,4)),
       grouplabs=c('log10(FeNO)',"log10(BEC)"),
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=7
)

forplo(as.data.frame(BEC_GINA[,c("estimate","2.5 %","97.5 %")]),
       row.labels = rownames(BEC_GINA),
       em="aRR",
       left.align=FALSE,
       xlim= c(0.2,14),
       shade.every=1,
       shade.col='gray',
       left.bar= FALSE,
       margin.left=10,
       margin.right=10,
       add.columns=p_round(BEC_GINA[,"p.value"], digits = 2),
       add.colnames=c('p-value'),
       #groups=c(rep(1,4)),
       #grouplabs=c('GINA step'),
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=7
)
FeNO_BEC_GINA[c(1,5,2,6,3,7,4,8),]
FeNO_BEC_GINA
forplo(as.data.frame(FeNO_BEC_GINA[c(1,5,2,6,3,7,4,8),c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c("log10(FeNO)","log10(BEC)","log10(FeNO)","log10(BEC)","log10(FeNO)","log10(BEC)","log10(FeNO)","log10(BEC)"), 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,2),rep(3,2),rep(4,2)),
       grouplabs=c('GINA step 1-2 (N=541)','GINA step 3 (N=859)',"GINA step 4 (N=2555)", "GINA step 5 (N=2558)"),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=6,
       
       
       #Adding the p-value
       pval=p_round(FeNO_BEC_GINA[c(1,5,2,6,3,7,4,8),"p.value"], digits = 2),
       ci.sep="-",
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey"
)
# Adding a title to the top-left corner of the plot
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)



# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_T2_inflammation_in_GINA_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)

# Create the forest plot
forplo(as.data.frame(FeNO_BEC_GINA[c(1,5,2,6,3,7,4,8),c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c("log10(FeNO)", "log10(BEC)", "log10(FeNO)", "log10(BEC)", 
                      "log10(FeNO)", "log10(BEC)", "log10(FeNO)", "log10(BEC)"), 
       left.align = FALSE,
       
       #Define the groups
       groups = c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2)),
       grouplabs = c('GINA step 1-2 (N=541)', 'GINA step 3 (N=859)', 
                     "GINA step 4 (N=2555)", "GINA step 5 (N=2558)"),
       
       #Define the limits of the X axis
       xlim = c(0.2, 10),
       
       #Define the arrows
       favorlabs = c('Lower risk', 'Higher risk'),
       add.arrow.left = TRUE,
       add.arrow.right = TRUE,
       arrow.left.length = 5,
       arrow.right.length = 8.5,
       
       #Adding the p-value
       pval = p_round(FeNO_BEC_GINA[c(1,5,2,6,3,7,4,8), "p.value"], digits = 2),
       ci.sep = "-",
       
       #Define the shading
       shade.every = 1,
       shade.col = 'gray',
       
       #Define the margin
       margin.left = 11,
       margin.right = 10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char = 20,
       size = 1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col = "grey"
)

# Add a title to the top-left corner of the plot
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)

# Close the PNG device
dev.off()

#======================================================================================================================================
##Evaluation of interaction term FENO X ICS_DOSE
summary(Data_Oracle$ICS_DOSE_NUMERIC)
Data_Oracle$ICS_DOSE_CLASS <- relevel(Data_Oracle$ICS_DOSE_CLASS, ref = "High")

res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~FeNO_Log_imputated*ICS_DOSE_CLASS+Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool

summary(Data_Oracle$ICS_DOSE_CLASS_0_or_Low_combine)

Data_Oracle$ICS_DOSE_CLASS_0_or_Low_combine<- relevel(Data_Oracle$ICS_DOSE_CLASS_0_or_Low_combine, ref = "High")

res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~FeNO_Log_imputated*ICS_DOSE_CLASS_0_or_Low_combine+Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool



Data_Oracle$LABA_prescribed_0no_1yes <- factor(
  Data_Oracle$LABA_prescribed_0no_1yes,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
summary(Data_Oracle$LABA_prescribed_0no_1yes)

##Evaluation of the pronostic value of inflammatory biomarkes in the differents groups of ICS DOSE
###GROUP No ICS
ORACLE_No_ICS<- subset(Data_Oracle,ICS_DOSE_CLASS == "0")
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) #+ as.factor(Enrolled_Trial_name)
                         , data = subset(ORACLE_No_ICS, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
Feno_No_ICS<-res_pool[7,]
BEC_No_ICS<-res_pool[8,]

###GROUP Low ICS
ORACLE_low_ICS<- subset(Data_Oracle,ICS_DOSE_CLASS == "Low")
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(ORACLE_low_ICS, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
Feno_low_ICS<-res_pool[7,]
BEC_low_ICS<-res_pool[8,]

###GROUP No or Low ICS
ORACLE_No_or_low_ICS<- subset(Data_Oracle,ICS_DOSE_CLASS == "Low"|ICS_DOSE_CLASS == "0")
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(ORACLE_No_or_low_ICS, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
Feno_No_or_low_ICS<-res_pool[7,]
BEC_No_or_low_ICS<-res_pool[8,]

###GROUP Medium ICS
ORACLE_Medium_ICS<- subset(Data_Oracle,ICS_DOSE_CLASS == "Medium")
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(ORACLE_Medium_ICS, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
Feno_medium_ICS<-res_pool[7,]
BEC_medium_ICS<-res_pool[8,]
###GROUP High ICS
ORACLE_High_ICS<- subset(Data_Oracle,ICS_DOSE_CLASS == "High")
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +ACQ_baseline_score_mean_imputated  +FEV1_preBD_per10_Baseline_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(ORACLE_High_ICS, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
Feno_high_ICS<-res_pool[7,]
BEC_high_ICS<-res_pool[8,]

#Merging the data in dataframes
FeNO_ICS_dose<-rbind(Feno_No_ICS,Feno_low_ICS,Feno_medium_ICS,Feno_high_ICS)
BEC_ICS_dose<-rbind(BEC_No_ICS,BEC_low_ICS,BEC_medium_ICS,BEC_high_ICS)
summary(Data_Oracle$ICS_DOSE_CLASS)

#Identifying the row to the dose of ICS and numbers of patients
rownames(FeNO_ICS_dose)<-c(paste("No ICS (N=",sum(Data_Oracle$ICS_DOSE_CLASS == 0, na.rm = TRUE)/10,")",sep=""),
                           paste("Low (N=",sum(Data_Oracle$ICS_DOSE_CLASS == "Low", na.rm = TRUE)/10,")",sep=""),
                           paste("Medium (N=",sum(Data_Oracle$ICS_DOSE_CLASS == "Medium", na.rm = TRUE)/10,")",sep=""),
                           paste("High (N=",sum(Data_Oracle$ICS_DOSE_CLASS == "High", na.rm = TRUE)/10,")",sep=""))
rownames(BEC_ICS_dose)<-c(paste("No ICS (N=",sum(Data_Oracle$ICS_DOSE_CLASS == 0, na.rm = TRUE)/10,")",sep=""),
                           paste("Low (N=",sum(Data_Oracle$ICS_DOSE_CLASS == "Low", na.rm = TRUE)/10,")",sep=""),
                           paste("Medium (N=",sum(Data_Oracle$ICS_DOSE_CLASS == "Medium", na.rm = TRUE)/10,")",sep=""),
                           paste("High (N=",sum(Data_Oracle$ICS_DOSE_CLASS == "High", na.rm = TRUE)/10,")",sep=""))

FeNO_BEC_ICS_dose<-rbind(FeNO_ICS_dose,BEC_ICS_dose)


#Creating of the forrestplot
forplo(as.data.frame(FeNO_ICS_dose[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c('No ICS (N=218)','Low dose (N=1592)',"Medium dose (N=1400)", "High dose (N=3303)"), 
       left.align=FALSE,
       
       #Define the groups
       #groups=c(rep(1,2),rep(2,2),rep(3,2),rep(4,2)),
       #grouplabs=c('No ICS (N=218)','Low dose (N=1592)',"Medium dose (N=1400)", "High dose (N=3303)"),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=6,
       
       #Adding the p-value
       pval=p_round(FeNO_ICS_dose[,"p.value"], digits = 2),
       ci.sep="-",
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Remove the left bar
       left.bar= FALSE,
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey"
)
# Adding a title to the top-left corner of the plot
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)

BEC_ICS_dose
forplo(as.data.frame(BEC_ICS_dose[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c('No ICS (N=218)','Low dose (N=1592)',"Medium dose (N=1400)", "High dose (N=3303)"), 
       left.align=FALSE,
       
       #Define the groups
       #groups=c(rep(1,2),rep(2,2),rep(3,2),rep(4,2)),
       #grouplabs=c('No ICS (N=218)','Low dose (N=1592)',"Medium dose (N=1400)", "High dose (N=3303)"),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=6,
       
       #Adding the p-value
       pval=p_round(BEC_ICS_dose[,"p.value"], digits = 2),
       ci.sep="-",
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Remove the left bar
       left.bar= FALSE,
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey"

)
# Adding a title to the top-left corner of the plot
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)


# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_T2_inflammation_in_ICS_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)



FeNO_BEC_ICS_dose[c(1,5,2,6,3,7,4,8),]
forplo(as.data.frame(FeNO_BEC_ICS_dose[c(1,5,2,6,3,7,4,8),c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c("log10(FeNO)","log10(BEC)","log10(FeNO)","log10(BEC)","log10(FeNO)","log10(BEC)","log10(FeNO)","log10(BEC)"), 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,2),rep(3,2),rep(4,2)),
       grouplabs=c('No ICS (N=218)','Low dose (N=1592)',"Medium dose (N=1400)", "High dose (N=3303)"),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8,
       
       
       #Adding the p-value
       pval=p_round(FeNO_BEC_ICS_dose[c(1,5,2,6,3,7,4,8),"p.value"], digits = 2),
       ci.sep="-",
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey"
)
# Adding a title to the top-left corner of the plot
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)
dev.off()
#======================================================================================================================================
#Analysis of subgroup of ICS and attack history in all patients

#Subgroup 1 = ICS No and no asthma attack history 
Data_Oracle_ICS_no_Attack_no<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="0" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
nrow(Data_Oracle_ICS_no_Attack_no)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_no_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
FeNO_group_ICS_no_Attack_no <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_no_Attack_no <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_no_Attack_no
BEC_group_ICS_no_Attack_no
#Subgroup 2 = ICS No and attack asthma history
Data_Oracle_ICS_no_Attack_yes<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="0" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_no_Attack_yes$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_no_Attack_yes)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_no_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
FeNO_group_ICS_no_Attack_yes <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_no_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_no_Attack_yes 

#Subgroup 3 = ICS low and no asthma attack history 
Data_Oracle_ICS_low_Attack_no<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="Low" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_low_Attack_no$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_low_Attack_no)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

FeNO_group_ICS_low_Attack_no <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_low_Attack_no <-res_pool[7,c(1,2,7,8,6)]

#Subgroup 4 = ICS low and asthma attack history 
Data_Oracle_ICS_low_Attack_yes<-Data_Oracle%>% 
  filter(ICS_DOSE_CLASS=="Low" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_low_Attack_yes$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_low_Attack_yes)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_low_Attack_yes <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_low_Attack_yes <-res_pool[7,c(1,2,7,8,6)]

#Subgroup 5 = ICS medium and no asthma attack history 
Data_Oracle_ICS_medium_Attack_no<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="Medium" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_medium_Attack_no$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_medium_Attack_no)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
FeNO_group_ICS_medium_Attack_no <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_medium_Attack_no <-res_pool[7,c(1,2,7,8,6)]

#Subgroup 6 = ICS medium and asthma attack history 
Data_Oracle_ICS_medium_Attack_yes<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="Medium" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_medium_Attack_yes$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_medium_Attack_yes)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_medium_Attack_yes <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_medium_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_medium_Attack_yes
BEC_group_ICS_medium_Attack_yes

#Subgroup 7 = ICS high and no asthma attack history 
Data_Oracle_ICS_high_Attack_no<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="High" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_high_Attack_no$Enrolled_Trial_name)

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_high_Attack_no <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_high_Attack_no <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_high_Attack_no 
BEC_group_ICS_high_Attack_no

#Subgroup 8 = ICS high and asthma attack history 
Data_Oracle_ICS_high_Attack_yes<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="High" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_high_Attack_yes$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_no_Attack_no)/10
nrow(Data_Oracle_ICS_no_Attack_yes)/10
nrow(Data_Oracle_ICS_medium_Attack_no)/10
nrow(Data_Oracle_ICS_medium_Attack_yes)/10
nrow(Data_Oracle_ICS_high_Attack_no)/10
nrow(Data_Oracle_ICS_high_Attack_yes)/10

Sum_ICS_low <- (nrow(Data_Oracle_ICS_low_Attack_no) / 10) + (nrow(Data_Oracle_ICS_low_Attack_yes) / 10)

Sum_ICS_medium <- (nrow(Data_Oracle_ICS_medium_Attack_no) / 10) + (nrow(Data_Oracle_ICS_medium_Attack_yes) / 10)

Sum_ICS_high <- (nrow(Data_Oracle_ICS_high_Attack_no) / 10) + (nrow(Data_Oracle_ICS_high_Attack_yes) / 10)

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_high_Attack_yes <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_high_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_high_Attack_yes
BEC_group_ICS_high_Attack_yes


BEC_ICS_Attack<-rbind(
  BEC_group_ICS_low_Attack_no,
  BEC_group_ICS_low_Attack_yes,
  BEC_group_ICS_medium_Attack_no,
  BEC_group_ICS_medium_Attack_yes,
  BEC_group_ICS_high_Attack_no,
  BEC_group_ICS_high_Attack_yes
)

FeNO_ICS_Attack<-rbind(
  FeNO_group_ICS_low_Attack_no,
  FeNO_group_ICS_low_Attack_yes,
  FeNO_group_ICS_medium_Attack_no,
  FeNO_group_ICS_medium_Attack_yes,
  FeNO_group_ICS_high_Attack_no,
  FeNO_group_ICS_high_Attack_yes
)


#Identifying the row to the dose of ICS and numbers of patients
rownames(BEC_ICS_Attack)<-c(paste("No (N=",nrow(Data_Oracle_ICS_low_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_low_Attack_yes)/10,")",sep=""),
                            paste("No (N=",nrow(Data_Oracle_ICS_medium_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_medium_Attack_yes)/10,")",sep=""),
                            paste("No (N=",nrow(Data_Oracle_ICS_high_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_high_Attack_yes)/10,")",sep="")
)
rownames(FeNO_ICS_Attack)<-c(paste("No (N=",nrow(Data_Oracle_ICS_low_Attack_no)/10,")",sep=""),
                             paste("Yes (N=",nrow(Data_Oracle_ICS_low_Attack_yes)/10,")",sep=""),
                             paste("No (N=",nrow(Data_Oracle_ICS_medium_Attack_no)/10,")",sep=""),
                             paste("Yes (N=",nrow(Data_Oracle_ICS_medium_Attack_yes)/10,")",sep=""),
                             paste("No (N=",nrow(Data_Oracle_ICS_high_Attack_no)/10,")",sep=""),
                             paste("Yes (N=",nrow(Data_Oracle_ICS_high_Attack_yes)/10,")",sep="")
)





# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_BEC_in_ICS_and_Attack_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)


forplo(as.data.frame(BEC_ICS_Attack[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = rownames(BEC_ICS_Attack), 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,2),rep(3,2)),
       grouplabs=c('Low dose ICS',
                   'Medium dose ICS',
                   'High dose ICS'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       
       #Adding the p-value
       pval=p_round(BEC_ICS_Attack[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey"
)
# Adding a title to the top-left corner of the plot
mtext("B", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)

dev.off()

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_FeNO_in_ICS_and_Attack_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)


forplo(as.data.frame(FeNO_ICS_Attack[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = rownames(FeNO_ICS_Attack), 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,2),rep(3,2)),
       grouplabs=c('Low dose ICS',
                   'Medium dose ICS',
                   'High dose ICS'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       
       #Adding the p-value
       pval=p_round(FeNO_ICS_Attack[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey"
)
# Adding a title to the top-left corner of the plot
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)

dev.off()






#Evaluation of the interaction between FeNO and attack history term according to the ICS Dose
## In patient low ICS dose
Data_Oracle_ICS_low<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="Low")
nrow(Data_Oracle_ICS_low)/10
Data_Oracle_ICS_low$Age_imputated
res_comb = NULL 
Prediction_all_rcs_FeNO<-c()
Prediction_all_rcs_BEC<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low, .imp == i))
  #Generation of prediction for the effect of FeNO in RCS
  model_FeNO<- glm.nb(Number_severe_asthma_attacks_during_followup ~ rcs(FeNO_baseline_ppb_imputated, 4)*Any_severe_attack_previous_12m_0no_1yes_imputated+Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_ICS_low, .imp == i))
  Prediction_value_rcs_FeNO<- predict(model_FeNO, type="response")
  Prediction_all_rcs_FeNO<-cbind(Prediction_all_rcs_FeNO,Prediction_value_rcs_FeNO)
  #Generation of prediction for the effect of BEC in RCS
  model_BEC<- glm.nb(Number_severe_asthma_attacks_during_followup ~ rcs(Blood_Eos_baseline_x10_9_cells_per_L_imputated, 4)*Any_severe_attack_previous_12m_0no_1yes_imputated+Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_ICS_low, .imp == i))
  Prediction_value_rcs_BEC<- predict(model_BEC, type="response")
  Prediction_all_rcs_BEC<-cbind(Prediction_all_rcs_BEC,Prediction_value_rcs_BEC)
  
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_FeNO_X_Attack_in_low_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_FeNO_X_Attack_in_low_ICS

prediction_combine_rcs_FeNO<-data.frame(rowMeans(Prediction_all_rcs_FeNO))
colnames(prediction_combine_rcs_FeNO)<-"Prediction"
Table_prediction_rcs_FeNO<-cbind(subset(Data_Oracle_ICS_low, .imp == i),prediction_combine_rcs_FeNO)
Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated
Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated, levels = c("1", "0"))

Spline_FeNO_in_low_ICS<-ggplot(data = Table_prediction_rcs_FeNO, aes(x = FeNO_baseline_ppb_imputated, y = Prediction, linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(color = "darkred", size = 0.8) +  # Set all lines to black with distinct patterns
  xlab("FeNO (ppb)") +
  ylab("Estimated Annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes (N=838)", "No (N=754)"),
    values = c("solid", "dashed"),  # Specify distinct line patterns
    name = "Any attack past 12m",
    guide = guide_legend(
      keywidth = 2                      # Increase the width of the legend symbol
    )
  ) +
  scale_x_continuous(
    trans = 'log2',
    breaks = c(0, 10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.3, 0.85),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.15, vjust = 1),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 
Spline_FeNO_in_low_ICS

prediction_combine_rcs_BEC<-data.frame(rowMeans(Prediction_all_rcs_BEC))

colnames(prediction_combine_rcs_BEC)<-"Prediction"
Table_prediction_rcs_BEC<-cbind(subset(Data_Oracle_ICS_low, .imp == i),prediction_combine_rcs_BEC)
Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated
Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated, 
                                                                                 levels = c("1", "0"))

Spline_BEC_in_low_ICS<-ggplot(data = Table_prediction_rcs_BEC, aes(x = Blood_Eos_baseline_x10_9_cells_per_L_imputated, y = Prediction, linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(color = "darkred", size = 0.8) +  # Set all lines to black with distinct patterns
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes (N=838)", "No (N=754)"),
    values = c("solid", "dashed"),  # Specify distinct line patterns
    name = "Any attack past 12m",
    guide = guide_legend(
      keywidth = 2                      # Increase the width of the legend symbol
    )
  ) +
  scale_x_log10(breaks = c(0.1,0.15, 0.3, 0.6, 1, 1.5)) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(xlim = c(0.1, 2),ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.3, 0.85),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.15, vjust = 1),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 



res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated+Eosinophils_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_low_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_Attack_in_low_ICS


## In patient medium ICS dose
Data_Oracle_ICS_medium<-Data_Oracle%>% 
  filter(ICS_DOSE_CLASS=="Medium")

nrow(Data_Oracle_ICS_medium)/10
res_comb = NULL 
Prediction_all_rcs_FeNO<-c()
Prediction_all_rcs_BEC<-c()

i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium, .imp == i))
  #Generation of prediction for the effect of FeNO in RCS
  model_FeNO<- glm.nb(Number_severe_asthma_attacks_during_followup ~ rcs(FeNO_baseline_ppb_imputated, 4)*Any_severe_attack_previous_12m_0no_1yes_imputated+Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_ICS_medium, .imp == i))
  Prediction_value_rcs_FeNO<- predict(model_FeNO, type="response")
  Prediction_all_rcs_FeNO<-cbind(Prediction_all_rcs_FeNO,Prediction_value_rcs_FeNO)
  #Generation of prediction for the effect of BEC in RCS
  model_BEC<- glm.nb(Number_severe_asthma_attacks_during_followup ~ rcs(Blood_Eos_baseline_x10_9_cells_per_L_imputated, 4)*Any_severe_attack_previous_12m_0no_1yes_imputated+Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_ICS_medium, .imp == i))
  Prediction_value_rcs_BEC<- predict(model_BEC, type="response")
  Prediction_all_rcs_BEC<-cbind(Prediction_all_rcs_BEC,Prediction_value_rcs_BEC)
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_FeNO_X_Attack_in_medium_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_FeNO_X_Attack_in_medium_ICS

prediction_combine_rcs_FeNO<-data.frame(rowMeans(Prediction_all_rcs_FeNO))
colnames(prediction_combine_rcs_FeNO)<-"Prediction"
Table_prediction_rcs_FeNO<-cbind(subset(Data_Oracle_ICS_medium, .imp == i),prediction_combine_rcs_FeNO)
Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated
Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated, 
                                                                                      levels = c("1", "0"))
nrow(subset(Data_Oracle_ICS_medium,Any_severe_attack_previous_12m_0no_1yes_imputated==1))/10
nrow(subset(Data_Oracle_ICS_medium,Any_severe_attack_previous_12m_0no_1yes_imputated==0))/10



Spline_FeNO_in_medium_ICS<-ggplot(data = Table_prediction_rcs_FeNO, aes(x = FeNO_baseline_ppb_imputated, y = Prediction, linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(color = "darkred", size = 0.8) +  # Set all lines to black with distinct patterns
  xlab("FeNO (ppb)") +
  ylab("Estimated Annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes (N=1343)", "No (N=57)"),
    values = c("solid", "dashed"),  # Specify distinct line patterns
    name = "Any attack past 12m",
    guide = guide_legend(
      keywidth = 2                      # Increase the width of the legend symbol
    )
  ) +
  scale_x_continuous(
    trans = 'log2',
    breaks = c(0, 10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.3, 0.85),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.15, vjust = 1),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  )



prediction_combine_rcs_BEC<-data.frame(rowMeans(Prediction_all_rcs_BEC))

colnames(prediction_combine_rcs_BEC)<-"Prediction"
Table_prediction_rcs_BEC<-cbind(subset(Data_Oracle_ICS_medium, .imp == i),prediction_combine_rcs_BEC)
Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated
Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated, 
                                                                                     levels = c("1", "0"))

Spline_BEC_in_medium_ICS<-ggplot(data = Table_prediction_rcs_BEC, aes(x = Blood_Eos_baseline_x10_9_cells_per_L_imputated, y = Prediction, linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(color = "darkred", size = 0.8) +  # Set all lines to black with distinct patterns
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes (N=1343)", "No (N=57)"),
    values = c("solid", "dashed"),  # Specify distinct line patterns
    name = "Any attack past 12m",
    guide = guide_legend(
      keywidth = 2                      # Increase the width of the legend symbol
    )
  ) +
  scale_x_log10(breaks = c(0.1,0.15, 0.3, 0.6, 1, 1.5)) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(xlim = c(0.1, 2),ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.3, 0.85),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.15, vjust = 1),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 



res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated+Eosinophils_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_medium_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_Attack_in_medium_ICS

## In patient high ICS dose
Data_Oracle_ICS_high<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="High")
Data_Oracle_ICS_high$Any_attack_or_hospitalization_previous_12_months
nrow(Data_Oracle_ICS_high)
res_comb = NULL 
Prediction_all_rcs_FeNO<-c()
Prediction_all_rcs_BEC<-c() 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high, .imp == i))
  #Generation of prediction for the effect of FeNO in RCS
  model_FeNO<- glm.nb(Number_severe_asthma_attacks_during_followup ~ rcs(FeNO_baseline_ppb_imputated, 4)*Any_severe_attack_previous_12m_0no_1yes_imputated+Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_ICS_high, .imp == i))
  Prediction_value_rcs_FeNO<- predict(model_FeNO, type="response")
  Prediction_all_rcs_FeNO<-cbind(Prediction_all_rcs_FeNO,Prediction_value_rcs_FeNO)
  #Generation of prediction for the effect of BEC in RCS
  model_BEC<- glm.nb(Number_severe_asthma_attacks_during_followup ~ rcs(Blood_Eos_baseline_x10_9_cells_per_L_imputated, 4)*Any_severe_attack_previous_12m_0no_1yes_imputated+Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_ICS_high, .imp == i))
  Prediction_value_rcs_BEC<- predict(model_BEC, type="response")
  Prediction_all_rcs_BEC<-cbind(Prediction_all_rcs_BEC,Prediction_value_rcs_BEC)
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_FeNO_X_Attack_in_high_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]

prediction_combine_rcs_FeNO<-data.frame(rowMeans(Prediction_all_rcs_FeNO))
colnames(prediction_combine_rcs_FeNO)<-"Prediction"
Table_prediction_rcs_FeNO<-cbind(subset(Data_Oracle_ICS_high, .imp == i),prediction_combine_rcs_FeNO)
Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated
Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(Table_prediction_rcs_FeNO$Any_severe_attack_previous_12m_0no_1yes_imputated, 
                                                                                      levels = c("1", "0"))
nrow(subset(Data_Oracle_ICS_high,Any_severe_attack_previous_12m_0no_1yes_imputated==1))/10
nrow(subset(Data_Oracle_ICS_high,Any_severe_attack_previous_12m_0no_1yes_imputated==0))/10



Spline_FeNO_in_high_ICS<-ggplot(data = Table_prediction_rcs_FeNO, aes(x = FeNO_baseline_ppb_imputated, y = Prediction, linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(color = "darkred", size = 0.8) +  # Set all lines to black with distinct patterns
  xlab("FeNO (ppb)") +
  ylab("Estimated Annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes (N=3009)", "No (N=294)"),
    values = c("solid", "dashed"),  # Specify distinct line patterns
    name = "Any attack past 12m",
    guide = guide_legend(
      keywidth = 2                      # Increase the width of the legend symbol
    )
  ) +
  scale_x_continuous(
    trans = 'log2',
    breaks = c(0, 10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.3, 0.85),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.15, vjust = 1),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 
Spline_FeNO_in_high_ICS


prediction_combine_rcs_BEC<-data.frame(rowMeans(Prediction_all_rcs_BEC))

colnames(prediction_combine_rcs_BEC)<-"Prediction"
Table_prediction_rcs_BEC<-cbind(subset(Data_Oracle_ICS_high, .imp == i),prediction_combine_rcs_BEC)
Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated
Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(Table_prediction_rcs_BEC$Any_severe_attack_previous_12m_0no_1yes_imputated, 
                                                                                     levels = c("1", "0"))


Spline_BEC_in_high_ICS<-ggplot(data = Table_prediction_rcs_BEC, aes(x = Blood_Eos_baseline_x10_9_cells_per_L_imputated, y = Prediction, linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(color = "darkred", size = 0.8) +  # Set all lines to black with distinct patterns
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes (N=3009)", "No (N=294)"),
    values = c("solid", "dashed"),  # Specify distinct line patterns
    name = "Any attack past 12m",
    guide = guide_legend(
      keywidth = 2                      # Increase the width of the legend symbol
    )
  ) +
  scale_x_log10(breaks = c(0.1,0.15, 0.3, 0.6, 1, 1.5)) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(xlim = c(0.1, 2),ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.3, 0.85),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.15, vjust = 1),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 
Spline_BEC_in_high_ICS



res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated+Eosinophils_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_high_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_Attack_in_high_ICS




Table_T2_biomarkers_X_attack_in_ICSgroup<-rbind(
  BEC_group_ICS_low_Attack_no,
  BEC_group_ICS_low_Attack_yes,
  Interaction_BEC_X_Attack_in_low_ICS,
  BEC_group_ICS_medium_Attack_no,
  BEC_group_ICS_medium_Attack_yes,
  Interaction_BEC_X_Attack_in_medium_ICS,
  BEC_group_ICS_high_Attack_no,
  BEC_group_ICS_high_Attack_yes,
  Interaction_BEC_X_Attack_in_high_ICS,
  FeNO_group_ICS_low_Attack_no,
  FeNO_group_ICS_low_Attack_yes,
  Interaction_FeNO_X_Attack_in_low_ICS,
  FeNO_group_ICS_medium_Attack_no,
  FeNO_group_ICS_medium_Attack_yes,
  Interaction_FeNO_X_Attack_in_medium_ICS,
  FeNO_group_ICS_high_Attack_no,
  FeNO_group_ICS_high_Attack_yes,
  Interaction_FeNO_X_Attack_in_high_ICS
)
Table_T2_biomarkers_X_attack_in_ICSgroup
ics_dose_values <- c(
  "Low", "Low", "Low",
  "Medium", "Medium", "Medium",
  "High", "High", "High",
  "Low", "Low", "Low",
  "Medium", "Medium", "Medium",
  "High", "High", "High"
)
Table_T2_biomarkers_X_attack_in_ICSgroup$`ICS dose` <- ics_dose_values
Table_T2_biomarkers_X_attack_in_ICSgroup <- Table_T2_biomarkers_X_attack_in_ICSgroup[, 
                                                                                     c("ICS dose", setdiff(names(Table_T2_biomarkers_X_attack_in_ICSgroup), "ICS dose"))
]

Table_T2_biomarkers_X_attack_in_ICSgroup$estimate_range <- with(
  Table_T2_biomarkers_X_attack_in_ICSgroup,
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2), "-", round(`97.5 %`, 2), ")"  # Add confidence interval
  )
)
Table_T2_biomarkers_X_attack_in_ICSgroup
colnames(Table_T2_biomarkers_X_attack_in_ICSgroup)

write_xlsx(Table_T2_biomarkers_X_attack_in_ICSgroup,"Table_T2_biomarkers_X_attack_in_ICSgroup_all.xlsx")



Spline_FeNO_in_low_ICS
Spline_BEC_in_low_ICS
Spline_FeNO_in_medium_ICS
Spline_BEC_in_medium_ICS
Spline_FeNO_in_high_ICS
Spline_BEC_in_high_ICS



#Creation of the multipanel of T2 inflammation biomarkers according to asthma attack history and ICS dose
# Adjust margin for individual plots
Spline_FeNO_in_low_ICS <- Spline_FeNO_in_low_ICS + 
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 5))

Spline_BEC_in_low_ICS <- Spline_BEC_in_low_ICS + 
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 5))

Spline_FeNO_in_medium_ICS <- Spline_FeNO_in_medium_ICS + 
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 5))

Spline_BEC_in_medium_ICS <- Spline_BEC_in_medium_ICS + 
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 5))

Spline_FeNO_in_high_ICS <- Spline_FeNO_in_high_ICS + 
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 5))

Spline_BEC_in_high_ICS <- Spline_BEC_in_high_ICS + 
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 5))





plot_spline_T2_inflammation_in_ICS_and_attack <- (
  Spline_FeNO_in_low_ICS + Spline_BEC_in_low_ICS + 
    Spline_FeNO_in_medium_ICS + Spline_BEC_in_medium_ICS + 
    Spline_FeNO_in_high_ICS + Spline_BEC_in_high_ICS
) + 
  plot_layout(nrow = 3) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold") # Increase tag size
  ) # Automatically adds tags A, B, C, ...

# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_T2_inflammation_in_ICS_and_attack.png"

ggsave(output_path, plot_spline_T2_inflammation_in_ICS_and_attack, width = 12, height = 18, dpi = 600)



#Creation of the multipanel of T2 inflammation biomarkers according to asthma attack history and ICS dose (Only low and high dose)
plot_spline_T2_inflammation_in_ICS_and_attack_low_high <- (
  Spline_FeNO_in_low_ICS + Spline_BEC_in_low_ICS +
    Spline_FeNO_in_high_ICS + Spline_BEC_in_high_ICS
) + 
  plot_layout(nrow = 2) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold") # Increase tag size
  ) # Automatically adds tags A, B, C, ...

plot_spline_T2_inflammation_in_ICS_and_attack_low_high <- (
  Spline_FeNO_in_low_ICS + Spline_BEC_in_low_ICS +
    Spline_FeNO_in_high_ICS + Spline_BEC_in_high_ICS
) + 
  plot_layout(nrow = 2) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold"), # Increase tag size
    plot.tag.position = c(0.05, 0.95) # Move tags to the left
  )


Spline_FeNO_in_low_ICS <- Spline_FeNO_in_low_ICS + labs(title = "Low ICS Dose") + theme(plot.title = element_text(hjust = 0.5))
Spline_BEC_in_low_ICS <- Spline_BEC_in_low_ICS + labs(title = "Low ICS Dose") + theme(plot.title = element_text(hjust = 0.5))

Spline_FeNO_in_medium_ICS <- Spline_FeNO_in_medium_ICS + labs(title = "Medium ICS Dose") + theme(plot.title = element_text(hjust = 0.5))
Spline_BEC_in_medium_ICS <- Spline_BEC_in_medium_ICS + labs(title = "Medium ICS Dose") + theme(plot.title = element_text(hjust = 0.5))

Spline_FeNO_in_high_ICS <- Spline_FeNO_in_high_ICS + labs(title = "High ICS Dose") + theme(plot.title = element_text(hjust = 0.5))
Spline_BEC_in_high_ICS <- Spline_BEC_in_high_ICS + labs(title = "High ICS Dose") + theme(plot.title = element_text(hjust = 0.5))


plot_spline_T2_inflammation_in_ICS_and_attack_low_high <- (
  Spline_FeNO_in_low_ICS + Spline_BEC_in_low_ICS +
    Spline_FeNO_in_high_ICS + Spline_BEC_in_high_ICS
) + 
  plot_layout(nrow = 2) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold", margin = margin(r = 20)) # Add margin to move tags left
  )


# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_T2_inflammation_in_ICS_low_high_and_attack.png"
ggsave(output_path, plot_spline_T2_inflammation_in_ICS_and_attack_low_high, width = 14, height = 14, dpi = 600)


plot_spline_FeNO_in_ICS_and_attack_low_high <- (
  Spline_FeNO_in_low_ICS + Spline_FeNO_in_medium_ICS + Spline_FeNO_in_high_ICS
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold", margin = margin(r = 20)) # Add margin to move tags left
  )


# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_FeNO_in_ICS_and_attack.png"
ggsave(output_path, plot_spline_FeNO_in_ICS_and_attack_low_high, width = 18, height = 7, dpi = 600)


plot_spline_BEC_in_ICS_and_attack_low_high <- (
  Spline_BEC_in_low_ICS + Spline_BEC_in_medium_ICS + Spline_BEC_in_high_ICS
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold", margin = margin(r = 20)) # Add margin to move tags left
  )


# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_BEC_in_ICS_and_attack.png"
ggsave(output_path, plot_spline_BEC_in_ICS_and_attack_low_high, width = 18, height = 7, dpi = 600)

#======================================================================================================================================
# Similar analyis for the Effect of FeNO and BEC in subgroups according to the percentile 75-25
#Subgroup 1 = ICS No and no asthma attack history 
Data_Oracle_ICS_no_Attack_no<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="0" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
nrow(Data_Oracle_ICS_no_Attack_no$BEC_p_imputated)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_p_imputated+BEC_p_imputated + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_no_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
FeNO_group_ICS_no_Attack_no <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_no_Attack_no <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_no_Attack_no
BEC_group_ICS_no_Attack_no
#Subgroup 2 = ICS No and attack asthma history
Data_Oracle_ICS_no_Attack_yes<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="0" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_no_Attack_yes$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_no_Attack_yes)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_p_imputated+BEC_p_imputated + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_no_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
FeNO_group_ICS_no_Attack_yes <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_no_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_no_Attack_yes 

#Subgroup 3 = ICS low and no asthma attack history 
Data_Oracle_ICS_low_Attack_no<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="Low" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_low_Attack_no$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_low_Attack_no)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_p_imputated+BEC_p_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

FeNO_group_ICS_low_Attack_no <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_low_Attack_no <-res_pool[7,c(1,2,7,8,6)]

#Subgroup 4 = ICS low and asthma attack history 
Data_Oracle_ICS_low_Attack_yes<-Data_Oracle%>% 
  filter(ICS_DOSE_CLASS=="Low" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_low_Attack_yes$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_low_Attack_yes)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_p_imputated+BEC_p_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_low_Attack_yes <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_low_Attack_yes <-res_pool[7,c(1,2,7,8,6)]

#Subgroup 5 = ICS medium and no asthma attack history 
Data_Oracle_ICS_medium_Attack_no<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="Medium" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_medium_Attack_no$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_medium_Attack_no)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_p_imputated+BEC_p_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
FeNO_group_ICS_medium_Attack_no <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_medium_Attack_no <-res_pool[7,c(1,2,7,8,6)]

#Subgroup 6 = ICS medium and asthma attack history 
Data_Oracle_ICS_medium_Attack_yes<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="Medium" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_medium_Attack_yes$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_medium_Attack_yes)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_p_imputated+BEC_p_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_medium_Attack_yes <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_medium_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_medium_Attack_yes
BEC_group_ICS_medium_Attack_yes

#Subgroup 7 = ICS high and no asthma attack history 
Data_Oracle_ICS_high_Attack_no<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="High" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_high_Attack_no$Enrolled_Trial_name)

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_p_imputated+BEC_p_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_high_Attack_no <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_high_Attack_no <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_high_Attack_no 
BEC_group_ICS_high_Attack_no

#Subgroup 8 = ICS high and asthma attack history 
Data_Oracle_ICS_high_Attack_yes<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="High" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_high_Attack_yes$Enrolled_Trial_name)
nrow(Data_Oracle_ICS_no_Attack_no)/10
nrow(Data_Oracle_ICS_no_Attack_yes)/10
nrow(Data_Oracle_ICS_medium_Attack_no)/10
nrow(Data_Oracle_ICS_medium_Attack_yes)/10
nrow(Data_Oracle_ICS_high_Attack_no)/10
nrow(Data_Oracle_ICS_high_Attack_yes)/10

Sum_ICS_low <- (nrow(Data_Oracle_ICS_low_Attack_no) / 10) + (nrow(Data_Oracle_ICS_low_Attack_yes) / 10)

Sum_ICS_medium <- (nrow(Data_Oracle_ICS_medium_Attack_no) / 10) + (nrow(Data_Oracle_ICS_medium_Attack_yes) / 10)

Sum_ICS_high <- (nrow(Data_Oracle_ICS_high_Attack_no) / 10) + (nrow(Data_Oracle_ICS_high_Attack_yes) / 10)
Sum_ICS_low
Sum_ICS_medium
Sum_ICS_high

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_p_imputated+BEC_p_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_high_Attack_yes <-res_pool[6,c(1,2,7,8,6)]
BEC_group_ICS_high_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
FeNO_group_ICS_high_Attack_yes
BEC_group_ICS_high_Attack_yes


BEC_ICS_Attack<-rbind(
  BEC_group_ICS_low_Attack_no,
  BEC_group_ICS_low_Attack_yes,
  BEC_group_ICS_medium_Attack_no,
  BEC_group_ICS_medium_Attack_yes,
  BEC_group_ICS_high_Attack_no,
  BEC_group_ICS_high_Attack_yes
)

FeNO_ICS_Attack<-rbind(
  FeNO_group_ICS_low_Attack_no,
  FeNO_group_ICS_low_Attack_yes,
  FeNO_group_ICS_medium_Attack_no,
  FeNO_group_ICS_medium_Attack_yes,
  FeNO_group_ICS_high_Attack_no,
  FeNO_group_ICS_high_Attack_yes
)


#Identifying the row to the dose of ICS and numbers of patients
rownames(BEC_ICS_Attack)<-c(paste("No (N=",nrow(Data_Oracle_ICS_low_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_low_Attack_yes)/10,")",sep=""),
                            paste("No (N=",nrow(Data_Oracle_ICS_medium_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_medium_Attack_yes)/10,")",sep=""),
                            paste("No (N=",nrow(Data_Oracle_ICS_high_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_high_Attack_yes)/10,")",sep="")
)
rownames(FeNO_ICS_Attack)<-c(paste("No (N=",nrow(Data_Oracle_ICS_low_Attack_no)/10,")",sep=""),
                             paste("Yes (N=",nrow(Data_Oracle_ICS_low_Attack_yes)/10,")",sep=""),
                             paste("No (N=",nrow(Data_Oracle_ICS_medium_Attack_no)/10,")",sep=""),
                             paste("Yes (N=",nrow(Data_Oracle_ICS_medium_Attack_yes)/10,")",sep=""),
                             paste("No (N=",nrow(Data_Oracle_ICS_high_Attack_no)/10,")",sep=""),
                             paste("Yes (N=",nrow(Data_Oracle_ICS_high_Attack_yes)/10,")",sep="")
)





# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_BEC_p_25_75_in_ICS_and_Attack_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)


forplo(as.data.frame(BEC_ICS_Attack[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = rownames(BEC_ICS_Attack), 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,2),rep(3,2)),
       grouplabs=c('Low dose ICS',
                   'Medium dose ICS',
                   'High dose ICS'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       
       #Adding the p-value
       pval=p_round(BEC_ICS_Attack[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey"
)
# Adding a title to the top-left corner of the plot
mtext("B", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)

dev.off()

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_FeNO_p_25_75_in_ICS_and_Attack_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)


forplo(as.data.frame(FeNO_ICS_Attack[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = rownames(FeNO_ICS_Attack), 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,2),rep(3,2)),
       grouplabs=c('Low dose ICS',
                   'Medium dose ICS',
                   'High dose ICS'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       
       #Adding the p-value
       pval=p_round(FeNO_ICS_Attack[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey"
)
# Adding a title to the top-left corner of the plot
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)

dev.off()






#Evaluation of the interaction between FeNO and attack history term according to the ICS Dose
## In patient low ICS dose
Data_Oracle_ICS_low<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="Low")
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +BEC_p_imputated+FeNO_p_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low, .imp == i))
  
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
Interaction_FeNO_X_Attack_in_low_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]


res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +BEC_p_imputated+FeNO_p_imputated+BEC_p_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_low_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_Attack_in_low_ICS


## In patient medium ICS dose
Data_Oracle_ICS_medium<-Data_Oracle%>% 
  filter(ICS_DOSE_CLASS=="Medium")

nrow(Data_Oracle_ICS_medium)/10
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +BEC_p_imputated+FeNO_p_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_FeNO_X_Attack_in_medium_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +BEC_p_imputated+FeNO_p_imputated+BEC_p_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_medium_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]

## In patient high ICS dose
Data_Oracle_ICS_high<-Data_Oracle %>% 
  filter(ICS_DOSE_CLASS=="High")
Data_Oracle_ICS_high$Any_attack_or_hospitalization_previous_12_months
nrow(Data_Oracle_ICS_high)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +BEC_p_imputated+FeNO_p_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_FeNO_X_Attack_in_high_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +BEC_p_imputated+FeNO_p_imputated+BEC_p_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_high_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_Attack_in_high_ICS




Table_T2_biomarkers_X_attack_in_ICSgroup<-rbind(
  BEC_group_ICS_low_Attack_no,
  BEC_group_ICS_low_Attack_yes,
  Interaction_BEC_X_Attack_in_low_ICS,
  BEC_group_ICS_medium_Attack_no,
  BEC_group_ICS_medium_Attack_yes,
  Interaction_BEC_X_Attack_in_medium_ICS,
  BEC_group_ICS_high_Attack_no,
  BEC_group_ICS_high_Attack_yes,
  Interaction_BEC_X_Attack_in_high_ICS,
  FeNO_group_ICS_low_Attack_no,
  FeNO_group_ICS_low_Attack_yes,
  Interaction_FeNO_X_Attack_in_low_ICS,
  FeNO_group_ICS_medium_Attack_no,
  FeNO_group_ICS_medium_Attack_yes,
  Interaction_FeNO_X_Attack_in_medium_ICS,
  FeNO_group_ICS_high_Attack_no,
  FeNO_group_ICS_high_Attack_yes,
  Interaction_FeNO_X_Attack_in_high_ICS
)
Table_T2_biomarkers_X_attack_in_ICSgroup
ics_dose_values <- c(
  "Low", "Low", "Low",
  "Medium", "Medium", "Medium",
  "High", "High", "High",
  "Low", "Low", "Low",
  "Medium", "Medium", "Medium",
  "High", "High", "High"
)
Table_T2_biomarkers_X_attack_in_ICSgroup$`ICS dose` <- ics_dose_values
Table_T2_biomarkers_X_attack_in_ICSgroup <- Table_T2_biomarkers_X_attack_in_ICSgroup[, 
                                                                                     c("ICS dose", setdiff(names(Table_T2_biomarkers_X_attack_in_ICSgroup), "ICS dose"))
]

Table_T2_biomarkers_X_attack_in_ICSgroup$estimate_range <- with(
  Table_T2_biomarkers_X_attack_in_ICSgroup,
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2), "-", round(`97.5 %`, 2), ")"  # Add confidence interval
  )
)
Table_T2_biomarkers_X_attack_in_ICSgroup
colnames(Table_T2_biomarkers_X_attack_in_ICSgroup)
Table_T2_biomarkers_X_attack_in_ICSgroup
write_xlsx(Table_T2_biomarkers_X_attack_in_ICSgroup,"Table_T2_biomarkers_p_25_75_X_attack_in_ICSgroup_all.xlsx")


#======================================================================================================================================
#Subgroup analysis for FEV1
##Group FEV >=70%
Data_Oracle_only_with_baseline_lung_70_and_more <- Data_Oracle %>%
  filter(FEV1_preBD_Baseline_by_group_imputated==">=70%")
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung_70_and_more, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
BEC_group_FEV1_70_and_more <-res_pool[11,c(1,2,7,8,6)]
FeNO_group_FEV1_70_and_more <-res_pool[10,c(1,2,7,8,6)]



##Group FEV 60-<70%
Data_Oracle_only_with_baseline_lung_60_70 <- Data_Oracle %>%
  filter(FEV1_preBD_Baseline_by_group_imputated=="60-<70%")
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung_60_70, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
BEC_group_FEV1_60_70 <-res_pool[11,c(1,2,7,8,6)]
FeNO_group_FEV1_60_70 <-res_pool[10,c(1,2,7,8,6)]

##Group FEV 50-60
Data_Oracle_only_with_baseline_lung_50_60 <- Data_Oracle %>%
  filter(FEV1_preBD_Baseline_by_group_imputated=="50-<60%")
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung_50_60, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
BEC_group_FEV1_50_60 <-res_pool[11,c(1,2,7,8,6)]
FeNO_group_FEV1_50_60 <-res_pool[10,c(1,2,7,8,6)]

##Group FEV <50

Data_Oracle_only_with_baseline_lung_50_and_less <- Data_Oracle %>%
  filter(FEV1_preBD_Baseline_by_group_imputated=="<50%")

summary(Data_Oracle_only_with_baseline_lung_50_and_less$Enrolled_Trial_name)
subset_data <- subset(Data_Oracle_only_with_baseline_lung_50_and_less, .imp == 3)
summary(subset_data)

summary(Data_Oracle_only_with_baseline_lung_50_and_less$Treatment_step_1and2)


Data_Oracle_only_with_baseline_lung_50_and_less$Treatment_step_1_2_3_combine <- ifelse(
  Data_Oracle_only_with_baseline_lung_50_and_less$Treatment_step_1and2 %in% c("Step 1-2", "Step 3"),
  "Step 1-3",
  Data_Oracle_only_with_baseline_lung_50_and_less$Treatment_step_1and2
)
summary(Data_Oracle_only_with_baseline_lung_50_and_less$Treatment_step_1and2)

res_comb = NULL 
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1_2_3_combine
                         +Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung_50_and_less, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
BEC_group_FEV1_50_and_less <-res_pool[10,c(1,2,7,8,6)]
FeNO_group_FEV1_50_and_less <-res_pool[9,c(1,2,7,8,6)]


BEC_FEV1<-rbind(BEC_group_FEV1_70_and_more,BEC_group_FEV1_60_70,BEC_group_FEV1_50_60,BEC_group_FEV1_50_and_less)
FeNO_FEV1<-rbind(FeNO_group_FEV1_70_and_more,FeNO_group_FEV1_60_70,FeNO_group_FEV1_50_60,FeNO_group_FEV1_50_and_less)

#Identifying the row to the dose of ICS and numbers of patients
rownames(BEC_FEV1)<-c(paste("FEV1 >=70%\n(N=",round(sum(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated==">=70%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 60-<70%\n(N=",round(sum(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated=="60-<70%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 50-<60%\n(N=",round(sum(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated=="50-<60%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 <50%\n(N=",round(sum(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated=="<50%", na.rm = TRUE)/10),")",sep=""))
rownames(FeNO_FEV1)<-c(paste("FEV1 >=70%\n(N=",round(sum(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated==">=70%", na.rm = TRUE)/10),")",sep=""),
                       paste("FEV1 60-<70%\n(N=",round(sum(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated=="60-<70%", na.rm = TRUE)/10),")",sep=""),
                       paste("FEV1 50-<60%\n(N=",round(sum(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated=="50-<60%", na.rm = TRUE)/10),")",sep=""),
                       paste("FEV1 <50%\n(N=",round(sum(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated=="<50%", na.rm = TRUE)/10),")",sep=""))
BEC_FEV1
BEC_FEV1
FeNO_FEV1
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_BEC_in_FEV1_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)

forplo(as.data.frame(BEC_FEV1[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = rownames(BEC_FEV1), 
       left.align=FALSE,
       
       #Define the groups
       #groups=c(rep(1,2),rep(2,2),rep(3,2)),
       #grouplabs=c('Low dose ICS',
                   #'Medium dose ICS',
                   #'High dose ICS'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       #Remove the left bar
       left.bar= FALSE,
       
       #Adding the p-value
       pval=p_round(BEC_FEV1[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)
dev.off()

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_FeNO_in_FEV1_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)

forplo(as.data.frame(FeNO_FEV1[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = rownames(FeNO_FEV1), 
       left.align=FALSE,
       
       #Define the groups
       #groups=c(rep(1,2),rep(2,2),rep(3,2)),
       #grouplabs=c('Low dose ICS',
       #'Medium dose ICS',
       #'High dose ICS'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       #Remove the left bar
       left.bar= FALSE,
       
       #Adding the p-value
       pval=p_round(FeNO_FEV1[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)
dev.off()

#======================================================================================================================================
#Subgroup analysis of the effect of attack history 
#Subgroup 1 = No asthma attack history
Data_Oracle_Attack_no<-Data_Oracle %>% 
  filter(Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
Data_Oracle_Attack_no

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2 +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 

FeNO_group_Attack_no <-res_pool[10,c(1,2,7,8,6)]
BEC_group_Attack_no <-res_pool[11,c(1,2,7,8,6)]

#Subgroup 2 = ICS No and attack asthma history
Data_Oracle_Attack_yes<-Data_Oracle %>% 
  filter(Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
Data_Oracle$Treatment_step_1and2
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2 +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
FeNO_group_Attack_yes <-res_pool[10,c(1,2,7,8,6)]
BEC_group_Attack_yes <-res_pool[11,c(1,2,7,8,6)]

FeNO_group_Attack<-rbind(FeNO_group_Attack_no,FeNO_group_Attack_yes)
BEC_group_Attack<-rbind(BEC_group_Attack_no,BEC_group_Attack_yes)

paste("No (N=",(nrow(Data_Oracle_Attack_no))/10,")",sep="")
rownames(BEC_group_Attack)<-c(paste("No (N=",(nrow(Data_Oracle_Attack_no))/10,")",sep=""),
                      paste("Yes (N=",(nrow(Data_Oracle_Attack_yes))/10,")",sep=""))
rownames(FeNO_group_Attack)<-c(paste("No (N=",(nrow(Data_Oracle_Attack_no))/10,")",sep=""),
                       paste("Yes (N=",(nrow(Data_Oracle_Attack_yes))/10,")",sep=""))
FeNO_BEC_group_Attack<-rbind(FeNO_group_Attack,BEC_group_Attack)
FeNO_BEC_group_Attack
rownames(FeNO_BEC_group_Attack[1:2,])


forplo(as.data.frame(FeNO_BEC_group_Attack[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c(rownames(FeNO_BEC_group_Attack[1:2,]),rownames(FeNO_BEC_group_Attack[1:2,])), 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,2)),
       grouplabs=c('log10(FeNO)',
                   'log10(BEC)'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       #Remove the left bar
       left.bar= TRUE,
       
       #Adding the p-value
       pval=p_round(FeNO_BEC_group_Attack[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_T2_inflammation_in_attack_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)



FeNO_BEC_group_Attack[c(1,3,2,4),]
rownames(FeNO_BEC_group_Attack[c(1,2),])
FeNO_BEC_group_Attack
forplo(as.data.frame(FeNO_BEC_group_Attack[c(1,3,2,4),c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c('log10(FeNO)','log10(BEC)','log10(FeNO)','log10(BEC)') ,
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,2)),
       grouplabs=c("No attack past 12m\n(N=1303)",
                   "≥1 attack past 12m\n(N=5210)"),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       #Remove the left bar
       left.bar= TRUE,
       
       #Adding the p-value
       pval=p_round(FeNO_BEC_group_Attack[c(1,3,2,4),"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)
dev.off()



#subgroup analysis 
Data_Oracle_Attack_yes$Blood_Eos_baseline_x10_9_cells_per_L_imputated

#Subgroup analysis by BEC categories
#IN patient with asthma attack history
Data_Oracle_Attack_yes$BEC_Category <- cut(
  Data_Oracle_Attack_yes$Blood_Eos_baseline_x10_9_cells_per_L_imputated,
  breaks = c(-Inf, 0.15, 0.3, 0.6, Inf),  # Define the breakpoints
  labels = c("BEC<0.15", "BEC=0.15-0.3", "BEC=0.3-0.6", "BEC>0.6"),  # Define the labels
  right = FALSE  # Include the left edge in the interval, e.g., 0.15 belongs to "BEC=0.15-0.3"
)
summary(Data_Oracle_Attack_yes$BEC_Category)

res_comb = NULL 

i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2 +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+BEC_Category + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
BEC_category_on_attack_in_attack_yes<-res_pool[11:13,c(1,2,7,8,6)]


#IN patient without asthma attack history
Data_Oracle_Attack_no$BEC_Category <- cut(
  Data_Oracle_Attack_no$Blood_Eos_baseline_x10_9_cells_per_L_imputated,
  breaks = c(-Inf, 0.15, 0.3, 0.6, Inf),  # Define the breakpoints
  labels = c("BEC<0.15", "BEC=0.15-0.3", "BEC=0.3-0.6", "BEC>0.6"),  # Define the labels
  right = FALSE  # Include the left edge in the interval, e.g., 0.15 belongs to "BEC=0.15-0.3"
)
summary(Data_Oracle_Attack_no$BEC_Category)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2 +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+BEC_Category + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
BEC_category_on_attack_in_attack_no<-res_pool[11:13,c(1,2,7,8,6)]
BEC_category_on_attack_in_attack_yes


BEC_category_in_attack_groups<-rbind(
  BEC_category_on_attack_in_attack_no,
  BEC_category_on_attack_in_attack_yes
)
BEC_category_in_attack_groups

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_BEC_subgroups_on_risk_according_to_attack_history.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)
BEC_category_in_attack_groups

forplo(as.data.frame(BEC_category_in_attack_groups[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR*",
       
       #Define the labels
       row.labels = c('0.15-0.3 x10^9 cells/L','0.3-0.6 x10^9 cells/L','>0.6 x10^9 cells/L','0.15-0.3 x10^9 cells/L','0.3-0.6 x10^9 cells/L','>0.6 x10^9 cells/L') ,
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,3),rep(2,3)),
       grouplabs=c("BEC categories with\nno attack past 12m (N=1303)",
                   "BEC categories with\n≥1 attack past 12m (N=5210)"),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=6.5,
       
       #Remove the left bar
       left.bar= TRUE,
       
       #Adding the p-value
       pval=p_round(BEC_category_in_attack_groups[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=13,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)

dev.off()



#Subgroup analysis by FeNO categories
Data_Oracle_Attack_yes$FENO_Category <- cut(
  Data_Oracle_Attack_yes$FeNO_baseline_ppb_imputated,
  breaks = c(-Inf, 25, 50, 75, Inf),  # Define the breakpoints
  labels = c("FeNO<25", "FeNO=25-<50", "FeNO=50-75", "FeNO>75"),  # Define the labels
  right = FALSE  # Include the left edge in the interval, e.g., 0.15 belongs to "BEC=0.15-0.3"
)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2 +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+FENO_Category + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_category_on_attack_in_attack_yes<-res_pool[11:13,c(1,2,7,8,6)]
FeNO_category_on_attack_in_attack_yes

summary(Data_Oracle_Attack_no$FENO_Category)
Data_Oracle_Attack_no$FENO_Category <- cut(
  Data_Oracle_Attack_no$FeNO_baseline_ppb_imputated,
  breaks = c(-Inf, 25, 50, 75, Inf),  # Define the breakpoints
  labels = c("FeNO<25", "FeNO=25-<50", "FeNO=50-75", "FeNO>75"),  # Define the labels
  right = FALSE  # Include the left edge in the interval, e.g., 0.15 belongs to "BEC=0.15-0.3"
)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2 +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+FENO_Category + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_category_on_attack_in_attack_no<-res_pool[11:13,c(1,2,7,8,6)]


FeNO_category_in_attack_groups<-rbind(
  FeNO_category_on_attack_in_attack_no,
  FeNO_category_on_attack_in_attack_yes
)


round(summary(Data_Oracle_Attack_no$FENO_Category)/10)
round(summary(Data_Oracle_Attack_yes$FENO_Category)/10)

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_FeNO_subgroups_on_risk_according_to_attack_history.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)





forplo(as.data.frame(FeNO_category_in_attack_groups[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR*",
       
       #Define the labels
       row.labels = c('25-<50 ppb (N=688)','50-75 ppb (N=340)','>75 ppb (N=145)','25-<50 ppb (N=2761)','50-75 ppb (N=1446)','>75 ppb (N=1446)') ,
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,3),rep(2,3)),
       grouplabs=c("FeNO categories with\nno attack past 12m (N=1303)",
                   "FeNO categories with\n≥1 attack past 12m (N=5210)"),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=6.5,
       
       #Remove the left bar
       left.bar= TRUE,
       
       #Adding the p-value
       pval=p_round(FeNO_category_in_attack_groups[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=13,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)

dev.off()





#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
## Spline curve for the significant interactions
Data_Oracle$Treatment_step_1and2
#======================================================================================================================================
##Eosinophils_X_FEV1

## BEC_X_FEV1
variable_1 <- "Blood_Eos_baseline_x10_9_cells_per_L_imputated"
variable_2 <- "FEV1_preBD_Baseline_by_group_imputated"

Prediction_all_rcs <- c()
for (i in 1:10) {
  model <- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(", 
                                   variable_1, ", 4)*", variable_2, 
                                   "+Age_imputated+Gender_imputated+BMI_per_5_imputated+
                                   Treatment_step_1and2+ACQ_baseline_score_mean_imputated+
                                   Any_severe_attack_previous_12m_0no_1yes_imputated+
                                   FeNO_Log_imputated+offset(log(Follow_up_duration_days)) + 
                                   as.factor(Enrolled_Trial_name)")), 
                  data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs <- predict(model, type = "response")
  Prediction_all_rcs <- cbind(Prediction_all_rcs, Prediction_value_rcs)
}

prediction_combine_rcs <- data.frame(rowMeans(Prediction_all_rcs))
colnames(prediction_combine_rcs) <- "Prediction"

Table_prediction_rcs <- cbind(subset(Data_Oracle, .imp == 1), prediction_combine_rcs)

# Plot
Spline_BEC_X_FEV1 <- ggplot(data = Table_prediction_rcs, 
                            aes(x = .data[[variable_1]], 
                                y = Prediction, 
                                linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkblue", alpha = 0.5) +
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated Annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    name = "FEV1 (% of predicted)",
    labels = c(">= 70% (N=2304)", "60-<70% (N=1658)", "50-<60% (N=1305)", "<50% (N=1246)"),
    guide = guide_legend(keywidth = 2)
  ) +
  # Fix: Remove 0 from breaks and use log10 scale
  scale_x_log10(
    breaks = c(0.1, 0.3, 0.6, 1, 1.5)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) +
  theme_minimal(base_size = 10) +
  coord_cartesian(xlim = c(0.1, 2), ylim = c(0.4, 3)) +
  theme(
    legend.position = c(0.25, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) 

Spline_BEC_X_FEV1

##FeNO_X_FEV1
variable_1<-"FeNO_baseline_ppb_imputated"
variable_2<-"FEV1_preBD_Baseline_by_group_imputated"

Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated+ACQ_baseline_score_mean_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))

colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle, .imp == i),prediction_combine_rcs)

Spline_FeNO_X_FEV1<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkblue", alpha = 0.5) +
  xlab('FeNO (ppb)') +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), 
                        name = "FEV1 (% of predicted)" ,
                        labels = c(">= 70% (N=2304)", "60-<70% (N=1658)", "50-<60% (N=1305)", "<50% (N=1246)"),  # Custom labels
                        guide = guide_legend(
                          keywidth = 2                      # Increase the width of the legend symbol
                        )) +
  scale_x_continuous(
    trans = 'log2',
    breaks = c(0, 10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2,2.5)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(ylim = c(0.4, 3)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.25, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  )
Spline_FeNO_X_FEV1

## FeNO_X_FEV1
variable_1 <- "FeNO_baseline_ppb_imputated"
variable_2 <- "FEV1_preBD_Baseline_by_group_imputated"

Prediction_all_rcs <- c()
for (i in 1:10) {
  model <- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(", 
                                   variable_1, ", 4)*", variable_2, 
                                   "+Age_imputated+Gender_imputated+BMI_per_5_imputated+
                                   Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated+
                                   ACQ_baseline_score_mean_imputated+Eosinophils_Log_imputated+
                                   offset(log(Follow_up_duration_days)) + 
                                   as.factor(Enrolled_Trial_name)")), 
                  data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs <- predict(model, type = "response")
  Prediction_all_rcs <- cbind(Prediction_all_rcs, Prediction_value_rcs)
}

prediction_combine_rcs <- data.frame(rowMeans(Prediction_all_rcs))
colnames(prediction_combine_rcs) <- "Prediction"

# Fix: Use .imp == 1 instead of last i value
Table_prediction_rcs <- cbind(subset(Data_Oracle, .imp == 1), prediction_combine_rcs)

# Plot
Spline_FeNO_X_FEV1 <- ggplot(data = Table_prediction_rcs, 
                             aes(x = .data[[variable_1]], 
                                 y = Prediction, 
                                 linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkblue", alpha = 0.5) +
  xlab("FeNO (ppb)") +
  ylab("Estimated Annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    name = "FEV1 (% of predicted)",
    labels = c(">= 70% (N=2304)", "60-<70% (N=1658)", "50-<60% (N=1305)", "<50% (N=1246)"),
    guide = guide_legend(keywidth = 2)
  ) +
  # Fix: Remove 0 from breaks and use log10 scale
  scale_x_log10(
    breaks = c(10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) +
  theme_minimal(base_size = 10) +
  coord_cartesian(ylim = c(0.4, 3)) +
  theme(
    legend.position = c(0.25, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) 

Spline_FeNO_X_FEV1



#Creation of the multipanel of T2 inflammation biomarkers according to asthma attack history and ICS dose
plot_spline_T2_inflammation_in_FEV1 <- (
  Spline_BEC_X_FEV1 + Spline_FeNO_X_FEV1
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold"), # Increase tag size
    plot.margin = margin(t = 10, r = 10, b = 10, l = 5) # Adjust margins: top, right, bottom, left
  ) # Automatically adds tags A, B, C, ...

# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_T2_inflammation_in_FEV1_groups.png"
ggsave(output_path, plot_spline_T2_inflammation_in_FEV1, width = 12, height = 6, dpi = 600)
#======================================================================================================================================

## FeNO_X_attack
variable_1 <- "FeNO_baseline_ppb_imputated"
variable_2 <- "Any_severe_attack_previous_12m_0no_1yes_imputated"

Prediction_all_rcs <- c()
for (i in 1:10) {
  model <- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(", 
                                   variable_1, ", 4)*", variable_2, 
                                   "+Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+
                                   ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+
                                   Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) +
                                   as.factor(Enrolled_Trial_name)")), 
                  data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs <- predict(model, type = "response")
  Prediction_all_rcs <- cbind(Prediction_all_rcs, Prediction_value_rcs)
}

prediction_combine_rcs <- data.frame(rowMeans(Prediction_all_rcs))
colnames(prediction_combine_rcs) <- "Prediction"

# Fix: Use .imp == 1 instead of last i value
Table_prediction_rcs <- cbind(subset(Data_Oracle, .imp == 1), prediction_combine_rcs)

# Fix: Ensure factor conversion is correct
Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(as.character(Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated), 
                                                                                 levels = c("1", "0"))

# Plot
Spline_FeNO_X_Attack <- ggplot(data = Table_prediction_rcs, 
                               aes(x = FeNO_baseline_ppb_imputated, 
                                   y = Prediction, 
                                   linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkred", alpha = 0.5) +
  xlab("FeNO (ppb)") +
  ylab("Estimated Annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes (N=5210)", "No (N=1303)"),
    values = c("solid", "dashed"),
    name = "Any attack past 12m",
    guide = guide_legend(keywidth = 2)
  ) +
  # Fix: Remove 0 from breaks and use log10 scale
  scale_x_log10(
    breaks = c(10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2)) +
  theme_minimal(base_size = 10) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) 

Spline_FeNO_X_Attack



## EosX_attack
variable_1 <- "Blood_Eos_baseline_x10_9_cells_per_L_imputated"
variable_2 <- "Any_severe_attack_previous_12m_0no_1yes_imputated"

Prediction_all_rcs <- c()
for (i in 1:10) {
  model <- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(", 
                                   variable_1, ", 4)*", variable_2, 
                                   "+Age_imputated+Gender_imputated+BMI_per_5_imputated+
                                   Treatment_step_1and2+ACQ_baseline_score_mean_imputated+
                                   FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+
                                   offset(log(Follow_up_duration_days)) + 
                                   as.factor(Enrolled_Trial_name)")), 
                  data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs <- predict(model, type = "response")
  Prediction_all_rcs <- cbind(Prediction_all_rcs, Prediction_value_rcs)
}

prediction_combine_rcs <- data.frame(rowMeans(Prediction_all_rcs))
colnames(prediction_combine_rcs) <- "Prediction"

# Fix: Use .imp == 1 instead of last i value
Table_prediction_rcs <- cbind(subset(Data_Oracle, .imp == 1), prediction_combine_rcs)

# Fix: Ensure Attack history variable is a factor
Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated <- 
  factor(as.character(Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated), 
         levels = c("1", "0"))

# Plot
Spline_BEC_X_Attack <- ggplot(data = Table_prediction_rcs, 
                              aes(x = Blood_Eos_baseline_x10_9_cells_per_L_imputated, 
                                  y = Prediction, 
                                  linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkred", alpha = 0.5) +
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated Annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes (N=5210)", "No (N=1303)"),
    values = c("solid", "dashed"),
    name = "Any attack past 12m",
    guide = guide_legend(keywidth = 2)
  ) +
  # Fix: Remove 0 from breaks and use log10 scale
  scale_x_log10(
    breaks = c(0.1, 0.15, 0.3, 0.6, 1, 1.5)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2)) +
  theme_minimal(base_size = 10) +
  coord_cartesian(xlim = c(0.1, 2), ylim = c(0, 2)) +
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) 

Spline_BEC_X_Attack








#Creation of the multipanel of T2 inflammation biomarkers according to asthma attack history 
plot_spline_T2_inflammation_in_Attack <- (
  Spline_FeNO_X_Attack + Spline_BEC_X_Attack
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold") # Increase tag size
  ) # Automatically adds tags A, B, C, ...

# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_T2_inflammation_in_Attack_groups.png"
ggsave(output_path, plot_spline_T2_inflammation_in_Attack, width = 12, height = 6, dpi = 600)

# Creation of the multipanel of T2 inflammation biomarkers according to asthma attack history 
plot_spline_T2_inflammation_in_Attack <- (
  Spline_FeNO_X_Attack + Spline_BEC_X_Attack
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold"), # Increase tag size
    plot.margin = margin(t = 10, r = 10, b = 10, l = 5) # Adjust margins: top, right, bottom, left
  )

# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_T2_inflammation_in_Attack_groups.png"
ggsave(output_path, plot_spline_T2_inflammation_in_Attack, width = 12, height = 6, dpi = 600)
#==========================================================================================================================================
#Evaluation of the effect of FeNO in patient with attack
#Subgroup Attack
Attack_Yes_Group<-subset(Data_Oracle,Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
Attack_Yes_Group
# Initialize a list to store results for each age group
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(Attack_Yes_Group$Age_by_group_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, Age_by_group_imputated == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    imputed_data
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        #Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_age_group<-results_df

FeNO_in_Attack_per_age_group
# Sex
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(Attack_Yes_Group$Gender_imputated)

# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, Gender_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        #Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_sex_group<-results_df

FeNO_in_Attack_per_sex_group
# BMI
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(Attack_Yes_Group$BMI_by_group_imputated)

# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, BMI_by_group_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        #BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_BMI_group<-results_df
FeNO_in_Attack_per_BMI_group



# Smoking history
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(Attack_Yes_Group$Smoking_Statut_imputated)
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, Smoking_Statut_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))
results_df

FeNO_in_Attack_per_smoking_history<-results_df
FeNO_in_Attack_per_smoking_history

# Atopy
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(Attack_Yes_Group$Atopy_history_imputated)
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, Atopy_history_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))
results_df

FeNO_in_Attack_per_Atopy_history<-results_df

# Airborne allergen sensibilisation
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(Attack_Yes_Group$Airborne_allergen_sensibilisation_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, Airborne_allergen_sensibilisation_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    imputed_data
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        #Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))
results_df

FeNO_in_Attack_per_Airborne_allergen_sensibilisation<-results_df

# CRSsNP
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(Attack_Yes_Group$CRSsNP_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, CRSsNP_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))
results_df

FeNO_in_Attack_per_CRSsNP<-results_df

# CRSwNP
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(Attack_Yes_Group$CRSwNP_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, CRSwNP_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))
results_df

FeNO_in_Attack_per_CRSwNP<-results_df

# GINA step
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- c("Step 1-2","Step 3","Step 4","Step 5")
summary(Attack_Yes_Group$Treatment_step_1and2)

# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, Treatment_step_1and2 == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        #Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_GINA_group<-results_df
FeNO_in_Attack_per_GINA_group


# ICS_DOSE_CLASS
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- c("Low","Medium","High")
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, ICS_DOSE_CLASS == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        #Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_ICS_group<-results_df
FeNO_in_Attack_per_ICS_group

# ACQ5_by_group_imputated
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated

summary(Attack_Yes_Group$ACQ_baseline_score_mean_imputated)

Attack_Yes_Group <- Attack_Yes_Group %>%
  mutate(ACQ_group_2 = case_when(
    ACQ_baseline_score_mean_imputated < 3 ~ "<3",
    ACQ_baseline_score_mean_imputated >= 3 ~ ">=3"
  ))

# Convert to factor for easier analysis and consistent levels
Attack_Yes_Group$ACQ_group_2 <- factor(
  Attack_Yes_Group$ACQ_group_2, 
  levels = c("<3", ">=3")
)
groups <- levels(Attack_Yes_Group$ACQ_group_2)


# Loop through each ACQ group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, ACQ_group_2 == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        #ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_ACQ_group<-results_df
FeNO_in_Attack_per_ACQ_group

# FEV1_preBD_Baseline_by_group_imputated
res_comb_by_group <- list()
# Get the unique values
Attack_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated<-as.factor(Attack_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated)
groups <- levels(Attack_Yes_Group$FEV1_preBD_Baseline_by_group_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, FEV1_preBD_Baseline_by_group_imputated == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    imputed_data
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        #FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_FEV1_group<-results_df



# Eosinophils_by_group_imputated
res_comb_by_group <- list()
# Get the unique values
Attack_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated<-as.factor(Attack_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated)
groups <- levels(Attack_Yes_Group$Eosinophils_by_group_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, Eosinophils_by_group_imputated == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        #Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_BEC_group<-results_df
FeNO_in_Attack_per_BEC_group


# IgE_by_group_imputated
res_comb_by_group <- list()
# Get the unique values
Attack_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated<-as.factor(Attack_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated)
groups <- levels(Attack_Yes_Group$IgE_by_group_imputated)
summary(Attack_Yes_Group$IgE_by_group_imputated)
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(Attack_Yes_Group, IgE_by_group_imputated == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    imputed_data
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        #Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the FeNO_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "FeNO_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

FeNO_in_Attack_per_IgE_group<-results_df
FeNO_in_Attack_per_IgE_group


FeNO_Attack_per_groups<-rbind(
  FeNO_in_Attack_per_age_group,
  FeNO_in_Attack_per_sex_group,
  FeNO_in_Attack_per_BMI_group,
  FeNO_in_Attack_per_smoking_history,
  FeNO_in_Attack_per_Atopy_history,
  FeNO_in_Attack_per_Airborne_allergen_sensibilisation,
  FeNO_in_Attack_per_CRSsNP,
  FeNO_in_Attack_per_CRSwNP,
  FeNO_in_Attack_per_ICS_group,
  FeNO_in_Attack_per_ACQ_group,
  #FeNO_in_Attack_per_Attack_group,
  FeNO_in_Attack_per_FEV1_group,
  FeNO_in_Attack_per_BEC_group,
  FeNO_in_Attack_per_IgE_group
)


# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitivity_analysis_Forest_plot_FeNO_in_Attack_according_others_characteristics.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 9, height = 15, units = "in", res = 300)

forplo(as.data.frame(FeNO_Attack_per_groups[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = FeNO_Attack_per_groups$Group, 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,4),rep(2,2),rep(3,4),rep(4,2),rep(5,2),rep(6,2),rep(7,2),rep(8,2),rep(9,3),rep(10,2),rep(11,4),rep(12,3),rep(13,3)),
       grouplabs=c('Age',"Sex","BMI","Smoking history","Atopy history","Airborne allergen sensitisation","CRSsNP","CRSwNP","ICS dose","ACQ-5","FEV1%","BEC","IgE"),
       
       #Define the limits of the X axis
       xlim= c(0.1,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=8,
       arrow.right.length=8,
       
       #Remove the left bar
       left.bar= TRUE,
       
       
       #Adding the p-value
       pval=p_round(FeNO_Attack_per_groups[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=14,
       margin.right=8,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5#,
       
       #Characteristic of the error bar (color when not significant)
       #insig.col="grey",
       #diamond=nrow(results_df_with_all),
)
dev.off()

FeNO_Attack_per_groups_truncated<-FeNO_Attack_per_groups[c(1,2,3,4,5,6,7,8,9,10,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32),]
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitivity_analysis_Forest_plot_FeNO_in_Attack_according_others_main_characteristics.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 9, height = 12, units = "in", res = 300)

forplo(as.data.frame(FeNO_Attack_per_groups_truncated[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = FeNO_Attack_per_groups_truncated$Group, 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,4),rep(2,2),rep(3,4),rep(4,2),rep(5,2),rep(6,2),rep(7,2),rep(8,3),rep(9,2),rep(10,4),rep(11,3)),
       grouplabs=c('Age',"Sex","BMI","Atopy history","Airborne allergen sensitisation","CRSsNP","CRSwNP","ICS dose","ACQ-5","FEV1%","BEC"),
       
       #Define the limits of the X axis
       xlim= c(0.1,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=8,
       arrow.right.length=8,
       
       #Remove the left bar
       left.bar= TRUE,
       
       
       #Adding the p-value
       pval=p_round(FeNO_Attack_per_groups_truncated[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=14,
       margin.right=8,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5#,
       
       #Characteristic of the error bar (color when not significant)
       #insig.col="grey",
       #diamond=nrow(results_df_with_all),
)
dev.off()

#======================================================================================================================================
##FeNO_X_GINA
variable_1<-"FeNO_baseline_ppb_imputated"

variable_2<-"Treatment_step_1and2"
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Age_imputated+Gender_imputated+BMI_per_5_imputated+ACQ_baseline_score_mean_imputated+Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))

colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle, .imp == i),prediction_combine_rcs)

# Set the desired order of levels in Treatment_step_1and2
Table_prediction_rcs$Treatment_step_1and2
Table_prediction_rcs$Treatment_step_1and2 <- factor(Table_prediction_rcs$Treatment_step_1and2, 
                                                      levels = c("Step 1-2", "Step 3", "Step 4", "Step 5"))

Spline_FeNO_X_GINA<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "#6A0DAD", alpha = 0.5) +
  xlab('FeNO (ppb)') +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), 
                        name = "GINA \nTreatment step",
                        guide = guide_legend(
                          keywidth = 2                      # Increase the width of the legend symbol
                        )
  ) + # Use distinct line patterns
  scale_x_continuous(
    trans = 'log2',
    breaks = c(0, 10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  )
Spline_FeNO_X_GINA

##EOS_X_GINA
variable_1<-"Blood_Eos_baseline_x10_9_cells_per_L_imputated"
variable_2<-"Treatment_step_1and2"
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Age_imputated+Gender_imputated+BMI_per_5_imputated+ACQ_baseline_score_mean_imputated+Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))

colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle, .imp == i),prediction_combine_rcs)

# Set the desired order of levels in Treatment_step_1and2
Table_prediction_rcs$Treatment_step_1and2 <- factor(Table_prediction_rcs$Treatment_step_1and2, 
                                                      levels = c("Step 1-2", "Step 3", "Step 4", "Step 5"))

Spline_BEC_X_GINA<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "#6A0DAD", alpha = 0.5) +
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), 
                        name = "GINA \nTreatment step",
                        guide = guide_legend(
                          keywidth = 2                      # Increase the width of the legend symbol
                        )) +
  scale_x_log10(breaks = c(0.1,0.15, 0.3, 0.6, 1, 1.5)) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(xlim = c(0.1, 2),ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 

Spline_BEC_X_GINA

#Creation of the multipanel of T2 inflammation biomarkers according to the GINA step
plot_spline_T2_inflammation_in_GINA <- (
  Spline_FeNO_X_GINA + Spline_BEC_X_GINA
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold"), # Increase tag size
    plot.margin = margin(t = 10, r = 10, b = 10, l = 5) # Adjust margins: top, right, bottom, left
  ) # Automatically adds tags A, B, C, ...


# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_T2_inflammation_in_GINA_groups.png"
ggsave(output_path, plot_spline_T2_inflammation_in_GINA , width = 12, height = 6, dpi = 600)


#======================================================================================================================================
##FeNO_X_ICS DOSE
Data_Oracle$ICS_DOSE_CLASS
colnames(Data_Oracle)
variable_1<-"FeNO_baseline_ppb_imputated"
variable_2<-"ICS_DOSE_CLASS"
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Age_imputated+Gender_imputated+BMI_per_5_imputated+ACQ_baseline_score_mean_imputated+Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))
nrow(prediction_combine_rcs)


colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle, .imp == i),prediction_combine_rcs)
Table_prediction_rcs$ICS_DOSE_CLASS
Table_prediction_rcs$ICS_DOSE_CLASS<- factor(Table_prediction_rcs$ICS_DOSE_CLASS, 
                                             levels = c("0","Low", "Medium","High"))

Table_prediction_rcs$ICS_DOSE_CLASS
levels(Table_prediction_rcs$ICS_DOSE_CLASS) <- 
  replace(levels(Table_prediction_rcs$ICS_DOSE_CLASS), 
          levels(Table_prediction_rcs$ICS_DOSE_CLASS) == "0", "No")
summary(Table_prediction_rcs$ICS_DOSE_CLASS)
Spline_FeNO_X_ICS<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "#D55E00", alpha = 0.5) +
  xlab('FeNO (ppb)') +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), 
                        name = "ICS Dose",
                        guide = guide_legend(
                          keywidth = 2                      # Increase the width of the legend symbol
                        )) + # Use distinct line patterns
  scale_x_continuous(
    trans = 'log2',
    breaks = c(0, 10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 




##Eos_X_ICS DOSE
Data_Oracle$FeNO_Log_imputated
colnames(Data_Oracle)
variable_1<-"Blood_Eos_baseline_x10_9_cells_per_L_imputated"
variable_2<-"ICS_DOSE_CLASS"
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Age_imputated+Gender_imputated+BMI_per_5_imputated+ACQ_baseline_score_mean_imputated+Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))
nrow(prediction_combine_rcs)


colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle, .imp == i),prediction_combine_rcs)
Table_prediction_rcs$ICS_DOSE_CLASS
levels(Table_prediction_rcs$ICS_DOSE_CLASS) <- 
  replace(levels(Table_prediction_rcs$ICS_DOSE_CLASS), 
          levels(Table_prediction_rcs$ICS_DOSE_CLASS) == "0", "No")
Table_prediction_rcs$ICS_DOSE_CLASS <- relevel(Table_prediction_rcs$ICS_DOSE_CLASS, ref = "No")
Table_prediction_rcs$ICS_DOSE_CLASS <- factor(
  Table_prediction_rcs$ICS_DOSE_CLASS,
  levels = c("No", "Low", "Medium", "High")
)

Spline_BEC_X_ICS<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "#D55E00", alpha = 0.5) +
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), 
                        name = "ICS Dose",
                        guide = guide_legend(
                          keywidth = 2                      # Increase the width of the legend symbol
                        )) + # Use distinct line patterns
  scale_x_log10(breaks = c(0.1,0.15, 0.3, 0.6, 1, 1.5)) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(xlim = c(0.1, 2),ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 
Spline_BEC_X_ICS
Spline_FeNO_X_ICS
#Creation of the multipanel of T2 inflammation biomarkers according to the GINA step
plot_spline_T2_inflammation_in_ICS <- (
  Spline_FeNO_X_ICS + Spline_BEC_X_ICS
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold"), # Increase tag size
    plot.margin = margin(t = 10, r = 10, b = 10, l = 5) # Adjust margins: top, right, bottom, left
  ) # Automatically adds tags A, B, C, ...


# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_T2_inflammation_in_ICS_groups.png"
ggsave(output_path, plot_spline_T2_inflammation_in_ICS , width = 12, height = 6, dpi = 600)
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PART E: Sensitivity analysis for the main interactions 
#======================================================================================================================================
#Evaluation of the interaction between BEC and FEV1 (%) only in trials that have the value of FEV1 pre BD (all trials except Novel_START and PRACTICAL)
Data_Oracle_only_with_baseline_lung <- Data_Oracle %>%
  filter(Enrolled_Trial_name!="Novel_START" &Enrolled_Trial_name!="PRACTICAL")

##Calculation of interaction term BEC X FEV1
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated + Eosinophils_Log_imputated*FEV1_preBD_per10_Baseline_imputated+ offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
R2_mean
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_FEV1_only_trials_w_FEV1<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_FEV1_only_trials_w_FEV1_w_SD<-res_pool[nrow(res_pool),c(1,2,3,7,8,6)]
Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated

#Subgroup analysis
##Group FEV >=70%
Data_Oracle_only_with_baseline_lung_70_and_more <- Data_Oracle_only_with_baseline_lung %>%
  filter(FEV1_preBD_Baseline_by_group_imputated==">=70%")
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung_70_and_more, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
BEC_group_FEV1_70_and_more <-res_pool[11,c(1,2,7,8,6)]
FeNO_group_FEV1_70_and_more <-res_pool[10,c(1,2,7,8,6)]

##Group FEV 60-<70%
Data_Oracle_only_with_baseline_lung_60_70 <- Data_Oracle_only_with_baseline_lung %>%
  filter(FEV1_preBD_Baseline_by_group_imputated=="60-<70%")
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung_60_70, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
BEC_group_FEV1_60_70 <-res_pool[10,c(1,2,7,8,6)]
FeNO_group_FEV1_60_70 <-res_pool[9,c(1,2,7,8,6)]

##Group FEV 50-60
Data_Oracle_only_with_baseline_lung_50_60 <- Data_Oracle_only_with_baseline_lung %>%
  filter(FEV1_preBD_Baseline_by_group_imputated=="50-<60%")
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung_50_60, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
BEC_group_FEV1_50_60 <-res_pool[10,c(1,2,7,8,6)]
FeNO_group_FEV1_50_60 <-res_pool[9,c(1,2,7,8,6)]

##Group FEV <50
Data_Oracle_only_with_baseline_lung_50_and_less <- Data_Oracle_only_with_baseline_lung %>%
  filter(FEV1_preBD_Baseline_by_group_imputated=="<50%")
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated + FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_baseline_lung_50_and_less, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool 
BEC_group_FEV1_50_and_less <-res_pool[10,c(1,2,7,8,6)]
FeNO_group_FEV1_50_and_less <-res_pool[9,c(1,2,7,8,6)]


BEC_FEV1<-rbind(BEC_group_FEV1_70_and_more,BEC_group_FEV1_60_70,BEC_group_FEV1_50_60,BEC_group_FEV1_50_and_less)
FeNO_FEV1<-rbind(FeNO_group_FEV1_70_and_more,FeNO_group_FEV1_60_70,FeNO_group_FEV1_50_60,FeNO_group_FEV1_50_and_less)
BEC_FEV1
#Identifying the row to the dose of ICS and numbers of patients
rownames(BEC_FEV1)<-c(paste("FEV1 >=70%\n(N=",round(sum(Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated==">=70%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 60-<70%\n(N=",round(sum(Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated=="60-<70%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 50-<60%\n(N=",round(sum(Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated=="50-<60%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 <50%\n(N=",round(sum(Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated=="<50%", na.rm = TRUE)/10),")",sep=""))
rownames(FeNO_FEV1)<-c(paste("FEV1 >=70%\n(N=",round(sum(Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated==">=70%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 60-<70%\n(N=",round(sum(Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated=="60-<70%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 50-<60%\n(N=",round(sum(Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated=="50-<60%", na.rm = TRUE)/10),")",sep=""),
                      paste("FEV1 <50%\n(N=",round(sum(Data_Oracle_only_with_baseline_lung$FEV1_preBD_Baseline_by_group_imputated=="<50%", na.rm = TRUE)/10),")",sep=""))

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitiviy_analysis_Forrest_plot_BEC_in_FEV1_subgroups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)

forplo(as.data.frame(BEC_FEV1[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = rownames(BEC_FEV1), 
       left.align=FALSE,
       
       #Define the groups
       #groups=c(rep(1,2),rep(2,2),rep(3,2)),
       #grouplabs=c('Low dose ICS',
       #'Medium dose ICS',
       #'High dose ICS'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=5,
       arrow.right.length=8.5,
       
       #Remove the left bar
       left.bar= FALSE,
       
       #Adding the p-value
       pval=p_round(BEC_FEV1[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)

mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)
dev.off()

forplo(as.data.frame(FeNO_FEV1[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = rownames(FeNO_FEV1), 
       left.align=FALSE,
       
       #Define the groups
       #groups=c(rep(1,2),rep(2,2),rep(3,2)),
       #grouplabs=c('Low dose ICS',
       #'Medium dose ICS',
       #'High dose ICS'),
       
       #Define the limits of the X axis
       xlim= c(0.2,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=4,
       arrow.right.length=6,
       
       #Remove the left bar
       left.bar= FALSE,
       
       #Adding the p-value
       pval=p_round(FeNO_FEV1[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=11,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)
mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)

##Spline curve of Eosinophils_X_FEV1
variable_1<-"Blood_Eos_baseline_x10_9_cells_per_L_imputated"
variable_2<-"FEV1_preBD_Baseline_by_group_imputated"
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Age_imputated+Gender_imputated+BMI_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+Any_severe_attack_previous_12m_0no_1yes_imputated+FeNO_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle_only_with_baseline_lung, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))

colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle_only_with_baseline_lung, .imp == i),prediction_combine_rcs)

Spline_BEC_X_FEV1_sensitivity<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkblue", alpha = 0.5) +
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), 
                        name = "FEV1 \n(% of predicted)" ,
                        guide = guide_legend(
                          keywidth = 2                      # Increase the width of the legend symbol
                        )) + # Use distinct line patterns
  scale_x_log10(breaks = c(0.1, 0.3, 0.6, 1, 1.5)) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2,2.5)) +  # Custom y-axis ticks
  coord_cartesian(xlim = c(0.1, 2),ylim = c(0.4, 3)) +  # Restrict visible range on y-axis
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  )
Spline_BEC_X_FEV1_sensitivity


##Spline curve of FeNO_X_FEV1
variable_1<-"FeNO_baseline_ppb_imputated"
variable_2<-"FEV1_preBD_Baseline_by_group_imputated"
Data_Oracle_only_with_baseline_lung$Eosinophils_Log_imputated
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Age_imputated+Gender_imputated+BMI_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+Any_severe_attack_previous_12m_0no_1yes_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle_only_with_baseline_lung, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))

colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle_only_with_baseline_lung, .imp == i),prediction_combine_rcs)

Spline_FeNO_X_FEV1_sensitivity<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, linetype = .data[[variable_2]])) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkblue", alpha = 0.5) +
  xlab('FeNO (ppb)') +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), 
                        name = "FEV1 \n(% of predicted)" ,
                        guide = guide_legend(
                          keywidth = 2                      # Increase the width of the legend symbol
                        )) +
  scale_x_continuous(
    trans = 'log2',
    breaks = c(0, 10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2,2.5)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(ylim = c(0.4, 3)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  )

dev.off()

#Creation of the multipanel of T2 inflammation biomarkers according to FEV1
plot_spline_T2_inflammation_in_FEV1_sensitivity <- (
  Spline_BEC_X_FEV1_sensitivity + Spline_FeNO_X_FEV1_sensitivity
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold"), # Increase tag size
    plot.margin = margin(t = 10, r = 10, b = 10, l = 5) # Adjust margins: top, right, bottom, left
    
  )
plot_spline_T2_inflammation_in_FEV1_sensitivity
# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitivity_analysis_Spline_T2_inflammation_in_FEV1_groups.png"
ggsave(output_path, plot_spline_T2_inflammation_in_FEV1_sensitivity, width = 12, height = 6, dpi = 600)




## Interaction BEC X FEV1 according of the trial
# Initialize a list to store results for each trial
results_by_trial <- list()
# Loop through each trial in Enrolled_Trial_name
for (trial in unique(Data_Oracle_only_with_baseline_lung$Enrolled_Trial_name)) {
  # Filter the data for the current trial
  trial_data <- subset(Data_Oracle_only_with_baseline_lung, Enrolled_Trial_name == trial)
  # Check if Treatment_step_1and2 and Any_severe_attack_previous_12m_0no_1yes have only one unique value
  include_treatment_step <- length(unique(trial_data$Treatment_step_1and2)) > 1
  include_any_attack <- length(unique(trial_data$Any_severe_attack_previous_12m_0no_1yes_imputated)) > 1
  # Initialize objects for results storage
  res_comb <- list()
  R_2 <- c()
  # Perform the analysis for each imputation
  for (i in 1:10) {
    # Start building the formula dynamically
    base_formula <- "Number_severe_asthma_attacks_during_followup ~ Age_per_10_imputated + Gender_imputated + BMI_per_5_imputated + ACQ_baseline_score_mean_imputated + FeNO_Log_imputated + Eosinophils_Log_imputated * FEV1_preBD_per10_Baseline_imputated + offset(log(Follow_up_duration_days))"
    # Add Treatment_step_1and2 if it has more than one unique level
    if (include_treatment_step) {
      base_formula <- paste(base_formula, "+ Treatment_step_1and2")
    }
    # Add Any_severe_attack_previous_12m_0no_1yes if it has more than one unique level
    if (include_any_attack) {
      base_formula <- paste(base_formula, "+ Any_severe_attack_previous_12m_0no_1yes_imputated")
    }
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(as.formula(base_formula), data = subset(trial_data, .imp == i))
    # Calculate R-squared for the model
    R_2 <- c(R_2, with(summary(res_comb[[i]]), 1 - deviance / null.deviance))
  }
  # Calculate mean R-squared
  R2_mean <- mean(R_2)
  # Pool results
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  # Extract relevant results (adjust based on your specific needs)
  res_pool
  Results_multivariate_analysis <- res_pool[nrow(res_pool), c(1, 2,3, 7, 8, 6)]
  Results_multivariate_analysis
  # Store results for the current trial
  results_by_trial[[trial]] <- list(
    R2_mean = R2_mean,
    Multivariate_Results = Results_multivariate_analysis
  )
}
results_by_trial
# Creation of a dataframe to do the compilation of the data
results_df <- data.frame()
# Loop through the trials in results_by_trial
for (trial in names(results_by_trial)) {
  # Extract the multivariate results for the current trial
  trial_results <- results_by_trial[[trial]]$Multivariate_Results
  # Add the term and trial information
  trial_results <- cbind(trial = trial, term = rownames(trial_results), trial_results)
  # Select and rename columns
  trial_results <- trial_results[, c("trial", "term", "estimate","std.error", "2.5 %", "97.5 %", "p.value")]
  # Append the results to the data frame
  results_df <- rbind(results_df, trial_results)
}

results_df

Data_Oracle_only_with_baseline_lung$Enrolled_Trial_name<-as.factor(Data_Oracle_only_with_baseline_lung$Enrolled_Trial_name)
Subject_per_trial<-summary(Data_Oracle_only_with_baseline_lung$Enrolled_Trial_name)/10


Interaction_BEC_X_FEV1_only_trials_w_FEV1_w_SD <- Interaction_BEC_X_FEV1_only_trials_w_FEV1_w_SD %>%
  dplyr::rename(trial = term) %>%
  mutate(trial = "Pooled_effect")
Interaction_BEC_X_FEV1_only_trials_w_FEV1_w_SD
results_df_with_all<-rbind(results_df[,c("trial","estimate",'std.error',"2.5 %","97.5 %","p.value")],Interaction_BEC_X_FEV1_only_trials_w_FEV1_w_SD)
results_df_with_all
results_df_without_PACT <- results_df_with_all[results_df_with_all$trial != "PACT", ]

results_df_without_PACT
# View the updated dataframe
print(results_df_without_PACT)

results_df_without_PACT[,"trial"]

Subject_per_trial <- c(
  AZISAST = 54, BENRAP2B = 211, CAPTAIN = 1218, COSTA = 145, DREAM = 155, 
  DRI12544 = 155, EXTRA = 420, LAVOLTA_1 = 362, LAVOLTA_2 = 354, 
  LUSTER_1 = 298, LUSTER_2 = 287, LUTE = 66, MILLY = 112, NAVIGATOR = 517, 
  PATHWAY = 133, QUEST = 634, STRATOS_1 = 388, STRATOS_2 = 413, VERSE = 50,Pooled_effect=5988
)

# Add a new column to results_df_without_PACT
results_df_without_PACT$trial_with_subjects <- paste0(
  results_df_without_PACT$trial, " (N=", Subject_per_trial[results_df_without_PACT$trial], ")"
)


# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitivity_analysis_Forest_plot_interaction_BEC_X_FEV1_per_trial.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 9, height = 6, units = "in", res = 300)

# In forrest plot -> View the results data frame
forplo(as.data.frame(results_df_without_PACT[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = results_df_without_PACT$trial_with_subjects, 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,nrow(results_df_without_PACT)-1),2),
       grouplabs=c('Trial name',"Overall"),
       
       #Define the limits of the X axis
       xlim= c(0.1,10),
       
       #Define the arrows
       favorlabs=c('Negative Interaction','Positive Interaction'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=9,
       arrow.right.length=9,
       
       #Remove the left bar
       left.bar= FALSE,
       
       
       #Adding the p-value
       pval=p_round(results_df_without_PACT[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=10,
       margin.right=8,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
       diamond=nrow(results_df_without_PACT),
)
# Fit a random-effects meta-analysis model

meta_interaction <- rma(
  yi = results_df_without_PACT[1:nrow(results_df_without_PACT)-1,"estimate"], # Effect sizes (interaction coefficients)
  sei = results_df_without_PACT[1:nrow(results_df_without_PACT)-1,"std.error"],      # Standard errors
  method = "REML"               # Random-effects model
)

# Extract heterogeneity statistics
I2 <- meta_interaction$I2          # I² statistic
I2

text(
  x = 0.045,  # Adjust x based on your plot's x-axis range
  y = 1,  # Adjust y to a position just below the last row
  labels = paste("I² =", round(I2 / 100, 2), sep = ""),
  cex = 1,  # Font size
  font = 1,   # Bold font
  col = "black"  # Text color
)
dev.off()
#======================================================================================================================================
## Interaction FeNO X asthma attack history only in trials that asthma attack history have value of yes and no
Studies_included<- c("CAPTAIN","EXTRA","LAVOLTA_1","LAVOLTA_2","LUTE","VERSE","MILLY","Novel_START","PACT","PRACTICAL")
Data_Oracle_only_with_asthma_history_yes_or_no  <- Data_Oracle %>%
  filter(Enrolled_Trial_name %in% Studies_included)

nrow(Data_Oracle_only_with_asthma_history_yes_or_no )
##Interaction term FeNO X asthma attack history 
res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_per_10_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2 + FeNO_Log_imputated*Any_severe_attack_previous_12m_0no_1yes_imputated + Eosinophils_Log_imputated+FEV1_preBD_per10_Baseline_imputated+ offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle_only_with_asthma_history_yes_or_no, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
R2_mean
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
Results_multivariate_analysis<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_FeNO_X_Attack_only_trials_w_Attack<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_FeNO_X_Attack_only_trials_w_Attack
##Evaluation of the interaction according of the trial
# Initialize a list to store results for each trial
results_by_trial <- list()
# Loop through each trial in Enrolled_Trial_name
for (trial in unique(Data_Oracle_only_with_asthma_history_yes_or_no$Enrolled_Trial_name)) {
  # Filter the data for the current trial
  trial_data <- subset(Data_Oracle_only_with_asthma_history_yes_or_no, Enrolled_Trial_name == trial)
  # Check if Treatment_step_1and2 and Any_severe_attack_previous_12m_0no_1yes have only one unique value
  include_treatment_step <- length(unique(trial_data$Treatment_step_1and2)) > 1
  include_any_attack <- length(unique(trial_data$Any_severe_attack_previous_12m_0no_1yes_imputated)) > 1
  # Initialize objects for results storage
  res_comb <- list()
  R_2 <- c()
  # Perform the analysis for each imputation
  for (i in 1:10) {
    # Start building the formula dynamically
    base_formula <- "Number_severe_asthma_attacks_during_followup ~ Age_per_10_imputated + Gender_imputated + BMI_per_5_imputated + ACQ_baseline_score_mean_imputated + FeNO_Log_imputated*Any_severe_attack_previous_12m_0no_1yes_imputated + Eosinophils_Log_imputated +FEV1_preBD_per10_Baseline_imputated + offset(log(Follow_up_duration_days))"
    # Add Treatment_step_1and2 if it has more than one unique level
    if (include_treatment_step) {
      base_formula <- paste(base_formula, "+ Treatment_step_1and2")
    }
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(as.formula(base_formula), data = subset(trial_data, .imp == i))
    
    # Calculate R-squared for the model
    R_2 <- c(R_2, with(summary(res_comb[[i]]), 1 - deviance / null.deviance))
  }
  # Calculate mean R-squared
  R2_mean <- mean(R_2)
  res_pool
  # Pool results
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  # Extract relevant results (adjust based on your specific needs)
  Results_multivariate_analysis <- res_pool[nrow(res_pool), c(1, 2,3, 7, 8, 6)]
  # Store results for the current trial
  results_by_trial[[trial]] <- list(
    R2_mean = R2_mean,
    Multivariate_Results = Results_multivariate_analysis
  )
}
# Creation of a dataframe to do the compilation of the data
results_df <- data.frame()
# Loop through the trials in results_by_trial
for (trial in names(results_by_trial)) {
  # Extract the multivariate results for the current trial
  trial_results <- results_by_trial[[trial]]$Multivariate_Results
  # Add the term and trial information
  trial_results <- cbind(trial = trial, term = rownames(trial_results), trial_results)
  trial_results
  # Select and rename columns
  trial_results <- trial_results[, c("trial", "term", "estimate","std.error", "2.5 %", "97.5 %", "p.value")]
  # Append the results to the data frame
  results_df <- rbind(results_df, trial_results)
}
results_df

Interaction_FeNO_X_Attack_only_trials_w_Attack


Interaction_FeNO_X_Attack_only_trials_w_Attack <- Interaction_FeNO_X_Attack_only_trials_w_Attack %>%
  dplyr::rename(trial = term) %>%
  mutate(trial = "Pooled_effect")


results_df[,c("trial","estimate","2.5 %","97.5 %","p.value")]

results_df_with_all<-rbind(results_df[,c("trial","estimate","2.5 %","97.5 %","p.value")],Interaction_FeNO_X_Attack_only_trials_w_Attack)


Subject_per_trial <- c(
CAPTAIN = 1218, EXTRA = 420, LAVOLTA_1 = 362, LAVOLTA_2 = 354, 
   LUTE = 66, MILLY = 112, Novel_START = 218, 
  QUEST = 634, PACT=16,PRACTICAL = 307,VERSE = 50, Pooled_effect=3123
)

# Add a new column to results_df_without_PACT
results_df_with_all$trial_with_subjects <- paste0(
  results_df_with_all$trial, " (N=", Subject_per_trial[results_df_with_all$trial], ")"
)
results_df_with_all$trial_with_subjects
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitivity_analysis_Forest_plot_interaction_FeNO_X_Attack_per_trial.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 9, height = 6, units = "in", res = 300)


# In forrest plot -> View the results data frame
forplo(as.data.frame(results_df_with_all[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = results_df_with_all$trial_with_subjects, 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,nrow(results_df_with_all)-1),2),
       grouplabs=c('Trial name',"Overall"),
       
       #Define the limits of the X axis
       xlim= c(0.01,100),
       
       #Define the arrows
       favorlabs=c('Negative Interaction','Positive Interaction'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=9,
       arrow.right.length=9,
       
       #Remove the left bar
       left.bar= FALSE,
       
       
       #Adding the p-value
       pval=p_round(results_df_with_all[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=10,
       margin.right=8,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
       diamond=nrow(results_df_with_all),
)








results_df
# Fit a random-effects meta-analysis model
meta_interaction <- rma(
  yi = results_df$estimate, # Effect sizes (interaction coefficients)
  sei = results_df$std.error,      # Standard errors
  method = "REML"               # Random-effects model
)
meta_interaction
# Extract heterogeneity statistics
I2 <- meta_interaction$I2          # I² statistic


text(
  x = 0.002,  # Adjust x based on your plot's x-axis range
  y = 1,  # Adjust y to a position just below the last row
  labels = paste("I² =", round(I2 / 100, 2), sep = ""),
  cex = 1,  # Font size
  font = 1,   # Bold font
  col = "black"  # Text color
)
dev.off()

##Spline curve FeNO_X_attack history
variable_1<-"FeNO_baseline_ppb_imputated"
variable_2<-"Any_severe_attack_previous_12m_0no_1yes_imputated"
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Age_imputated+Gender_imputated+BMI_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle_only_with_asthma_history_yes_or_no, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))

colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle_only_with_asthma_history_yes_or_no, .imp == i),prediction_combine_rcs)
Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated
Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated, 
                                                                       levels = c("1", "0"))

Spline_FeNO_X_Attack_sensitiviy<-ggplot(data = Table_prediction_rcs, aes(x = FeNO_baseline_ppb_imputated, y = Prediction, linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkred", alpha = 0.5) +
  xlab("FeNO (ppb)") +
  ylab("Estimated Annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes", "No"),
    values = c("solid", "dashed"),  # Specify distinct line patterns
    name = "Any attack\npast 12m",
    guide = guide_legend(
      keywidth = 2                      # Increase the width of the legend symbol
    )
  ) +
  scale_x_continuous(
    trans = 'log2',
    breaks = c(0, 10, 25, 50, 100, 200)
  ) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  )



##Spline curve Eos_X_attack history
variable_1<-"Blood_Eos_baseline_x10_9_cells_per_L_imputated"
variable_2<-"Any_severe_attack_previous_12m_0no_1yes_imputated"
Data_Oracle_only_with_asthma_history_yes_or_no$FeNO_Log_imputated
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle_only_with_asthma_history_yes_or_no, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))

colnames(prediction_combine_rcs)<-"Prediction"
Table_prediction_rcs<-cbind(subset(Data_Oracle_only_with_asthma_history_yes_or_no, .imp == i),prediction_combine_rcs)
Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated
Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated <- factor(Table_prediction_rcs$Any_severe_attack_previous_12m_0no_1yes_imputated, 
                                                                                 levels = c("1", "0"))


Spline_BEC_X_Attack_sensitiviy<-ggplot(data = Table_prediction_rcs, aes(x = Blood_Eos_baseline_x10_9_cells_per_L_imputated, y = Prediction, linetype = Any_severe_attack_previous_12m_0no_1yes_imputated)) +
  geom_smooth(aes(linetype = .data[[variable_2]]), 
              method = "gam", se = TRUE, 
              fill = "grey70", color = "darkred", alpha = 0.5) +
  xlab("BEC (x10^9 cells/L)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_linetype_manual(
    labels = c("Yes", "No"),
    values = c("solid", "dashed"),  # Specify distinct line patterns
    name = "Any attack\npast 12m",
    guide = guide_legend(
      keywidth = 2                      # Increase the width of the legend symbol
    )
  ) +
  scale_x_log10(breaks = c(0.1,0.15, 0.3, 0.6, 1, 1.5)) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  theme_minimal(base_size = 10) +  # Minimal theme with clean white background
  coord_cartesian(xlim = c(0.1, 2),ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) 

Spline_BEC_X_Attack_sensitiviy

#Creation of the multipanel of T2 inflammation biomarkers according to asthma attack history 
plot_spline_T2_inflammation_in_Attack_sensitivity <- (
  Spline_FeNO_X_Attack_sensitiviy + Spline_BEC_X_Attack_sensitiviy
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold"), # Increase tag size
    plot.margin = margin(t = 10, r = 10, b = 10, l = 5) # Adjust margins: top, right, bottom, left
  
  ) # Automatically adds tags A, B, C, ...

# Print the combined plot
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitivity_analysis_Spline_T2_inflammation_in_Attack_groups.png"
ggsave(output_path, plot_spline_T2_inflammation_in_Attack_sensitivity, width = 12, height = 6, dpi = 600)

#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#Make a subgroup analysis according to ICS dose
#Subgroup 1 = ICS No and no asthma attack history 
Data_Oracle_ICS_no_Attack_no<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="0" &
         Any_severe_attack_previous_12m_0no_1yes_imputated=="0")

#All the patients are in the Novel START trial (so the enrolled trial was removed as factor)
#res_comb = NULL 
#i<-1
#for(i in 1:10){
  #res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_no_Attack_no, .imp == i))
#} 

#res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
#res_pool 
#FeNO_group_ICS_no_Attack_no <-res_pool[7,c(1,2,7,8,6)]
#BEC_group_ICS_no_Attack_no <-res_pool[8,c(1,2,7,8,6)]


#Subgroup 2 = ICS No and attack asthma history
#Data_Oracle_ICS_no_Attack_yes<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  #filter(ICS_DOSE_CLASS=="0" &
           #Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
#Data_Oracle_ICS_no_Attack_yes

#All the patients are in the Novel START trial (so the enrolled trial was removed as factor)
#res_comb = NULL 
#i<-1
#for(i in 1:10){
  #res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_no_Attack_yes, .imp == i))
#} 

#res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
#res_pool 
#FeNO_group_ICS_no_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
#BEC_group_ICS_no_Attack_yes <-res_pool[8,c(1,2,7,8,6)]


#Subgroup 3 = ICS low and no asthma attack history 
Data_Oracle_ICS_low_Attack_no<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="Low" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_low_Attack_no$Enrolled_Trial_name)
#Patient are coming from these trials "PRACTICAL" "PACT"      "MILLY"     "VERSE"     "CAPTAIN" 
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_low_Attack_no <-res_pool[7,c(1,2,7,8,6)]
BEC_group_ICS_low_Attack_no <-res_pool[8,c(1,2,7,8,6)]

#Subgroup 4 = ICS low and asthma attack history 
Data_Oracle_ICS_low_Attack_yes<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="Low" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_low_Attack_yes$Enrolled_Trial_name)

#Patient are coming from these trials "PRACTICAL" "PACT"      "MILLY"     "VERSE"     "CAPTAIN" 
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_low_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
BEC_group_ICS_low_Attack_yes <-res_pool[8,c(1,2,7,8,6)]

#Subgroup 5 = ICS medium and no asthma attack history 
Data_Oracle_ICS_medium_Attack_no<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="Medium" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_medium_Attack_no$Enrolled_Trial_name)
#Patient are coming from these trials "EXTRA" "MILLY" "LUTE"  "VERSE" 
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_medium_Attack_no <-res_pool[7,c(1,2,7,8,6)]
BEC_group_ICS_medium_Attack_no <-res_pool[8,c(1,2,7,8,6)]

#Subgroup 6 = ICS medium and asthma attack history 
Data_Oracle_ICS_medium_Attack_yes<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="Medium" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_medium_Attack_yes$Enrolled_Trial_name)
#Patient are coming from these trials "EXTRA"     "MILLY"     "LUTE"      "VERSE"     "STRATOS_1" "STRATOS_2"
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_medium_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
BEC_group_ICS_medium_Attack_yes <-res_pool[8,c(1,2,7,8,6)]
FeNO_group_ICS_medium_Attack_yes
BEC_group_ICS_medium_Attack_yes

#Subgroup 7 = ICS high and no asthma attack history 
Data_Oracle_ICS_high_Attack_no<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="High" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="0")
unique(Data_Oracle_ICS_high_Attack_no$Enrolled_Trial_name)
#Patient are coming from these trials "EXTRA"     "LAVOLTA_1" "LAVOLTA_2" "MILLY"     "LUTE"      "VERSE" 
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high_Attack_no, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_high_Attack_no <-res_pool[7,c(1,2,7,8,6)]
BEC_group_ICS_high_Attack_no <-res_pool[8,c(1,2,7,8,6)]
FeNO_group_ICS_high_Attack_no 
BEC_group_ICS_high_Attack_no
#Subgroup 8 = ICS high and asthma attack history 
Data_Oracle_ICS_high_Attack_yes<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="High" &
           Any_severe_attack_previous_12m_0no_1yes_imputated=="1")
unique(Data_Oracle_ICS_high_Attack_yes$Enrolled_Trial_name)
#Patient are coming from these trials "EXTRA"     "LAVOLTA_1" "LAVOLTA_2" "MILLY"     "LUTE"      "VERSE"     "STRATOS_1" "STRATOS_2"
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +FeNO_Log_imputated+Eosinophils_Log_imputated+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high_Attack_yes, .imp == i))
} 

res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
FeNO_group_ICS_high_Attack_yes <-res_pool[7,c(1,2,7,8,6)]
BEC_group_ICS_high_Attack_yes <-res_pool[8,c(1,2,7,8,6)]
FeNO_group_ICS_high_Attack_yes
BEC_group_ICS_high_Attack_yes


BEC_ICS_Attack<-rbind(
  BEC_group_ICS_low_Attack_no,
  BEC_group_ICS_low_Attack_yes,
  BEC_group_ICS_medium_Attack_no,
  BEC_group_ICS_medium_Attack_yes,
  BEC_group_ICS_high_Attack_no,
  BEC_group_ICS_high_Attack_yes
)

FeNO_ICS_Attack<-rbind(
  FeNO_group_ICS_low_Attack_no,
  FeNO_group_ICS_low_Attack_yes,
  FeNO_group_ICS_medium_Attack_no,
  FeNO_group_ICS_medium_Attack_yes,
  FeNO_group_ICS_high_Attack_no,
  FeNO_group_ICS_high_Attack_yes
)


#Identifying the row to the dose of ICS and numbers of patients
rownames(BEC_ICS_Attack)<-c(paste("No (N=",nrow(Data_Oracle_ICS_low_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_low_Attack_yes)/10,")",sep=""),
                            paste("No (N=",nrow(Data_Oracle_ICS_medium_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_medium_Attack_yes)/10,")",sep=""),
                            paste("No (N=",nrow(Data_Oracle_ICS_high_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_high_Attack_yes)/10,")",sep="")
                            )
rownames(FeNO_ICS_Attack)<-c(paste("No (N=",nrow(Data_Oracle_ICS_low_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_low_Attack_yes)/10,")",sep=""),
                            paste("No (N=",nrow(Data_Oracle_ICS_medium_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_medium_Attack_yes)/10,")",sep=""),
                            paste("No (N=",nrow(Data_Oracle_ICS_high_Attack_no)/10,")",sep=""),
                            paste("Yes (N=",nrow(Data_Oracle_ICS_high_Attack_yes)/10,")",sep="")
)

forplo(as.data.frame(BEC_ICS_Attack[,c("estimate","2.5 %","97.5 %")]),
       row.labels = rownames(BEC_ICS_Attack), 
       em="aRR",
       left.align=FALSE,
       xlim= c(0.2,10),
       shade.every=1,
       shade.col='gray',
       left.bar= FALSE,
       margin.left=10,
       margin.right=10,
       add.columns=p_round(BEC_ICS_Attack[,"p.value"], digits = 2),
       add.colnames=c('p-value'),
       groups=c(rep(1,2),rep(2,2),rep(3,2)),
       grouplabs=c('Low dose ICS',
                   'Medium dose ICS',
                   'High dose ICS')
       
)

forplo(as.data.frame(FeNO_ICS_Attack[,c("estimate","2.5 %","97.5 %")]),
       row.labels = rownames(FeNO_ICS_Attack),
       em="aRR",
       left.align=FALSE,
       xlim= c(0.2,10),
       shade.every=1,
       shade.col='gray',
       left.bar= FALSE,
       margin.left=10,
       margin.right=10,
       add.columns=p_round(FeNO_ICS_Attack[,"p.value"], digits = 2),
       add.colnames=c('p-value'),
       groups=c(rep(1,2),rep(2,2),rep(3,2)),
       grouplabs=c('Low dose ICS',
                   'Medium dose ICS',
                   'High dose ICS')
       
)



#Evaluation of the interaction between FeNO and attack history term according to the ICS Dose
## In patient low ICS dose
Data_Oracle_ICS_no<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="0")
nrow(Data_Oracle_ICS_no)
Data_Oracle_ICS_low<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="Low")
nrow(Data_Oracle_ICS_low)/10

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_FeNO_X_Attack_in_low_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_FeNO_X_Attack_in_low_ICS

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated+Eosinophils_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_low, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_low_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_Attack_in_low_ICS

## In patient medium ICS dose
Data_Oracle_ICS_medium<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="Medium")
Data_Oracle_ICS_medium$Any_attack_or_hospitalization_previous_12_months
nrow(Data_Oracle_ICS_medium)/10
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_FeNO_X_Attack_in_medium_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_FeNO_X_Attack_in_medium_ICS

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated+Eosinophils_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_medium, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_medium_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_Attack_in_medium_ICS

## In patient high ICS dose
Data_Oracle_ICS_high<-Data_Oracle_only_with_asthma_history_yes_or_no %>% 
  filter(ICS_DOSE_CLASS=="High")
Data_Oracle_ICS_high$Any_attack_or_hospitalization_previous_12_months
nrow(Data_Oracle_ICS_high)
res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_FeNO_X_Attack_in_high_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_FeNO_X_Attack_in_high_ICS

res_comb = NULL 
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated +Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated+FEV1_preBD_per10_Baseline_imputated  +Eosinophils_Log_imputated+FeNO_Log_imputated+Eosinophils_Log_imputated*Any_attack_or_hospitalization_previous_12_months+as.factor(Enrolled_Trial_name) + offset(log(Follow_up_duration_days)), data = subset(Data_Oracle_ICS_high, .imp == i))
} 
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_Attack_in_high_ICS<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_BEC_X_Attack_in_high_ICS


Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack<-rbind(
  BEC_group_ICS_low_Attack_no,
  BEC_group_ICS_low_Attack_yes,
  Interaction_BEC_X_Attack_in_low_ICS,
  BEC_group_ICS_medium_Attack_no,
  BEC_group_ICS_medium_Attack_yes,
  Interaction_BEC_X_Attack_in_medium_ICS,
  BEC_group_ICS_high_Attack_no,
  BEC_group_ICS_high_Attack_yes,
  Interaction_BEC_X_Attack_in_high_ICS,
  FeNO_group_ICS_low_Attack_no,
  FeNO_group_ICS_low_Attack_yes,
  Interaction_FeNO_X_Attack_in_low_ICS,
  FeNO_group_ICS_medium_Attack_no,
  FeNO_group_ICS_medium_Attack_yes,
  Interaction_FeNO_X_Attack_in_medium_ICS,
  FeNO_group_ICS_high_Attack_no,
  FeNO_group_ICS_high_Attack_yes,
  Interaction_FeNO_X_Attack_in_high_ICS
)
ics_dose_values <- c(
  "Low", "Low", "Low",
  "Medium", "Medium", "Medium",
  "High", "High", "High",
  "Low", "Low", "Low",
  "Medium", "Medium", "Medium",
  "High", "High", "High"
)
Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack$`ICS dose` <- ics_dose_values
Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack <- Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack[, 
                                                                                     c("ICS dose", setdiff(names(Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack), "ICS dose"))
]

Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack$estimate_range <- with(
  Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack,
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2), "-", round(`97.5 %`, 2), ")"  # Add confidence interval
  )
)
Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack
colnames(Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack)

write_xlsx(Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack,"Table_T2_biomarkers_X_attack_in_ICSgroup_only_trials_with_attack.xlsx")
#======================================================================================================================================
#======================================================================================================================================
#Impact of adding interaction on the performance of the model

### Multivariate (no interaction)) for asthma attack
Data_Oracle$Age_imputated
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+ offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
} 
R2_no_interaction<-mean(R_2)
AIC_no_interaction <- mean(AIC_values)
AICc_no_interaction <- mean(AICc_values)
BIC_no_interaction <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
Results_multivariate_analysis<-res_pool[2:12,c(1,2,7,8,6)]

Interaction_BEC_X_FEV1_no_interaction <-NA
Interaction_FeNO_X_GINA_no_interaction <- NA
Interaction_FeNO_X_Attack_no_interaction <- NA


### + FEV1*BEC
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
lrt_results <- list()  # Store LRT results for each imputation

i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2+Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated*Eosinophils_Log_imputated+FeNO_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
  
} 
R2_w_BEC_X_FEV1<-mean(R_2)
AIC_w_BEC_X_FEV1 <- mean(AIC_values)
AICc_w_BEC_X_FEV1 <- mean(AICc_values)
BIC_w_BEC_X_FEV1  <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_BEC_X_FEV1_w_BEC_X_FEV1 <- with(
  res_pool[nrow(res_pool), c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)
Interaction_FeNO_X_GINA_w_BEC_X_FEV1 <- NA
Interaction_FeNO_X_Attack_w_BEC_X_FEV1 <- NA

Data_Oracle$Treatment_step_1and2
### + FENO*GINA
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  +Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+FeNO_Log_imputated*Treatment_step_1and2 + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
} 
R2_w_FeNO_X_GINA<-mean(R_2)
AIC_w_FeNO_X_GINA <- mean(AIC_values)
AICc_w_FeNO_X_GINA <- mean(AICc_values)
BIC_w_FeNO_X_GINA  <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Interaction_BEC_X_FEV1_w_FeNO_X_GINA <- NA
Interaction_FeNO_X_GINA_w_FeNO_X_GINA <- with(
  res_pool[nrow(res_pool)-2, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)
Interaction_FeNO_X_Attack_w_FeNO_X_GINA <- NA



### + FENO*Attack
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated   +FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+FeNO_Log_imputated*Any_severe_attack_previous_12m_0no_1yes_imputated+Treatment_step_1and2 + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
} 
R2_w_FeNO_X_Attack<-mean(R_2)
AIC_w_FeNO_X_Attack <- mean(AIC_values)
AICc_w_FeNO_X_Attack <- mean(AICc_values)
BIC_w_FeNO_X_Attack  <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)

Interaction_BEC_X_FEV1_w_FeNO_X_Attack <- NA
Interaction_FeNO_X_GINA_w_FeNO_X_Attack <- NA
Interaction_FeNO_X_Attack_w_FeNO_X_Attack <- with(
  res_pool[nrow(res_pool), c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)





### + FEV1*BEC + FeNO*GINA
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  +Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated*Eosinophils_Log_imputated+FeNO_Log_imputated*Treatment_step_1and2 + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
} 
R2_w_BEC_X_FEV1_and_FeNO_X_GINA<-mean(R_2)
AIC_w_BEC_X_FEV1_and_FeNO_X_GINA <- mean(AIC_values)
AICc_w_BEC_X_FEV1_and_FeNO_X_GINA <- mean(AICc_values)
BIC_w_BEC_X_FEV1_and_FeNO_X_GINA  <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool

Interaction_BEC_X_FEV1_w_BEC_X_FEV1_and_FeNO_X_GINA<- with(
  res_pool[nrow(res_pool)-3, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)

Interaction_FeNO_X_GINA_w_BEC_X_FEV1_and_FeNO_X_GINA<- with(
  res_pool[nrow(res_pool)-2, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)
Interaction_FeNO_X_Attack_w_BEC_X_FEV1_and_FeNO_X_GINA<-NA



### + FEV1*BEC + FeNO*Attack
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated +FEV1_preBD_per10_Baseline_imputated*Eosinophils_Log_imputated+FeNO_Log_imputated*Any_severe_attack_previous_12m_0no_1yes_imputated+Treatment_step_1and2 + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
} 
R2_w_BEC_X_FEV1_and_FeNO_X_Attack<-mean(R_2)
AIC_w_BEC_X_FEV1_and_FeNO_X_Attack <- mean(AIC_values)
AICc_w_BEC_X_FEV1_and_FeNO_X_Attack <- mean(AICc_values)
BIC_w_BEC_X_FEV1_and_FeNO_X_Attack  <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool


Interaction_BEC_X_FEV1_w_BEC_X_FEV1_and_FeNO_X_Attack<- with(
  res_pool[nrow(res_pool)-1, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)


Interaction_FeNO_X_GINA_w_BEC_X_FEV1_and_FeNO_X_Attack<-NA

Interaction_FeNO_X_Attack_w_BEC_X_FEV1_and_FeNO_X_Attack<- with(
  res_pool[nrow(res_pool), c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)


### + FENO*Attack + FeNO*GINA
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated   +FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+FeNO_Log_imputated*Any_severe_attack_previous_12m_0no_1yes_imputated+FeNO_Log_imputated*Treatment_step_1and2 + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
} 
R2_w_FeNO_X_Attack_and_FeNO_X_GINA<-mean(R_2)
AIC_w_FeNO_X_Attack_and_FeNO_X_GINA <- mean(AIC_values)
AICc_w_FeNO_X_Attack_and_FeNO_X_GINA <- mean(AICc_values)
BIC_w_FeNO_X_Attack_and_FeNO_X_GINA  <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool

Interaction_BEC_X_FEV1_w_FeNO_X_Attack_and_FeNO_X_GINA<-NA

Interaction_FeNO_X_Attack_w_FeNO_X_Attack_and_FeNO_X_GINA<- with(
  res_pool[nrow(res_pool)-3, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)

Interaction_FeNO_X_GINA_w_FeNO_X_Attack_and_FeNO_X_GINA<- with(
  res_pool[nrow(res_pool)-2, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)

### + FENO*Attack*GINA
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated   +FEV1_preBD_per10_Baseline_imputated+Eosinophils_Log_imputated+FeNO_Log_imputated*Any_severe_attack_previous_12m_0no_1yes_imputated*Treatment_step_1and2 + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
} 
R2_w_FeNO_X_Attack_X_GINA<-mean(R_2)
AIC_w_FeNO_X_Attack_X_GINA <- mean(AIC_values)
AICc_w_FeNO_X_Attack_X_GINA <- mean(AICc_values)
BIC_w_FeNO_X_Attack_X_GINA  <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)


### + BEC*FEV1 + FENO*Attack+ FeNO*GINA
res_comb = NULL 
R_2<-c()
AIC_values <- c()
AICc_values <- c()
BIC_values <- c()
i<-1
for(i in 1:10){
  #This is the model that I used based on fleur results of the main variables associated to asthma attack risk.
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Age_imputated+Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated +FEV1_preBD_per10_Baseline_imputated*Eosinophils_Log_imputated+FeNO_Log_imputated*Any_severe_attack_previous_12m_0no_1yes_imputated+Treatment_step_1and2*FeNO_Log_imputated + offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  AICc_values <- c(AICc_values, AICc(res_comb[[i]]))
  BIC_values <- c(BIC_values, BIC(res_comb[[i]]))
} 
R2_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA<-mean(R_2)
AIC_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA <- mean(AIC_values)
AICc_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA <- mean(AICc_values)
BIC_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA  <- mean(BIC_values)
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool

Interaction_BEC_X_FEV1_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA<- with(
  res_pool[nrow(res_pool)-4, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)

Interaction_FeNO_X_Attack_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA<- with(
  res_pool[nrow(res_pool)-3, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)

Interaction_FeNO_X_GINA_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA<- with(
  res_pool[nrow(res_pool)-2, c(1, 2, 7, 8, 6)],
  paste0(
    round(estimate, 2),  # Round estimate to 2 decimal places
    " (", round(`2.5 %`, 2),  # Round 2.5% to 2 decimal places
    "-", round(`97.5 %`, 2),  # Round 97.5% to 2 decimal places
    ")"
  )
)


R2_no_interaction 
R2_w_BEC_X_FEV1
R2_w_FeNO_X_GINA
R2_w_FeNO_X_Attack
R2_w_BEC_X_FEV1_and_FeNO_X_GINA
R2_w_BEC_X_FEV1_and_FeNO_X_Attack
R2_w_FeNO_X_Attack_and_FeNO_X_GINA
R2_w_FeNO_X_Attack_X_GINA
R2_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
AIC_no_interaction 
AIC_w_BEC_X_FEV1
AIC_w_FeNO_X_GINA
AIC_w_FeNO_X_Attack
AIC_w_BEC_X_FEV1_and_FeNO_X_GINA
AIC_w_BEC_X_FEV1_and_FeNO_X_Attack
AIC_w_FeNO_X_Attack_and_FeNO_X_GINA
AIC_w_FeNO_X_Attack_X_GINA
AIC_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
BIC_no_interaction 
BIC_w_BEC_X_FEV1
BIC_w_FeNO_X_GINA
BIC_w_FeNO_X_Attack
BIC_w_BEC_X_FEV1_and_FeNO_X_GINA
BIC_w_BEC_X_FEV1_and_FeNO_X_Attack
BIC_w_FeNO_X_Attack_and_FeNO_X_GINA
BIC_w_FeNO_X_Attack_X_GINA
BIC_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA


# Define the interactions
interactions <- c(
  "Multivariable model",
  "+ BEC*FEV1",
  "+ FeNO*GINA",
  "+ FeNO*Attack",
  "+ BEC*FEV1 + FeNO*GINA",
  "+ BEC*FEV1 + FeNO*Attack",
  "+ FeNO*Attack + FeNO*GINA",
  #"+ FeNO*Attack*GINA",
  "+ BEC*FEV1+ FeNO*Attack+ FeNO*GINA"
)

# Define the terms exported

Interaction_BEC_X_FEV1 <- c(
  Interaction_BEC_X_FEV1_no_interaction,
  Interaction_BEC_X_FEV1_w_BEC_X_FEV1,
  Interaction_BEC_X_FEV1_w_FeNO_X_GINA,
  Interaction_BEC_X_FEV1_w_FeNO_X_Attack,
  Interaction_BEC_X_FEV1_w_BEC_X_FEV1_and_FeNO_X_GINA,
  Interaction_BEC_X_FEV1_w_BEC_X_FEV1_and_FeNO_X_Attack,
  Interaction_BEC_X_FEV1_w_FeNO_X_Attack_and_FeNO_X_GINA,
  #Interaction_BEC_X_FEV1_w_FeNO_X_Attack_X_GINA,
  Interaction_BEC_X_FEV1_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
)

Interaction_FeNO_X_Attack <- c(
  Interaction_FeNO_X_Attack_no_interaction,
  Interaction_FeNO_X_Attack_w_BEC_X_FEV1,
  Interaction_FeNO_X_Attack_w_FeNO_X_GINA,
  Interaction_FeNO_X_Attack_w_FeNO_X_Attack,
  Interaction_FeNO_X_Attack_w_BEC_X_FEV1_and_FeNO_X_GINA,
  Interaction_FeNO_X_Attack_w_BEC_X_FEV1_and_FeNO_X_Attack,
  Interaction_FeNO_X_Attack_w_FeNO_X_Attack_and_FeNO_X_GINA,
  #Interaction_FeNO_X_Attack_w_FeNO_X_Attack_X_GINA,
  Interaction_FeNO_X_Attack_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
)

Interaction_FeNO_X_GINA <- c(
  Interaction_FeNO_X_GINA_no_interaction,
  Interaction_FeNO_X_GINA_w_BEC_X_FEV1,
  Interaction_FeNO_X_GINA_w_FeNO_X_GINA,
  Interaction_FeNO_X_GINA_w_FeNO_X_Attack,
  Interaction_FeNO_X_GINA_w_BEC_X_FEV1_and_FeNO_X_GINA,
  Interaction_FeNO_X_GINA_w_BEC_X_FEV1_and_FeNO_X_Attack,
  Interaction_FeNO_X_GINA_w_FeNO_X_Attack_and_FeNO_X_GINA,
  #Interaction_FeNO_X_GINA_w_FeNO_X_Attack_X_GINA,
  Interaction_FeNO_X_GINA_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
)




R2_values <- c(
  R2_no_interaction,
  R2_w_BEC_X_FEV1,
  R2_w_FeNO_X_GINA,
  R2_w_FeNO_X_Attack,
  R2_w_BEC_X_FEV1_and_FeNO_X_GINA,
  R2_w_BEC_X_FEV1_and_FeNO_X_Attack,
  R2_w_FeNO_X_Attack_and_FeNO_X_GINA,
  #R2_w_FeNO_X_Attack_X_GINA,
  R2_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
)

AIC_values <- c(
  AIC_no_interaction,
  AIC_w_BEC_X_FEV1,
  AIC_w_FeNO_X_GINA,
  AIC_w_FeNO_X_Attack,
  AIC_w_BEC_X_FEV1_and_FeNO_X_GINA,
  AIC_w_BEC_X_FEV1_and_FeNO_X_Attack,
  AIC_w_FeNO_X_Attack_and_FeNO_X_GINA,
  #AIC_w_FeNO_X_Attack_X_GINA,
  AIC_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
)
AICc_values <- c(
  AICc_no_interaction,
  AICc_w_BEC_X_FEV1,
  AICc_w_FeNO_X_GINA,
  AICc_w_FeNO_X_Attack,
  AICc_w_BEC_X_FEV1_and_FeNO_X_GINA,
  AICc_w_BEC_X_FEV1_and_FeNO_X_Attack,
  AICc_w_FeNO_X_Attack_and_FeNO_X_GINA,
  #AICc_w_FeNO_X_Attack_X_GINA,
  AICc_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
)

BIC_values <- c(
  BIC_no_interaction,
  BIC_w_BEC_X_FEV1,
  BIC_w_FeNO_X_GINA,
  BIC_w_FeNO_X_Attack,
  BIC_w_BEC_X_FEV1_and_FeNO_X_GINA,
  BIC_w_BEC_X_FEV1_and_FeNO_X_Attack,
  BIC_w_FeNO_X_Attack_and_FeNO_X_GINA,
  #BIC_w_FeNO_X_Attack_X_GINA,
  BIC_w_BEC_X_FEV1_and_FeNO_X_Attack_and_FeNO_X_GINA
)

# Create the data frame
results_interactions_model_performance <- data.frame(
  Interaction = interactions,
  Interaction_BEC_X_FEV1=Interaction_BEC_X_FEV1,
  Interaction_FeNO_X_GINA=Interaction_FeNO_X_GINA,
  Interaction_FeNO_X_Attack=Interaction_FeNO_X_Attack,
  R2 = R2_values,
  AIC = AIC_values,
  AICc = AICc_values,
  BIC = BIC_values
)

# Print the data frame
results_interactions_model_performance


write_xlsx(results_interactions_model_performance,"Results_interactions_model_performance.xlsx")
#======================================================================================================================================
# PART G: Screening of all potentials interactions
#For the analysis on of potentals interactions=Using data_imputated_without_systematically_missing
### Mutivariate (no interaction)) for asthma attack

res_comb = NULL 
R_2<-c()
i<-1
for(i in 1:10){
  res_comb[[i]] = glm.nb(Number_severe_asthma_attacks_during_followup ~Gender_imputated+BMI_per_5_imputated +ACQ_baseline_score_mean_imputated  + Treatment_step_1and2 +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+ offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  R_2<-c(R_2,with(summary(res_comb[[i]]), 1 - deviance/null.deviance))
} 
R2_mean<-mean(R_2)
R2_mean
res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
res_pool
Results_multivariate_analysis<-res_pool[2:12,c(1,2,7,8,6)]
Results_multivariate_analysis
#======================================================================================================================================
# Interactions analysis for asthma attack risk

##Create the loop to calculate all the interactions of inflammatory biomarkers
variable_1<- c("Eosinophils_Log","FeNO_Log")
variable_2<- c("Age_per_10","Gender","BMI_per_5","Smoking_Statut","Atopy_history","Airborne_allergen_sensibilisation","CRSsNP","CRSwNP","IgE_Log")
compilation_interaction<- data.frame()
AICC_values<-c()
for (v1 in variable_1) {
  for (v2 in variable_2) {
    res_comb = NULL
    if (v2 == "Treatment_step_1and2") {
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"_imputated*",v2,"+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      } 
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[(nrow(res_pool)-1):nrow(res_pool),]
      interaction
    } else {
      valid_trials <- unique(Data_Oracle$Enrolled_Trial_name[!is.na(Data_Oracle[[v2]])])
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"_imputated*",v2,"_imputated+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i & Enrolled_Trial_name %in% valid_trials))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      }
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[nrow(res_pool),]
    }
    compilation_interaction<- rbind(compilation_interaction,interaction)
  }
}
compilation_interaction_attack_Total<-compilation_interaction[,c("term","estimate","p.value","2.5 %","97.5 %")]
colnames(compilation_interaction_attack_Total)<-c("term","estimate","p_value","PCT_2_5","PCT_97_5")
compilation_interaction_attack_Total

##Remove the columns that are not interactions terms
Table_interaction_multivariate_Inflamm_X_Baseline  <- compilation_interaction_attack_Total %>%
  filter(!grepl("as.factor\\(Enrolled_Trial_name\\)", term))
Table_interaction_multivariate_Inflamm_X_Baseline

##Adding the confidence interval in this format [X.XX;X.XX]
Table_interaction_multivariate_Inflamm_X_Baseline <- Table_interaction_multivariate_Inflamm_X_Baseline %>% 
  mutate(confidence_interval=paste(format(round(estimate, digits=2), nsmall = 2)," [",format(round(PCT_2_5, digits=2), nsmall = 2) ,";",format(round(PCT_97_5, digits=2), nsmall = 2) ,"]",sep=""))
Table_interaction_multivariate_Inflamm_X_Baseline


##Calculation of False discovery rate (FDR)
FDR_Inflamm_X_Baseline<-data.frame(p.adjust(Table_interaction_multivariate_Inflamm_X_Baseline$p_value,method= "fdr"))
colnames(FDR_Inflamm_X_Baseline)<- "FDR"
Table_interaction_multivariate_Inflamm_X_Baseline<-cbind(Table_interaction_multivariate_Inflamm_X_Baseline,FDR_Inflamm_X_Baseline)
Table_interaction_multivariate_Inflamm_X_Baseline

## Formatting p-value and FDR in a format with a number of digit adequate
Table_interaction_multivariate_Inflamm_X_Baseline <- Table_interaction_multivariate_Inflamm_X_Baseline %>% 
  mutate(p_value_format=paste("",format.pval(pv = p_value, digits = 4,nsmall = 3),"",sep="")) %>% 
  mutate(FDR_format= paste("",format.pval(pv = FDR, digits = 2,nsmall = 1),"",sep="")) 
Table_interaction_multivariate_Inflamm_X_Baseline


##Create the loop to calculate all the interactions with IgE
colnames(Data_Oracle)
Data_Oracle$Smoking_Statut
variable_1<- c("IgE_Log")
variable_2<- c("Age_per_10","Gender","BMI_per_5","Smoking_Statut","Atopy_history","Airborne_allergen_sensibilisation","CRSsNP","CRSwNP","Treatment_step_1and2","Any_severe_attack_previous_12m_0no_1yes","ACQ_baseline_score_mean","FEV1_preBD_per10_Baseline")
compilation_interaction<- data.frame()
AICC_values<-c()
for (v1 in variable_1) {
  for (v2 in variable_2) {
    res_comb = NULL
    if (v2 == "Treatment_step_1and2") {
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"_imputated*",v2,"+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      } 
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[(nrow(res_pool)-1):nrow(res_pool),]
      interaction
    } else {
      valid_trials <- unique(Data_Oracle$Enrolled_Trial_name[!is.na(Data_Oracle[[v2]])])
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"_imputated*",v2,"_imputated+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i & Enrolled_Trial_name %in% valid_trials))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      }
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[nrow(res_pool),]
    }
    compilation_interaction<- rbind(compilation_interaction,interaction)
  }
}
compilation_interaction_attack_Total<-compilation_interaction[,c("term","estimate","p.value","2.5 %","97.5 %")]
colnames(compilation_interaction_attack_Total)<-c("term","estimate","p_value","PCT_2_5","PCT_97_5")
compilation_interaction_attack_Total

##Remove the columns that are not interactions terms
Table_interaction_multivariate_IgE_X_Baseline  <- compilation_interaction_attack_Total %>%
  filter(!grepl("as.factor\\(Enrolled_Trial_name\\)", term))

##Adding the confidence interval in this format [X.XX;X.XX]
Table_interaction_multivariate_IgE_X_Baseline <- Table_interaction_multivariate_IgE_X_Baseline %>% 
  mutate(confidence_interval=paste(format(round(estimate, digits=2), nsmall = 2)," [",format(round(PCT_2_5, digits=2), nsmall = 2) ,";",format(round(PCT_97_5, digits=2), nsmall = 2) ,"]",sep=""))
Table_interaction_multivariate_IgE_X_Baseline


##Calculation of False discovery rate (FDR)
FDR_IgE_X_Baseline<-data.frame(p.adjust(Table_interaction_multivariate_IgE_X_Baseline$p_value,method= "fdr"))
colnames(FDR_IgE_X_Baseline)<- "FDR"
Table_interaction_multivariate_IgE_X_Baseline<-cbind(Table_interaction_multivariate_IgE_X_Baseline,FDR_IgE_X_Baseline)
Table_interaction_multivariate_IgE_X_Baseline

## Formatting p-value and FDR in a format with a number of digit adequate
Table_interaction_multivariate_IgE_X_Baseline <- Table_interaction_multivariate_IgE_X_Baseline %>% 
  mutate(p_value_format=paste("",format.pval(pv = p_value, digits = 4,nsmall = 3),"",sep="")) %>% 
  mutate(FDR_format= paste("",format.pval(pv = FDR, digits = 2,nsmall = 1),"",sep="")) 
Table_interaction_multivariate_IgE_X_Baseline


##Create the loop to calculate all the interactions
colnames(Data_Oracle)
Data_Oracle$Treatment_step_1and2
Data_Oracle$Smoking_Statut
variable_1<- c("Age_per_10","BMI_per_5","Smoking_Statut","Atopy_history","Airborne_allergen_sensibilisation","CRSsNP","CRSwNP","IgE_Log","Treatment_step_1and2","Any_severe_attack_previous_12m_0no_1yes","ACQ_baseline_score_mean","FEV1_preBD_per10_Baseline","Eosinophils_Log","FeNO_Log")
variable_2<- c("Age_per_10","BMI_per_5","Smoking_Statut","Atopy_history","Airborne_allergen_sensibilisation","CRSsNP","CRSwNP","IgE_Log","Treatment_step_1and2","Any_severe_attack_previous_12m_0no_1yes","ACQ_baseline_score_mean","FEV1_preBD_per10_Baseline","Eosinophils_Log","FeNO_Log")
compilation_interaction<- data.frame()
AICC_values<-c()
for (v1 in variable_1[1:(length(variable_1)-1)]) {
  for (v2 in variable_2[((which(variable_2==v1)+1):length(variable_2))]) {
    res_comb = NULL
    if (v1 == v2) {
    } else if (v1 == "Treatment_step_1and2"){
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"*",v2,"_imputated+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      } 
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[(nrow(res_pool)-1):nrow(res_pool),]
      compilation_interaction<- rbind(compilation_interaction,interaction)
    }else if (v2 == "Treatment_step_1and2"){
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"_imputated*",v2,"+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      } 
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[(nrow(res_pool)-1):nrow(res_pool),]
      compilation_interaction<- rbind(compilation_interaction,interaction)
    }else {
      valid_trials <- unique(Data_Oracle$Enrolled_Trial_name[!is.na(Data_Oracle[[v1]])&!is.na(Data_Oracle[[v2]])])
      for (i in 1:10){
        res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ ",v1,"_imputated*",v2,"_imputated+Gender_imputated+BMI_per_5_imputated +Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)",sep="")), data = subset(Data_Oracle, .imp == i & Enrolled_Trial_name %in% valid_trials))
        AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
      }
      res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
      interaction <- res_pool[nrow(res_pool),]
      compilation_interaction<- rbind(compilation_interaction,interaction)
    }
    
  }
}
compilation_interaction_attack_Total<-compilation_interaction[,c("term","estimate","p.value","2.5 %","97.5 %")]
colnames(compilation_interaction_attack_Total)<-c("term","estimate","p_value","PCT_2_5","PCT_97_5")
compilation_interaction_attack_Total$term

##Remove the columns that are not interactions terms
Table_interaction_multivariate_all  <- compilation_interaction_attack_Total %>%
  filter(!grepl("as.factor\\(Enrolled_Trial_name\\)", term))
Table_interaction_multivariate_all
Table_interaction_multivariate_all <- Table_interaction_multivariate_all %>%
  filter(term != "Treatment_step_1and2Step 1-2:Eosinophils_Log_imputated") %>% 
  filter(term != "Treatment_step_1and2Step 3:Eosinophils_Log_imputated") %>% 
  filter(term != "Treatment_step_1and2Step 5:Eosinophils_Log_imputated") %>% 
  filter(term != "Treatment_step_1and2Step 1-2:FeNO_Log_imputated") %>% 
  filter(term != "Treatment_step_1and2Step 3:FeNO_Log_imputated") %>% 
  filter(term != "Treatment_step_1and2Step 5:FeNO_Log_imputated") %>% 
  filter(term != "Any_severe_attack_previous_12m_0no_1yes_imputated:Eosinophils_Log_imputated") %>% 
  filter(term != "Any_severe_attack_previous_12m_0no_1yes_imputated:FeNO_Log_imputated") %>% 
  filter(term != "ACQ_baseline_score_mean_imputated:Eosinophils_Log_imputated") %>% 
  filter(term != "ACQ_baseline_score_mean_imputated:FeNO_Log_imputated") %>% 
  filter(term != "FEV1_preBD_per10_Baseline_imputated:Eosinophils_Log_imputated") %>% 
  filter(term != "FEV1_preBD_per10_Baseline_imputated:FeNO_Log_imputated") %>% 
  filter(term != "FeNO_Log_imputated:IgE_Log_imputated")
  

##Adding the confidence interval in this format [X.XX;X.XX]
Table_interaction_multivariate_complete<- Table_interaction_multivariate_all %>% 
  mutate(confidence_interval=paste(format(round(estimate, digits=2), nsmall = 2)," (",format(round(PCT_2_5, digits=2), nsmall = 2) ,"-",format(round(PCT_97_5, digits=2), nsmall = 2) ,")",sep=""))
Table_interaction_multivariate_complete$term

##Calculation of False discovery rate (FDR)
FDR_complete<-data.frame(p.adjust(Table_interaction_multivariate_complete$p_value,method= "fdr"))
colnames(FDR_complete)<- "FDR"
Table_interaction_multivariate_complete<-cbind(Table_interaction_multivariate_complete,FDR_complete)
Table_interaction_multivariate_complete

## Formatting p-value and FDR in a format with a number of digit adequate
Table_interaction_multivariate_complete <- Table_interaction_multivariate_complete %>% 
  mutate(p_value_format=paste("",format.pval(pv = p_value, digits = 4,nsmall = 3),"",sep="")) %>% 
  mutate(FDR_format= paste("",format.pval(pv = FDR, digits = 2,nsmall = 1),"",sep="")) 

Table_interaction_multivariate_complete

#Save the table in xlsx format
write_xlsx(Table_interaction_multivariate_complete,"Table_interaction_X_X_multivariate.xlsx")


## Removing the interaction between inflammatory biomarkers and 
terms_to_remove <- c(
  "Treatment_step_1and2Step 1-2:Eosinophils_Log_imputated",
  "Treatment_step_1and2Step 3:Eosinophils_Log_imputated",
  "Treatment_step_1and2Step 5:Eosinophils_Log_imputated",
  "Treatment_step_1and2Step 1-2:FeNO_Log_imputated",
  "Treatment_step_1and2Step 3:FeNO_Log_imputated",
  "Treatment_step_1and2Step 5:FeNO_Log_imputated",
  "Any_severe_attack_previous_12m_0no_1yes_imputated:Eosinophils_Log_imputated",
  "FEV1_preBD_per10_Baseline_imputated:Eosinophils_Log_imputated",
  "FEV1_preBD_per10_Baseline_imputated:FeNO_Log_imputated",
  "Eosinophils_Log_imputated:FeNO_Log_imputated"
)

# Remove the rows where term matches any value in terms_to_remove
Table_interaction_multivariate_complete_without_inflammation <- Table_interaction_multivariate_complete %>%
  filter(!term %in% terms_to_remove)

Table_interaction_multivariate_complete_without_inflammation
#======================================================================================================================================
#Creation of the dataframe of all interaction: 
Table_interaction_multivariate_complete_var1_var2 <- Table_interaction_multivariate_complete_without_inflammation %>%
  separate(term, into = c("Variable1", "Variable2"), sep = ":")
Table_interaction_multivariate_complete_var1_var2 
data_heatmap <-Table_interaction_multivariate_complete_var1_var2[,c("Variable1","Variable2","estimate","FDR","p_value")]
All_variables <- c(unique(data_heatmap$Variable1), unique(data_heatmap$Variable2))
for (u in unique(All_variables)){
  data_heatmap<-rbind(data_heatmap, c(u,u,NA, NA, NA))
}
data_heatmap
data_heatmap <- data_heatmap %>%
  mutate(FDR = as.numeric(FDR),
         estimate = as.numeric(estimate))
data_heatmap <- data_heatmap %>% 
  mutate(color = case_when(
    is.na(FDR) ~ -2,               # Assign -2 if FDR is NA
    FDR > 0.05 ~ -1,               # Assign -1 if FDR > 0.05
    FDR < 0.05 ~ estimate,         # Assign estimate if FDR < 0.05
    TRUE ~ NA_real_                # Assign NA_real_ for numeric NA
  ))



data_heatmap_estimate<-data_heatmap[,c("Variable1","Variable2","estimate")]
heatmap_estimate_df <- pivot_wider(data_heatmap_estimate, names_from = Variable2, values_from = estimate)
heatmap_estimate_df <-data.frame(heatmap_estimate_df)

##Adding the colnames for the variable 1
rownames(heatmap_estimate_df)<-heatmap_estimate_df$Variable1

##Remove the column called Variable1
heatmap_estimate_df <- heatmap_estimate_df[, !(names(heatmap_estimate_df) %in% "Variable1")]
heatmap_estimate_df


# Reorder columns to move "Age_per_10" as the first column
heatmap_estimate_df <- heatmap_estimate_df[, c("Age_per_10_imputated", setdiff(names(heatmap_estimate_df), "Age_per_10_imputated"))]

#Determine the colors of estimate with 
heatmap_estimate_df

#Creation of upper triangle p-value and FDR
data_heatmap_estimate_p_value <- data_heatmap %>%
  mutate(
    estimate_FDR = ifelse(
      is.na(estimate),
      NA, # Assign NA if estimate is NA
      paste0(round(estimate, 2), "\n(FDR=", round(FDR, 2), ")")
    )
  )

data_heatmap_format<-data_heatmap_estimate_p_value[,c("Variable1","Variable2","estimate_FDR")]
data_heatmap_format_df <- pivot_wider(data_heatmap_format, names_from = Variable2, values_from = estimate_FDR)

data_heatmap_format_df <-data.frame(data_heatmap_format_df)

##Adding the colnames for the variable 1
rownames(data_heatmap_format_df)<-data_heatmap_format_df$Variable1

##Remove the column called Variable1
data_heatmap_format_df <- data_heatmap_format_df[, !(names(data_heatmap_format_df) %in% "Variable1")]
data_heatmap_format_df


# Reorder columns to move "Age_per_10" as the first column
data_heatmap_format_df <- data_heatmap_format_df[, c("Age_per_10_imputated", setdiff(names(data_heatmap_format_df), "Age_per_10_imputated"))]



# Create a similar heatmap dataframe for FDR values
data_heatmap_fdr <- data_heatmap[, c("Variable1", "Variable2", "FDR")]

# Pivot the FDR data to match the heatmap structure
heatmap_fdr_df <- pivot_wider(data_heatmap_fdr, names_from = Variable1, values_from = FDR)
heatmap_fdr_df <- data.frame(heatmap_fdr_df)

# Ensure row names match Variable1
rownames(heatmap_fdr_df) <- heatmap_fdr_df$Variable2
heatmap_fdr_df
# Remove the Variable2 column from FDR dataframe
heatmap_fdr_df <- heatmap_fdr_df[,!(names(heatmap_fdr_df) %in% "Variable2")]
#Change the order to put Age_per_10 as the first row
heatmap_fdr_df <-rbind(heatmap_fdr_df[nrow(heatmap_fdr_df),],heatmap_fdr_df[1:(nrow(heatmap_fdr_df)-1),])
heatmap_fdr_df
# Ensure both dataframes are matrices
upper_triangle <- as.matrix(heatmap_estimate_df)

lower_triangle <- as.matrix(heatmap_fdr_df)
lower_triangle

# Combine the two triangles
combined_matrix <- upper_triangle
combined_matrix[lower.tri(combined_matrix)] <- lower_triangle[lower.tri(lower_triangle)]

combined_matrix_numeric <- apply(combined_matrix, c(1, 2), function(x) as.numeric(x))
combined_matrix_numeric
# Optional: Set diagonal to NA (if not needed)
#diag(combined_matrix) <- NA


#CREATION of the MATRIX of colors
data_heatmap_color<-data_heatmap[,c("Variable1","Variable2","color")]
###For the triangle in the upper right
heatmap_color_upper_right <- pivot_wider(data_heatmap_color, names_from = Variable2, values_from = color)
heatmap_color_upper_right <-data.frame(heatmap_color_upper_right)
##Adding the colnames for the variable 1
rownames(heatmap_color_upper_right)<-heatmap_color_upper_right$Variable1
##Remove the column called Variable1
heatmap_color_upper_right <- heatmap_color_upper_right[, !(names(heatmap_color_upper_right) %in% "Variable1")]
# Reorder columns to move "Age_per_10" as the first column
heatmap_color_upper_right <- heatmap_color_upper_right[, c("Age_per_10_imputated", setdiff(names(heatmap_color_upper_right), "Age_per_10_imputated"))]
heatmap_color_upper_right

###For the triangle in the lower left
# Pivot the FDR data to match the heatmap structure
heatmap_color_lower_left <- pivot_wider(data_heatmap_color, names_from = Variable1, values_from = color)
heatmap_color_lower_left <- data.frame(heatmap_color_lower_left)
heatmap_color_lower_left$Variable2
# Ensure row names match Variable1
rownames(heatmap_color_lower_left) <- heatmap_color_lower_left$Variable2
# Remove the Variable2 column from FDR dataframe
heatmap_color_lower_left <- heatmap_color_lower_left[,!(names(heatmap_color_lower_left) %in% "Variable2")]
#Change the order to put Age_per_10 as the first row
heatmap_color_lower_left <-rbind(heatmap_color_lower_left[nrow(heatmap_color_lower_left),],heatmap_color_lower_left[1:(nrow(heatmap_color_lower_left)-1),])
heatmap_color_upper_right
#Combine into a single dataframe
combined_matrix_color <- heatmap_color_upper_right
combined_matrix_color[lower.tri(combined_matrix_color)] <- heatmap_color_lower_left[lower.tri(heatmap_color_lower_left)]
combined_matrix_color_numeric <- apply(combined_matrix_color, c(1, 2), function(x) as.numeric(x))
combined_matrix_color_numeric
# Define a custom color palette function
custom_palette <- function(x) {
  if (is.na(x)) {
    return("black")  # Use a specific color or make it transparent for NA
  } else if (x == -2) {
    return("black")
  } else if (x == -1) {
    return("lightgrey")
  } else {
    # Scale values from 0 to 2.5 to colors from blue to red
    scaled_value <- rescale(x, to = c(1, 100), from = c(0, 2))
    return(colorRampPalette(c("darkred","white","darkgreen"))(100)[
      round(scaled_value)
    ])
  }
}

# Apply the custom color mapping to the matrix
custom_colors <- matrix(
  apply(combined_matrix_color_numeric, c(1, 2), function(x) custom_palette(x)),
  nrow = nrow(combined_matrix_color_numeric),
  ncol = ncol(combined_matrix_color_numeric),
  dimnames = dimnames(combined_matrix_color_numeric)
)


# Apply the custom color mapping to the matrix
custom_colors <- matrix(
  apply(combined_matrix_color_numeric, c(1, 2), custom_palette),
  nrow = nrow(combined_matrix_color_numeric),
  ncol = ncol(combined_matrix_color_numeric),
  dimnames = dimnames(combined_matrix_color_numeric)
)
combined_matrix_numeric
# Create the heatmap
#rownames(combined_matrix_numeric) <-c("Age","Sex (Male)","BMI","Smoking","Atopy","Airborne allergen","CRSsNP","CRSwNP","Treatment step (1-2 vs 3-4)","Treatment step (5 vs 3-4)","Attack history","ACQ","FEV1%","Eosinophils","FeNO","IgE")
#colnames(combined_matrix_numeric) <-c("Age","Sex (Male)","BMI","Smoking","Atopy","Airborne allergen","CRSsNP","CRSwNP","Treatment step (1-2 vs 3-4)","Treatment step (5 vs 3-4)","Attack history","ACQ","FEV1%","Eosinophils","FeNO","IgE")
rownames(combined_matrix_numeric) <-c("Age (per 10 y)","BMI (per 5 kg/m^2)","Smoking history*","Atopy history*","Airborne allergen sensitisation*","CRSsNP*","CRSwNP*","Log10(IgE)","Treatment step (1-2 vs. 4)","Treatment step (5 vs. 4)","Any attack history past 12m","ACQ-5 (per 0.5 increase)","FEV1% preBD (per 10% decrease)","Log10(BEC)","Log10(FeNO)")
colnames(combined_matrix_numeric) <-c("Age (per 10 y)","BMI (per 5 kg/m^2)","Smoking history*","Atopy history*","Airborne allergen sensitisation*","CRSsNP*","CRSwNP*","Log10(IgE)","Treatment step (1-2 vs. 4)","Treatment step (5 vs. 4)","Any attack history past 12m","ACQ-5 (per 0.5 increase)","FEV1% preBD (per 10% decrease)","Log10(BEC)","Log10(FeNO)")
custom_colors
rownames(combined_matrix_numeric)
combined_matrix_numeric



heatmap_object <- Heatmap(
  matrix = combined_matrix_numeric,
  col = colorRamp2(c(0.5, 1, 2), c("darkred", "lightgrey", "darkgreen")), # Adjusted scale
  cell_fun = function(j, i, x, y, width, height, fill) {
    # Draw the filled rectangle (cell background)
    grid.rect(x, y, width, height, gp = gpar(fill = custom_colors[i, j], col = "black", lwd = 1)) # Add border with "col"
    # Add the text inside the cell
    grid.text(sprintf("%.2f", combined_matrix_numeric[i, j]), x, y, gp = gpar(fontsize = 14))
  },
  show_row_names = TRUE,
  show_column_names = FALSE,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  name = "Interaction Factor (aRR)",
  row_title = "FDR-adjusted p-value", # Add text on the left side
  row_title_gp = gpar(fontsize = 16, fontface = "bold"), # Customize the style of the row title
  row_names_gp = gpar(fontsize = 14), # Increase y-axis (row) text size
  column_title = "Interaction Factor (aRR)", # Add text on top of the heatmap
  column_title_gp = gpar(fontsize = 16, fontface = "bold"), # Customize the style of the column title
  bottom_annotation = HeatmapAnnotation(
    text = anno_text(
      rownames(combined_matrix_numeric),
      rot = 45, # Rotate the text
      offset = unit(1, "npc"),
      just = "right",
      gp = gpar(fontsize = 14) # Increase annotation text size
    ),
    annotation_height = max_text_width(rownames(combined_matrix_numeric)) # Adjust annotation height
  ),
  show_heatmap_legend = FALSE, # Hide the legend
  heatmap_legend_param = list(
    title = "Legend Title", # Add a legend title
    title_gp = gpar(fontsize = 14, fontface = "bold"), # Increase legend title text size
    labels_gp = gpar(fontsize = 14) # Increase legend label text size
  )
)



heatmap_object

class(heatmap_object)

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Heatmap_Interactions.png"
png(output_path, width = 4000, height = 2500, res = 300) # Higher resolution (300 DPI) and larger size

# Render the heatmap to the file
ComplexHeatmap::draw(heatmap_object, padding = unit(c(-0.2, 0.7, 0.2, 1.6), "cm"))
# Close the device
dev.off()
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
##Interaction IgE X Nasal polyposis

##Selection of the trials that assess nasal polyposis and IgE
filtered_trials <- unique(Data_Oracle$Enrolled_Trial_name[!is.na(Data_Oracle$CRSwNP)&!is.na(Data_Oracle$IgE_Log)])

#Removing trials that all patients did not have NP (LUTE)
filtered_trials <- filtered_trials[filtered_trials != "LUTE"]
filtered_trials 
  
Data_Table_1_Polyposis <- Data_Oracle %>%
  filter((Enrolled_Trial_name %in% filtered_trials))

nrow(Data_Table_1_Polyposis)
Data_Table_1_Polyposis$Nasal_polyposis_imputated<-as.factor(Data_Table_1_Polyposis$Nasal_polyposis_imputated)
summary(Data_Table_1_Polyposis$IgE_Log_imputated)
summary(Data_Table_1_Polyposis$CRSwNP_imputated)
sum(is.na(Data_Table_1_Polyposis$IgE_Log_imputated_no_systematically_missing))
nrow(Data_Table_1_Polyposis)
#Impact of adding interaction on model performance

##Baseline model
res_comb = NULL
AIC_baseline<-c()
AIC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Table_1_Polyposis, .imp == i))
  AIC_values<-c(AIC_values,AIC(res_comb[[i]]))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)

AIC_baseline<-mean(AIC_values)
AIC_baseline
## + NP
Data_Table_1_Polyposis$CRSwNP_imputated
res_comb = NULL
AIC_w_NP<-c()
AIC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+CRSwNP_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Table_1_Polyposis, .imp == i))
  AIC_values<-c(AIC_values,AIC(res_comb[[i]]))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool
AIC_w_NP<-mean(AIC_values)
AIC_w_NP
res_pool
NP_prognosis<-res_pool[12,c(1,2,7,8,6)]

## + IgE
Data_Table_1_Polyposis$CRSwNP_imputated
res_comb = NULL
AIC_w_IgE<-c()
AIC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+IgE_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Table_1_Polyposis, .imp == i))
  AIC_values<-c(AIC_values,AIC(res_comb[[i]]))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
AIC_w_IgE<-mean(AIC_values)
AIC_w_IgE
IgE_prognosis<-res_pool[12,c(1,2,7,8,6)]
IgE_prognosis

## + NP + IgE
Data_Table_1_Polyposis$IgE_Log_imputated
res_comb = NULL
AIC_w_NP_and_IgE<-c()
AIC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+CRSwNP_imputated+IgE_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Table_1_Polyposis, .imp == i))
  AIC_values<-c(AIC_values,AIC(res_comb[[i]]))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool
AIC_w_NP_and_IgE<-mean(AIC_values)
AIC_w_NP_and_IgE
IgE_prognosis_with_NP<-res_pool[13,c(1,2,7,8,6)]


## + NP * IgE
Data_Table_1_Polyposis$IgE_Log_imputated
res_comb = NULL
AIC_w_NP_X_IgE<-c()
AIC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+CRSwNP_imputated*IgE_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Table_1_Polyposis, .imp == i))
  AIC_values<-c(AIC_values,AIC(res_comb[[i]]))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
AIC_w_NP_X_IgE<-mean(AIC_values)
AIC_w_NP_X_IgE

Interaction_IgE_X_CRSwNP_prognosis<-res_pool[nrow(res_pool),c(1,2,7,8,6)]
Interaction_IgE_X_CRSwNP_only_trials_w_NP<-Interaction_IgE_X_CRSwNP_prognosis
Interaction_IgE_X_CRSwNP_only_trials_w_NP_w_SD<-res_pool[nrow(res_pool),c(1,2,3,7,8,6)]

Table_term_NP_IgE<-rbind(NP_prognosis,IgE_prognosis,IgE_prognosis_with_NP,Interaction_IgE_X_CRSwNP_prognosis)
Table_term_NP_IgE

new_row <- data.frame(
  term = "Multivariable model",
  estimate = NA,
  `2.5 %` = NA,
  `97.5 %` = NA,
  p.value = NA
)
colnames(new_row) <- colnames(Table_term_NP_IgE)

# Combine the new row with the existing table
Table_term_baseline_NP_IgE <- rbind(new_row,Table_term_NP_IgE)
Table_term_baseline_NP_IgE
# Update the `term` column with the new labels
Table_term_baseline_NP_IgE$term <- c(
  "Multivariable model",
  "+ CRSwNP",
  "+ Log10(IgE)",
  "+ CRSwNP + Log10(IgE)",
  "+ CRSwNP + Log10(IgE) + CRSwNP X Log10(IgE)"
)

# Display the updated table
Table_term_baseline_NP_IgE

Table_term_baseline_NP_IgE$AIC <- c(
  AIC_baseline,
  AIC_w_NP,
  AIC_w_IgE,
  AIC_w_NP_and_IgE,
  AIC_w_NP_X_IgE
)
Table_term_baseline_NP_IgE

write_xlsx(Table_term_baseline_NP_IgE,"Table_Model_performance_NP_IgE.xlsx")
#======================================================================================================================================
#Subgroups analysis of the impact IgE
##all patients
res_comb = NULL
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+IgE_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Table_1_Polyposis, .imp == i))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
IgE_all<-res_pool[res_pool[,"term"]=="IgE_Log_imputated",]

#Subgroup No CRSwNP
CRSwNP_No_Group<-subset(Data_Table_1_Polyposis,CRSwNP_imputated=="No")
res_comb = NULL
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+IgE_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(CRSwNP_No_Group, .imp == i))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
IgE_no_CRSwNP<-res_pool[res_pool[,"term"]=="IgE_Log_imputated",]

#Subgroup CRSwNP
CRSwNP_Yes_Group<-subset(Data_Table_1_Polyposis,CRSwNP_imputated=="Yes")
res_comb = NULL
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+IgE_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(CRSwNP_Yes_Group, .imp == i))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
IgE_CRSwNP<-res_pool[res_pool[,"term"]=="IgE_Log_imputated",]

#Subgroup CRSsNP
CRSsNP_Yes_Group<-subset(Data_Table_1_Polyposis,CRSsNP_imputated=="Yes")
res_comb = NULL
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+IgE_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(CRSsNP_Yes_Group, .imp == i))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
IgE_CRSsNP<-res_pool[res_pool[,"term"]=="IgE_Log_imputated",]
IgE_CRSsNP

#Subgroup No CRSsNP and no CRSwNO
CRSsNP_No_CRSwNP_No_Group<-subset(Data_Table_1_Polyposis,CRSsNP_imputated=="No"&CRSwNP_imputated=="No")
res_comb = NULL
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+IgE_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(CRSsNP_No_CRSwNP_No_Group, .imp == i))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool
IgE_CRSsNP_No_CRSwNP_No<-res_pool[res_pool[,"term"]=="IgE_Log_imputated",]
IgE_CRSsNP_No_CRSwNP_No


IgE_with_or_without_CRSwNP<-rbind(IgE_no_CRSwNP,IgE_CRSwNP,IgE_all)
rownames(IgE_with_or_without_CRSwNP)<-c("No CRSwNP","CRSwNP","All")
IgE_with_or_without_CRSwNP
# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitiviy_analysis_Forrest_plot_IgE_in_CRSwNP_groups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 5, units = "in", res = 300)


forplo(as.data.frame(IgE_with_or_without_CRSwNP[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c(paste("Without CRSwNP\n(N=",nrow(subset(CRSwNP_No_Group,.imp==1)),")",sep=""),paste("With CRSwNP\n(N=",nrow(subset(CRSwNP_Yes_Group,.imp==1)),")",sep=""),"All subjects\n(N=4103)"),
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,2),rep(2,1)),
       grouplabs=c("Subgroup","Overall"),
       
       #Define the limits of the X axis
       xlim= c(0.5,2),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=8.5,
       arrow.right.length=8.5,
       
       #Remove the left bar
       left.bar= FALSE,
       
       
       #Adding the p-value
       pval=p_round(IgE_with_or_without_CRSwNP[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=8,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)
dev.off()

IgE_Nil_CRSsNP_CRSwNP<-rbind(IgE_CRSsNP_No_CRSwNP_No,IgE_CRSsNP,IgE_CRSwNP,IgE_all)
rownames(IgE_Nil_CRSsNP_CRSwNP)<-c("No CRSsNP & No CRSwNP","CRSsNP","CRSwNP","All")
IgE_Nil_CRSsNP_CRSwNP

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitiviy_analysis_Forrest_plot_IgE_in_CRS_groups.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 6, units = "in", res = 300)


forplo(as.data.frame(IgE_Nil_CRSsNP_CRSwNP[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c(paste("No CRS\n(N=",nrow(subset(CRSsNP_No_CRSwNP_No_Group,.imp==1)),")",sep=""),paste("CRSsNP\n(N=",nrow(subset(CRSsNP_Yes_Group,.imp==1)),")",sep=""),paste("CRSwNP\n(N=",nrow(subset(CRSwNP_Yes_Group,.imp==1)),")",sep=""),"All subjects\n(N=4103)"),
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,3),rep(2,1)),
       grouplabs=c("Subgroup","Overall"),
       
       #Define the limits of the X axis
       xlim= c(0.5,2),
       
       #Define the arrows
       favorlabs=c('Lower risk','High risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=8.5,
       arrow.right.length=8.5,
       
       #Remove the left bar
       left.bar= FALSE,
       
       
       #Adding the p-value
       pval=p_round(IgE_Nil_CRSsNP_CRSwNP[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=6,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)



dev.off()




colnames(Data_Table_1_Polyposis)
variable_1<-"Total_IgE_imputated"
variable_2<-"CRSwNP_imputated"
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Table_1_Polyposis, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))
colnames(prediction_combine_rcs)<-"Prediction"

Table_prediction_rcs<-cbind(subset(Data_Table_1_Polyposis, .imp == 1),prediction_combine_rcs)
summary(Data_Table_1_Polyposis$CRSwNP_imputated)

Spline_IgE_X_NP<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, colour = .data[[variable_2]])) +
  geom_smooth() +
  xlab("Total IgE (IU/mL)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_color_manual(values = c("black", "#E69F00"), name = "Self-reported\nCRSwNP", labels = c("No (N=3511)", "Yes (N=592)")) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  scale_x_continuous(
    trans = "log10",
    breaks = c(10, 30,100, 300, 1000,3000),  # Set custom breaks
    labels = c("10", "30","100", "300", "1000","3000")  # Custom labels to match breaks
  ) +
  coord_cartesian(xlim=c(3,5000),ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(legend.position = c(0.2, 0.85)) +
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  )
Spline_IgE_X_NP
Density_IgE_X_NP<-ggplot(subset(Data_Table_1_Polyposis, .imp == 1), aes(x = Total_IgE, fill = CRSwNP_imputated)) +
  geom_density(alpha = 0.7, color = "black") +
  scale_x_continuous(
    trans = "log10",
    breaks = c(10, 30,100, 300, 1000,3000),  # Set custom breaks
    labels = c("10", "30","100", "300", "1000","3000")  # Custom labels to match breaks
  ) +
  labs(title = NULL,
       x = "Total IgE (IU/mL)",
       y = "Density",
       fill = "Self-reported\nCRSwNP") +  # Custom legend title
  theme_minimal() +
  coord_cartesian(xlim=c(3,5000)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) +
  scale_fill_manual(
    values = c("grey", "#E69F00"),  # Customize colors for each group
    labels = c("No (N=3511)", "Yes (N=592)")  # Custom labels for legend categories
  )
Density_IgE_X_NP

Data_Table_1_Polyposis <- Data_Table_1_Polyposis %>%
  mutate(CRS_category = case_when(
    CRSwNP_imputated == "Yes" ~ "CRSwNP",
    CRSsNP_imputated == "Yes" ~ "CRSsNP",
    CRSwNP_imputated == "No" & CRSsNP_imputated == "No" ~ "No CRS",
    TRUE ~ NA # Handles any unexpected cases
  ))


Data_Table_1_Polyposis$CRS_category <- factor(
  Data_Table_1_Polyposis$CRS_category,
  levels = c("No CRS", "CRSsNP", "CRSwNP")
)


summary(Data_Table_1_Polyposis$CRS_category)
variable_1<-"Total_IgE_imputated"
variable_2<-"CRS_category"
Prediction_all_rcs<-c()
for (i in 1:10){
  model<- glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ rcs(",variable_1,", 4)*",variable_2,"+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+Any_severe_attack_previous_12m_0no_1yes_imputated+FEV1_preBD_per10_Baseline_imputated+FeNO_Log_imputated+Eosinophils_Log_imputated+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Table_1_Polyposis, .imp == i))
  Prediction_value_rcs<- predict(model, type="response")
  Prediction_all_rcs<-cbind(Prediction_all_rcs,Prediction_value_rcs)
}
prediction_combine_rcs<-data.frame(rowMeans(Prediction_all_rcs))
colnames(prediction_combine_rcs)<-"Prediction"

Table_prediction_rcs<-cbind(subset(Data_Table_1_Polyposis, .imp == 1),prediction_combine_rcs)

Spline_IgE_X_CRS_Category<-ggplot(data = Table_prediction_rcs, aes(x = .data[[variable_1]], y = Prediction, colour = .data[[variable_2]])) +
  geom_smooth() +
  xlab("Total IgE (IU/mL)") +
  ylab("Estimated annualised Severe Asthma Attack Rate") +
  scale_color_manual(values = c("black", "darkgreen","#E69F00"), name = "Self-reported", labels = c("No CRS", "CRSsNP","CRSwNP")) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5,2)) +  # Custom y-axis ticks
  scale_x_continuous(
    trans = "log10",
    breaks = c(10, 30,100, 300, 1000,3000),  # Set custom breaks
    labels = c("10", "30","100", "300", "1000","3000")  # Custom labels to match breaks
  ) +
  coord_cartesian(xlim=c(3,5000),ylim = c(0, 2)) +  # Restrict visible range on y-axis
  theme(legend.position = c(0.2, 0.85)) +
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  )
Spline_IgE_X_CRS_Category
Data_Table_1_Polyposis$IgE_by_group_imputated

Density_IgE_X_CRS_Category<-ggplot(subset(Data_Table_1_Polyposis, .imp == 1), aes(x = Total_IgE, fill = CRS_category)) +
  geom_density(alpha = 0.7, color = "black") +
  scale_x_continuous(
    trans = "log10",
    breaks = c(10, 30,100, 300, 1000,3000),  # Set custom breaks
    labels = c("10", "30","100", "300", "1000","3000")  # Custom labels to match breaks
  ) +
  labs(title = NULL,
       x = "Total IgE (IU/mL)",
       y = "Density",
       fill = "Self-reported") +  # Custom legend title
  theme_minimal() +
  coord_cartesian(xlim=c(3,5000)) +  # Restrict visible range on y-axis
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(colour = "black", size = 0.5, linetype = "solid"),
    legend.title = element_text(face = "bold", size = 12),  # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 24, hjust = -0.16, vjust = 2.5),  # Align title to the left
    plot.margin = margin(t = 10, r = 10, b = 10, l = 30),  # Increase top margin
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(size = 0.5, colour = "black"),  # Add ticks to both axes
    axis.ticks.length = unit(0.2, "cm"),                     # Adjust tick length
    panel.background = element_rect(fill = "white", colour = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = NA)   # Set overall plot background to white
  ) +
  scale_fill_manual(values = c("grey","darkgreen", "#E69F00"))  # Customize colors for each group



#Creation of the multipanel of T2 inflammation biomarkers according to asthma attack history and ICS dose
plot_spline_IgE_X_CRSwNP <- (
  Spline_IgE_X_NP + Density_IgE_X_NP
) + 
  plot_layout(nrow = 1) + 
  plot_annotation(
    tag_levels = "A"
  ) & theme(
    plot.tag = element_text(size = 30, face = "bold"), # Increase tag size
    plot.margin = margin(t = 10, r = 10, b = 10, l = 3) # Adjust margins: top, right, bottom, left
  ) # Automatically adds tags A, B, C, ...
plot_spline_IgE_X_CRSwNP

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Spline_IgE_in_CRSwNP_groups.png"
ggsave(output_path, plot_spline_IgE_X_CRSwNP, width = 12, height = 6, dpi = 600)

#======================================================================================================================================
#Comparison to the biomarkers in their predictive value in patients with CRSwNP

## impact of FeNO
#### Calculate the percentile 25 and 75  and the difference
CRSwNP_Yes_Group$FeNO_Log_imputated
quantile(CRSwNP_Yes_Group$FeNO_baseline_ppb_imputated)[2]
quantile(CRSwNP_Yes_Group$FeNO_baseline_ppb_imputated)[4]
FeNO_delta_75_25<- quantile(CRSwNP_Yes_Group$FeNO_Log_imputated)[4]-quantile(CRSwNP_Yes_Group$FeNO_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset

### Scaling FeNO based on the 25th and 75th percentiles (method by Prof. Frank Harrell)
CRSwNP_Yes_Group <- CRSwNP_Yes_Group %>%
  mutate(FeNO_p_imputated = FeNO_Log_imputated / FeNO_delta_75_25)  # Scale log FeNO by dividing by 0.47 (between the 25th and 75th percentiles)

### Calculating the effect of FeNO change 75-25 quartile
res_comb = NULL
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ Eosinophils_Log_imputated+IgE_Log_imputated+FeNO_p_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(CRSwNP_Yes_Group, .imp == i))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)

FeNO_25_75<-res_pool[res_pool[,"term"]=="FeNO_p_imputated",]
FeNO_25_75


## impact of BEC
### Calculate the percentile 25 and 75  and the difference
CRSwNP_Yes_Group$Eosinophils_Log_imputated
quantile(CRSwNP_Yes_Group$Blood_Eos_baseline_x10_9_cells_per_L_imputated)[2]
quantile(CRSwNP_Yes_Group$Blood_Eos_baseline_x10_9_cells_per_L_imputated)[4]
BEC_delta_75_25<- quantile(CRSwNP_Yes_Group$Eosinophils_Log_imputated)[4]-quantile(CRSwNP_Yes_Group$Eosinophils_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset
BEC_delta_75_25
# Scaling BEC based on the 25th and 75th percentiles (method by Prof. Frank Harrell)
CRSwNP_Yes_Group <- CRSwNP_Yes_Group %>%
  mutate(BEC_p_imputated = Eosinophils_Log_imputated / BEC_delta_75_25)  # Scale log BEC by dividing by 0.47 (between the 25th and 75th percentiles)

### Calculating the effect of BEC change 75-25 quartile
res_comb = NULL
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated+IgE_Log_imputated+BEC_p_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(CRSwNP_Yes_Group, .imp == i))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool
BEC_25_75<-res_pool[res_pool[,"term"]=="BEC_p_imputated",]
BEC_25_75


## impact of IgE
### Calculate the percentile 25 and 75  and the difference
CRSwNP_Yes_Group
quantile(CRSwNP_Yes_Group$Total_IgE_imputated)[2]
quantile(CRSwNP_Yes_Group$Total_IgE_imputated)[4]
IgE_delta_75_25<- quantile(CRSwNP_Yes_Group$IgE_Log_imputated)[4]-quantile(CRSwNP_Yes_Group$IgE_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset

# Scaling BEC based on the 25th and 75th percentiles (method by Prof. Frank Harrell)
CRSwNP_Yes_Group <- CRSwNP_Yes_Group %>%
  mutate(IgE_p_imputated = IgE_Log_imputated / IgE_delta_75_25)  # Scale BEC by dividing by 0.268 (between the 25th and 75th percentiles)

### Calculating the effect of BEC change 75-25 quartile
res_comb = NULL
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated+Eosinophils_Log_imputated+IgE_p_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(CRSwNP_Yes_Group, .imp == i))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool
IgE_25_75<-res_pool[res_pool[,"term"]=="IgE_p_imputated",]
IgE_25_75

T2_biomarkers_25_75<-rbind(FeNO_25_75,BEC_25_75,IgE_25_75)


# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Forrest_plot_T2_biomarkers_in_CRSwNP.png"
# Open a PNG device with high resolution
png(filename = output_path, width = 8, height = 5, units = "in", res = 300)


forplo(as.data.frame(T2_biomarkers_25_75[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = c("FeNO\n(56 vs 19 ppb)","BEC\n(0.57 vs 0.19 ×10^9 cells/L)","IgE\n(371 vs 72 IU/mL)"),
       left.align=FALSE,
       
       #Define the groups
       #groups=c(rep(1,3),rep(2,1)),
       #grouplabs=c("Subgroup","Overall"),
       
       #Define the limits of the X axis
       xlim= c(0.5,2),
       
       #Define the arrows
       favorlabs=c('Lower risk','High risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=6,
       arrow.right.length=6,
       
       #Remove the left bar
       left.bar= FALSE,
       
       
       #Adding the p-value
       pval=p_round(T2_biomarkers_25_75[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=12,
       margin.right=10,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
)
dev.off()

#======================================================================================================================================
## Interaction IgE X CRSwNP according of the trial
# Initialize a list to store results for each trial
results_by_trial <- list()
# Loop through each trial in Enrolled_Trial_name
for (trial in unique(Data_Table_1_Polyposis$Enrolled_Trial_name)) {
  trial
  # Filter the data for the current trial
  trial_data <- subset(Data_Table_1_Polyposis, Enrolled_Trial_name == trial)
  # Check if Treatment_step_1and2 and Any_severe_attack_previous_12m_0no_1yes have only one unique value
  include_treatment_step <- length(unique(trial_data$Treatment_step_1and2)) > 1
  include_any_attack <- length(unique(trial_data$Any_severe_attack_previous_12m_0no_1yes_imputated)) > 1
  # Initialize objects for results storage
  res_comb <- list()
  R_2 <- c()
  # Perform the analysis for each imputation
  for (i in 1:10) {
    i
    # Start building the formula dynamically
    base_formula <- "Number_severe_asthma_attacks_during_followup ~ Age_per_10_imputated + Gender_imputated + BMI_per_5_imputated + ACQ_baseline_score_mean_imputated +FEV1_preBD_per10_Baseline_imputated+ FeNO_Log_imputated + Eosinophils_Log_imputated+IgE_Log_imputated*CRSwNP_imputated  + offset(log(Follow_up_duration_days))"
    include_treatment_step
    # Add Treatment_step_1and2 if it has more than one unique level
    if (include_treatment_step) {
      base_formula <- paste(base_formula, "+ Treatment_step_1and2")
    }
    
    # Add Any_severe_attack_previous_12m_0no_1yes if it has more than one unique level
    if (include_any_attack) {
      base_formula <- paste(base_formula, "+ Any_severe_attack_previous_12m_0no_1yes_imputated")
    }
    base_formula
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(as.formula(base_formula), data = subset(trial_data, .imp == i))
    
    # Calculate R-squared for the model
    R_2 <- c(R_2, with(summary(res_comb[[i]]), 1 - deviance / null.deviance))
  }
  # Calculate mean R-squared
  R2_mean <- mean(R_2)
  # Pool results
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  # Extract relevant results (adjust based on your specific needs)
  Results_multivariate_analysis <- res_pool[nrow(res_pool), c(1, 2,3, 7, 8, 6)]
  # Store results for the current trial
  results_by_trial[[trial]] <- list(
    R2_mean = R2_mean,
    Multivariate_Results = Results_multivariate_analysis
  )
}
results_by_trial
# Creation of a dataframe to do the compilation of the data
results_df <- data.frame()
# Loop through the trials in results_by_trial
for (trial in names(results_by_trial)) {
  # Extract the multivariate results for the current trial
  trial_results <- results_by_trial[[trial]]$Multivariate_Results
  # Add the term and trial information
  trial_results <- cbind(trial = trial, term = rownames(trial_results), trial_results)
  
  # Select and rename columns
  trial_results <- trial_results[, c("trial", "term", "estimate","std.error", "2.5 %", "97.5 %", "p.value")]
  # Append the results to the data frame
  results_df <- rbind(results_df, trial_results)
}
trial_results


Data_Table_1_Polyposis$Enrolled_Trial_name<-as.factor(Data_Table_1_Polyposis$Enrolled_Trial_name)
Subject_per_trial<-summary(Data_Table_1_Polyposis$Enrolled_Trial_name)/10
Subject_per_trial

Interaction_IgE_X_CRSwNP_only_trials_w_NP_w_SD <- Interaction_IgE_X_CRSwNP_only_trials_w_NP_w_SD %>%
  dplyr::rename(trial = term) %>%
  mutate(trial = "Pooled_effect")



results_df_with_all<-rbind(results_df[,c("trial","estimate","std.error","2.5 %","97.5 %","p.value")],Interaction_IgE_X_CRSwNP_only_trials_w_NP_w_SD)

# View the updated dataframe


Subject_per_trial <- c(
  AZISAST = 54, BENRAP2B = 211, COSTA = 145, DREAM = 155, 
  DRI12544 = 155, EXTRA = 420, LAVOLTA_1 = 362, LAVOLTA_2 = 354, 
  MILLY = 112, NAVIGATOR = 517, 
  PATHWAY = 133, QUEST = 634, STRATOS_1 = 388, STRATOS_2 = 413, VERSE = 50,Pooled_effect=5988
)

# Add a new column to results_df_with_all
results_df_with_all$trial_with_subjects <- paste0(
  results_df_with_all$trial, " (N=", Subject_per_trial[results_df_with_all$trial], ")"
)
results_df_with_all

# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitivity_analysis_Forest_plot_interaction_IgE_X_CRSwNP_per_trial.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 9, height = 6, units = "in", res = 300)

# In forrest plot -> View the results data frame
forplo(as.data.frame(results_df_with_all[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = results_df_with_all$trial_with_subjects, 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,nrow(results_df_with_all)-1),2),
       grouplabs=c('Trial name',"Overall"),
       
       #Define the limits of the X axis
       xlim= c(0.1,10),
       
       #Define the arrows
       favorlabs=c('Negative Interaction','Positive Interaction'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=9,
       arrow.right.length=9,
       
       #Remove the left bar
       left.bar= FALSE,
       
       
       #Adding the p-value
       pval=p_round(results_df_with_all[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=10,
       margin.right=8,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5,
       
       #Characteristic of the error bar (color when not significant)
       insig.col="grey",
       diamond=nrow(results_df_with_all),
)
meta_interaction <- rma(
  yi = results_df_with_all[1:nrow(results_df_with_all)-1,"estimate"], # Effect sizes (interaction coefficients)
  sei = results_df_with_all[1:nrow(results_df_with_all)-1,"std.error"],      # Standard errors
  method = "REML"               # Random-effects model
)

# Extract heterogeneity statistics
I2 <- meta_interaction$I2          # I² statistic
I2

text(
  x = 0.045,  # Adjust x based on your plot's x-axis range
  y = 1,  # Adjust y to a position just below the last row
  labels = paste("I² =", round(I2 / 100, 2), sep = ""),
  cex = 1,  # Font size
  font = 1,   # Bold font
  col = "black"  # Text color
)
dev.off()
#mtext("A", side = 3, line = 2.5, adj = -1, cex = 2, font = 2)



#======================================================================================================================================
#Evaluation of the effect of Ige in patient with co-existing CRSwNP
#Subgroup CRSwNP
CRSwNP_Yes_Group<-subset(Data_Table_1_Polyposis,CRSwNP_imputated=="Yes")

# Initialize a list to store results for each age group
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(CRSwNP_Yes_Group$Age_by_group_imputated)

# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, Age_by_group_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        #Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_age_group<-results_df


# Sex
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(CRSwNP_Yes_Group$Gender_imputated)

# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, Gender_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        #Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_sex_group<-results_df


# BMI
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(CRSwNP_Yes_Group$BMI_by_group_imputated)

# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, BMI_by_group_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        #BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_BMI_group<-results_df




# Smoking history
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(CRSwNP_Yes_Group$Smoking_Statut_imputated)
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, Smoking_Statut_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))
results_df

IgE_in_CRSwNP_per_smoking_history<-results_df


# Atopy
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(CRSwNP_Yes_Group$Atopy_history_imputated)
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, Atopy_history_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))
results_df

IgE_in_CRSwNP_per_Atopy_history<-results_df

# Airborne allergen sensibilisation
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- levels(CRSwNP_Yes_Group$Airborne_allergen_sensibilisation_imputated)
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, Airborne_allergen_sensibilisation_imputated == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))
results_df

IgE_in_CRSwNP_per_Airborne_allergen_sensibilisation<-results_df






# GINA step
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- c("Step 4","Step 5")
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, Treatment_step == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        #Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_GINA_group<-results_df



# ICS_DOSE_CLASS
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated
groups <- c("Medium","High")
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, ICS_DOSE_CLASS == group)
  
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        #Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_ICS_group<-results_df


# ACQ5_by_group_imputated
res_comb_by_group <- list()
# Get the unique values of Age_by_group_imputated

summary(CRSwNP_Yes_Group$ACQ_baseline_score_mean_imputated)

CRSwNP_Yes_Group <- CRSwNP_Yes_Group %>%
  mutate(ACQ_group_2 = case_when(
    ACQ_baseline_score_mean_imputated < 3 ~ "<3",
    ACQ_baseline_score_mean_imputated >= 3 ~ ">=3"
  ))

# Convert to factor for easier analysis and consistent levels
CRSwNP_Yes_Group$ACQ_group_2 <- factor(
  CRSwNP_Yes_Group$ACQ_group_2, 
  levels = c("<3", ">=3")
)
groups <- levels(CRSwNP_Yes_Group$ACQ_group_2)


# Loop through each ACQ group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, ACQ_group_2 == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        #ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_ACQ_group<-results_df



# Any_severe_attack_previous_12m_0no_1yes_imputated
res_comb_by_group <- list()
# Get the unique values
CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated<-as.factor(CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated)
groups <- levels(CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, Any_severe_attack_previous_12m_0no_1yes_imputated == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        #Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_Attack_group<-results_df




# FEV1_preBD_Baseline_by_group_imputated
res_comb_by_group <- list()
# Get the unique values
CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated<-as.factor(CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated)
groups <- levels(CRSwNP_Yes_Group$FEV1_preBD_Baseline_by_group_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, FEV1_preBD_Baseline_by_group_imputated == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        #FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_FEV1_group<-results_df




# FeNO_baseline_by_group_imputated
res_comb_by_group <- list()
# Get the unique values
CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated<-as.factor(CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated)
groups <- levels(CRSwNP_Yes_Group$FeNO_baseline_by_group_imputated)
summary(CRSwNP_Yes_Group$FeNO_baseline_by_group_imputated)
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, FeNO_baseline_by_group_imputated == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        #FeNO_Log_imputated +
        Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_FeNO_group<-results_df



# Eosinophils_by_group_imputated
res_comb_by_group <- list()
# Get the unique values
CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated<-as.factor(CRSwNP_Yes_Group$Any_severe_attack_previous_12m_0no_1yes_imputated)
groups <- levels(CRSwNP_Yes_Group$Eosinophils_by_group_imputated)
groups
# Loop through each age group
for (group in groups) {
  group
  # Subset the data for the current age group
  group_data <- subset(CRSwNP_Yes_Group, Eosinophils_by_group_imputated == group)
  group_data
  # Initialize a list to store results for the current age group
  res_comb <- list()
  
  # Loop through imputations
  for (i in 1:10) {
    
    # Subset the data for the current imputation and age group
    imputed_data <- subset(group_data, .imp == i)
    # Fit the model for the current imputation
    res_comb[[i]] <- glm.nb(
      Number_severe_asthma_attacks_during_followup ~ 
        Age_imputated +
        Gender_imputated +
        BMI_per_5_imputated +
        Treatment_step_1and2 +
        ACQ_baseline_score_mean_imputated +
        Any_severe_attack_previous_12m_0no_1yes_imputated +
        FEV1_preBD_per10_Baseline_imputated +
        FeNO_Log_imputated +
        #Eosinophils_Log_imputated +
        IgE_Log_imputated +
        offset(log(Follow_up_duration_days)) +
        as.factor(Enrolled_Trial_name),
      data = imputed_data
    )
    
    
  }
  
  # Pool the results for the current age group
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract the IgE_Log_imputated term and store it
  res_comb_by_group[[as.character(group)]] <- res_pool[res_pool[,"term"] == "IgE_Log_imputated",]
}

# Combine results into a single data frame for review
results_df <- do.call(rbind, lapply(names(res_comb_by_group), function(group) {
  cbind(Group = group, res_comb_by_group[[group]])
}))

IgE_in_CRSwNP_per_BEC_group<-results_df




IgE_CRSwNP_per_groups<-rbind(
  IgE_in_CRSwNP_per_age_group,
  IgE_in_CRSwNP_per_sex_group,
  IgE_in_CRSwNP_per_BMI_group,
  IgE_in_CRSwNP_per_smoking_history,
  IgE_in_CRSwNP_per_Atopy_history,
  IgE_in_CRSwNP_per_Airborne_allergen_sensibilisation,
  IgE_in_CRSwNP_per_ICS_group,
  IgE_in_CRSwNP_per_ACQ_group,
  #IgE_in_CRSwNP_per_Attack_group,
  IgE_in_CRSwNP_per_FEV1_group,
  IgE_in_CRSwNP_per_FeNO_group,
  IgE_in_CRSwNP_per_BEC_group
)


# Define the file path and name
output_path <- "/Users/macbookpro/Documents/École/Doctorat en recherche/Articles/Interaction_analysis/Figures_interaction/Figures_interactions_all/Sensitivity_analysis_Forest_plot_IgE_in_CRSwNP_according_others_characteristics.png"

# Open a PNG device with high resolution
png(filename = output_path, width = 9, height = 12, units = "in", res = 300)

forplo(as.data.frame(IgE_CRSwNP_per_groups[,c("estimate","2.5 %","97.5 %")]),
       #Define the units
       em="aRR",
       
       #Define the labels
       row.labels = IgE_CRSwNP_per_groups$Group, 
       left.align=FALSE,
       
       #Define the groups
       groups=c(rep(1,4),rep(2,2),rep(3,4),rep(4,2),rep(5,2),rep(6,2),rep(7,2),rep(8,2),rep(9,4),rep(10,3),rep(11,3)),
       grouplabs=c('Age',"Sex","BMI","Smoking history","Atopy history","Airborne allergen sensitisation","ICS dose","ACQ-5","FEV1%","FeNO","BEC"),
       
       #Define the limits of the X axis
       xlim= c(0.1,10),
       
       #Define the arrows
       favorlabs=c('Lower risk','Higher risk'),
       add.arrow.left=TRUE,
       add.arrow.right=TRUE,
       arrow.left.length=8,
       arrow.right.length=8,
       
       #Remove the left bar
       left.bar= TRUE,
       
       
       #Adding the p-value
       pval=p_round(IgE_CRSwNP_per_groups[,"p.value"], digits = 2),
       ci.sep="-",
       
       #Define the shading
       shade.every=1,
       shade.col='gray',
       
       #Define the margin
       margin.left=14,
       margin.right=8,
       margin.bottom = 1,
       
       #Characteristics of the points (char is equivalent of pch)
       char=20,
       size=1.5#,
       
       #Characteristic of the error bar (color when not significant)
       #insig.col="grey",
       #diamond=nrow(results_df_with_all),
)
dev.off()

#Subgroup CRSwNP
res_comb = NULL
AIC_values <- c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Age_imputated+Gender_imputated+BMI_per_5_imputated+Treatment_step_1and2+ACQ_baseline_score_mean_imputated+ Any_severe_attack_previous_12m_0no_1yes_imputated +FEV1_preBD_per10_Baseline_imputated + Eosinophils_Log_imputated+FeNO_Log_imputated*IgE_Log_imputated +offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(CRSwNP_Yes_Group, .imp == i))
  AIC_values <- c(AIC_values, AIC(res_comb[[i]]))
  }
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool
mean(AIC_values)
#======================================================================================================================================
#Evaluation of the effect of integration or not of the differents interactions
colnames(Data_Oracle)
Interaction_terms<- c("+","+FeNO_Log:Any_severe_attack_previous_12m_0no_1yes+","+FeNO_Log:Treatment_step+","+FeNO_Log:Treatment_step_1and2+","+FeNO_Log:Treatment_step_1and2+","+FeNO_Log:Treatment_step_1and2+","+Eosinophils_Log:FEV1_preBD_per10_Baseline+","+FeNO_Log:Any_severe_attack_previous_12m_0no_1yes+Eosinophils_Log:FEV1_preBD_per10_Baseline+","+FeNO_Log:Any_severe_attack_previous_12m_0no_1yes+Eosinophils_Log:FEV1_preBD_per10_Baseline+FeNO_Log:Treatment_step_1and2+","+Eosinophils_Log:FEV1_preBD_per10_Baseline+FeNO_Log:Treatment_step_1and2:Any_severe_attack_previous_12m_0no_1yes+")
int<-""
i<-1
Int_term<-c()
AICC_mean<-c()
R2_mean<-c()
for (int in Interaction_terms) {
  AICC_values<-c()
  R2_values<-c()
  res_comb = NULL
  for (i in 1:10){
    res_comb[[i]] =glm.nb(as.formula(paste("Number_severe_asthma_attacks_during_followup ~ Treatment_step_1and2+ACQ_baseline_score_mean+ Any_severe_attack_previous_12m_0no_1yes    +FEV1_preBD_per10_Baseline+ FeNO_Log + Eosinophils_Log",int,"+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")), data = subset(Data_Oracle, .imp == i))
    AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
    R2_values<-c(R2_values,1 - (summary(res_comb[[i]])$deviance / summary(res_comb[[i]])$null.deviance))
  }
  res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
  Int_term<-c(Int_term,int)
  AICC_mean<-c(AICC_mean,mean(AICC_values))
  R2_mean<-c(R2_mean,mean(R2_values))
}
Interactions_R2_and_AICc<- data.frame(Int_term,R2_mean,AICC_mean)
Interactions_R2_and_AICc

#Final model
res_comb = NULL
AICC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~ Treatment_step_1and2+ACQ_baseline_score_mean+Any_severe_attack_previous_12m_0no_1yes+FEV1_preBD_per10_Baseline+ FeNO_Log + Eosinophils_Log+Eosinophils_Log:FEV1_preBD_per10_Baseline+FeNO_Log:Any_severe_attack_previous_12m_0no_1yes+FeNO_Log:Treatment_step_1and2+offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name), data = subset(Data_Oracle, .imp == i))
  AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
  R2_values<-c(R2_values,1 - (summary(res_comb[[i]])$deviance / summary(res_comb[[i]])$null.deviance))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool 
AICC_mean<-mean(AICC_values)
R2_mean<-mean(R2_values)
AICC_mean
summary(Data_Oracle$Treatment_step)



