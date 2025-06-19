# global.R

# Loading all required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(car)
library(pROC)
library(corrplot)
library(broom)
library(ggpubr)

# Loading the data
df <- read.csv("data/heart_failure_clinical_records_dataset.csv")

# Converting data types according to need
df$anaemia <- factor(df$anaemia, labels = c("No", "Yes"))
df$diabetes <- factor(df$diabetes, labels = c("No", "Yes"))
df$high_blood_pressure <- factor(df$high_blood_pressure, labels = c("No", "Yes"))
df$sex <- factor(df$sex, labels = c("Female", "Male"))
df$smoking <- factor(df$smoking, labels = c("No", "Yes"))
df$DEATH_EVENT <- factor(df$DEATH_EVENT, labels = c("Survived", "Died"))
df$DEATH_EVENT_num <- as.numeric(df$DEATH_EVENT == "Died")


# Common variable lists
numeric_cols <- c("age", "creatinine_phosphokinase", "ejection_fraction",
                  "platelets", "serum_creatinine", "serum_sodium", "time")

group_vars <- c("sex", "anaemia", "diabetes", "high_blood_pressure")


df$DEATH_EVENT_num <- as.numeric(df$DEATH_EVENT == "Died")
surv_obj <- Surv(df$time, df$DEATH_EVENT_num)
km_fit <- survfit(surv_obj ~ 1)