
options(digits=2)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(xlsx)) install.packages("xlsx", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(knitr)
library(ggthemes)
library(ggplot2)
library(xlsx)

data <- read.xlsx("Bandab1400.xlsx", sheetName = "Sheet1", header = TRUE)

#_______________________________________________________
# Employee's Information
#_______________________________________________________
print("number of unique employees tested:")
sum <- length(as.array(unique(data$PatientID)))
sum

data_m <- data %>% group_by(PatientID) %>% filter(Sex == "M")
data_f <- data %>% group_by(PatientID) %>% filter(Sex == "F")
no_of_m <- length(unique(data_m$PatientID))
no_of_f <- length(unique(data_f$PatientID))

print("Average Age of all employees tested:")
Ages <- data %>% group_by(PatientID) %>% .$Age
mean(as.numeric(Ages))

#Average Age of the men tested
Ages <- data_m %>% group_by(PatientID) %>% .$Age
mean_age_m <- mean(as.numeric(Ages))
#Youngest man
min_age_m <- min(Ages)
#Oldest man
max_age_m <- max(Ages)

#Average Age of the women tested
Ages <- data_f %>% group_by(PatientID) %>% .$Age
mean_age_f <- mean(as.numeric(Ages))
#Youngest woman
min_age_f <- min(Ages)
#Oldest woman
max_age_f <- max(Ages)

table1 <- data.frame(Sex = c("Male", "Female"),
                        Number = c(no_of_m, no_of_f),
                        Percentage = c(no_of_m/sum, no_of_f/sum),
                        AverageAge = c(mean_age_m, mean_age_f),
                        minAge = c(min_age_m, min_age_f),
                        maxAge = c(max_age_m, max_age_f))
kable(table1, caption = "Employee's information Table")

rm(Ages, max_age_f, max_age_m, mean_age_m, mean_age_f, min_age_m, min_age_f, no_of_m, no_of_f, sum)

#_______________________________________________________
# LDL ANALYSIS
#_______________________________________________________

data %>% group_by(PatientID) %>%
  filter(Name == "LDL,Serum....................") %>%
  mutate(LDL = as.numeric(Ans), ID = PatientID, Sex = Sex) %>%
  ggplot(aes(ID,LDL, col = Sex)) +
  theme_base() +
  geom_abline(slope = 0, intercept = 130, col = "red", size = 1) +
  geom_abline(slope = 0, intercept = 57, col = "green" , size = 1) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  scale_y_continuous(breaks = c(57,130)) +
  theme(axis.text.x=element_blank()) +
  xlab("IDs") + ylab("LDL")


#Number of People with LDL higher than 130:
data %>% group_by(PatientID) %>% filter(Name == "LDL,Serum....................") %>%
  mutate(Ans = as.numeric(Ans)) %>% filter(Ans>130) %>% .$PatientID %>% length()

# Overall Average of LDL:
data %>% group_by(PatientID) %>% filter(Name == "LDL,Serum....................") %>%
  mutate(Ans = as.numeric(Ans)) %>% .$Ans %>% mean()

#_______________________________________________________
# Triglyceride ANALYSIS
#_______________________________________________________

data %>% group_by(PatientID) %>%
  filter(Name == "Triglyceride,Serum...........") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  summarize(Triglyceride = Ans, ID = PatientID, Sex = Sex) %>%
  ggplot(aes(ID, Triglyceride, col = Sex)) +
  theme_base() +
  geom_abline(slope = 0, intercept = 50, col = "green", size = 1) +
  geom_abline(slope = 0, intercept = 150, col = "red", size = 1) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  scale_y_continuous(breaks = c(50,150)) +
  theme(axis.text.x=element_blank()) +
  xlab("IDs") + ylab("Triglyceride")


#Number of People with Triglyceride higher than 150:
data %>% group_by(PatientID) %>%
  filter(Name == "Triglyceride,Serum...........") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  select(PatientID,Name,Ans) %>%
  filter(Ans>150) %>% .$PatientID %>% length()

# overall average of Triglyceride:
data %>% group_by(PatientID) %>% filter(Name == "Triglyceride,Serum...........") %>%
  mutate(Ans = as.numeric(Ans)) %>% .$Ans %>% mean()

#_______________________________________________________
# Cholesterol ANALYSIS
#_______________________________________________________

data %>% group_by(PatientID) %>%
  filter(Name == "Cholesterol,Total,Serum.....") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  summarize(Cholesterol = Ans, ID = PatientID, Sex = Sex) %>%
  ggplot(aes(ID, Cholesterol, col = Sex)) +
  theme_base() +
  geom_abline(slope = 0, intercept = 200, col = "orange", size = 1) +
  geom_abline(slope = 0, intercept = 240, col = "red", size = 1) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  scale_y_continuous(breaks = c(200,240)) +
  theme(axis.text.x=element_blank()) +
  xlab("IDs") + ylab("Cholesterol")

#Number of People with Cholesterol higher than 200:
data %>% group_by(PatientID) %>%
  filter(Name == "Cholesterol,Total,Serum.....") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  select(PatientID,Name,Ans) %>%
  filter(Ans>200) %>% .$PatientID %>% length()

# overall average of Cholesterol:
data %>% group_by(PatientID) %>% filter(Name == "Cholesterol,Total,Serum.....") %>%
  mutate(Ans = as.numeric(Ans)) %>% .$Ans %>% mean()


#_______________________________________________________
# Fasting Blood Sugar (FBS) ANALYSIS
#_______________________________________________________

data %>% group_by(PatientID) %>%
  filter(Name == "Fasting Blood Sugar...........") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  summarize(FBS = Ans, ID = PatientID, Sex = Sex) %>%
  ggplot(aes(ID, FBS, col = Sex)) +
  theme_base() +
  geom_abline(slope = 0, intercept = 65, col = "green", size = 1) +
  geom_abline(slope = 0, intercept = 100, col = "red", size = 1) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  scale_y_continuous(breaks = c(65,100)) +
  theme(axis.text.x=element_blank()) +
  xlab("IDs") + ylab("Fasting Blood Sugar (FBS)")

#Number of People with FBS higher than 100:
data %>% group_by(PatientID) %>%
  filter(Name == "Fasting Blood Sugar...........") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  select(PatientID,Name,Ans) %>%
  filter(Ans>100) %>% .$PatientID %>% length()

# overall average of Cholesterol:
data %>% group_by(PatientID) %>% filter(Name == "Fasting Blood Sugar...........") %>%
  mutate(Ans = as.numeric(Ans)) %>% .$Ans %>% mean()


#_______________________________________________________
# Serum Glutamic Pyruvic Transaminase (SGPT)
#_______________________________________________________

data %>% group_by(PatientID) %>%
  filter(Name == "SGPT (ALT),Serum.............") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  summarize(SGPT = Ans, ID = PatientID, Sex = Sex) %>%
  ggplot(aes(ID, SGPT, col = Sex)) +
  theme_base() +
  geom_abline(slope = 0, intercept = 33, col = "red", size = 1) +
  geom_abline(slope = 0, intercept = 41, col = "red", size = 1) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  scale_y_continuous(breaks = c(33,41)) +
  theme(axis.text.x=element_blank()) +
  xlab("IDs") + ylab("SGPT (ALT)")

#Number of Men with SGPT higher than 41:
data %>% group_by(PatientID) %>%
  filter(Sex == "M") %>%
  filter(Name == "SGPT (ALT),Serum.............") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  select(PatientID,Name,Ans) %>%
  filter(Ans>41) %>% .$PatientID %>% length()

#Number of Women with SGPT higher than 33:
data %>% group_by(PatientID) %>%
  filter(Sex == "F") %>%
  filter(Name == "SGPT (ALT),Serum.............") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  select(PatientID,Name,Ans) %>%
  filter(Ans>33) %>% .$PatientID %>% length()

# overall average of SGPT in men:
data %>% group_by(PatientID) %>% filter(Name == "SGPT (ALT),Serum.............") %>%
  filter(Sex == "M") %>%
  mutate(Ans = as.numeric(Ans)) %>% .$Ans %>% mean()

# overall average of SGPT in women:
data %>% group_by(PatientID) %>% filter(Name == "SGPT (ALT),Serum.............") %>%
  filter(Sex == "F") %>%
  mutate(Ans = as.numeric(Ans)) %>% .$Ans %>% mean()


#_______________________________________________________
# Serum Glutamic-Oxaloacetic Transaminase (SGOT)
#_______________________________________________________

data %>% group_by(PatientID) %>%
  filter(Name == "SGOT (AST),Serum.............") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  summarize(SGOT = Ans, ID = PatientID, Sex = Sex) %>%
  ggplot(aes(ID, SGOT, col = Sex)) +
  theme_base() +
  geom_abline(slope = 0, intercept = 32, col = "red", size = 1) +
  geom_abline(slope = 0, intercept = 40, col = "red", size = 1) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  scale_y_continuous(breaks = c(32,40)) +
  theme(axis.text.x=element_blank()) +
  xlab("IDs") + ylab("SGOT (AST)")

#Number of Men with SGOT higher than 40:
data %>% group_by(PatientID) %>%
  filter(Sex == "M") %>%
  filter(Name == "SGOT (AST),Serum.............") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  select(PatientID,Name,Ans) %>%
  filter(Ans>40) %>% .$PatientID %>% length()

#Number of Women with SGOT higher than 32:
data %>% group_by(PatientID) %>%
  filter(Sex == "F") %>%
  filter(Name == "SGOT (AST),Serum.............") %>%
  mutate(Ans = as.numeric(Ans)) %>%
  select(PatientID,Name,Ans) %>%
  filter(Ans>32) %>% .$PatientID %>% length()

# overall average of SGOT in men:
data %>% group_by(PatientID) %>% filter(Name == "SGOT (AST),Serum.............") %>%
  filter(Sex == "M") %>%
  mutate(Ans = as.numeric(Ans)) %>% .$Ans %>% mean()

# overall average of SGOT in women:
data %>% group_by(PatientID) %>% filter(Name == "SGOT (AST),Serum.............") %>%
  filter(Sex == "F") %>%
  mutate(Ans = as.numeric(Ans)) %>% .$Ans %>% mean()


#_______________________________________________________
# Prostate-Specific-Antigen (PSA)
#_______________________________________________________

data %>% group_by(PatientID) %>%
  filter(Name == "PSA,Total (CLIA)..............") %>%
  select(PatientID, Ans) %>%
  mutate(Ans = as.numeric(Ans)) %>%
  summarize(PSA = Ans , ID = PatientID) %>%
  ggplot(aes(ID, PSA)) + theme_base() +
  geom_abline(slope = 0, intercept = 3.1, col = "red", size = 1) +
  geom_abline(slope = 0, intercept = 4.4, col = "red", size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = c(3.1,4.4)) +
  theme(axis.text.x=element_blank())+
  xlab("IDs") + ylab("Prostate Specific Antigen (PSA)")
