## Feb 14 Data Analysis

# setup
library(tidyverse)
library(papaja)
library(broom)
library(knitr)
library(tinytex)
library(kableExtra)

# Data setting
EM_dataset_wide <- read.csv("data/EM-Study1_Feb14.csv")
EM_dataset_long <- EM_dataset_wide %>%
  pivot_longer(cols = c("Surprise.Important", "Surprise.Interesting", "Surprise.Difficulty", "Surprise.Motivation", "Awe.Important", "Awe.Interesting", "Awe.Difficulty", "Awe.Motivation", "Enjoyment.Important", "Enjoyment.Interesting", "Enjoyment.Difficulty", "Enjoyment.Motivation", "Neutral.Important", "Neutral.Interesting", "Neutral.Difficulty", "Neutral.Motivation", "Fear.Important", "Fear.Interesting", "Fear.Difficulty", "Fear.Motivation", "Sadness.Important", "Sadness.Interesting", "Sadness.Difficulty", "Sadness.Motivation"), 
               names_to = "Name", 
               values_to = "Result") %>%
  separate_wider_delim(Name,
                       delim = ".",
                       names = c("Emotion", "Measure")) %>%
  separate_wider_delim(Result,
                       delim = ".",
                       names = c("Score", "Note"))
EM_dataset_long$Child.Gender <- factor(EM_dataset_long$Child.Gender)
EM_dataset_long$Child.Race <- factor(EM_dataset_long$Child.Race)
EM_dataset_wide$Child.Gender <- factor(EM_dataset_wide$Child.Gender)
EM_dataset_wide$Child.Race <- factor(EM_dataset_wide$Child.Race)
EM_dataset_long$Score <- as.numeric(EM_dataset_long$Score)
EM_dataset_long$Emotion <- factor(EM_dataset_long$Emotion, ordered = FALSE)

# r descriptive-analysis-demographic
EM_dataset_wide %>%
  summarize(mean = mean(Child.Age.In.Years),
            median = median(Child.Age.In.Years),
            sd = sd(Child.Age.In.Years),
            range = diff(range(Child.Age.In.Years)))
summary(EM_dataset_wide$Child.Gender)
summary(EM_dataset_wide$Child.Race)

# r descriptive-analysis-data
EM_dataset_long %>%
  group_by(Emotion, Measure) %>%
  summarize(mean = mean(Score),
            sd = sd(Score)) %>%
  kable()

# r regression-model-motivation1, include=FALSE
filtered_data_1 <- subset(EM_dataset_long, Measure == "Motivation")
filtered_data_1$Emotion <- relevel(filtered_data_1$Emotion, ref = "Neutral")
regmodel_1 <- lm(Score ~ Emotion + (1|ID), data = filtered_data_1)

regression_results_1 <- tidy(regmodel_1)
regression_table_1 <- regression_results_1 %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate_if(is.numeric, function(x) sprintf("%.3f", x)) %>%
  kable(format = "markdown", align = "c", caption = "EM Regression Table (Emotion)")
regression_table_1

# r regression-model-motivation2, include=FALSE
EM_dataset_long$Emotion <- relevel(EM_dataset_long$Emotion, ref = "Neutral")
regmodel_2 <- lm(Score ~ Emotion*Measure + (1|ID), data = EM_dataset_long)

regression_results_2 <- tidy(regmodel_2)
regression_table_2 <- regression_results_2 %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate_if(is.numeric, function(x) sprintf("%.3f", x)) %>%
  kable(format = "markdown", align = "c", caption = "EM Regression Table (Emotion and Measure)")
regression_table_2
