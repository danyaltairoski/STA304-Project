library(coefplot) 
library(ggstats) 
library(ggplot2) 
library(dplyr) 
library(readxl)
#import sample data change work space 
setwd("C:/Users/10713/Desktop/304GW R") 
Dataset <- read_excel("Dataset.xlsx") 
Tidy_data <- attach(Dataset)
########################### Hypothesis 1 Mann-Whitney Test 
df <- data.frame(
as = Tidy_data$academic_satisfaction,
mode = Tidy_data$mode_of_learning )
withOls_df <- filter(df, mode == 1) withoutOls = c(2,4,4)
data <- data.frame(
  group = rep(c("withoutOls", "withOls"),
              times=c(length(withOls_df$as), length(withoutOls))), value = c(withOls_df$as, withoutOls)
)
wilcox.test(value ~ group, data=data)
#define groups for boxplot
engage <- Dataset$academic_satisfaction[Dataset$mode_of_learning == 2]
engage_OLR <- Dataset$academic_satisfaction[Dataset$mode_of_learning == 1]
# Create a boxplot
boxplot(engage, engage_OLR, names = c("Engagement-Study", "Engagement-study w/ OLR"),
        main = "Comparing Academic Satisfaction: Online Resources vs. Traditional Learning",
        ylab = "Academic Satisfaction", col = c("green", "orange"))
##########################
#set Likert level 
likert_level <- c(
"1",
"2",
"3",
"4",
"5"
)
#copy to new variable with level
df_all <- tibble(academic_satisfaction, recording_impact, annotate_impact, piazza_impact, online_understanding) %>%
  mutate(across(everything(), ~ factor(.x, levels = likert_level , labels = c("Strongly disagree",
                                                                              "Disagree", "Neither disagree nor agree",
                                                                              "Agree",
                                                                              "Strongly agree"))))

#generate centered bar plot with all data 
gglikert(df_all, variable_labels = c(
academic_satisfaction = "Academic satisfaction", recording_impact = "Recording impact", annotate_impact = "Annotate impact", piazza_impact = "Piazza impact", online_understanding = "Online understanding"
))
#multiple linear regression model Hypothesis 2
model <- lm(academic_satisfaction ~ recording_impact + annotate_impact + piazza_impact, data = Dataset) summary(model)
# Diagnostic plots, set to 2x2 grid 
par(mfrow = c(2, 2))
# 1. Residuals vs. Fitted 
plot(model, which = 1)
# 2. Q-Q Plot 
plot(model, which = 2)
# 3. Scale-Location Plot 
plot(model, which = 3)
# 4. Residuals vs. Leverage Plot 
plot(model, which = 5)
# set back to 1x1 grid 
par(mfrow = c(1, 1))
#use coefficient plot to visualize multiple linear regression slope 
coefplot(model, intercept = FALSE, title = "Coefficient Plot",
xlab = "Coefficient value",
ylab = "",
newNames=c(academic_satisfaction = "Academic satisfaction",
           recording_impact = "Recording impact", annotate_impact = "Annotate impact", piazza_impact = "Piazza impact"
))
