# Install necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stats")
install.packages('car')

# Load the packages
library(ggplot2)
library(dplyr)
library(stats)
library(car)

# Load the dataset
depression_data <- read_excel("C:/Users/Student/Downloads/Depression.xlsx")



c_names <- gsub('[ /]','_',names(depression_data))

c_names <- gsub('[,.:]','',c_names)

names(depression_data) <- c_names


# Apply One-Hot Encoding to 'Age_Factor'

depression_data$Age_Factor <- as.factor(depression_data$Age)

depression_data <- depression_data %>%
  pivot_wider(names_prefix = "AgeGrp_", names_from = Age_Factor, values_from = Age_Factor,
              values_fn = function(x) 1, values_fill = list(Age_Factor = 0))

# One-Hot Encoding for 'Hours Spent Studying'
depression_data$Study_Hours <- as.factor(depression_data$`How_many_hours_do_you_spend_studying_each_day?`)
depression_data <- depression_data %>%
  pivot_wider(names_prefix = "Study_Hrs_", names_from = Study_Hours, values_from = Study_Hours, 
              values_fn = function(x) 1, values_fill = list(Study_Hours = 0))

# One-Hot Encoding for 'Hours on Social Media'
  depression_data$Social_Media_Hours <- as.factor(depression_data$`How_many_hours_do_you_spend_on_social_media_per_day?`)
depression_data <- depression_data %>%
  pivot_wider(names_prefix = "SocialMedia_Hrs_", names_from = Social_Media_Hours, values_from = Social_Media_Hours, 
              values_fn = function(x) 1, values_fill = list(Social_Media_Hours = 0))


# One-Hot Encoding for 'Number of Electronic Gadgets'
depression_data$Gadgets_Count <- as.factor(depression_data$`How_many_of_the_electronic_gadgets_(eg_mobile_phone_computer_laptop_PSP_PS4_Wii_etc)_do_you_have_in_your_home_or_your_student_accommodation_mess_hall?`)
depression_data <- depression_data %>%
  pivot_wider(names_prefix = "Gadgets_", names_from = Gadgets_Count, values_from = Gadgets_Count, 
              values_fn = function(x) 1, values_fill = list(Gadgets_Count = 0))



# Convert 'Last Semester GPA' to numeric
depression_data$Your_Last_Semester_GPA <- as.numeric(depression_data$Your_Last_Semester_GPA)

# first few rows of the modified dataframe
head(depression_data)


summary_stats <- summary(depression_data)


# Histogram for a continuous variable (e.g., GPA)
ggplot(depression_data, aes(x=Your_Last_Semester_GPA)) + 
  geom_histogram(binwidth = 0.8, fill="blue", color="black") +
  labs(title="Histogram of Last Semester GPA")

# Bar plot for a categorical variable 
ggplot(depression_data, aes(x=Gender)) +
  geom_bar(fill="blue") +
  labs(title="Bar Plot of Gender Distribution")

# Discrete and Continuous Probability Distributions
ggplot(depression_data, aes(x=Your_Last_Semester_GPA)) + 
  geom_density(fill="blue") +
  labs(title="Density Plot of Your_Last_Semester_GPA")





# Additional Analysis: Discrete Probability Distribution 
table_gadgets <- table(depression_data$`Gadgets_4 - 6`)
barplot(table_gadgets, main = "Bar Plot of Gadgets (4 - 6)",col = 'blue')


table_gadgets <- table(depression_data$`Gadgets_1 - 3`)
barplot(table_gadgets, main = "Bar Plot of Gadgets (1 - 3)",col = 'blue')


table_gadgets <- table(depression_data$`Gadgets_More than 6`)
barplot(table_gadgets, main = "Bar Plot of Gadgets (More than 6)",col = 'blue')


table_gadgets <- table(depression_data$`Gadgets_None`)
barplot(table_gadgets, main = "Bar Plot of Gadgets (None)",col = 'blue')

# Additional Analysis: Sampling Distributions 

n_samples <- 1000  # Number of samples to generate
sample_means <- numeric(n_samples)

for (i in 1:n_samples) {
  sample_data <- sample(depression_data$Your_Last_Semester_GPA, replace = TRUE, size = length(depression_data$Your_Last_Semester_GPA))
  sample_means[i] <- mean(sample_data)
}

# Plot the sampling distribution of the mean
hist(sample_means, main = "Sampling Distribution of GPA Mean",col = 'blue')

# Additional Analysis: Assumption Checks 
# Check for normality assumption (Shapiro-Wilk test)
shapiro_test_gpa <- shapiro.test(depression_data$Your_Last_Semester_GPA)
shapiro_test_residuals <- shapiro.test(multiple_regression_model$residuals)

# Check for equal variance assumption (Levene's test)
# Convert 'Gender' to factor
depression_data$Gender <- as.factor(depression_data$Gender)
levene_test <- leveneTest(Feeling_down_depressed_or_hopeless ~ Gender, data = depression_data)


# Sampling Distributions
mean_gpa <- mean(depression_data$Your_Last_Semester_GPA)
sd_gpa <- sd(depression_data$Your_Last_Semester_GPA)

# Statistical Inference: Hypothesis Testing
t_test_result <- t.test(Your_Last_Semester_GPA ~ Gender, data=depression_data)

# Check for Normality
shapiro_test_gpa <- shapiro.test(depression_data$Your_Last_Semester_GPA)

# Non-Parametric Test (if normality assumption is violated)
kruskal_test <- kruskal.test(Your_Last_Semester_GPA ~ Gender, data=depression_data)

# Perform the Wilcoxon Rank Sum Test (Mann-Whitney U Test) for GPA between Male and Female
wilcox_test <- wilcox.test(Your_Last_Semester_GPA ~ Gender, data = depression_data)

# Perform Fisher's Exact Test
fisher_test_result <- fisher.test(depression_data$Gender, depression_data$Educational_Level)

# Perform the Wilcoxon Signed-Rank Test for a paired comparison 
wilcox_signed_rank_test <- wilcox.test(depression_data$Feeling_bad_about_yourself_or_that_you_are_a_failure_or_not_have_let_yourself_or_your_family_down, depression_data$Your_Last_Semester_GPA, paired = TRUE)


# Regression Analysis
# Multiple Linear Regression
multiple_regression_model <- lm(Feeling_down_depressed_or_hopeless ~ depression_data$`AgeGrp_19 to 24 years` + depression_data$`AgeGrp_18 years or less` + depression_data$`AgeGrp_25 years and above` + Gender + Your_Last_Semester_GPA + depression_data$`Study_Hrs_2 - 4 hours` + depression_data$`Study_Hrs_1 - 2 hours`, data = depression_data)
multiple_regression_summary <- summary(multiple_regression_model)


# Plotting the residuals
residuals <- resid(multiple_regression_model)
plot(residuals, main = "Residuals Plot", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red") 


# ANOVA Test
anova_result <- aov(Feeling_down_depressed_or_hopeless ~ Educational_Level, data = depression_data)
anova_summary <- summary(anova_result)


# Output results
print(summary_stats)
print(t_test_result)
print(shapiro_test_gpa)
print(kruskal_test)
print(multiple_regression_summary)
print(anova_summary)
print(shapiro_test_gpa)
print(shapiro_test_residuals)
print(levene_test)
print(wilcox_test)
print(fisher_test_result)
print(wilcox_signed_rank_test)

