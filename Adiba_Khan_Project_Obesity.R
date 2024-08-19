# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
df<- read.csv("Project_data_v2.csv")

# Filter the dataset for Obesity / Weight Status
df_obesity <- subset(df, Class == "Obesity / Weight Status")



# Recode blank and null values in Ageyears as 'Missing'
df_obesity <- df_obesity %>%
  mutate(Ageyears = ifelse(is.na(Ageyears) | Ageyears == "", 'Missing', Ageyears))

# Impute missing values in Data_Value using the mean for each Ageyears group
df_obesity <- df_obesity %>%
  group_by(Ageyears) %>%
  mutate(Data_Value = ifelse(is.na(Data_Value), mean(Data_Value, na.rm = TRUE), Data_Value)) %>%
  ungroup()




# Summary statistics
summary(df_obesity)

# Calculate mean and standard deviation of Data_Value by Ageyears
summary_stats <- df_obesity %>%
  group_by(Ageyears) %>%
  summarise(
    Mean = mean(Data_Value, na.rm = TRUE),
    Std_Dev = sd(Data_Value, na.rm = TRUE)
  )

# Display the results
print(summary_stats)
# Visualize the distribution of obesity percentages across different age groups
ggplot(df_obesity, aes(x = Ageyears, y = Data_Value, fill = Ageyears )) +
  geom_boxplot() +
  labs(title = "Obesity Percentage Distribution Across Age Groups", x = "Age Groups", y = "Obesity Percentage")

# Histogram of Data_Value
ggplot(df_obesity, aes(x = Data_Value)) +
  geom_histogram(binwidth = 5, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = "Histogram of Obesity Percentages", x = "Obesity Percentage", y = "Count")

# Density plot of Data_Value
ggplot(df_obesity, aes(x = Data_Value)) +
  geom_density(fill = 'blue', alpha = 0.7) +
  labs(title = "Density Plot of Obesity Percentages", x = "Obesity Percentage", y = "Density")

# Summary statistics
summary(df_obesity$Data_Value)

# Perform Shapiro-Wilk test for normality within each Ageyears group
shapiro_tests <- df_obesity %>%
  group_by(Ageyears) %>%
  summarise(shapiro_test = list(shapiro.test(Data_Value)))

# Extract and display Shapiro-Wilk test results
shapiro_results <- shapiro_tests %>%
  rowwise() %>%
  mutate(
    W_statistic = shapiro_test$statistic,
    p_value = shapiro_test$p.value
  ) %>%
  select(Ageyears, W_statistic, p_value)

# Display the results
print(shapiro_results)
# Perform Levene's test for homogeneity of variances
levene_test <- leveneTest(Data_Value ~ Ageyears, data = df_obesity)

# Display the results of the test
levene_test


# ANOVA
anova_model <- aov(Data_Value ~ Ageyears, data = df_obesity)
summary(anova_model)


# Perform Tukey HSD test
tukey_test <- TukeyHSD(anova_model)

# Display the results
print(tukey_test)