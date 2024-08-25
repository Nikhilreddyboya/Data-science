install.packages("readxl")
library(readxl)
df <- read_excel("Dataset_2024.xlsx")

head(df)

# Check the structure of the dataset
str(df)
# Generate summary statistics
summary(df)


# Check the column names
colnames(df)


# Plot pairwise relationships
pairs(df)

View(df)
print(str(df))

length(unique(df$`Age (years)`))
unique(df$`Age (years)`)

library(ggplot2)


# Boxplots
par(mfrow=c(2,3))  # Arrange plots in a grid
boxplot(df$`Body fat (%)` ~ cut(df$`Age (years)`, 4), main="Age vs Body fat (%)", xlab="Age (years)", ylab="Body fat (%)")
boxplot(df$`Body fat (%)` ~ cut(df$`Chest circumference (cm)`, 4), main="Chest circumference vs Body fat (%)", xlab="Chest circumference (cm)", ylab="Body fat (%)")
boxplot(df$`Body fat (%)` ~ cut(df$`Density (g/cm³)`, 4), main="Density vs Body fat (%)", xlab="Density (g/cm³)", ylab="Body fat (%)")
boxplot(df$`Body fat (%)` ~ cut(df$`Knee circumference (cm)`, 4), main="Knee circumference vs Body fat (%)", xlab="Knee circumference (cm)", ylab="Body fat (%)")
boxplot(df$`Body fat (%)` ~ cut(df$`Weight (lbs)`, 4), main="Weight vs Body fat (%)", xlab="Weight (lbs)", ylab="Body fat (%)")

# Check the column names in your dataset
colnames(df)


# Ensure there are no leading or trailing spaces in column names
colnames(df) <- trimws(colnames(df))

# Now calculate the correlation matrix using exact column names
correlation_matrix <- cor(df[, c("Age (years)", "Body fat (%)", "Chest circumference (cm)", "Density (g/cm³)", "Knee circumference (cm)", "Weight (lbs)")])

# Print correlation matrix
print(correlation_matrix)

# Interpretation of correlation matrix
cat("\nInterpretation of correlation matrix:\n")
cat("The correlation matrix shows the pairwise correlations between variables.\n")


install.packages("e1071")
library(e1071)
skew <- apply(df, 2, skewness)
print(skew)
cat("\nInterpretation of skewness:\n")
cat("Skewness measures the asymmetry of the data distribution.\n")
cat("A skewness value of 0 indicates a symmetric distribution.\n")
cat("Negative skewness (skewness < 0) indicates a left-skewed (long left tail) distribution.\n")
cat("Positive skewness (skewness > 0) indicates a right-skewed (long right tail) distribution.\n")


# Test of normality (Shapiro-Wilk test example)
shapiro_test <- apply(df, 2, shapiro.test)
print(shapiro_test)

# Example plot for normality check
par(mfrow=c(2,3))  # Arrange plots in a grid
hist(df$`Age (years)`, main="Age Histogram")
hist(df$`Body fat (%)`, main="Body fat (%) Histogram")
hist(df$`Chest circumference (cm)`, main="Chest circumference Histogram")
hist(df$`Density (g/cm³)`, main="Density Histogram")
hist(df$`Knee circumference (cm)`, main="Knee circumference Histogram")
hist(df$`Weight (lbs)`, main="Weight Histogram")

# Full model using lm()
full_model <- lm(`Body fat (%)` ~ `Age (years)` + `Chest circumference (cm)` + `Density (g/cm³)` + `Knee circumference (cm)` + `Weight (lbs)`, data=df)

# Print summary of the full model
summary(full_model)

# Variance Inflation Factor (VIF) to check multidisciplinary
vif_full <- car::vif(full_model)
print(vif_full)

# Final model selection (example with 3 variables)
final_model <- lm(`Body fat (%)` ~ `Age (years)` + `Chest circumference (cm)` + `Weight (lbs)`, data=df)
summary(final_model)

# Residual analysis to check model assumptions
plot(final_model, which=1)  # Residuals vs Fitted
plot(final_model, which=2)  # Normal Q-Q plot
plot(final_model, which=3)  # Scale-Location plot
plot(final_model, which=4)  # Residuals vs Leverage

# Example conclusion comments
cat("\nConclusion Comments:\n")
cat("Based on the analysis, the final model suggests that age, chest circumference, and weight are significant predictors of body fat percentage among men. The model assumptions were checked, and the conditions for residuals were largely met, indicating a valid model for predicting body fat percentage based on the provided data.")



       