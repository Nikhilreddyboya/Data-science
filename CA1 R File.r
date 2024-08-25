
library(readxl)


# Correct file path format for R
file_path <- "C:/Users/User/Downloads/Dataset_2024 (1).xlsx"


# Load the dataset
data <- read_excel(file_path)


# View the first few rows of the dataset
head(data)

str(data)

summary(data)

#Boxplot

library(ggplot2)

ggplot(data, aes(x=factor(1), y=`Age (years)`)) +
  geom_boxplot() +
  labs(title="Boxplot of Age (years)",
       x="Visual Aids",
       y="Age (years)") +
  theme_minimal()

ggplot(data, aes(x=factor(1), y=`Body fat (%)`)) +
  geom_boxplot() +
  labs(title="Boxplot of Body fat (%)",
       x="Visual Aids",
       y="Body fat (%)") +
  theme_minimal()

ggplot(data, aes(x=factor(1), y=`Chest circumference (cm)`)) +
  geom_boxplot() +
  labs(title="Boxplot of Chest circumference (cm)",
       x="Visual Aids",
       y="Chest circumference (cm)") +
  theme_minimal()

ggplot(data, aes(x=factor(1), y=`Density (g/cm³)`)) +
  geom_boxplot() +
  labs(title="Boxplot of Density (g/cm³)",
       x="Visual Aids",
       y="Density (g/cm³)") +
  theme_minimal()


ggplot(data, aes(x=factor(1), y=`Knee circumference (cm)`)) +
  geom_boxplot() +
  labs(title="Boxplot of Knee circumference (cm)",
       x="Visual Aids",
       y="Knee circumference (cm)") +
  theme_minimal()

ggplot(data, aes(x=factor(1), y=`Weight (lbs)`)) +
  geom_boxplot() +
  labs(title="Boxplot of Weight (lbs)",
       x="Visual Aids",
       y="Weight (lbs)") +
  theme_minimal()


mean(data$`Body fat (%)`)
median(data$`Body fat (%)`)
skewness <- function(x) {
  n <- length(x)
  m3 <- sum((x - mean(x))^3) / n
  s3 <- (sum((x - mean(x))^2) / n)^(3/2)
  m3 / s3
}
skewness(data$`Body fat (%)`)

shapiro.test(data$`Body fat (%)`)


# Descriptive statistics and confidence interval for Body fat (%)
mean <- mean(data$`Body fat (%)`)
sd <- sd(data$`Body fat (%)`)
n <- length(data$`Body fat (%)`)
error <- qt(0.975, df=n-1) * sd / sqrt(n)  # Calculate standard error
lower <- mean - error  # Lower bound of confidence interval
upper <- mean + error  # Upper bound of confidence interval

# Display mean, standard deviation, and confidence interval
list(mean=mean, sd=sd, lower=lower, upper=upper)



# Example data
df <- data.frame(
  student = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
  without_aids = c(50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61),
  with_aids = c(58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70))



t_test_result <- t.test(df$without_aids, df$with_aids, paired=TRUE)
print(t_test_result)

# Print the t-test result
print(t_test_result)

# Extract relevant information
p_value <- t_test_result$p.value
confidence_interval <- t_test_result$conf.int

# Interpret confidence interval
cat("\nConfidence Interval of the Difference (95%):", confidence_interval, "\n\n")

# Make conclusion based on p-value
if (p_value < 0.05) {
  cat("Based on the paired t-test, there is a statistically significant difference in lecture quality between using visual aids and not using visual aids.")
} else {
  cat("Based on the paired t-test, there is no statistically significant difference in lecture quality between using visual aids and not using visual aids.")
}

