student_lifestyle_dataset <- read.csv("student_lifestyle_dataset.csv")

data <- student_lifestyle_dataset

# Select variables
SleepHours <- data$Sleep_Hours_Per_Day
GPA <- data$GPA


#  Summary Statistics    #MARITSIA 

cat("\n=== Sleep Summary ===\n")   #x_axis
print(summary(SleepHours))
print(paste("Variance:", var(SleepHours)))
print(paste("SD:", sd(SleepHours)))
print(paste("IQR:", IQR(SleepHours)))
sleep_range <- max(SleepHours) - min(SleepHours)
cat("Range of Sleep Hours:", round(sleep_range, 2), "\n")

cat("\n=== GPA Summary ===\n")    #y_axis
print(summary(GPA))
print(paste("Variance:", var(GPA)))
print(paste("SD:", sd(GPA)))
print(paste("IQR:", IQR(GPA)))
gpa_range <- max(GPA) - min(GPA)
cat("Range of GPA:", round(gpa_range, 2), "\n")

#  Graphs             #MARITSIA & Marly 

par(mfrow = c(2, 3))
hist(SleepHours, main="Histogram - Sleep", xlab="Hours")
boxplot(SleepHours, main="Boxplot - Sleep", ylab="Hours")
dotchart(SleepHours, main="Dotplot - Sleep", xlab="Hours")

hist(GPA, main="Histogram - GPA", xlab="GPA")
boxplot(GPA, main="Boxplot - GPA", ylab="GPA")
plot(SleepHours, GPA, main="GPA vs Sleep", xlab="Hours", ylab="GPA")
par(mfrow = c(1,1))

# Correlation Test      #LOGY

cat("\n=== Correlation Test ===\n")
cor_test <- cor.test(SleepHours, GPA)
print(cor_test)

#  (Regression Model)   #JANA

model <- lm(GPA ~ SleepHours, data = data)
summary(model)
plot(SleepHours, GPA, main="Regression Line")
abline(model, col="red", lwd=2)

#(Regression Assumptions) #TOKA 

residuals <- resid(model)

cat("\n=== Shapiro-Wilk Normality Test ===\n")
print(shapiro.test(residuals))

par(mfrow = c(1,2))
qqnorm(residuals, main="QQ Plot")
qqline(residuals, col="red", lwd=2)
plot(fitted(model), residuals, main="Residuals vs Fitted")
abline(h=0, col="blue")
par(mfrow = c(1,1))

# Inference using Sample Mean vs Population Mean Test    # Marly

# Select the variable and define the hypothesis value
GPA <- student_lifestyle_dataset$GPA
u_0 <- 3.0 # Hypothesized Population Mean (u)

# Calculate and display the Sample Mean
x_bar <- mean(GPA)
print(paste("Sample Mean (x-bar) :", round(x_bar, 4))) 

# Execute the Sample Mean Hypothesis Test
t_test_gpa <- t.test(GPA, mu = u_0, alternative = "two.sided")

# Display the Full Test Results 
print("R Output for Sample Means Hypothesis Test (u = 3.0)")
names(t_test_gpa$estimate) <- c("Sample Mean (x-bar)")
print(t_test_gpa)

