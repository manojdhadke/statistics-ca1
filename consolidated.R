## Question 1

# Dataset Source: https://www.kaggle.com/datasets/rkiattisak/traveler-trip-data
library(help = "graphics")
data <- read.csv("Travel details dataset.csv")
###################################################################################################
## (a) Describe the dataset using appropriate plots/curves/charts
plot(data)
par(mfrow=c(3,3), mar=c(2,5,2,1), las=1, bty="n")
# scatter plot
plot(data$Duration..days., data$Traveler.age, type= "h", xlab = 'Duration Days', ylab = 'Age of Traveller', main = 'Duration Vs Age', col = 'green')

# Vertical bar plot
barplot(data$Duration..days., main = 'Duration',xlab = 'Duration', col='red',horiz = FALSE)

# Horizontal bar plot
barplot(data$Traveler.age, main = 'Age',xlab = 'Age', col= 'green',horiz = TRUE)

# Histogram
hist(data$Accommodation.cost, main = 'Accommodation Cost',xlab = 'Cost', col='red')

# Box plots
boxplot(data$Transportation.cost, main = "Transportation Cost")
boxplot(data$Accommodation.cost, main = "Accommodation Cost")

###################################################################################################
## (b) Consider one of continuous attributes, and compute central and variational measures. (8)
age <- data$Traveler.age;
print(age)
summary(age)
mean_data <- mean(age,na.rm = T) 
# mean(age,0.10,na.rm = T)
median_data <- median(age,na.rm = T)

#Mode
calculate_mode <- function(v) {
  uniq_value <- unique(v)
  uniq_value[which.max(tabulate(match(v, uniq_value)))]
}
mode_data <- calculate_mode(age)

print(paste("Central Measures:"))
print(paste("Mean:", mean_data))
print(paste("Median:", median_data))
print(paste("Mode:", mode_data))

# Variational measures
min_data <- min(age)
max_data <- max(age)
age_range <- range(age,na.rm = T)
var_data <- var(age,na.rm = T)
sd_data <- sd(age)
cv_data <- (sd_data/mean_data) * 100
iqr_data <- IQR(age,na.rm = T)
coefficient_Variation <- sd(age)/mean(age)

print(paste("Variational Measures:"))
print(paste("Range:", age_range))
print(paste("Min:", max_data))
print(paste("Max:", min_data))
print(paste("IQR:", iqr_data))
print(paste("Variance:", var_data))
print(paste("Standard Deviation:", sd_data))
print(paste("Coefficient of Variation:", coefficient_Variation))

###################################################################################################
## (c) For a particular variable of the dataset, use Chebyshev's rule, and propose one-sigma interval. Based on your proposed interval, specify the outliers if any.
# print(data$Traveler.age)
# range(data$Traveler.age)
# diff(range(data$Traveler.age))
# mean(range(data$Traveler.age))
# var(data$Traveler.age)
# sd(data$Traveler.age)
# k <- seq(1:5)
# Cheb <- sapply(k, function(k) 1-1/k^2)
# data.frame(k, Cheb)

traveler_age_mean <- mean(data$Traveler.age)
traveler_age_sd <- sd(data$Traveler.age)

k <- 2 # Can adjust the value of k as needed
prop_within_k_sd <- 1 - 1/k^2
print(prop_within_k_sd)
data.frame(k, prop_within_k_sd)
one_sigma_lower <- traveler_age_mean - k * traveler_age_sd
one_sigma_upper <- traveler_age_mean + k * traveler_age_sd


cat("One-sigma interval:", one_sigma_lower, " to ", one_sigma_upper, "\n")
# Identify outliers as any data points that fall outside the one-sigma interval
outliers <- data$Traveler.age < one_sigma_lower | data$Traveler.age > one_sigma_upper

# Printing total number of outliers
cat("Number of outliers:", sum(outliers), "\n")
# Printing the outliers
cat("Outliers list:", which(outliers), "\n")

###################################################################################################
## (d) Explain how the box-plot technique can be used to detect outliers. Apply this technique for one attribute of the dataset create detect outlier function

is_outlier <- function(value) {
  
  # Calculating quartiles, IQR
  Q1 <- quantile(value, probs=.25)
  Q3 <- quantile(value, probs=.75)
  
  IQR = Q3-Q1
  
  # Returns true or false based on condition
  return(value > Q3 + (IQR*1.5) | value < Q1 - (IQR*1.5))
}

# Function to get data back with removed outliers
get_removed_outlier_data <- function(data, columns=names(data)) {
  data <- data[!is_outlier(data[[columns[1]]]), ]
  print("Removed outliers successfully!")
  return(data)
}

data <- get_removed_outlier_data(data, c('Transportation.cost'))
data <- get_removed_outlier_data(data, c('Accommodation.cost'))
# Box plots
boxplot(data$Transportation.cost, main = "Transportation Cost without Outliers")
boxplot(data$Accommodation.cost, main = "Accommodation Cost without Outliers")

###################################################################################################

## Question 2
install.packages("STAT")
install.packages(MASS)

library(ggplot2)
# Dataset Source: https://www.kaggle.com/datasets/rkiattisak/traveler-trip-data
data <- read.csv("Travel details dataset.csv")

###################################################################################################
# a) Select four variables of the dataset, and propose an appropriate probability model to quantify uncertainty of each variable.

#Normal distribution
age <- data$Traveler.age
age_mean_data <- mean(age,na.rm = T)
age_sd_data <- sd(age)

#Normal distribution: The probability density function: dnorm
norm_pdf <- dnorm(age, mean = age_mean_data, sd = age_sd_data)
norm_data <- data.frame("AGE" = age, "Density" = norm_pdf)
ggplot(norm_data, aes(x = age, y = Density)) + geom_point()

#Normal distribution: The cumulative density function: pnorm
norm_cdf <- pnorm(age, age_mean_data, age_sd_data)
norm_data <- cbind(norm_data, "CDF_LowerTail" = norm_cdf)
cost <- data$Accommodation.cost
ggplot(norm_data, aes(x = age, y = cost)) + geom_point()

#Normal distribution: The quantile function: qnorm
prob.range <- seq(0, 1, 0.001)
qnorm_data <- data.frame("Probability" = prob.range, "Age" = qnorm(prob.range, age_mean_data, age_sd_data))
ggplot(qnorm_data, aes(x = Probability, y = Age)) + geom_point()

#Normal distribution: The random sampling function: rnorm
norm_samples <- c(100, 1000, 10000)
norm_dataframe <- do.call(rbind, lapply(norm_samples, function(x) data.frame("samples" = x, "Age" = rnorm(x, age_mean_data, age_sd_data))))
# show one facet per random sample of a given size
ggplot() + geom_histogram(data = norm_dataframe, aes(x = Age)) + facet_wrap(.~samples, scales = "free_y")


#Bernoulli model
data$Traveler.gender <- ifelse(data$Traveler.gender == "male", 0, 1)
gen_model <- glm(data$Traveler.gender ~ 1, data = data, family = binomial())
summary(gen_model)


#Binomial model
Nationality <- data$Traveler.nationality
data$Accommodation.type <- ifelse(data$Accommodation.type == "Hotel", 1, 0)
Accommodation <- data$Accommodation.type
Destination <- data$Destination
Age <- data$Traveler.age
model <- glm(Accommodation ~ Nationality + Age + Destination, data = data, family = binomial())
summary(model)


# Multinomial model
# Load the nnet package
library(nnet)
Accommodation <- data$Accommodation.type
Gender <- data$Traveler.gender
Destination <- data$Destination
Age <- data$Traveler.age
dest_model <- multinom(Destination ~ Age + Gender + Accommodation, data = data)
summary(dest_model)

# Poisson model
Cost <- data$Transportation.cost
cost_model <- glm(Cost ~ Age + Destination, data = data, family = poisson())
summary(cost_model)

# Geometric model to the Number of Previous Visits variable
library(STAT)
library(MASS)
Accommodation <- data$Accommodation.type
Gender <- data$Traveler.gender
Destination <- data$Destination
Age <- data$Traveler.age
# Binomial distribution with a logit link function, which is equivalent to a geometric distribution.
acco_model <- glm(Accommodation ~ Age + Gender + Destination, data = data, family = binomial(link = "logit"))
summary(acco_model)                       

###################################################################################################


# b) For each model in part (a), estimate the parameters of model.

#Normal distribution:
days <- data$Duration..days

days_mu <- mean(days)
days_sigma <- sd(days)
cat("Estimated mean:", days_mu, "\n")
cat("Estimated standard deviation:", days_sigma, "\n")

#Bernoulli model
data$Duration..days <- ifelse(data$Duration..days > 10, 5, 0)
days <- data$Duration..days
days_prob <- mean(days)
cat("Estimated probability of a long stay:", days_prob, "\n")


#Binomial model
data$Duration..days <- ifelse(data$Duration..days > 10, 5, 0)
days <- data$Duration..days
Trips <- 1
model <- glm(cbind(days, Trips - days) ~ Destination, data = data, family = "binomial")
summary(model)


# Multinomial model
# Load the nnet package
library(nnet)
Accommodation <- data$Accommodation.type
# Define the outcome variable as a factor with three levels
StayLength <- cut(jitter(data$Duration..days), breaks = quantile(jitter(data$Duration..days), probs = c(0, 0.33, 0.66, 1)), labels = c("short", "medium", "long"))
print(StayLength)
dest_model <- multinom(StayLength ~ Accommodation, data = data)
summary(dest_model)


# Poisson model
Cost <- data$Transportation.cost
data$Traveler.gender <- ifelse(data$Traveler.gender == "male", 0, 1)
Gender <- data$Traveler.gender
poi_model <- glm(Gender ~ Cost, data = data, family = poisson)
summary(poi_model)
intercept <- coef(poi_model)[1]
avg_rate <- exp(intercept)
cat("Gender who travels most", round(avg_rate, 2))

###################################################################################################

# c) Express the way in which each model can be used for the predictive analytics, then find the prediction for each attribute.

#Bernoulli model
#Example: Suppose we want to predict the gender of a traveler based on their age, nationality, and destination. 
Gender <- ifelse(data$Traveler.gender == "male", 0, 1)
Destination <- data$Destination
Age <- data$Traveler.age
Nationality <- data$Traveler.nationality
gender_model <- glm(Gender ~ Age + Nationality + Destination, data = data, family = binomial())
gender_prediction <- predict(gender_model, type = "response")
print(gender_prediction)

# Binomial model:
# Example: To predict the accommodation type for each traveler in the dataset based on their nationality, age, and destination
Nationality <- data$Traveler.nationality
Age <- data$Traveler.age
Destination <- data$Destination
Accommodation <- data$Accommodation.type
accommodation_model <- glm(Accommodation ~ Nationality + Age + Destination, data = data, family = binomial())
accommodation_prediction <- predict(accommodation_model, type = "response")
print(accommodation_prediction)

# Multinomial model:
# Example: Suppose we want to predict the Destination of a traveler's trip based on their age, gender, and Accommodation 
# Loading the nnet package
library(nnet)
Accommodation <- data$Accommodation.type
Gender <- data$Traveler.gender
Destination <- data$Destination
Age <- data$Traveler.age
dest_model <- multinom(Destination ~ Age + Gender + Accommodation, data = data)
dest_model_purpose <- predict(dest_model, type = "class")
print(dest_model_purpose)

# Poisson model: 
# Example: Suppose we want to predict the cost of visit to a certain destination in a year based on their age and nationality.
Cost <- data$Transportation.cost
cost_model <- glm(Cost ~ Age + Destination, data = data, family = poisson())
cost_prediction <- predict(visit_model, type = "response")
print(cost_prediction)

###################################################################################################
## Question 3
###################################################################################################
# (a) Consider two categorical variables of the dataset, develop a binary decision
# making strategy to check whether two variables are independent at the significant level alpha=0.01

# Dataset Source: https://www.kaggle.com/datasets/rkiattisak/traveler-trip-data
library(help = "graphics")
data <- read.csv("Travel details dataset.csv")

##	i). Stating the hypotheses
# Null hypothesis : Two categorical variables selected, Traveler gender and Accommodation type are independent 
# at the significant level alpha=0.01
# Alternative hypothesis :Two categorical variables selected, Traveler gender and Accommodation type are dependent

# ii). Find statistic and critical values

# Chi-square test is the best method to define the statistical relationship between two categorical variables, ie whether they are independent or not
# First calculating chi-square statistic and compare it to the critical value from the chi-square distribution

# Contingency table of Traveler gender and Accommodation type from data frame data
vGender <- data$Traveler.gender
vAccType <- data$Accommodation.type
c_table <- table(vGender,vAccType)
cat("Contingency Table:", c_table, "\n")

# chi-square test against c_table 
chsq_test <- chisq.test(c_table)


# Finding chi-square statistic and p-value.P-value is the area under the density curve of chi-square distribution
# to the right of the value of the test statistic.P means the probability, for a given statistical model that, when the null hypothesis is true, the statistical summary would be equal to or more extreme than the actual observed results

chsq_statistic <- chsq_test$statistic
p_value <- chsq_test$p.value

cat("chi-square statistic:", chsq_statistic, "\n")
cat("p_value:", p_value, "\n")

# Finding critical value. In hypothesis tests, Critical value implies boundary of how extreme 
# a test statistic we need to reject the Null hypothesis. 
# To determine this we need to have Number of degrees of freedom,Number and type of Tails,The level of significance

rows <- nrow(c_table)
cols <- ncol(c_table)
df <- (rows - 1) * (cols - 1)
critical_value <- qchisq(0.99, df)
cat("critical_value:", critical_value, "\n")

# iii).	Explain your decision and Interpret results

# Here the chi-square Statistic is 7.113908 , Probability or P_Value is 0.417117  and Critical value is 18.47531 
# Decision making is based on that if chi-square statistic is greater than critical value then we have to reject Null hypothesis 
# Here we do not require to reject the Null hypothesis since the values obtained are justifying our Null hypothesis.
# So the conclusion is the Selected two categorical variables are independent at the 0.01 level of significance. The p-value provides additional evidence for the decision.
# ie  p-value(0.417117) is greater than the level of significance (0.01), and it supports our null hypothesis.

if (chsq_statistic > critical_value) {
  cat(" The two categorical variables,Traveler gender and Accommodation type are not independent thus rejecting Null Hypothesis\n")
} else {
  cat("The two categorical variables,Traveler gender and Accommodation type are independent at the 0.01 level of significance\n")
}


###################################################################################################
# (b) Consider one categorical variable, apply goodness of fit test to evaluate whether a 
# candidate set of probabilities can be appropriate to quantify the uncertainty of class frequency
# at the significant level alpha=0.05 

# Null hypothesis: The observed class frequencies fit the candidate set of probabilities.
# Alternative hypothesis: The observed class frequencies do not fit the candidate set of probabilities.
# Calculating chi-square statistic and comparing it with critical value from the chi-square distribution with (k - 1) degrees of freedom, 
# where k is the number of categories in the categorical variable, at the alpha level of significance 0.05.
# If the chi-square statistic is greater than the critical value, we reject the null hypothesis. 

# Calculate the observed frequencies
obs_freq <- table(data$Transportation.type)

# Set the candidate set of probabilities
p <- c(0.5, 0.3, 0.1, 0.1)

# Calculate the expected frequencies
exp_freq <- sum(obs_freq) * p

# Calculate the chi-squared test statistic
chi_sq <- sum((obs_freq - exp_freq)^2 / exp_freq)

# Calculate the degrees of freedom
df <- length(p) - 1

# Calculate the p-value
p_value <- 1 - pchisq(chi_sq, df)

# Compare the p-value to the significance level alpha
alpha <- 0.05
if (p_value < alpha) {
  print("Reject null hypothesis")
} else {
  print("Fail to reject null hypothesis")
}
###################################################################################################
# (c) Consider one continuous variable in the dataset, and apply test of mean for a proposed candidate of m at the significant level alpha=0.05.

age <- data$Traveler.age
# Set the proposed candidate mean
mue_value <- 40

# Calculate the sample mean and sample standard deviation
age_mean <- mean(age)
age_sd <- sd(age)

# Calculate the t-test statistic
t_stat <- (age_mean - mue_value) / (age_sd / sqrt(length(age)))

# Calculate the degrees of freedom
freedom <- length(age) - 1

# Calculate the p-value
p_value <- 2 * pt(abs(t_stat), freedom, lower.tail = FALSE)

# Comparing the p-value to the significance level alpha as per question
alpha <- 0.05
if (p_value < alpha) {
  print("Comment: Rejecting null hypothesis")
} else {
  print("Comment: Can not reject null hypothesis")
}
###################################################################################################