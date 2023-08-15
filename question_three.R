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
# (c) Consider one continuous variable in the dataset, and apply test of mean for a proposed candidate of 𝜇 at the significant level alpha=0.05.

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