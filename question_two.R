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
