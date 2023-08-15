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
