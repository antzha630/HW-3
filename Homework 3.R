
library(ggplot2)
library(tidyverse)
library(mosaic)

# Problem 1: 
# Theory A: Gas stations charge more if they lack direct competition in sight.
gas <- read.csv('gasprices.csv')
ggplot(gas, aes(x = Competitors, y = Price)) +
  geom_boxplot() + 
  labs(title = "Gas Prices by Competitor Availability",
       x = "Competitors in Sight",
       y = "Price of Gas") +
  theme_minimal()
boot_comp <- do(1000) * (diff(mean(Price ~ Competitors, data = resample(gas))))
ggplot(boot_comp, aes(x = Y)) + 
  geom_histogram(fill = "blue", color = "black") +
  labs(
    title = "Bootstrap Histogram of Price Difference by Competitor Availability",
    x = "Bootstrapped Mean Difference in Price",
    y = "Frequency"
  ) +
  theme_minimal()
confint(boot_comp, level = 0.95)
# Theory B: The richer the area, the higher the gas prices.
plot(gas$Income, gas$Price, 
     xlab = "Median Household Income", 
     ylab = "Price of Gas", 
     main = "Scatterplot Between Median Household Income and Price of Gas",
     col = "blue", pch = 20)  
abline(lm(Price ~ Income, data = gas), col = "red", lwd = 2)
round(cor(gas$Income, gas$Price),digits = 2)
boot_income <- do(1000) * cor(Price ~ Income, data = resample(gas))
ggplot(boot_income) + 
  geom_histogram(aes(x = cor), binwidth = 0.01) +
  labs(
    title = "Bootstrap Histogram of Correlation Between Income and Gas Prices",
    x = "Bootstrapped Correlation Coefficient",
    y = "Frequency"
  ) +
  theme_minimal()
confint(boot_income, level = 0.95)
# Theory C : Gas stations at stoplights charge more.
ggplot(gas, aes(x = Stoplight, y = Price)) +
  geom_boxplot() + 
  labs(title = "Gas Prices by Stoplight Availability",
       x = "Stoplight in Sight",
       y = "Price of Gas") +
  theme_minimal()
boot_stoplight <- do(1000) * (diff(mean(Price ~ Stoplight, data = resample(gas))))
ggplot(boot_stoplight) + 
  geom_histogram(aes(x = Y)) +
  labs(
    title = "Bootstrap Histogram of Price Difference by Stoplight Availability",
    x = "Bootstrapped Mean Difference in Price",
    y = "Frequency"
  ) +
  theme_minimal()
confint(boot_stoplight, level = 0.95)
# Theory D: Highways
ggplot(gas, aes(x = Highway, y = Price)) +
  geom_boxplot() + 
  labs(title = "Gas Prices by Highway Availability",
       x = "Highways in Sight",
       y = "Price of Gas") +
  theme_minimal()
boot_highway <- do(1000) * (diff(mean(Price ~ Highway, data = resample(gas))))
ggplot(boot_highway) + 
  geom_histogram(aes(x = Y)) + 
  labs(
    title = "Bootstrap Histogram of Price Difference by Highway Availability",
    x = "Bootstrapped Mean Difference in Price",
    y = "Frequency"
  ) +
  theme_minimal()
confint(boot_highway, level = 0.95)
# Theory E: Shell vs Other Brands
ggplot(gas, aes(x = Name == "Shell", y = Price, fill = Name == "Shell")) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Non-Shell", "Shell")) +
  labs(
    title = "Gas Prices by Shell and Non-Shell",
    x = "Gas Station Type",
    y = "Price of Gas",
    fill = "Gas Station"
  ) +
  theme_minimal()
boot_shell <- do(1000) * {
  data <- resample(gas)
  mean_shell <- mean(data$Price[data$Name == "Shell"])
  mean_other <- mean(data$Price[data$Name != "Shell"])
  mean_shell - mean_other
}
ggplot(boot_shell) + 
  geom_histogram(aes(x = result), binwidth = 0.01) +
  labs(
    title = "Bootstrap Histogram of Price Difference by Shell Company or Not",
    x = "Bootstrapped Mean Difference in Price",
    y = "Frequency"
  ) +
  theme_minimal()
confint(boot_shell, level = 0.95)

# Problem 2 Mercedes S Class Cars
#Part A
sclass <- read.csv('sclass.csv')
filtered_one <- subset(sclass, year == 2011 & trim == "63 AMG")
boot_mileage <- do(1000) * mean(~mileage, data = resample(filtered_one))
ggplot(boot_mileage) + 
  geom_histogram(aes(x = mean))
confint(boot_mileage, level = 0.95)

#Part B
filtered_two <- subset(sclass, year == 2014 & trim == "550")
filtered_two$isBlack <- ifelse(filtered_two$color == "Black", TRUE, FALSE)
boot_black <- do(1000) * prop(~isBlack, data = resample(filtered_two))
head(boot_black)
ggplot(boot_black) + 
  geom_histogram(aes(x = prop_TRUE), binwidth = 0.01)
confint(boot_black, level = 0.95)

# Problem 3 
# PART A
nbc_data <- read.csv("nbc_pilotsurvey.csv")
filtered_three <- subset(nbc_data, Show == "Living with Ed" | Show == "My Name is Earl")
boot_happy <- do(1000) * {
  diff_mean <- diff(mean(Q1_Happy ~ Show, data = resample(filtered_three)))
  data.frame(diff_means = diff_mean)
}
confint(boot_happy, level = 0.95)
# PART B
filtered_four <- subset(nbc_data, Show == "The Biggest Loser" | Show == "The Apprentice: Los Angeles")
boot_annoyed <- do(1000) * {
  diff_mean <- diff(mean(Q1_Annoyed ~ Show, data = resample(filtered_four)))
  data.frame(diff_means = diff_mean)
}
confint(boot_annoyed, level = 0.95)
# PART C
filtered_five <- subset(nbc_data, Show == "Dancing with the Stars")
filtered_five$isConfusing <- ifelse(filtered_five$Q2_Confusing >= 4, TRUE, FALSE)
boot_confusing <- do(1000) * prop(~isConfusing, data = resample(filtered_five))
confint(boot_confusing, level = 0.95)




# Problem 4 
ebay <- read.csv("ebay.csv")
ebay$rev_ratio <- ebay$rev_after / ebay$rev_before
boot_diff <- do(10000) * {
  diff_means <- diff(mean(rev_ratio ~ adwords_pause, data = resample(ebay)))
  data.frame(diff_means = diff_means)  
}
head(boot_diff)
ggplot(boot_diff, aes(x = diff_means)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Bootstrap Distribution of Revenue Ratio Difference",
       x = "Difference in Revenue Ratio",
       y = "Frequency")

confint(boot_diff, level = 0.95)

