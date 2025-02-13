
library(ggplot2)
library(tidyverse)
library(mosaic)

# Problem 1: 
# Theory A: Gas stations charge more if they lack direct competition in sight.
gas <- read.csv('gasprices.csv')
# boxplot of competitiors vs price
ggplot(gas, aes(x = Competitors, y = Price)) +
  geom_boxplot() + 
  labs(title = "Gas Prices by Competitor Availability",
       x = "Competitors in Sight",
       y = "Price of Gas") +
  theme_minimal()
# Bootstrap sampling for the difference in mean prices based on competitors
boot_comp <- do(1000) * (diff(mean(Price ~ Competitors, data = resample(gas))))
# histogram of the bootstrapped mean price differences
ggplot(boot_comp, aes(x = Y)) + 
  geom_histogram(fill = "blue", color = "black") +
  labs(
    title = "Bootstrap Histogram of Price Difference by Competitor Availability",
    x = "Bootstrapped Mean Difference in Price",
    y = "Frequency"
  ) +
  theme_minimal()
# confidence interval
confint(boot_comp, level = 0.95)
# Theory B: The richer the area, the higher the gas prices.
plot(gas$Income, gas$Price, 
     xlab = "Median Household Income", 
     ylab = "Price of Gas", 
     main = "Scatterplot Between Median Household Income and Price of Gas",
     col = "blue", pch = 20)  
# regression line
abline(lm(Price ~ Income, data = gas), col = "red", lwd = 2)
# correlation coefficient
round(cor(gas$Income, gas$Price),digits = 2)
# Bootstrap sampling for the correlation between income and gas prices
boot_income <- do(1000) * cor(Price ~ Income, data = resample(gas))
# histogram of the bootstrapped correlation coefficients
ggplot(boot_income) + 
  geom_histogram(aes(x = cor), binwidth = 0.01) +
  labs(
    title = "Bootstrap Histogram of Correlation Between Income and Gas Prices",
    x = "Bootstrapped Correlation Coefficient",
    y = "Frequency"
  ) +
  theme_minimal()
# confidence interval
confint(boot_income, level = 0.95)
# Theory C : Gas stations at stoplights charge more.
# boxplot of stoplight vs gas price
ggplot(gas, aes(x = Stoplight, y = Price)) +
  geom_boxplot() + 
  labs(title = "Gas Prices by Stoplight Availability",
       x = "Stoplight in Sight",
       y = "Price of Gas") +
  theme_minimal()
# Bootstrap sampling for the difference in mean prices by stoplight availability
boot_stoplight <- do(1000) * (diff(mean(Price ~ Stoplight, data = resample(gas))))
ggplot(boot_stoplight) + 
  geom_histogram(aes(x = Y)) +
  labs(
    title = "Bootstrap Histogram of Price Difference by Stoplight Availability",
    x = "Bootstrapped Mean Difference in Price",
    y = "Frequency"
  ) +
  theme_minimal()
# confidence interval
confint(boot_stoplight, level = 0.95)
# Theory D: Highways
# boxplot of highways vs gas price
ggplot(gas, aes(x = Highway, y = Price)) +
  geom_boxplot() + 
  labs(title = "Gas Prices by Highway Availability",
       x = "Highways in Sight",
       y = "Price of Gas") +
  theme_minimal()
# bootstrap sampling
boot_highway <- do(1000) * (diff(mean(Price ~ Highway, data = resample(gas))))
ggplot(boot_highway) + 
  geom_histogram(aes(x = Y)) + 
  labs(
    title = "Bootstrap Histogram of Price Difference by Highway Availability",
    x = "Bootstrapped Mean Difference in Price",
    y = "Frequency"
  ) +
  theme_minimal()
# confidence interval
confint(boot_highway, level = 0.95)
# Theory E: Shell vs Other Brands
# boxplot of shell and non shell vs gas prices
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
# bootstrap sampling
boot_shell <- do(1000) * {
  data <- resample(gas)
  # diff of means for shell and non shell
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
# confidence interval
confint(boot_shell, level = 0.95)






# Problem 2 Mercedes S Class Cars
#Part A
# filter only year 2011 and trim 63 AMG rows
sclass <- read.csv('sclass.csv')
filtered_one <- subset(sclass, year == 2011 & trim == "63 AMG")
# bootstrap sampling
boot_mileage <- do(1000) * mean(~mileage, data = resample(filtered_one))
# histogram of bootstrapped mean mileage
ggplot(boot_mileage) + 
  geom_histogram(aes(x = mean))
# confidence interval
confint(boot_mileage, level = 0.95)

#Part B
# filter with only year 2014 and trim 550
filtered_two <- subset(sclass, year == 2014 & trim == "550")
# new row that says if the color is blue means true and color not black is false
filtered_two$isBlack <- ifelse(filtered_two$color == "Black", TRUE, FALSE)
# bootstrap sampling
boot_black <- do(1000) * prop(~isBlack, data = resample(filtered_two))
ggplot(boot_black) + 
  geom_histogram(aes(x = prop_TRUE), binwidth = 0.01)
# confidence interval
confint(boot_black, level = 0.95)





# Problem 3 
# PART A
nbc_data <- read.csv("nbc_pilotsurvey.csv")
# filter for two shows
filtered_three <- subset(nbc_data, Show == "Living with Ed" | Show == "My Name is Earl")
# bootstrap sampling for diff in happiness
boot_happy <- do(1000) * {
  diff_mean <- diff(mean(Q1_Happy ~ Show, data = resample(filtered_three)))
  data.frame(diff_means = diff_mean)
}
# confidence interval happy
confint(boot_happy, level = 0.95)
# PART B
# filter so only these two shows
filtered_four <- subset(nbc_data, Show == "The Biggest Loser" | Show == "The Apprentice: Los Angeles")
# bootstrap sampling for diff in annoy
boot_annoyed <- do(1000) * {
  diff_mean <- diff(mean(Q1_Annoyed ~ Show, data = resample(filtered_four)))
  data.frame(diff_means = diff_mean)
}
#confidence interval
confint(boot_annoyed, level = 0.95)
# PART C
# filter only this one show
filtered_five <- subset(nbc_data, Show == "Dancing with the Stars")
# add a confusing column where it puts true if the score is 4 or up and false if not
filtered_five$isConfusing <- ifelse(filtered_five$Q2_Confusing >= 4, TRUE, FALSE)
# Bootstrap sampling for proportion
boot_confusing <- do(1000) * prop(~isConfusing, data = resample(filtered_five))
# confidence interval proportion confusing
confint(boot_confusing, level = 0.95)




# Problem 4 
ebay <- read.csv("ebay.csv")
# new column rev ratio which is just revenue after divided by revenue before
ebay$rev_ratio <- ebay$rev_after / ebay$rev_before
# bootstrap sampling for diff in mean revenue ratios between treatment and control groups
boot_diff <- do(10000) * {
  diff_means <- diff(mean(rev_ratio ~ adwords_pause, data = resample(ebay)))
  data.frame(diff_means = diff_means)  
}
ggplot(boot_diff, aes(x = diff_means)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Bootstrap Distribution of Revenue Ratio Difference",
       x = "Difference in Revenue Ratio",
       y = "Frequency")
# confidence interval for diff in mean revenue ratios between two groups
confint(boot_diff, level = 0.95)

