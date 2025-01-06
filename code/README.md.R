#Load the Mass package
install.packages("MASS")
library(MASS)

#Load the Boston Housing Project data and check help page for details
?Boston
data("Boston")
View(Boston)

#Q1: Based on this data set, provide an estimate for the population mean of medv. Call this estimate μˆ.
#medv: median value of owner-occupied homes in $1000s.
#Population mean of medv
mu_hat <- mean(Boston$medv)
mu_hat
#Ans: 22.5328
#interpretation: The average median value of owner-occupied homes in the Boston area is approximately $22,532.80, 
#based on this sample. This value represents the central tendency of home values in the population.

#Q2:Provide an estimate of the standard error of μˆ. Interpret this result.
#Sample size
n <- nrow(Boston)
#Standard error of mu_hat(pop. mean of medv)
se_mu_hat <- sd(Boston$medv) / sqrt(n)
se_mu_hat
#Ans: 0.4089
#Interpretation: The standard error of 0.4089 indicates the expected variability in our estimate of the mean 
#if we were to repeatedly sample from the population. In other words, this is the average deviation of our sample mean from the true population mean.

#Q3:Now estimate the standard error of the sample mean μˆ using the bootstrap.
#How does this compare to your answer from (Q2)

# Steps:
# Resample the data with replacement.
# Compute the mean for each resample.
# Repeat this process many times (e.g., 1000 iterations).
# Calculate the standard deviation of the bootstrap sample means as the bootstrap estimate of SE.

# Set seed for reproducibility
set.seed(123)

# Number of bootstrap iterations
n_bootstrap <- 1000

# Bootstrap sampling
bootstrap_means <- replicate(n_bootstrap, {
  sample_data <- sample(Boston$medv, replace = TRUE)
  mean(sample_data)
})

# Bootstrap estimate of SE
bootstrap_se_mu_hat <- sd(bootstrap_means)
bootstrap_se_mu_hat
#Ans:0.4185
#Interpretation: The bootstrap estimate of the standard error of the mean is slightly larger than the analytical estimate (0.4089 vs. 0.4185). This small difference suggests that the formula-based and bootstrap-based methods are consistent, 
# but bootstrap often accounts for potential non-normality or sampling variability more robustly.


# Q4:Based on this data set, provide an estimate for the median value of medv 
# in the population. Call it as μˆmedian.
# Estimate for the population median
mu_hat_median <- median(Boston$medv)
mu_hat_median
#Ans: 21.2
# The median value of owner-occupied homes is $21,200, suggesting that 50% of the homes have a value below this amount,and 50% are above. 
# This is a robust measure of central tendency, especially when the data may be skewed.

# Q5: We now estimate the standard error of sample median μˆmedian. 
# Unfortunately, there is no simple formula for computing the standard error of the median. 
# Instead, estimate the standard error of the sample median μˆmedian using the bootstrap. 
# Comment on your findings.

# Bootstrap sampling for the median
bootstrap_medians <- replicate(n_bootstrap, {
  sample_data <- sample(Boston$medv, replace = TRUE)
  median(sample_data)
})

# Bootstrap estimate of SE for the median
bootstrap_se_mu_hat_median <- sd(bootstrap_medians)
bootstrap_se_mu_hat_median

#Ans: 0.3780
#Interpretation:The standard error for the sample median, estimated via bootstrap, is 0.3780. This indicates the expected variability in the sample median if we repeatedly draw samples from the population. Unlike the mean, there is no simple formula for the SE of the median, 
# and bootstrap provides a practical approach to estimate this uncertainty.


#Visualization of results of the bootstrap statistics (mean and median) and 
#Compare them to the original estimates.

# Histogram for bootstrap means
hist(bootstrap_means, breaks = 30, main = "Bootstrap Distribution of Sample Means",
     xlab = "Bootstrap Sample Means", col = "skyblue", border = "white")
abline(v = mu_hat, col = "red", lwd = 2) # Add line for original mean
legend("topright", legend = c("Original Mean"), col = c("red"), lty = 1, lwd = 2)


# Density plot for bootstrap means
plot(density(bootstrap_means), main = "Density of Bootstrap Sample Means",
     xlab = "Bootstrap Sample Means", col = "blue", lwd = 2)
abline(v = mu_hat, col = "red", lwd = 2) # Add line for original mean
legend("topright", legend = c("Original Mean"), col = c("red"), lty = 1, lwd = 2)

# Histogram for bootstrap medians
hist(bootstrap_medians, breaks = 30, main = "Bootstrap Distribution of Sample Medians",
     xlab = "Bootstrap Sample Medians", col = "lightgreen", border = "white")
abline(v = mu_hat_median, col = "red", lwd = 2) # Add line for original median
legend("topright", legend = c("Original Median"), col = c("red"), lty = 1, lwd = 2)

# Density plot for bootstrap medians
plot(density(bootstrap_medians), main = "Density of Bootstrap Sample Medians",
     xlab = "Bootstrap Sample Medians", col = "darkgreen", lwd = 2)
abline(v = mu_hat_median, col = "red", lwd = 2) # Add line for original median
legend("topright", legend = c("Original Median"), col = c("red"), lty = 1, lwd = 2)

# Compare density plots for means and medians
plot(density(bootstrap_means), col = "blue", lwd = 2, 
     main = "Bootstrap Distributions of Mean and Median",
     xlab = "Bootstrap Values")
lines(density(bootstrap_medians), col = "darkgreen", lwd = 2)
abline(v = mu_hat, col = "blue", lty = 2, lwd = 2) # Original mean
abline(v = mu_hat_median, col = "darkgreen", lty = 2, lwd = 2) # Original median
legend("topright", 
       legend = c("Bootstrap Means", "Bootstrap Medians", "Original Mean", "Original Median"), 
       col = c("blue", "darkgreen", "blue", "darkgreen"), 
       lty = c(1, 1, 2, 2), lwd = 2)

#Save these plots in figures directory:
# Save Histogram for Bootstrap Means
png("figures/bootstrap_means_hist.png", width = 800, height = 600)
hist(bootstrap_means, breaks = 30, main = "Bootstrap Distribution of Sample Means",
     xlab = "Bootstrap Sample Means", col = "skyblue", border = "white")
abline(v = mu_hat, col = "red", lwd = 2)
legend("topright", legend = c("Original Mean"), col = c("red"), lty = 1, lwd = 2)
dev.off()

# Save Density Plot for Bootstrap Means
png("figures/bootstrap_means_density.png", width = 800, height = 600)
plot(density(bootstrap_means), main = "Density of Bootstrap Sample Means",
     xlab = "Bootstrap Sample Means", col = "blue", lwd = 2)
abline(v = mu_hat, col = "red", lwd = 2)
legend("topright", legend = c("Original Mean"), col = c("red"), lty = 1, lwd = 2)
dev.off()

# Save Histogram for Bootstrap Medians
png("figures/bootstrap_medians_hist.png", width = 800, height = 600)
hist(bootstrap_medians, breaks = 30, main = "Bootstrap Distribution of Sample Medians",
     xlab = "Bootstrap Sample Medians", col = "lightgreen", border = "white")
abline(v = mu_hat_median, col = "red", lwd = 2)
legend("topright", legend = c("Original Median"), col = c("red"), lty = 1, lwd = 2)
dev.off()

# Save Density Plot for Bootstrap Medians
png("figures/bootstrap_medians_density.png", width = 800, height = 600)
plot(density(bootstrap_medians), main = "Density of Bootstrap Sample Medians",
     xlab = "Bootstrap Sample Medians", col = "darkgreen", lwd = 2)
abline(v = mu_hat_median, col = "red", lwd = 2)
legend("topright", legend = c("Original Median"), col = c("red"), lty = 1, lwd = 2)
dev.off()

# Save Comparison Plot of Means and Medians
png("figures/bootstrap_comparison_density.png", width = 800, height = 600)
plot(density(bootstrap_means), col = "blue", lwd = 2, 
     main = "Bootstrap Distributions of Mean and Median",
     xlab = "Bootstrap Values")
lines(density(bootstrap_medians), col = "darkgreen", lwd = 2)
abline(v = mu_hat, col = "blue", lty = 2, lwd = 2)
abline(v = mu_hat_median, col = "darkgreen", lty = 2, lwd = 2)
legend("topright", 
       legend = c("Bootstrap Means", "Bootstrap Medians", "Original Mean", "Original Median"), 
       col = c("blue", "darkgreen", "blue", "darkgreen"), 
       lty = c(1, 1, 2, 2), lwd = 2)
dev.off()




