library(qcc)

# Data
wafer <- 1:30
x <- c(16.8, 14.9, 18.3, 16.5, 17.1, 17.4, 15.9, 14.4, 15.0, 15.7, 
       17.1, 15.9, 16.4, 15.8, 15.4, 15.4, 14.3, 16.1, 15.8, 15.9, 
       15.2, 16.7, 15.2, 14.7, 17.9, 14.8, 17.0, 16.2, 15.6, 16.3)

# Create a data frame
data_frame <- data.frame(Wafer = wafer, x = x)

# Calculate moving ranges
moving_ranges <- c(NA, diff(data_frame$x))

# Create individuals control chart
ind_chart <- qcc(data_frame$x, type = "xbar.one", plot = TRUE, title = "Individuals Chart")

# Create moving range control chart
mr_chart <- qcc(moving_ranges, type = "R", plot = TRUE, title = "Moving Range Chart")

# Estimate process mean and standard deviation
process_mean <- mean(data_frame$x)
#process_sd <- sd(data_frame$x)

cat("Process Mean:", process_mean, "\n")
#cat("Process Standard Deviation:", process_sd, "\n")