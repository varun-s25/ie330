library(ggplot2)

# Data
wafer <- 1:30
x <- c(16.8, 14.9, 18.3, 16.5, 17.1, 17.4, 15.9, 14.4, 15.0, 15.7, 
       17.1, 15.9, 16.4, 15.8, 15.4, 15.4, 14.3, 16.1, 15.8, 15.9, 
       15.2, 16.7, 15.2, 14.7, 17.9, 14.8, 17.0, 16.2, 15.6, 16.3)

# Create a data frame
data_frame <- data.frame(Wafer = wafer, x = x)

# Calculate moving ranges
moving_ranges <- c(NA, diff(data_frame$x))

# Calculate averages and standard deviation
avg <- mean(data_frame$x)
std_dev <- sd(data_frame$x)

avgmr <- mean(moving_ranges, na.rm = TRUE)
stddevmr <- sd(moving_ranges, na.rm = TRUE)

cat("")

# Calculate UCL and LCL
ucl <- avg + 3 * std_dev
lcl <- avg - 3* std_dev

uclmr <- avgmr + 3 * stddevmr
lclmr <- avgmr - 3 * stddevmr

# Create individuals control chart
ind_chart <- ggplot(data_frame, aes(x = Wafer, y = x)) +
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = c(ucl, lcl), linetype = "dashed", color = "red") + 
  labs(title = "Individuals Control Chart",
       x = "Wafer",
       y = "x")

print(ind_chart)

# Create moving range control chart
mr_chart <- ggplot(data_frame, aes(x = Wafer, y = moving_ranges)) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = c(uclmr, lclmr), linetype = "dashed", color = "red") + 
  labs(title = "Moving Range Control Chart",
       x = "Wafer",
       y = "Moving Range") + 
  theme_minimal()

print(mr_chart)