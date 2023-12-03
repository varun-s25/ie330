library(ggplot2)

# part a

# data
week <- 1:20
spelling_errors <- (c(3, 6, 0, 5, 9, 5, 2, 2, 3, 2, 1, 6, 9, 8, 6, 4, 13, 3, 0, 7))/1000

data_frame = data.frame(Week = week, Errors = spelling_errors)

# avg
avg <- (sum(spelling_errors) / (length(week)))

# ucl and lcl
center <- avg
ucl <- (center + 3 * sqrt(avg/1000))
lcl <- (max(center - 3 * sqrt(avg/1000), 0))

# plot
u_chart <- ggplot(data_frame, aes(x = Week, y = Errors)) +
  geom_line(color = 'blue', size = 1) +
  geom_point(color = 'blue', size = 3) +
  geom_hline(yintercept = c(center, ucl, lcl), linetype = c('solid', 'dashed', 'dashed'), color = c('red', 'green', 'green')) +
  geom_hline(yintercept = c(ucl, lcl), linetype = 'dashed', color = 'green') +
  labs(title = 'U Chart', x = 'Week', y = 'No. of Spelling Errors',
       caption = paste("avg =", round(center, 5), "\n", "UCL =", round(ucl, 5), "LCL =", round(lcl, 5)))

print(u_chart)

# statistical control check
out_of_control_points <- data_frame$Errors > ucl | data_frame$Errors < lcl
if (any(out_of_control_points)) {
  cat("Out-of-control points found at weeks:", data_frame$Week[out_of_control_points], "\n")
} else {
  cat("No out-of-control points found.\n")
}

cat("UCL =", ucl, "LCL =", lcl)
# part b

sample_weeks <- c(1:16, 18:20)
fraction_misspelled <- spelling_errors[sample_weeks]

data_frame_revised = data.frame(Week = sample_weeks, Errors = fraction_misspelled)


avg_revised <- (sum(fraction_misspelled) / (length(sample_weeks)))

# ucl and lcl
center_revised <- avg_revised
ucl_revised <- (center_revised + 3 * sqrt(avg_revised/1000))
lcl_revised <- (max(center_revised - 3 * sqrt(avg_revised/1000), 0))

# plot
u_chart_revised <- ggplot(data_frame_revised, aes(x = Week, y = Errors)) +
  geom_line(color = 'blue', size = 1) +
  geom_point(color = 'blue', size = 3) +
  geom_hline(yintercept = c(center_revised, ucl_revised, lcl_revised), linetype = c('solid', 'dashed', 'dashed'), color = c('red', 'green', 'green')) +
  geom_hline(yintercept = c(ucl_revised, lcl_revised), linetype = 'dashed', color = 'green') +
  labs(title = 'U Chart Revised', x = 'Week', y = 'No. of Spelling Errors',
       caption = paste("avg =", round(center, 5), "\n", "UCL =", round(ucl_revised, 5), "LCL =", round(lcl_revised, 5)))

print(u_chart_revised)

cat("Revised UCL =", ucl_revised, "Revised LCL = ", lcl_revised)