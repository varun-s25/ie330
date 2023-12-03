library(ggplot2)

# data
week <- 1:20
spelling_errors <- (c(3, 6, 0, 5, 9, 5, 2, 2, 3, 2, 1, 6, 9, 8, 6, 4, 13, 3, 0, 7))/1000

data_frame = data.frame(Week = week, Errors = spelling_errors)

# lambda
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