library(ggplot2)


efficiencies_long <- reshape2::melt(efficiencies_df, id.vars = 'hour', variable.name = 'Employee', value.name = 'CustomersServed')

# Plot
ggplot(efficiencies_long, aes(x = hour, y = CustomersServed, color = Employee, group = Employee)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:8, labels = paste(9:16, "h")) +
  labs(title = "Employee Efficiency Over Time",
       x = "Hour of the Day",
       y = "Number of Customers Served") +
  theme_minimal() +
  theme(legend.title = element_blank())
