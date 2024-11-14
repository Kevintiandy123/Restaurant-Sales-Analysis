#Load the dataset----
url <- "https://raw.githubusercontent.com/Kevintiandy123/Restaurant-Sales-Analysis/refs/heads/main/Fast_Food_Sales.csv"
data <- read.csv(url)

install.packages("ggplot2")
install.packages("ISLR")

# View a few rows of the dataset to confirm it's loaded
View(data)

#GGplot : data----
library(ggplot2)
library(ISLR)

ggplot(data = data)

install.packages("knitr")
install.packages("kableExtra")

# Load all required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(knitr)
library(kableExtra)

# 1. Revenue Analysis with carefully positioned labels
# Calculate total sales for each item
item_sales <- aggregate(transaction_amount ~ item_name, data, sum)

revenue_plot <- ggplot(item_sales, aes(x = reorder(item_name, transaction_amount), y = transaction_amount)) +
  geom_bar(stat = "identity", fill = "#FF6B6B", width = 0.7) +
  geom_text(aes(label = sprintf("₹%s", format(transaction_amount, big.mark = ","))),
            hjust = -0.1,  # Adjusted position
            vjust = 0.5,   # Center vertically
            size = 3.5,    # Text size
            color = "black") + 
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2)),  # Add 20% space on top for labels
    labels = scales::comma_format(prefix = "₹")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 20, r = 60, b = 20, l = 20, unit = "pt")  # Increased right margin
  ) +
  labs(title = "Revenue by Menu Item",
       subtitle = "Total transaction amount for each item",
       x = "Item Name",
       y = "Revenue (₹)")

# Display plot
print(revenue_plot)

# 2. Time of Day Sales with improved labels
time_sales <- aggregate(transaction_amount ~ time_of_sale, data, sum)
time_sales$percentage <- with(time_sales, sprintf("%.1f%%", transaction_amount/sum(transaction_amount)*100))

time_plot <- ggplot(time_sales, aes(x = time_of_sale, y = transaction_amount, fill = time_of_sale)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("₹%s\n%s", 
                                format(transaction_amount, big.mark = ","), 
                                percentage)),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  scale_fill_manual(values = c("Morning" = "#FED766", 
                               "Afternoon" = "#2AB7CA",
                               "Evening" = "#FE4A49",
                               "Night" = "#851E3E",
                               "Midnight" = "#4C4C4C")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(title = "Sales Distribution Across Time of Day",
       x = "Time Period",
       y = "Total Sales (₹)")

print(time_plot)


# 3. Payment Distribution with improved labels
payment_data <- aggregate(transaction_amount ~ transaction_type, data, sum)
payment_data$percentage <- with(payment_data, sprintf("%.1f%%", transaction_amount/sum(transaction_amount)*100))

payment_plot <- ggplot(payment_data, aes(x = "", y = transaction_amount, fill = transaction_type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = sprintf("₹%s\n%s", 
                                format(transaction_amount, big.mark = ","),
                                percentage)),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Cash" = "#4CAF50", 
                               "Online" = "#2196F3",
                               "Others" = "#FFC107")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  ) +
  labs(title = "Payment Method Distribution",
       fill = "Payment Type")

print(payment_plot)

# 4. Item Type Analysis with better positioned labels
item_plot <- ggplot(data, aes(x = item_type, y = quantity, fill = item_type)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  stat_summary(fun = mean, 
               geom = "text", 
               aes(label = sprintf("Mean: %.1f", ..y..)),
               vjust = -2,  # Increased distance from point
               size = 3.5) +
  scale_fill_manual(values = c("Beverages" = "#69B3A2", 
                               "Fastfood" = "#E67F83")) +
  # Add more space at the top for labels
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(t = 40, r = 20, b = 20, l = 20, unit = "pt")  # Added top margin
  ) +
  labs(title = "Order Quantity Distribution by Item Type",
       x = "Category",
       y = "Quantity Ordered")

print(item_plot)

# 5. Daily Sales Trend with better positioned labels
# First get the date of max transaction
max_sale <- data[which.max(data$transaction_amount), ]

trend_plot <- ggplot(data, aes(x = date, y = transaction_amount)) +
  geom_line(color = "#2C3E50", size = 1) +
  geom_smooth(method = "loess", color = "#E74C3C", fill = "#F1948A", alpha = 0.2) +
  # Add point for maximum value
  geom_point(data = max_sale, color = "red", size = 3) +
  # Add label with more space and better formatting
  geom_text(data = max_sale,
            aes(label = sprintf("Peak: ₹%s", format(transaction_amount, big.mark = ","))),
            vjust = -2,    # Move label up
            hjust = 0.5,   # Center horizontally
            size = 3.5,
            color = "red") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +  # Add more space at top
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 40, r = 20, b = 20, l = 20, unit = "pt")  # Added top margin
  ) +
  labs(title = "Daily Sales Trend",
       x = "Date",
       y = "Sales Amount (₹)")

print(trend_plot)

# 6. Price vs Quantity Analysis with improved labels
price_plot <- ggplot(data, aes(x = item_price, y = quantity, color = item_type)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  annotate("text", 
           x = max(data$item_price), 
           y = max(data$quantity),
           label = sprintf("Correlation: %.2f", cor(data$item_price, data$quantity)),
           hjust = 1, 
           vjust = 1,
           size = 3.5) +
  scale_color_manual(values = c("Beverages" = "#3498DB", "Fastfood" = "#E74C3C")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  ) +
  labs(title = "Price vs Quantity Relationship",
       x = "Item Price (₹)",
       y = "Quantity Ordered",
       color = "Item Type")

print(price_plot)

# 7. Service Analysis with boxplot and clear labels
service_plot <- ggplot(data, aes(x = received_by, y = transaction_amount, fill = received_by)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +  # Made boxplot visible
  stat_summary(fun = mean, 
               geom = "text", 
               aes(label = sprintf("Avg: ₹%s", format(round(..y.., 0), big.mark = ","))),
               vjust = -1.5,  # Moved labels up
               size = 3.5) +
  scale_fill_manual(values = c("Mr." = "#3498DB", "Mrs." = "#E74C3C")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +  # Added space for labels
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(t = 40, r = 20, b = 20, l = 20, unit = "pt")
  ) +
  labs(title = "Transaction Amount Distribution by Server",
       x = "Server",
       y = "Transaction Amount (₹)")

print(service_plot)

# Display all plots
print(revenue_plot)
print(time_plot)
print(payment_plot)
print(item_plot)
print(trend_plot)
print(price_plot)
print(service_plot)
