# Read
dataset <- read_csv('shopping_trends.csv')

# Summary
summary(dataset)

# Missing
colSums(is.na(dataset))
na.omit(dataset)

# Duplicates
dataset <- distinct(dataset)

# Gender Distribution
barplot(table(dataset$Gender), 
        main = "Gender Distribution", 
        xlab = "Gender", 
        ylab = "Count")

# Top 10 Purchased Item
item_counts <- sort(table(dataset$`Item Purchased`), decreasing = TRUE)

top_items <- head(item_counts, 10)

barplot(top_items, 
        main = "Top 10 Purchased Items", 
        las = 2,  # Rotate axis labels
        xlab = "Item", 
        ylab = "Count")

# Subscription Status vs. Categories
category_subscription <- table(dataset$Category, dataset$`Subscription Status`)

barplot(category_subscription, 
        main = "Subscription Status by Category", 
        xlab = "Category", 
        ylab = "Proportion", 
        legend = rownames(category_subscription))

# Purchases by Location
location_data <- aggregate(dataset$`Purchase Amount (USD)`, 
                           by = list(Location = dataset$Location), 
                           sum)

symbols(1:nrow(location_data), location_data$x, 
        circles = sqrt(location_data$x/pi), 
        inches = 0.1, 
        main = "Total Purchases by Location", 
        xlab = "Location Index", 
        ylab = "Total Purchase Amount (USD)")

# Review Rating vs. Purchase Amount
plot(dataset$`Review Rating`, dataset$`Purchase Amount (USD)`, 
     main = "Review Rating vs. Purchase Amount", 
     xlab = "Review Rating", 
     ylab = "Purchase Amount (USD)", 
     pch = '*')
