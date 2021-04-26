# Association Rules

library(arules)
groceries <- read.transactions("/home/sathvik/EC8/ML/Lab/Lab11/groceries.csv", sep = ",")
summary(groceries)

inspect(groceries[1:5])
itemFrequency(groceries[, 1:3])
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

image(groceries[1:5])
image(sample(groceries, 100))

# Step 3 – training a model on the data

apriori(groceries) # default parameters

groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 2))
groceryrules

# Step 4 – evaluating model performance
summary(groceryrules)
inspect(groceryrules[1:3])

# Step 5 – improving model performance
inspect(sort(groceryrules, by = "lift")[1:5])

berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# Saving association rules to a file or data frame
# write(groceryrules, file = "groceryrules.csv",
#       sep = ",", quote = TRUE, row.names = FALSE)
# groceryrules_df <- as(groceryrules, "data.frame")
# str(groceryrules_df)
