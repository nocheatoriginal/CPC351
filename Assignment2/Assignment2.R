setwd("~/Desktop/HHN/Sem-6/courses/CPC351/CPC351-repo/Assignment2") # Set working directory!!

#
# Imports:
#
library(ggplot2)

#
# Question 1: Read the food nutrition data set and print structure/summary statistics.
#
food_nutrition_df <- read.csv("Data/Food_Nutrition_Dataset.csv")

str(food_nutrition_df)
head(food_nutrition_df, 10)
summary(food_nutrition_df)

#
# Question 2: Check for missing values:
#
food_nutrition_values <- food_nutrition_df[,3:8]

na_values <- colSums(is.na(food_nutrition_values))

barplot(na_values, las = 2, ylab = "Number of NA-values", main = "NA-values per column")



zero_values <- sapply(food_nutrition_values, function(col) {
  sum(!is.na(col) & col == 0)
})

barplot(zero_values, las = 2, ylab = "Number of zero-values", main = "Zero-values per column")

# Total missing values including NA and 0.0
missing_values <- na_values + zero_values
barplot(missing_values, las = 2, ylab = "Number of missing values", main = "Missing values per column")



# Print a table of all 0-value entries:
entry_name <- food_nutrition_df[[1]]
to_num <- function(v) suppressWarnings(as.numeric(as.character(v)))
zero_pos <- which(sapply(food_nutrition_values, function(col) {
  v <- to_num(col)
  !is.na(v) & v == 0
}), arr.ind = TRUE)

zero_table <- data.frame(
  Name   = entry_name[zero_pos[, "row"]],
  Column = colnames(food_nutrition_values)[zero_pos[, "col"]],
  Id     = zero_pos[, "row"],
  Value  = food_nutrition_values[zero_pos]
)

View(zero_table)

#
# Question 3: Display the number of unique food categories.
#
ggplot(food_nutrition_df, aes(x = food_nutrition_df[[2]])) +
  geom_bar() +
  coord_flip() +
  labs(x = "Category", y = "Frequency", title = "Number of unique food categories") +
  theme_minimal()

#
# Question 4: Find the top 10 foods with the highest calories.
#
top10_calories <- food_nutrition_df[order(food_nutrition_df[[3]], decreasing = TRUE), c(1, 3)]
top10_calories <- top10_calories[!is.na(top10_calories[[2]]), ]

top10_calories <- head(top10_calories, 10)
names(top10_calories) <- c("Name", "Calories")

View(top10_calories)

#
# Question 5: Calculate the average calories, protein, carbs, and fat per category.
#
avg_by_category <- aggregate(
  food_nutrition_df[, c("calories", "protein", "carbs", "fat")],
  by = list(category = food_nutrition_df$category),
  FUN = function(x) mean(x, na.rm = TRUE)
)

avg_by_category <- avg_by_category[order(avg_by_category$calories, decreasing = TRUE), ]

View(avg_by_category)

