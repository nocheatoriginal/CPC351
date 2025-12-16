setwd("~/Desktop/HHN/Sem-6/courses/CPC351/CPC351-repo/Assignment2")

food_nutrition_df <- read.csv("Data/Food_Nutrition_Dataset.csv")

summary(food_nutrition_df)


food_nutrition_values <- food_nutrition_df[,3:8]
summary(food_nutrition_values)