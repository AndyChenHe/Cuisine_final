library(dplyr)

# Analyzes data generated from input filters
analyze <- function(input) {
  names <- c("id", "rating", "course", "cuisine", "carb", "fat", "protein")
  data <- input %>%
    select(id, rating, attributes.course, attributes.cuisine,
           contains("Carbohydrate"), contains("lipid"), contains("Protein"))
  colnames(data) <- names
  data
}

# Puts analyzed data in visualization-compatible format
visualize <- function(data) {
  data %>%
    select(-cuisine) %>%
    gather()
}

# Creates pie chart
test <- function(data) {
  ggplot(data, aes(x = "", y = value, fill = key)) +
    geom_bar(stat = "identity") +
    coord_polar("y")
}

# Supports 'analyze'
element <- function(check, phrase) {
  is.element(phrase, check)
}
