library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
source("create_table.R")
# This function will return a summary table that including general information about
# # recipes users select
  # queries <- c("Asian", "American", "Main Dishes")
  # a <- generate_param(queries)
  # b <- create_table(a)
  # basic_table <- b
  # c <- return_summary(basic_table, "Asian", "American")
  # c
  # cuisine1 <- "Asian"
  # cuisine2 <- "American"
  # return_description(c,cuisine1,cuisine2)
return_summary <- function(basic_table, cuisine1, cuisine2) {
  # Becasue some cuisine attributes returned the vector list, 
  # so the first step is checking the cuisine type of the recipe. 
  basic_table$is_cuisine1 <- "1"
  basic_table$is_cuisine2 <- "2"
  index = 1
  for (cuisine_type in basic_table[,"attributes.cuisine"]) {
    existence <- any(cuisine_type == cuisine1)
     basic_table[index, "is_cuisine1"] <- existence
     index <- index + 1
  }
  index = 1
  for (cuisine_type in basic_table[,"attributes.cuisine"]) {
    existence <- any(cuisine_type == cuisine2)
    basic_table[index, "is_cuisine2"] <- existence
    index <- index + 1
  }
  
  # Based on the results, create a summary table for both cuisines
  cuisine1_summary <- basic_table %>% 
    filter(is_cuisine1 == TRUE) %>% 
    summarise(
      number_of_result = n(),
      average_time = round(mean(totalTimeInSeconds, na.rm=TRUE) * 10 / 60) / 10,
      average_rating = round(mean(rating, na.rm=TRUE) * 10) / 10
    )
  cuisine2_summary <- basic_table %>% 
    filter(is_cuisine2 == TRUE) %>% 
    summarise(
      number_of_result = n(),
      average_time = round(mean(totalTimeInSeconds, na.rm=TRUE) * 10 / 60) / 10,
      average_rating = round(mean(rating, na.rm=TRUE) * 10) / 10
    )
  
  #store the results in the vector list and return that list
  users_result <- c(cuisine1_summary[1,1], cuisine1_summary[1,2], cuisine1_summary[1,3], 
                    cuisine2_summary[1,1], cuisine2_summary[1,2], cuisine2_summary[1,3])
  users_result
}

# This function will return the description for the general tabs
# It will generate description based on the list that including the summartive information used
# This function will take the summary_list (the list that including summarative information),
# cuisine1's name and cuisine2's name as parameters.
return_description <- function(summary_list, cuisine1_name, cuisine2_name) {
  # Assign values for variables that description need based on the position of the 
  # list that including certain summartive information
  # The position of each info located in fixed in the summary_list
  number_of_results_for_cuisine1 <- summary_list[[1]]
  average_cooking_time_for_cuisine1 <- summary_list[[2]]
  average_rating_for_cuisine1 <- summary_list[[3]]
  number_of_results_for_cuisine2 <- summary_list[[4]]
  average_cooking_time_for_cuisine2 <- summary_list[[5]]
  average_rating_for_cuisine2 <- summary_list[[6]]
 
  # Checking whether both cuisines exisit
  if (number_of_results_for_cuisine1 == 0 || number_of_results_for_cuisine2 == 0) { # if any of cuisines doesn't exisit
    if (number_of_results_for_cuisine1 != 0) {
      description <- paste0(
        "Accoridng to your current selection, we only find lots of recipes for ", cuisine1_name,  "! ",
        "The average rating for ", cuisine1_name, " is ", average_rating_for_cuisine1,
        " and the average cooking time for ", cuisine1_name, " is ", average_rating_for_cuisine1,
        " mins. The following table shows all results that match you choices.
        If you want to learn details information about this cuisine,
        you can navigate to other tabs in this page."
      )
    } else {
      description <- paste0(
        "Accoridng to your current selection, we only find lots of recipes for ", cuisine2_name,  "! ",
        "The average rating for ", cuisine2_name, " is ", average_rating_for_cuisine2,
        " and the average cooking time for ", cuisine2_name, " is ", average_rating_for_cuisine2,
        " mins. The following table shows all results that match you choices.
        If you want to learn details information about this cuisine,
        you can navigate to other tabs in this page."
      )
    }
  } else { #Which means both cuisines are exisiting
      #Finding out the cuisine that required the shortest preparation time
      if (average_cooking_time_for_cuisine1 > average_cooking_time_for_cuisine2) {
        time_saving_cuisine_name <- cuisine2_name
        time_saving_cuisine <- average_cooking_time_for_cuisine2
      } else if (average_cooking_time_for_cuisine1 < average_cooking_time_for_cuisine2) {
        time_saving_cuisine_name <- cuisine1_name
        time_saving_cuisine <- average_cooking_time_for_cuisine1
      } else {
        time_saving_cuisine_name <- paste0(cuisine1_name, " cuisine or ", cuisine2_name, " cuisine ")
        time_saving_cuisine <- paste0(average_cooking_time_for_cuisine1, "or", average_cooking_time_for_cuisine2)
      }
      
      #Create the description
      description <- paste0(
        "Accoridng to your current selection, we find lots of recipes for you!
      Among those recipes, there are ", number_of_results_for_cuisine1, " recipes belong to ",
        cuisine1_name, " and ", number_of_results_for_cuisine2, " recipes belong to ", cuisine2_name, ".
      The average rating for ", cuisine1_name, " is ", average_rating_for_cuisine1, ", and
      the average rating for ", cuisine2_name, " is ", average_rating_for_cuisine2, ". If your want
      to do or learn quick cooking, you should consider about ", time_saving_cuisine_name, ". Becasue, 
      the avergae cooking time for ", cuisine1_name, " is ", average_cooking_time_for_cuisine1, 
        " mins, and the average cooking time for ", cuisine2_name, " is ", average_cooking_time_for_cuisine2,
        " mins. The following table shows all results that match you choices.
      If you want to learn details information about those cuisine, 
      you can navigate to other tabs in this page."
      )  
  }
  description
}







