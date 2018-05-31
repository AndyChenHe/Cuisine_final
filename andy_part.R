library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
source("create_table.R")
return_summary <- function(basic_table, cuisine1, cuisine2) {
  
  cuisine1_table <- mutate(basic_table, is1 = lapply(attributes.cuisine, element, cuisine1)) %>%
    filter(is1 == TRUE)
  cuisine2_table <- mutate(basic_table, is1 = lapply(attributes.cuisine, element, cuisine2)) %>% 
    filter(is1 == FALSE)

  # Based on the results, create a summary table for both cuisines
  cuisine1_summary <- cuisine1_table %>% 
    summarise(
      number_of_result = n(),
      average_time = round(mean(totalTimeInSeconds, na.rm=TRUE) * 10 / 60) / 10,
      average_rating = round(mean(rating, na.rm=TRUE) * 10) / 10
    )
  
  cuisine2_summary <- basic_table %>% 
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
        "According to your current selection, we have found lots of recipes for ", cuisine1_name,  "! ",
        "The average rating for ", cuisine1_name, " is ", average_rating_for_cuisine1,
        " and the average cooking time for ", cuisine1_name, " is ", average_cooking_time_for_cuisine1,
        " mins. The following table shows all the results that match your choices.
        If you want to learn detailed information about this cuisine,
        you can navigate to other tabs on this page."
      )
    } else {
      description <- paste0(
        "According to your current selection, we have found lots of recipes for ", cuisine2_name,  "! ",
        "The average rating for ", cuisine2_name, " is ", average_rating_for_cuisine2,
        " and the average cooking time for ", cuisine2_name, " is ", average_cooking_time_for_cuisine2,
        " mins. The following table shows all the results that match your choices.
        If you want to learn detailed information about this cuisine,
        you can navigate to other tabs on this page."
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
        "According to your current selection, we have found lots of recipes for you!
      Among those recipes, there are ", number_of_results_for_cuisine1, " recipes that belong to ",
        cuisine1_name, " and ", number_of_results_for_cuisine2, " recipes that belong to ", cuisine2_name, ".
      The average rating for ", cuisine1_name, " is ", average_rating_for_cuisine1, ", and
      the average rating for ", cuisine2_name, " is ", average_rating_for_cuisine2, ". If your want
      to do or learn how to cook fast cuisines, you should consider ", time_saving_cuisine_name, ". Because, 
      the avergae cooking time for ", cuisine1_name, " is ", average_cooking_time_for_cuisine1, 
        " mins, and the average cooking time for ", cuisine2_name, " is ", average_cooking_time_for_cuisine2,
        " mins. The following table shows all the results that match your choices.
      If you want to learn more detailed information about those cuisines, 
      you can navigate to other tabs on this page."
      )  
  }
  description
}

# This function will check whether it is a cuisine 1 or two depends on the params
element <- function(check, # List of cuisine types
                    phrase # Cuisine name 
                    ){ 
  is.element(phrase, check)
}



