library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

############# Basic Set up ##############

apikey = "6173072958d3d557ad3bdf7115bf3a8c";
app_id = "cfbca5a8";
base_uri <- "http://api.yummly.com/v1/api/"

############# Create params list ############# 

# The following function will create a param list for fetching
# It will take an array of vectors that included all queries that need to be used (based on the control bar) 
# as the parameter
# It will return a list for queries parameters
generate_param <- function(queries) {
  query_params_list <- list("_app_id" = app_id, "_app_key" = apikey, "maxResult" = 50, "start"=1)
  index = 0
  index_for_ingredients <- 1
  the_last_index_for_params <- 4
  for (queryItem in queries) {
    index <- index + 1
    if (queryItem != "No limit") { 
      the_last_index_for_params <- the_last_index_for_params + 1
      if(index == 1) {
        queryItem <- tolower(queryItem)
        queryItem <- paste0("cuisine^cuisine-", queryItem)
        query_params_list[["allowedCuisine[]1" ]] <- queryItem
        names(query_params_list)[[the_last_index_for_params]] <- "allowedCuisine[]" #users have to input something for cuisine 1
      } else if(index == 2) {
        queryItem <- tolower(queryItem)
        queryItem <- paste0("cuisine^cuisine-", queryItem)
        query_params_list[["allowedCuisine[]2" ]] <- queryItem
        names(query_params_list)[[the_last_index_for_params]] <- "allowedCuisine[]"
      } else if(index == 3) {
        queryItem <- paste0("course^course-", queryItem)
        query_params_list[[ "allowedCourse[]" ]] <- queryItem  #users have to input something for cuisine 1, so it won't out of boundary.
      } else if(index <= 4){
        query_param_list_name <- paste0("diet[]", index)
        query_params_list[[ query_param_list_name ]] <- queryItem
        names(query_params_list)[[the_last_index_for_params]] <- "allowedDiet[]"
      } else {
        query_param_list_name <- paste0("allowedAllergy[]", index)
        query_params_list[[ query_param_list_name ]] <- queryItem
        names(query_params_list)[[the_last_index_for_params]] <- "allowedAllergy[]"  
      }
    }
  }
  query_params_list
}


############# Create the table that need to for analyzing ############# 

# Create the table that need to for analyzing the information about foods 
# (information including flavor, prep time, nutrition) and return that table
# It takes queries (a list of queries parameter) as parameter
# It will return a table that include all information that need to be used for analyzing
create_table <- function(queries){
  #Create the table that include the all the information for all of the recipes (except the nutrition contents)
  all_recipes_uri <- paste0(base_uri, "recipes")
  #queries_param <- generate_param(queries)
  response <- GET(all_recipes_uri, query = queries)
  body <- content(response, "text")
  recipe_parent <- fromJSON(body)
  recipe_parent$matches
  is.data.frame(recipe_parent$matches)
  recipes_true <- recipe_parent$matches
  recipes_flatten <- flatten(recipes_true)
  recipes_flatten
  # create the nutrition table that including all nutrition info for recipes
   nutrition_table <- create_a_nutrition_table(recipes_flatten)
  # Add nutrition info to the recipes_flatten table
   recipes_and_nutrition <- left_join(recipes_flatten, nutrition_table, by = c("id" = "recipe_id")) 
   recipes_and_nutrition
}  
  
############# create nutrition table that included all nutrition info for the recipes above

# This following function will create a sample (has the standard form) table for nutrition
# It will return the sample table
create_sample <- function() {
  nutrition_uri <- "http://api.yummly.com/v1/api/recipe/Char-Siu-Pork-2137829"
  query_params <- list("_app_id" = app_id, "_app_key" = apikey)
  response <- GET(nutrition_uri, query = query_params)
  body <- content(response, "text")
  recipe_details_parent <- fromJSON(body)
  nutrition <- recipe_details_parent$nutritionEstimates
  if (is.data.frame(nutrition) & (length(nutrition) != 0)) {
    nutrition <- flatten(nutrition)
    nutrition <- nutrition %>%
      select(description,value, unit.name) %>%
      filter(description != "Energy")
    nutrition <- nutrition %>%
      mutate(description_including_unit = paste( nutrition$description, "( in", nutrition$unit.name, ")")) %>%
      select(description_including_unit, value) %>%
      filter(
        description_including_unit == "Carbohydrate, by difference ( in gram )" |
          description_including_unit == "Total lipid (fat) ( in gram )" |
          description_including_unit == "Protein ( in gram )" |
          description_including_unit == "Water ( in gram )" |
          description_including_unit == "Sugars, total ( in gram )" |
          description_including_unit == "Folate, total ( in gram )" |
          description_including_unit == "Sodium, Na ( in gram )" |
          description_including_unit == "Magnesium, Mg ( in gram )" |
          description_including_unit == "Calcium, Ca ( in gram )" |
          description_including_unit == "Potassium, K ( in gram )"
      ) %>%
      mutate(recipe_id = "sameple")
    
    # spread by column "attribute"
    nutrition1 <- spread(nutrition, key = description_including_unit, value = value)
  }
}

# This function will create the nurition table based on the exisit table that included all recipes information
# It will take recipes_flatten(the table that influded all other information about the recipes) as a parameter
# It will return a nutrition table that included all repcipes' nutrition information
create_a_nutrition_table <- function(recipes_flatten) {
  # load the sample of the nutrition table, nutrition 1 means sample nutrition table
  nutrition1 <- create_sample()
  # append more rows about the nutritions (based on the ids we got from the above table) to the intial table
  # The reason for using for loop here is in each loop it will make a function call to fetch the data in 
  # another list.
  for (id_number in c(1 : nrow(recipes_flatten))){ 
     nutrition_uri <- paste0(base_uri, "recipe/", recipes_flatten$id[id_number] )
     query_params <- list("_app_id" = app_id, "_app_key" = apikey)
     response <- GET(nutrition_uri, query = query_params)
     body <- content(response, "text")
     recipe_parent <- fromJSON(body)
     body <- content(response, "text")
     recipe_details_parent <- fromJSON(body)
     nutrition <- recipe_details_parent$nutritionEstimates
     if (is.data.frame(nutrition)) {
       nutrition <- flatten(nutrition)
       nutrition <- nutrition %>% 
         select(description,value, unit.name) %>% 
         filter(description != "Energy")
       nutrition <- nutrition %>% 
         mutate(description_including_unit = paste( nutrition$description, "( in", nutrition$unit.name, ")")) %>% 
         select(description_including_unit, value) %>% 
         filter(
             description_including_unit == "Carbohydrate, by difference ( in gram )" |
             description_including_unit == "Total lipid (fat) ( in gram )" |
             description_including_unit == "Protein ( in gram )" |
             description_including_unit == "Water ( in gram )" |
             description_including_unit == "Sugars, total ( in gram )" |
             description_including_unit == "Folate, total ( in gram )" |
             description_including_unit == "Sodium, Na ( in gram )" |
             description_including_unit == "Magnesium, Mg ( in gram )" |
             description_including_unit == "Calcium, Ca ( in gram )" |
             description_including_unit == "Potassium, K ( in gram )" 
         ) %>% 
         mutate(recipe_id = recipes_flatten$id[id_number])
       # spread by column "attribute"
       new_nutrition_contents <- spread(nutrition, key = description_including_unit, value = value)
       # set the value equal to 0 if some nutritions didn't exisit for this receipe
       names(new_nutrition_contents)
       if (length(names(nutrition1)) != length(names(new_nutrition_contents))) {
         for(all_nutritions in names(nutrition1)) {
           appear <- 0
           for (current_nutritions in names(new_nutrition_contents)) {
             if(all_nutritions == current_nutritions) {
               appear <- appear + 1
             }
           }
           if(appear == 0) {
             new_nutrition_contents[[all_nutritions]] = 0
           }
         }
       }
       #add new rows to nutrition1 (origin nutrition table)
       nutrition1 <- rbind(nutrition1, new_nutrition_contents)  
     }
  }
  nutrition1
}


