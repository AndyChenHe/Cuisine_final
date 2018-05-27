library(httr)
library(jsonlite)
library(dplyr)
library("tidyr")

# Get base URI


apikey = "6173072958d3d557ad3bdf7115bf3a8c";
app_id = "cfbca5a8";

base_uri <- "http://api.yummly.com/v1/api/"

"?_app_id=cfbca5a8&_app_key=6173072958d3d557ad3bdf7115bf3a8c& " 

"http://api.yummly.com/v1/api/recipes?_app_id=YOUR_ID&_app_key=YOUR_APP_KEY&q=onion+soup
&maxResult=10&start=10"

queries <- c("", "American", "", "")
query_key <- c("excludedIngredient[]", "allowedCuisine[]", "allowedCuisine[]", "allowedCourse[]")

generate_param <- function(queries) {
  query_params_list <- list("_app_id" = app_id, "_app_key" = apikey, "maxResult" = 100, "start"=1)
  index = 0
  for (queryItem in queries) {
    index <- index + 1
    if (queryItem != "") {
      if(index == 2) {
        queryItem <- tolower(queryItem)
        queryItem <- paste0("cuisine^cuisine-", queryItem)
        query_params_list[["allowedCuisine[]1" ]] <- queryItem
        names(query_params_list)[[5]] <- "allowedCuisine[]" #users have to input something for cuisine 1
      } else if(index == 3) {
        queryItem <- tolower(queryItem)
        queryItem <- paste0("cuisine^cuisine-", queryItem)
        query_params_list[["allowedCuisine[]2" ]] <- queryItem
        names(query_params_list)[[6]] <- "allowedCuisine[]"
      } else if(index == 4) {
        queryItem <- paste0("course^course-", queryItem)
        query_params_list[[ query_key[[index]] ]] <- queryItem  #users have to input something for cuisine 1, so it won't out of boundary.
      }
    }
  }
  query_params_list
}

generate_param(queries)

#create_table <- function(queries){
  #Create the table that include the all the information for all of the recipes (except the nutrition contents)
  all_recipes_uri <- paste0(base_uri, "recipes")
  response <- GET(all_recipes_uri, query = generate_param(queries))
  body <- content(response, "text")
  recipe_parent <- fromJSON(body)
  body <- content(response, "text")
  recipe_parent <- fromJSON(body)
  is.data.frame(recipe_parent)
  is.list(recipe_parent)  
  recipe_parent$matches
  is.data.frame(recipe_parent$matches)
  recipes_true <- recipe_parent$matches
  recipes_flatten <- flatten(recipes_true)
  nrow(recipes_flatten)

  
  # # load the initial nutrition table
  # nutrition_uri <- paste0(base_uri, "recipe/", recipes_flatten$id[1] )
  # nutrition_uri
  # #nutrition_uri <- "http://api.yummly.com/v1/api/recipe/Barbecue-Brisket-Sandwiches-My-Recipes"
  # query_params <- list("_app_id" = app_id, "_app_key" = apikey)
  # response <- GET(nutrition_uri, query = query_params)
  # body <- content(response, "text")
  # recipe_parent <- fromJSON(body)
  # body <- content(response, "text")
  # recipe_details_parent <- fromJSON(body)
  # nutrition <- recipe_details_parent$nutritionEstimates
  # is.data.frame(recipe_details_parent)
  # if (is.data.frame(nutrition)) {
  #   nutrition <- flatten(nutrition)
  #   nutrition <- nutrition %>% 
  #     select(description,value, unit.name) %>% 
  #     filter(description != "Energy")
  #   nutrition <- nutrition %>% 
  #     mutate(description_including_unit = paste( nutrition$description, "( in", nutrition$unit.name, ")")) %>% 
  #     select(description_including_unit, value) %>% 
  #     filter(
  #       description_including_unit == "Carbohydrate, by difference ( in gram )" |
  #         description_including_unit == "Total lipid (fat) ( in gram )" |
  #         description_including_unit == "Protein ( in gram )" |
  #         description_including_unit == "Water ( in gram )" |
  #         description_including_unit == "Sugars, total ( in gram )" |
  #         description_including_unit == "Folate, total ( in gram )" |
  #         description_including_unit == "Iron, Fe ( in gram )" |
  #         description_including_unit == "Magnesium, Mg ( in gram )" |
  #         description_including_unit == "Calcium, Ca ( in gram )" |
  #         description_including_unit == "Potassium, K ( in gram )" 
  #     )
  #   
  #   # spread by column "attribute"
  #   nutrition1 <- spread(nutrition, key = description_including_unit, value = value)
  #   nutrition1 <- mutate(nutrition1, recipe_id = recipes_flatten$id[1])
  # }  
  # 
  # 
  # # append more rows about the nutritions (based on the ids we got from the above table) to the intial table
  # for (id_number in c(2 : nrow(recipes_flatten))){
  #   nutrition_uri <- paste0(base_uri, "recipe/", recipes_flatten$id[id_number] )
  #   nutrition_uri
  #   #nutrition_uri <- "http://api.yummly.com/v1/api/recipe/Barbecue-Brisket-Sandwiches-My-Recipes"
  #   query_params <- list("_app_id" = app_id, "_app_key" = apikey)
  #   response <- GET(nutrition_uri, query = query_params)
  #   body <- content(response, "text")
  #   recipe_parent <- fromJSON(body)
  #   body <- content(response, "text")
  #   recipe_details_parent <- fromJSON(body)
  #   nutrition <- recipe_details_parent$nutritionEstimates
  #   nutrition
  #   if (is.data.frame(nutrition)) {
  #     nutrition <- flatten(nutrition)
  #     nutrition <- nutrition %>% 
  #       select(description,value, unit.name) %>% 
  #       filter(description != "Energy")
  #     nutrition <- nutrition %>% 
  #       mutate(description_including_unit = paste( nutrition$description, "( in", nutrition$unit.name, ")")) %>% 
  #       select(description_including_unit, value) %>% 
  #       filter(
  #         description_including_unit == "Carbohydrate, by difference ( in gram )" |
  #           description_including_unit == "Total lipid (fat) ( in gram )" |
  #           description_including_unit == "Protein ( in gram )" |
  #           description_including_unit == "Water ( in gram )" |
  #           description_including_unit == "Sugars, total ( in gram )" |
  #           description_including_unit == "Folate, total ( in gram )" |
  #           description_including_unit == "Iron, Fe ( in gram )" |
  #           description_including_unit == "Magnesium, Mg ( in gram )" |
  #           description_including_unit == "Calcium, Ca ( in gram )" |
  #           description_including_unit == "Potassium, K ( in gram )" 
  #       ) %>% 
  #       mutate(recipe_id = recipes_flatten$id[id_number])
  #     # spread by column "attribute"
  #     new_nutrition_contents <- spread(nutrition, key = description_including_unit, value = value)
  #     new_nutrition_contents <- mutate(new_nutrition_contents, recipe_id = recipes_flatten$id[id_number])
  #     nutrition1 <- rbind(nutrition1, new_nutrition_contents)  
  #   }
  # }
  # 
  # recipes_and_nutrition <- left_join(recipes_flatten, nutrition1, by = c("id" = "recipe_id")) 
#}



   

############ calls 
  