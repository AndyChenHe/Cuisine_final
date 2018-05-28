library(shiny)
library(ggplot2)
library(dplyr)
source("create_table.R")

cuisine_list <- c("American", "Italian", "Asian", "Mexican", "Southern & Soul Food", "French", 
                 "Southwestern", "Barbecue", "Indian", "Chinese", "Cajun & Creole", "English", 
                 "Mediterranean", "Greek", "Spanish", "German", "Thai", 
                 "Moroccan", "Irish", "Japanese", "Cuban", "Hawaiin", "Swedish", "Hungarian", "Portugese")
diet_list <- c ("No limit", "Lacto vegetarian", "Ovo vegetarian", "Pescetarian", "Vegan", "Vegetarian")
course_list <- c("No limit", "Main Dishes", "Desserts", "Side Dishes", "Lunch and Snacks", "Appetizers", "Salads, Breads", 
                 "Breakfast and Brunch", "Soups", "Beverages", "Condiments and Sauces", "Cocktails")
allergy_type <- c("Wheat-Free" = "392^Wheat-Free", "Gluten-Free" = "393^Gluten-Free", "Peanut-Free "= "394^Peanut-Free",
                  "Tree Nut-Free" = "395^Tree Nut-Free", "Dairy-Free" = "396^Dairy-Free", "Egg-Free" = "397^Egg-Free",
                  "Seafood-Free" = "398^Seafood-Free", "Sesame-Free" = "399^Sesame-Free", 
                  "Soy-Free" = "400^Soy-Free", "Sulfite-Free" = "401^Sulfite-Free")

#create UI for the shiny App
ui <- fluidPage(
  titlePanel("Cuisine test"),
  sidebarLayout(
    # Control panels
    sidebarPanel(
      # User can use this drop down menu to select the state they want to see
      selectInput("cuisine1_name", label="The name of the Cuisine 1", 
                  choices= unique(cuisine_list)),
            selectInput("cuisine2_name", label="The name of the Cuisine 2", 
                  choices= unique(cuisine_list)),
      selectInput("Allowed diet", label="Allowed diet", 
                  choices= unique(diet_list)),
      checkboxGroupInput("variable", "Variables to show:",
                         allergy_type)
   ),
    # This panel will be used to show the graph and table
    mainPanel(
      tabsetPanel(type = "tabs",
                  # This tab is for
                  tabPanel("General"
                    
                  ),
                  
                  # This tab is for
                  tabPanel("Flavor Profile"
                           
                  ),
                  
                  # This tab is for
                  tabPanel("Macronutrition"
                           
                  ),
                  
                  # This tab is for
                  tabPanel("Micronutrition"
                           
                  ),
                  
                  # This tab is for
                  tabPanel("Preparation"
                           
                  )
      )

    )# main panel stops here 
  )
)


# Create the server for the shiny app
server <- function(input, output) {
  
# Andy works here
  
  

  
# Kara workds here r  

}

#Create a new app
shinyApp(ui = ui, server = server)
