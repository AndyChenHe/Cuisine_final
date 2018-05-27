library(shiny)
library(ggplot2)
library(dplyr)
source("create_table.R")

cuisine_list <- c("American", "Italian", "Asian", "Mexican", "Southern & Soul Food", "French", 
                 "Southwestern", "Barbecue", "Indian", "Chinese", "Cajun & Creole", "English", 
                 "Mediterranean", "Greek", "Spanish", "German", "Thai", 
                 "Moroccan", "Irish", "Japanese", "Cuban", "Hawaiin", "Swedish", "Hungarian", "Portugese")
diet_list <- c ("Lacto vegetarian", "Ovo vegetarian", "Pescetarian", "Vegan", "Vegetarian")
course_list <- c("Main Dishes", "Desserts", "Side Dishes", "Lunch and Snacks", "Appetizers", "Salads, Breads", 
                 "Breakfast and Brunch", "Soups", "Beverages", "Condiments and Sauces", "Cocktails")

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
