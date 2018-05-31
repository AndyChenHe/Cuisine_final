library(shiny)
library(ggplot2)
library(dplyr)
source("create_table.R")
source("andy_part.R")

# Create a list for control bar's choices
cuisine_list <- c("American", "Italian", "Asian", "Mexican", "Southern & Soul Food", "French", 
                 "Southwestern", "Barbecue", "Indian", "Chinese", "Cajun & Creole", "English", 
                 "Mediterranean", "Greek", "Spanish", "German", "Thai", 
                 "Moroccan", "Irish", "Japanese", "Cuban", "Hawaiin", "Swedish", "Hungarian", "Portugese")
course_list <- c("No limit", "Main Dishes", "Desserts", "Side Dishes", "Lunch and Snacks", "Appetizers", "Salads, Breads", 
                 "Breakfast and Brunch", "Soups", "Beverages", "Condiments and Sauces", "Cocktails")
diet_list <- c ("No limit"= "No limit", "Lacto vegetarian" = "388^Lacto vegetarian", "Ovo vegetarian" = "389^Ovo vegetarian", 
                "Pescetarian" = "390^Pescetarian", "Vegan" = "386^Vegan", "Vegetarian" = "387^Lacto-ovo vegetarian", 
                "Paleo" = "403^Paleo")
allergy_type <- c("Wheat-Free" = "392^Wheat-Free", "Gluten-Free" = "393^Gluten-Free", "Peanut-Free "= "394^Peanut-Free",
                  "TreeNut-Free" = "395^Tree Nut-Free", "Dairy-Free" = "396^Dairy-Free", "Egg-Free" = "397^Egg-Free",
                  "Seafood-Free" = "398^Seafood-Free", "Sesame-Free" = "399^Sesame-Free", 
                  "Soy-Free" = "400^Soy-Free", "Sulfite-Free" = "401^Sulfite-Free")
length(allergy_type)

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
      selectInput("allowed_course", label="Allowed course", 
                  choices= unique(course_list)),
      radioButtons("diet", "Allowed diet:",
                         diet_list),
      checkboxGroupInput("allergy_type", "Allergy:",
                         allergy_type),
      submitButton("Submit")

   ),
    # This panel will be used to show the graph and table
    mainPanel(
      tabsetPanel(type = "tabs",
                  # This tab is for
                  tabPanel("General",
                    h1(
                      "General information of your choice"
                    ),
                    textOutput("general_description"),
                    dataTableOutput("data")
                  ),
                  
                  # This tab is for
                  tabPanel("Flavor Profile",
                    dataTableOutput("data2")        
                  ),
                  
                  # This tab is for
                  tabPanel("Macronutrition"
                           
                  ),
                  
                  # This tab is for
                  tabPanel("Micronutrition"
                           
                  )
      )

    )# main panel stops here 
  )
)


# Create the server for the shiny app
server <- function(input, output) {
  
  reactive_table <- reactiveValues()
  reactive_table$table <- ""
  
  initialize_table_need_to_be_used <- function(){
    queries <- c(input$cuisine1_name, input$cuisine2_name, input$allowed_course, input$diet, input$allergy_type)
    queries_list <-generate_param(queries)
    reactive_table$table <- create_table(queries_list)
  }
  
  #render the description summary of our table
  output$general_description <- renderText({
    initialize_table_need_to_be_used()
    summary_list <- return_summary(reactive_table$table, input$cuisine1_name, input$cuisine2_name)
    cuisine1_name <- input$cuisine1_name
    cuisine2_name <- input$cuisine2_name
    description <- return_description(summary_list, cuisine1_name, cuisine2_name)
    description
  })
  
  # render the table that including all the information about all cuisines
  output$data <- renderDataTable({
    recipe_details <- reactive_table$table %>% 
      select(recipeName,attributes.cuisine, ingredients, rating, totalTimeInSeconds)
    recipe_details
  })

  
# Kara workds here r  

  
  
}

#Create a new app
shinyApp(ui = ui, server = server)
