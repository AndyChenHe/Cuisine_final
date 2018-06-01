#install.packages("shiny")
library(shiny)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
source("create_table.R")
#install.packages("httr")
library(httr)
#install.packages("jsonlite")
library(jsonlite)
#install.packages("tidyr")
library(tidyr)
source("micro_Kara's_Part .R")

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
                  tabPanel("Micronutrition", 
                           dataTableOutput("micro_table"),
                           dataTableOutput("micro_table_sum"),
                           plotOutput("micro_plot"),
                           tags$p("Micronutrient contents include Calcium, Folate, 
                                  Magnesium, Potassium, Sodium and Sugars. It's interesting to see
                                  the comparsion of these nutrients in different types of cuisines. 
                                  For example, American cuisines tend to have more suagr in their meals
                                  compared to other countries. One comparison shows that an American cuisine
                                  had 55 grams of sugar compared to other French cuisines that had around
                                  10 to 13 grams of sugar in their meals. It should be noted too that Americans,
                                  on average tend to be more overweight then people in other countries. There
                                  is a direct correlation between the amount of sugar in American cuisine 
                                  and obesity rates compared to sugar content in other countries. Secondly, 
                                  American cuisines tend to have more sodium compared other cuisines. We can 
                                  make the same observation with sodium content as we did with sugars and its
                                  relation to the obesity epidemic in the United States. Of course, we must 
                                  also assume that most people in American eat mostly American food. When looking
                                  into what the most popular dishes were in America, the list consisted of Apple
                                  pie, Meatloaf, Buffalo Wings, etc. which tells us that American's are eating
                                  mostly American foods. Because of this fact, it is safe to make the assumption
                                  that because American's eat mostly American food, they are consuming more sugar
                                  and sodium which in turn contributes to US obesity rates."), 
                           tags$p("You will also see that all meals have more sugar then any other micro nutrient
                                  across the board. Clearly, all cuisines contain more sugar for the purpose of 
                                  making the meal taste good. But, American cuisine contains the most sugar of any 
                                  other types of cuisines. This means, if the user is looking for a healthy meal 
                                  option, they should stir away from American cuisines and go for a French or Asian
                                  cuisine instead.")
                                  
                           
                  ),
                  
                  # This tab is for
                  tabPanel("Preparation"
                           
                  )
      )
      
    )# main panel stops here 
  )
)
server <- function(input, output) {
  reactive_table <- reactiveValues()
  reactive_table$table <- ""
  reactive_table$micro_nutrition_table <- ""
  reactive_table$micro_nutrition_sum_table <- ""
  
  initialize_table_need_to_be_used <- function(){
    queries <- c(input$cuisine1_name, input$cuisine2_name, input$allowed_course, input$diet, input$allergy_type)
    queries_list <-generate_param(queries)
    reactive_table$table <- create_table(queries_list)
  }
  
  micro_nutrient_table <- function() {
    micro_filtered <- select()
  }
  

  # Andy works here
  output$data <- renderDataTable({
    initialize_table_need_to_be_used()
    table <- reactive_table$table
    table
  })
  
  output$data2 <- renderDataTable({
    new_table <- plot(reactive_table$table)
    new_table
  })
  
  # Kara works here 
  #renders table with all microbutrient contents of each cuisine 
  output$micro_table <- renderDataTable({
    reactive_table$micro_nutrition_table <- process_basic_table(reactive_table$table, input$cuisine1_name, input$cuisine2_name)
    reactive_table$micro_nutrition_table  
    })
  #renders summary table with average micro nutrient contents of each cuisine 
  output$micro_table_sum <- renderDataTable({
    reactive_table$micro_nutrition_sum_table <- summ_table(reactive_table$micro_nutrition_table) 
    reactive_table$micro_nutrition_sum_table
  })
  #renders bar chart for summary micronutrient contents 
  output$micro_plot <- renderPlot({
     b <- micro_vis(reactive_table$micro_nutrition_sum_table)
     b
   })

    

}

#Create a new app
shinyApp(ui = ui, server = server)

