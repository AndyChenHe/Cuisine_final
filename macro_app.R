library(shiny)
library(ggplot2)
library(dplyr)
source("create_table.R")

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
                    dataTableOutput("data")
                  ),
                  
                  # This tab is for
                  tabPanel("Flavor Profile",
                    dataTableOutput("data2")        
                  ),
                  
                  # This tab is for
                  tabPanel("Macronutrition",
                    plotOutput("macro_pie1"),
                    plotOutput("macro_pie2"),
                    dataTableOutput("macro_table"),
                    h2("Diet Advisory"),
                    textOutput("macro_summary")
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
  
  reactive_table <- reactiveValues()
  
  initialize_table_need_to_be_used <- function() {
    queries <- c(input$cuisine1_name, input$cuisine2_name, input$allowed_course,
        input$diet, input$allergy_type)
    queries_list <-generate_param(queries)
    reactive_table$table <- create_table(queries_list)
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
  
# Kara workds here r  

  
  
  # Macro (Brendan)
  
  source("./macro_support.R")
  
  macro_table1 <- reactive({
    analyze(reactive_table$table) %>%
      mutate(is1 = lapply(cuisine, element, input$cuisine1_name)) %>%
      filter(is1 == TRUE) %>%
      summarize(cuisine = input$cuisine1_name, carb = mean(carb, na.rm = TRUE),
                fat = mean(fat, na.rm = TRUE), protein = mean(protein, na.rm = TRUE))
  })

  output$macro_pie1 <- renderPlot({
    test(visualize(macro_table1()))
  })
  
  macro_table2 <- reactive({
    analyze(reactive_table$table) %>%
      mutate(is1 = lapply(cuisine, element, input$cuisine1_name)) %>%
      filter(is1 == FALSE) %>%
      summarize(cuisine = input$cuisine2_name, carb = mean(carb, na.rm = TRUE),
                fat = mean(fat, na.rm = TRUE), protein = mean(protein, na.rm = TRUE))
  })
  
  output$macro_pie2 <- renderPlot({
    test(visualize(macro_table2()))
  })
  
  output$macro_table <- renderDataTable({
    bind_rows(macro_table1(), macro_table2())
  })
  
  macro_nut1 <- reactive({
    macro_table1() %>%
      visualize() %>%
      filter(value == max(value)) %>%
      select(key)
  })
  
  macro_nut2 <- reactive({
    value <- macro_table2() %>%
      visualize() %>%
      filter(value == max(value)) %>%
      select(key)
  })
  
  macro_comments1 <- reactive({
    comments <- list(fat = "weight gain or constipation", 
                     carb = "diabetes or obesity",
                     protein = "high cholesterol or kidney problems")
    if(macro_nut1() == "fat") {
      return(comments$fat)
    } else if(macro_nut1 == "carb") {
      return(comments$carb)
    } else {
      return(comments$protein)
    }
  })
  
  macro_comments2 <- reactive({
    comments <- list(fat = "weight gain or constipation", 
                     carb = "diabetes or obesity",
                     protein = "high cholesterol or kidney problems")
    if(macro_nut2() == "fat") {
      return(comments$fat)
    } else if(macro_nut2() == "carb") {
      return(comments$carb)
    } else {
      return(comments$protein)
    }
  })
  
  output$macro_summary <- renderText({
    text <- paste0(
      input$cuisine1_name, " cuisine is high in ", macro_nut1(), "s. ",
      "Diets high in ", macro_nut1(), "s may lead to ", macro_comments1(), ". "
    )
    if(macro_nut2() == macro_nut1()) {
      text <- paste0(text, "The same if true of ",
                     input$cuisine2_name, " cuisine." )
    } else {
      text <- paste0(
        text, input$cuisine2_name, " cuisine is high in ", macro_nut2(), "s. ",
        "Diets high in ", macro_nut2(), "s may lead to ", macro_comments2(), ". "
      )
    }
  })
}

# Create a new app
shinyApp(ui = ui, server = server)