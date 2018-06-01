library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
source("create_table.R")
source("andy_part.R")
source("micro_Kara's_Part .R")
source("./macro_support.R")
source("Flavor_app2.R")

# Create a list for control bar's choices
cuisine_list <- c("American", "Italian", "Asian", "Mexican", "Southern & Soul Food", "French", 
                  "Southwestern", "Barbecue", "Indian", "Chinese", "Cajun & Creole", "English", 
                  "Mediterranean", "Greek", "Spanish", "German", "Thai", 
                  "Moroccan", "Irish", "Japanese", "Cuban", "Hawaiin", "Swedish", "Hungarian", "Portugese")
cuisine_list2 <- c("Italian", "American", "Asian", "Mexican", "Southern & Soul Food", "French", 
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
  
  theme = shinytheme("yeti"),
  
  titlePanel("Cuisine test"),
  sidebarLayout(
    # Control panels
    sidebarPanel(
      # User can use this drop down menu to select the state they want to see
      selectInput("cuisine1_name", label="Cuisine 1", 
                  choices= unique(cuisine_list), selected = "American"),
      selectInput("cuisine2_name", label="Cuisine 2", 
                  choices= unique(cuisine_list2), selected = "Asian"),
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
                  # This tab is for genreal information
                  tabPanel("General",
                           h1(
                             "General information of your choice"
                           ),
                           textOutput("general_description"),
                           dataTableOutput("data")
                  ),
                  #
                  tabPanel("Flavor",
                           h3("Flavor Of Each Cuisine"),
                           p("Our group wanted to look at the differences between flavors from each cuisine. Depending on what
                             the filters you select from left panel, the flavor averages will change as you cycle through the
                             options; for example, if you select a course, such as dessert, the sweet flavor averages will be
                             higher than other categories. Play around with the filter panel and check out each diet, course,
                             and allergy filters."),
                           dataTableOutput("Flavor_table_sum1"),
                           dataTableOutput("Flavor_Table_First"),
                           dataTableOutput("Flavor_table_sum2"),
                           dataTableOutput("Flavor_Table_Second")
                           ),
                  # This tab is for table and analysis for Macronutrition
                  tabPanel("Macronutrition",
                           plotOutput("macro_pie1"),
                           plotOutput("macro_pie2"),
                           dataTableOutput("macro_table"),
                           h2("Diet Advisory"),
                           textOutput("macro_summary")
                  ),
                  
                  # This tab is for table and analysis for Micronutrition 
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
  
  ####################################################
  ####################################################
  
  #renders table with all microbutrient contents of each cuisine 
  output$Flavor_Table_First <- renderDataTable({
    #creates basic table for cuisine 1
    reactive_table$Flavors_table1 <- flavor_process_basic_table1(reactive_table$table, input$cuisine1_name)
    
    reactive_table$Flavors_table1  
  })
  
  output$Flavor_Table_Second <- renderDataTable({
    #creates basic table for cuisine 1
    reactive_table$Flavors_table2 <- flavor_process_basic_table2(reactive_table$table, input$cuisine2_name)
    
    reactive_table$Flavors_table2  
  })
  #renders summary table of flavors from each cuisine 
  output$Flavor_table_sum1 <- renderDataTable({
    #creates summary table for cuisine 1
    reactive_table$Flavors_sum_table1 <- flavor_summ_table_first(reactive_table$Flavors_table1) 
    
    reactive_table$Flavors_sum_table1
  })
  output$Flavor_table_sum2 <- renderDataTable({
    #creates summary table for cuisine 2
    reactive_table$Flavors_sum_table2 <- flavor_summ_table_second(reactive_table$Flavors_table2) 
    
    reactive_table$Flavors_sum_table2
  })

  #################################################### 
  ####################################################
  # Kara workds here r
  
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
    bar_for_micro <- micro_vis(reactive_table$micro_nutrition_sum_table)
    bar_for_micro
  })
  
  # Macro (Brendan)
  
  # Table of means of cuisine 1 values
  macro_table1 <- reactive({
    analyze(reactive_table$table) %>%
      mutate(is1 = lapply(cuisine, element, input$cuisine1_name)) %>%
      filter(is1 == TRUE) %>%
      summarize(cuisine = input$cuisine1_name, carb = mean(carb, na.rm = TRUE),
                fat = mean(fat, na.rm = TRUE), protein = mean(protein, na.rm = TRUE))
  })
  
  # Pie chart of cuisine 1 values
  output$macro_pie1 <- renderPlot({
    test(visualize(macro_table1()))
  })
  
  # Table of means of cuisine 1 values
  macro_table2 <- reactive({
    analyze(reactive_table$table) %>%
      mutate(is1 = lapply(cuisine, element, input$cuisine1_name)) %>%
      filter(is1 == FALSE) %>%
      summarize(cuisine = input$cuisine2_name, carb = mean(carb, na.rm = TRUE),
                fat = mean(fat, na.rm = TRUE), protein = mean(protein, na.rm = TRUE))
  })
  
  # Pie chart of means of cuisine 2 values
  output$macro_pie2 <- renderPlot({
    test(visualize(macro_table2()))
  })
  
  # Table of mean cuisine values
  output$macro_table <- renderDataTable({
    bind_rows(macro_table1(), macro_table2())
  })
  
  # Cuisine 1's prevalent nutrient
  macro_nut1 <- reactive({
    macro_table1() %>%
      visualize() %>%
      filter(value == max(value)) %>%
      select(key)
  })
  
  # Cuisine 2's prevalent nutrient
  macro_nut2 <- reactive({
    value <- macro_table2() %>%
      visualize() %>%
      filter(value == max(value)) %>%
      select(key)
  })
  
  # Generate comment on cuisine 1's prevalent nutrient
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
  
  # Generate comment on cuisine 2's prevalent nutrient
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
  
  # Paragraph of information on prevalent nutrients in either cuisine
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

#Create a new app
shinyApp(ui = ui, server = server)