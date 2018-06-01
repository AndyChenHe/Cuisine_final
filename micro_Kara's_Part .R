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

#filters basic table for only micro nutrient contents 
process_basic_table <- function(basic_table, cuisine1, cuisine2) {
micro_table <- basic_table
micro_table_2 <- micro_table %>% 
  select(
    attributes.cuisine, `Calcium, Ca ( in gram )`, `Folate, total ( in gram )`,
    `Magnesium, Mg ( in gram )`, `Potassium, K ( in gram )`, `Sodium, Na ( in gram )`, 
    `Sugars, total ( in gram )`) 

micro_table_2
}

#creates a summary table with average values of each micro nutrient
summ_table <- function(micro_table2) {
  summ_micro_table_2 <- summarise(micro_table2, Calcium = mean(micro_table2$`Calcium, Ca ( in gram )`, na.rm = TRUE),
                                 Folate = mean(micro_table2$`Folate, total ( in gram )`, na.rm = TRUE),
                                 Magnesium = mean(micro_table2$`Magnesium, Mg ( in gram )`, na.rm = TRUE),
                                 Potassium = mean(micro_table2$`Potassium, K ( in gram )`, na.rm = TRUE),
                                 Sodium = mean(micro_table2$`Sodium, Na ( in gram )`, na.rm = TRUE),
                                 Sugar = mean(micro_table2$`Sugars, total ( in gram )`, na.rm = TRUE)
                                 
  )
  summ_micro_table_2
  micro_nutrition_sum_table}


# Creates bar chart with Micro-nutrient data 
micro_vis <- function(micro_table_2) {
  #converts wide data to long data 
  micro_table2_long <- gather(micro_table_2, 
                             key = nutrition,
                             value = value_in_gram,
                             Calcium, Folate, Magnesium, Potassium,
                             Sodium, Sugar
                            )
  #creates bar graph 
  sum_bar <- ggplot(micro_table2_long) +
    geom_bar( mapping = aes(x = nutrition  , y = value_in_gram, fill = nutrition), stat = "identity") +
    labs(
      #labels x and y axis of graph 
      title = "Micronutrient Contents in Cuisines", 
      y = "Nutrient Levels", 
      x = "Micronutrient" 
    )
    sum_bar
}

