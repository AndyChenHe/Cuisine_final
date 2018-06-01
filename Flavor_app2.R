library(dplyr)
library(fmsb)
################################
#filters 1st cuisine for flavors
flavor_process_basic_table1 <- function(basic_table, cuisine1) {
  flavor_table <- basic_table
  flavor_table <- flavor_table %>% 
    select(
      attributes.cuisine, flavors.piquant, flavors.meaty, flavors.sweet, flavors.bitter, 
      flavors.sour, flavors.salty) 
  
  flavor_table
}


#creates a summary table of first cuisine with average values of each flavor
flavor_summ_table_first <- function(flavor_table) {
  flavor_table_first_sum <- summarise(
                                      flavor_table, piquant = mean(flavor_table$"flavors.piquant", na.rm = TRUE),
                                      meaty = mean(flavor_table$"flavors.meaty", na.rm = TRUE),
                                      sweet = mean(flavor_table$"flavors.sweet", na.rm = TRUE),
                                      bitter = mean(flavor_table$"flavors.bitter", na.rm = TRUE), 
                                      sour = mean(flavor_table$"flavors.sour", na.rm = TRUE),
                                      salty = mean(flavor_table$"flavors.salty", na.rm = TRUE) 
  )
  flavor_table_first_sum
}

################################
#filters 2nd cuisine for flavors
flavor_process_basic_table2 <- function(basic_table, cuisine2) {
  flavor_table <- basic_table
  flavor_table_second <- flavor_table %>% 
    select(
      attributes.cuisine, flavors.piquant, flavors.meaty, flavors.sweet, flavors.bitter, 
      flavors.sour, flavors.salty) 
  
  flavor_table_second
}

#creates a summary table of second cuisine with average values of each flavor
flavor_summ_table_second <- function(flavor_table_second) {
  flavor_summ_table2 <- summarise(
                                  flavor_table_second, piquant = mean(flavor_table_second$"flavors.piquant", na.rm = TRUE),
                                  meaty = mean(flavor_table_second$"flavors.meaty", na.rm = TRUE),
                                  sweet = mean(flavor_table_second$"flavors.sweet", na.rm = TRUE),
                                  bitter = mean(flavor_table_second$"flavors.bitter", na.rm = TRUE), 
                                  sour = mean(flavor_table_second$"flavors.sour", na.rm = TRUE),
                                  salty = mean(flavor_table_second$"flavors.salty", na.rm = TRUE)
                       
  )
  flavor_summ_table2
}

cuisine2_spider <- function(flavor_table_second) {
  
  # The default radar chart proposed by the library:
  radarchart(flavor_table_second, cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,.2))
  
  par(mar=c(1, 2, 2, 1)) #decrease default margin
  #loop over rows to draw them, add 1 as max and 0 as min for each var
  lapply(1:4, function(i) { 
    radarchart(rbind(rep(1,6), rep(0,6), flavor_table_second[i,-1]), 
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,.2)
    )
  })
}
