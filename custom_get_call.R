apikey <- "6173072958d3d557adbdf7115bf3a8c"
app_id <- "cfbca588"
base_url <- paste0("http://api.yummly.com/v1/api/recipes?_app_id=", app_id,
                   "&_app_key=", apikey, "&")
params <- c("q", "allowedIngredient[]", "excludedIngredient[]",
            "allowedAllergy[]", "allowedDiet[]", "allowedCuisine[]",
            "excludedCuisine[]", "allowedCourse[]", "excludedCourse[]",
            "maxTotalTimeInSeconds", "nutrition", "maxResult", "start", "flavor")

c(input$search, , , , , , , , , , , , , , )

# Create vector "input" corresponding to params
# Call custom "get" function w/ "input
### Create list of parameters w/ params as keys and "input" as values
##### Use list() in conjuection setNames()
### Iterate over list
##### Check value is not blank
##### Append formatted key and value to each other
##### Store all as one string
### Append key/values to base url
### Make GET call to API and return data
# Use in tab visualizations/writeups

p <- setNames("allowedCourse[]", "dinner")
# list w/ allowedCourse[]="dinner"

# inside for loop of i
# i = 8
paste0(base_url, params[8], "=course^course-", p[params[8]])
# return "http://api.yummly.com/v1/api/recipes?_app_id=YOUR_ID&_app_key=YOUR_APP_KEY&q=onion+soup
# &allowedCourse[]=course^course-Appetizers