library(shiny)
library(shinysurveys)
library(tibble)
library(googlesheets4)
library(googledrive) # Not actually using this package

# In console: gs4_auth(email = "jannahmoussaoui@gmail.com", cache = ".secrets")
## A .secrets repo will be created. Do not share. Replace with your own email
gs4_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com")
drive_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com") # Not using this package

# Create questions
## ShinySurveys expects these to be in a data frame
df <- data.frame(
  question = c("Session ID:", 
               "Code name:",
               "Group:",
               rep("Box of matches", 15),
               rep("Food concentrate", 15),
               rep("50 feet of nylon rope", 15),
               rep("Parachute silk", 15),
               rep("Portable heating unit", 15),
               rep("Two .45 caliber pistol", 15),
               rep("One case of dehydrated milk", 15),
               rep("Two 100 lb. tanks of oxygen", 15),
               rep("Stellar map", 15),
               rep("Self-inflating life raft", 15),
               rep("Magnetic compass", 15),
               rep("20 liters of water", 15),
               rep("Signal flares", 15),
               rep("First aid kit, including injection needle", 15),
               rep("Solar-powered FM receiver-transmitter", 15)),
  option = c(NA, 
             NA,
             NA,
             rep(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"), 15)),
  input_type = c("text", 
                 "text",
                 "text",
                 rep("matrix", 225)),
  input_id = c("session_id", "code_name", "group", rep("nasa_matrix", 225)),
  dependence = NA,
  dependence_value = NA,
  required = TRUE
)

# Define the user interface
## No free-floating text box argument so use survey_description for matrix question
ui <- fluidPage(
  surveyOutput(df,
               survey_title = "NASA Exercise: Survival on the Moon",
               survey_description = "You are a member of a space crew originally scheduled to rendezvous with a mother ship on the lighted surface of the moon. However, due to mechanical difficulties, your ship was forced to land at a spot some 200 miles from the rendezvous point. During reentry and landing, much of the equipment aboard was damaged and, since survival depends on reaching the mother ship, the most critical items available must be chosen for the 200-mile trip. Below are listed the 15 items left intact and undamaged after landing. Your task is to rank order them in terms of their importance for your crew in allowing them to reach the rendezvous point. Place the number 1 by the most important item, the number 2 by the second most important, and so on through number 15 for the least important",
               theme = "#000000")
)

# Define the server  
server <- function(input, output, session) {
  renderSurvey()
  observeEvent(input$submit, {
    response_data <- tibble(
      Session_ID = input$session_id,
      Code_name = input$code_name,
      Group = input$group,
      NASA_Matrix = as.character(input$nasa_matrix)
    )
    print(response_data)

    # Use googlesheets4 functions to append to the sheet
    sheet_id <- "1XvwU5RxdHTjB_kiEZeGXBxE_3s46905HjsPilRfMZ2g"
    sheet_name <- "raw_data"
    
    # Find the sheet or create a new one
    ss <- gs4_find(sheet_name)
    if (is.null(ss)) {
      ss <- gs4_create(sheet_name)
      sheet_write(response_data, ss = ss, sheet = sheet_name)
    } else {
      # Append the data to the sheet
      sheet_append(ss, response_data)
    }
  })
}

# Here is our app
shinyApp(ui, server)
