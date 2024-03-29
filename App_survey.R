library(shiny)
library(shinysurveys)
library(tidyverse)
library(googlesheets4)
library(googledrive)

############################################################################################################################################################

# The components of a shiny surveys are just the data.frame for question + user interface + the server functions + a call to the shinyApp function
# I use hashtags to break apart mini-sections like this, and dashes to break apart where we define the ui and server

############################################################################################################################################################

# We need to authenticate with a token to access Google API
## In console:
## setwd(your/directory/here)
## gs4_auth(email = "jannahmoussaoui@gmail.com", cache = ".secrets")
## A .secrets repo will be created. Don't git commit this!
## Replace below with your own email


# On JTramer's blog, not sure this bit is *actually* needed
# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = TRUE,
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = "nasa/.secrets"
# )
# 

gs4_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com")
drive_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com")

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#-------DEFINE THE QUESTIONS

# Create questions
## ShinySurveys expects questions to be in a data frame
## For simplicity, we'll create a data frame per question type
## Then, we'll bind these together

df1 <- data.frame(question = "Are you completing this as an individual or with a group?",
                  option = t(c("Individual", "Group")),
                  input_type = "y/n",
                  input_id = "individual",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE) %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option") %>% 
  select(-name)
df2 <- data.frame(question = "What group are you a part of?",
                  option = t(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")),
                  input_type = "select",
                  input_id = "group",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE) %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option") %>% 
  select(-name)

df3 <- data.frame(question = "What secret alias would you like to use? This will be displayed on a graph.",
                  option = "e.g., Clark Kent",
                  input_type = "text",
                  input_id = "code_name",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE) 

df4 <- data.frame(
  question = rep(
    c(
      "Rank the item: Box of matches", "Rank the item: Food concentrate", "Rank the item: Fifty feet of nylon rope",
      "Rank the item: Parachute silk", "Rank the item: Portable heating unit", "Rank the item: Two .45 caliber pistol",
      "Rank the item: One case of dehydrated milk", "Rank the item: Two 100 lb. tanks of oxygen",
      "Rank the item: Stellar map", "Rank the item: Self-inflating life raft", "Rank the item: Magnetic compass",
      "Rank the item: Twenty liters of water", "Rank the item: Signal flares",
      "Rank the item: First aid kit, including injection needle",
      "Rank the item: Solar-powered FM receiver-transmitter"
    ), each = 15
  ),
  option = rep(
    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"), times = 15
  ),
  input_type = rep("matrix", 225),
  input_id = rep(
    paste("nasa_matrix", rep(1:15, each = 15)), times = 15
  ),
  dependence = NA,
  dependence_value = NA,
  required = TRUE
)


# Now we put them together
df <- bind_rows(df1, df2, df3, df4) 

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#-------DEFINE THE UI

ui <- fluidPage(
  surveyOutput(df,
               survey_title = "NASA Exercise: Survival on the Moon",
               survey_description = "You are a member of a space crew originally scheduled to rendezvous with a mother 
               ship on the lighted surface of the moon. However, due to mechanical difficulties, your ship was forced to land at 
               a spot some 200 miles from the rendezvous point. During reentry and landing, much of the equipment aboard was 
               damaged and, since survival depends on reaching the mother ship, the most critical items available must be chosen
               for the 200-mile trip. Below are listed the 15 items left intact and undamaged after landing. Your task is to rank 
               order them in terms of their importance for your crew in allowing them to reach the rendezvous point. Place the 
               number 1 by the most important item, the number 2 by the second most important, and so on through number 15 for
               the least important",
               theme = "#000000")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#-------DEFINE THE UI
server <- function(input, output, session) {
  renderSurvey()
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Your response has been recorded!",
      "Feel free to exit the tab."
    ))
    response_data <- getSurveyData()
    
    # Use googlesheets4 functions to append to the sheet
    sheet_id <- "1XvwU5RxdHTjB_kiEZeGXBxE_3s46905HjsPilRfMZ2g" #ID is pulled from the URL of the spreadsheet, but we could also use the URL
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

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#-------CALL SHINYAPP FUNCTION
shinyApp(ui, server)

