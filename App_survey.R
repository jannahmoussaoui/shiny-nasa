library(tidyr)
library(dplyr)
library(shiny)
library(shinysurveys)
library(tibble)
library(googlesheets4)
library(googledrive)

############################################################################################################################################################

# We need to authenticate with a token to access Google API
## In console:
## setwd(app_directory_here)
## gs4_auth(email = "jannahmoussaoui@gmail.com", cache = ".secrets")
## A .secrets repo will be created. Don't git commit this!
## Replace below with your own email

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "nasa/.secrets"
)
gs4_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com")
drive_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com")

############################################################################################################################################################

# Create questions
## ShinySurveys expects these to be in a data frame
## There's a way to do this with matrix-ing and an actual matrix input
## But for whatever reason, it renders the getSurvetData() function useless
## So I will just bind a bunch of dataframes together...it's inefficient but it works

df1 <- data.frame(question = "Are you completing this as an individual or with a group?",
                  option = t(c("Individual", "Group")),
                  input_type = "y/n",
                  input_id = "individual",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE) %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")  
df2 <- data.frame(question = "What group are you a part of?",
                  option = t(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")),
                  input_type = "select",
                  input_id = "group",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE) %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option") 

df3 <- data.frame(question = "What secret alias would you like to use? This will be displayed on a graph.",
                  option = "e.g., Clark Kent",
                  input_type = "text",
                  input_id = "code_name",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE) 

df4 <- data.frame(question = "Rank the item: Box of matches",
                  option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                  input_type = "select",
                  input_id = "matches",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option") 

df5 <- data.frame(question = "Rank the item: Food Concentrate",
                  option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                  input_type = "select",
                  input_id = "food",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option") 

df6 <- data.frame(question = "Rank the item: 50 feet of nylon rope",
                  option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                  input_type = "select",
                  input_id = "nylon",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option") 

df7 <- data.frame(question = "Rank the item: Parachute silk",
                  option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                  input_type = "select",
                  input_id = "silk",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option") 

df8 <- data.frame(question = "Rank the item: Portable heating unit",
                  option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                  input_type = "select",
                  input_id = "heating",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option") 

df9 <- data.frame(question = "Rank the item: Two .45 caliber pistol",
                  option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                  input_type = "select",
                  input_id = "pistol",
                  dependence = NA,
                  dependence_value = NA,
                  required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

df10 <- data.frame(question = "Rank the item: One case of dehydrated milk",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "milk",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

df11 <- data.frame(question = "Rank the item: Two 100 lb. tanks of oxygen",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "oxygen",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

df12 <- data.frame(question = "Rank the item: Stellar map",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "map",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

df13 <- data.frame(question = "Rank the item: Self-inflating life raft",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "raft",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

df14 <- data.frame(question = "Rank the item: Magnetic compass",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "compass",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

df15 <- data.frame(question = "Rank the item: 20 liters of water",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "water",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")


df16 <- data.frame(question = "Rank the item: Signal flares",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "flare",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

df17 <- data.frame(question = "Rank the item: First aid kit, including injection needle",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "injection",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

df18 <- data.frame(question = "Rank the item: Solar-powered FM receiver-transmitter",
                   option = t(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")),
                   input_type = "select",
                   input_id = "receiver",
                   dependence = NA,
                   dependence_value = NA,
                   required = TRUE
)  %>%
  pivot_longer(cols = starts_with("option"),
               values_to = "option")

# Now we put them together
df <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18) 

########################################################################################################################################################
# Define the user interface
## Use survey_description for select question
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
########################################################################################################################################################
## Define the server
server <- function(input, output, session) {
  renderSurvey()
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Your response has been recorded!",
      "Feel free to exit the tab."
    ))
    response_data <- getSurveyData()
    
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

