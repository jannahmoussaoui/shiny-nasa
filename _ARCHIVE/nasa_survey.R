library(shiny)
library(shinysurveys)

if (interactive()) {
  df <- data.frame(
    question = c("Session ID:",
                 "Users",
                 "Code name:",
                 "Group:", 
                 "Box of matches",
                 "Food concentrate",
                 "50 feet of nylon rope",
                 "Parachute silk",
                 "Portable heating unit",
                 "Two .45 caliber pistol",
                 "One case of dehydrated milk",
                 "Two 100 lb. tanks of oxygen",
                 "Stellar map",
                 "Self-inflating life raft",
                 "Magnetic compass",
                 "20 liters of water",
                 "Signal flares",
                 "First aid kit, including injection needle",
                 "Solar-powered FM receiver-transmitter"), 
    option = NA,
    input_type = "text", 
    input_id = c("session_id",
                 "users",
                 "code_name",
                 "group",
                 "Q2_1",
                 "Q2_2",
                 "Q2_3",
                 "Q2_4",
                 "Q2_5",
                 "Q2_6",
                 "Q2_7",
                 "Q2_8",
                 "Q2_9",
                 "Q2_10",
                 "Q2_11",
                 "Q2_12",
                 "Q2_13",
                 "Q2_14",
                 "Q2_15"),
    dependence = NA,
    dependence_value = NA,
    required = TRUE
  )
  
  ui <- fluidPage(
    surveyOutput(df,
                 survey_title = "NASA Exercise: Survival on the Moon",
                 survey_description = "You are a member of a space crew originally scheduled to rendezvous with 
                 a mother ship on the lighted surface of the moon. However, due to mechanical difficulties, your
                 ship was forced to land at a spot some 200 miles from the rendezvous point. During reentry and 
                 landing, much of the equipment aboard was damaged and, since survival depends on reaching the
                 mother ship, the most critical items available must be chosen for the 200-mile trip. Below are 
                 listed the 15 items left intact and undamaged after landing. Your task is to rank order them 
                 in terms of their importance for your crew in allowing them to reach the rendezvous point. Place
                 the number 1 by the most important item, the number 2 by the second most important, and so on 
                 through number 15 for the least important",
                 theme = "#000000")
  )
  
  server <- function(input, output, session) {
    renderSurvey()
    observeEvent(input$submit, {
      raw_data <- getSurveyData()
      print(raw_data)
      write.csv(raw_data, "raw_data.csv")
    })
  }
  shinyApp(ui, server)
}