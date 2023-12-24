library(shiny)
library(tibble)
library(shinyjs)
library(googlesheets4)
library(ggplot2)
library(tidyr)
library(dplyr)

gs4_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com")

app <- "http://jannahmoussaoui.shinyapps.io/NASA_Survey"
user <- "/?user_id="

# Define UI
ui <- navbarPage(
  "NASA Moon Survival",
  tabsetPanel(
    tabPanel("Step 1",
             HTML("<h3>Introduction</h3>
                  <p>Decision making is challenging. We often rely on others' judgments--or at least solicit advice--when we're making challenging decisions. But is this the best approach? 
                  This shiny app recreates the procedures employed by Hamada et al. (2020) to provide an interactive demonstration comparing the normativity of decisions when made in solitude, 
                  collaborations, and by aggregation. This activity was designed to be implemented in a synchronous classroom setting.</p>
                  <p>Before we can get started, we'll need to give this classroom a session name. We will also need to know who is present.</p>"),
             HTML("<h3>Session name</h3>"),
             textInput("session_name", "What do you want to name this session? We recommend using something relatively unique to distinguish this session from others.", placeholder = "e.g., course CRN"),
             HTML("<h3>Participant names</h3>"),
             textInput("participant_names", "Please list the names of all the people who will be, participating in this activity. Separate names with commas.", placeholder = "e.g., Jannah, Fatima, Ahmed, Aminah...")
    ),
    tabPanel("Step 2",
             HTML("<h3>Instructions</h3>
                  <p>Everyone will be completing this activity twice: first individually and then within a group. We have already grouped everyone up, as seen in the table below. Remember 
                  the group to which you have been assigned as you will need to provide this in the survey.</p>
                  <p>Once everyone has completed the task by themselves, you may group up and complete the activity a second time, albeit with the ability to consult group members.</p>
                  <h3>Groups</h3>"),
             tableOutput("groupTable"),
             HTML("<h3>Activity Link</h3>
                  <p>To access the activity, follow the link below."),
             htmlOutput("surveyLink")
    ),
    tabPanel("Step 3",
             actionButton("generateResultsBtn", "Generate Results"),
             tableOutput("generatedResults")
    )
  ),
  theme = bslib::bs_theme(bootswatch = "darkly")
)

# Define server
server <- function(input, output, session) {
  # Reactive value to store session members
  session_members <- reactiveVal(NULL)
  
  observeEvent(input$participant_names, {
    # Get participant names and store in session_members
    session_members(strsplit(input$participant_names, ",")[[1]])
    
    # Check if there are at least 3 participants
    if (!is.null(session_members()) && length(session_members()) <= 2) {
      showNotification("You need at least 3 participants!", type = "error", duration = 5)
    }
  })
  
  observeEvent(input$session_name, {
    # Create the survey link when the session_name is provided
    if (!is.null(input$session_name)) {
      link <- paste0(app, user, input$session_name)
      output$surveyLink <- renderUI({
        HTML(paste(tags$a(href = link, target = "_blank", link)))
      })
    }
  })
  
  observeEvent(input$participant_names, {
    # Generate groups when the session_name is provided
    if (!is.null(session_members()) && length(session_members()) >= 3) {
      participants <- session_members()
      num_participants <- length(participants)
      
      # Group size range,
      group_size <- 3
      
      # Calculate the number of participants to assign to each group
      participants_per_group <- rep(group_size, floor(num_participants / group_size))
      remainder <- num_participants %% group_size
      
      # Distribute remaining participants to the first group(s) (only if remainder > 0)
      if (remainder > 0) {
        for (i in 1:remainder) {
          participants_per_group[i] <- participants_per_group[i] + 1
        }
      }
      
      # Randomly assign participants to groups
      set.seed(1) # Set seed for reproducibility 
      group_assignment <- rep(LETTERS[1:length(participants_per_group)], times = participants_per_group)
      
      # Create a tibble with group names and participants
      result <- tibble(Group = LETTERS[1:length(participants_per_group)], Participants = NA_character_)
      
      # Fill the Participants column with assigned names
      for (i in 1:length(participants_per_group)) {
        group_participants <- participants[group_assignment == LETTERS[i]]
        result$Participants[i] <- paste(group_participants, collapse = ", ")
      }
      
      # Output the result as a reactive value
      output$groupTable <- renderTable({
        result
      }, striped = TRUE, hover = TRUE)
    }
  })
  
  observeEvent(input$generateResultsBtn, {
    # Load data from Google Sheets
    raw_data <- read_sheet("1XvwU5RxdHTjB_kiEZeGXBxE_3s46905HjsPilRfMZ2g")
    raw_data <- raw_data[, !names(raw_data) %in% "question_type"]
    
    raw_data_wide <- raw_data %>%
      pivot_wider(
        names_from = question_id,
        values_from = response,
        values_fn = list(response = list)
      ) %>%
      unnest(cols = c(individual, group, code_name, matches, food, nylon, silk, heating, pistol, milk, oxygen, map, raft, compass, water, flare, injection, receiver)) %>%
      select(code_name, everything()) %>%
      rename(session = subject_id)
    
    session_raw_data_wide <- dplyr::filter(raw_data_wide, session == input$session_name)
    
    # Display the result in the Shiny app
    output$generatedResults <- renderTable({
      session_raw_data_wide
    }, striped = TRUE, hover = TRUE)
  })
}

# Run the app
shinyApp(ui, server)
