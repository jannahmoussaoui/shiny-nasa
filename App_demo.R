library(shiny)
library(tibble)
library(shinyjs)
library(googlesheets4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(cowplot)

############################################################################################################################################################
# this assumes you have .secrets in the same directory and therefore don't need to reauthenticate
gs4_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com")
############################################################################################################################################################

# Link to the live survey
app <- "https://jannahmoussaoui.shinyapps.io/survey/"
app_no <- "A link will be available once you provide a name for the session."
user <- "?user_id="

############################################################################################################################################################

# Define UI
ui <- navbarPage(
  "Demo", # This will be the title on the tab we open
  tabsetPanel(
    tabPanel("Introduction",
             HTML("<h4>Background</h4>
                  <p>Making decisions, especially under certainty, is challenging. Sometimes, we might decide to ask others for advice. 
                  But is this the best approach? This shiny app recreates the procedures employed by Hamada et al. (2020) to provide an
                  interactive demonstration comparing the normativity of decisions when made individually, in groups, and through the 
                  wisdom of crowds. To do this, everyone will complete a short activity called the 'NASA Exercise: Survival on the Moon'
                  by themselves. Then, we'll split everyone into groups and have everyone complete the task again. Please note that this 
                  activity was designed to be implemented in a synchronous classroom setting but can likely be adapted to work in other contexts.</p>
                  <br></br>
                  <h4>How to Cite This</h4>"),
    ),
    tabPanel("Instructions",
             HTML("<h4>Step 1: Come up with a session name</h4>
                  <p>First, we need to pick out a relatively unique name for this session. This will allow us to send everyone to the same link.</p>"),
             textInput("session_name", "Session Name:", placeholder = "e.g., course CRN"),
             HTML("<br></br>
                  <h4>Step 2: List out the particiapnts</h4>
                  <p>Now, we need to know how many people are going to complete this activity. We'll use this list to help divide people into groups.
                  <b>Separate names with commas.</b></p>"),
             textInput("participant_names", "Participants:", width = "50%", placeholder = "e.g., Jannah, Jason..."),
             HTML("<br></br>
                  <h4>Step 3: Number of Groups</h4>
                  <p>This activity works best with groups of 3-4 people but we can make groups as big or as small as we like."),
             sliderInput("num_groups", "Number of Groups:",
                         min = 1, max = 3, value = 2),  # Set an initial max value just to satisfy the function. This basically gets overwritten instantly
             HTML("<br></br>
                  <h4>Step 4: View groups</h4>"),
             tableOutput("groupTable"),
             HTML("<br></br>
                  <h4>Step 5: Complete the acitvity individually.</h4>
                  <p>The link below will redirect you to the NASA Moon Survival Task. Follow the instructions provided in the link.</p>"),
             htmlOutput("surveyLink"),
             HTML("<br></br>
                  <h4>Step 6: Getting into groups</h4>
                  <p>Once everyone has completed the survey, get into groups and designate one device for recording the responses. You'll notice the link 
                  is the same. Be sure to indicate that you are completing this as a group!</p>"),
             htmlOutput("surveyLink2")
    ),
    tabPanel("See the results",
             actionButton("plotResultsBtn", "Plot the results"),
             plotOutput("plotResults")
    )
  ),
  theme = bslib::bs_theme(bootswatch = "flatly")
)

############################################################################################################################################################

# Define server
server <- function(input, output, session) {
# Create the survey link when the session_name is provided; need an if else because 
## we don't want to direct to the link if the session_name is empty
  observeEvent(input$session_name, {
    if (!is.null(input$session_name) && input$session_name !="") {
      link <- paste0(app, user, input$session_name)
    } else {
      link <- app_no
    }
      output$surveyLink <- renderUI({
        HTML(paste(tags$a(href = link, target = "_blank", link)))
      })
  })
  
# Duplicating the lines above because for whatever reason when we try to print the htmlOutput 
## in the ui section it interrupts the other outputs. If we decide we want to build a different
## survey for groups vs. individuals (and remove the MC item), we can make the links different
## not doing that now bc it involves a lot more lines of code than its worth for removing 1 MC item
  observeEvent(input$session_name, {
    if (!is.null(input$session_name) && input$session_name !="") {
      link <- paste0(app, user, input$session_name)
    } else {
      link <- app_no
    }
    output$surveyLink2 <- renderUI({
      HTML(paste(tags$a(href = link, target = "_blank", link)))
    })
  })
  
  
# We're going to write a function to divide people into groups
  divide_names_into_groups <- function(names, num_groups) {
    # Use sample to ensure people's group assignment is random
    shuffled_names <- sample(names)
    # Use integer division %/% to figure out the minimum number of participants in each group
    group_size <- length(shuffled_names) %/% num_groups
    # It's likely things won't divide perfectly so calculate the remainder
    remainder <- length(shuffled_names) %% num_groups
    # Create an empty list to store groups. We'll need this "container" so the for loop can assign people to the corresponding groups
    groups <- vector("list", length = num_groups)
    # To figure out when we've finished distributing the remainders, we need to track our current position. Start at 1
    current_position <- 1
    # We will use a for loop over the groups
    for (i in 1:num_groups) {
      # If i <= remainder, we're dealing with a group that should get an additional member. Otherwise, we've moved past the initial groups and add 0.
      current_size <- group_size + ifelse(i <= remainder, 1, 0)
      # Assign names using our shuffled vector
      groups[[i]] <- shuffled_names[current_position:(current_position + current_size - 1)] # we started at 1 so subtract 1
      # Update the current position in the shuffled list for the next iteration
      current_position <- current_position + current_size
    }
    # return groups
    return(groups)
  }
  
  
  # Dynamically update the max value of the slider based on the number of people
  observe({
    names_count <- length(strsplit(input$participant_names, ",")[[1]])
    updateSliderInput(session, "num_groups", max = names_count)
  })
  
  groups_data <- reactive({
    # Extract names and number of groups from input
    names_vector <- strsplit(input$participant_names, ",")[[1]]
    num_groups <- input$num_groups
    # Call the group division function
    groups <- divide_names_into_groups(names_vector, num_groups)
    groups
  })
  
  observe({
    # Update the table whenever the groups_data reactive changes
    output$groupTable <- renderTable({
      data.frame(Group = LETTERS[1:input$num_groups], Members = sapply(groups_data(), paste, collapse = ", "))
    })
  })
  
  
  observeEvent(input$plotResultsBtn, {
    # Load data from Google Sheets
    raw_data <- read_sheet("1XvwU5RxdHTjB_kiEZeGXBxE_3s46905HjsPilRfMZ2g") # Google sheet ID. Either request access or replace with a new sheet
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
    
    session_raw_data_wide <- dplyr::filter(raw_data_wide, session == input$session_name) # important because it hides the rest of the (non-session-relevant) data
    
    ############################################################
    # reshape the data to re-use Jason's code
    ## pasted in the sections we want
    dat <- session_raw_data_wide %>% # If manually testing the code, i.e., not launching shiny, drop "session_" and reference "raw_data_wide" here #############################################################
      rename(ID = code_name) %>%
      rename(Source = individual) %>%
      rename(Group = group) %>%
      rename("1" = matches) %>%
      rename("2" = food) %>%
      rename("3" = nylon) %>%
      rename("4" = silk) %>%
      rename("5" = heating) %>%
      rename("6" = pistol) %>%
      rename("7" = milk) %>%
      rename("8" = oxygen) %>%
      rename("9" = map) %>%
      rename("10" = raft) %>%
      rename("11" = compass) %>%
      rename("12" = water) %>%
      rename("13" = flare) %>%
      rename("14" = injection) %>%
      rename("15" = receiver) %>%
      pivot_longer(cols = 5:19) %>%
      rename(item = name)
    true_scores <- c(15, 4, 6, 8, 13, 11, 12, 1, 3, 9, 14, 2, 10, 7, 5)
    dat$value <- as.numeric(dat$value)
    dat$item <- as.numeric(dat$item)
    
    # create a column of error scores
    dat %<>% mutate(true_score = !!true_scores[dat$item], abs_error = abs(value - true_score))
    
    # aggregate scores using the Borda method
    ## just just requires us to average the item rankings from individuals,
    ## then convert those to ranks
    Borda <- dat %>% 
      group_by(Group, item) %>% 
      summarize(M = mean(value)) %>% 
      ungroup() 
    Borda %<>% reframe (Group, item, value = rank(M), by = Group) 
    
    # calculate error scors for Borda rankings, then get group averages
    Borda %<>% mutate(true_score = !!true_scores[Borda$item], error = abs(value - true_score))
    Borda %<>% group_by(Group) %>% 
     summarize(abs_error = mean(error))%>% 
     mutate(Source = "Aggregate") 
    
    # combined data will hold individual, group, and aggregate scores
    ## get summed error score by source, group, and ID
    combined_data <- dat %>% 
      group_by(Source, ID, Group) %>% 
      summarize(abs_error = sum(abs_error)) %>% ungroup()
    
    # put the Borda scores in the same format as the combined_data object
    Borda %<>% mutate(ID = str_c("agg_", "Group")) %>% 
      relocate(Source, ID, Group, abs_error) 
    combined_data %<>% rbind(Borda)
    combined_data %<>% mutate(Source = as.factor(Source) %>% 
                                fct_relevel( "Individual", "Group", "Aggregate"))
    combined_data %<>% mutate(ID_label = if_else(Source == "Individual", ID, ""))
    
    ############################################################################################
    # Make one tweak to Jason's code
    ## Remove aggregate (so it doesn't plot) for groups comprised solely of 1 person
    combined_data <- combined_data %>%
      group_by(Source, Group) %>%
      mutate(
        num_per_group = n_distinct(ID),
        num_per_source = paste(num_per_group, Source, sep = "_")
      ) %>%
      ungroup()
    
    combined_data <- combined_data %>%
      group_by(Group) %>%
      filter(!(any(num_per_source == "1_Individual") & Source == "Aggregate")) %>%
      ungroup()

    #############################################################################################
    
    # draw the plot
    draw_plot <- ggplot(combined_data, aes(x = Group, y = abs_error, fill = Source, group = ID)) +
      geom_bar(stat = "identity", position = position_dodge(.8), width = 0.75, alpha = .5, color = "lightgray") +
      geom_point(stat = "identity", aes(fill = Source), position = position_dodge(.8), shape = 21,  size = 3, show.legend = FALSE) +
      geom_text(aes(label = ID_label),
                position = position_dodge(0.8), vjust = .5, size = 4, angle = 90, hjust = -.15) +
      scale_fill_manual(values = c(Individual = "#ED2024", Group = "#FFCDD2", Aggregate = "blue")) +
      labs(title = "Error Scores for Individuals, Groups, and Aggregate", x = "Groups", y = "Absolute Error") +
      theme_minimal() +
      theme(legend.position = "top", legend.key.size = unit(20, "point"), legend.spacing.x = unit(10, "point"),
            legend.title = element_blank(), 
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 15)) +
      coord_cartesian(ylim = c(0, 145))
    
    # Going to use this to display the result in the Shiny app as ggplot
    output$plotResults <- renderPlot({
      draw_plot
    })
  })
}

# Run the app
shinyApp(ui, server)

