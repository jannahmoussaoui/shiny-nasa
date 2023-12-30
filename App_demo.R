library(shiny)
library(tibble)
library(shinyjs)
library(googlesheets4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(cowplot)
library(tidyverse)

############################################################################################################################################################

# Shiny apps are comprised of three components: a UI, server, and a call to the shinyApp function, i.e., ShinyApp(ui, server)
## We start with a little bit of set-up, and then move on to define our UI and server
## I use hashtags to break apart mini-sections like this, and dashes to divide  those more distinct components

############################################################################################################################################################

# This assumes we have .secrets in the same directory and therefore don't need to reauthenticate 
## .secrets would have been created from our intial authentification, as occured in the App_survey.R script
gs4_auth(cache = ".secrets", email = "jannahmoussaoui@gmail.com")

############################################################################################################################################################

# Link to the live survey
app <- "https://jannahmoussaoui.shinyapps.io/survey/"
no_name_provided <- "A link will be available once you provide a name for the session." # We only want people with a session name to access the task
user <- "?user_id="

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#-------DEFINE THE UI

ui <- navbarPage(
  "Demo", # This will be the title of the tab we open.
  tabsetPanel(
    tabPanel("Introduction",
             HTML("<br>
             <h5><b>Background</b></h5>
                  <p>Making decisions, especially under uncertainty, is challenging. Sometimes, to aid our decision making, we might solicit advice from others
                  or even make decisions collaboratively. In this activity, we provide a demonstration of individual and group decision making on an unfamiliar
                  task, the NASA Exercise, which presents a moon-based survival task. As in Hamada et al. (2020), we invite people to complete this task first
                  individually and then in groups.</p>
                  <p>Please note that this activity was designed to be implemented in a synchronous classroom setting.</p>
                  <br>
                  <h5><b>Source</b></h5>
                  <p><a href=https://github.com/jannahmoussaoui/shiny-nasa>GitHub Repository</a></p>"),
    ),
    tabPanel("Instructions",
             HTML("<br>
             <h5><b>Step 1: Come up with a session name</b></h5>
                  <p>First, we need to pick out a relatively unique name for this session. This will allow us to send everyone to the same link.
                  <b>Tip: note down your session name so that you may reference it as needed.</b></p>"),
             textInput("session_name", "Session Name:", placeholder = "e.g., course CRN"),
             HTML("<br>
                  <h5><b>Step 2: List out the particiapnts</b></h5>
                  <p>Now, we need to know how many people are going to complete this activity. We'll use this list to help divide people into groups.
                  <b>Separate names with commas.</b></p>"),
             textInput("participant_names", "Participants:", width = "50%", placeholder = "e.g., Jannah, Jason..."),
             HTML("<br>
                  <h5><b>Step 3: Number of Groups</b></h5>
                  <p>This activity works best with groups of 3-4 people but we can make groups as big or as small as we like."),
             # We need to set an initial max just to satisfy the required arguments.
             ## Our server is "watching" this, so it'll get overwritten instantly.
             sliderInput("num_groups", "Number of Groups:",
                         min = 1, max = 3, value = 2),
             HTML("<br>
                  <h5><b>Step 4: View groups</b></h5>"),
             tableOutput("groupTable"),
             HTML("<br>
                  <h5><b>Step 5: Complete the acitvity individually.</b></h5>
                  <p>The link below will redirect you to the NASA Moon Survival Task. Follow the instructions provided in the link.</p>"),
             htmlOutput("surveyLink"),
             HTML("<br>
                  <h5><b>Step 6: Get into groups</b></h5>
                  <p>Once everyone has completed the survey, get into groups and designate one device for recording the responses. You'll notice the link 
                  is the same. Be sure to indicate that you are completing this as a group!</p>"),
             htmlOutput("surveyLink2"),
             HTML("<br>
                  <h5><b>Step 7: View the results</b></h5>
                  <p>Now that everyone has submitted the surveys indiviudally and again in their groups, we can plot the results. Navigate to the tab labelled 'results'</p>")
    ),
    tabPanel("Results",
             HTML("<br>"),
             actionButton("plotResultsBtn", "Plot the results"),
             plotOutput("plotResults"),
             actionButton("plotScoreKeyBtn", "NASA's Error Score Interpretation"),
             tableOutput("plotScoreKey"),
             HTML("<br>"),
             actionButton("plotAnswerKeyBtn", "NASA's Answers and Explanation"),
             tableOutput("plotAnswerKey")
    )
  ),
  theme = bslib::bs_theme(bootswatch = "sandstone")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#-------DEFINE the server

server <- function(input, output, session) {

# SESSION NAME + SURVEY LINK ###########################################################################################################################################################

# Observe and observeEvent allow us to create reacitivity
## Several things should happens only when text input, slider, or button are interacted with
  
  # Create the survey link when the session_name is provided
## Use if else so that people must pick a session_name to get the link
## otherwise we throw an error
  observeEvent(input$session_name, {
    if (!is.null(input$session_name) && input$session_name !="") {
      link <- paste0(app, user, input$session_name)
    } else {
      link <- no_name_provided
    }
      output$surveyLink <- renderUI({
        HTML(paste(tags$a(href = link, target = "_blank", link)))
      })
  }) 
# Duplicate the lines above because for whatever reason when we try to print the htmlOutput 
## in the ui section it interrupts the other outputs. If we decide we want to build a different
## survey for groups vs. individuals (and remove the "Are you completing this as an individual or 
## in a group?" from the survey, we can make the links different.
  observeEvent(input$session_name, {
    if (!is.null(input$session_name) && input$session_name !="") {
      link <- paste0(app, user, input$session_name)
    } else {
      link <- no_name_provided
    }
    output$surveyLink2 <- renderUI({
      HTML(paste(tags$a(href = link, target = "_blank", link)))
    })
  })

# MAKING GROUPS ###########################################################################################################################################################

  # We're going to write a function to divide people into groups
## First, shuffle the names so that we're randomly putting people in groups
## Use integer division %/% to figure out the minimum number of participants in each group (the quotient)
## It's likely things won't divide perfectly so calculate the remainder with modulus %%
## Create an empty list to to store our groups. We'll use this as a container.
## We're going to loop to create each one of our groups, so we need to keep track of our position on the original list
## We'll use current_position and start indexing at 1 to do this
## On our first iteration, the group at the minimum will have n = quotient, and if the iteration is <= remainder, we add 1
## if the iteration (1) is greater than the remainder (0), there are no remainders, and we add 0
## Once we know the group size, we can assign names
## Then we update our current position so that the next iteration starts in the right spot and
## we can be sure we're adding people to the list container without overlap
  divide_names_into_groups <- function(names, num_groups) {
    shuffled_names <- sample(names)
    group_size <- length(shuffled_names) %/% num_groups
    remainder <- length(shuffled_names) %% num_groups
    groups <- vector("list", length = num_groups)
    current_position <- 1
    for (i in 1:num_groups) {
      current_size <- group_size + ifelse(i <= remainder, 1, 0)
      groups[[i]] <- shuffled_names[current_position:(current_position + current_size - 1)] # we started at 1 so subtract 1
      current_position <- current_position + current_size
    }
    # return groups
    return(groups)
  }
  
# We want people to select how many groups but want to ensure they choose reasonable values
## Dynamically update the max value on the slider to match the number of people
## The number of groups should never exceed the number of participants
  observe({
    names_count <- length(strsplit(input$participant_names, ",")[[1]])
    updateSliderInput(session, "num_groups", max = names_count)
  })
  
# Extract the number of people provided in the text input  
## Call the group division function
  groups_data <- reactive({
    names_vector <- strsplit(input$participant_names, ",")[[1]]
    num_groups <- input$num_groups
    groups <- divide_names_into_groups(names_vector, num_groups)
    groups
  })
  
# Update the table whenever the # of groups changes
## Letter the groups  
  observe({
    output$groupTable <- renderTable({
      data.frame(Group = LETTERS[1:input$num_groups], Members = sapply(groups_data(), paste, collapse = ", "))
    })
  })
  
# DATA COLLECTION UNDERWAY VIA APP_survey.R ################################################################################################################################
  
#PLOTTING THE DATA###########################################################################################################################################################
  
  observeEvent(input$plotResultsBtn, {
    # Load data from Google Sheets
    raw_data <- read_sheet("1XvwU5RxdHTjB_kiEZeGXBxE_3s46905HjsPilRfMZ2g") # Google sheet ID. To recreate the app, replace with a diff ID
    raw_data <- raw_data[, !names(raw_data) %in% "question_type"] #%>%
    raw_data <- raw_data %>%
      mutate(
        question_id = case_when(
          question_id == "rank_the_item_box_of_matches" ~ "matches",
          question_id == "rank_the_item_food_concentrate" ~ "food",
          question_id == "rank_the_item_fifty_feet_of_nylon_rope" ~ "nylon",
          question_id == "rank_the_item_parachute_silk" ~ "silk",
          question_id == "rank_the_item_portable_heating_unit" ~ "heating",
          question_id == "rank_the_item_two_45_caliber_pistol" ~ "pistol",
          question_id == "rank_the_item_one_case_of_dehydrated_milk" ~ "milk",
          question_id == "rank_the_item_two_100_lb_tanks_of_oxygen" ~ "oxygen",
          question_id == "rank_the_item_stellar_map" ~ "map",
          question_id == "rank_the_item_selfinflating_life_raft" ~ "raft",
          question_id == "rank_the_item_magnetic_compass" ~ "compass",
          question_id == "rank_the_item_twenty_liters_of_water" ~ "water",
          question_id == "rank_the_item_signal_flares" ~ "flare",
          question_id == "rank_the_item_first_aid_kit_including_injection_needle" ~ "injection",
          question_id == "rank_the_item_solarpowered_fm_receivertransmitter" ~ "receiver",
          TRUE ~ question_id
        )
      )
    
    raw_data_wide <- raw_data %>%
      pivot_wider(
        names_from = question_id,
        values_from = response,
        values_fn = list(response = list)
      ) %>%
      unnest(cols = c(individual, group, code_name, matches, food, nylon, silk, heating, pistol, milk, oxygen, map, raft, compass, water, flare, injection, receiver)) %>%
      select(code_name, everything()) %>%
      rename(session = subject_id)
    
    # In case people hit submit twice, we want to remove duplicate scores
    ## Otherwise, their  error score will be doubled
    raw_data_wide <- raw_data_wide %>%
      distinct()
    
    session_raw_data_wide <- dplyr::filter(raw_data_wide, session == input$session_name) # important because it hides the rest of the (non-session-relevant) data; if manually testing the code, comment this out
    
 
# reshape the data to re-use Jason's code
## pasted in the sections we want
    dat <- session_raw_data_wide %>% # If manually testing the code, drop "session_" and reference "raw_data_wide" here 
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
    ## also count N per group
    dat %<>% mutate(true_score = !!true_scores[dat$item], abs_error = abs(value - true_score))
    num_per_group <- dat %>%
      filter(Source == "Individual") %>%
      group_by(Group) %>%
      summarize(num_per_group = n_distinct(ID))
    
    # aggregate scores using the Borda method
    ## just requires us to average the item rankings from individuals,
    ## then convert those to ranks
    ## But first, let's only do this for N /group =/= 1
    Borda <- dat %>%
      left_join(num_per_group, by = "Group") %>%
      mutate(num_per_group = coalesce(num_per_group, 1)) %>%
      filter(!num_per_group == 1) %>%
      filter(!Source == "Group") %>% 
      group_by(Group, item) %>% 
      summarize(M = mean(value)) %>% 
      ungroup() %>%
      group_by(Group) %>% 
      mutate(value = rank(M, ties.method = "last")) %>% 
      ungroup()
    Borda %<>% reframe(Group, item, value)
    
    # calculate error scores for Borda rankings, then get group averages
    Borda %<>% mutate(true_score = !!true_scores[Borda$item], error = abs(value - true_score))
    Borda %<>% group_by(Group) %>% 
     summarize(abs_error = sum(error))%>% 
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
    
    # the output
    output$plotResults <- renderPlot({
      draw_plot
    })
    
  })
 
  # We should also have a way to show the score interpretation as well as the answer key
  ## We'll put these both in tibbles and just print as a table
  ## First for interpreting the score ranges
   observeEvent(input$plotScoreKeyBtn, {
    score_key <- data.frame(Rating=c("Excellent", "Good", "Average", "Fair", 
                                 "Poor--suggests use of Earth-bound logic", 
                                 "Very poor--you are one of the casualties of the space program!"), 
                        Score=c("0-25", "26-32", "33-45", "46-55", "56-70", "71-112"))
   # tibble(score_key)
    
    # the output
    output$plotScoreKey <- renderTable({
      score_key
    })
  })
   
  # Now we do the same for the answer key
  observeEvent(input$plotAnswerKeyBtn, {
    answer_key <- data.frame(Item=c("Two 100 lb. tanks of oxygen", "20 liters of water", "Stellar map",
                                    "Food concentrate", "Solar-powered FM receiver-transmitter", "50 feet of nylon rope",
                                    "First aid kit, including injection needle", "Parachute silk", "Self-inflating life raft",
                                    "Signal flares", "Two .45 caliber pistols", "One case of degydrated milk", 
                                    "Portable heating unit", "Magnetic compass", "Box of matches"),
                             Ranking=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                             Reasoning=c("Most pressing survival need (weight is not a factor since gravity is one-sixth of the Eath's--each tank would weigh only about 17lbs. on the moon",
                                              "Needed for replacement of tremendous liquid loss on the light side", "Primary means of navigation - star patterns appear essentially identical on the moon as on Earth", 
                                              "Efficient means of supplying every requirements", "For communication with mother ship (but FM requires line-of-sight transmission and can only be used over short ranges)", 
                                              "Useful in scaling cliffs and tying injured together", "Needles connected to vials of vitamins, medicines, etc. will fit special aperture in NASA space suit",
                                              "Protection from the sun's rays", "CO2 bottle in military raft may be used for propulsion", "Use as distress signal when the mother ship is sighted", 
                                              "Possible means of self-propulsion", "Bulkier duplication of food concetrate", "Not needed unless on the dark side", 
                                              "The magnetic field on the mood is not polarized, so it's worthless for navigation", 
                                              "Virtually worthless--there's no oxygen on the moon to sustain combustion"))
 #  tibble(answer_key)
        # the output
    output$plotAnswerKey <- renderTable({
      answer_key
    })
  })
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#-------CALL SHINY APP FUNCTION
shinyApp(ui, server)

