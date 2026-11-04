# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(leaflet)

print("Libraries loaded - it is working")

# Source your existing scripts (removed data_preparation.R)
source("01_load_data.R")
print("Data loading script sourced - it is working")

source("02_algorithm.R")
print("Algorithm script sourced - it is working")

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Questionnaire App - Social Counselling Munich"),
  
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #f4f4f4;
      }
      .question-panel {
        background: white;
        padding: 20px;
        margin: 10px 0;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .results-panel {
        background: white;
        padding: 20px;
        margin: 10px 0;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .submit-btn {
        background-color: #3498db;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        font-size: 16px;
      }
      .submit-btn:hover {
        background-color: #2980b9;
      }
      .branch-question {
        background-color: #f8f9fa;
        border-left: 4px solid #3498db;
        padding: 15px;
        margin: 10px 0;
      }
      .map-container {
        height: 500px;
        margin-bottom: 20px;
      }
      .list-container {
        max-height: 600px;
        overflow-y: auto;
      }
    "))
  ),
  
  # Add JavaScript for conditional panels
  tags$script(HTML("
    Shiny.addCustomMessageHandler('submitted', function(submitted) {
      if (submitted) {
        $('input[name=\"submitted\"]').val('true').trigger('change');
      } else {
        $('input[name=\"submitted\"]').val('false').trigger('change');
      }
    });
  ")),
  
  # Hidden input to track submission state
  tags$input(id = "submitted", name = "submitted", type = "hidden", value = "false"),
  
  conditionalPanel(
    condition = "input.submitted == 'false'",
    div(class = "question-panel",
        h2("Counselling Questionnaire"),
        p("Please answer the following questions to help us understand your needs:"),
        
        # Question 1
        h3("Question 1"),
        p(strong("About which topic, broadly speaking, are you seeking counselling? You can choose multiple.")),
        checkboxGroupInput("counselling_topics", 
                           label = NULL,
                           choices = list(
                             "Work" = "work",
                             "Education" = "education", 
                             "Eldercare" = "eldercare",
                             "Asylum" = "asylum",
                             "Health" = "health",
                             "Social aid" = "social aid",
                             "Debt" = "debt",
                             "Housing" = "housing",
                             "Family and relationship" = "family",
                             "Children and youth" = "children",
                             "Legal advice" = "legal advice",
                             "Violence and discrimination" = "violence and discrimination",
                             "Other" = "other",
                             "Skip this question" = "skip_question1"
                           )),
        br(),
        
        # BRANCHED QUESTION FOR HEALTH
        conditionalPanel(
          condition = "input.counselling_topics && input.counselling_topics.includes('health')",
          div(class = "branch-question",
              h4("Health-related Follow-up"),
              p("Since you selected health, please specify which type:"),
              checkboxGroupInput("health_specifics",
                                 "Which health-related counselling do you need?",
                                 choices = list(
                                   "Mental health" = "mental health",
                                   "Disability" = "disability", 
                                   "Health" = "health"
                                 ))
          )
        ),
        
        # Question 2  
        h3("Question 2"),
        p(strong("Which characteristic(s) apply to you or the person you seek counselling for? You can choose multiple.")),
        checkboxGroupInput("characteristics",
                           label = NULL, 
                           choices = list(
                             "Woman*" = "women",
                             "Man*" = "men",
                             "LGBTQ+" = "LGBTQ+",
                             "Migrant" = "migrant",
                             "Teenager (about 12-18 years)" = "teenagers (12-18)",
                             "Young adult (about 18-26 years)" = "young adults (18-26)", 
                             "Adult (from 27 years)" = "adult (27+)",
                             "Senior (about 60+ years)" = "senior",
                             "Child (up to 12 years)" = "children",
                             "Pregnant and/or infant" = "pregnant or infant",
                             "(at risk of being) unhoused" = "unhoused",
                             "Trans" = "trans",
                             "Intersex" = "intersex"
                           )),
        
        # BRANCHED QUESTION FOR MIGRANT
        conditionalPanel(
          condition = "input.characteristics && input.characteristics.includes('migrant')",
          div(class = "branch-question",
              h4("Migration Background Follow-up"),
              p("Since you selected migrant, please specify:"),
              checkboxGroupInput("migrant_specifics",
                                 "Which specific background applies?",
                                 choices = list(
                                   "Migrant" = "migrant",
                                   "Refugee" = "refugee",
                                   "Sinti and Roma" = "sinti and roma"
                                 ))
          )
        ),
        
        br(),
        actionButton("submit", "Submit Questionnaire", class = "submit-btn"),
        
        # Validation message
        uiOutput("validation_message")
    )
  ),
  
  # Results Panel with Map and List
  conditionalPanel(
    condition = "input.submitted == 'true'",
    div(class = "results-panel",
        h2("Your Matched Counselling Offers"),
        p("Based on your responses, here are the counselling services that match your needs:"),
        
        # Summary of responses
        div(style = "background-color: #f8f9fa; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
            h4("Your Questionnaire Responses:"),
            verbatimTextOutput("summary")
        ),
        
        # Map and List side by side
        fluidRow(
          column(6,
                 h3("Map of Matched Offers"),
                 div(class = "map-container",
                     leafletOutput("offer_map", height = "500px")
                 )
          ),
          column(6,
                 h3("List of Matched Offers"),
                 div(class = "list-container",
                     uiOutput("offer_list")
                 )
          )
        ),
        
        br(),
        actionButton("reset", "Fill Out Another Questionnaire", class = "submit-btn")
    )
  )
)

print("UI defined - it is working")

# Server logic
server <- function(input, output, session) {
  
  print("Server function started - it is working")
  
  values <- reactiveValues(
    submitted = FALSE,
    responses = data.frame(),
    user_vector = NULL,
    matched_offers = NULL
  )
  
  print("Reactive values initialized - it is working")
  
  # Your original matching vector function with branched support
  create_matching_vector <- function(counselling_topics, characteristics, 
                                     health_specifics = NULL, migrant_specifics = NULL) {
    
    print("Creating matching vector - it is working")
    
    # Vector template (your original)
    matching_vector <- c(
      "education" = 0,
      "work" = 0,
      "mental health" = 0,
      "asylum" = 0,
      "eldercare" = 0,
      "family" = 0,
      "social aid" = 0,
      "housing" = 0,
      "legal advice" = 0,
      "debt" = 0,
      "health" = 0,
      "violence and discrimination" = 0,
      "children" = 0,
      "disability" = 0,
      "migrant" = 0,
      "refugee" = 0,
      "women" = 0,
      "men" = 0,
      "young adults (18-26)" = 0,
      "adult (27+)" = 0,
      "teenagers (12-17)" = 0,
      "senior" = 0,
      "pregnant or infant" = 0,
      "unhoused" = 0,
      "sinti and roma" = 0,
      "LGBTQ+" = 0,
      "Mittelschule grade 8/9" = 0,
      "resettlement and humanitarian programs" = 0,
      "relatives" = 0,
      "social emergency" = 0,
      "delinquent" = 0,
      "sex work" = 0,
      "epileptic" = 0,
      "brain injury" = 0,
      "drugs" = 0,
      "crime" = 0,
      "sex and love" = 0,
      "digital" = 0,
      "sexual violence" = 0,
      "without ID documents" = 0,
      "forced marriage" = 0,
      "domestic violence" = 0,
      "FGM/C" = 0,
      "trans" = 0,
      "intersex" = 0,
      "STIs" = 0,
      "low income" = 0,
      "Ukraine" = 0,
      "Bulgaria, Romania" = 0,
      "native language" = 0
    )
    
    for(topic in counselling_topics) {
      if(topic %in% names(matching_vector)) {
        matching_vector[topic] <- 1
      }
    }
    
    for(characteristic in characteristics) {
      if(characteristic %in% names(matching_vector)) {
        matching_vector[characteristic] <- 1
      }
    }
    
    # Handle health specifics from branched question
    if(!is.null(health_specifics)) {
      for(health in health_specifics) {
        if(health %in% names(matching_vector)) {
          matching_vector[health] <- 1
        }
      }
    }
    
    # Handle migrant specifics from branched question
    if(!is.null(migrant_specifics)) {
      for(migrant in migrant_specifics) {
        if(migrant %in% names(matching_vector)) {
          matching_vector[migrant] <- 1
        }
      }
    }
    
    print("Matching vector created successfully - it is working")
    return(matching_vector)
  }
  
  validate_inputs <- function() {
    print("Validating inputs - it is working")
    errors <- c()
    
    if (length(input$counselling_topics) == 0) {
      errors <- c(errors, "Please select at least one counselling topic or choose 'Skip this question'")
    }
    
    if (length(input$characteristics) == 0) {
      errors <- c(errors, "Please select at least one characteristic")
    }
    
    print(paste("Validation complete, errors found:", length(errors), "- it is working"))
    return(errors)
  }
  
  # Validation message output
  output$validation_message <- renderUI({
    if (input$submit > 0) {
      errors <- validate_inputs()
      if (length(errors) > 0) {
        div(style = "color: red; margin-top: 10px;",
            h4("Please fix the following errors:"),
            tags$ul(lapply(errors, tags$li))
        )
      }
    }
  })
  
  # Handle form submission
  observeEvent(input$submit, {
    print("Submit button clicked - it is working")
    
    errors <- validate_inputs()
    
    if (length(errors) == 0) {
      print("No validation errors, processing submission - it is working")
      
      # Create the matching vector including branched responses
      user_vector <- create_matching_vector(input$counselling_topics, input$characteristics,
                                            input$health_specifics, input$migrant_specifics)
      
      print("User vector created - it is working")
      
      # Run your algorithm to get rankings
      rankings <- ranking_algo(user_vector, data_wide)
      print("Rankings calculated - it is working")
      
      # Get matched offers (only those with positive rankings)
      matched_indices <- which(rankings > 0)
      ranked_indices <- matched_indices[order(rankings[matched_indices], decreasing = TRUE)]
      
      print(paste("Found", length(matched_indices), "matched offers - it is working"))
      
      # Store comprehensive results
      values$user_vector <- user_vector
      values$matched_offers <- list(
        indices = ranked_indices,
        rankings = rankings[ranked_indices]
      )
      
      print("Results stored - it is working")
      
      # Store the responses including branched data
      new_response <- data.frame(
        timestamp = Sys.time(),
        counselling_topics = paste(input$counselling_topics, collapse = "; "),
        characteristics = paste(input$characteristics, collapse = "; "),
        health_specifics = paste(input$health_specifics %||% "", collapse = "; "),
        migrant_specifics = paste(input$migrant_specifics %||% "", collapse = "; "),
        stringsAsFactors = FALSE
      )
      
      values$responses <- rbind(values$responses, new_response)
      values$submitted <- TRUE
      
      print("Response data stored - it is working")
      
      session$sendCustomMessage("submitted", TRUE)
      print("Submission complete - it is working")
    } else {
      print("Validation errors found, not processing submission - it is working")
    }
  })
  
  # Handle reset
  observeEvent(input$reset, {
    print("Reset button clicked - it is working")
    
    # Reset all inputs
    updateCheckboxGroupInput(session, "counselling_topics", selected = character(0))
    updateCheckboxGroupInput(session, "characteristics", selected = character(0))
    updateCheckboxGroupInput(session, "health_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "migrant_specifics", selected = character(0))
    
    values$submitted <- FALSE
    values$user_vector <- NULL
    values$matched_offers <- NULL
    session$sendCustomMessage("submitted", FALSE)
    
    print("Reset complete - it is working")
  })
  
  # Display summary of responses
  output$summary <- renderText({
    print("Rendering summary - it is working")
    
    if (nrow(values$responses) > 0) {
      latest <- values$responses[nrow(values$responses), ]
      
      summary_parts <- c(
        paste("Counselling Topics:", latest$counselling_topics),
        paste("Characteristics:", latest$characteristics)
      )
      
      if(latest$health_specifics != "") {
        summary_parts <- c(summary_parts, paste("Health Specifics:", latest$health_specifics))
      }
      
      if(latest$migrant_specifics != "") {
        summary_parts <- c(summary_parts, paste("Migrant Background:", latest$migrant_specifics))
      }
      
      summary_parts <- c(summary_parts, 
                         paste("Number of matched offers:", length(values$matched_offers$indices)),
                         paste("Submitted at:", format(latest$timestamp, "%Y-%m-%d %H:%M:%S")))
      
      print("Summary rendered - it is working")
      paste(summary_parts, collapse = "\n")
    }
  })
  
  # Create the interactive map
  output$offer_map <- renderLeaflet({
    print("Rendering map - it is working")
    
    if (!is.null(values$matched_offers)) {
      matched_indices <- values$matched_offers$indices
      
      # Get map data for matched offers
      map_data <- data_map[data_map$ID %in% matched_indices, ]
      
      if (nrow(map_data) > 0) {
        print("Map data found, creating leaflet map - it is working")
        
        leaflet(map_data) %>%
          addTiles() %>%
          setView(lng = 11.576124, lat = 48.137154, zoom = 11) %>%  # Munich coordinates
          addMarkers(
            lng = ~long,
            lat = ~lat,
            popup = ~paste0(
              "<strong>", Name, "</strong><br>",
              address, "<br>",
              "<small>Ranking: ", 
              values$matched_offers$rankings[match(ID, values$matched_offers$indices)], 
              "</small>"
            ),
            clusterOptions = markerClusterOptions()
          )
      } else {
        print("No map data available - it is working")
        
        leaflet() %>%
          addTiles() %>%
          setView(lng = 11.576124, lat = 48.137154, zoom = 11) %>%
          addPopups(lng = 11.576124, lat = 48.137154, 
                    popup = "No locations available for your matched offers")
      }
    }
  })
  
  # Create the offer list
  output$offer_list <- renderUI({
    print("Rendering offer list - it is working")
    
    if (!is.null(values$matched_offers)) {
      matched_indices <- values$matched_offers$indices
      
      if (length(matched_indices) > 0) {
        # Get list data for matched offers
        list_data <- data_list[data_list$ID %in% matched_indices, ]
        
        # Order by ranking
        list_data <- list_data[match(matched_indices, list_data$ID), ]
        list_data <- list_data[!is.na(list_data$ID), ]  # Remove NA matches
        
        if (nrow(list_data) > 0) {
          print(paste("Rendering", nrow(list_data), "offer items - it is working"))
          
          offer_items <- lapply(1:nrow(list_data), function(i) {
            div(
              style = "border-bottom: 1px solid #eee; padding: 15px 0;",
              div(style = "margin-bottom: 10px;",
                  HTML(paste0("<strong>Rank: ", i, "</strong>"))
              ),
              HTML(list_data$list_output[i])
            )
          })
          
          div(offer_items)
        } else {
          print("No list data available - it is working")
          
          div(
            style = "text-align: center; padding: 20px; color: #666;",
            h4("No detailed information available"),
            p("Your matched offers don't have detailed descriptions available in the database.")
          )
        }
      } else {
        print("No matching offers found - it is working")
        
        div(
          style = "text-align: center; padding: 20px; color: #666;",
          h4("No matching offers found"),
          p("Try adjusting your criteria or contact us directly for personalized assistance.")
        )
      }
    }
  })
  
  print("All server outputs defined - it is working")
}

# Helper function for null coalescing
`%||%` <- function(x, y) if(is.null(x)) y else x

print("Helper function defined - it is working")

print("Starting Shiny app - it is working")
shinyApp(ui = ui, server = server)