# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Questionnaire App"),
  
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
    "))
  ),
  
  conditionalPanel(
    condition = "!input.submitted",
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
                             "Mental health" = "mental health",
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
                                   "Drugs" = "drugs",
                                   "STIs" = "STIs",
                                   "Brain related" = "brain related"
                                 ))
          )
        ),
        
        # BRANCHED QUESTION FOR MENTAL HEALTH
        conditionalPanel(
          condition = "input.counselling_topics && input.counselling_topics.includes('mental health')",
          div(class = "branch-question",
              h4("Mental Health Follow-up"),
              p("Since you selected mental health, please specify:"),
              checkboxGroupInput("mental_health_specifics",
                                 "Which mental health-related counselling do you need?",
                                 choices = list(
                                   "Drugs" = "drugs"
                                 ))
          )
        ),
        
        # BRANCHED QUESTION FOR ELDERCARE
        conditionalPanel(
          condition = "input.counselling_topics && input.counselling_topics.includes('eldercare')",
          div(class = "branch-question",
              h4("Eldercare Follow-up"),
              p("Since you selected eldercare, please specify:"),
              checkboxGroupInput("eldercare_specifics",
                                 "Which eldercare-related counselling do you need?",
                                 choices = list(
                                   "Relatives" = "relatives"
                                 ))
          )
        ),
        
        # BRANCHED QUESTION FOR LEGAL ADVICE
        conditionalPanel(
          condition = "input.counselling_topics && input.counselling_topics.includes('legal advice')",
          div(class = "branch-question",
              h4("Legal Advice Follow-up"),
              p("Since you selected legal advice, please specify:"),
              checkboxGroupInput("legal_advice_specifics",
                                 "Which legal advice-related counselling do you need?",
                                 choices = list(
                                   "Drugs" = "drugs",
                                   "Crime and delinquent" = "crime and delinquent",
                                   "Digital" = "digital",
                                   "Low income" = "low income"
                                 ))
          )
        ),
        
        # BRANCHED QUESTION FOR FAMILY AND RELATIONSHIP
        conditionalPanel(
          condition = "input.counselling_topics && input.counselling_topics.includes('family')",
          div(class = "branch-question",
              h4("Family and Relationship Follow-up"),
              p("Since you selected family and relationship, please specify:"),
              checkboxGroupInput("family_specifics",
                                 "Which family and relationship-related counselling do you need?",
                                 choices = list(
                                   "Crime and delinquency" = "crime and delinquency",
                                   "Relatives of delinquents" = "relatives of delinquents",
                                   "Sex and love" = "sex and love",
                                   "Drugs" = "drugs"
                                 ))
          )
        ),
        
        # BRANCHED QUESTION FOR CHILDREN
        conditionalPanel(
          condition = "input.counselling_topics && input.counselling_topics.includes('children')",
          div(class = "branch-question",
              h4("Children and Youth Follow-up"),
              p("Since you selected children and youth, please specify:"),
              checkboxGroupInput("children_specifics",
                                 "Which children and youth-related counselling do you need?",
                                 choices = list(
                                   "Drugs" = "drugs"
                                 ))
          )
        ),
        
        # BRANCHED QUESTION FOR VIOLENCE AND DISCRIMINATION
        conditionalPanel(
          condition = "input.counselling_topics && input.counselling_topics.includes('violence and discrimination')",
          div(class = "branch-question",
              h4("Violence and Discrimination Follow-up"),
              p("Since you selected violence and discrimination, please specify:"),
              checkboxGroupInput("violence_specifics",
                                 "Which violence and discrimination-related counselling do you need?",
                                 choices = list(
                                   "Drugs" = "drugs",
                                   "Sexual violence" = "sexual violence",
                                   "Domestic violence" = "domestic violence",
                                   "FGM/C" = "FGM/C",
                                   "Forced marriage" = "forced marriage"
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
                                   "Refugee" = "refugee",
                                   "Resettlement program" = "resettlement program",
                                   "Without ID documents" = "without ID documents"
                                 ))
          )
        ),
        
        # BRANCHED QUESTION FOR LGBTQ+
        conditionalPanel(
          condition = "input.characteristics && input.characteristics.includes('LGBTQ+')",
          div(class = "branch-question",
              h4("LGBTQ+ Follow-up"),
              p("Since you selected LGBTQ+, please specify:"),
              checkboxGroupInput("lgbtq_specifics",
                                 "Which LGBTQ+-related counselling do you need?",
                                 choices = list(
                                   "Relatives" = "relatives",
                                   "Trans" = "trans",
                                   "Intersex" = "intersex"
                                 ))
          )
        ),
        
        br(),
        actionButton("submit", "Submit Questionnaire", class = "submit-btn"),
        
        # Validation message
        uiOutput("validation_message")
    )
  ),
  
  # Results 
  conditionalPanel(
    condition = "input.submitted",
    div(class = "question-panel",
        h2("Thank You!"),
        p("Your questionnaire has been submitted successfully."),
        h3("Your Responses:"),
        verbatimTextOutput("summary"),
        br(),
        actionButton("reset", "Fill Out Another Questionnaire", class = "submit-btn")
    )
  )
)

# server logic
server <- function(input, output, session) {
  
  values <- reactiveValues(
    submitted = FALSE,
    responses = data.frame()
  )
  
  # matching vector
  create_matching_vector <- function(counselling_topics, characteristics, 
                                     health_specifics = NULL, mental_health_specifics = NULL,
                                     eldercare_specifics = NULL, legal_advice_specifics = NULL,
                                     family_specifics = NULL, children_specifics = NULL,
                                     violence_specifics = NULL, migrant_specifics = NULL,
                                     lgbtq_specifics = NULL) {
    #vector template
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
      "resettlement program" = 0,
      "relatives" = 0,
      "social emergency" = 0,
      "crime and delinquent" = 0,
      "sex work" = 0,
      "epileptic" = 0,
      "brain related" = 0,
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
      "native language" = 0,
      "crime and delinquency" = 0,
      "relatives of delinquents" = 0
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
    
    # Handle all branched question specifics
    specifics_list <- list(
      health_specifics, mental_health_specifics, eldercare_specifics,
      legal_advice_specifics, family_specifics, children_specifics,
      violence_specifics, migrant_specifics, lgbtq_specifics
    )
    
    for(specifics in specifics_list) {
      if(!is.null(specifics)) {
        for(specific in specifics) {
          if(specific %in% names(matching_vector)) {
            matching_vector[specific] <- 1
          }
        }
      }
    }
    
    return(matching_vector)
  }
  
  validate_inputs <- function() {
    errors <- c()
    
    # At least one topic must be selected (unless "skip this question" is selected)
    if (length(input$counselling_topics) == 0) {
      errors <- c(errors, "Please select at least one counselling topic or choose 'Skip this question'")
    }
    
    # At least one characteristic must be selected
    if (length(input$characteristics) == 0) {
      errors <- c(errors, "Please select at least one characteristic")
    }
    
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
    errors <- validate_inputs()
    
    if (length(errors) == 0) {
      # Create the matching vector including all branched responses
      user_vector <- create_matching_vector(
        input$counselling_topics, input$characteristics,
        input$health_specifics, input$mental_health_specifics,
        input$eldercare_specifics, input$legal_advice_specifics,
        input$family_specifics, input$children_specifics,
        input$violence_specifics, input$migrant_specifics,
        input$lgbtq_specifics
      )
      
      # Store the responses including all branched data
      new_response <- data.frame(
        timestamp = Sys.time(),
        counselling_topics = paste(input$counselling_topics, collapse = "; "),
        characteristics = paste(input$characteristics, collapse = "; "),
        health_specifics = paste(input$health_specifics %||% "", collapse = "; "),
        mental_health_specifics = paste(input$mental_health_specifics %||% "", collapse = "; "),
        eldercare_specifics = paste(input$eldercare_specifics %||% "", collapse = "; "),
        legal_advice_specifics = paste(input$legal_advice_specifics %||% "", collapse = "; "),
        family_specifics = paste(input$family_specifics %||% "", collapse = "; "),
        children_specifics = paste(input$children_specifics %||% "", collapse = "; "),
        violence_specifics = paste(input$violence_specifics %||% "", collapse = "; "),
        migrant_specifics = paste(input$migrant_specifics %||% "", collapse = "; "),
        lgbtq_specifics = paste(input$lgbtq_specifics %||% "", collapse = "; "),
        stringsAsFactors = FALSE
      )
      
      values$responses <- rbind(values$responses, new_response)
      values$user_vector <- user_vector  # Store the vector for display
      values$submitted <- TRUE
      
      # Update the conditional panel
      session$sendCustomMessage("submitted", TRUE)
    }
  })
  
  # Handle reset
  observeEvent(input$reset, {
    # Reset all inputs
    updateCheckboxGroupInput(session, "counselling_topics", selected = character(0))
    updateCheckboxGroupInput(session, "characteristics", selected = character(0))
    updateCheckboxGroupInput(session, "health_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "mental_health_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "eldercare_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "legal_advice_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "family_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "children_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "violence_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "migrant_specifics", selected = character(0))
    updateCheckboxGroupInput(session, "lgbtq_specifics", selected = character(0))
    
    values$submitted <- FALSE
    session$sendCustomMessage("submitted", FALSE)
  })
  
  # Display summary of responses
  output$summary <- renderText({
    if (nrow(values$responses) > 0) {
      latest <- values$responses[nrow(values$responses), ]
      
      summary_parts <- c(
        paste("Counselling Topics:", latest$counselling_topics),
        paste("Characteristics:", latest$characteristics)
      )
      
      # Add all non-empty branched responses
      branch_fields <- c(
        "Health Specifics" = "health_specifics",
        "Mental Health Specifics" = "mental_health_specifics", 
        "Eldercare Specifics" = "eldercare_specifics",
        "Legal Advice Specifics" = "legal_advice_specifics",
        "Family Specifics" = "family_specifics",
        "Children Specifics" = "children_specifics",
        "Violence Specifics" = "violence_specifics",
        "Migrant Background" = "migrant_specifics",
        "LGBTQ+ Specifics" = "lgbtq_specifics"
      )
      
      for(i in 1:length(branch_fields)) {
        field_name <- names(branch_fields)[i]
        field_value <- latest[[branch_fields[i]]]
        if(field_value != "") {
          summary_parts <- c(summary_parts, paste(paste0(field_name, ":"), field_value))
        }
      }
      
      summary_parts <- c(summary_parts, paste("Submitted at:", format(latest$timestamp, "%Y-%m-%d %H:%M:%S")))
      
      paste(summary_parts, collapse = "\n")
    }
  })
  
  # JavaScript to handle conditional panels
  observe({
    session$sendCustomMessage("submitted", values$submitted)
  })
}

# Helper function for null coalescing
`%||%` <- function(x, y) if(is.null(x)) y else x

shinyApp(ui = ui, server = server)