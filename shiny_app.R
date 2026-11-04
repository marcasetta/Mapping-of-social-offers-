library(shiny)
library(bslib)
library(shinyjs)
library(leaflet)



# Prepare data
source("01_load_data.R")
source("02_algorithm.R")




library(shiny)
library(bslib)

ui <- bslib::page_navbar(
  theme = bslib::bs_theme(bootswatch = "journal", version=5),
  
  tags$style(HTML("
  .hidden {
    display: none;
  }
  .navbar-brand {
    margin-right: auto; /* Pushes the title to the left */
  }
  body {
    background-color: #dee2e6;
  }
")),
  
  
  title = h3("Social Counselling in Munich"),
  
  nav_panel("DE",
            layout_sidebar(
              sidebar = sidebar(width = "40%",
                                id = "sidebar_left",
                                h1("Questionnaire"),
                                hr(),
                                p("Please answer the following questions to help us understand your needs:"),
                                
                                card(
                                  class = "card bg-light mb-3",
                                  # Question 1
                                  card_header("Question 1"),
                                  card_body(
                                    h4(strong("About what are you seeking counselling?")),
                                    p("You can choose multiple."),
                                    checkboxGroupInput("counselling_topics",
                                                       label = NULL,
                                                       choices = list(
                                                         "Work" = "work",
                                                         "Education" = "education",
                                                         "Eldercare" = "eldercare",
                                                         "Asylum" = "asylum",
                                                         "Health" = "health and disability",
                                                         "Mental health" = "mental health",
                                                         "Social aid" = "social aid",
                                                         "Debt" = "debt",
                                                         "Housing" = "housing",
                                                         "Family and relationship" = "family",
                                                         "Children" = "children",
                                                         "Legal advice" = "legal advice",
                                                         "Violence and discrimination" = "violence and discrimination",
                                                         "Other" = "other"
                                                       )),
                                  )),
                                
                                # Question 2
                                card(
                                  class = "card bg-light mb-3",
                                  card_header("Question 2"),
                                  card_body(
                                    h4(strong("What applies to you or the person you seek counselling for?")),
                                    p("You can choose multiple."),
                                    checkboxGroupInput("characteristics",
                                                       label = NULL,
                                                       choices = list(
                                                         "Woman*" = "women",
                                                         "LGBTQ+" = "LGBTQ+",
                                                         "Migrant or Migrant Background" = "migrant",
                                                         "Teenager (about 12-18 years)" = "teenagers (12-17)",
                                                         "Young adult (about 18-26 years)" = "young adults (18-26)",
                                                         "Adult (from 27 years)" = "adult (27+)",
                                                         "Senior (about 60+ years)" = "senior",
                                                         "Child" = "children",
                                                         "Pregnancy and infant care" = "pregnant or infant",
                                                         "(at risk of being) unhoused" = "unhoused",
                                                         "No answer" = "other"
                                                       )),
                                  )),
                                
                                # FOLLOW-UP QUESTION
                                conditionalPanel(
                                  condition = "(input.counselling_topics  input.counselling_topics.length > 0 
                                        ['health and disability', 'mental health', 'eldercare',
                                         'legal advice', 'family', 'children', 'violence and discrimination', 'other']
                                        .some(topic => input.counselling_topics.includes(topic))) |
                                        (input.characteristics  input.characteristics.length > 0 
                                        ['women', 'migrant', 'LGBTQ+', 'children', 'other']
                                        .some(characteristic => input.characteristics.includes(characteristic)))",
                                  card(
                                    class = "card bg-light mb-3",
                                    card_header("Follow-up Question"),
                                    card_body(
                                      h4(strong("Please specify if you need a specific kind of counselling:")),
                                      
                                      checkboxGroupInput("specifics",
                                                         "If nothing applies, skip this question.",
                                                         choices = NULL)
                                    )
                                  )),
                                
                                # SUBMIT BUTTON
                                actionButton("submit", "Submit Answers", class = "btn btn-lg btn-primary"),
                                
                                # # Validation message
                                uiOutput("validation_message")
              ),
              
              
              layout_sidebar(
                sidebar = sidebar(width = "45%",
                                  position = "right",
                                  id = "sidebar_right",
                                  open = FALSE,
                                  useShinyjs(),
                                  h1("Your personalized results"),
                                  hr(),
                                  uiOutput("htmlContent")
                ),
                
                leafletOutput("map"),
                div(id = "emergency_card", class = "card text-white bg-primary mb-3", style = "position: absolute; top: 50px; right: 10px; z-index: 1000; width: 30%; padding: 10px;",
                   div(style = "display: flex; justify-content: space-between; align-items: center; position: relative;",
                       h4("Haben Sie einen Notfall?"),
                       actionButton("emergency", label = "❌", style = "border: none; background: none; cursor: pointer; font-size: 20px; color: white;")
                   ),
                   br(),
                   p(tags$strong("Notruf:"), " 112"),
                   p(tags$strong("Krisendienst"), " für psychische Notfälle (24h):", br(), "0800 / 655 3000"),
                   p(tags$strong("Bahnhofsmission"), br(), "24h am Hauptbahnhof"),
                ),
                
                border_radius = FALSE,
                fillable = TRUE,
                class = "p-0",
              )
            )
  )
  
)


# server logic
server <- function(input, output, session) {
  
  
  # QUESTIONNAIRE ---------------------
  
  values <- reactiveValues(
    submitted = FALSE,
    responses = character(0),
    scoreIDs = numeric(0),
  )
  

  # change the categories for the follow-up question
  observeEvent(c(input$counselling_topics, input$characteristics), {
    choices_list = c()
    if (any(c("health and disability", "mental health", "legal advice", "family", "children", "violence and discrimination") %in% input$counselling_topics) |
        any(c("children") %in% input$characteristics)) {
      choices_list = c(choices_list, "Drugs" = "drugs")
    }
    if ("health and disability" %in% input$counselling_topics) {
      choices_list = c(choices_list, "STIs" = "STIs")
    }
    if ("health and disability" %in% input$counselling_topics) {
      choices_list = c(choices_list, "Brain related health issue" = "brain related health issue")
    }
    if ("migrant" %in% input$characteristics) {
      choices_list = c(choices_list, "Refugee" = "refugee")
    }
    if ("migrant" %in% input$characteristics) {
      choices_list = c(choices_list, "Resettlement and Humanitarian Programs" = "resettlement and humanitarian programs")
    }
    if ("LGBTQ+" %in% input$characteristics) {
      choices_list = c(choices_list, "Trans" = "trans")
    }
    if ("LGBTQ+" %in% input$characteristics) {
      choices_list = c(choices_list, "Intersex" = "intersex")
    }
    if ("eldercare" %in% input$counselling_topics | "LGBTQ+" %in% input$characteristics) {
      choices_list = c(choices_list, "I'm a relative" = "relatives")
    }
    if ("legal advice" %in% input$counselling_topics | "women" %in% input$characteristics) {
      choices_list = c(choices_list, "Crime" = "crime")
    }
    if ("legal advice" %in% input$counselling_topics) {
      choices_list = c(choices_list, "I have a low income" = "low income")
    }
    if ("family" %in% input$counselling_topics) {
      choices_list = c(choices_list, "Sex and Love" = "sex and love")
    }
    if ("violence and discrimination" %in% input$counselling_topics) {
      choices_list = c(choices_list, "Sexual Violence" = "sexual violence")
    }
    if ("violence and discrimination" %in% input$counselling_topics) {
      choices_list = c(choices_list, "Domestic Violence" = "domestic violence")
    }
    if ("women" %in% input$characteristics | "LGBTQ+" %in% input$characteristics) {
      choices_list = c(choices_list, "FGM/C" = "FGM/C")
    }
    if ("women" %in% input$characteristics | "LGBTQ+" %in% input$characteristics) {
      choices_list = c(choices_list, "forced marriage" = "forced marriage")
    }
    if ("other" %in% input$counselling_topics | "other" %in% input$characteristics) {
      choices_list = unique(c("Drugs" = "drugs", "STIs" = "STIs", "Refugee" = "refugee",
                              "Refugee" = "refugee", "Crime" = "crime", "Sexual Violence" = "sexual violence",
                              "Domestic Violence" = "domestic violence", "FGM/C" = "FGM/C", "forced marriage" = "forced marriage"))
    }
    updateCheckboxGroupInput(session, "specifics", choices = c(choices_list, "None of them" = "other"))
  })
  
  # matching vector
  create_matching_vector <- function(counselling_topics = NULL, characteristics = NULL, 
                                     specifics = NULL) {
    #vector template
    # answers <- c(
    #   "education" = 0,
    #   "work" = 0,
    #   "mental health" = 1,
    #   "asylum" = 0,
    #   "eldercare" = 0,
    #   "family" = 0,
    #   "social aid" = 0,
    #   "housing" = 0,
    #   "legal advice" = 0,
    #   "debt" = 0,
    #   "health and disability" = 0,
    #   "violence and discrimination" = 1,
    #   "children" = 0,
    #   "migrant" = 0,
    #   "refugee" = 0,
    #   "women" = 0,
    #   "LGBTQ+" = 0,
    #   "young adults (18-26)" = 0,
    #   "adult (27+)" = 0,
    #   "teenagers (12-17)" = 0,
    #   "senior" = 0,
    #   "pregnant or infant" = 0,
    #   "unhoused" = 0,
    #   "LGBTQ+" = 0,
    #   "resettlement and humanitarian programs" = 0,
    #   "relatives" = 0,
    #   "crime" = 0,
    #   "sex work" = 0,
    #   "brain related health issue" = 0,
    #   "drugs" = 0,
    #   "sex and love" = 0,
    #   "digital" = 0,
    #   "sexual violence" = 0,
    #   "without ID documents" = 0,
    #   "forced marriage" = 0,
    #   "domestic violence" = 0,
    #   "FGM/C" = 0,
    #   "trans" = 0,
    #   "intersex" = 0,
    #   "STIs" = 0,
    #   "low income" = 0,
    #   "native language" = 0
    # )
    matching_vector = rep(0, ncol(data_wide))
    names(matching_vector) = colnames(data_wide)
    
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
    
    for(specific in specifics) {
      if(specific %in% names(matching_vector)) {
        matching_vector[specific] <- 1
      }
    }
    
    return(matching_vector)
  }
  
  validate_inputs <- function() {
    errors <- c()
    
    # At least one option must be selected (unless "skip this question" is selected)
    if ((length(input$counselling_topics) == 0 | all(input$counselling_topics == "other")) &&
        (length(input$characteristics) == 0 | all(input$characteristics == "other"))) {
      errors <- c(errors, "Please select at least one option.")
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
      responses <- create_matching_vector(
        input$counselling_topics, input$characteristics, input$specifics)
      values$responses <- responses  # Store the vector for display
      score <- ranking_algo(responses, data_wide)
      values$scoreIDs <- data_list$ID[score]
      values$submitted <- TRUE
      
      # toggle sidebars
      toggle_sidebar(id = "sidebar_right", open = TRUE)
      toggle_sidebar(id = "sidebar_left", open = FALSE)
    }
  })
  
  # Handle submit
  observe({
    if (values$submitted) {
      session$sendCustomMessage(type = "submitted", message = values$submitted)
      # remove emergency information
      shinyjs::hide("emergency_card")
    }
  })
  
  
  
  # MAIN APP ---------------------
  
  # Render the interactive map
  output$map <- renderLeaflet({
    if (length(values$scoreIDs) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 11.5820, lat = 48.1351, zoom = 12)
    } else {
      
      this_data_map = data_map[data_map$ID %in% values$scoreIDs,]
      # this_data_map = this_data_map[!is.na(this_data_map$lat)]
      suppressWarnings(
        leaflet(data = this_data_map) %>%
          addTiles() %>%
          setView(lng = 11.5820, lat = 48.1351, zoom = 12) %>%
          addMarkers(lng = this_data_map$long, lat = this_data_map$lat, popup = this_data_map$Popup)
      )
    }
  })
  
  output$buttons_ui <- renderUI({
    btns <- lapply(names(data_list), function(id) {
      actionButton(inputId = paste0("toggle_", id), label = paste("Toggle", id))
    })
    do.call(tagList, btns)
  })
  
  output$htmlContent <- renderUI({

    if (length(values$scoreIDs)) {
      # Toggle Content
      panels <- lapply(values$scoreIDs, function(i) {
        tagList(
          actionButton(inputId = paste0("toggle_", i),
                       class = "btn btn-info",
                       label = HTML(data_list$short_output[data_list$ID == i])),
          div(
            id = paste0("panel_", i),
            class = "hidden",
            style = "border: 1px solid #ccc; padding: 10px; margin-top: 5px;",
            HTML(data_list$list_output[data_list$ID == i])
          )
        )
      })
      do.call(tagList, panels)
    } else {
      p("Please submit the questionnaire.")
    }

  })

  observe({
    lapply(values$scoreIDs, function(i) {
      observeEvent(input[[paste0("toggle_", i)]], {
        toggleClass(id = paste0("panel_", i), class = "hidden")
      }, ignoreInit = TRUE)
    })
  })
  
  observeEvent(input$emergency, {
    shinyjs::hide("emergency_card")  # Hide the card when the close button is clicked
  })
  
}


shinyApp(ui = ui, server = server)


shinyApp(ui = ui, server = server)
