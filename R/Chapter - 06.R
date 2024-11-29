# Load packages ----
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(googledrive)
library(googlesheets4)


# Configuring Google Sheets ----
gs4_auth()

## Initializing the sheet with correct column names ----
initial_data <- tibble(
  cm1 = 0,
  cm2 = 0,
  cs1 = 0,
  cs2 = 0,
  gender = 0,
  age = 0,
  edu = 0,
  protest = 0,
  timestamp = 0
)

gs_name <- "DCEBookTest"

# Create the Google Sheet.
# NB! This will overwrite any existing sheet with the same name
gs <- gs4_create(
  gs_name,
  sheets = list(
    "DCEBookTest" = initial_data
  )
)

# Let us append some data to see that it works as intended
appended_data <- tibble(
  cm1 = 1,
  cm2 = 1,
  cs1 = "Prog 1",
  cs2 = "Prog 2",
  gender = "Male",
  age = 25,
  edu = "Primary",
  protest = 0,
  timestamp = Sys.time()
)

sheet_append(
  ss = gs,
  data = appended_data,
  sheet = gs_name
)

# Read in the design ----
design <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Data/design-windmills.rds")))

design <- design$design

# Attributes
attribute_descriptions <- c(
  "Size of wind farms",
  "Maximum height of turbines",
  "Reduction of red kite population",
  "Minimum distance to residential areas",
  "Monthly surcharge to power bill"
)

# Create a list of temporary choice tasks
ct <- lapply(seq_len(nrow(design)), function(i) {
  return(
    # Grab the design, create the choice task, and modify for correct display.
    design[i, ] |>
      select(-block) |>
      pivot_longer(
        cols = everything(),
        names_to = c("alt", ".value"),
        values_to = "level",
        names_sep = "_"
      ) |>
      mutate(
        farm = farm2 * 2 + farm3 * 3,
        farm = ifelse(farm == 0 | is.na(farm), 1, farm),
        farm = case_when(
          farm == 1 ~ "Small",
          farm == 2 ~ "Medium",
          farm == 3 ~ "Large"
        ),
        height = height2 * 2 + height3 * 3,
        height = ifelse(height == 0 | is.na(height), 1, height),
        height = case_when(
          height == 1 ~ "Low",
          height == 2 ~ "Medium",
          height == 3 ~ "High"
        ),
        redkite = ifelse(is.na(redkite), 0, redkite + 10),
        redkite = paste0(redkite, "%"),
        distance = ifelse(is.na(distance), 750, distance * 1000 + 750),
        distance = paste0(distance, "m"),
        cost = ifelse(is.na(cost), 0, cost),
        cost = paste0("€", cost)
      ) |>
      select(farm, height,
             redkite, distance, cost, -farm2, -farm3, -height2, -height3, -sq, alt) |>
      pivot_longer(
        cols = -alt,
        names_to = "attribute",
        values_to = "level"
      ) |>
      pivot_wider(
        names_from = alt,
        values_from = level
      ) |>
      mutate(
        attribute = attribute_descriptions
      )
  )
})

# Shiny application ----
## The UI ----
ui <- fluidPage(
  # State that we want to use Javascript
  useShinyjs(),

  # Set the theme
  theme = shinytheme("cosmo"),

  # Define some CSS
  tags$style(
    type = "text/css",
    "tr:last-child {border-bottom:1px solid #D5D5D5;}
     td {border-left:1px solid #D5D5D5; border-right:1px solid #D5D5D5;}
     .choicebuttons .radio-inline {margin: 0 10% 0 3%;}",
    HTML("td:first-child { font-weight: bold }")
  ),

  # Application title
  title = "Discrete Choice Experiment",

  # Define the title panel
  titlePanel(
    div(
      img(src = "images/your-logo.jpg", height = 50),
      p("Your title here"),
      align = "center"
    )
  ),

  # Survey introduction
  div(
    id = "intro",
    h4("Introduction"),
    p("Some basic information about your survey including how to contact the
      authors", style = "text-align: left;"),
    h4("Thank you for your participation"),
    actionButton(
      inputId = "click2socdem",
      label = "Next",
      style = "text-align: center;"
    )
  ),

  # Socio-demographic questions wrapped in a hidden() environment to facilitate
  # reveal each time the user clicks the next button
  hidden(
    div(
      id = "socdem",
      h3("Sociodemographic characteristics"),

      # Gender
      radioButtons(
        inputId = "gender",
        label   = "Please select your gender",
        choices = c(
          "Female" = "female",
          "Male" = "male",
          "Other" = "other",
          "Prefer not to say" = "prefer_not_to_say"
        ),
        inline  = TRUE
      ),

      # Age
      numericInput(
        inputId = "age",
        label = "What is your age?",
        value = FALSE,
        min = 16,
        max = 100,
        step = 1
      ),

      #Educational level
      radioButtons(
        inputId = "edu",
        label   = "What is your highest completed education degree?",
        choices = c(
          "None completed or primary" = "primary",
          "Secondary education" = "secondary",
          "College or higher" = "tertiary"
        ),
        inline  = TRUE
      ),

      # Include the action button send people to the next page of the survey
      actionButton(
        inputId = "click2explanation",
        label = "Next"
      ),
      style = "text-align: center;"
    ),


    # Explanation of the DCE
    div(
      id = "explanation",
      p(
        "Next, we will show you two different hypothetical scenarios.
        We will ask you to please answer, for each one of them, which of the three
        possible programmes would you choose regarding onshore wind power.",
        style = "text-align: left;"
      ),
      p(
        "All answers are anonymous and confidential. There are no right or wrong
        answers. We simply want to know your opinion.",
        style = "text-align: left;"
      ),

      # Include the action button to send people on to the next page
      actionButton(
        inputId = "click2choice1",
        label = "Next"
      ),
      style = "text-align: center;"
    ),

    # Choice task 1
    div(
      id = "choicetask1",
      h4(
        strong("Choice task number 1"),
        style = "text-align: center;"
      ),

      div(
        id = "ct1",
        class = "shiny-input-radiogroup",
        tableOutput("tct1")
      ),

      # Include the action button that sends people to the next choice task.
      actionButton(
        inputId = "click2choice2",
        label = "Next"
      ),
      style = "text-align: center;"
    ),

    # Choice task 2
    div(
      id = "choicetask2",
      h4(
        strong("Choice task number 2"),
        style = "text-align: center;"
      ),

      div(
        id = "ct2",
        class = "shiny-input-radiogroup",
        tableOutput("tct2")
      ),

      # Include the action button that sends people to the next choice task.
      actionButton(
        inputId = "click2protest",
        label = "Next"
      ),
      style = "text-align: center;"
    ),

    # Gathering data on potential protesters
    div(
      id = "protest",
      textInput(
        "protest",
        "Why did you choose programme 1 in all of the situations presented?",
        width = 500
      ),
      actionButton(
        inputId = "click2thanksprotest",
        label = "Next"
      ),
      style = "text-align: center;"
    ),

    # Show a message while saving the results
    h3(
      id = "savingmessage",
      "We are saving your answers, this will take a moment ..."
    ),

    div(
      id = "submit",
      actionButton(
        inputId = "click2thanks",
        label = "Next"
      ),
      style = "text-align: center;"
    ),

    # Thank you slide
    div(
      id = "thankyou",
      h3("Thank you for participating in this survey."),
      p(
        "The results will be available in",
        a(href = "http://www.et.bs.ehu.es/~etpmaxxp/", "this website"),
        "shortly."
      ),
      div(
        tags$button(
          id = "close",
          type = "button",
          class = "btn action-button",
          onclick = "setTimeout(function(){window.close();},500);",
          "End survey"
        ),
        style = "text-align: center;")
    )
  )
)

# Server side
server <- function(input, output) {
  # Define a set of mandatory fields
  mandatory_fields <- c("gender", "age", "edu")

  # Observe button click for the button to advance to sociodem and reveal the
  # button if all mandatory fields are filled
  observeEvent(
    input$click2socdem, {
      hide("intro")
      show("socdem")

      #Only show next button when answers are filled
      observe({
        mandatory_filled <- vapply(
          mandatory_fields,
          function(x) {
            !is.null(input[[x]]) && input[[x]] != ""
          },
          logical(1)
        )

        # Check if all mandatory fields are filled
        mandatory_filled <- all(mandatory_filled)

        # Toggle the next button
        toggleState(
          id = "click2explanation",
          condition = mandatory_filled & input$age > 15
        )
      })
    }
  )

  # Observer button click to adavence to explanation
  observeEvent(
    input$click2explanation, {
      hide("socdem")
      show("explanation")
    }
  )

  # Add observer for choice task 1
  observeEvent(
    input$click2choice1, {
      hide("explanation")
      show("choicetask1")

      #Only show next button when a choice is selected:
      observe({
        toggleState(
          id = "click2choice2",
          condition = !is.null(input$ct1)
        )
      })
    }
  )

  # Add observer for choice task 2
  observeEvent(
    input$click2choice2, {
      hide("choicetask1")
      show("choicetask2")
      show("submit")

      #Only show next button when a choice is selected:
      observe({
        toggleState(
          id = "click2thanks",
          condition = !is.null(input$ct2)
        )
      })
    }
  )

  # Save the results to database when the submit button is clicked
  observeEvent(
    input$click2thanks, {
      # Show protest question of all choices are for the SQ
      if (input$ct1 == "Prog 1" & input$ct2 == "Prog 1") {
        hide("choicetask2")
        hide("submit")
        show("protest")

        observe({
          toggleState(
            id = "click2thanksprotest",
            condition = nchar(input$protest) > 0)
        })

        observeEvent(
          input$click2thanksprotest, {
            hide("protest")
            show("savingmessage")
            save_data("DCEBookTest", form_data())
            hide("savingmessage")
            show("thankyou")
          })

      } else{
        hide("choicetask2")
        hide("submit")
        show("savingmessage")
        save_data("DCEBookTest", form_data())
        hide("savingmessage")
        show("thankyou")
      }
    }
  )

  # Set up the rendering of the choice tasks
  nct_shown <- 2
  rct <- sample(seq_len(nrow(design)), nct_shown)
  ct_shown <- vector(mode = "list", length = nct_shown)

  for(j in seq_along(ct_shown)){
    ct_shown[[j]] <- rbind(
      ct[[rct[j]]],
      c(
        "",
        paste('<input type="radio" name="ct', j, '" value="Prog 1"/>', sep = ""),
        paste('<input type="radio" name="ct', j, '" value="Prog 2"/>', sep = ""),
        paste('<input type="radio" name="ct', j, '" value="Prog 3"/>', sep = "")
      )
    )

    rownames(ct_shown[[j]])[nrow(ct_shown[[j]])] <- "?WHICH PROGRAMME DO YOU PREFER?"

  }

  # Render the choice task tables
  output$tct1 <- renderTable(
    ct_shown[[1]],
    # rownames = TRUE,
    # colnames = TRUE,
    sanitize.text.function = function(x) {
      x
    },
    align = c("lccc"),
    width = "90%",
    spacing = "s",
    striped = TRUE
  )

  output$tct2 <- renderTable(
    ct_shown[[2]],
    # rownames = TRUE,
    # colnames = TRUE,
    sanitize.text.function = function(x) {
      x
    },
    align = c("lccc"),
    width = "90%",
    spacing = "s",
    striped = TRUE
  )

  # Define a timestamp as part of the collected data
  end_time <- function() {
    format(Sys.time(), "%Y%m%d-%H%M%OS")
  }

  #Function to save the data
  save_data <- function(sheetname, data) {
    # First get the ID for the sheet
    ss <- drive_get(sheetname)

    sheet_append(
      ss = gs,
      data = data,
      sheet = "DCEBookTest"
    )
  }

  #Reactive object to temporarily save the user's data
  form_data <- reactive({
    tibble(
      cm1 = input$ct1,
      cm2 = input$ct2,
      cs1 = rct[1],
      cs2 = rct[2],
      gender = input$gender,
      age = input$age,
      edu = input$edu,
      protest = input$protest,
      timestamp = Sys.time()
    )
  })

  # Final observer to ensure that the app closes when the user is done
  observe({
    if (input$close > 0) stopApp()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
