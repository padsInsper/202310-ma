# Load required libraries
library(shiny)

# UI definition
ui <- fluidPage(
  titlePanel("Monty Hall Game"),
  sidebarLayout(
    sidebarPanel(
      actionButton("start_game", "Start New Game"),
      actionButton("door1", "Choose Door 1"),
      actionButton("door2", "Choose Door 2"),
      actionButton("door3", "Choose Door 3"),
      conditionalPanel(
        condition = "!is.null(chosen_door)",
        actionButton("switch_door", "Switch Door"),
        actionButton("stay_door", "Stay with Current Door")
      )
    ),
    mainPanel(
      textOutput("instruction"),
      textOutput("result")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Game state variables
  chosen_door <- reactiveVal(NULL)
  prize_door <- reactiveVal(NULL)
  opened_door <- reactiveVal(NULL)

  # Choose door 1
  observeEvent(input$door1, {
    choose_door(1)
  })

  # Choose door 2
  observeEvent(input$door2, {
    choose_door(2)
  })

  # Choose door 3
  observeEvent(input$door3, {
    choose_door(3)
  })

  # Start a new game
  observeEvent(input$start_game, {
    prize_door(sample(1:3, 1))
    chosen_door(NULL)
    opened_door(NULL)
    output$instruction <- renderText("Choose a door!")
    output$result <- renderText("")
  })

  choose_door <- function(door) {

    if (is.null(prize_door())) return()

    chosen_door(door)
    doors_to_open <- setdiff(1:3, c(chosen_door(), prize_door()))

    if (length(doors_to_open) == 1) {
      opened_door(doors_to_open)
    } else {
      opened_door(sample(doors_to_open, 1))
    }

    print(opened_door())
    output$instruction <- renderText(paste("You chose door", chosen_door(),
                                           ". Monty opened door", opened_door(),
                                           ". Do you want to switch or stay?"))
  }

  # Switch door
  observeEvent(input$switch_door, {
    if (is.null(chosen_door())) return()

    remaining_doors <- setdiff(1:3, c(chosen_door(), opened_door()))
    chosen_door(remaining_doors[1])
    check_win()
  })

  # Stay with current door
  observeEvent(input$stay_door, {
    if (is.null(chosen_door())) return()

    check_win()
  })

  # Check win condition
  check_win <- function() {
    if (chosen_door() == prize_door()) {
      output$result <- renderText("Congratulations! You won!")
    } else {
      output$result <- renderText("Sorry, you lost. Try again!")
    }

    output$instruction <- renderText("Click 'Start New Game' to play again.")
  }
}

# Run the application
shinyApp(ui = ui, server = server)