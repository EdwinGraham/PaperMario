library(shiny)

ui <- fluidPage(
  
  titlePanel("Paper Mario Solver"),
  tags$h3("Solve battle line-up puzzles in Paper Mario: The Origami King"),
  tags$h5("by Edwin Graham"),
  
  # Buttons to indicate enemy positions
  fluidRow(
    column(1, offset = 5, checkboxInput("box112", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box101", "", value = FALSE)),
  ),
  fluidRow(
    column(1, offset = 1, checkboxInput("box111", "", value = FALSE)),
    column(1, offset = 3, checkboxInput("box212", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box201", "", value = FALSE)),
    column(1, offset = 3, checkboxInput("box102", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 2, checkboxInput("box211", "", value = FALSE)),
    column(1, offset = 2, checkboxInput("box312", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box301", "", value = FALSE)),
    column(1, offset = 2, checkboxInput("box202", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 3, checkboxInput("box311", "", value = FALSE)),
    column(1, offset = 1, checkboxInput("box412", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box401", "", value = FALSE)),
    column(1, offset = 1, checkboxInput("box302", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 4, checkboxInput("box411", "", value = FALSE)),
    column(1, offset = 2, checkboxInput("box402", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 0, checkboxInput("box110", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box210", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box310", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box410", "", value = FALSE)),
    column(1, offset = 4, checkboxInput("box403", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box303", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box203", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box103", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 0, checkboxInput("box109", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box209", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box309", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box409", "", value = FALSE)),
    column(1, offset = 4, checkboxInput("box404", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box304", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box204", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box104", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 4, checkboxInput("box408", "", value = FALSE)),
    column(1, offset = 2, checkboxInput("box405", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 3, checkboxInput("box308", "", value = FALSE)),
    column(1, offset = 1, checkboxInput("box407", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box406", "", value = FALSE)),
    column(1, offset = 1, checkboxInput("box305", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 2, checkboxInput("box208", "", value = FALSE)),
    column(1, offset = 2, checkboxInput("box307", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box306", "", value = FALSE)),
    column(1, offset = 2, checkboxInput("box205", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 1, checkboxInput("box108", "", value = FALSE)),
    column(1, offset = 3, checkboxInput("box207", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box206", "", value = FALSE)),
    column(1, offset = 3, checkboxInput("box105", "", value = FALSE))
  ),
  fluidRow(
    column(1, offset = 5, checkboxInput("box107", "", value = FALSE)),
    column(1, offset = 0, checkboxInput("box106", "", value = FALSE)),
  ),
  
  # Number of moves
  textInput("nMoves", label = "Number of moves", value = "3"),
  
  fluidRow(
    # Button to run function to find solution
    actionButton("go", "Go"),
    
    # Button to reset page
    actionButton("reset", "Reset")
  ),
  
  # Output text
  verbatimTextOutput("directions")
)

server <- function(input, output, session) {
  
  source("functions.R")
  
  dir <- eventReactive(input$go, {
    enemyPositions <- list()
    if (input$box101) enemyPositions <- c(enemyPositions, list(c(1, 1)))
    if (input$box102) enemyPositions <- c(enemyPositions, list(c(1, 2)))
    if (input$box103) enemyPositions <- c(enemyPositions, list(c(1, 3)))
    if (input$box104) enemyPositions <- c(enemyPositions, list(c(1, 4)))
    if (input$box105) enemyPositions <- c(enemyPositions, list(c(1, 5)))
    if (input$box106) enemyPositions <- c(enemyPositions, list(c(1, 6)))
    if (input$box107) enemyPositions <- c(enemyPositions, list(c(1, 7)))
    if (input$box108) enemyPositions <- c(enemyPositions, list(c(1, 8)))
    if (input$box109) enemyPositions <- c(enemyPositions, list(c(1, 9)))
    if (input$box110) enemyPositions <- c(enemyPositions, list(c(1, 10)))
    if (input$box111) enemyPositions <- c(enemyPositions, list(c(1, 11)))
    if (input$box112) enemyPositions <- c(enemyPositions, list(c(1, 12)))
    if (input$box201) enemyPositions <- c(enemyPositions, list(c(2, 1)))
    if (input$box202) enemyPositions <- c(enemyPositions, list(c(2, 2)))
    if (input$box203) enemyPositions <- c(enemyPositions, list(c(2, 3)))
    if (input$box204) enemyPositions <- c(enemyPositions, list(c(2, 4)))
    if (input$box205) enemyPositions <- c(enemyPositions, list(c(2, 5)))
    if (input$box206) enemyPositions <- c(enemyPositions, list(c(2, 6)))
    if (input$box207) enemyPositions <- c(enemyPositions, list(c(2, 7)))
    if (input$box208) enemyPositions <- c(enemyPositions, list(c(2, 8)))
    if (input$box209) enemyPositions <- c(enemyPositions, list(c(2, 9)))
    if (input$box210) enemyPositions <- c(enemyPositions, list(c(2, 10)))
    if (input$box211) enemyPositions <- c(enemyPositions, list(c(2, 11)))
    if (input$box212) enemyPositions <- c(enemyPositions, list(c(2, 12)))
    if (input$box301) enemyPositions <- c(enemyPositions, list(c(3, 1)))
    if (input$box302) enemyPositions <- c(enemyPositions, list(c(3, 2)))
    if (input$box303) enemyPositions <- c(enemyPositions, list(c(3, 3)))
    if (input$box304) enemyPositions <- c(enemyPositions, list(c(3, 4)))
    if (input$box305) enemyPositions <- c(enemyPositions, list(c(3, 5)))
    if (input$box306) enemyPositions <- c(enemyPositions, list(c(3, 6)))
    if (input$box307) enemyPositions <- c(enemyPositions, list(c(3, 7)))
    if (input$box308) enemyPositions <- c(enemyPositions, list(c(3, 8)))
    if (input$box309) enemyPositions <- c(enemyPositions, list(c(3, 9)))
    if (input$box310) enemyPositions <- c(enemyPositions, list(c(3, 10)))
    if (input$box311) enemyPositions <- c(enemyPositions, list(c(3, 11)))
    if (input$box312) enemyPositions <- c(enemyPositions, list(c(3, 12)))
    if (input$box401) enemyPositions <- c(enemyPositions, list(c(4, 1)))
    if (input$box402) enemyPositions <- c(enemyPositions, list(c(4, 2)))
    if (input$box403) enemyPositions <- c(enemyPositions, list(c(4, 3)))
    if (input$box404) enemyPositions <- c(enemyPositions, list(c(4, 4)))
    if (input$box405) enemyPositions <- c(enemyPositions, list(c(4, 5)))
    if (input$box406) enemyPositions <- c(enemyPositions, list(c(4, 6)))
    if (input$box407) enemyPositions <- c(enemyPositions, list(c(4, 7)))
    if (input$box408) enemyPositions <- c(enemyPositions, list(c(4, 8)))
    if (input$box409) enemyPositions <- c(enemyPositions, list(c(4, 9)))
    if (input$box410) enemyPositions <- c(enemyPositions, list(c(4, 10)))
    if (input$box411) enemyPositions <- c(enemyPositions, list(c(4, 11)))
    if (input$box412) enemyPositions <- c(enemyPositions, list(c(4, 12)))

    paperMarioBattle(enemyPositions, input$nMoves)
    
  })
  
  output$directions <- renderText({
    paste0(dir(), collapse = "/n")
  })
  
  observeEvent(input$reset, {
    updateCheckboxInput(session, "box101", value = FALSE)
    updateCheckboxInput(session, "box102", value = FALSE)
    updateCheckboxInput(session, "box103", value = FALSE)
    updateCheckboxInput(session, "box104", value = FALSE)
    updateCheckboxInput(session, "box105", value = FALSE)
    updateCheckboxInput(session, "box106", value = FALSE)
    updateCheckboxInput(session, "box107", value = FALSE)
    updateCheckboxInput(session, "box108", value = FALSE)
    updateCheckboxInput(session, "box109", value = FALSE)
    updateCheckboxInput(session, "box110", value = FALSE)
    updateCheckboxInput(session, "box111", value = FALSE)
    updateCheckboxInput(session, "box112", value = FALSE)
    updateCheckboxInput(session, "box201", value = FALSE)
    updateCheckboxInput(session, "box202", value = FALSE)
    updateCheckboxInput(session, "box203", value = FALSE)
    updateCheckboxInput(session, "box204", value = FALSE)
    updateCheckboxInput(session, "box205", value = FALSE)
    updateCheckboxInput(session, "box206", value = FALSE)
    updateCheckboxInput(session, "box207", value = FALSE)
    updateCheckboxInput(session, "box208", value = FALSE)
    updateCheckboxInput(session, "box209", value = FALSE)
    updateCheckboxInput(session, "box210", value = FALSE)
    updateCheckboxInput(session, "box211", value = FALSE)
    updateCheckboxInput(session, "box212", value = FALSE)
    updateCheckboxInput(session, "box301", value = FALSE)
    updateCheckboxInput(session, "box302", value = FALSE)
    updateCheckboxInput(session, "box303", value = FALSE)
    updateCheckboxInput(session, "box304", value = FALSE)
    updateCheckboxInput(session, "box305", value = FALSE)
    updateCheckboxInput(session, "box306", value = FALSE)
    updateCheckboxInput(session, "box307", value = FALSE)
    updateCheckboxInput(session, "box308", value = FALSE)
    updateCheckboxInput(session, "box309", value = FALSE)
    updateCheckboxInput(session, "box310", value = FALSE)
    updateCheckboxInput(session, "box311", value = FALSE)
    updateCheckboxInput(session, "box312", value = FALSE)
    updateCheckboxInput(session, "box401", value = FALSE)
    updateCheckboxInput(session, "box402", value = FALSE)
    updateCheckboxInput(session, "box403", value = FALSE)
    updateCheckboxInput(session, "box404", value = FALSE)
    updateCheckboxInput(session, "box405", value = FALSE)
    updateCheckboxInput(session, "box406", value = FALSE)
    updateCheckboxInput(session, "box407", value = FALSE)
    updateCheckboxInput(session, "box408", value = FALSE)
    updateCheckboxInput(session, "box409", value = FALSE)
    updateCheckboxInput(session, "box410", value = FALSE)
    updateCheckboxInput(session, "box411", value = FALSE)
    updateCheckboxInput(session, "box412", value = FALSE)
    updateTextInput(
      session,
      "nMoves",
      value = "3"
    )
    
  })
}

shinyApp(ui, server)
