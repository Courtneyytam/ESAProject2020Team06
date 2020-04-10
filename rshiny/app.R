library(shiny)
library(shinydashboard)
library(lubridate)

cards_data <- read.csv(file="cards.csv")
cards <- cards_data$Card

ui <- dashboardPage(skin="black",
  dashboardHeader(title = span("Input Card", style = "font-size: 18px; font-weight: bold; font-family: monospace")),
  dashboardSidebar(
    tags$head( 
      tags$style(HTML(".main-sidebar {font-family: monospace}"))
    ),
    selectizeInput(
      'card_desp', '1. Card', choices = cards,
      options = list(
        placeholder = 'select card',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    actionButton('submit', 'Submit'),
    hr(),
    fluidRow(column(4,
        actionButton('start','Start Game'),
        actionButton('stop','End Game')
      )
    )
  ),
  dashboardBody(
    fluidRow(
      titlePanel(
        h1("Going Viral", align = "center", style='font-size: 42px; font-weight: bold; font-family: monospace')
      )
    ),
    fluidRow(
      column(12,align="center", style='font-size: 42px; font-weight: bold; font-family: monospace',
        valueBoxOutput("Infected", width=3),
        valueBoxOutput("Recovered", width=3),
        valueBoxOutput("Funding", width=3),
        valueBoxOutput("Time", width=3)
      )
    ),
    fluidRow(
      img(src='map.png',width='1200',style="display: block; margin-left: auto; margin-right: auto;")
    )
  )
)

server <- function(input, output, session) {
  timer <- reactiveVal(7200)
  active <- reactiveVal(FALSE)
  
  infected <- reactiveVal(0)
  recovered <- reactiveVal(0)
  funding <- reactiveVal(100)

  observeEvent(input$submit, {
    values <- subset(cards_data, cards_data[,1] == input$card_desp)
    infected(infected() + values[,2])
    recovered(recovered() + values[,3])
    funding(funding() + values[,4])
  })

  output$Infected <- renderValueBox({
    valueBox(renderText(infected()),"Total Infected", color = "red")
  })
  output$Recovered <- renderValueBox({
    valueBox(renderText(recovered()), "Total Recovered", color = "green")
  })
  output$Funding <- renderValueBox({
    valueBox(renderText(funding()), "Total Funding", color = "yellow")
  })
  output$Time <- renderValueBox({
    valueBox(renderText({paste(seconds_to_period(timer()))}), "Time Remaining", color = "black")
  })
  
  observe({
    invalidateLater(7200, session)
    isolate({
      if(active()) {
        timer(timer()-1)
        if(timer()<1) {
          active(FALSE)
          showModal(modalDialog(title =  span("Game Over", style = "font-size: 24px; font-weight: bold; font-family: monospace"), 
                                renderText({paste("You were able to save ", recovered, " people.")}),
                                renderText({paste("You allowed ", infected, " people to be infected.")}),
                                renderText({paste("Your final score is ", recovered - infected, ".")}),
                                style='font-size: 16px; font-weight: bold; font-family: monospace'))
        }
      }
    })
  })
  
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {
    active(FALSE)
    showModal(modalDialog(title =  span("Game Over", style = "font-size: 24px; font-weight: bold; font-family: monospace"), 
                renderText({paste("You were able to save ", recovered(), " people.")}),
                renderText({paste("You allowed ", infected(), " people to be infected.")}),
                renderText({paste("Your final score is ", max(recovered() - infected(), 0), ".")}),
                style='font-size: 16px; font-weight: bold; font-family: monospace'))
  })
}

shinyApp(ui = ui, server = server)
