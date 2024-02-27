library(shiny)
library(scales)
library(magrittr)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: lightblue;
        }
        .input-box {
          margin-bottom: 10px;
        }
        #title-container {
          display: flex;
          align-items: center;
        }
        #title-image {
          margin-right: 10px;
        }
      ")
    )
  ),

  titlePanel(
   #### div(id = "title-container",
    ####    img(id = "title-image", src = "Bloomfield_Research_Group_No_Text_Logo.jpg", height = 50, width = 50),
        "The Bloomfield Research Group"
    ),



  # Tab layout
  tabsetPanel(
    tabPanel("Data Input",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   # DUP Support Inputs
                   div(class = "input-box",
                       numericInput("dup_c2de", "DUP C2DE Support",
                                    min = 0, max = 100, value = 50)),
                   div(class = "input-box",
                       numericInput("dup_abc1", "DUP ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # UUP Support Inputs
                   div(class = "input-box",
                       numericInput("uup_c2de", "UUP C2DE Support",
                                    min = 0, max = 100, value = 50)),
                   div(class = "input-box",
                       numericInput("uup_abc1", "UUP ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # Alliance Support Inputs
                   div(class = "input-box",
                       numericInput("alliance_c2de", "Alliance C2DE Support",
                                    min = 0, max = 100, value = 50)),
                   div(class = "input-box",
                       numericInput("alliance_abc1", "Alliance ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # TUV Support Inputs
                   div(class = "input-box",
                       numericInput("tuv_c2de", "TUV C2DE Support",
                                    min = 0, max = 100, value = 50)),
                   div(class = "input-box",
                       numericInput("tuv_abc1", "TUV ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # Sinn Fein Support Inputs
                   div(class = "input-box",
                       numericInput("sinn_fein_c2de", "Sinn Fein C2DE Support",
                                    min = 0, max = 100, value = 50)),
                   div(class = "input-box",
                       numericInput("sinn_fein_abc1", "Sinn Fein ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # SDLP Support Inputs
                   div(class = "input-box",
                       numericInput("sdlp_c2de", "SDLP C2DE Support",
                                    min = 0, max = 100, value = 50)),
                   div(class = "input-box",
                       numericInput("sdlp_abc1", "SDLP ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # Other Support Inputs
                   div(class = "input-box",
                       numericInput("other_c2de", "Other C2DE Support",
                                    min = 0, max = 100, value = 50)),
                   div(class = "input-box",
                       numericInput("other_abc1", "Other ABC1 Support",
                                    min = 0, max = 100, value = 50))
                 ),
                 mainPanel()
               )
             )
    ),
    tabPanel("East Belfast",
             plotOutput("east_belfast_pie", width = "100%", height = "600px")
    ),
    tabPanel("North Down",
             plotOutput("north_down_pie", width = "100%", height = "600px")
    ),
    tabPanel("Belfast South & Mid Down",
             plotOutput("belfast_south_mid_down_pie", width = "100%", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive expression to process data based on user inputs
  data <- reactive({
    data <- data.frame(
      Party = c("DUP", "UUP", "Alliance", "TUV", "Sinn Fein", "SDLP", "Other"),
      C2DE_Support = c(input$dup_c2de, input$uup_c2de, input$alliance_c2de,
                       input$tuv_c2de, input$sinn_fein_c2de, input$sdlp_c2de,
                       input$other_c2de),
      ABC1_Support = c(input$dup_abc1, input$uup_abc1, input$alliance_abc1,
                       input$tuv_abc1, input$sinn_fein_abc1, input$sdlp_abc1,
                       input$other_abc1)
    )
    # Calculate overall support
    data <- calculate_overall_support(data)
    data
  })

  # Reactive expression for East Belfast data
  data_east_belfast <- reactive({
    data_east_belfast <- data()
    data_east_belfast <- data_east_belfast[c("Party", "Overall_Support")]
    data_east_belfast
  })

  # Reactive expression for North Down data
  data_north_down <- reactive({
    data_north_down <- data()
    data_north_down <- data_north_down[c("Party", "Overall_Support")]
    data_north_down
  })

  # Reactive expression for Belfast South & Mid Down data
  data_belfast_south_mid_down <- reactive({
    data_belfast_south_mid_down <- data()
    data_belfast_south_mid_down <- data_belfast_south_mid_down[c("Party", "Overall_Support")]
    data_belfast_south_mid_down
  })


  # Render pie chart for East Belfast
  output$east_belfast_pie <- renderPlot({
    data_east <- data()
    data_east <- data_east[c("Party", "Overall_Support")]
    pie(data_east$Overall_Support, labels = paste0(data_east$Party, " (", percent(data_east$Overall_Support / sum(data_east$Overall_Support)), ")"), main = "East Down Party Support", col = rainbow(length(data_east$Overall_Support)))
  })

  # Render pie chart for North Down
  output$north_down_pie <- renderPlot({
    data_north <- data()
    data_north <- data_north[c("Party", "Overall_Support")]
    pie(data_north$Overall_Support, labels = paste0(data_north$Party, " (", percent(data_north$Overall_Support / sum(data_north$Overall_Support)), ")"), main = "North Down Party Support", col = rainbow(length(data_north$Overall_Support)))
  })

  # Render pie chart for Belfast South & Mid Down
  output$belfast_south_mid_down_pie <- renderPlot({
    data_belfast <- data()
    data_belfast <- data_belfast[c("Party", "Overall_Support")]
    pie(data_belfast$Overall_Support, labels = paste0(data_belfast$Party, " (", percent(data_belfast$Overall_Support / sum(data_belfast$Overall_Support)), ")"), main = "Belfast South & Mid Down Party Support", col = rainbow(length(data_belfast$Overall_Support)))
  })
}

# function to calculate overall support for each party
calculate_overall_support <- function(data) {
  data$Overall_Support <- data$C2DE_Support + data$ABC1_Support
  # Ensure overall support does not exceed 100
  data$Overall_Support <- pmin(data$Overall_Support, 100)
  data
}

# Run the application
shinyApp(ui = ui, server = server)
