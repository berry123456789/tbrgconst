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
        .party-box {
          margin-bottom: 10px;
          padding: 10px;
          border: 1px solid black;
        }
        .party-name {
          font-weight: bold;
          font-size: 20px;
          margin-bottom: 5px;
        }
        .nav-tabs > li > a {
          color: black;
        }
      ")
    )
  ),

  titlePanel(div(
    tags$img(src = "C:/Users/sambe/OneDrive/Work_from_placement/shiny/tbrg/photo/Bloomfield_Research_Group_No_Text_Logo.jpg", height = 60, width = 60),
    "The Bloomfield Research Group"
  )),

  # Tab layout
  tabsetPanel(
    tabPanel("Data Input",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   # DUP Party Inputs
                   div(class = "party-box",
                       div(class = "party-name", "DUP"),
                       numericInput("dup_c2de", "C2DE Support",
                                    min = 0, max = 100, value = 50),
                       numericInput("dup_abc1", "ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # UUP Party Inputs
                   div(class = "party-box",
                       div(class = "party-name", "UUP"),
                       numericInput("uup_c2de", "C2DE Support",
                                    min = 0, max = 100, value = 50),
                       numericInput("uup_abc1", "ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # Alliance Party Inputs
                   div(class = "party-box",
                       div(class = "party-name", "Alliance"),
                       numericInput("alliance_c2de", "C2DE Support",
                                    min = 0, max = 100, value = 50),
                       numericInput("alliance_abc1", "ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # TUV Party Inputs
                   div(class = "party-box",
                       div(class = "party-name", "TUV"),
                       numericInput("tuv_c2de", "C2DE Support",
                                    min = 0, max = 100, value = 50),
                       numericInput("tuv_abc1", "ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # Sinn Fein Party Inputs
                   div(class = "party-box",
                       div(class = "party-name", "Sinn Fein"),
                       numericInput("sinn_fein_c2de", "C2DE Support",
                                    min = 0, max = 100, value = 50),
                       numericInput("sinn_fein_abc1", "ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # SDLP Party Inputs
                   div(class = "party-box",
                       div(class = "party-name", "SDLP"),
                       numericInput("sdlp_c2de", "C2DE Support",
                                    min = 0, max = 100, value = 50),
                       numericInput("sdlp_abc1", "ABC1 Support",
                                    min = 0, max = 100, value = 50)),

                   # Other Party Inputs
                   div(class = "party-box",
                       div(class = "party-name", "Other"),
                       numericInput("other_c2de", "C2DE Support",
                                    min = 0, max = 100, value = 50),
                       numericInput("other_abc1", "ABC1 Support",
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
server <- function(input, output, session) {

  # Function to calculate overall support for each party
  calculate_overall_support <- function(data) {
    # Replace NA values with 0
    data[is.na(data)] <- 0

    data$Overall_Support <- data$C2DE_Support + data$ABC1_Support
    # Ensure overall support does not exceed 100
    data$Overall_Support <- pmin(data$Overall_Support, 100)
    data
  }


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

  # Reactive expression for checking if support exceeds 100%
  support_exceeds_100 <- reactive({
    overall_support <- rowSums(data()[, c("C2DE_Support", "ABC1_Support")])
    any(overall_support > 100)
  })

  # Show message if support exceeds 100%
  observe({
    if (support_exceeds_100()) {
      showModal(
        modalDialog(
          title = "Warning",
          "Total support for a party cannot exceed 100%."
        )
      )
    }
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
    pie(data_east$Overall_Support, labels = paste0(data_east$Party, " (", scales::percent(data_east$Overall_Support / sum(data_east$Overall_Support)), ")"), main = "East Down Party Support", col = rainbow(length(data_east$Overall_Support)))
  })

  # Render pie chart for North Down
  output$north_down_pie <- renderPlot({
    data_north <- data()
    data_north <- data_north[c("Party", "Overall_Support")]
    pie(data_north$Overall_Support, labels = paste0(data_north$Party, " (", scales::percent(data_north$Overall_Support / sum(data_north$Overall_Support)), ")"), main = "North Down Party Support", col = rainbow(length(data_north$Overall_Support)))
  })

  # Render pie chart for Belfast South & Mid Down
  output$belfast_south_mid_down_pie <- renderPlot({
    data_belfast <- data()
    data_belfast <- data_belfast[c("Party", "Overall_Support")]
    pie(data_belfast$Overall_Support, labels = paste0(data_belfast$Party, " (", scales::percent(data_belfast$Overall_Support / sum(data_belfast$Overall_Support)), ")"), main = "Belfast South & Mid Down Party Support", col = rainbow(length(data_belfast$Overall_Support)))
  })
}

shinyApp(ui = ui, server = server)
