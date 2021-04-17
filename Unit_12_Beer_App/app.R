library(shiny)

bud_blue <- c("#13294b")
bud_red <- c("#c8102e")

ui <- fluidPage(
    
    sidebarLayout(
        
        sidebarPanel(
            
            fileInput("beer_file", "Please Upload Your Beer Data File",
                      accept = c("text/csv","text/comma-separated-values, text/plain",
                                 ".csv")),
            fileInput("brewery_file", "Please Upload Your Brewery Data File",
                      accept = c("text/csv","text/comma-separated-values, text/plain",
                                 ".csv")),
            radioButtons("var_type", "Please Select Your Variable",
                         c("IBU",
                           "ABV")),
            radioButtons("plot_type", "Select Type of Plot:",
                         c("Box Plot", 
                           "Histogram")),
            checkboxInput("lm_line", "Add Simple Linear Regression Line to Scatter Plot?", FALSE),
            selectInput("state", "Filter by State","")
            ),
        mainPanel(
            
            plotOutput("distPlot"),
            plotOutput("scatPlot")
            
                )
            )
        )

server <- function(input, output, session){
  library(tidyverse)
  
  beer_df <- reactive({
    inFile <- input$beer_file
    if (is.null(inFile)){
     return(NULL)
    }
    read.csv(inFile$datapath, header = T)
  })
  
  brewery_df <- reactive({
    inFile <- input$brewery_file
    if (is.null(inFile)){
      return(NULL)}
    read.csv(inFile$datapath, header = T)
  })
  
 #beer_brew <- eventReactive(is.null(brewery_df) == FALSE, {
   #left_join(beer_df(), brewery_df(), by = c("Brewery_id" = "Brew_ID"))
 #})
  
  observe({
    states <- as.character(brewery_df()$State)
    states <- sort(states)
    updateSelectInput(session, "state", "Fitler by State",
                      choices = states, 
                      selected = "")
  
  
  output$distPlot <- renderPlot ({
    
    state_name <- input$state
    
    if(input$var_type == "IBU" && input$plot_type == "Box Plot"){
          x <- beer_df()$IBU
          boxplot(x, col = bud_red, main = "Box Plot of IBU", xlab = "IBU")
        }
    
    if(input$var_type == "IBU" && input$plot_type == "Histogram"){
          x <- beer_df()$IBU
          hist(x, col = bud_blue, main = "Distribution of IBU", xlab = "IBU")
        }
    
    if(input$var_type == "ABV" && input$plot_type == "Box Plot"){
          x <- beer_df()$ABV*100
          boxplot(x, col = bud_red, main = "Box Plot of ABV", xlab = "ABV")
        }
    
    if(input$var_type == "ABV" && input$plot_type == "Histogram"){
          x <- beer_df()$ABV*100
          hist(x, col = bud_blue, main = "Distribution of ABV", xlab = "ABV")
      }
    })
  
  output$scatPlot <- renderPlot ({
    x <- beer_df()$IBU
    y <- beer_df()$ABV * 100
    
    if(input$lm_line == TRUE){
      plot(x, y, main = "ABV by IBU", xlab = "IBU", ylab = "ABV")
      abline(lm(y~x, data = beer_df()), col = bud_red)
    }
    
    if(input$lm_line == FALSE){
      plot(x, y, main = "ABV by IBU", xlab = "IBU", ylab = "ABV")
    }
  })
  
 
  })
}

shinyApp(ui, server)