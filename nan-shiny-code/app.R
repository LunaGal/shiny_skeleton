#Nan Himmelsbach 15DEC23


library(shiny)


if (interactive()) {
  
  ui <- fluidPage(
    "howdy, let's get started!", 
    sidebarLayout(
      sidebarPanel(
        fileInput("upload1", "Upload a data file", multiple = FALSE, accept = ".csv"), 
        checkboxInput("header", "Header", TRUE)),
      
      sidebarLayout(    
        sidebarPanel(  
          fileInput("upload2", "Upload a metadata file", multiple = FALSE, accept = ".csv"), 
          checkboxInput("header", "Header", TRUE)),
        
        
        mainPanel(
          tableOutput("contents")),
        
      )
    )
  )
  
  server <- function(input, output, session) {
    output$contents <- renderTable({
      file <- input$upload1
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a .csv file"))
      
      read.csv(file$datapath, header = input$header)
    })
    output$contents <- renderTable({
      file2 <- input$upload2
      ext <- tools::file_ext(file2$datapath)
      
      req(file2)
      validate(need(ext == "csv", "Please upload a .csv file"))
      
      read.csv(file2$datapath, header = input$header)
    })
  }
  shinyApp(ui, server)
}