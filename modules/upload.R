# Module Credit: https://git.wur.nl/-/snippets/92
library(readxl)
uploadUI <- function(id, ...) {
  ns <- NS(id)
  fileInput(ns("file_upload"), ..., accept = c(".csv", ".xls", ".xlsx", ".xlsm"))
}

uploadSRV <- function(id, attempt = function() {NULL}, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      values <- reactiveValues(data = NULL)
      
      observeEvent(input$file_upload,{
        filename <- input$file_upload$datapath
        data <- tryCatch({attempt(filename)}, error = function(e) {NULL})
        if (!is.null(data)) {
          values$data <- data
        } else {
          sheets   <- character(0)
          headers  <- character(0)
          continue <- T
          
          if (grepl("*[.]xlsx$|[.]xls$|[.]xlsm$", filename)) {
            tryCatch({sheets <- excel_sheets(filename)}, error = function(e) {})
            tryCatch({
              df <- read_excel(filename, 1, n_max = 0, .name_repair = "minimal")
              y <- convert_intTOdate(names(df)) %>% 
                duplicated_check()
              # headers <- unique(y)
              headers <- y
              
            }, error = function(e) {})
            
          } else if (grepl("*[.]csv$", filename)) {
            tryCatch({headers <- unique(names(read.csv(filename, nrows = 0)))}, error = function(e) {})
          } else {
            showModal(modalDialog("Unexpected file type!"))
            continue <- F
          }
          if (continue) {
            showModal(modalDialog(
              if (length(sheets) > 0) selectInput(session$ns("selectSheet"), "Select sheet", choices = sheets, selected = sheets[1]),
              selectizeInput(session$ns("selectColumns"), "Select columns to read", headers, headers, multiple = T,
                             options = list(plugins = list("remove_button"))),
              sliderInput(session$ns("sliderSkipRows"), "Row skip", 0, 30, 0, 1),
              title     = "Select which table to load",
              footer    = tagList(modalButton("Dismiss"), actionButton(session$ns("btnOK"), "OK")),
              size      = "l",
              easyClose = F))
          }
        }
      })
      
      updateHeaders <- reactive({
        list(req(input$selectSheet), req(input$sliderSkipRows))
      })
      
      observeEvent(updateHeaders(), {
        if (!is.null(input$selectSheet)) {
          filename <- input$file_upload$datapath
          headers <- character(0)
          
          tryCatch({
            df <- read_excel(filename, input$selectSheet, n_max = 0, skip = req(input$sliderSkipRows), .name_repair = "minimal")
            y <- convert_intTOdate(names(df)) %>% 
              duplicated_check()
            # headers <- unique(y)
            headers <- y
          },
          
          error = function(e) {}
          )
          
          if (length(headers) == 0) {
            tryCatch({headers <- unique(names(read.csv(filename, nrows = 0, skip = req(input$sliderSkipRows))))}, error = function(e) {})
          }
          updateSelectizeInput(session, "selectColumns", choices = headers, selected = headers)
        }
      })
      
      observeEvent(input$btnOK, {
        removeModal()
        filename <- input$file_upload$datapath
        tryCatch({
          if (grepl("*[.]xlsx$|[.]xls$|[.]xlsm$", filename)) {
            data <- as.data.frame(read_excel(filename, sheet = input$selectSheet, skip = input$sliderSkipRows, na = ""))
            # data <- as.data.frame(read_excel(filename, sheet = input$selectSheet, skip = input$sliderSkipRows, na = "",
            #                                  col_types = "text", .name_repair = "minimal"))
            y <- convert_intTOdate(names(data)) %>% 
              duplicated_check()
            names(data) <- y
            data
            
          } else if(grepl("*[.]csv$", filename)) {
            data <- read.csv(filename, skip = input$sliderSkipRows,
                             na.strings = "", stringsAsFactors = F, colClasses = "character")
            
          }
          
          values$data <- data[, colnames(data) %in% input$selectColumns]
        })
      })
      return(reactive({values$data}))
    })
}

#---------------------
# HOW TO :
#---------------------
# https://www.youtube.com/watch?v=ng7uLN1uxV4
# render_table <- function(df){
#   
#   # DT::datatable(df, rownames= FALSE, options = (list(scrollX = TRUE)) %>% head())
#   # DT::datatable(df, rownames= FALSE, filter = 'top') --> TODO not working now
#   # DT::datatable(df, editable = 'cell')
#   DT::datatable(df, extensions = 'Buttons',
#                 options = list(dom = 'Bfrtip',
#                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                scrollX = TRUE))
# } 
# 
# ui <- fluidPage(
#   uploadUI("model1", "Select CSV or Excel File")
# )
# 
# server <- function(input, output, session){
#   df <- uploadSRV("model1")
#   output$table <- DT::renderDataTable( 
#     render_table(df())
#   )
# }
# 
# shinyApp(ui, server)

