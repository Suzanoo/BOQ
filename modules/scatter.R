sct_plotUI <- function(id){
  ns <- shiny::NS(id)
  div(
    hr(),
    plotlyOutput(ns("scatter_plot"))
  )
}

sct_plotSRV <- function(id, df1, df2){
  moduleServer(
    id, 
    function(input, output, session){
      output$scatter_plot <- renderPlotly({
        if(!is.null(df1)){
          # fig <- plot_ly(df, x = ~date)
          # fig <- fig %>% add_trace(y = ~acc.plan, name = 'PLAN', type = "scatter",  mode = 'lines+markers')
          # fig <- fig %>% add_trace(y = ~acc.actual, name = 'ACTUAL', type = "scatter",mode = 'lines+markers')
          # fig
          
          fig <- plot_ly(df1, x = ~date) %>% 
            plotly::layout(xaxis = list(tickangle = 90))
          fig <- fig %>% add_trace(y = ~csum, name = 'PLAN', type = "scatter",  mode = 'lines+markers')
          fig <- fig %>% add_trace(data = df2, y = ~csum, name = 'ACTUAL', type = "scatter",mode = 'lines+markers')
          fig
        }
        
      })
    })
}

#---------------------
# HOW TO :
#---------------------
# source("upload.R")
# 
# ui <- fluidPage(
#   uploadUI("model1", "Select Excel File"),
#   sct_plotUI("plot1")
# )
# 
# server <- function(input, output, session){
#   data <- uploadSRV("model1")
#   
#   df <- reactive({
#     req(data())
#     if(!is.null(data())){
#       data() %>% 
#         progress_table() %>%
#         as.data.frame()
#     }
#   })
# 
#   observe({
#     req(df())
#     df <- df()
#     if(!is.null(df)){
#       sct_plotSRV("plot1", df)
#     }
#   })
# }
# 
# shinyApp(ui, server)





