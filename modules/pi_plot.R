# https://r-graph-gallery.com/piechart-ggplot2.html

# function to create option of ComboBox.shinyInput
options_created <- function(data){
  data %>% 
    as_tibble() %>% 
    setNames("key") %>% 
    mutate(text = data)
}

pi_plotUI <- function(id){
  ns <- shiny::NS(id)
  div(
    hr(),
    plotlyOutput(ns("pi_plot"))
  )
}

pi_plotSRV <- function(id, data, selector, polar = FALSE, logscale = FALSE){
  moduleServer(
    id, 
    function(input, output, session){
      output$pi_plot <- renderPlotly({
        req(data)
        if(!is.null(data)){
          z <- selector
          if(!is.null(z)){
            if(logscale == FALSE){
              df <- data %>% 
                filter(.[[1]] == z) %>% # at column1 : filter by input from user select and want to plot its child
                select(2, AMOUNT) %>% # at column2 : select child item to plot
                setNames(c("DOMAIN", "AMOUNT")) %>% 
                dplyr::group_by_at(1) %>% 
                dplyr::summarise(AMOUNT = sum(AMOUNT)) %>% 
                as.data.frame()
              
              # bar plot
              if(polar == FALSE){
                df %>% 
                  plotly::plot_ly(x = ~DOMAIN, y = ~AMOUNT, type = 'bar', color = ~DOMAIN) %>% 
                  plotly::layout(
                    xaxis = list(title = FALSE, showticklabels = FALSE),
                    legend = list(orientation = "h", # show entries horizontally
                                  xanchor = "center", # use center of legend as anchor
                                  x = 0.5)
                  )
              }
              # donut plot
              else{
                df %>% 
                  plotly::plot_ly(labels = ~DOMAIN, values = ~AMOUNT) %>%
                  plotly::add_pie(hole = 0.6) %>% 
                  plotly::layout(
                    legend = list(orientation = "h", # show entries horizontally
                                  xanchor = "center", # use center of legend as anchor
                                  x = 0.5)
                  )
              }
            }
            # bar-log scale plot
            else{
              df <- data %>% 
                filter(.[[1]] == z) %>% # at column1 : filter by input from user select and want to plot its child
                select(2, QTY) %>% # at column2 : select child item to plot
                setNames(c("DOMAIN", "QTY")) %>% 
                as.data.frame()
              
              df %>% 
                # https://plotly.com/r/builtin-colorscales/
                plotly::plot_ly(x = ~DOMAIN, y = ~QTY, type = 'bar', color = ~DOMAIN) %>% 
                plotly::layout(yaxis = list(title = 'Qty (Log Scale)', type = 'log'),
                               xaxis = list(title = FALSE, showticklabels = FALSE),
                               legend = list(orientation = "h", # show entries horizontally
                                             xanchor = "center", # use center of legend as anchor
                                             x = 0.5)
                )
            }
          }
        }
      })
    })
}

#---------------------
# HOW TO :
#---------------------
# ui <- fluidPage(
#   pi_plotUI("pi_plot1")
# )
# 
# server <- function(input, output, session){
#   df <- reactive({
#     file_path <- "../BOQ.xlsx"
#     # file_path <- (paste0(getwd(), "/BOQ.xlsx"))
#     df<- readxl::read_excel(path = file_path, sheet = 1, na = "0") %>%
#       filter(QTY != 0) %>% 
#       replace(is.na(.), 0)
#     df
#   })
# 
#   observe({
#     req(data())
#     df1 <- df() %>%
#       # If u want to render column 1
#       # U will select column 1, 2 and Amount
#       select(1, 2, Amount)
# 
#     pi_plotSRV("pi_plot1", df1)
#   })
# }
# 
# shinyApp(ui, server)






