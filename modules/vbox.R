## https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
library(shiny)
library(dplyr)
library(highcharter)
hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

vbox_render <- function(vb){
  list <- vb
  #chart type -> "area", "line", "column"
  hc <- hchart(list$data, list$chart_type, hcaes(x, y), name = list$label)  %>% 
    hc_size(height = 75) %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  valueBoxSpark(
    value = list$value,
    title = toupper(list$label),
    sparkobj = hc,
    subtitle = tagList(HTML("&uarr;"), list$subtitle),
    info = list$info,
    icon = list$icon,
    width = 4,
    color = list$color,
    href = NULL
  )
}
# -----------
# MODULES
# -----------
vboxUI <- function(id){
  ns <- NS(id)
  shinydashboard::valueBoxOutput(outputId = ns("vbox"))
}

vboxSRV <- function(id, list){
  
  moduleServer(
    id,
    function(input, output, session){
      output$vbox <- shinydashboard::renderValueBox({
        vbox_render(list)
      })
    }
  )
}

# -----------
# HOW TO
# -----------
# set.seed(123)
# 
# N <- 20
# 
# x <- cumsum(rnorm(N)) + 0.5 * cumsum(runif(N))
# x <- round(200*x)
# 
# df <- data.frame(
#   x = sort(as.Date(Sys.time() - lubridate::days(1:N))),
#   y = abs(x)
# )
# 
# # -----------
# # TODO why color in fluidpage
# ui <- fluidPage(
#   vbox_ui("vb1"),
#   vbox_ui("vb2"),
#   vbox_ui("vb3")
# )
# 
# # ui <- shinydashboard::dashboardPage(
# #   shinydashboard::dashboardHeader(),
# #   shinydashboard::dashboardSidebar(disable = TRUE),
# #   shinydashboard::dashboardBody(
# #     h4("Hello"),
# #     vbox_ui("vb1"),
# #     vbox_ui("vb2"),
# #     vbox_ui("vb3")
# #     
# #   )
# # )
# 
# server <- function(input, output, session){
#   
#   vb1 <- reactive({
#     list(
#       value = "1,345",
#       data = df,# with column name x, y
#       label = "Test",
#       chart_type = "line",
#       subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
#       info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
#       icon = icon("code"),
#       color = "orange"
#     )
#   })
#   
#   vbox_server("vb1", vb1())
#   vbox_server("vb2", vb1())
#   vbox_server("vb3", vb1())
# }
# 
# shinyApp(ui, server) 



#-------------------------------------------------------------





