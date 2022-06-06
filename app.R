# Tutorials
# https://appsilon.github.io/shiny.fluent/articles/st-sales-reps-dashboard.html

# Official docs
# https://developer.microsoft.com/en-us/fluentui#/controls/web

library(shiny)
library(dplyr)
library(ggplot2)
library(glue)
library(sass)
library(shiny.fluent)
library(shiny.router)

rm(list = ls(), envir = globalenv())

source("function.R")
source("modules/upload.R")
source("modules/choices.R")
source("modules/combo.R")
source("modules/pi_plot.R")
source("layout.R")
source("page.R")
# ------------
# define the available routes:
router <- make_router(
  route("/", home_page),
  route("table", table_page),
  route("wbs", wbs_page),
  route("progress", progress_report),
  route("level", floor_wbs),
  route("material", material_query)
  )

# Add shiny.router dependencies manually:
# they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

# ------------
ui <- fluidPage(
  layout(router$ui),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    shiny_router_script_tag
))

# ------------
server <- function(input, output, session) {
  router$server(input, output, session)
  
  #---------------------------
  ## Upload excel
  #---------------------------
  # CONNSTANT
  item_boq <- c("WBS_1", "WBS_2", "WBS_3", "WBS_4", "DESCRIPTION",
                "UNIT", "MAT.", "LAB.", "TOTAL")
  
  # initial floor name --> change later by user
  floor = c("SUB", "L1", "1M", "L2", "L3", "CM1A/B", "CM2A/B",
            "CM3A/B", "CM4A/B", "CM5A/B", "CM6A/B", "L4", "L4M")  
  
  # call module upload
  data0 <- uploadSRV("file_upload")
  
  data <- reactive({
    # default --> use sample excel 
    if(is.null(data0())){
      data <- as.data.frame(read_excel("BOQ.xlsx", sheet = 1, na = "")) %>% 
        filter(QTY != 0) %>% # get rid of missing value for table render and bar&pi render
        replace(is.na(.), 0) %>% 
        mutate(across(is.numeric, round, digits = 4))
      
      data
    }
    # use new file from upload
    else{
      data <- data0() %>% 
        filter(QTY != 0) %>% # get rid of missing value for table render and bar&pi render
        replace(is.na(.), 0) %>% 
        mutate(across(is.numeric, round, digits = 4))
      
      data
    }
  })
  
  #---------------------------
  ## render table
  #---------------------------
  df <- reactive({
    req(data())
    df <- data() 
    
    # grep column of date-time
    col <- grep("[0-9]{4}...[0-9]{2}", names(df))
    if(!is.null(col)){
      df <- df %>% 
        select(-col) # get rid off date-time column
    }else{
      df
    } 
  })
  
  output$table <- DT::renderDataTable( 
    if(!is.null(df())){
      render_table(df())
    }
  )
  
  #---------------------------
  ## WBS 
  #---------------------------
  ## render bar&pi graph for WBS Level 1- Level 4
  # render WBS level 1
  v1 <- eventReactive(input$wbs_Btn1, {
    req <- df()
    df() %>%
      select(WBS_1, AMOUNT) %>% 
      setNames(c("DOMAIN", "AMOUNT")) %>%
      dplyr::group_by(DOMAIN) %>%
      dplyr::summarise(AMOUNT = sum(AMOUNT)) %>%
      as.data.frame()
  })
  
  output$wbs1 <- renderPlotly({
    req(v1())
    if(!is.null(v1())){
      v1() %>% 
        plotly::plot_ly(labels = ~DOMAIN, values = ~AMOUNT) %>%
        plotly::add_pie(hole = 0.6) %>%
        plotly::layout(legend = list(orientation = "h", # show entries horizontally
                                     xanchor = "center", # use center of legend as anchor
                                     x = 0.5)) # put legend in center of x-axis
    }
  })
  
  # render WBS level 2
  v2 <- reactive({
    req <- df()
    df() %>% 
      select(1) %>%
      unique() %>% 
      pull
  })
  
  combo2 <- callModule(combo_SRV, "combo2", v2())
  
  observeEvent(input$wbs_Btn2, {
    req(df())
    df1 <- df() %>%
      select(1, 2, AMOUNT) # Level 1, Level 2, AMOUNT
    pi_plotSRV("pi_plot2", df1, combo2(), polar = TRUE)
  })
  
  
  
  
  
  
  
  
  
  
 
}

shinyApp(ui, server)