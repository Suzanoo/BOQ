# Tutorials
# https://appsilon.github.io/shiny.fluent/articles/st-sales-reps-dashboard.html

# Official docs
# https://developer.microsoft.com/en-us/fluentui#/controls/web

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
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
source("modules/vbox.R")
source("modules/scatter.R")
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
    div(
      HTML('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">'),
      HTML('<link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">')
      ),
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    div(
      HTML('<script src="main.js" defer></script>')
    ),
    shiny_router_script_tag
  ),
  collapsible = TRUE,
)

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
  floor = c("SUB", "L1", "1M", "L2", "L3", "MC1", "MC2",
            "MC3", "MC4", "MC5", "MC6", "L4", "L4M", "External")  
  
  # call module upload
  data0 <- uploadSRV("file_upload")
  
  data <- reactive({
    # default --> use sample excel 
    if(is.null(data0())){
      as.data.frame(read_excel("BOQ.xlsx", sheet = 1, na = "")) %>% 
        filter(QTY != 0) %>% # get rid of missing value for table render and bar&pi render
        replace(is.na(.), 0) %>% 
        mutate(dplyr::across(is.numeric, round, digits = 4))
      
    }
    # use new file from upload
    else{
      data0() %>% 
        filter(QTY != 0) %>% # get rid of missing value for table render and bar&pi render
        replace(is.na(.), 0) %>% 
        mutate(dplyr::across(is.numeric, round, digits = 4))
      
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
  
  output$table <- DT::renderDataTable(server = FALSE, {# server = FALSE == download whole table
    if(!is.null(df())){
      render_table(df())
    }
  })

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
      pull()
  })
  
  combo2 <- callModule(combo_SRV, "combo2", v2())
  
  observeEvent(input$wbs_Btn2, {
    req(df())
    df1 <- df() %>%
      select(1, 2, AMOUNT) # Level 1, Level 2, AMOUNT
    pi_plotSRV("pi_plot2", df1, combo2(), polar = TRUE)
  })
  
  # # render WBS level 3
  v3<- reactive({
    req <- df()
    df() %>% 
      select(2) %>%
      unique() %>% 
      pull()
  })
  
  combo3 <- callModule(combo_SRV, "combo3", v3())
  
  observeEvent(input$wbs_Btn3, {
    req(df())
    df1 <- df() %>%
      select(2, 3, AMOUNT) # Level 2, Level 3, AMOUNT
    pi_plotSRV("pi_plot3", df1, combo3())
  })
  
  ## render WBS level 4
  v4 <- reactive({
    req <- df()
    df() %>% 
      select(3) %>%
      unique() %>% 
      pull()
  })
  
  combo4 <- callModule(combo_SRV, "combo4", v4())
  
  observeEvent(input$wbs_Btn4, {
    req(df())
    df1 <- df() %>%
      select(3, 4, AMOUNT)
    pi_plotSRV("pi_plot4", df1, combo4())
  })
  
  ## render Description
  v5 <- reactive({
    req <- df()
    df() %>% 
      select(4) %>%
      unique() %>% 
      pull()
  })
  
  combo5 <- callModule(combo_SRV, "combo5", v5())
  
  observeEvent(input$wbs_Btn5, {
    req(df())
    df1 <- df() %>%
      select(4, 5, QTY) # Level 3, Level 4, QTY
    pi_plotSRV("pi_plot5", df1, combo5(), logscale = TRUE)
  })
  
  #---------------------------
  ## WBS by Floor Selected
  #---------------------------
  # placeholder
  f <- reactiveValues(selector = NULL, Z = NULL)
  s <- reactive({
    req(df())
    names(df())
  })
  
  # create choice for user select floor names
  choices <- choices_SRV("floorDefine", s())

  # update a floor from user
  observe({
    if(is.null(choices())){
      f$selector <- floor
      f$Z <- callModule(combo_SRV, "floor_selected",  f$selector)
    }else{
      f$selector <-  choices()
      f$Z <- callModule(combo_SRV, "floor_selected",  f$selector)
    }
  })

  # print selected floor name
  output$floorDefinePrint <- renderPrint(
    choices()
  )
  
  # create data frame matches floor selected
  floor_df <- eventReactive(input$floorBtn, {
    selector <- f$Z()$text
    df <- df()
    
    # in case no have floor column of new file upload
    # prevent Warning: Error in if: argument is of length zero
    # https://stackoverflow.com/questions/27350636/argument-is-of-length-zero-in-if-statemen
    
    check <- names(df)
    
    if(is.null(selector)){
      selector <- NULL
    }else{
      if(!selector %in% check){
        selector <- NULL
      }else{
        selector <- selector
      }
    }

    if(!is.null(selector)){
      y <- df %>% 
        select(matches(selector)) %>% # select spec floor column
        select(1) %>% # protect duplicate (ex. L4, L4M, ...) we select only column 1
        setNames(("QTY"))
      
      df %>% 
        select(matches(item_boq)) %>% 
        bind_cols(y) %>% 
        relocate(QTY, .after = UNIT) %>% 
        filter(!is.null(QTY) & QTY != 0 & QTY != "") %>% 
        mutate(AMOUNT = QTY*TOTAL)
    }else{
      shinyalert::shinyalert("Error!", "No floor column or  you are not define truth floor name at table tab or you are not select a floor before click render.", type = "error")
      return(NULL)
    }
  })
  
  # render table of selected floor
  output$floor_table <- DT::renderDataTable(server = FALSE, {
    if(!is.null(floor_df())){
      render_table(floor_df())
    }
  })

  # render wbs 3 of selected floor
  output$floor <- renderPlotly({
    req(floor_df())
    if(!is.null(floor_df())){
      floor_df() %>%
        select(WBS_3, AMOUNT) %>% # at column2 : select child item to plot
        setNames(c("DOMAIN", "AMOUNT")) %>%
        dplyr::group_by(DOMAIN) %>%
        dplyr::summarise(AMOUNT = sum(AMOUNT)) %>%
        as.data.frame() %>%
        plotly::plot_ly(labels = ~DOMAIN, values = ~AMOUNT) %>%
        plotly::add_pie(hole = 0.6) %>%
        plotly::layout(legend = list(orientation = "h", # show entries horizontally
                                     xanchor = "center", # use center of legend as anchor
                                     x = 0.5)) # put legend in center of x-axis
      
    }
  })
  
  output$floor_cost <- renderPrint({
    if(!is.null(floor_df())){
      AMOUNT <- floor_df() %>% 
        select(AMOUNT) %>% 
        unlist() %>% 
        sum()
      
      paste0(f$Z()$text, " : ", "Total cost = ", AMOUNT, " THB")
    }
    
  })
  
  ## render wbs 4 of selected floor
  v6 <- reactive({
    req(floor_df())
    if(!is.null(floor_df())){
      floor_df() %>%
        select(3) %>%
        unique() %>%
        pull()
    }
  })
  # TODO solve Warning in eval_tidy(qs) : restarting interrupted promise evaluation
  # https://mailund.dk/posts/promises-and-lazy-evaluation/
  combo6 <- callModule(combo_SRV, "combo6", v6())
  
  observeEvent(input$wbs_Btn6, {
    req(floor_df())
    df1 <- floor_df() %>%
      select(3, 4, AMOUNT)
    pi_plotSRV("pi_plot6", df1, combo6())
  })

  ## render description of selected floor
  v7 <- reactive({
    req(floor_df())
    if(!is.null(floor_df())){
      floor_df() %>%
        select(4) %>%
        unique() %>%
        pull()
    }
  })
  # TODO Warning in eval_tidy(qs) : restarting interrupted promise evaluation
  combo7 <- callModule(combo_SRV, "combo7", v7())
  
  observeEvent(input$wbs_Btn7, {
    req(floor_df())
    df1 <- floor_df() %>%
      select(4, 5, QTY)
    pi_plotSRV("pi_plot7", df1, combo7(), logscale = TRUE)
  })
  
  
  #---------------------------
  ## Material Query
  #---------------------------
  mat_query <- eventReactive(input$matBtn1, {
    df <- df()
    query <- input$query
    
    # filter matches query
    if(input$query != ""){
      materialQuery(df, query)
    }else{
      shinyalert::shinyalert("Error!", "Assign a query item first", type = "error")
      return(NULL)
    }
  })
  
  # render table of query item
  output$mat_table <- DT::renderDataTable(server = FALSE, {
    if(!is.null(mat_query())){
      render_table(mat_query())
    }
  })
  
  # render graph of query item
  # mode <- radioSRV("radio1", c("stack", "subplot"))
  observeEvent(input$radio, {
    x <- input$radio
    updateChoiceGroup.shinyInput(
      session = shiny::getDefaultReactiveDomain(),
      "radio",
      label = ("Select  Model"),
      options = options_created(c("stack", "subplot")),
      value = x)
  })
  
  query_plot <- eventReactive(input$matBtn2, {
    df <- mat_query()
    query <- input$query
    floors_name <- f$selector
    mode <- input$radio
    
    if(input$query != "" & !is.null(input$query)){
      if(mode == "stack"){
        stackBarPlot(df, floors_name)
      }else{
        multi_barplot(df, floors_name)
      }
      
    }else{
      shinyalert::shinyalert("Error!", "Assign a query item first", type = "error")
      return(NULL)
    }
  })
  
  output$mat_plot <- renderPlotly({
    if(!is.null(query_plot())){
      query_plot()
    }
  })
  
  #---------------------------
  ## Progress Report
  #---------------------------
  # create estimate and actual progress table prepare for plotting
  df1 <- reactive({
    file_path <- (paste0(getwd(), "/BOQ.xlsx"))
    tryCatch(
      expr = {
        # read plan sheet 2
        df1 <- readxl::read_excel(path = file_path, sheet = 2, na = "") %>%
          filter(QTY != 0) %>%
          filter(AMOUNT != 0) %>% 
          replace(is.na(.), 0) %>%
          mutate(across(is.numeric, round, digits = 4)) %>% 
          mutate(weight = AMOUNT/sum(AMOUNT)) %>% 
          relocate(weight, .after = AMOUNT)
        
      },
      error = function(e){
        shinyalert::shinyalert("Error!", "No plan sheet or actual sheet in BOQ.xlsx file.", type = "error")
      }
    )
  })
  
  df2 <- reactive({
    file_path <- (paste0(getwd(), "/BOQ.xlsx"))
    tryCatch(
      expr = {
        # read plan sheet 2
        df2 <- readxl::read_excel(path = file_path, sheet = 3, na = "") %>%
          filter(QTY != 0) %>%
          filter(AMOUNT != 0) %>% 
          replace(is.na(.), 0) %>%
          mutate(across(is.numeric, round, digits = 4)) %>% 
          mutate(weight = AMOUNT/sum(AMOUNT)) %>% 
          relocate(weight, .after = AMOUNT)
        
      },
      error = function(e){
        shinyalert::shinyalert("Error!", "No plan sheet or actual sheet in BOQ.xlsx file.", type = "error")
      }
    )
  })
  
  
  plan.actual.table <- eventReactive(input$progressBtn, {
    
    df1 <- df1()
    df2 <- df2()

    if(!is.null(df1) & !is.null(df2)){
      
      # plan table
      names(df1) <- int_to_date(names(df1)) # change date-time format ex. 44592 --> 31/1/22
      index1 <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df1))
      
      if(length(index1) != 0){
        plan <- df1[index1] # fiter only date-time column
        
        weight <- df1 %>% select(matches("weight"))
        
        plan <- plan %>%
          mutate_all(.,function(col){weight$weight*col/100})
        
        plan <-  progress_table(plan)
        
      }else{
        shinyalert::shinyalert("Error!", "No date-time column in plan sheet", type = "error")
        plan <- NULL
        plan
      }
      
      # actual table
      names(df2) <- int_to_date(names(df2))
      index2 <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df2))
      
      if(length(index2) != 0){
        actual <- df2[index2]
        weight <- df2 %>% select(matches("weight"))
        
        actual <- actual %>%
          mutate_all(.,function(col){weight$weight*col/100})
        
        actual <-  progress_table(actual) %>% 
          filter(progress != 0)
      }else{
        shinyalert::shinyalert("Error!", "No date-time column in actual sheet", type = "error")
        actual <- NULL
        actual
      }
      
    }else{
      shinyalert::shinyalert("Error!", "No plan sheet or actual sheet in BOQ.xlsx file.", type = "error")
    }

    # store & return
    if(!is.null(plan) & !is.null(actual)){
      list(plan, actual)
    }else{
      NULL
    }

  })
  
  # render S-curve
  observeEvent(input$progressBtn, {
    req(plan.actual.table())
    list <- plan.actual.table()

    if(!is.null(list)){
      plan <- list[[1]]
      actual <- list[[2]]
      sct_plotSRV("scatter1", plan, actual)
    }
  })
  
  # render value box
  observeEvent(input$progressBtn, {
    req(plan.actual.table())
    df1 <- df1()
    df2 <- df2()
    
    if(!is.null(df1) & !is.null(df1)){
      col <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df1))
      time <- length(col)
      
      list <- plan.actual.table()
      plan <- list[[1]] %>% mutate(dplyr::across(is.numeric, round, digits = 2))
      actual <- list[[2]] %>% mutate(dplyr::across(is.numeric, round, digits = 2))
      
      x <- actual %>% 
        select(progress) %>% 
        filter(progress > 0) %>% 
        pull() %>% 
        length()
      
      this <- actual$progress[x]
      acc.this <- actual$csum[x]
      acc.previous <- actual$csum[x-1]
      status <- round((acc.this - plan$csum[x])*time*30/100, digits = 2) #convert month to days
      
      # Actual table
      df1 <- actual %>%
        select(date, progress) %>%
        setNames(c("x", "y")) %>%
        as.data.frame()
      
      # Acc.Actual table
      df2 <- actual %>%
        select(date, csum) %>%
        setNames(c("x", "y")) %>%
        as.data.frame()
      
      vb1 <- list(
        value = paste0(acc.previous,"%"),
        data = df2,# with column name x, y
        label = "",
        chart_type = "",
        subtitle = "",
        info = "",
        icon = icon("code"),
        color = "orange"
      )
      
      vboxSRV("vb1", vb1)
      
      vb2 <- list(
        value = paste0(this,"%"),
        data = df1,# with column name x, y
        label = "",
        chart_type = "",
        subtitle = "",
        info = "",
        icon = icon("code"),
        color = "orange"
      )
      
      vboxSRV("vb2", vb2)
      
      vb3 <- list(
        value = paste0(acc.this,"%"),
        data = df1,# with column name x, y
        label = "",
        chart_type = "column",
        subtitle = "",
        info = "",
        icon = icon("code"),
        color = "orange"
      )
      
      vboxSRV("vb3", vb3)
      
      vb4 <- list(
        value = paste0(status, " days"),
        data = df2,# with column name x, y
        label = "",
        chart_type = "line",
        subtitle = "",
        info = "",
        icon = icon("code"),
        color = "orange"
      )
      
      vboxSRV("vb4", vb4)
    }

  })
}

shinyApp(ui, server)