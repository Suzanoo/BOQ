options_created <- function(data){
  if(!is.null(data)){
    data %>% 
      as_tibble() %>% 
      setNames("key") %>% 
      mutate(text = data)
  }
  
}

choices_UI <- function(id){
  ns <- shiny::NS(id)
  div(
    tagList(
      reactOutput(ns("modal")),
      PrimaryButton.shinyInput(ns("showModal"), text = "Select items"),
    )
  )
}

choices_SRV <- function(id, choices){
  moduleServer(
    id, 
    function(input, output, session){
      y <- reactiveValues(valurs = NULL)
      
      modalVisible <- reactiveVal(FALSE)
      observeEvent(input$showModal, modalVisible(TRUE))
      observeEvent(input$hideModal, modalVisible(FALSE))
      observeEvent(input$btnOK, modalVisible(FALSE))
      observeEvent(input$dismiss, modalVisible(FALSE))
      
      output$modal <- renderReact({
        Modal(isOpen = modalVisible(),
              
              Stack(tokens = list(padding = "15px", childrenGap = "10px"),
                    div(style = list(display = "flex"),
                        Text("Title", variant = "large"),
                        div(style = list(flexGrow = 1)),
                        IconButton.shinyInput(session$ns("hideModal"), iconProps = list(iconName = "Cancel")),
                    ),
                    div(
                      # p("A paragraph of text."),
                      # p("Another paragraph."),
                      Dropdown.shinyInput(session$ns("dropdown"),
                                          value = "NULL",
                                          options = options_created(choices),
                                          multiSelect = TRUE,
                                          placeholder = "Select items"),
                      
                      shiny::tagList(ActionButton.shinyInput(session$ns("dismiss"), "Dismiss"),
                                     ActionButton.shinyInput(session$ns("btnOK"), "OK"))
                      
                    )
              )
        )
      })
      observeEvent(input$btnOK, {
        y$values <- input$dropdown
        # print(y$values)
      })
      return(reactive({y$values}))
    })
}

#---------------------
# HOW TO :
#---------------------
# ui <- fluidPage(
#   choices_UI('modal1'),
#   PrimaryButton.shinyInput("button1", "OK")
# )
# 
# server <- function(input, output, session){
#   heads <- reactive({
#         file_path <- "../BOQ.xlsx"
#         # file_path <- (paste0(getwd(), "/BOQ.xlsx"))
#         df <- readxl::read_excel(path = file_path, sheet = 1, na = "0") %>%
#           filter(QTY != 0) %>%
#           replace(is.na(.), 0)
#         names(df)
#       })
# 
#   y <- choices_SRV('modal1', heads())
#   
#   observeEvent(input$button1, {
#     z <- y()
#     print(length(z))
#     print(paste0(z, sep = " "))
#   })
# }
# 
# shinyApp(ui, server)






