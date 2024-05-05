library(shiny)
options_created <- function(data){
  data %>% 
    as_tibble() %>% 
    setNames("key") %>% 
    mutate(text = data)
}

combo_UI <- function(id) {
  ns <- NS(id)
  ComboBox.shinyInput(ns("combo"), options = NULL, placeholder = "Select one to Render")
}

combo_SRV <- function(input, output, session, selector = NULL) {
  ns <- session$ns
  
  observeEvent(selector, {
    y <- input$combo
    updateComboBox.shinyInput(
      session = session,
      inputId = 'combo',
      options = options_created(c(selector)),
      value = y 
    )
  })

  return(reactive({input$combo}))
}

# ui <- fixedPage(
#   combo_UI("Module1"),
#   combo_UI("Module2"),
#   textOutput("mod1Text"),
#   textOutput("mod2Text")
# )
# 
# server <- function(input, output, session) {
# 
#   mod1 <- callModule(combo_SRV, "Module1", c("Cat", "Dog", "Rabbit", "Fish"))
#   mod2 <- callModule(combo_SRV, "Module2", c("Apple", "Orange", "Pear", "Rambutan"))
# 
#   observe({
#     if (is(mod1(), "character")) print("Ah-ha!")
#   })
# 
#   output$mod1Text <- renderText({
#     print(mod1())
#     paste("Module 1 selection is", mod1())
#   })
# 
#   output$mod2Text <- renderText({
#     print(mod2())
#     paste("Module 2 selection is", mod2())
#   })
# }
# 
# shinyApp(ui, server)