home_page <- makePage(
  "",
  "",
  ""
)
# ------------
table_page <- makePage(
  "Select Excel File to Render Table",
  "",
  contents = div(
    hr(),
    fluidRow(
      uploadUI("file_upload", ""),
      makeCard("", DT::DTOutput("table"), style = " background-color : lightgrey;"),
      choices_UI("floorDefine"),
      # PrimaryButton.shinyInput("floorDefineBtn", text = "All floor names = ? Click to assign"),
      verbatimTextOutput("floorDefinePrint")
    )
  )
)

# ------------
wbs_page <- makePage(
  "Working Breakdown Structure : WBS",
  "",
  contents = div(
    fluidRow(
      div(class = 'col-lg-4 col-md-12 col-sm-12',
          makeCard("WBS LEVEL 1 - unit : THB",
                   Stack(
                     plotlyOutput("wbs1"),
                     PrimaryButton.shinyInput("wbs_Btn1", text = "Click to Render")
                   ),
                   style = " background-color : lightgrey;"
          )
      ),
      div(class = 'col-lg-4 col-md-12 col-sm-12',
          makeCard("WBS LEVEL 2 - unit : THB",
                   Stack(combo_UI("combo2"),
                         pi_plotUI("pi_plot2"),
                         PrimaryButton.shinyInput("wbs_Btn2", text = "Click to Render")
                   ),
                   style = " background-color : lightgrey;"
          )
      ),
      div(class = 'col-lg-4 col-md-12 col-sm-12',
          makeCard("WBS LEVEL 3 - unit : THB",
                   Stack(combo_UI("combo3"),
                         pi_plotUI("pi_plot3"),
                         PrimaryButton.shinyInput("wbs_Btn3", text = "Click to Render")
                   ),
                   style = " background-color : lightgrey;"
          )
      )
    ),
    fluidRow(
      div(class = 'col-lg-4 col-md-12 col-sm-12',
          makeCard("WBS LEVEL 4 - unit : THB",
                   Stack(combo_UI("combo4"),
                         pi_plotUI("pi_plot4"),
                         PrimaryButton.shinyInput("wbs_Btn4", text = "Click to Render")
                   ),
                   style = " background-color : lightgrey;"
          )
      ),
      div(class = 'col-lg-8 col-md-12 col-sm-12',
          makeCard("DESCRIPTION : multi-unit : m, m2, m3, kg,...",
                   Stack(combo_UI("combo5"),
                         pi_plotUI("pi_plot5"),
                         PrimaryButton.shinyInput("wbs_Btn5", text = "Click to Render")
                   ),
                   style = " background-color : lightgrey;"
          )
      )
    )
  )
)

# ------------
floor_wbs <- makePage(
  "FLOOR WBS",
  "",
  contents = div(
    uiOutput('floors_ui'),
    fluidRow(
      div(class = 'col-lg-12 col-md-12 col-sm-12',
          Stack(
            h3("Select a floor to analysis"),
            combo_UI('floor_selected'),
            shiny::tags$hr(),
            PrimaryButton.shinyInput("floorBtn", text = "Select a floor and click"),
            makeCard("", DT::DTOutput("floor_table"), style = " background-color : lightgrey;")
            
          )
      )
    ),
    fluidRow(
      div(class = 'col-lg-4 col-md-12 col-sm-12',
          Stack(
            makeCard("WBS LEVEL 3 - unit : THB",
                     Stack(
                       plotlyOutput("floor"),
                       verbatimTextOutput('floor_cost')
                     ),
                     style = " background-color : lightgrey;"
            )
          )
      ),
      div(class = 'col-lg-4 col-md-12 col-sm-12',
          makeCard("WBS LEVEL 4 - unit : THB",
                   Stack(combo_UI("combo6"),
                         pi_plotUI("pi_plot6"),
                         PrimaryButton.shinyInput("wbs_Btn6", text = "Click to Render")
                   ),
                   style = " background-color : lightgrey;"
          )
      ),
      div(class = 'col-lg-4 col-md-12 col-sm-12',
          makeCard("DESCRIPTION : multi-unit : m, m2, m3, kg,...",
                   Stack(combo_UI("combo7"),
                         pi_plotUI("pi_plot7"),
                         PrimaryButton.shinyInput("wbs_Btn7", text = "Click to Render")
                   ),
                   style = " background-color : lightgrey;"
          )
      )
    )
    
  )
)

# ------------
material_query <- makePage(
  "MATERIAL QUERY",
  "",
  contents = div(
    fluidRow(
      Stack(
        textInput('query', "Search",
                  placeholder = 'concrete, formwork, 20mm, ...'),
        PrimaryButton.shinyInput("matBtn1", text = "Click to Render Table"),
        makeCard("", DT::DTOutput("mat_table"), style = " background-color : lightgrey;")
      )
    ),
    fluidRow(
      Stack(
        ChoiceGroup.shinyInput("radio", label = "Select Model",
                               options = options_created(c("stack", "subplot")),
                               value = "stack"
        ),
        PrimaryButton.shinyInput("matBtn2", text = "Click to Render Graph"),
        hr(),
        makeCard("Query Plot",
                 plotlyOutput("mat_plot"),
                 style = " background-color : lightgrey;")
       
      )
    )
  )
)

# ------------
progress_report <- makePage(
  "",
  "",
  content <- div(
    fluidRow(
      br(),
      div(class = 'col-lg-3 col-md-3 col-sm-6',
          makeCard("Previous", vboxUI("vb1"), style = " background-color : #edb707;")
      ),
      div(class = 'col-lg-3 col-md-3 col-sm-6',
          makeCard("This Month", vboxUI("vb2"), style = " background-color : green;")
      ),
      div(class = 'col-lg-3 col-md-3 col-sm-6',
          makeCard("Acc.This Month", vboxUI("vb3"), style = " background-color : #03adfc;")
      ),
      div(class = 'col-lg-3 col-md-3 col-sm-6',
          makeCard("Status", vboxUI("vb4"), style = " background-color : #fc9003;")
      ),
    ),

    fluidRow(
      div(class = 'col-lg-12 col-md-12 col-sm-12',
          Stack(
            makeCard("", sct_plotUI("scatter1"), style = " background-color : lightgrey;"),
          )
      ),
      PrimaryButton.shinyInput("progressBtn", text = "Render Progress")
    )
  )
)