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
      choices_UI("floor_name"),
      PrimaryButton.shinyInput("fBtn", text = "All floor names = ? Click to assign"),
      verbatimTextOutput("floor_print")
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
  "",
  "",
  contents = div(
    
  )
)

# ------------
material_query <- makePage(
  "",
  "",
  contents = div(
    
  )
)

# ------------
progress_report <- makePage(
  "",
  "",
  contents = div(
    
  )
)