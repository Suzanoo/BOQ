home_page <- makePage(
  content <- div(
    h3('วิธีใช้งาน'),
    p('-Dashboard เขียนด้วย ภาษา R ผู้ใช้ที่ลง R และ RStudio ไว้ในเครื่องสามารถ download source code ไปรันบนเครื่องได้ที่ github :  ได้ที่ github :  ',
      a(href = "https://github.com/Suzanoo/boq", "https://github.com/Suzanoo/boq")),
    
    p('-สามารถ upload excel file มาวิเคราะ์ได้โดยมีรูปแบบการเตรียมอธิบายด้านล่าง'),
    p('-ในแพคเกจ จะมีไฟล์ตัวอย่าง  BOQ.xlsx ใช้เป็น default ของ dashboard นี้ ผู้ใช้สามารถ download ไฟล์นี้ที่ tab Table หรือ จาก package ที่ดาวน์โหลดมาจากลิงค์ด้านบนก็ได้วน์โหลดมาจากลิงค์ด้านบนก็ได้'),
    p('ในไฟล์ตัวอย่าง BOQ.xlsx แบ่งเป็น 3 ส่วน'),
    tags$ol(
      tags$li('ส่วนที่จำเป็นต้องมีและตั้งชื่อให้ตรงกัน ได้แก่ column --> “WBS_1", "WBS_2", "WBS_3", "WBS_4", "DESCRIPTION", "UNIT",
“QTY”, "MAT.", "LAB.", "TOTAL”LAB.", "TOTAL"'),
      tags$li('ส่วนของชื่อแต่ละชั้น ตั้งชื่อต่างกันได้  โปรมแกรมจะให้ระบุชื่อที่หน้าเพจ BOQ Table ได้แก่ column --> “SUB, "L1”, “L2”, “L3”, “L4”, “Extenal”, ...’ หรือถ้าไม่ต้องการแยกเป็นชั้นๆก็ใส่ปริมาณรวมไว้ที่ column “QTY” ทั้งหมดก็ได้ และ tab WBS by Floor Select ก็ข้ามไป'),
      tags$li('ส่วน Progress - S curve ไม่จำเป็นต้องมี ได้แก่ column -->”Percent_Wt”, column  วันที่ต่างๆ แต่ถ้าต้องการทำ S-curve ดูการเตรียมข้อมูลจากไฟล์ BOQ.pdf ที่ดาวน์โหลดจากลิงค์ด้านบน'),
      tags$li('ทุกๆ column สามารถสลับตำแหน่งได้'),
    ),
    
    p('-Dashboard มีทั้งหมด 6 pages'),
    tags$ol(
      tags$li('เพจ Home'),
      tags$li('เพจ BOQ Table : แสดงในรูปแบบตาราง และด้านล่างมีปุ่มให้ผู้ใช้ป้อนข้อมูลชื่อชั้น เพื่อนำไปใช้งานที่เพจ WBS Floor Select ต่อไป'),
      tags$li('WBS : แสดงกราฟของ working breakdown structure ระดับ 1-4 ซึ่งเพียงพอสำหรับทำ master schedule โดย 3 กราฟแรกหน่วยเป็นเงิน กราฟสุดท้ายมีหน่วยตามวัสดุในตาราง excel เช่น concrete = m3, formwork = m2, …'),
      tags$li('WBS by Floor Select  เหมือนข้อ 3 แต่แสดงกราฟของเฉพาะชั้นตามที่ผู้ใช้เลือก โดยอ้างอิงชื่อของชั้นตามที่ผู้ใช้กรอกให้ข้อมูลในหน้า Table'),
      tags$li('Material Query : แสดงรายการวัสดุตามที่ผู้ใช้ป้อนคำสอบถามเช่น 320ksc, formwork, D20, ...'),
      tags$li('Progress Report : หน้านี้ไม่จำเป็นต้องมี แต่ถ้าใน file excel มีข้อมูล progress สามารถ render graph ได้ รูปแบบการเตรียมข้อมูลดูได้จากไฟล์ BOQ.pdf ที่โหลดจากแพคเกจจากลิงค์ด้านบน'),
      
    ),
    p('-โปรแกรมใช้ แพคเกจ plotly ในการทำกราฟ สามารถดูข้อมูลบนกราฟแบบ interaction ได้ เป็นประโยชน์มากในการ presentation')
    
  )
)
# ------------
table_page <- makePage(
  contents = div(
    h3("Upload new file"),
    hr(),
    fluidRow(
      uploadUI("file_upload", ""),
      makeCard("", DT::DTOutput("table"), style = " background-color : lightgrey;"),
      h4("Floor name of new file = ???  Click!"),
      h5("Skip this if no have new file"),
      choices_UI("floorDefine"),
      # PrimaryButton.shinyInput("floorDefineBtn", text = "All floor names = ? Click to assign"),
      verbatimTextOutput("floorDefinePrint")
    )
  )
)

# ------------
wbs_page <- makePage(
  contents = div(
    h3("Working Breakdown Structure : WBS"),
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
  contents = div(
    h3("MATERIAL QUERY"),
    h6("Multi-query with comma ',' ex. --> 320ksc, 240ksc, lean"),
    fluidRow(
      Stack(
        textInput('query', "Search",
                  placeholder = 'concrete, formwork, 20mm, ...'),
        PrimaryButton.shinyInput("matBtn1", text = "Click to Render Table"),
        makeCard("",
                 DT::DTOutput("mat_table"),
                 style = " background-color : lightgrey;"),
        
      )
    ),
   
    fluidRow(
      Stack(
        ChoiceGroup.shinyInput("radio", label = "Select Model",
                               options = options_created(c("stack", "subplot")),
                               value = "stack"
        ),
        hr(),
        PrimaryButton.shinyInput("matBtn2", text = "Click to Render Graph"),
        makeCard("Query Plot",
                 plotlyOutput("mat_plot"),
                 style = " background-color : lightgrey;")
       
      )
    ),
    
  )
)

# ------------
progress_report <- makePage(
  content <- div(
    h3("Progrress Report"),
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