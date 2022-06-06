makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

# https://rstudio.github.io/DT/shiny.html
# https://www.youtube.com/watch?v=ng7uLN1uxV4
render_table <- function(df){
  
  # DT::datatable(df, rownames= FALSE, options = (list(scrollX = TRUE)) %>% head())
  # DT::datatable(df, rownames= FALSE, filter = 'top', options = list(
  #   pageLength = 5, autoWidth = TRUE
  # )) #--> TODO not working now
  # DT::datatable(df, editable = 'cell')
  DT::datatable(df, rownames= FALSE, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               pageLength = 5,
                               scrollX = TRUE))
} 

# change date-time format of ex 44592 --> 31/1/22
convert_intTOdate <- function(y){
  list <- NULL
  for(i in c(1:length(y))){
    if(!is.na(stringr::str_match(y[i], "[0-9]{5}"))){
      list[i] = as.Date(as.numeric(y[i]), origin = "1899-12-30") %>%
        as.character() #save to as character of date format
    }else{
      list[i] = y[i]
    }
  }
  list
}














