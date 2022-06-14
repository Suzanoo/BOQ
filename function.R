makeCard <- function(title, content, size = 12, style = "", depth = 8) {
  div(
    class = glue::glue("card ms-depth-{depth} ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

makePage <- function (contents) {
  contents
}

# https://rstudio.github.io/DT/shiny.html
# https://www.youtube.com/watch?v=ng7uLN1uxV4
render_table <- function(df){
  
  # DT::datatable(df, rownames= FALSE, options = (list(scrollX = TRUE)) %>% head())
  # DT::datatable(df, rownames= FALSE, filter = 'top', options = list(
  #   pageLength = 5, autoWidth = TRUE
  # )) #--> TODO not working now
  # DT::datatable(df, editable = 'cell')
  DT::datatable(df,
                rownames= TRUE,
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel'),
                  pageLength = 5,
                  scrollX = TRUE)
                )

} 

# change date-time format of ex 44592 --> 31/1/22
int_to_date <- function(y){
  list <- NULL
  for(i in c(1:length(y))){
    if(!is.na(stringr::str_match(y[i], "[0-9]{4}"))){
      x <- substr(y[i], start = 1, stop = 5)
      list[i] = as.Date(as.numeric(x), origin = "1899-12-30") %>%
        as.character() #save to as character of date format
    }else{
      list[i] = y[i]
    }
  }
  list
}

# change date-time format of ex 44592...52 --> 31/1/22
convert_intTOdate <- function(y){
  list <- NULL
  for(i in c(1:length(y))){
    if(!is.na(stringr::str_match(y[i], "[0-9]{4}...[0-9]{2}"))){
      x <- substr(y[i], start = 1, stop = 5)
      list[i] = as.Date(as.numeric(x), origin = "1899-12-30") %>%
        as.character() #save to as character of date format
    }else{
      list[i] = y[i]
    }
  }
  list
}

duplicated_check <- function(y){
  list <- NULL
  for (i in c(1:length(y))){
    if(!y[i] %in% list){
      list[i] <- y[i]
    }else{
      list[i] <- paste0(y[i],".y") # add suffix ".y" if duplicated
    }
  }
  list
}

# change date-time format of ex.44592...26 --> 31/1/22
# format_dt <- function(df){
#   name <- names(df) %>%
#     substr(start = 1, stop = 5) %>%
#     convert_intTOdate()
#   names(df) <- name
#   df
# }

# material query
materialQuery <- function(df, query){
  query <- strsplit(query, split = ",| ,|, ")
  
  x <- lapply(query, stringr::str_to_lower) %>% unlist
  y <- lapply(query, stringr::str_to_upper) %>% unlist
  z <- lapply(query, stringr::str_to_title) %>% unlist
  pattern <- c(x, y, z)
  df %>% 
    filter(grepl(paste0(pattern, collapse="|"), DESCRIPTION))
}

# create table based on query input
query_table <- function(df, floors_name){
  df1 <- df %>%
    select(DESCRIPTION, matches(floors_name)) %>%
    tidyr::pivot_longer(-DESCRIPTION) %>%
    tidyr::pivot_wider(names_from = 1, values_fn = sum)
  names(df1)[1] <- "Floor"
  df1
}

# Multi bar plot
multi_barplot <- function(df, floors_name){
  
  # create query table
  df2 <- query_table(df, floors_name)
  
  # transpose table
  df3 <- df2 %>% 
    tidyr::pivot_longer(-Floor) %>% 
    as.data.frame() %>% 
    setNames(c("Floor", "Description", "QTY"))
  
  # plot
  fig <- df3 %>%   
    split(df3$Description) %>% 
    purrr::map(plot_ly, x = ~Floor, y = ~QTY, type = "bar", name = ~Description)
  
  fig <- subplot(fig, nrows = ceiling(nrow(df3)/length(floors_name)))
  fig 
  
}

stackBarPlot <- function(df, floors_name){
  df2 <- query_table(df, floors_name)
  df3 <- df2 %>% 
    tidyr::pivot_longer(-Floor) %>% 
    as.data.frame() %>% 
    setNames(c("Floor", "Description", "QTY"))
  
  fig <- df3 %>% 
    plotly::plot_ly(x = ~Description, y = ~QTY, type = 'bar', 
                    name = ~Floor, color = ~Floor) %>%
    plotly::layout(yaxis = list(title = 'Qty (Log Scale)', type = 'log'),
                   barmode = 'stack')
  
  fig
}

# method to create each progress table
progress_table <- function(df){
  df <- df %>% 
    mutate(Temp = "Temp") %>% 
    group_by(Temp) %>% 
    dplyr::summarise_if(is.numeric, sum) %>% # sum all column
    tidyr::pivot_longer(-Temp) %>% # transpose matrix
    tidyr::pivot_wider(names_from = 1, values_fn = sum) %>% 
    setNames(c("date", "progress")) %>% 
    mutate(csum = cumsum(progress)) # cal accumulate 
  df
}









