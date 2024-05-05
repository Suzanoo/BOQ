# read plan sheet
# file_path <- "../BOQ.xlsx"
file_path <- (paste0(getwd(), "/BOQ.xlsx"))
df1 <- readxl::read_excel(path = file_path, sheet = 2, na = "") %>%
  filter(QTY != 0) %>%
  filter(AMOUNT != 0) %>% 
  replace(is.na(.), 0) %>%
  mutate(across(is.numeric, round, digits = 4)) %>% 
  mutate(weight = AMOUNT/sum(AMOUNT)) %>% 
  relocate(weight, .after = AMOUNT)

names(df1) <- int_to_date(names(df1))
index1 <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df1))
plan <- df1[index1]
weight <- df1 %>% select(matches("weight"))

plan <- plan %>%
  mutate_all(.,function(col){weight$weight*col/100})

plan <-  progress_table(plan)

#-----------------------
df2 <- readxl::read_excel(path = file_path, sheet = 3, na = "") %>%
  filter(QTY != 0) %>%
  filter(AMOUNT != 0) %>% 
  replace(is.na(.), 0) %>%
  mutate(across(is.numeric, round, digits = 4)) %>% 
  mutate(weight = AMOUNT/sum(AMOUNT)) %>% 
  relocate(weight, .after = AMOUNT)

names(df2) <- int_to_date(names(df2))
index2 <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df2))
actual <- df2[index2]
weight <- df2 %>% select(matches("weight"))

actual <- actual %>%
  mutate_all(.,function(col){weight$weight*col/100})

actual <-  progress_table(actual)

col <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df1))
time <- length(col)

x <- actual %>% 
  select(progress) %>% 
  filter(progress > 0) %>% 
  pull() %>% 
  length()

this <- actual$progress[x]
acc.this <- actual$csum[x]
acc.previous <- actual$csum[x-1]
status <- round((acc.this - plan$csum[x])*time*30/100, digits = 2) #convert month to days





