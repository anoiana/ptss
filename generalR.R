if(!require(pacman)) install.packages("pacman");pacman::p_load(
 tidyverse, haven, magrittr, furrr,Hmisc, grid, ggplotify, forestplot, knitr, kableExtra, DT, network, sna, reactable, xaringan, bookdown,scriptName
)

##################################
##################################
# functions ----------------------
# label ----
lb = function(...,vn=T){
 def1 =paste0(ensyms(...))
 if(length(def1)==1) def1 = c(stringr::str_extract(def1,"[a-z]+"),def1)

 if(vn){
  a = dplyr::case_when(def1[1] == "def"~ "định nghĩa",
                       def1[1] == "thm" ~ "định lí",
                       def1[1] == "exm" ~ " ví dụ",
                       def1[1] == "exr" ~ " bài tập",
                       def1[1] == "lem" ~ "bổ đề",
                       def1[1] == "eq" ~ "phương trình",
                       def1[1] == "fig" ~ "hình"
  )%>%
   paste0("*",.,"*")
 }
 else{
  a = dplyr::case_when(def1[1] == "def"~ "definition",
                       def1[1] == "thm" ~ "theorem",
                       def1[1] == "exm" ~ " example",
                       def1[1] == "exr" ~ " exercise",
                       def1[1] == "lem" ~ "lemma",
                       def1[1] == "eq" ~ "equation",
                       def1[1] == "fig" ~ "figure"
  )%>%
   paste0("*",.,"*")
 }
 paste0(colorize(a, color = "black")," \\@ref(",def1[1],":",def1[2],")")%>%
  str_remove_all('`')
}
# function ----
colorize <- function(x, color= "#F5B7B1",size = "18"){
 size = paste0(size,"px")
 sprintf("<span style='color: %s; font-size: %s;'>%s</span>", color, size, x)
}
# table ---
draw_table = purrr::partial(reactable::reactable,bordered = TRUE, striped = TRUE, highlight = TRUE, fullWidth = FALSE)
# print from right side
rs = function(x) paste0("<P align=right> ",x," </P>")

newdate_func = function(update){
  if(!file.exists('date_list.rds')){saveRDS(list(), file = 'date_list.rds')}

  filename=knitr::current_input(dir = FALSE)
  mylist <- readRDS(file ='date_list.rds')

  if(update|!(filename%in%names(mylist))){
    mylist[[filename]]<- format(Sys.time(),format = c('%Y-%m-%d') )
    saveRDS(mylist, file = 'date_list.rds')}
  readRDS(file ='date_list.rds')[[filename]]
}

totitle = function(x) paste("&#10001; ",stringr::str_to_title(x))
