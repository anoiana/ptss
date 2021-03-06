if(!require(pacman)) install.packages("pacman");pacman::p_load(
 tidyverse, magrittr, Hmisc, reactable, cvms
)

##################################
##################################
# functions ----------------------
colorize <- function(x, color= "#F5B7B1",size = "18"){
  size = paste0(size,"px")
  sprintf("<span style='color: %s; font-size: %s;'>%s</span>", color, size, x)
}
# label ----
lb = function(...){
 def1 =paste0(rlang::ensyms(...))
 if(length(def1)==1) def1 = c(stringr::str_extract(def1,"[a-z]+"),def1)

  a = dplyr::case_when(def1[1] == "def"~ "Def.",
                       def1[1] == "thm" ~ "Theorem",
                       def1[1] == "exm" ~ "Ex.",
                       def1[1] == "exr" ~ "Exercise",
                       def1[1] == "lem" ~ "Lemma",
                       def1[1] == "eq" ~ "Eq.",
                       def1[1] == "fig" ~ "Fig.",
                       def1[1] == "tab"~"Table"
  )%>% paste0("*",.,"*")

 paste0(colorize(a, color = "#A569BD")," \\@ref(",def1[1],":",def1[2],")")%>%
  str_remove_all('`')
}

# table ---
draw_table = purrr::partial(reactable::reactable,bordered = TRUE, striped = TRUE,
                            highlight = TRUE, fullWidth = FALSE, resizable = TRUE)

# print from right side
rs = function(x) paste0("<P align=right> ",x," </P>")

newdate_func = function(update){
 if(!file.exists('date_list.rds')){saveRDS(list(), file = 'date_list.rds')}

 filename=knitr::current_input(dir = FALSE)
 mylist <- readRDS(file ='date_list.rds')

 if(update|!(filename%in%names(mylist))){
  mylist[[filename]]<- format(Sys.time(),format = c('%Y-%m-%d') )%>% {paste("&#x1F4C5;",.)}
  saveRDS(mylist, file = 'date_list.rds')}
 readRDS(file ='date_list.rds')[[filename]]
}
##############################
totitle = function(x, icon = "&#10001; "){
  b = str_split(x," +")[[1]]
  word = c("and","as","but","of","from")
  word = stringr::str_c("(^",word,"$)")%>% stringr::str_c(collapse = "|")%>%
    stringr::str_c(.,"|([A-Z]+)")
  check= str_detect(b, word)
  ifelse(check,b,str_to_title(b))%>%
    paste(collapse = " ")%>%
    paste(icon,.)
}


#############################


my_text = function(x, font = "Arial", size = 19){
  sprintf("<p style='font-family:%s; font-size:%s px;'>%s</p>", font, size, x)
}

################## square sign for complete of proof ####################
proved = function(x = "$\\blacksquare$", size = 18){
  sprintf("<p style='float:right; font-size:%spx'>%s</p></p>$\\quad$", size,x)
}


############### collapse button ###############################

collapsable_button = function(name = "button name"){
  list(
    initialize =
      sprintf('<button data-toggle="collapse" data-target="#demo">%s</button>',name),
    begin = '<div id="demo" class="collapse">',
    end = '</div>'
  )
}
