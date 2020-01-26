library(tidyverse)

source("src/part_1_test.R")

 n #' Retrieve members of parliament
#'
#' Side effect: the members info is saved in the clean_data folder
#'
#' @return A tibble with the columns as specified
retrieve_members <- function(){
  url <- "https://www.tweedekamer.nl/kamerleden_en_commissies/alle_kamerleden"
  member_link <- httr::GET(url) %>% httr::content() 
  members <- member_link %>% rvest::html_nodes(".member__name")
  member_names <- members %>% rvest::html_text()
  member_names <- member_names[-151]

  party <- member_link %>%  rvest::html_nodes(".member__tag")
  member_party <- party %>% rvest::html_text()
  member_party <- member_party[-151]

  link <- member_link %>% rvest::html_nodes((".member__name"))
  member_url <- link %>% rvest::html_attr("href")
  member_url <- member_url[-151]

  info <- member_link %>%  rvest::html_nodes("td")
  member_info <- info %>% rvest::html_text()
  
  member_city <- vector(mode = "logical", length = 0)
  member_age<- vector(mode = "logical", length = 0)
  member_days_active <- vector(mode = "logical", length = 0)
  
  i <- 1
  x <- 1
  for (i in (1:max(row_number(member_info))))  {
    if (member_info[i] == "Woonplaats") {
      member_city[x] <- member_info[i+1]
      i = i + 2
      x = x + 1
    } else if (member_info[i] == "Leeftijd") {
      member_age[x] <- readr::parse_number(member_info[i+1])
      i = i + 2
      x = x + 1
    } else if (member_info[i] == "Anciënniteit") {
      member_days_active[x] <- readr::parse_number(member_info[i+1])
      i = i + 2
      x = x + 1
    } else {
      i= i + 1
    }
  }  
  
  member_age <- member_age[!is.na(member_age)]
  member_days_active <- member_days_active[!is.na(member_days_active)]
  member_city <- member_city[!is.na(member_city)]
  member_city <- member_city[-151]

  
  
  members_df <- data.frame(name  = member_names, 
                           party = member_party, 
                           url = member_url,
                           age = member_age,
                           city = member_city,
                           days_active = member_days_active)
  
  
  saveRDS(members_df, file = "clean_data/members.Rds")
  return(members_df)
}

#' Retrieve activity of a single member of parliament
#'
#' @param member_url The url to the personal page of a memberretr
#'
#' @return A tibble with the columns as specified

#member_url <- "/kamerleden_en_commissies/alle_kamerleden/esch-em-van-pvdd"

retrieve_member_activity <- function(member_url){

url <- str_c("https://www.tweedekamer.nl", member_url) 
 
page <- httr::GET(url) %>% httr::content()
full_list_links <- page %>% rvest::html_nodes(".read-more") %>% rvest::html_attr("href") %>% str_replace_all("dpp=15","dpp=1000")
member_data_question <- data.frame()
member_data_amendementen <- data.frame() 
member_data_moties <- data.frame()
 
  
if (grepl("vragen", full_list_links[1])){
    url_questions <-  str_c("https://www.tweedekamer.nl", full_list_links[1])
    page_questions <- httr::GET(url_questions) %>% httr::content()
    
    question_title <- page_questions %>% rvest::html_nodes("h3") %>%  rvest::html_text()
    question_url <- page_questions %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
    question_id <- page_questions %>% rvest::html_nodes(".id") %>%  rvest::html_text()
    question_date <- page_questions %>% rvest::html_nodes(".date") %>%  rvest::html_text()
    
    member_data_question <- tibble(
      member_url = member_url[1],
      type = "Schriftelijke Vragen",
      title = question_title,
      url = question_url,
      id = question_id,
      date = question_date
  )
} else if (grepl("Moties", full_list_links[1])){
      url_moties <- str_c("https://www.tweedekamer.nl", full_list_links[1])
      page_moties <- httr::GET(url_moties) %>% httr::content()
      
      moties_title <- page_moties %>% rvest::html_nodes("h3") %>%  rvest::html_text()
      moties_url <- page_moties %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
      moties_id <- page_moties %>% rvest::html_nodes(".id") %>%  rvest::html_text()
      moties_date <- page_moties %>% rvest::html_nodes(".date") %>%  rvest::html_text()
      
      member_data_moties <- tibble(
        member_url = member_url,
        type = "Moties",
        title = moties_title,
        url = moties_url,
        id = moties_id,
        date = moties_date
      )  
} else if (grepl("Amendement", full_list_links[1])){
    url_amendementen <- str_c("https://www.tweedekamer.nl", full_list_links[1])
    page_amendementen <- httr::GET(url_amendementen) %>% httr::content()
    
    amendementen_title <- page_amendementen %>% rvest::html_nodes("h3") %>%  rvest::html_text()
    amendementen_url <- page_amendementen %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
    amendementen_id <- page_amendementen %>% rvest::html_nodes(".id") %>%  rvest::html_text()
    amendementen_date <- page_amendementen %>% rvest::html_nodes(".date") %>%  rvest::html_text()
    
    member_data_amendementen <- tibble(
      member_url = member_url,
      type = "Amendementen",
      title = amendementen_title,
      url = amendementen_url,
      id = amendementen_id,
      date = amendementen_date
    ) 
}
    
if (grepl("vragen", full_list_links[2])){
    url_questions <-  str_c("https://www.tweedekamer.nl", full_list_links[2])
    page_questions <- httr::GET(url_questions) %>% httr::content()
    
    question_title <- page_questions %>% rvest::html_nodes("h3") %>%  rvest::html_text()
    question_url <- page_questions %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
    question_id <- page_questions %>% rvest::html_nodes(".id") %>%  rvest::html_text()
    question_date <- page_questions %>% rvest::html_nodes(".date") %>%  rvest::html_text()
    
    member_data_question <- tibble(
      member_url = member_url[1],
      type = "Schriftelijke Vragen",
      title = question_title,
      url = question_url,
      id = question_id,
      date = question_date
    )
} else if (grepl("Moties", full_list_links[2])){
    url_moties <- str_c("https://www.tweedekamer.nl", full_list_links[2])
    page_moties <- httr::GET(url_moties) %>% httr::content()
    
    moties_title <- page_moties %>% rvest::html_nodes("h3") %>%  rvest::html_text()
    moties_url <- page_moties %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
    moties_id <- page_moties %>% rvest::html_nodes(".id") %>%  rvest::html_text()
    moties_date <- page_moties %>% rvest::html_nodes(".date") %>%  rvest::html_text()
    
    member_data_moties <- tibble(
      member_url = member_url,
      type = "Moties",
      title = moties_title,
      url = moties_url,
      id = moties_id,
      date = moties_date
    )  
} else if (grepl("Amendement", full_list_links[2])){
    url_amendementen <- str_c("https://www.tweedekamer.nl", full_list_links[2])
    page_amendementen <- httr::GET(url_amendementen) %>% httr::content()
    
    amendementen_title <- page_amendementen %>% rvest::html_nodes("h3") %>%  rvest::html_text()
    amendementen_url <- page_amendementen %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
    amendementen_id <- page_amendementen %>% rvest::html_nodes(".id") %>%  rvest::html_text()
    amendementen_date <- page_amendementen %>% rvest::html_nodes(".date") %>%  rvest::html_text()
    
    member_data_amendementen <- tibble(
      member_url = member_url,
      type = "Amendementen",
      title = amendementen_title,
      url = amendementen_url,
      id = amendementen_id,
      date = amendementen_date
    )   
}   
   
if (grepl("vragen", full_list_links[3])){
  url_questions <-  str_c("https://www.tweedekamer.nl", full_list_links[3])
  page_questions <- httr::GET(url_questions) %>% httr::content()
  
  question_title <- page_questions %>% rvest::html_nodes("h3") %>%  rvest::html_text()
  question_url <- page_questions %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
  question_id <- page_questions %>% rvest::html_nodes(".id") %>%  rvest::html_text()
  question_date <- page_questions %>% rvest::html_nodes(".date") %>%  rvest::html_text()
  
  member_data_question <- tibble(
    member_url = member_url[1],
    type = "Schriftelijke Vragen",
    title = question_title,
    url = question_url,
    id = question_id,
    date = question_date
  )
} else if (grepl("Moties", full_list_links[3])){
  url_moties <- str_c("https://www.tweedekamer.nl", full_list_links[3])
  page_moties <- httr::GET(url_moties) %>% httr::content()
  
  moties_title <- page_moties %>% rvest::html_nodes("h3") %>%  rvest::html_text()
  moties_url <- page_moties %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
  moties_id <- page_moties %>% rvest::html_nodes(".id") %>%  rvest::html_text()
  moties_date <- page_moties %>% rvest::html_nodes(".date") %>%  rvest::html_text()
  
  member_data_moties <- tibble(
    member_url = member_url,
    type = "Moties",
    title = moties_title,
    url = moties_url,
    id = moties_id,
    date = moties_date
  )  
} else if (grepl("Amendement", full_list_links[3])){
  url_amendementen <- str_c("https://www.tweedekamer.nl", full_list_links[3])
  page_amendementen <- httr::GET(url_amendementen) %>% httr::content()
  
  amendementen_title <- page_amendementen %>% rvest::html_nodes("h3") %>%  rvest::html_text()
  amendementen_url <- page_amendementen %>% rvest::html_nodes("h3") %>% rvest::html_attr("href") #werkt nog niet
  amendementen_id <- page_amendementen %>% rvest::html_nodes(".id") %>%  rvest::html_text()
  amendementen_date <- page_amendementen %>% rvest::html_nodes(".date") %>%  rvest::html_text()
  
  member_data_amendementen <- tibble(
    member_url = member_url,
    type = "Amendementen",
    title = amendementen_title,
    url = amendementen_url,
    id = amendementen_id,
    date = amendementen_date
  )   
}  

  activity <- rbind(member_data_question, member_data_amendementen, member_data_moties)
  activity <- data.frame(activity)

  
  #
  # Maak de functie af, sla je data op in activity zodat de regels hieronder goed werken
  #

  return(activity)
}
  
#' Retrieve and save activity for the given members
#'
#' Side effect: the activity is saved in the clean_data folder
#'
#' @param members_df The tibble of members from retrieve_members() function
#'
#' @return A tibble with all activity for all the given members
save_all_activity <- function(members_df){
  member_activity <- purrr::map_df(members_df$url, retrieve_member_activity)
  saveRDS(member_activity, file =  "clean_data/member_activity.Rds")
}
  
