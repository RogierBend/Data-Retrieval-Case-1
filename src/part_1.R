library(tidyverse)

#' Retrieve members of parliament
#'
#' Side effect: the members info is saved in the clean_data folder
#' 
#' @return A tibble with the columns as specified
retrieve_members <- function(){
  url <- "https://www.tweedekamer.nl/kamerleden_en_commissies/alle_kamerleden"
  member_link <- httr::GET(url) %>% httr::content() 
  members <- member_link %>% rvest::html_nodes(".member__name")
  member_names <- members %>% rvest::html_text()

  party <- member_link %>%  rvest::html_nodes(".member__tag")
  member_party <- party %>% rvest::html_text()

  link <- member_link %>% rvest::html_nodes((".member__name"))
  member_url <- link %>% rvest::html_attr("href")
  
  info <- member_link %>%  rvest::html_nodes("td")
  member_info <- info %>% rvest::html_text()
 

  member_df <- data.frame(Naam = member_names, 
                          partij = member_party, 
                          url = member_url)
  
  saveRDS(members_df, file = "clean_data/members.Rds")
  return(members_df)
}

#' Retrieve activity of a single member of parliament
#'
#' @param member_url The url to the personal page of a member
#'
#' @return A tibble with the columns as specified
retrieve_member_activity <- function(member_url){
  url <- str_c("https://www.tweedekamer.nl", member_url) 
  
  page <- httr::GET(url) %>% httr::content()
  full_list_links <- page %>% rvest::html_nodes(".read-more") %>% rvest::html_attr("href") %>% str_replace_all("dpp=15","dpp=1000")
  
  #
  # Maak de functie af, sla je data op in activity zodat de regels hieronder goed werken
  #
  
  #Loop alle links voor 1 persoon
  # vragen nummer mag leeg blijven
  
  
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
  
