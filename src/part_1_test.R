#' Test function to test your work
#'
#' @return Nothing, it only outputs information to the console
tester <- function(){
  library(dplyr)
  tryCatch({
    # Check main project setup
    files <- list.files(all.files = TRUE)
    test({any(stringr::str_detect(files, "[.]Rproj$"))}, "This is an RStudio project.", files)
    test({"raw_data" %in% files}, "Contains raw_data folder.", files)
    test({"clean_data" %in% files}, "Contains clean_data folder.", files)
    test({"src" %in% files}, "Contains src folder.", files)
    test({"docs" %in% files}, "Contains docs folder.", files)
    test({"test_data" %in% files}, "Contains test_data folder.", files)
    test({!any(stringr::str_detect(files, "[.](R|r)$"))}, "Contains no .R files in main project folder.", files)
    test({".git" %in% files}, "Uses Git repository.", optional = TRUE, files)
    
    # Check existence of code files in the right place
    files <- list.files("src")
    test({"part_1.R" %in% files}, "Contains part_1.R in the src/ folder. ", 
         more_info = "Download this file from the assignment page.", files)
    test({"part_1_test.R" %in% files}, "Contains part_1_test.R in the src/ folder. ", 
         more_info = "Download this file from the assignment page.", files)
    test({"part_1.Rmd" %in% files}, "Contains part_1.Rmd in the src/ folder. ", 
         more_info = "The code of your report should go in the src/ folder", files)
    
    # Check existence of report output files in the right place
    files <- list.files("docs")
    test({"part_1.html" %in% files}, "Contains part_1.html in the docs/ folder. ", files)
    
    # Check existence of check_data files in the right place
    files <- list.files("test_data")
    test({"check_members.Rds" %in% files}, "Contains check_members.Rds in the test_data/ folder. ", 
         more_info = "Download this file from the assignment page.", files)
    test({"check_member_activity.Rds" %in% files}, "Contains check_member_activity.Rds in the test_data/ folder. ", 
         more_info = "Download this file from the assignment page.", files)
       
  }, error = function(e){
    test({FALSE}, "Did you change the working directory?")
    return()
  })
  
  tryCatch({
    # Source code file and check function
    source("src/part_1.R", local = TRUE)
    check_members_df <- readRDS("test_data/check_members.Rds")
    members_df <- retrieve_members()
    # Test column names
    test({all(suppressWarnings({names(members_df) == names(check_members_df)}))}, 
         stringr::str_c("Columns of output of retrieve_members() should be {",stringr::str_c(names(check_members_df), collapse = ", "),"}"), 
         more_info = stringr::str_c("but instead it is {",stringr::str_c(names(members_df), collapse = ", "),"}"), 
         members_df, check_members_df)
    
    # Test if all members are present
    test({setequal(members_df$name, check_members_df$name)}, 
         stringr::str_c("All 150 members are present"), 
         more_info = stringr::str_c("The following persons are present in your set but should not be: {", stringr::str_c(setdiff(members_df$name, check_members_df$name), collapse = ", "), "} while the following members are missing from your set: {", stringr::str_c(setdiff(check_members_df$name, members_df$name), collapse = ", "),"}"), 
         members_df, check_members_df)
    
    # Test if the two dataframes are exactly the same
    exactly_same <- all_equal(members_df, check_members_df)
    test(exactly_same, "The retrieve_members output is exactly right. ", more_info = exactly_same, members_df, check_members_df)
    
    # Test activity, use member Ziengs as example
    check_activity <- readRDS("test_data/check_member_activity.Rds")
    check_activity_summary <- check_activity %>% count(type)
    activity <- members_df %>% filter(str_detect(name, "Ziengs")) %>% .$url %>% 
      retrieve_member_activity() %>% filter(lubridate::year(date) <= 2019)
    activity_summary <- activity %>% count(type)
    
    # Test column names
    test({all(suppressWarnings({names(activity) == names(check_activity)}))}, 
         stringr::str_c("Columns of output of retrieve_member_activity() should be {",stringr::str_c(names(check_activity), collapse = ", "),"}"), 
         more_info = stringr::str_c("but instead it is {",stringr::str_c(names(activity), collapse = ", "),"}"), 
         activity, check_activity)
    
    # Test if every activity is present
    test({setequal(activity$url, check_activity$url)}, 
         stringr::str_c("All activities are scraped"), 
         more_info = stringr::str_c("The following activities are present in your set but should not be: {", stringr::str_c(setdiff(activity$url, check_activity$url), collapse = ", "), "} while the following members are missing from your set: {", stringr::str_c(setdiff(check_activity$url, activity$url), collapse = ", "),"}"), 
         activity, check_activity)
    
    # Test activity summary
    exactly_same <- all_equal(activity_summary, check_activity_summary)
    test(exactly_same, "The number of activities per type is correct. ", more_info = exactly_same, activity_summary, check_activity_summary)
    
    # Test activity
    exactly_same <- all_equal(activity, check_activity)
    test(exactly_same, "The output of retrieve_member_activity is correct. ", more_info = exactly_same, activity, check_activity)
    
  }, error = function(e){
    test({FALSE}, "Is the part_1.R present in the src/ folder of your project?")
    return()
  })
}

#' This is an internal function, used for testing
#'
#' @param testexpr A piece of code to test something
#' @param message The message to the user
#' @param ... Some data needed to run the test
#' @param optional Whether this part of the assignment is optional
#' @param more_info More information to show when the test fails (to help you figure out what's going on)
#'
#' @return Nothing, this function just prints something to the console
test <- function(testexpr, message, ..., optional = FALSE, more_info = ""){
  if (isTRUE(eval(testexpr))){
    if (optional){
      print(stringr::str_c("OK: ",message, " (optional, well done!)"))
    } else {
      print(stringr::str_c("OK: ",message))
    }
  } else {
    if (optional){
      print(stringr::str_c("Failed: ",message, more_info, " (optional)"))
      
    } else {
      message(stringr::str_c("     Failed: ",message, more_info))
    }
  }
}
  