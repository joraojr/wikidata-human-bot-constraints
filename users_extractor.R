# intalling packages
install.packages("rlang")
install.packages("tidyverse")

library(rlang)
library(magrittr)
library(dplyr)
#library(tidyverse)

install.packages("httr")
library(httr)
install.packages("rvest")
library(rvest)



setwd("/home/nicolas/Documentos/phd_courses/data-analysis-r")

file.name <- "constraint-corrections-single.tsv"
data      <- read.delim(file.name, header = FALSE, sep = "\t")
dim(data)

colnames(data)
col.names <- c(
  "constraint.statement",
  "revision.id.url",
  "subject.t0", "predicate.t0", "object.t0",
  "follows.symbol",
  "subject.t1", "predicate.t1", "object.t1", 
  "cud.action",
  "V11", "V12", "V13", "V14")

colnames(data) <- col.names

data_strings <- data %>%
  mutate(
    revision_id = sub(".*/(\\d+)>", "\\1", revision.id.url),
    property_id = sub(".*/(P\\d+)>", "\\1", predicate.t0),
    result_string = paste0(
      "https://www.wikidata.org/w/index.php?title=Property:", property_id,
      "&oldid=", revision_id
    )
  )

#dim(data_strings)[1] == dim(data)[1]
#url = data_strings$result_string[1]
#response <- GET(url)

wikidata.url = "https://www.wikidata.org"

# Function to flush data to the file
flush <- function(count) {
  if (count > 0) {
    cat("Flushing data to the file...\n")
    sink()
    cat("\n")  # Add a newline
  }
}


# Initialize variables for flushing
flush_interval <- 100
counter <- 0
total_counter <- 0
output_file <- "output_data_2.tsv"
write_table_header <- FALSE  # To indicate whether to write the header

# Iterate over data_strings
# nrow(data_strings)
for (i in 1:100) {
  url <- data_strings$result_string[i]
  revision_id <- data_strings$revision_id[i]
  
  # Make an HTTP GET request to the URL
  response <- GET(url)
  
  # Check if the request was successful (HTTP status code 200 indicates success)
  if (response$status_code == 200) {
    # Get the content of the response (HTML content of the webpage)
    page_content <- content(response, as = "text")
    
    # Parse the HTML content into a DOM object
    webpage <- read_html(page_content)
    
    # Select the div element with id "contentSub"
    content_sub_div <- webpage %>% html_node("#contentSub")
    
    # Navigate to the link inside the div
    user_link <- content_sub_div %>% html_node("a")
    
    # Extract the href attribute (which contains the user ID)
    user_id <- html_attr(user_link, "href")
    
    # Construct the full user URL
    full_user_url <- paste0(wikidata.url, user_id)
    
    # Write the data to the TSV file
    data_to_write <- data.frame(Full_User_URL = full_user_url, data_strings[i, ])
    
    if (write_table_header) {
      write.table(data_to_write, file = output_file, sep = "\t", col.names = TRUE, quote = FALSE, row.names = FALSE)
      write_table_header <- FALSE
    } else {
      write.table(data_to_write, file = output_file, sep = "\t", col.names = FALSE, quote = FALSE, row.names = FALSE, append = TRUE)
    }
    
    counter <- counter + 1
    total_counter <- total_counter + 1
    # Check if it's time to flush the data to the file
    if (counter >= flush_interval) {
      flush(counter)
      counter <- 0  # Reset the counter
      
      cat("Number of processed revisions: ", total_counter, "\n")
      r.sec <- rnorm(n = 1, mean = 5, sd = 1)
      Sys.sleep(r.sec)
    }
  } else {
    cat("HTTP request failed for URL", url, "with status code:", response$status_code, "\n")
  }
}

# Flush any remaining data
flush(counter)


      
