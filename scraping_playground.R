library(tidyverse)
library(rvest)

# retrieve page content - takes either a URL string or a vector of strings which constitute a url (will be collapsed to one string using "/")
get_content <- function(url) {
  if (is.vector(url)) {
    url <- str_c(url, collapse = "/")
  }
  url %>%
    read_html()
}


institute <- "Emnid"
year_batch = "2001"

btw_url <- str_c("https://www.wahlrecht.de/umfragen/",
                 str_to_lower(institute),
                 ifelse(year_batch != "current", str_c("/", year_batch), ""),
                 ".htm"
)

# get page content and extract polls table from html
page_content <- get_content(btw_url)
tab_element <- html_node(page_content, xpath = "//table[@class='wilko']")
tab <- html_table(tab_element, fill = TRUE)

# convert the percentage strings into double
party_cols <- 3:(length(tab) - 3) # TODO check if this is general for all tables
party_df <- tab[party_cols] %>%
  map(function(x) as.numeric(str_replace(str_replace(x, ",", "."), " %",""))) %>%
  as.data.frame()

# combine all relevant columns to a single df
info_cols <- (length(tab) - 1) : length(tab)
df <- cbind(tab[1], party_df, tab[info_cols])
colnames(df)[1] <- "publication_date"


# Befragte and Zeitraum not available for all years and institutes, therefore check and add if necessary
( vars_collapsed <- str_c(colnames(df), collapse = "; ") )
if(str_detect(vars_collapsed, "(Befragte|Zeitraum)")) {
  message("table contains Befragte and Zeitraum")
}