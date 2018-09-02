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


year_mapping <- list(
  list("2005", c(2005:2009)),
  list("2002", c(2002:2005))
)

scrape_polls_institute <- function(institute, year_batch = "current", include_election = TRUE, gather = TRUE) {
  
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
  
  
  # remove irrelevant rows
  #df <- df[-c(1:3), ] # TODO check if we can always remove the first 3 rows ---> doesn't work for 2005!
  
  tbl <- df %>%
    filter(!is.na(.[3])) %>% # remove irrelevant rows in the header
    mutate(institute = institute,
           publication_date = lubridate::dmy(publication_date),
           Befragte = as.numeric(str_replace(str_replace(Befragte, fixed("."), ""), "[^0-9]+", "")),
           polling_start = str_split_fixed(Zeitraum, "–", n = 2)[, 1],
           polling_end = str_split_fixed(Zeitraum, "–", n = 2)[, 2],
           btw = str_trim(Zeitraum) == "Bundestagswahl"
           ) %>%
    as_tibble()
  
  # remove election results if specified in function arguments
  if (!include_election) {
    tbl <- tbl %>%
      filter(!btw)
  }
  
  # gather data if argument gather = TRUE
  if (gather) {
    tbl <- tbl %>%
      gather(key = party, value = share, -c("publication_date", "Befragte", "Zeitraum", "polling_start", "polling_end", "btw", "institute")) %>%
      select(publication_date, institute, party, share, everything()) %>%
      mutate(party = factor(party, levels = c("CDU.CSU", "SPD", "GRÜNE", "FDP", "LINKE", "Linke.PDS", "PDS", "AfD", "PIRATEN", "NPD", "REP", "Sonstige"))) %>%
      arrange(publication_date, party)
  }
  
  tbl
}
   
polls_tbl_2005 <- scrape_polls_institute("Allensbach", "2005")

# polls_tbl_2005 <- scrape_polls_institute("Allensbach", "2005")
# polls_2005_long <- polls_tbl_2005 %>%
#   gather(key = party, value = share, -c("publication_date", "Befragte", "Zeitraum", "polling_start", "polling_end", "btw", "institute")) %>%
#   select(publication_date, institute, party, share, everything()) %>%
#   mutate(party = factor(party, levels = c("CDU.CSU", "SPD", "GRÜNE", "FDP", "LINKE", "Linke.PDS", "PDS", "AfD", "PIRATEN", "NPD", "REP", "Sonstige"))) %>%
#   arrange(publication_date, party)
# 
# polls_tbl_current <- scrape_polls_institute("Allensbach")
# polls_current_long <-  polls_tbl_current %>%
#   gather(key = party, value = share, -c("publication_date", "Befragte", "Zeitraum", "polling_start", "polling_end", "btw", "institute")) %>%
#   select(publication_date, institute, party, share, everything()) %>%
#   mutate(party = factor(party, levels = c("CDU.CSU", "SPD", "GRÜNE", "FDP", "LINKE", "Linke.PDS", "PDS", "AfD", "PIRATEN", "NPD", "REP", "Sonstige"))) %>%
#   arrange(publication_date, party)
# 
# polls_long_combined <- rbind(polls_current_long, polls_2005_long)
# 
# polls_wide <- polls_long_combined %>%
#   spread(party, share)


dimap_current <- scrape_polls_institute("dimap")

emnid_2001 <- scrape_polls_institute("emnid", 2001)

ggplot(dimap_current_long, aes(publication_date, share, col = party)) +
  geom_line()
