# # Filename: A-League_kit_colours.R

## R code based off Guy Abel's work for Euro squads - https://github.com/guyabel/uefa-ec
## scrape_colours: team kit colours

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries, directories & parameters
library(tidyverse)
library(rvest)

# Directories to read in data files
path = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
input_path = paste(path, "/Input", sep="")
output_path = paste(path, "/R_output", sep="")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
a_league_teams = read_csv("a_league_teams.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
get_kit_colours <- function(u){
  h <- paste0("https://en.wikipedia.org", u) %>%
    read_html()
  
  #kit colour
  tibble(
    shirt = h %>%
      html_nodes(".toccolours td:nth-child(1) div:nth-child(3)") %>%
      html_attr("style") %>%
      str_split(";") %>%
      .[[1]] %>%
      str_subset("background-color") %>%
      str_remove("background-color:") %>%
      str_trim(),
    away = h %>%
      html_nodes(".toccolours td:nth-child(2) div:nth-child(3)") %>%
      html_attr("style") %>%
      str_split(";") %>%
      .[[1]] %>%
      str_subset("background-color") %>%
      str_remove("background-color:") %>%
      str_trim(),
    shorts = h %>%
      html_nodes(".toccolours td:nth-child(1) div:nth-child(7)") %>%
      html_attr("style") %>%
      str_split(";") %>%
      .[[1]] %>%
      str_subset("background-color") %>%
     str_remove("background-color:") %>%
      str_trim(),
    socks = h %>%
      html_nodes(".toccolours td:nth-child(1) div:nth-child(9)") %>%
      html_attr("style") %>%
      str_split(";") %>%
      .[[1]] %>%
      str_subset("background-color") %>%
      str_remove("background-color:") %>%
      str_trim(),
#    third = h %>%
#      html_nodes(".toccolours td:nth-child(3) div:nth-child(3)") %>%
#      html_attr("style") %>%
#      str_split(";") %>%
#      .[[1]] %>%
#      str_subset("background-color") %>%
#      str_remove("background-color:") %>%
#      str_trim()
  )
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
data <- a_league_teams %>%
  select(abbrev, team_url) %>%
  distinct() %>%
  # Failed to map with Wellington Phoenix only having home kit shown
  filter(!(abbrev %in% c("WEL", "NQF")))
dim(data)

data0 <- data %>%
  mutate(kit = map(.x = team_url, .f = ~get_kit_colours(u = .x)))

data_kits <- data0 %>%
  unnest(kit) %>%
  mutate(shirt = ifelse(str_length(shirt) == 7, shirt, paste0(shirt, "0"))) %>%
  rename(kit_shirt = shirt,
         kit_shorts = shorts,
         kit_socks = socks,
         kit_away = away)

data_kits

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format

setwd(output_path)
write.csv(data_kits, file = "A_league_kit_colours.csv")
setwd(input_path)
