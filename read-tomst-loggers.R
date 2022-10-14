# Description: read code from TMS-4 files
# Creation: 2021-08-16



## Required Packages
library(tidyverse)

## Functions
read_tms4 <- function(file) {
  
  # Extract serial number from filename
  serial <- file %>% str_split(pattern = "_") %>% last() %>% pluck(3) %>% tail(n=2)
  print(file)
  
  # Read the data file
  data <- read_delim(file, delim = ";",
                     col_names = F, 
                     locale=locale(decimal_mark = ",")) # check this is consistent with delim settings in Lolly
  
  
  # Check file has contents. Empty files due to bad data download only have "File is empty" as text. 
  if (ncol(data) > 1) {
    # Create vector of column names
    vars <- c("Index", "Datetime_UTC", "TimeZone", "T1", "T2", "T3", "SoilMoistureCount", "shake",
              "errFlag", "empty")
    
    # Format data for output
    names(data) <- vars
    
    data_with_ID <- data  %>% 
      mutate(SerialID = serial) %>% 
      select(SerialID, everything()) %>% 
      mutate(Datetime_UTC = lubridate::parse_date_time(Datetime_UTC,orders = c("%Y.%m.%d %H:%M")))
    
    } else {
      print("empty file")
    data_with_ID <- NULL
  }
  

  return(data_with_ID)
}

## Read-in data files

tomst_folder <- "Projects/shrubification_model/"
files <- list.files(path = tomst_folder, pattern = "^data_*", full.names = T)

data <- map_dfr(files, read_tms4)

## Plot data

data_to_plot <- data %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2022-07-16 15:00")) %>% 
  pivot_longer(cols = T1:SoilMoistureCount,
               names_to = "Variable",
               values_to = "Value")

ggplot(data_to_plot, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color=SerialID)) + 
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal()
