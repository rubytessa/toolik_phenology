#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

tidy_data <- read_csv("pheno_transect_data_tidy.csv") #load dataframe "index_data"

## Useful Objects for Plotting 
species_list <- c("BETNAN", "SALPUL", "LEDPAL", "VACVIT", "CARBIG", "ERIVAG")
species_num <- c(1:6)
names(species_num) <- species_list

transect_list <- tidy_data$Transect %>% unique()
names(transect_list) <- transect_list

flag_list <- tidy_data$Flag %>% unique()
names(flag_list) <- flag_list


pageWithSidebar(
    headerPanel('Toolik Phenology Transects'),
    sidebarPanel(
        # Copy the chunk below to make a group of checkboxes
        checkboxGroupInput("species", label = h3("Species Selection"), 
                           choices = species_num,
                           selected = 1),
        checkboxGroupInput("transects", label = h3("Transect Selection"), 
                           choices = transect_list,
                           selected = 1),
        checkboxGroupInput("flags", label = h3("Flag Selection"), 
                           choices = flag_list,
                           selected = 1),
    ),
    mainPanel(
        
        
        # TEST SELECTION
        # hr(),
        # fluidRow(column(3, verbatimTextOutput("value"))),
        
        
        ## Main Plot 
        plotOutput('plot1'),
        
        plotOutput("bar_plot"),
        
        ## Data Table
        hr(),
        DT::dataTableOutput("data_table")
    )
)
