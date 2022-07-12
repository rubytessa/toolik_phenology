#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Required Packages & Data
library(tidyverse)
library(DT)
library(shiny)
library(markdown)

tidy_data <- read_csv("pheno_transect_data_tidy.csv") #load dataframe "index_data"

## Useful Objects for Plotting 
species_list <- c("BETNAN", "SALPUL", "LEDPAL", "VACVIT", "CARBIG", "ERIVAG")
species_num <- c(1:6)
names(species_num) <- species_list

transect_list <- tidy_data$Transect %>% unique()
names(transect_list) <- transect_list

flag_list <- tidy_data$Flag %>% unique()
names(flag_list) <- flag_list

## Colors
shades <- RColorBrewer::brewer.pal(6, "Set1")
transect_cols <- c("1" = shades[1], "2" = shades[2], "3" = shades[3], 
                   "4" = shades[4], "5" = shades[5], "6" = shades[6])

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        input_species <- unlist(species_list[as.numeric(input$species)])
        input_transect <- unlist(transect_list[as.numeric(input$transects)])
        input_flag <- unlist(flag_list[as.numeric(input$flags)])
        
        tidy_data %>% 
            filter(Species %in% input_species) %>% 
            filter(Transect %in% input_transect) %>% 
            filter(Flag %in% input_flag) %>% 
            
            ## tidying for plotting 
            filter(!is.na(count)) %>% 
            group_by(ID_Species, Transect, date, Species, ID, Pheno) %>% 
            summarize(total_length = sum(count),
                      avg_length = sum(count)/n())
    })
    
    output$value <- renderPrint({unlist(species_list[as.numeric(input$species)])})

    ### PLOTS ----- 
    output$plot1 <- renderPlot({

        ggplot(selectedData(), aes(x=date, y= total_length, color = factor(Transect), 
                                   group_by = ID_Species)) +
            geom_point() + 
            geom_line (data=selectedData()[!is.na(selectedData()$total_length),]) + 
            facet_grid(Pheno ~ Species) + 
            
            ## format
            scale_color_manual(values = transect_cols) + 
            theme_minimal()
      
    })
    
    ## Bar Plot
    
    selectedBar <- reactive({
        
        input_species <- unlist(species_list[as.numeric(input$species)])
        input_transect <- unlist(transect_list[as.numeric(input$transects)])
        input_flag <- unlist(flag_list[as.numeric(input$flags)])
        
        tidy_data %>% 
            filter(Species %in% input_species) %>% 
            filter(Transect %in% input_transect) %>% 
            filter(Flag %in% input_flag) %>% 
            
            ## tidying for plotting 
            filter(!is.na(count)) %>% 
            group_by(ID_Species, Transect, date, Species, ID, Pheno) %>% 
            summarize(total_length = sum(count),
                      avg_length = sum(count)/n())
        tidy_data %>% 
            filter(Species %in% c("BETNAN", "SALPUL")) %>% 
            filter(Transect == 1)
        
    }) 
    output$bar_plot <- renderPlot({
        
            ggplot(barData(), aes(x=date, y= count, fill = Pheno)) +
                geom_bar(stat = "identity") + 
                facet_grid(ID ~ Species) + 
        
                ## format
                scale_color_manual(values = transect_cols) + 
                theme_minimal()
            
    })
    
    

    # Datatable Output --------------------------------------------------------
    
    output$data_table <- DT::renderDataTable({
        DT::datatable(selectedData(), options = list(orderClasses = TRUE))
    })
    

})
