library(shiny)
library(ggplot2)
library(dplyr)

## define the user interface (ui)
ui = fluidPage(titlePanel(title = "USA Census Visualization",
                          windowTitle = "US Census Viz"),
               sidebarLayout(
                 sidebarPanel(
                   helpText("Create demographic maps with info from the 2010 census"),
                   selectInput(inputId = "select_Var",
                               label = "Choose a variable to display",
                               choices = c("Percent White",
                                           "Percent Black",
                                           "Percent Latino",
                                           "Percent Asian"))),
              mainPanel(#textOutput(outputId = "text")
                   plotOutput(outputId = "map"))
               ))
                 
               

server = function(input,output){
  #output$text = renderText(input$select_Var)
  output$map = renderPlot({
    ## define my data as a reactive object
    counties = reactive({
      
      counties = readRDS("counties.rds")
      library(ggplot2)
      counties_map=map_data("county")
      
      counties_map$name=paste(counties_map$region,counties_map$subregion,sep = ",")
      
      library(dplyr)
      
      ## merge two datasets
      
      full_join(counties,counties_map)
    })
    
    title=switch(input$select_Var,
                "Percent White"="% white",
                "Percent Black"="% black",
                "Percent Latino"="% hispanic",
                "Percent Asian"="% asian" 
                )
    
    race=switch(input$select_Var,
                "Percent White"=counties()$white,
                "Percent Black"=counties()$black,
                "Percent Latino"=counties()$hispanic,
                "Percent Asian"=counties()$asian
    )
    
    ## plot the map
    ggplot(counties(),
           aes(x = long,y = lat, group = group,fill = race)) +
      geom_polygon() +
      scale_fill_gradient(low="white",high = "darkred")
  })

}

shinyApp(ui = ui,server = server)





