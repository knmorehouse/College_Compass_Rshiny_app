library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(readr)

college_dataset_use <- read_csv("college_dataset_use_5.csv")

ui <- navbarPage("College Compass", id="nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 360, height = "auto",
                                            
                                            h2("Input your preferences"),
                                            
                                          
                                            
                                            selectInput(inputId = "degree", "Type of degree", college_dataset_use$`Degree Type`, selected = "Bachelor"),
                                            selectInput(inputId = "size", "Size of School", college_dataset_use$`Size of School`, selected = "Mid-Sized"),
                                            numericInput(inputId = "mathSAT", "Your Math SAT score", 600, min = 200, max = 800),
                                            numericInput(inputId = "readSAT", "Your Reading SAT score", 600, min = 200, max = 800),
                                            
                                            
                                            
                                            
                                            
                                            checkboxGroupInput(inputId = "region", "Region", choices = list("Northeast" = "Northeast",
                                                                                                            "Southeast" = "Southeast", 
                                                                                                            "Southwest" ="Southwest",
                                                                                                            "Midwest" = "Midwest", 
                                                                                                            "West" = "West", 
                                                                                                            "Great Lakes" = "Great Lakes"), 
                                                               selected = c("Northeast", "Southeast", "Southwest", "Midwest", "West", "Great Lakes")),
                                            checkboxGroupInput(inputId = "area", "Sourrounding area",  choices = list("Urban"= "Urban",
                                                                                                                      "Suburban"= "Suburban",
                                                                                                                      "Rural"= "Rural"), 
                                                               selected = c("Urban", "Suburban", "Rural"), inline = TRUE),
                                            h5("Your Number of Matches:", textOutput("txt") )
                                            
                              ),
                              
                              
                              
                              tags$div(id="cite",
                                       'Data scraped from the', tags$em('College Scorecard'), ' database by the Department of Education.'
                              )
                          )
                 ),
                 
                 
                 tabPanel("Explore Matches",
                          
                          hr(),
                          DT::dataTableOutput(outputId = "match_table") #change this to admistable
                 ),
                 tabPanel("About College Compass",
                          hr(),
                          tags$h1("Welcome to College Compass"),
                          tags$h3(tags$i("Connecting students to higher education one click at a time")),
                          tags$br(),
                          tags$h3(tags$strong("Our Mission:")),
                          tags$h4("With thousands of colleges to choose from, navigating the college process can be extrodinarily difficult. Luckily, College Compass is here to help. We've compiled data from over 4,000 colleges from the Department of Education's", 
                                  tags$i(tags$a(href="https://collegescorecard.ed.gov/data/", "Rscorecard")), 
                                  "to help match you with colleges that best fit your needs."), 
                          
                          tags$h3(tags$strong("Limitations of our app")),
                          tags$h4(tags$ul(
                            tags$li(tags$strong("Missing values:"), "Some schools fail to report all data. For example, almost 3,000 schools failed to report their SAT data. This dramatically decreases the number of 'matches.' For more a more complete documentation of 
                                    SAT scores, see", tags$i(tags$a(href="https://www.usnews.com/best-colleges", "US News' College Rankings."))), 
                            tags$li(tags$strong("International Opportunities:"), "While College Compass is geared towards students looking at colleges in the US, we recognize some students might be interested in schools overseas.", tags$i(tags$a(href="https://www.usnews.com/education/best-global-universities", "Click here")), "for information about the top universities in the world.") 
                            
                            )),
                          tags$h3(tags$strong("More information:")),
                          tags$h4(tags$ul(
                            tags$li(tags$a(href="https://studentaid.ed.gov/sa/types/grants-scholarships/finding-scholarships", 
                                           "Assistance applying for federal student aid")),
                            tags$li(tags$a(href="https://www.bestcollegereviews.org/best-study-abroad-programs/",
                                           "Study abroad programs")),
                            tags$li(tags$a(href="https://www.youtube.com/watch?v=vtOPju9_S8A",
                                           "Advice about choosing a school"))
                          ))
                 ))



# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

server <- function(input, output, session) {
  
  ## Interactive Map ###########################################
  output$txt <- renderText({ 
    dim(filter(college_reactive(), match == "Yes"))[1]
  })
  
  content <- paste("<h4>", college_dataset_use$Name, "</h4>",
                   "<strong>", college_dataset_use$City, "," , college_dataset_use$State, "</strong>", "</br>",
                   "<b> Admissions Rate </b>", college_dataset_use$`Admission Rate`, "</br>",
                   "<b> Tuition: </b>", college_dataset_use$Tuition, "</br>", 
                   "<b> Enrollment: </b>", college_dataset_use$Enrollment, "</br>",
                   "<b> Average Debt: </b>", college_dataset_use$`Average Debt`)

  
  output$map <- renderLeaflet({
    factpal <- colorFactor(c( "lightgoldenrod", "coral2", "blue2"), c("Info N/A", "No", "Yes"))
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>% 
      
      addCircles(data = college_reactive(),
                 lng = ~longitude, 
                 lat = ~latitude,
                 weight = 3,
                 radius=6000,  
                 stroke = TRUE, fillOpacity = 0.2,
                 color = ~factpal(match), 
                 opacity = 1, 
                 popup = content) %>%
      addLegend("topleft", pal = factpal, values = college_reactive()$match,
                title = "College Match?",
                opacity = 1)
    
    
  })
  
  
  
  
  output$match_table <- DT::renderDataTable({
    req(input$degree)
    college_table <- filter(college_dataset_use, 
                            `Degree Type` == input$degree, 
                            `Size of School` == input$size,
                            satmtmid >= input$mathSAT -50,
                            satvrmid >= input$readSAT -50, 
                            `Sourrounding area` %in% input$area, 
                            Region %in% input$region) %>%
      select(Name, City, State, `Admission Rate`, Tuition, Enrollment, URL) %>% 
      arrange(`Admission Rate`)
    DT::datatable(data = college_table, rownames = FALSE, escape = FALSE)
  })
  
  
  college_reactive <- reactive({
    college_dataset_use %>%
      mutate(
        match = if_else( `Degree Type` == input$degree &
                           `Size of School` == input$size &
                           satmtmid >= input$mathSAT &
                           satvrmid >= input$readSAT &
                           `Sourrounding area` %in% input$area &
                           Region %in% input$region, "Yes", "No", missing = "Info N/A")
      )
    
  })  
 
}

shinyApp(ui=ui, server=server)
