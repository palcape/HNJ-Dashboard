# Load Packages
library(googlesheets)
library(plotly)
library(shiny)
library(shinythemes)
library(tidyverse)


# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage("SLC HNJ Dashboard",
                           tabPanel("Dashboard",
                                    h3("Dashboard Overview"),
                                      p("Welome to the HNJ Dashboard. This dashboard is designed to allow you to explore the data related to the SLCO-HNJ project. Click 
                                      on the Category Bar at the top of the screen to see different categories of data. Once you've
                                      found a plot you like, you can use its interactive features to explore your data. Double click a series
                                      on the legend to isolate the plot to that one data series.")
                           ),       
                           tabPanel("Program Overview",
                                    h3("Program Overview"),
                                        plotlyOutput("POplot")
                           ),       
                           tabPanel("Client Demographics",        
                                    h3("Client Demographics"),
                                      h4("Gender"),
                                        plotlyOutput("GenderPlot"),
                                      h4("Age"),
                                        plotlyOutput("AgesLinePlot"),
                                      h4("Race/Ethnicity"),
                                        plotlyOutput("RaceLinePlot")
                           ),       
                           tabPanel("Referrals and Enrollments",
                                    h3("Referrals and Enrollments"),
                                      h4("Eligible per HMIS Data Pull"),
                                        plotlyOutput("HMISplot"),
                                      h4("TRH Location Endeavors"),
                                        plotlyOutput("TRHplot"),
                                      h4("Prescreen Eligibility"),
                                        plotlyOutput("PrescreenPlot")
                           ),       
                           tabPanel("Housing Placement and Services",
                                    h3("Housing Placement and Services"),
                                      h4("Roommate Placement Assessments Conducted"),
                                        plotlyOutput("RPACplot"),
                                      h4("New Placements into Housing"),
                                        plotlyOutput("NPiHplot"),
                                      h4("Awaiting Housing"),
                                        plotlyOutput("AHplot")
                           ),       
                           tabPanel("Behavioral Health",
                                    h3("Behavioral Health"),
                                      h4("Referrals"),
                                        plotlyOutput("ReferralsPlot"), 
                                      h4("HNJ Clinician Statistics"),
                                        plotlyOutput("CSplot"),
                                      h4("Enrolled in Additional Community Services"),
                                        plotlyOutput("EiACSplot")
                           ),         
                           tabPanel("Employment",
                                    h3("Employment"),
                                        plotlyOutput("EmploymentPlot")
                           ),         
                           tabPanel("Staffing",
                                    h3("Staffing"),
                                      h4("Staff to Client Ratio"),
                                        plotlyOutput("StCRplot"),
                                      h4("Staffing Positions"),
                                        plotlyOutput("SPplot")
                           ),       
                           tabPanel("Exits",
                                    h3("Exits"),
                                      h4("Clients that Lost Housing"),
                                        plotlyOutput("CtLHplot"),
                                      h4("Unplanned Exits"),
                                        plotlyOutput("UEtMplot"),
                                      h4("Planned Graduations"),
                                        plotlyOutput("PGplot")
                           )
                ),

                HTML('<center><img src="footer.jpg"></center>')
)
# Define Server
server <- function(input, output) {
  ax <- list(
    title = 'Month',
    zeroline = T,
    showline = T,
    zerolinewidth = 1,
    zerolinecolor = toRGB("white")
  )
  
  ## Import Data
  gap <- gs_title("HNJ Service Provider Report_Updated")
  myData <- gap %>%
    gs_read()

  ## Wrangling
  tData <- t(myData) # transposes the data
  tData <- as.data.frame(tData) # transforms the data into a dataframe
  tData <- tData[ ,-c(1, 6, 7, 11, 16, 25, 29, 56, 63, 69, 77, 81, 84, 96)] # removes header columns
  colnames(tData) <- as.character(unlist(tData[2,])) # assigns column names to second row
  tData <- tData[-c(1, 2, 5, 9, 13, 16, 17, 18, 19, 20, 21, 22), ] # removes quarterly total and duplicate rows
  xaxis <- rownames(tData) # assigns row names to a vector we can use in our graph
  months <- factor(xaxis,levels = c("October", "November", "December", "January",  "February", "March",  "April", "May", "June",  "July", "August"))
    ### Creating new Columns for Staffing Tab
    staff <- c(NA, 7, 7, 7, 7, 7, 7, 7, 7, 7)
    tData$Staff <- staff
    clients <- c(NA, 61, 67, 67, 67, 95, 103, 113, 127, NA)
    tData$Clients <- clients
    PositionsAvailable <- c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    tData$PositionsAvailable <- PositionsAvailable
    PositionsFilled <- c(NA, 7, 7, 7, 7, 7, 7, 7, 7, 8)
    tData$PositionsFilled <- PositionsFilled
    
  ## Graphics
    ### Program Overview
      #### Program Overview
      output$POplot <- renderPlotly({POplot <- plot_ly(x = months, y = strtoi(tData[ ,1]), name = 'New Clients', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[ ,2]), name = 'Total Clients', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[ ,3]), name = 'Clients Housed', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[ ,4]), name = 'Clients Permanently Housed', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = ax)
      })
    ### Client Demographics
      #### Gender
      output$GenderPlot <- renderPlotly({GenderPlot <- plot_ly(x = months, y = strtoi(tData[ ,5]), name = 'Male', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[ ,6]), name = 'Female', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Age 
      output$AgesLinePlot <- renderPlotly({AgesLinePlot <- plot_ly(x = months, y = strtoi(tData[ ,9]), name = '26-35', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[ ,10]), name = '36-45', mode = 'lines+markers')%>%
        add_trace(y = strtoi(tData[ ,11]), name = '45+', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Race
      output$RaceLinePlot <- renderPlotly({ RaceLinePlot <- plot_ly(x = months, y = strtoi(tData[ ,12]), name = 'American Indian', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,13]), name = 'Asian', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,14]), name = 'Pacific Islander', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,15]), name = 'Black/African American', mode = 'lines+markers')%>%
        add_trace(y = strtoi(tData[,16]), name = 'White', mode = 'lines+markers')%>%
        add_trace(y = strtoi(tData[,17]), name = 'Other: Two or more races', mode = 'lines+markers')%>%
        add_trace(y = strtoi(tData[,20]), name = 'Hispanic', mode = 'lines+markers')%>%
        add_trace(y = strtoi(tData[,21]), name = 'Other: Non-Hispanic', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
    ### Referrals and Enrollment 
      #### Eligible per HMIS Data Pull
      output$HMISplot <- renderPlotly({HMISplot <- plot_ly(x = months, y = strtoi(tData[,23]), type = 'bar', name = 'Eligible per HMIS Data Pull') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### TRH Location Endeavors
      output$TRHplot <- renderPlotly({TRHplot <- plot_ly(x = months, y = strtoi(tData[ ,24]), name = 'Attempted to Locate', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[ ,25]), name = 'Located and Prescreened', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
      })
      #### Prescreen Eligibility
      output$PrescreenPlot <- renderPlotly({PrescreenPlot <- plot_ly(x = months, y = strtoi(tData[ ,33]), name = 'Eligible after Prescreen', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[ ,34]), name = 'Ineligible after Prescreen', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
      })
    ### Housing Placement and Services
      #### Roommate Placement Assessments Conducted
      output$RPACplot <- renderPlotly({RPACplot <- plot_ly(x = months, y = strtoi(tData[,49]), type = 'bar', name = 'Roommate Placement Assessments Conducted')  %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### New Placements into Housing
      output$NPiHplot <- renderPlotly({NPiHplot <- plot_ly(x = months, y = strtoi(tData[ ,50]), name = 'Total', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[ ,51]), name = 'Single Occupancy Housing', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[ ,52]), name = 'Roommate Housing', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
      })
      #### Awaiting Housing
      output$AHplot <- renderPlotly({AHplot <- plot_ly(x = months, y = strtoi(tData[ ,53]), name = 'Awaiting Housing Placement', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[ ,54]), name = 'Not Housed within Three Months of Enrollment', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
      })
    ### Behavioral Health
      #### Referrals
      output$ReferralsPlot <- renderPlotly({ReferralsPlot <- plot_ly(x = months, y = strtoi(tData[,60]), name = 'Referred to BH Treatment', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,62]), name = 'Referred to BH Treatment after HNJ Assessment ', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### HNJ Clinician Statistics
      output$CSplot <- renderPlotly({CSplot <- plot_ly(x = months, y = strtoi(tData[,61]), name = 'HNJ Health Assessments Conducted', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,63]), name = 'Met with HNJ Clinician', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Enrolled in Additional Community Services
      output$EiACSplot <- renderPlotly({EiACSplot <- plot_ly(x = months, y = strtoi(tData[,64]), name = 'Total', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,65]), name = 'SUD', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,66]), name = 'Behavioral', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
    ### Employment
      #### Employment  
      output$EmploymentPlot <- renderPlotly({EmploymentPlot <- plot_ly(x = months, y = strtoi(tData[,67]), name = 'Employment Assessments Conducted', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,68]), name = 'Enrolled in Employment Services', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,69]), name = 'Participants with Source of Income', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Clients', rangemode = "tozero"), xaxis = list(title = 'Month', ax))
      })
    ### Staffing
      #### Staff to Client Ratio
      output$StCRplot <- renderPlotly({StCRplot <- plot_ly(x = months, y = strtoi(tData[,84]), name = 'Staff', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,85]), name = 'Clients', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month', ax))
      })
      #### Staffing Positions
      output$SPplot <- renderPlotly({SPplot <- plot_ly(x = months, y = strtoi(tData[,86]), name = 'Positions Available', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,87]), name = 'Positions Filled', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month', ax))
      })
    ### Exits
      #### Clients that Lost Housing this Month
      output$CtLHplot <- renderPlotly({CtLHplot <- plot_ly(x = months, y = strtoi(tData[,72]), name = 'Total', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,73]), name = 'Eviction', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,74]), name = 'Other', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Clients that Exitted', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Unplanned Exits this Month
      output$UEtMplot <- renderPlotly({UEtMplot <- plot_ly(x = months, y = strtoi(tData[75]), name = 'Total Unplanned Exits', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(tData[,76]), name = 'Jail/Prison', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,77]), name = 'Self-Termination', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,78]), name = 'Left without Contact', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,79]), name = 'Deceased', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,80]), name = 'Transfer to Another Program', mode = 'lines+markers') %>%
        add_trace(y = strtoi(tData[,81]), name = 'Other', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Clients that Exitted', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Planned Graduations
      output$PGplot <- renderPlotly({PGplot <- plot_ly(x = months, y = strtoi(tData[,82]), type = 'bar', name = 'Planned Graduations') %>%
        layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
}
# Run App
shinyApp(ui, server)