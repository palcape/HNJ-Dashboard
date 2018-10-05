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
                                      on the legend to isolate the plot to that one data series!")
                           ),
                           tabPanel("Program Overview",
                                    h3("Program Overview"),
                                      plotlyOutput("ProgramOverviewPlot"),
                                    h3("Client Demographics"),
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
                                      h4("Prescreens"),
                                      plotlyOutput("PrescreenPlot"),
                                    h4("Number of REACH Assessments Conducted"),
                                    plotlyOutput("assessmentsBarPlot")
                           ), 
                           tabPanel("Service Delivery",
                                    h3("Service Delivery"),
                                    h4("Number of Clients by Delivery Type"),
                                    plotlyOutput("serviceDeliveryLinePlot"),
                                    h4("Time Spent on Highest Needs of Client"),
                                    plotlyOutput("highestNeedBarPlot") 
                           ),
                           tabPanel("Employment",
                                    h3("Employment"),
                                    h4("Client Engagement"),
                                    plotlyOutput("employmentLinePlot"), 
                                    h4("Total Percent of Employment"),
                                    plotlyOutput("employmentBarPlot")
                           ),
                           tabPanel("Housing",
                                    h3("Housing"),
                                    h4("Client Numbers"),
                                    plotlyOutput("housingResidentLinePlot"), 
                                    h4("Average Length of Stay"),
                                    plotlyOutput("housingCapacityLinePlotLength"), 
                                    h4("Bed Days Filled"),
                                    plotlyOutput("bedDaysLinePlot")
                           ),
                           tabPanel("SUD Treatment",
                                    h3("SUD Treatment"),
                                    h4("SUD Numbers"),
                                    plotlyOutput("SUDLinePlot"), 
                                    h4("SUD hourly breakdown"),
                                    plotlyOutput("SUDBarPlot"),
                                    h3("UA Treatment"),
                                    h4("UA Numbers"),
                                    plotlyOutput("UALinePlot"),
                                    h4("UA Breakdown"),
                                    plotlyOutput("UASLinePlot")
                           ),
                           tabPanel("Recidivism",
                                    h3("Recidivism"),
                                    h4("Engagements Number"),
                                    plotlyOutput("engagementsLinePlot"), 
                                    h4("Contacts to disengaged individuals"),
                                    plotlyOutput("engagementsMethodsLinePlot")
                           ),
                           tabPanel("Staffing",
                                    h3("Staffing"),
                                    plotlyOutput("staffingLinePlot")
                           ),
                           tabPanel("Fidelity and Training",
                                    h3("Fidelity and Training"),
                                    plotlyOutput("fidelityScoreLinePlot") 
                           ),
                           tabPanel("Exits",
                                    h3("Exits"),
                                    h4("Number of Exits"),
                                    plotlyOutput("exitLinePlot"),
                                    h4("Overall Attrition"),
                                    plotlyOutput("exitAttritionLinePlot")
                           ),
                           tabPanel("Financial",
                                    h3("Financial Data"),
                                    plotlyOutput("financesLinePlot")
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
    
  
  ## Graphics
    ### Program Overview
      #### Program Overview
        ##### Program Overview
        output$ProgramOverviewPlot <- renderPlotly({ProgramOverviewPlot <- plot_ly(x = months, y = strtoi(tData[ ,1]), name = 'New Clients', type = 'scatter', mode = 'lines+markers')  %>%
          add_trace(y = strtoi(tData[ ,2]), name = 'Total Clients', mode = 'lines+markers') %>%
          add_trace(y = strtoi(tData[ ,3]), name = 'Clients Housed', mode = 'lines+markers') %>%
          add_trace(y = strtoi(tData[ ,4]), name = 'Clients Permanently Housed', mode = 'lines+markers') %>%
          layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = ax)
        })
      #### Client Demographics
        ##### Client Age 
        output$AgesLinePlot <- renderPlotly({AgesLinePlot <- plot_ly(x = months, y = strtoi(tData[ ,9]), name = '26-35', type = 'scatter', mode = 'lines+markers')  %>%
          add_trace(y = strtoi(tData[ ,10]), name = '36-45', mode = 'lines+markers')%>%
          add_trace(y = strtoi(tData[ ,11]), name = '45+', mode = 'lines+markers')%>%
          layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
        })
        ##### Client Race
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
        ##### Eligible per HMIS Data Pull
        output$HMISplot <- renderPlotly({HMISplot <- plot_ly(x = months, y = strtoi(tData[,23]), type = 'bar', name = 'Eligible per HMIS Data Pull') %>%
          layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
        })
      #### TRH Location Endeavors
        ##### TRH Location Endeavors
        output$TRHplot <- renderPlotly({TRHplot <- plot_ly(x = months, y = strtoi(tData[ ,24]), name = 'TRH Attempted to Locate', type = 'scatter', mode = 'lines+markers') %>%
          add_trace(y = strtoi(tData[ ,25]), name = 'TRH Located', mode = 'lines+markers') %>%
        layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
        })
      #### Prescreen 
      output$PrescreenPlot <- renderPlotly({PrescreenPlot <- plot_ly(x = months, y = strtoi(tData[,29]), type = 'bar', name = 'Randomized into REACH') %>%
      layout(yaxis = list(title = 'Avg. Contacts from Randomization to Enrollment', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    output$assessmentsBarPlot <- renderPlotly({assessmentsBarPlot <- plot_ly(x = months, y = strtoi(tData[,30]), type = 'bar', name = 'Randomized into REACH') %>%
      layout(yaxis = list(title = 'Assessments Conducted', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    # Service Delivery
    output$serviceDeliveryLinePlot <- renderPlotly({serviceDeliveryLinePlot <- plot_ly(x = months, y = strtoi(tData[,31]), name = 'Intensive Treatment', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = strtoi(tData[,32]), name = 'Transition', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,33]), name = 'Sustained Recovery', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,34]), name = 'Long-term Recovery', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,35]), name = '200 Hours of Therapy', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,37]), name = 'Completed MRT', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals Receiving', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    output$highestNeedBarPlot <- renderPlotly({ highestNeedBarPlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,36])), type = 'bar', name = 'Randomized into REACH') %>%
      layout(yaxis = list(title = '% of Time Spent On Highest Priority', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    # Employment 
    output$employmentLinePlot <- renderPlotly({employmentLinePlot <- plot_ly(x = months, y = strtoi(tData[,38]), name = 'Completed Assessment', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = strtoi(tData[,39]), name = 'Obtained Employment ', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,40]), name = 'Engaged With REACH Employment', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,41]), name = 'Obtained a Job with DWS', mode = 'lines+markers')%>% #could error with ?
      add_trace(y = strtoi(tData[,42]), name = 'Engaged with Vocational Training', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,44]), name = 'Lost Their Job', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    output$employmentBarPlot <- renderPlotly({employmentBarPlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,43])), type = 'bar', name = 'REACH Clients') %>%
      layout(yaxis = list(title = '% of REACH Clients Employed', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    
    # Housing
    output$housingResidentLinePlot <- renderPlotly({housingResidentLinePlot <- plot_ly(x = months, y = strtoi(tData[,45]), name = 'Completed Housing Assessments', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = strtoi(tData[,45]), name = 'In Need of Residence', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,46]), name = 'Placed in REACH Recovery Residence', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,47]), name = 'Currently Housed in REACH Recovery', mode = 'lines+markers')%>% #could error with ?
      add_trace(y = strtoi(tData[,49]), name = 'Unique Clients served in REACH Recovery', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Clients', rangemode = "tozero"), xaxis = list(title = 'Month', ax))
    })
    output$housingCapacityLinePlotLength <- renderPlotly({ housingCapacityLinePlot <- plot_ly(x = months, y = strtoi(tData[,48]), name = 'Average Length of Stay', type = 'scatter', mode = 'lines+markers')  %>%
      layout(yaxis = list(title = 'Days', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    output$bedDaysLinePlot <- renderPlotly({bedDaysLinePlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,50])), name = 'In Residence', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = as.numeric(sub("%", "", tData[,51])), name = 'By Transitional', mode = 'lines+markers') %>%
      layout(yaxis = list(title = '% of Bed Days Filled', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    # SUD treatment
    output$SUDLinePlot <- renderPlotly({ SUDLinePlot <- plot_ly(x = months, y = strtoi(tData[,53]), name = 'SUD', type = 'scatter', mode = 'lines+markers')  %>%
      layout(yaxis = list(title = 'Number Completed', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    output$UALinePlot <- renderPlotly({ SUDLinePlot <- plot_ly(x = months, y = strtoi(tData[,54]), name = 'UA', type = 'scatter', mode = 'lines+markers')  %>%
      layout(yaxis = list(title = 'Number Completed', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    output$UASLinePlot <- renderPlotly({UASLinePlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,55])), name = 'Positive', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = as.numeric(sub("%", "", tData[,56])), name = 'No-show', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Percent (%)', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    output$SUDBarPlot <- renderPlotly({SUDBarPlot <- plot_ly(x = months, y = as.double(sub("%", "", tData[,57]))/100, type = 'bar', name = 'REACH Clients') %>% #divide by 100 as hours are entered as a percentage
      layout(yaxis = list(title = 'Average Number of Hours Per Client', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    # Recidivism 
    output$engagementsLinePlot <- renderPlotly({engagementsLinePlot <- plot_ly(x = months, y = strtoi(tData[,58]), name = 'Post-Incarceration Re-engagements', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = strtoi(tData[,55]), name = 'Successful Re-engagements', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,55]), name = 'Left Unsuccessfully', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number Completed', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    output$engagementsMethodsLinePlot <- renderPlotly({engagementsMethodsLinePlot <- plot_ly(x = months, y = as.double(tData[,59]), name = 'Avg. Days Between Jail and Re-enrollment', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = as.double(tData[,60]), name = 'Contact Attempts', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    #Staffing 
    output$staffingLinePlot <- renderPlotly({staffingLinePlot <- plot_ly(x = months, y = strtoi(tData[,62]), name = 'Case Managers', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = strtoi(tData[,63]), name = 'Mentors', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,64]), name = 'Program Managers', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,65]), name = 'Admission Coordinators', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,66]), name = 'Therapists', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number on Staff', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    #Fidelity 
    output$fidelityScoreLinePlot <- renderPlotly({fidelityScoreLinePlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,67])), name = 'Staff Trained In Modalities', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = as.numeric(sub("%", "", tData[,78])), name = 'MRT groups with Supervision', mode = 'lines+markers') %>%
      add_trace(y = as.numeric(sub("%", "", tData[,69])), name = 'Clinicians Receiving Fidelity Checks', mode = 'lines+markers') %>%
      add_trace(y = as.numeric(sub("%", "", tData[,70])), name = 'Fidelity Score for MRT', mode = 'lines+markers') %>%
      add_trace(y = as.numeric(sub("%", "", tData[,71])), name = 'Fidelity Score for MI', mode = 'lines+markers') %>%
      add_trace(y = as.numeric(sub("%", "", tData[,72])), name = 'Fidelity Score for TA', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Percent (%)', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    #Exits 
    output$exitLinePlot <- renderPlotly({exitLinePlot <- plot_ly(x = months, y = strtoi(tData[,73]), name = 'Total Unplanned Exits', type = 'scatter', mode = 'lines+markers')  %>%
      add_trace(y = strtoi(tData[,74]), name = 'Jail', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,75]), name = 'Prison', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,76]), name = 'Self Termination', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,77]), name = 'No Contact', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,78]), name = 'Total Terminated by FSH', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,79]), name = 'Deceased', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,80]), name = 'Transfered Programs', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,82]), name = 'Planned Exits', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Clients that Exitted', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    output$exitAttritionLinePlot <- renderPlotly({exitAttritionLinePlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,81])), name = 'Attrition', type = 'scatter', mode = 'lines+markers')  %>%
      layout(yaxis = list(title = 'Percent (%)', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    #Finances
    output$financesLinePlot <- renderPlotly({financesLinePlot <- plot_ly(x = months, y = as.double(tData[,83]), name = 'Finances', type = 'scatter', mode = 'lines+markers')  %>%
      layout(yaxis = list(title = 'Dollars ($)', rangemode = "tozero"), xaxis = list(title = 'Month', rangemode = "tozero"))
    })
}
# Run App
shinyApp(ui, server)