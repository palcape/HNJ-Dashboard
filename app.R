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
                                      h4("Gender"),
                                        plotlyOutput("GenderHistogram"),
                                      h4("Age"),
                                        plotlyOutput("AgeLineChart"),
                                      h4("Race"),
                                        plotlyOutput("RaceHistogram"),
                                      h4("Ethnicity"),
                                        plotlyOutput("EthnicityHistogram")
                           ),
                           tabPanel("Referrals and Enrollments",
                                    h3("Referrals and Enrollments"),
                                      h4("TRH Successfully Located"),
                                        plotlyOutput("LocatedPlot"),
                                      h4("Eligible for Prescreen"),
                                        plotlyOutput("EligiblePrescreenPlot"),
                                      h4("Ineligble for Prescreen"),
                                        plotlyOutput("IneligiblePrescreenPlot"),
                                      h4("Prescreens Completed"),
                                        plotlyOutput("PrescreensCompletedPlot"),
                                      h4("Identified as Eligible for Prescreen After This Month"),
                                        plotlyOutput("IdentifiedAsEligiblePlot"),
                                      h4("Identified as Ineligible for Prescreen After This Month"),
                                        plotlyOutput("IdentifiedAsEligiblePlot"),
                                      h4("Sent to UCJC for Randomization"),
                                        plotlyOutput("UCJCPlot"),
                                      h4("Randomized into HNJ"),
                                        plotlyOutput("RandomizedIntoHNJPlot"),
                                      h4("PendingApproval"),
                                        plotlyOutput("PendingApprovalPlot"),
                                      h4("Days Between Initial Outreach and Prescreen Completion"),
                                        plotlyOutput("OutreachCompletionPlot"),
                                      h4("Days Between Referral to Treatment Group and Engagement with HNJ"),
                                        plotlyOutput("Engagement with HNJPlot"),
                                      h4("Days Between Meeting with Housing Specialist and Housing Placement"),
                                        plotlyOutput("HousingPlacementPlot")
                           ),
                           tabPanel("Housing Placement and Services",
                                    h3("Housing Placement and Services"),
                                      h4("Roommate Placement Assessments Conducted"),
                                        plotlyOutput("RoommateTestsPlot"),
                                      h4("New Placements into Housing"),
                                        plotlyOutput("HousingPlacementsPlot"),
                                      h4("Clients Awaiting Housing Placement"),
                                        plotlyOutput("AwaitingHousingPlot"),
                                      h4("Not Housed within Three Months of Enrollment"),
                                        plotlyOutput("NotHousedPlot"),
                                    h3("Rental Subsidies"),
                                      h4("Clients Receiving Rental Subsidies"),
                                        plotlyOutput("ReceivingSubsidiesPlot"),
                                      h4("Single Placement Monthly Rent"),
                                        plotlyOutput("MonthlyRentPlot"),
                                      h4("Roommate Monthly Rent"),
                                        plotlyOutput("RoommateRentPlot"),
                                      h4("Length of HNJ Rental Subsidy (Months)"),
                                        plotlyOutput("SubsidyLength"),
                                      h4("Clients Receiving Non-Concurrent Rental Subsidy"),
                                        plotlyOutput("NoncurrentSubsidyPlot")
                           ),
                           tabPanel("Behavioral Health",
                                    h3("Behavioral Health"),
                                      h4("Clients Referred to Behavioral Health Treatment"),
                                        plotlyOutput("ReferredtoTreatmentPlot"),
                                      h4("Behavioral Health Assessments Conducted"),
                                        plotlyOutput("HealthAssessmentsPlot"),
                                      h4("Clients Referred to Behavioral Health Treatment After Assessment"),
                                        plotlyOutput("AfterAssessmentPlot"),
                                      h4("Met with HNJ Clinician"),
                                        plotlyOutput("MetWithClinicianPlot"),
                                      h4("Enrolled in Additional Community Services"),
                                        plotlyOutput("CommunityServicesPlot")
                           ),
                           tabPanel("Employment",
                                    h3("Employment"),
                                      h4("Employment Assessments Conuducted"),
                                        plotlyOutput("EmploymentAssessmentsPlot"),
                                      h4("Participants Enrolled in Employment Services"),
                                        plotlyOutput("EmploymentServicesPlot"),
                                      h4("Participants with a Source of Income"),
                                        plotlyOutput("SourceOfIncomePlot")
                           ),
                           tabPanel("Staffing",
                                    h3("Staffing"),
                                      h4("Staff to Client Ratio"),
                                        plotlyOutput("StaffClientRatioPlot"),
                                      h4("Positions Available vs Positions Filled"),
                                        plotlyOutput("PositionsRatio")
                           ),
                           tabPanel("Exits",
                                    h3("Exits"),
                                      h4("Clients that Lost Housing"),
                                        plotlyOutput("ClientsLostHousingPlot"),
                                      h4("Unplanned Exits this Month"),
                                        plotlyOutput("UnplannedExitsPlot"),
                                      h4("Graduations from the Program"),
                                        plotlyOutput("GraduationsPlot")
                           ),
                           tabPanel("Financial",
                                    h3("Financial"),
                                      h4("Program Expenditures to Date"),
                                        plotlyOutput("ExpendituresToDate")
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
    tData <- tData[ ,-c(1, 6, 29, 56, 69, 77, 81, 84, 96)] # removes header columns
    tData <- tData[ ,-c(7, 11, 16, 24, 25, 28, 63)] # removes subheader columns
    colnames(tData) <- as.character(unlist(tData[2,])) # assigns column names to second row
    tData <- tData[-c(1, 2, 5, 9, 13, 17, 19, 20, 21, 22), ] # removes quarterly total and duplicate rows
    xaxis <- rownames(tData) # assigns row names to a vector we can use in our graph

  ## Plot Program Overview
    ## Plot Number of individuals randomized into REACH this month via line graph
    months <- factor(xaxis,levels = c("January", "February", "March",  "April",   "May",  "June", "July", "August",  "September", "October", "November",  "December"))
    
    ## Plot Program Overview: 
    output$programOverviewPlot <- renderPlotly({programOverviewPlot <- plot_ly(x = months, y = strtoi(tData[,1]), name = 'Randomized', type = 'scatter', mode = 'lines+markers')  %>%
      #Plot Number of individuals referred to REACH this month
      add_trace(y = strtoi(tData[,2]), name = 'Referred', mode = 'lines+markers') %>%
      #Plot Number of new clients enrolled in REACH this month
      add_trace(y = strtoi(tData[,3]), name = 'New Clients', mode = 'lines+markers') %>%
      #Plot Number of REACH clients actively receiving services
      add_trace(y = strtoi(tData[,4]), name = 'Receiving Services', mode = 'lines+markers') %>%
      #Plot Total number of individuals enrolled in REACH 
      add_trace(y = strtoi(tData[,5]), name = 'Total Enrolled', mode = 'lines+markers') %>%
      add_trace(y = strtoi(tData[,9]), name = 'Completed REACH', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = ax)
    })
    
    # Client Information 
    # Plot Client Information as Line Graph
    output$agesLinePlot <- renderPlotly({agesLinePlot <- plot_ly(x = months, y = strtoi(tData[,11]), name = '18-25', type = 'scatter', mode = 'lines+markers')  %>%
      #Plot Number of individuals referred to REACH this month
      add_trace(y = strtoi(tData[,12]), name = '26-35', mode = 'lines+markers') %>%
      #Plot Number of new clients enrolled in REACH this month
      add_trace(y = strtoi(tData[,13]), name = '35-44', mode = 'lines+markers') %>%
      #Plot Number of REACH clients actively receiving services
      add_trace(y = strtoi(tData[,14]), name = '45+', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    #Plot Race as Line Graph
    output$raceLinePlot <- renderPlotly({ raceLinePlot <- plot_ly(x = months, y = strtoi(tData[,15]), name = 'American Indian', type = 'scatter', mode = 'lines+markers')  %>%
      #Plot Number of individuals referred to REACH this month
      add_trace(y = strtoi(tData[,16]), name = 'Asian', mode = 'lines+markers') %>%
      #Plot Number of new clients enrolled in REACH this month
      add_trace(y = strtoi(tData[,17]), name = 'Black/African American', mode = 'lines+markers') %>%
      #Plot Number of REACH clients actively receiving services
      add_trace(y = strtoi(tData[,18]), name = 'Black/African American, White', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,19]), name = 'Pacific Islander', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,20]), name = 'Other: Single race', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,21]), name = 'Other: Two or more races', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,22]), name = 'White', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,23]), name = 'Mexican', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,24]), name = 'Not of Hispanic Origin', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,25]), name = 'Other: Hispanic', mode = 'lines+markers')%>%
      add_trace(y = strtoi(tData[,26]), name = 'Puerto Rican', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    # Referrals and Randomization 
    output$randomizedBarPlot <- renderPlotly({randomizedBarPlot <- plot_ly(x = months, y = strtoi(tData[,27]), type = 'bar', name = 'Randomized into REACH') %>%
      layout(yaxis = list(title = 'Number of Individuals Randomized into REACH', rangemode = "tozero"), xaxis = list(title = 'Month'))
    })
    
    output$betweenEnrollmentdBarPlot <- renderPlotly({betweenEnrollmentdBarPlot <- plot_ly(x = months, y = as.double(tData[,28]), type = 'bar', name = 'Randomized into REACH') %>%
      layout(yaxis = list(title = 'Avg. Days from Randomization to Enrollment'), xaxis = list(title = 'Month'))
    })
    
    output$contactsBetweenEnrollmentdBarPlot <- renderPlotly({contactsBetweenEnrollmentdBarPlot <- plot_ly(x = months, y = as.double(tData[,29]), type = 'bar', name = 'Randomized into REACH') %>%
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