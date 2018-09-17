library(googlesheets)
library(plotly)
library(shiny)
library(shinythemes)
library(tidyverse)


# Define UI
ui <- fluidPage(theme = shinytheme("journal"),
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
  
# Import Data
gap <- gs_title("HNJ Service Provider Report_Updated")
myData <- gap %>%
  gs_read(col_names = F)

## Wrangling

  # Transpose Data
  tData <- t(myData) # transposes the data
  tData <- as.tibble(tData) # transforms the data into a dataframe

  # Clean up Columns
  tData <- tData[ ,-c(2, 7, 30, 57, 70, 78, 82, 85, 97)] # removes header columns
  tData <- tData[ ,-c(6, 10, 15, 24, 60)] # removes subheader columns
  tData <- tData[-1, ] # removes the first row from the dataset
  colnames(tData) = tData[1, ] # assigns column names to the first row
  
  # Clean up Rows
  tData <- tData[-c(1, 4, 8, 12, 16, 18, 19, 20, 21), ] # removes quarterly total and duplicate rows
  
  has_rownames(tData)
  
  rownames(tData) <- tData[ ,1]
  rownames(tData) <-tData[ ,1] # assigns column names from the first row
  


tData <- tData %>% 
  slice(-1)
