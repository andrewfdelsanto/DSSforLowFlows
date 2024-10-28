
# Load R packages
library(shiny)
library(shinythemes)
library(dataRetrieval)
library(fasstr)
library(lattice)
library(latticeExtra)
library(streamstats)
library(leaflet)
library(Kendall)
library(ggplot2)
library(plyr)
library(caret)
library(randomForest)

###################################################################################################################################
# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("CAFE - Low Flow Calculator",
                           
                           ###Tabs      
                           ###################################################################################################################################
                           
                           #Tab 0 - Introduction
                           tabPanel("Introduction", icon = icon("info-circle"),
                                    
                                    # Input form
                                    sidebarPanel(
                                      h2("Welcome to CAFE!"),
                                      h5("This program, the Calculation Assistant for Flow Extremes (CAFE), is designed to help resource managers throughout the Northeast United States analyze and calculate both extreme low and high streamflow metrics."),
                                      h5("This is the low flow portion of the program. The high flow version of the program can be found at: https://andrewdelsanto.shinyapps.io/HighFlowDSS/ "),
                                      h5("Additionally, you can download this program's documentation below, which explains how to use the program and all functions/packages utilized:"),
                                      downloadButton("downloadBtn2", "Download Documentation")
                                    ),
                                    
                                    # Output display
                                    mainPanel(
                                      h2("CAFE - Calculation Assistant for Flow Extremes"),
                                      imageOutput("prettypicture")
                                    )
                                    
                           ), #tabPanel1       
                           #Tab 1 - For downloading NWIS Streamflow data
                           
                           navbarMenu("Gaged Flow Calculator", icon = icon("water"),
                                      ################################################################################################################################### 
                                      
                                      #Tab 0 - Explanation of this Menu & Module
                                      tabPanel("Explanation of this Module",
                                               
                                               # Input form
                                               sidebarPanel(
                                                 h3("Welcome to the Gaged Low Flow Calculator!"),
                                                 h5("This portion of the program allows the user to calculate the 7-day-2-year low flow (7Q2), 7-day-10-year low flow (7Q10), and 7-day-30-year low flow (7Q30) for any NWIS streamgage with daily streamflow data."),
                                                 h5("It follows standard procedures for calculating the low flows above using the procedure highlighted in the EPA's Low Flow Statistics Manual (https://www.epa.gov/system/files/documents/2023-01/low-flow-stats-tools-handbook-jan2023.pdf)."),
                                                 h5("This specific module uses the well-known FASSTR package to analyze the daily flow data and calculate the 7Q2, 7Q10, and 7Q30 (https://cran.r-project.org/web/packages/fasstr/vignettes/fasstr_users_guide.html)."),
                                                 h5("Additionally, it allows the user to: 1) subset the streamflow data given a starting and end date, 2) recalculate all metrics, and 3) scale the flows if the location of interest is slightly up or downstream from the gage being used."),
                                               ),
                                               
                                               # Output display
                                               mainPanel(
                                                 h2("EPA Low Flow Manual - 2023 Update"),
                                                 imageOutput("EPApic")
                                               ),          
                                      ),
                                      
                                      tabPanel("Gaged Tool Step 1: Download Streamflow Data",
                                               
                                               # Input form
                                               sidebarPanel(
                                                 h2("Retrieve Streamflow"),
                                                 h4("Enter an 8 digit NWIS gage number with daily flow data:"),
                                                 textInput(inputId = "gage", "Enter NWIS Gage Number:", value = "01013500"),
                                                 actionButton(inputId = "dataretrieval", label = "Retrieve Streamflow Data")
                                               ),
                                               
                                               # Output display
                                               mainPanel(
                                                 h4("If succesfully retrieved, the first and last dates of daily streamflow data available will populate below."),
                                                 textOutput(outputId = "result"),
                                                 textOutput(outputId = "firstdate"),
                                                 textOutput(outputId = "lastdate"),
                                                 h4("Once the data is successfully retrieved, you can download an Excel file with the date and daily streamflow (cfs) below."),
                                                 downloadButton("streamflowdownload", "Download Streamflow")
                                               )
                                               
                                               
                                      ), #tabPanel1
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 2 - Plotting initial raw streamflow
                                      tabPanel("Gaged Tool Step 2: Display Streamflow",
                                               
                                               sidebarPanel(
                                                 h2("Raw Streamflow Data Analysis"),
                                                 h4("If streamflow was successfully retrieved in Step 1, quantile statistics (in cfs) are given below and displayed on the plot to the right:"),
                                                 textOutput(outputId = "minflow"),
                                                 textOutput(outputId = "twentyfifth"),
                                                 textOutput(outputId = "medianflow"),
                                                 textOutput(outputId = "seventyfifth"),
                                                 textOutput(outputId = "maxflow"),
                                                 downloadButton("downloadfullstreamflowPlot", "Download Current Plot")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Interactive Streamflow Plot - Raw Streamflow Data"),
                                                 h4("The current y limits of the plot are given by dashed red lines."),
                                                 plotOutput("RawTimeSeriesPlot"),
                                                 sliderInput("XLimits", "Change X Limits:", min = as.Date("01-01-1900","%m-%d-%y"), max = as.Date("12-31-2022","%m-%d-%y"), value=c(as.Date("01-01-1900","%m-%d-%y"),as.Date("12-31-2022","%m-%d-%y")),timeFormat="%m-%d-%y", width = '100%'),
                                                 sliderInput("YLimits", "Change Y Limits:", min = 0, max = 10000, value = c(0, 10000), step = 100, width = '100%')
                                               ) # mainPanel
                                               
                                      ), #tabpanel2
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 3 - Plotting initial raw streamflow
                                      tabPanel("Gaged Tool Step 3: Display Streamflow (Log Scale)",
                                               
                                               sidebarPanel(
                                                 h2("Streamflow Analysis (Log Y Axis)"),
                                                 h5("Here, we plot streamflows again but with a log y scale."),
                                                 h5("This is best for gages with large maximum flows that distort the y axis on regular streamflow plots."),
                                                 textOutput(outputId = "minflowlog"),
                                                 textOutput(outputId = "twentyfifthlog"),
                                                 textOutput(outputId = "medianflowlog"),
                                                 textOutput(outputId = "seventyfifthlog"),
                                                 textOutput(outputId = "maxflowlog"),
                                                 downloadButton("downloadfullstreamflowPlotlog", "Download Current Plot")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Interactive Streamflow Plot - Log Y Scale"),
                                                 plotOutput("RawTimeSeriesPlotlog"),
                                                 sliderInput("XLimitslog", "Change X Limits:", min = as.Date("01-01-1900","%m-%d-%y"), max = as.Date("12-31-2022","%m-%d-%y"), value=c(as.Date("01-01-1900","%m-%d-%y"),as.Date("12-31-2022","%m-%d-%y")),timeFormat="%m-%d-%y", width = '100%'),
                                               ) # mainPanel
                                               
                                      ), #tabpanel3
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 4 - Plotting lowest flows
                                      tabPanel("Gaged Tool Step 4: Plot Low Flows",
                                               
                                               sidebarPanel(
                                                 h2("Low Flow Analysis"),
                                                 h4("Low Flows for the Full Record Data (cfs):"),
                                                 textOutput(outputId = "tenthpercentileflow"),
                                                 textOutput(outputId = "minflow2"),
                                                 h6("----------------------------------------------------------"),
                                                 textOutput(outputId = "fullrecordsevenqtwo"),
                                                 textOutput(outputId = "fullrecordsevenqten"),
                                                 textOutput(outputId = "fullrecordsevenqthirty"),
                                                 h6("----------------------------------------------------------"),
                                                 textOutput(outputId = "flatline"),
                                                 downloadButton("downloadfulllowflowsplot", "Download Current Plot")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Interactive Streamflow Plot - Bottom 10th Percentile of Flows"),
                                                 plotOutput("Lowest10Plot"),
                                                 sliderInput("XLimitslowflows", "Change X Limits:", min = as.Date("01-01-1900","%m-%d-%y"), max = as.Date("12-31-2022","%m-%d-%y"), value=c(as.Date("01-01-1900","%m-%d-%y"),as.Date("12-31-2022","%m-%d-%y")),timeFormat="%m-%d-%y", width = '100%'),
                                               ) # mainPanel
                                               
                                      ), #tabpanel4
                                      
                                      ####################################################################################################################################
                                      
                                      
                                      #Tab 5 - Plotting watershed and getting physical characteristics
                                      tabPanel("Gaged Tool Step 5: Mann-Kendall Test for Trends",
                                               # Input form
                                               sidebarPanel(
                                                 h2("Check for Trends"),
                                                 h4("Blum et al., 2019 suggests only using the last 30 years of streamflow data to calculate the 7Q10 if a trend  (P-Value < 0.05) is detected in the annual 7-day low flows."),
                                                 h4("See below for the results of the trend test:"),
                                                 textOutput("mktau"),
                                                 textOutput("mkpvalue"),
                                                 downloadButton("AnnualLowFlowDownload", "Download Plot")
                                               ),
                                               
                                               mainPanel(
                                                 h2("Plot of Annual 7-Day Low Flows (with Lowess Smoothing Line)"),
                                                 plotOutput("MannKendallPlot")
                                               )# mainPanel
                                               
                                      ), #tabpanel4
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 6 - Subsetting the Data
                                      tabPanel("Gaged Tool Step 6: Subset Data",
                                               
                                               sidebarPanel(
                                                 h2("Subset Data Tool"),
                                                 h4("Here, we allow you to subset the full streamflow record."),
                                                 h4("Choose a beginning date and an end date to trim the data. You can use the plot in Step 2 to help you select dates."),
                                                 h4("Note- All future calculations will use the subset record from this tab."),
                                                 h6("------------------------------------"),
                                                 h6("------------------------------------"),
                                                 dateInput("lowerbound", "Lower Bound Date:", value = "1992-01-01", format = "mm/dd/yyyy"),
                                                 dateInput("upperbound", "Upper Bound Date:", value = "2022-12-31", format = "mm/dd/yyyy"),
                                                 actionButton(inputId = "savesubset", label = "Save Subset Data?"),
                                                 textOutput(outputId = "savedsubset"),
                                               ),
                                               
                                               mainPanel(
                                                 h2("Choose New Subset"),
                                                 plotOutput("subsetplot"),
                                                 h4("Once you save a subset to the left, you can download the new Excel file below."),
                                                 downloadButton("subsetstreamflowdownload", "Download Subset Streamflow")
                                               )
                                               
                                      ), #tabpanel6
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 7 - Plotting lowest flows for subset data
                                      tabPanel("Gaged Tool Step 7: Plot Low Flows - Subset Data",
                                               
                                               sidebarPanel(
                                                 h2("Low Flow Analysis"),
                                                 h4("Low Flows for Subset Data (cfs):"),
                                                 textOutput(outputId = "tenthpercentileflowsubset"),
                                                 textOutput(outputId = "minflowsubset"),
                                                 
                                                 h6("----------------------------------------------------------"),
                                                 
                                                 textOutput(outputId = "minflowrepeat"),
                                                 textOutput(outputId = "fullrecordsevenqtworepeat"),
                                                 textOutput(outputId = "fullrecordsevenqtenrepeat"),
                                                 textOutput(outputId = "fullrecordsevenqthirtyrepeat"),
                                                 
                                                 h6("----------------------------------------------------------"),
                                                 
                                                 textOutput(outputId = "fullrecordsevenqtwosubset"),
                                                 textOutput(outputId = "fullrecordsevenqtensubset"),
                                                 textOutput(outputId = "fullrecordsevenqthirtysubset"),
                                                 
                                                 textOutput(outputId = "flatlinesubset"),
                                                 downloadButton("downloadsubsetlowflowsplot", "Download Current Plot")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Interactive Streamflow Plot - Bottom 10th Percentile of Flows for Chosen Subset Record"),
                                                 plotOutput("Lowest10Plotsubset"),
                                                 sliderInput("XLimitslowflowssubset", "Change X Limits:", min = as.Date("01-01-1900","%m-%d-%y"), max = as.Date("12-31-2022","%m-%d-%y"), value=c(as.Date("01-01-1900","%m-%d-%y"),as.Date("12-31-2022","%m-%d-%y")),timeFormat="%m-%d-%y", width = '100%'),
                                               ) # mainPanel
                                               
                                      ), #tabpanel6
                                      
                                      
                                      ####################################################################################################################################
                                      
                                      tabPanel("Gaged Tool Step 8: Flow Scaling",
                                               
                                               sidebarPanel(
                                                 h2("Area/Area Flow Scaling Tool"),
                                                 h4("Here, you can use a simple area/area ratio to scale up or down the values from before."),
                                                 textOutput(outputId = "gagedrainagearearesult"),
                                                 numericInput(inputId = "ungagedarea", "Enter the up/downstream watershed area (mi^2):", value = NULL),
                                                 actionButton(inputId = "flowscaling", label = "Direct Area/Area Flow Scaling"),
                                                 textOutput(outputId = "scalefactor"),
                                                 textOutput(outputId = "scalenote"),
                                                 h6("----------------------------------------------------------"),
                                                 textOutput(outputId = "scaledfullrecordsevenqtwosubset"),
                                                 textOutput(outputId = "scaledfullrecordsevenqtensubset"),
                                                 textOutput(outputId = "scaledfullrecordsevenqthirtysubset"),
                                                 
                                                 
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h2("Low Flows for Subset & Scaled Data"),
                                                 plotOutput("Lowest10Plotsubsetandscaled"),
                                                 sliderInput("XLimitslowflowssubsetandscaled", "Change X Limits:", min = as.Date("01-01-1900","%m-%d-%y"), max = as.Date("12-31-2022","%m-%d-%y"), value=c(as.Date("01-01-1900","%m-%d-%y"),as.Date("12-31-2022","%m-%d-%y")),timeFormat="%m-%d-%y", width = '100%'),
                                                 h4("Once you generate the plot above, you can download it using the button below."),
                                                 downloadButton("flowscaledplotdownload", "Download Flow Scaled Plot")
                                               ) # mainPanel
                                               
                                      ), #tabpanel8
                                      
                           ),#First Menu Navbar
                           
                           ###Ungaged Tabs Start
                           
                           navbarMenu("Ungaged Flow Estimator", icon = icon("faucet-drip"),
                                      
                                      ###################################################################################################################################
                                      
                                      #Tab 0 - Explanation of this Menu & Module
                                      tabPanel("Explanation of this Module",
                                               
                                               # Input form
                                               sidebarPanel(
                                                 h3("Welcome to the Ungaged Low Flow Estimator!"),
                                                 h5("This portion of the program allows the user to estimate the 7Q10 in ungaged watersheds, similar to the USGS' StreamStats program."),
                                                 h5("It follows the exact same procedure as StreamStats and even uses the StreamStats' API to delineate the watershed and calculate the physical characteristics of the watershed (area, elevation, etc.)."),
                                                 h5("However, the final regression equations used to calculate the 7Q10 utilize regionally-developed regression equations, rather than state-by-state equations."),
                                                 h5("The equations and corresponding methodologies can be found in this paper: https://www.mdpi.com/2073-4441/15/15/2813"),
                                                 h5("More information about the exact steps for this module are described in the next tab.")
                                               ),
                                               
                                               # Output display
                                               mainPanel(
                                                 h3("Low Flow (7Q10) Methodologies"),
                                                 imageOutput("sevenqtenpaper")
                                               )
                                               
                                      ), #tabPanel0       
                                      ###################################################################################################################################
                                      
                                      #Tab 1 - For StreamStats API
                                      tabPanel("Ungaged Tool Step 1: Background Information",
                                               
                                               sidebarPanel(
                                                 h2("Link to StreamStats"),
                                                 h3("As mentioned in the previous tab, if you prefer to use StreamStats' website to retrieve the watershed information, the link is included below."),
                                                 h4("Go to StreamStats' website:"),
                                                 h4("https://streamstats.usgs.gov/ss/")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h4("This program mimics the regression procedure for calculating extreme flows used by the USGS' StreamStats."),
                                                 h6("----------------------------------------------------------"),
                                                 h4("We have trained 6 regression equations, using 3 statistical methods, to calculate the 7Q10 in the northeast United States using a location's physical watershed characteristics."),
                                                 h4("The first three equations are trained on the full-record of streamflow data at every training location, similar to StreamStats."),
                                                 h4("The second three equations, however, are trained on the 30 most recent years of streamflow data at every training location, as requested by stakeholders."),
                                                 h4("For shorter-term extreme flows like the 7Q10, some organizations (for example, EPA Region 1) are required to use the last 30 years of streamflow data to calculate the 7Q10, rather than the traditional procedure of using as much data as is available."),
                                                 h4("Additionally, some papers have demonstrated that using the last 30 years of streamflow data to calculate the 7Q10 reduces error when there is an increasing or decreasing trend in the annual 7-day low flows (https://sites.tufts.edu/richardvogel/files/2019/09/Blum-et-al.-HSJ-2019.pdf)"),
                                                 h6("----------------------------------------------------------"),
                                                 h4("For the statistical methodologies, we have used multiple linear regression (MLR), multiple linear regression in log space (the traditional method, LTLR), and the machine learning algorithm Random Forest decision trees (RF)."),
                                                 h4("More detailed information on these methods can be found in the open-source paper cited in the previous tab."),
                                                 h6("----------------------------------------------------------"),
                                                 h4("These equations require knowing your ungaged watershed's area*, the average watershed elevation* and slope*, and the percents of the watershed considered wetland* and forest*."),
                                                 h4("If you have calculated or estimated these already, you can proceed to the final tab to estimate the 100-Year-Flood. If you have not, you have two options:"),
                                                 h4("1) You can go to StreamStats' website right now, delineate your basin, estimate the 5 characteristics above, and input them yourself on the final tab."),
                                                 h4("2) If you want to attempt to import them automatically from StreamStats, all you will need is a valid latitude and longitude of a blue pixel from StreamStats' map."),
                                                 h4("Regardless, both of these will require going to StreamStats' website, so the link on the left will take you to StreamStats."),
                                               ) # mainPanel
                                               
                                      ), #tabPanel1
                                      
                                      ###################################################################################################################################
                                      
                                      #Tab 2 - For StreamStats API
                                      tabPanel("Ungaged Tool Step 2: Contact StreamStats",
                                               
                                               sidebarPanel(
                                                 h2("Contacting StreamStats"),
                                                 h5("If you have found a valid latitude and longitude from StreamStats, please enter it below:"),
                                                 numericInput(inputId = "lat", "Latitude:", value = 42.38495),
                                                 numericInput(inputId = "lon", "Longitude:", value = -72.54446),
                                                 
                                                 h5("Attempt to contact StreamStats? Note- this may take a few minutes, so a pop-up will be displayed in the bottom right corner while waiting."),
                                                 actionButton(inputId = "calculate", label = "Calculate"),
                                                 
                                                 textOutput(outputId = "ssresult"),
                                                 textOutput(outputId = "arearesult"),
                                                 textOutput(outputId = "continue")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 leafletOutput("map")
                                               ) # mainPanel
                                               
                                      ), #tabPanel2
                                      
                                      ####################################################################################################################################
                                      
                                      #Tab 3 - Plotting watershed and getting physical characteristics
                                      tabPanel("Ungaged Tool Step 3: Display Watershed & Characteristics", 
                                               
                                               sidebarPanel(
                                                 h2("Display Results"),
                                                 h4("If delineation was successful in the last tab, the watershed itself, all available physical characteristics, and StreamStats' estimated 7Q10 (if available) are shown here."),
                                                 textOutput(outputId = "watershedarea"),
                                                 textOutput(outputId = "watershedelevation"),
                                                 textOutput(outputId = "watershedslope"),
                                                 textOutput(outputId = "watershedforest"),
                                                 textOutput(outputId = "watershedwetland"),
                                                 textOutput(outputId = "streamstats7q10")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 leafletOutput("watershed")
                                               ) # mainPanel
                                               
                                      ), #tabpanel3
                                      
                                      ###################################################################################################################################
                                      tabPanel("Ungaged Tool Step 4: Regression-Based 7Q10 Estimation", 
                                               
                                               sidebarPanel(
                                                 h2("Estimate 7Q10s"),
                                                 h4("Here, we use pre-trained regression equations to estimate the 7Q10."),
                                                 h4("For more information on the development of these regression equations, please see the paper cited in the explanation tab."),
                                                 h3("Please enter the physical characteristics below:"),
                                                 numericInput("ungaged7q10area", "Watershed Area (square mi):", min = 1, max = 1420, value = 100),
                                                 numericInput("ungaged7q10elevation", "Average Watershed Elevation (ft):", min = 10, max = 3337, value = 1000),
                                                 numericInput("ungaged7q10slope", "Average Watershed Slope (Whole number %):", min = 0, max = 32, value = 7),
                                                 numericInput("ungaged7q10forest", "Forest % (Whole Number) of Watershed:", min = 0, max = 100, value = 1),
                                                 numericInput("ungaged7q10wetland", "Wetland % (Whole Number) of Watershed:", min = 0, max = 100, value = 1),
                                                 actionButton(inputId = "regressions", label = "Calculate 7Q10s")
                                               ), # sidebarPanel
                                               
                                               mainPanel(
                                                 h3("Physical Characteristics (if StreamStats was succesfully contacted in Step 2):"),
                                                 textOutput(outputId = "watershedarearepeat"),
                                                 textOutput(outputId = "watershedelevationrepeat"),
                                                 textOutput(outputId = "watershedsloperepeat"),
                                                 textOutput(outputId = "watershedforestrepeat"),
                                                 textOutput(outputId = "watershedwetlandrepeat"),
                                                 textOutput(outputId = "streamstats7q10repeat"),
                                                 
                                                 h3("Full Record 7Q10 (cfs) Estimates:"),
                                                 verbatimTextOutput("MLRfull_output"),
                                                 verbatimTextOutput("LTLRfull_output"),
                                                 verbatimTextOutput("RFfull_output"),
                                                 
                                                 h3("Recent 30-Year 7Q10 (cfs) Estimates:"),
                                                 verbatimTextOutput("MLR30_output"),
                                                 verbatimTextOutput("LTLR30_output"),
                                                 verbatimTextOutput("RF30_output"),
                                                 
                                                 h3("Notes:"),
                                                 textOutput(outputId = "small_basins"),
                                                 textOutput(outputId = "large_basins")
                                               ) # mainPanel          
                                               
                                      ), #tabpanel4
                                      
                           ), #Menu Nav Bar for Ungaged Part
                           
                           #######################################################################################
                           navbarMenu("Apply Climate Change", icon = icon("sun-plant-wilt"),
                                      #Tab 0 - Explanation of this Menu & Module
                                      tabPanel("Explanation of this Module",
                                               
                                               # Input form
                                               sidebarPanel(
                                                 h3("Welcome to the Climate-Altered Low Flow Estimator!"),
                                                 h5("This portion of the program allows the user to estimate projected changes in the 7Q10 given future projected changes in climate."),
                                                 h5("This methodology follows the procedure given in the Transportation Research Board's (TRB's) National Cooperative Highway Research Program's (NCHRP) 15-61: Applying Climate Change Information to Hydrologic and Hydraulic Design of Transportation Infrastructure (Kilgore et al., 2019)."),
                                                 h5("The general procedure uses the regional, full-record logarithmic-regression equation from the previous module with the climate input variables adjusted for future climate projections."),
                                                 h5("More information can be found at: https://apps.trb.org/cmsfeed/TRBNetProjectDisplay.asp?ProjectID=4046")
                                               ),
                                               
                                               # Output display
                                               mainPanel(
                                                 h3("Applying Climate Change Information to Hydrologic and Hydraulic Design of Transportation Infrastructure"),
                                                 imageOutput("nchrppicture")
                                               )         
                                               
                                      ),
                                      
                                      tabPanel("Climate Change Projections: Climate-Altered 7Q10", 
                                               
                                               sidebarPanel(
                                                 h4("Enter your 7Q10 estimate:"),
                                                 numericInput("estimatedvalue", "Estimated 7Q10 (cfs):", min = 0, max = 100, value = 5),
                                                 # h4("Your Multiple Linear Regression-based, new projected 7Q10 estimate (cfs):"),
                                                 # textOutput(outputId = "MLRprojectedvalue"),
                                                 h4("Your new projected 7Q10 estimate using the Logarithmic-Regression Equation (cfs):"),
                                                 textOutput(outputId = "LTLRprojectedvalue")
                                               ), # sidebarPanel
                                               
                                               
                                               mainPanel(
                                                 h2("Projected Changes:"),
                                                 sliderInput("tempslider", "Projected Temperature Change (F):", min = 0, max = 20, value = 5, step = 1, width = '100%'),
                                                 sliderInput("precipslider", "Projected Precipitation Change (%):", min = -20, max = 20, value = -8, step = 1, width = '100%'),
                                                 
                                                 h3("Suggested Values for Short-term (2020-2060): "),
                                                 h4("RCP4.5: Temperature = +3F, Precipitation = +3%"),
                                                 h4("RCP8.5: Temperature = +4F, Precipitation = -1%"),
                                                 h3("Suggested Values for Long-term (2060-2100): "),
                                                 h4("RCP4.5: Temperature = +5F, Precipitation = -8%"),
                                                 h4("RCP8.5: Temperature = +10F, Precipitation = -15%"),
                                               ) # mainPanel
                                               
                                      ), #tabpanel4
                           ), #Final Menu Navbar
                           
                           tabPanel("Documentation", icon = icon("download"),
                                    
                                    sidebarPanel(
                                      h2("Download Documentation File"),
                                      h4("Here, you can download the current version's documentation."),
                                      downloadButton("downloadBtn", "Download Document")
                                    ), # sidebarPanel
                                    
                                    mainPanel(
                                      h4("Thank you for using our app!"),
                                      h4("Funding and project information can be found at: https://www.sciencebase.gov/catalog/item/5f2ac7ab82cef313eda0f21b"),
                                      imageOutput("fundingorgs")
                                    ) # mainPanel
                                    
                           ) #tabpanel5
                           
                           #Close ui
                ) # navbarPage
) # fluidPage

###################################################################################################################################