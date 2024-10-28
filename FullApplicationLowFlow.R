
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

###################################################################################################################################  
#Begin actual server code

# Define server function  
server <- function(input, output) {
  
  #First Tab - Documentation and pretty picture
  
  ####Documentation added to first tab too
  output$downloadBtn2 <- downloadHandler(
    filename = "documentation.docx",
    content = function(file) {
      file.copy("documentation.docx", file)
    }
  )
  
  ####Generate and output pretty picture
  output$prettypicture <- renderImage({
    # Get the path to the image file
    img_path <- "Picturefordss.png"
    
    # Return the image object
    list(src = img_path,
         width = "55%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
  ###################################################################
  #Introduction Tab to Gaged Flow Calculator
  
  ####Generate and output pretty picture
  output$EPApic <- renderImage({
    # Get the path to the image file
    img_path <- "EPAlowflowmanual.png"
    
    # Return the image object
    list(src = img_path,
         width = "55%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
  # Download streamflow data when button is clicked
  observeEvent(input$dataretrieval, {
    
    gage <- input$gage
    site_info <- readNWISsite(gage)
    drainage_area <- as.numeric(site_info$drain_area_va)
    
    # Retrieve streamflow data
    parameterCd <- "00060"
    rawDailyData <- readNWISdv(gage,parameterCd,"1900-01-01")
    
    # Output result
    output$result <- renderText("Download was successful!")
    
    firstdatestreamflow <- head(rawDailyData$Date, n=1)
    lastdatestreamflow <- tail(rawDailyData$Date, n=1)
    
    output$firstdate <- renderText({
      paste("The first date of streamflow data available is:", firstdatestreamflow)
    })
    
    output$lastdate <- renderText({
      paste("The last date of streamflow data available is:", lastdatestreamflow)
    })
    
    ###############################Tab2
    
    names(rawDailyData)[names(rawDailyData) == "X_00060_00003"] <- "Value"
    
    DailyFlows <- rawDailyData[c("Date","Value")]
    
    DailyFlows$Value <- as.numeric(DailyFlows$Value)
    
    output$streamflowdownload <- downloadHandler(
      filename = function() {
        "streamflow.csv"
      },
      
      content = function(file) {
        # Write your data generation logic here
        # Replace the following example with your own data
        
        write.csv(DailyFlows, file, row.names = FALSE)
      }
    )
    
    minflow <- min(DailyFlows$Value, na.rm = TRUE)
    maxflow <- max(DailyFlows$Value, na.rm = TRUE)
    quantiles <- quantile(DailyFlows$Value, seq(.25, .75, by = .25), na.rm = TRUE)
    
    # Update the slider input with the maximum y value rounded up to the 1000s
    observe({
      updateSliderInput(
        inputId = "YLimits",
        max = round_any(maxflow, 1000, f = ceiling),
        step = 10,
        value = c(0,round_any(maxflow, 1000, f = ceiling))
      )
    })
    
    observe({
      updateSliderInput(
        inputId = "XLimits",
        min = firstdatestreamflow,
        max = lastdatestreamflow,
        value = c(firstdatestreamflow,lastdatestreamflow)
      )
    })
    
    output$minflow <- renderText({
      paste("The Minimum Flow (dashed blue) is:  ", minflow)
    })
    
    output$twentyfifth <- renderText({
      paste("The 25th Percentile Flow (dashed blue) is:  ", quantiles[1])
    })
    
    output$medianflow <- renderText({
      paste("The Median Flow (dashed blue, slightly thicker) is:  ", quantiles[2])
    })
    
    output$seventyfifth <- renderText({
      paste("The 75th Percentile Flow (dashed blue) is:  ", quantiles[3])
    })
    
    output$maxflow <- renderText({
      paste("The Maximum Flow (dashed blue) is:  ", maxflow)
    })
    
    # Generate the plot
    RawTimeSeriesPlot <- reactive({
      # Generate the plot based on your data and logic
      # Replace the following example with your own plot
      
      ggplot(DailyFlows, aes(Date, Value)) +
        geom_line() +
        labs(x = "Date", y = "Streamflow (cfs)") +
        geom_hline(yintercept = quantiles[1], col = "blue", linetype = "dashed") +
        geom_hline(yintercept = quantiles[2], col = "blue", linetype = "dashed", size = 1) +
        geom_hline(yintercept = quantiles[3], col = "blue", linetype = "dashed") +
        geom_hline(yintercept = minflow, col = "blue", linetype = "dashed") +
        geom_hline(yintercept = maxflow, col = "blue", linetype = "dashed") +
        
        geom_hline(yintercept = input$YLimits[1], col = "red", linetype = "dashed", size = 0.25) +
        geom_hline(yintercept = input$YLimits[2], col = "red", linetype = "dashed", size = 0.25) +
        
        ylim(input$YLimits[1], input$YLimits[2]) +
        xlim(as.Date(input$XLimits[1],"%m-%d-%y"),as.Date(input$XLimits[2],"%m-%d-%y"))
    })
    
    # Render the plot
    output$RawTimeSeriesPlot <- renderPlot({
      plot <- RawTimeSeriesPlot()
      print(plot)
    })
    
    # Download the plot as a JPEG image
    output$downloadfullstreamflowPlot <- downloadHandler(
      filename = function() {
        "fullstreamflowplot.jpg"
      },
      content = function(file) {
        # Save the plot as a JPEG image
        ggsave(file, plot = RawTimeSeriesPlot(), device = "jpeg", width = 12, height = 6)
      }
    )
    
    ###############################Tab3
    
    # Update the slider input with the new xs
    
    observe({
      updateSliderInput(
        inputId = "XLimitslog",
        min = firstdatestreamflow,
        max = lastdatestreamflow,
        value = c(firstdatestreamflow,lastdatestreamflow)
      )
    })
    
    output$minflowlog <- renderText({
      paste("The Minimum Flow (dashed blue) is:  ", minflow)
    })
    
    output$twentyfifthlog <- renderText({
      paste("The 25th Percentile Flow (dashed blue) is:  ", quantiles[1])
    })
    
    output$medianflowlog <- renderText({
      paste("The Median Flow (dashed blue, slightly thicker) is:  ", quantiles[2])
    })
    
    output$seventyfifthlog <- renderText({
      paste("The 75th Percentile Flow (dashed blue) is:  ", quantiles[3])
    })
    
    output$maxflowlog <- renderText({
      paste("The Maximum Flow (dashed blue) is:  ", maxflow)
    })
    
    # Generate the plot
    RawTimeSeriesPlotlog <- reactive({
      # Generate the plot based on your data and logic
      # Replace the following example with your own plot
      
      sp <- {ggplot(DailyFlows, aes(Date, Value)) +
          geom_line() +
          labs(x = "Date", y = "Streamflow (cfs)") +
          geom_hline(yintercept = quantiles[1], col = "blue", linetype = "dashed") +
          geom_hline(yintercept = quantiles[2], col = "blue", linetype = "dashed", size = 1) +
          geom_hline(yintercept = quantiles[3], col = "blue", linetype = "dashed") +
          geom_hline(yintercept = minflow, col = "blue", linetype = "dashed") +
          geom_hline(yintercept = maxflow, col = "blue", linetype = "dashed") +
          
          xlim(as.Date(input$XLimitslog[1],"%m-%d-%y"),as.Date(input$XLimitslog[2],"%m-%d-%y"))}
      ylim(0, round_any(maxflow, 1000, f = ceiling))
      
      sp + scale_y_log10()
    })
    
    # Render the plot
    output$RawTimeSeriesPlotlog <- renderPlot({
      plot <- RawTimeSeriesPlotlog()
      print(plot)
    })
    
    # Download the plot as a JPEG image
    output$downloadfullstreamflowPlotlog <- downloadHandler(
      filename = function() {
        "fullstreamflowplotlogy.jpg"
      },
      content = function(file) {
        # Save the plot as a JPEG image
        ggsave(file, plot = RawTimeSeriesPlotlog(), device = "jpeg", width = 12, height = 6)
      }
    )
    
    ##################################################################################################Tab4
    
    observe({
      updateSliderInput(
        inputId = "XLimitslowflows",
        min = firstdatestreamflow,
        max = lastdatestreamflow,
        value = c(firstdatestreamflow,lastdatestreamflow)
      )
    })
    
    DailyFlows$Value[DailyFlows$Value<0.00000000000001] <- 0.00000001
    
    fullrecordhistoric7q10 <- compute_frequency_quantile(data = DailyFlows, roll_days = 7, return_period = 10, ignore_missing = TRUE)
    fullrecordhistoric7q10 <- round(fullrecordhistoric7q10, digits = 2)
    
    fullrecordhistoric7q30 <- compute_frequency_quantile(data = DailyFlows, roll_days = 7, return_period = 30, ignore_missing = TRUE)
    fullrecordhistoric7q30 <- round(fullrecordhistoric7q30, digits = 2)
    
    fullrecordhistoric7q2 <- compute_frequency_quantile(data = DailyFlows, roll_days = 7, return_period = 2, ignore_missing = TRUE)
    fullrecordhistoric7q2 <- round(fullrecordhistoric7q2, digits = 2)
    
    tenthpercentile <- quantile(DailyFlows$Value, 0.1, na.rm = TRUE)
    
    output$minflow2 <- renderText({
      paste("The Minimum Flow (dashed blue) is:  ", minflow)
    })
    
    output$tenthpercentileflow <- renderText({
      paste("The 10th Percentile Flow (dashed blue) is:  ", tenthpercentile)
    })
    
    output$fullrecordsevenqtwo <- renderText({
      paste("The Full Record 7Q2 (dashed red) is:  ", fullrecordhistoric7q2)
    })
    
    output$fullrecordsevenqten <- renderText({
      paste("The Full Record 7Q10 (dashed red, thicker) is:  ", fullrecordhistoric7q10)
    })
    
    output$fullrecordsevenqthirty <- renderText({
      paste("The Full Record 7Q30 (dashed red, thickest) is:  ", fullrecordhistoric7q30)
    })
    
    output$thisisjustanotherspace <- renderText({
      paste("---------------------------------")
    })
    
    output$flatline <- renderText({
      paste("Note- the 0 cfs line is given by the solid, transparent red line at the bottom of the graph. If your basin is small, it may approach, or even hit, 0 flow sometimes.")
    })
    
    # Generate the plot
    LowFlowsPlot <- reactive({
      # Generate the plot based on your data and logic
      # Replace the following example with your own plot
      
      ggplot(DailyFlows, aes(Date, Value)) +
        geom_line() +
        labs(x = "Date", y = "Streamflow (cfs)") +
        geom_hline(yintercept = fullrecordhistoric7q2, col = "red", linetype = "dashed") +
        geom_hline(yintercept = fullrecordhistoric7q10, col = "red", linetype = "dashed", size = 1) +
        geom_hline(yintercept = fullrecordhistoric7q30, col = "red", linetype = "dashed", size = 1.25) +
        
        geom_hline(yintercept = minflow, col = "blue", linetype = "dashed", size = 0.5, alpha = 0.5) +
        geom_hline(yintercept = tenthpercentile, col = "blue", linetype = "dashed", size = 0.5, alpha = 0.5) +
        geom_hline(yintercept = 0, col = "red", linetype = "solid", size = 0.5, alpha = 0.25) +
        
        xlim(as.Date(input$XLimitslowflows[1],"%m-%d-%y"),as.Date(input$XLimitslowflows[2],"%m-%d-%y")) +
        ylim(0, tenthpercentile)
    })
    
    # Render the plot
    output$Lowest10Plot <- renderPlot({
      plot <- LowFlowsPlot()
      print(plot)
    })
    
    # Download the plot as a JPEG image
    output$downloadfulllowflowsplot <- downloadHandler(
      filename = function() {
        "lowflowsplot.jpg"
      },
      content = function(file) {
        # Save the plot as a JPEG image
        ggsave(file, plot = LowFlowsPlot(), device = "jpeg", width = 12, height = 6)
      }
    )
    
    ##################################################################################Tab5
    
    annual7daylowflows <- calc_annual_lowflows(data = DailyFlows)
    annual7daylowflows = subset(annual7daylowflows, select = c("Min_7_Day", "Min_7_Day_Date"))
    annual7daylowflows$Min_7_Day_Date <- format(as.Date(annual7daylowflows$Min_7_Day_Date, format="%d/%m/%Y"),"%Y")
    
    MKTest <- MannKendall(annual7daylowflows$Min_7_Day)
    tau <- round(MKTest$tau, digits = 2)
    pvalue <- round(MKTest$sl, digits = 2)
    
    output$mktau <- renderText({
      paste("The Test Statistic Tau is:", tau)
    })
    
    output$mkpvalue <- renderText({
      paste("P-Value for the 2 Sided Mann-Kendall Test:", pvalue)
    })
    
    # Generate the plot
    AnnualLowFlowsPlot <- reactive({
      # Generate the plot based on your data and logic
      # Replace the following example with your own plot
      
      ggplot(annual7daylowflows, aes(x = as.Date(Min_7_Day_Date, format="%Y"), y = Min_7_Day)) +
        geom_point() +
        geom_smooth(method = "loess", color = "blue", se=F, na.rm = TRUE) +
        labs(x = "Year", y = "Annual 7-Day Low Flow (cfs)")
    })
    
    # Render the plot
    output$MannKendallPlot <- renderPlot({
      plot <- AnnualLowFlowsPlot()
      print(plot)
    })
    
    # Download the plot as a JPEG image
    output$AnnualLowFlowDownload <- downloadHandler(
      filename = function() {
        "AnnualLowFlowsPlot.jpg"
      },
      content = function(file) {
        # Save the plot as a JPEG image
        ggsave(file, plot = AnnualLowFlowsPlot(), device = "jpeg", width = 12, height = 6)
      }
    )
    
    
    ################################################################################Tab6
    
    # Plot
    
    # Generate the plot
    interactivesubsetPlot <- reactive({
      
      ggplot(data = DailyFlows, aes(x = Date, y = Value)) +
        geom_line() +
        labs(x = "Date", y = "Streamflow (cfs)") +
        geom_vline(xintercept = input$lowerbound, color = "blue", linetype = "dashed", size = 2) +
        geom_vline(xintercept = input$upperbound, color = "blue", linetype = "dashed", size = 2)
      
    })
    
    # Render the plot
    output$subsetplot <- renderPlot({
      plot <- interactivesubsetPlot()
      print(plot)
    })
    
    observeEvent(input$savesubset, {
      
      output$savedsubset <- renderText({
        paste("Subset Saved!")
      })
      
      savedlowerbound <- input$lowerbound
      savedupperbound <- input$upperbound
      
      SubsetDailyFlows <- DailyFlows[DailyFlows$Date > savedlowerbound & DailyFlows$Date < savedupperbound, ]
      
      output$subsetstreamflowdownload <- downloadHandler(
        filename = function() {
          "subsetstreamflow.csv"
        },
        
        content = function(file) {
          # Write your data generation logic here
          # Replace the following example with your own data
          
          write.csv(SubsetDailyFlows, file, row.names = FALSE)
        }
      )
      
      ##########################################################################Tab6
      
      observe({
        updateSliderInput(
          inputId = "XLimitslowflowssubset",
          min = savedlowerbound,
          max = savedupperbound,
          value = c(savedlowerbound,savedupperbound)
        )
      })
      
      subsethistoric7q10 <- compute_frequency_quantile(data = SubsetDailyFlows, roll_days = 7, return_period = 10, ignore_missing = TRUE)
      subsethistoric7q10 <- round(subsethistoric7q10, digits = 2)
      
      subsethistoric7q30 <- compute_frequency_quantile(data = SubsetDailyFlows, roll_days = 7, return_period = 30, ignore_missing = TRUE)
      subsethistoric7q30 <- round(subsethistoric7q30, digits = 2)
      
      subsethistoric7q2 <- compute_frequency_quantile(data = SubsetDailyFlows, roll_days = 7, return_period = 2, ignore_missing = TRUE)
      subsethistoric7q2 <- round(subsethistoric7q2, digits = 2)
      
      minflowsubset <- min(SubsetDailyFlows$Value, na.rm = TRUE)
      tenthpercentilesubset <- quantile(SubsetDailyFlows$Value, 0.1, na.rm = TRUE)
      
      
      output$minflowsubset <- renderText({
        paste("The Subset Record Minimum Flow (dashed blue) is:  ", minflowsubset)
      })
      
      output$tenthpercentileflowsubset <- renderText({
        paste("The Subset Record 10th Percentile Flow (dashed blue) is:  ", tenthpercentilesubset)
      })
      
      output$fullrecordsevenqtworepeat <- renderText({
        paste("The Full Record 7Q2 from before (transparent) is:  ", fullrecordhistoric7q2)
      })
      
      output$fullrecordsevenqtenrepeat <- renderText({
        paste("The Full Record 7Q10 from before (transparent) is:  ", fullrecordhistoric7q10)
      })
      
      output$fullrecordsevenqthirtyrepeat <- renderText({
        paste("The Full Record 7Q30 from before (transparent) is:  ", fullrecordhistoric7q30)
      })
      
      output$fullrecordsevenqtwosubset <- renderText({
        paste("The Subset Record 7Q2 (red) is:  ", subsethistoric7q2)
      })
      
      output$fullrecordsevenqtensubset <- renderText({
        paste("The Subset Record 7Q10 (red, thicker) is:  ", subsethistoric7q10)
      })
      
      output$fullrecordsevenqthirtysubset <- renderText({
        paste("The Subset Record 7Q30 (red, thickest) is:  ", subsethistoric7q30)
      })
      
      output$flatlinesubset <- renderText({
        paste("Note- Again, the 0 cfs line is given by the solid, transparent red line at the bottom.")
      })
      
      # Generate the plot
      LowFlowsPlotsubset <- reactive({
        # Generate the plot based on your data and logic
        # Replace the following example with your own plot
        
        ggplot(SubsetDailyFlows, aes(Date, Value)) +
          geom_line() +
          labs(x = "Date", y = "Streamflow (cfs)") +
          
          geom_hline(yintercept = minflowsubset, col = "blue", linetype = "dashed", size = 0.5, alpha = 0.5) +
          geom_hline(yintercept = tenthpercentilesubset, col = "blue", linetype = "dashed", size = 0.5, alpha = 0.5) +
          geom_hline(yintercept = 0, col = "red", linetype = "solid", size = 0.5, alpha = 0.25) +
          
          geom_hline(yintercept = fullrecordhistoric7q2, col = "red", linetype = "dashed", alpha = 0.15) +
          geom_hline(yintercept = fullrecordhistoric7q10, col = "red", linetype = "dashed", alpha = 0.15, size = 1) +
          geom_hline(yintercept = fullrecordhistoric7q30, col = "red", linetype = "dashed", alpha = 0.15, size = 1.25) +
          
          geom_hline(yintercept = subsethistoric7q2, col = "red", linetype = "dashed") +
          geom_hline(yintercept = subsethistoric7q10, col = "red", linetype = "dashed", size = 1) +
          geom_hline(yintercept = subsethistoric7q30, col = "red", linetype = "dashed", size = 1.25) +
          
          xlim(as.Date(input$XLimitslowflowssubset[1],"%m-%d-%y"),as.Date(input$XLimitslowflowssubset[2],"%m-%d-%y")) +
          ylim(0, tenthpercentilesubset)
      })
      
      # Render the plot
      output$Lowest10Plotsubset <- renderPlot({
        plot <- LowFlowsPlotsubset()
        print(plot)
      })
      
      # Download the plot as a JPEG image
      output$downloadsubsetlowflowsplot <- downloadHandler(
        filename = function() {
          "subsetlowflowsplot.jpg"
        },
        content = function(file) {
          # Save the plot as a JPEG image
          ggsave(file, plot = LowFlowsPlotsubset(), device = "jpeg", width = 12, height = 6)
        }
      )
      
      ##############################Tab7
      
      observe({
        updateSliderInput(
          inputId = "XLimitslowflowssubsetandscaled",
          min = savedlowerbound,
          max = savedupperbound,
          value = c(savedlowerbound,savedupperbound)
        )
      })
      
      output$gagedrainagearearesult <- renderPrint({
        cat("The drainage area for this streamgage", gage, "is", drainage_area, "square miles.")
      })
      
      observeEvent(input$flowscaling, {
        
        arearatio <- input$ungagedarea/drainage_area
        arearatio <- round(arearatio, digits = 2)
        
        SubsetandScaledDailyFlows <- SubsetDailyFlows
        SubsetandScaledDailyFlows$Value <- SubsetDailyFlows$Value*arearatio
        
        output$scalefactor <- renderText({
          paste("Area/Area Scale Factor is:", arearatio)
        })
        
        output$scalenote <- renderText({
          paste("Note- Ideal scale factors lie in the following range: 0.5 < X < 1.5")
        })
        
        subsetscaled7q2 <- round(subsethistoric7q2*arearatio, digits = 2)
        
        output$scaledfullrecordsevenqtwosubset <- renderText({
          paste("The Subset & Scaled 7Q2 (red) is:  ", subsetscaled7q2)
        })
        
        subsetscaled7q10 <- round(subsethistoric7q10*arearatio, digits = 2)
        
        output$scaledfullrecordsevenqtensubset <- renderText({
          paste("The Subset & Scaled 7Q10 (red, thick) is:  ", subsetscaled7q10)
        })
        
        subsetscaled7q30 <- round(subsethistoric7q30*arearatio, digits = 2)
        
        output$scaledfullrecordsevenqthirtysubset <- renderText({
          paste("The Subset & Scaled 7Q30 (red, thickest) is:  ", subsetscaled7q30)
        })
        
        minflowsubsetandscaled <- min(SubsetandScaledDailyFlows$Value, na.rm = TRUE)
        tenthpercentilesubsetandscaled <- as.numeric(quantile(SubsetandScaledDailyFlows$Value, 0.1, na.rm = TRUE))
        
        # Generate the plot
        LowFlowsPlotsubsetandscaled <- reactive({
          # Generate the plot based on your data and logic
          # Replace the following example with your own plot
          
          ggplot(SubsetandScaledDailyFlows, aes(Date, Value)) +
            geom_line(alpha = 0.5) +
            labs(x = "Date", y = "Streamflow (cfs)") +
            
            geom_hline(yintercept = minflowsubsetandscaled, col = "blue", linetype = "dashed", size = 0.5, alpha = 0.5) +
            geom_hline(yintercept = tenthpercentilesubsetandscaled, col = "blue", linetype = "dashed", size = 0.5, alpha = 0.5) +
            geom_hline(yintercept = 0, col = "red", linetype = "solid", size = 0.5, alpha = 0.25) +
            
            geom_hline(yintercept = subsetscaled7q2, col = "red", linetype = "dashed") +
            geom_hline(yintercept = subsetscaled7q10, col = "red", linetype = "dashed", size = 1) +
            geom_hline(yintercept = subsetscaled7q30, col = "red", linetype = "dashed", size = 1.25) +
            
            xlim(as.Date(input$XLimitslowflowssubsetandscaled[1],"%m-%d-%y"),as.Date(input$XLimitslowflowssubsetandscaled[2],"%m-%d-%y")) +
            ylim(0, tenthpercentilesubsetandscaled)
        })
        
        # Render the plot
        output$Lowest10Plotsubsetandscaled <- renderPlot({
          plot <- LowFlowsPlotsubsetandscaled()
          print(plot)
        })
        
        # Download the plot as a JPEG image
        output$flowscaledplotdownload <- downloadHandler(
          filename = function() {
            "flowscaledlowflowsplot.jpg"
          },
          content = function(file) {
            # Save the plot as a JPEG image
            ggsave(file, plot = LowFlowsPlotsubsetandscaled(), device = "jpeg", width = 12, height = 6)
          }
        )
        
        
      })#Observe Flow Scaling Event
      
      
    })#Observe Subset Event
    
    
  })#Download NWIS Data Event
  
  #####################################################################################################################
  ###Start Ungaged Portion
  
  ####Generate and output runoff prediction
  output$sevenqtenpaper <- renderImage({
    # Get the path to the image file
    img_path <- "7q10.png"
    
    # Return the image object
    list(src = img_path,
         width = "60%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
  ######################################################################################################################
  
  ######################################################################################################################
  #Code for Tab1 of Ungaged
  
  #There was originally code here but it ended up not being needed lol
  
  ###################################################################################################################################
  #Code for Tab2
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -71.5, lat = 42.5, zoom = 5) %>%
      addMarkers(lng = input$lon, lat = input$lat) # add marker at user input location
  })
  
  ss7q10 <- NULL
  ssarea <- NULL
  sselevation <- NULL
  ssslope <- NULL
  ssforest <- NULL
  sswetland <- NULL
  
  observeEvent(input$calculate, {
    
    setTimeout(100000)
    
    withProgress(message = "Contacting StreamStats...", value = 0, {
      ws <- delineateWatershed(xlocation = input$lon, ylocation = input$lat, crs = 4326, includeparameters = "true")
      output$message <- renderText("Website contacted successfully!")
    })
    
    #Extract landparameters from watershed object and display
    parameters <- ws$parameters
    
    state <- substr(ws$workspaceID,start=1,stop=2)
    #MA    
    
    if (state == "MA")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEV", parameters$code)
      slope10index <- match("BSLDEM10M", parameters$code)
      slope250index <- match("BSLDEM250", parameters$code)
      forestindex <- match("FOREST", parameters$code)
      wetlandindex <- match("WETLAND", parameters$code)
      
      drstindex <- match("DRFTPERSTR", parameters$code)
      maregindex <- match("MAREGION", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sselevation <- parameters$value[elevationindex]
      ss10slope <- parameters$value[slope10index]
      ss250slope <- parameters$value[slope250index]
      ssslope <- ss10slope
      ssforest <- parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      
      #ssdrst not returning correctly
      ssdrst <- parameters$value[drstindex]
      ssregion <- parameters$value[maregindex]
      
      #Something wrong with this equation
      ss7q10 <- (0.080)*(ssarea^1.170)*(ss250slope^0.514)*((ssdrst + 0.1)^1.180)*(10^(0.260*ssregion))
      ss7q10 <- round(ss7q10, digits = 2)
    }
    
    #RI
    if(state == "RI")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEV", parameters$code)
      slopeindex <- match("CSL10_85", parameters$code)
      forestindex <- match("FOREST", parameters$code)
      wetlandindex <- match("WETLAND", parameters$code)
      
      streamdensityindex <- match("STRDENED", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sselevation <- parameters$value[elevationindex]
      ssslopeftpermi <- parameters$value[slopeindex]
      ssslope <- (ssslopeftpermi/5280*100)
      ssslope <- round(ssslope, digits = 2)
      ssforest <- parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      
      #ssdrst not returning correctly
      streamdensity <- parameters$value[streamdensityindex]
      
      #Something wrong with this equation
      ss7q10 <- (0.0311)*(ssarea^1.71)*(streamdensity^-2.53)
      ss7q10 <- round(ss7q10, digits = 2)
    }
    #CT
    if(state == "CT")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEV", parameters$code)
      slopeindex <- match("CSL10_85", parameters$code)
      forestindex <- match("LC11DEV", parameters$code)
      wetlandindex <- match("WETLAND", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sselevation <- parameters$value[elevationindex]
      ssslopeftpermi <- parameters$value[slopeindex]
      ssslope <- (ssslopeftpermi/5280*100)
      ssslope <- round(ssslope, digits = 2)
      ssforest <- parameters$value[forestindex]
      ssforest <- 100-ssforest
      sswetland <- parameters$value[wetlandindex]
    }
    
    #VT
    if(state == "VT")
    {
      areaindex <- match("DRNAREA", parameters$code)
      wetlandindex <- match("LC06STOR", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sswetland <- parameters$value[wetlandindex]
      
    }
    
    #NH
    if(state == "NH")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEVMAX", parameters$code)
      slopeindex <- match("BSLDEM30M", parameters$code)
      forestindex <- match("LC11DEV", parameters$code)
      wetlandindex <- match("WETLAND", parameters$code)
      
      tempindex <- match("TEMP", parameters$code)
      precindex <- match("PREG_06_10", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      ssslope <- parameters$value[slopeindex]
      
      #maxelev <- parameters$value[elevationindex]
      #estchangeinelev <- ssarea^(1/2)*(ssslope/100)*5280
      #sselevation <- maxelev - (0.5)*estchangeinelev
      
      ssforest <- 100-parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      
      #ssdrst not returning correctly
      sstemp <- parameters$value[tempindex]
      ssprec <- parameters$value[precindex]
      
      #StreamStats
      ss7q10 <- (1.27688)*(10^5.33462)*(ssarea^1.39481)*(sstemp^-7.67405)*(ssprec^4.16826)
      ss7q10 <- round(ss7q10, digits = 2)
      
      #Dingman
      #ding7q10 <- (-2.22) + (1.25*log10(ssarea)) + (4*10^-4)*sselevation + 1.49*D
    }
    
    #ME
    if(state == "ME")
    {
      areaindex <- match("DRNAREA", parameters$code)
      elevationindex <- match("ELEV", parameters$code)
      slopeindex <- match("BSLDEM10M", parameters$code)
      forestindex <- match("LC11DEV", parameters$code)
      wetlandindex <- match("STORNWI", parameters$code)
      
      sandindex <- match("SANDGRAVAF", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      sselev <- parameters$value[elevationindex]
      ssslope <- parameters$value[slopeindex]
      ssforest <- 100-parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
      
      #ssdrst not returning correctly
      sssand <- parameters$value[sandindex]
      
      #Something wrong with this equation
      ss7q10 <- (0.023)*(ssarea^1.173)*(10^(2.54*sssand))
      ss7q10 <- round(ss7q10, digits = 2)
    }
    
    #NY
    if(state == "NY")
    {
      areaindex <- match("DRNAREA", parameters$code)
      slopeindex <- match("BSLOPCM", parameters$code)
      forestindex <- match("FOREST", parameters$code)
      wetlandindex <- match("STORAGE", parameters$code)
      
      ssarea <- parameters$value[areaindex]
      
      ssslopeftpermi <- parameters$value[slopeindex]
      ssslope <- (ssslopeftpermi/5280*100)
      ssslope <- round(ssslope, digits = 2)
      
      ssforest <- parameters$value[forestindex]
      sswetland <- parameters$value[wetlandindex]
    }
    
    
    if (is.null(ssarea) == TRUE)
    {
      
      output$ssresult <- renderText({
        paste("We are sorry- contacting StreamStats was unsuccessful.")
      })
      
      output$arearesult <- renderText({
        paste("It is likely because your lat/long did not land on a valid StreamStats pixel. Please return to Step 1 and verify that your lat and long are valid.")
      })
      
      output$continue <- renderText({
        paste("If you are sure your point is valid, please refresh the page and try again. For some reason, StreamStats API only works ~75% of the time on the first try.")
      })
      
    }
    
    
    else
    {
      
      output$ssresult <- renderText({
        paste("Contacting StreamStats was a success!")
      })
      
      output$arearesult <- renderText({
        paste("Area of your watershed (sq mi) is:", ssarea)
      })
      
      output$continue <- renderText({
        paste("If this is correct, please proceed to Step 3.")
      })
      
      
    }
    
    ###################################################################################################################################
    #Code for Tab 3
    
    output$watershedarea <- renderText({
      paste("Area (sq mi):", ssarea)
    })
    
    output$watershedelevation <- renderText({
      paste("Elevation (ft):", sselevation)
    })
    
    output$watershedslope <- renderText({
      paste("Slope (%):", ssslope)
    })
    
    output$watershedforest <- renderText({
      paste("Forest %:", ssforest)
    })
    
    output$watershedwetland <- renderText({
      paste("Wetland %:", sswetland)
    })
    
    output$streamstats7q10 <- renderText({
      paste("StreamStats' 7Q10 (cfs):", ss7q10)
    })
    
    output$watershed <- renderLeaflet({
      leafletWatershed(ws)
    }) 
    
    ###################################################################################################################################    
    
    output$watershedarearepeat <- renderText({
      paste("Area (sq mi):", ssarea)
    })
    
    output$watershedelevationrepeat <- renderText({
      paste("Elevation (ft):", sselevation)
    })
    
    output$watershedsloperepeat <- renderText({
      paste("Slope (%):", ssslope)
    })
    
    output$watershedforestrepeat <- renderText({
      paste("Forest %:", ssforest)
    })
    
    output$watershedwetlandrepeat <- renderText({
      paste("Wetland %:", sswetland)
    })
    
    output$streamstats7q10repeat <- renderText({
      paste("StreamStats' 7Q10 (cfs):", ss7q10)
    })
    
  })##observe event  
  
  ungaged7q10precip <- 8
  ungaged7q10temp <- 30
  
  observeEvent(input$regressions, {
    
    output$MLRfull_output <- renderText({
      intercept <- 0
      areapiece <- input$ungaged7q10area * 0.0579833
      wetlandpiece <- input$ungaged7q10wetland * -0.0618991
      forestpiece <- input$ungaged7q10forest * 0.0048811
      elevationpiece <- input$ungaged7q10elevation * 0.0014461
      slopepiece <- input$ungaged7q10slope * -0.2804801
      
      precippiece <- ungaged7q10precip * 0.3421899
      temppiece <- ungaged7q10temp * -0.0466718
      
      result <- intercept + areapiece + wetlandpiece + forestpiece + elevationpiece + slopepiece + precippiece + temppiece
      result <- round(result, digits = 2)
      paste("The full record, multiple linear regression estimated 7Q10 (cfs) is:", result)
    })
    
    output$LTLRfull_output <- renderText({
      intercept <- 4.27157
      areapiece <- log10(input$ungaged7q10area) * 1.31308
      wetlandpiece <- log10(input$ungaged7q10wetland) * -0.02036
      forestpiece <- log10(input$ungaged7q10forest) * 0.22437
      elevationpiece <- log10(input$ungaged7q10elevation) * -0.11573
      slopepiece <- log10(input$ungaged7q10slope) * -0.19413
      
      precippiece <- log10(ungaged7q10precip) * 0.31049
      temppiece <- log10(ungaged7q10temp) * -4.37462
      
      logresult <- intercept + areapiece + wetlandpiece + forestpiece + elevationpiece + slopepiece + precippiece + temppiece
      result <- 10^logresult
      result <- round(result, digits = 2)
      paste("The full record, logarithmic regression (StreamStats methodology) estimated 7Q10 (cfs) is:", result)
    })
    
    output$RFfull_output <- renderText({
      load("randomforest.RData")
      
      df <- data.frame(matrix(ncol = 7, nrow = 1))
      colnames(df)<-c("Area","forestper","wetlandper","avgelev","slopeper","mincumprec","maxavghightemp")
      df$Area <- input$ungaged7q10area
      df$forestper <- input$ungaged7q10forest
      df$wetlandper <- input$ungaged7q10wetland
      df$avgelev <- input$ungaged7q10elevation
      df$slopeper <- input$ungaged7q10slope
      df$mincumprec <- ungaged7q10precip
      df$maxavghightemp <- ungaged7q10temp
      
      prediction <- predict(rfmodel7,df)
      result <- round(prediction, digits = 2)
      paste("The full record, random forest machine learning estimated 7Q10 (cfs) is:", result)
    })
    ##
    
    #30YearRecords    
    output$MLR30_output <- renderText({
      intercept <- 0
      areapiece <- input$ungaged7q10area * 0.0605044
      wetlandpiece <- input$ungaged7q10wetland * -0.0625036
      forestpiece <- input$ungaged7q10forest * 0.0137480
      elevationpiece <- input$ungaged7q10elevation * 0.0015859
      slopepiece <- input$ungaged7q10slope * -0.2683481
      
      precippiece <- ungaged7q10precip * 0.3764543
      temppiece <- ungaged7q10temp * -0.0903300
      
      result <- intercept + areapiece + wetlandpiece + forestpiece + elevationpiece + slopepiece + precippiece + temppiece
      result <- round(result, digits = 2)
      paste("The last 30-year record, multiple linear regression estimated 7Q10 (cfs) is:", result)
    })
    
    output$LTLR30_output <- renderText({
      intercept <- 7.89315
      areapiece <- log10(input$ungaged7q10area) * 1.34350
      wetlandpiece <- log10(input$ungaged7q10wetland) * -0.01075
      forestpiece <- log10(input$ungaged7q10forest) * 0.24179
      elevationpiece <- log10(input$ungaged7q10elevation) * -0.14117
      slopepiece <- log10(input$ungaged7q10slope) * -0.20875
      
      precippiece <- log10(ungaged7q10precip) * 0.31222
      temppiece <- log10(ungaged7q10temp) * -6.86155
      
      logresult <- intercept + areapiece + wetlandpiece + forestpiece + elevationpiece + slopepiece + precippiece + temppiece
      result <- 10^logresult
      result <- round(result, digits = 2)
      paste("The last 30-year record, logarithmic regression (StreamStats methodology) estimated 7Q10 (cfs) is:", result)
    })
    
    output$RF30_output <- renderText({
      load("randomforestrecent.RData")
      
      df <- data.frame(matrix(ncol = 7, nrow = 1))
      colnames(df)<-c("Area","forestper","wetlandper","avgelev","slopeper","mincumprec","maxavghightemp")
      df$Area <- input$ungaged7q10area
      df$forestper <- input$ungaged7q10forest
      df$wetlandper <- input$ungaged7q10wetland
      df$avgelev <- input$ungaged7q10elevation
      df$slopeper <- input$ungaged7q10slope
      df$mincumprec <- ungaged7q10precip
      df$maxavghightemp <- ungaged7q10temp
      
      prediction <- predict(rfmodel7,df)
      result <- round(prediction, digits = 2)
      paste("The last 30-year record, random forest machine learning estimated 7Q10 (cfs) is:", result)
    })
    
    output$small_basins <- renderText({
      paste("For basins under 70 miles^2, we recommend logarithmic regression or StreamStats.")
    })
    ##
    
    output$large_basins <- renderText({
      paste("For basins over 70 miles^2, we recommend multiple linear regression or machine learning.")
    })
    
  })
  
  #########################################################################
  ###Start Applying Climate Change Portion
  
  ####Generate and output runoff prediction
  output$nchrppicture <- renderImage({
    # Get the path to the image file
    img_path <- "nchrp.png"
    
    # Return the image object
    list(src = img_path,
         width = "55%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
  #########################################################################################################################################Tab 5
  
  output$LTLRprojectedvalue <- renderText({
    
    currentestimatedsevenqten <- input$estimatedvalue
    inlogspace <- log10(currentestimatedsevenqten)
    
    # ungaged7q10precip <- 8
    # ungaged7q10temp <- 30
    
    correctunitsprecip <- ungaged7q10precip*(input$precipslider/100)
    correctunitstemp <- input$tempslider*(5/9)
    
    averageprecip <- ungaged7q10precip
    averagetemp <- ungaged7q10temp
    
    precippiecechange <- log10(averageprecip+correctunitsprecip) - log10(averageprecip)
    temppiecechange <- log10(averagetemp+correctunitstemp) - log10(averagetemp)
    
    precipchange <- precippiecechange * 0.31049
    tempchange <- temppiecechange * -4.37462
    
    logresult <- inlogspace + precipchange + tempchange
    result <- 10^logresult
    
    result <- round(result, digits = 2)
    paste("", result)
  })
  
  ########################################################################Documentation Tab
  output$downloadBtn <- downloadHandler(
    filename = "documentation.docx",
    content = function(file) {
      file.copy("documentation.docx", file)
    }
  )
  ####Generate and output runoff prediction
  output$fundingorgs <- renderImage({
    # Get the path to the image file
    img_path <- "FundingandPartnerOrganizations.png"
    
    # Return the image object
    list(src = img_path,
         width = "55%",
         height = "auto",
         alt = "Image")
  },
  deleteFile = FALSE) # Keep the image file after the app session ends
  
}

###################################################################################################################################
#Creates Shiny object and runs app
shinyApp(ui = ui, server = server)

