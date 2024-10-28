
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