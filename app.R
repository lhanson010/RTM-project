#Input:

#User
#Date <- No
#Asset ID
#Start Year
#End year
#Mean warmest location temp
#Mean coolest location temp
#USL
#LSL

#Data: One asset at a time, multiple files




#Output:
#Table containing
#USL
#LSL
#UL and formula
#LL and formula
#Monitoring probe mean
#Temperature standard deviation and 2 standard deviations
#Mean - 2SD
#Mean + 2SD
#or confidence interval?

#Plot
#Trend line
#Line for Mean +/- 2SD
#Line for UL, LL
#Line for USL, LSL
#Even distribution of date/times on x axis
#Temp on y axis (min - 1, max + 1 or USL/LSL + 2)
#Legend for each line
#Title with "asset_id" + "start year, end year"

#Space for additional comments

#"Generated (date) by (user)"

library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)
library(data.table)
library(ggplot2)

# pull hourly function

pull_hourly <- function(df) {
  #, asset_id){
  # show progress
  # look for the asset ID in any of the list names and create a new list, "right frames", that has only the data you want
  # Make sure dimensions are correct
  #right_frames <- big_list[sapply(names(big_list), grepl, pattern = asset_id)] %>%
  # lapply(function(x) x[,1:2])
  # combine the separately pulled data frames into one, ensuring they are in order
  # combined_data <- bind_rows(big_list) %>% arrange(t_stamp)
  print(head(df))
  tstamps <- as.POSIXct(df$t_stamp)
  #date_time_parse(combined_data[,1],zone = "America/New_York",
  #format = "%Y-%m-%d %H:%M:%S")
  #identify where differences are negative and add 1; this represents start of standard time
  end_dst <- which(diff(tstamps) < 0) + 1
  if (length(end_dst) > 0) {
    print(end_dst)
    #identify problematic hour
    problem_hour <- round(tstamps[end_dst], "hours")
    print(problem_hour)
    #work on time after DST ends (will only be encountered once a year)
    #could handle edge case where rtm starts on the day of DST, but not for now
    std_time_times <- tstamps[end_dst:length(tstamps)]
    #of the times following the end of dst, which match the duplicate hour. Then add
    #back the dst days so indexing is correct when we remove these indeces
    #floor so everything up to 1:59 duplicate is removed
    rm_indeces <- which(lubridate::floor_date(std_time_times, unit = "hour") == problem_hour) +
      end_dst - 1
    #remove duplicate DST entries
    tstamps <- tstamps[-rm_indeces]
  }
  #create key of hours to round to
  hours_key <- seq(
    from = round(tstamps[1], "hours"),
    to = round(tstamps[length(tstamps)], "hours"),
    by = 3600
  )
  # findInterval to identify which intervals between recording timestamps contain HH:00 values
  new_index <- findInterval(hours_key, tstamps)
  
  # create and return trimmed data
  hourly_data <- df[new_index, ]
  return(hourly_data)
}


options(shiny.maxRequestSize = 500 * 1024 ^ 2)

ui <- fluidPage(
  titlePanel("Routine Trend Monitoring (RTM) tool"),
  sidebarLayout(
    sidebarPanel(
      textInput("user", "Enter your full name"),
      textInput("asset_id", "Enter the unit asset ID"),
      selectInput(
        "val_type",
        "Was the most recent validation the initial validation or a revalidation?",
        choices = c("Initial", "Revalidation")
      ),
      selectInput(
        "moved_probe",
        "Was the monitoring probe moved following validation?",
        choices = c("Yes", "No"),
      ),
      uiOutput("moved_probe"),
      numericInput(
        "mean_protocol",
        "Mean temperature from the monitoring probe from the most recent protocol",
        value = 1
      ),
      numericInput(
        "mean_warm",
        "Mean Warmest Location Average Temperature from the most recent protocol",
        value = 1
      ),
      numericInput(
        "mean_cold",
        "Mean Coolest Location Average Temperature from the most recent protocol",
        value = 0
      ),
      numericInput("usl", "Upper Specification Limit", value = 0),
      numericInput("lsl", "Lower Specification Limit", value = 0),
      fileInput(
        "upload",
        NULL,
        buttonLabel = "Upload Trend Data",
        multiple = TRUE,
        accept = ".csv"
      ),
      #c(".xlsx", ".xls")),
      actionButton("show_data", "Analyze Data"),
      downloadButton("report", "Download Data (zip file)")
    ),
    mainPanel(
      tableOutput("parameters1"),
      tableOutput("parameters2"),
      plotOutput("plot1", width = "100%"),
      plotOutput("plot2", width = "100%")
    )
  )
)
server <- function(input, output, session) {
  output$moved_probe <- renderUI({
    req(input$moved_probe)
    if (input$moved_probe == "Yes") {
      dateInput("move_date", "Select a date", value = Sys.Date())
      
    }
  })
  
  # temp var to store data
  cached_data <- reactiveValues()
  
################ Upload data #########################
  
  observeEvent(input$upload, { 
    req(input$upload)
    
    
    file_amount <- nrow(input$upload)
    processed_data <- list()
    for (i in 1:file_amount) {
      csv_data <- fread(input$upload$datapath[i])
      processed_data[[i]] <- pull_hourly(csv_data)
      rm(csv_data)
      gc()
    }
    cached_data$data <- bind_rows(processed_data) %>% arrange(t_stamp)
  })
  
############### Process Data ###########################  
  
  observeEvent(input$show_data, {
    req(input$asset_id)
    req(input$mean_protocol)
    req(input$mean_cold)
    req(input$mean_warm)
    req(input$lsl)
    req(input$usl)
    req(input$val_type)
    req(input$moved_probe)
    
    mv_probe <- (input$moved_probe == "Yes")
    
    reval <- (input$val_type == "Revalidation")
    
    #data_list <- lapply(input$upload$datapath, read_excel)
    print("test")
    hourly <- cached_data$data
    print("test")
    names(hourly) <- c("t_stamp", "temp")
    print(head(hourly))
    #cached_data$data <- combined_data
    
    cached_data$start_date <- format(hourly$t_stamp[1], "%d-%b-%Y")
    cached_data$end_date <- format(hourly$t_stamp[nrow(hourly)], "%d-%b-%Y")
    cached_data$move_date <- NULL
    
    if (mv_probe) {
      req(input$move_date)
      mv_date <- input$move_date
      cached_data$move_date <- format(mv_date, "%d-%b-%Y")
    }
    
    
    ######## TABLE AND PLOT CREATION + EVALUATION ######################################
    OOS <- FALSE
    risk <- FALSE
    if (mv_probe & reval) {
      
      ########## Handle first part of data ###############################################
      print("butt")
      seg_1 <- hourly %>% filter(t_stamp < as.POSIXct(mv_date))
      print(nrow(seg_1))
      
      # in seg 1, the UCL and LCL are (USL - (mean_warm - mean_protocol)) and (LSL + (mean_protocol - mean_cool))
      UCL <- input$usl - (input$mean_warm - input$mean_protocol)
      LCL <- input$lsl + (input$mean_protocol - input$mean_cold)
      
      ##plot 1 data frame
      #maybe bootstrap here
      plot_frame <- seg_1 %>% mutate(
        UCL = UCL,
        LCL = LCL,
        USL = input$usl,
        LSL = input$lsl,
        UStL = quantile(temp, 0.975),
        LStL = quantile(temp, 0.025),
        total_mean = mean(temp)
      )
      
      print(names(plot_frame))
      rm(seg_1)
      gc()
      ##plot 1 object
      print("plot beg")
      cached_data$p1 <- ggplot(data = plot_frame, aes(
        x = t_stamp,
        ymax = 10,
        ymin = 0
      )) +
        geom_line(aes(
          y = temp, 
          color = "Temperature",
          linetype = "Temperature")) +
        geom_line(aes(
          y = USL,
          color = "Specification Limits",
          linetype = "Specification Limits"
        )) +
        geom_line(aes(
          y = LSL,
          color = "Specification Limits",
          linetype = "Specification Limits"
        )) +
        geom_line(aes(
          y = UCL,
          color = "Control Limits",
          linetype = "Control Limits"
        )) +
        geom_line(aes(
          y = LCL,
          color = "Control Limits",
          linetype = "Control Limits"
        )) +
        geom_line(aes(
          y = UStL,
          color = "Statistical Limits",
          linetype = "Statistical Limits"
        )) +
        geom_line(aes(
          y = LStL,
          color = "Statistical Limits",
          linetype = "Statistical Limits"
        )) +
        geom_line(aes(
          y = total_mean,
          color = "Mean",
          linetype = "Mean"
        )) +
        scale_color_manual(name = "Legend", values = c(
          "Temperature" = "red",  # Example color (default ggplot2 palette)
          "Specification Limits" = "magenta2",
          "Control Limits" = "orange",
          "Statistical Limits" = "blue",
          "Mean" = "black"
        )) +
        scale_linetype_manual(name = "Legend", values = c(
          "Temperature" = "solid",
          "Specification Limits" = "solid",
          "Control Limits" = "dotted",
          "Statistical Limits" = "dashed",
          "Mean" = "solid"
        )) +
        labs(
          title = paste0("RTM-", input$asset_id, " ", format(plot_frame$t_stamp[1], "%d-%b-%Y"), "-", format(mv_date, "%d-%b-%Y")),
          x = "Date",
          y = "Hourly Temperature (\u00B0C)"
        ) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y") +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))
      print("plot end")
      if (sum(plot_frame$temp > UCL) > 0 |
          sum(plot_frame$temp < LCL) > 0) {
        OOS <- TRUE
      }
      print(OOS)
      if (plot_frame$UStL[1] > UCL | plot_frame$LStL[1] < LCL) {
        risk <- TRUE
      }
      print(risk)
      ##renderTable object
      cached_data$table_1 <- data.frame(
        Parameter = c(
          "Asset ID",
          "Start Date",
          "End Date",
          "Operating Range",
          "Chamber Mean",
          "Upper Control Limit",
          "Lower Control Limit",
          "Upper Statistical Limit",
          "Lower Statistical Limit",
          "Investigation Required?",
          "Risk of OOC?",
          "Date Generated"
        ),
        Value = c(
          input$asset_id,
          format(plot_frame$t_stamp[1], "%d-%b-%Y"),
          format(plot_frame$t_stamp[nrow(plot_frame)], "%d-%b-%Y"),
          paste0(input$lsl, "\u00B0C", " - ", input$usl, "\u00B0C"),
          paste0(round(plot_frame$total_mean[1], 1), "\u00B0C"),
          paste0(round(plot_frame$UCL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$LCL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$UStL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$LStL[1], 1), "\u00B0C"),
          ifelse(OOS, "Yes", "No"),
          ifelse(risk, "Yes", "No"),
          format(Sys.Date(), "%d-%b-%Y")
        )
      )
      
      rm(plot_frame)
      gc()
      
      ####################### Plot segment 2 after moving ####################################
      OOS <- FALSE
      risk <- FALSE
      seg_2 <- hourly %>% filter(t_stamp >= as.POSIXct(mv_date))
      print(head(hourly))
      print(mv_date)
      print(head(seg_2))
      initial_mean <- mean(head(seg_2$temp, 100))
      UCL <- max(input$usl - (input$mean_warm - initial_mean), input$usl)
      LCL <- min(input$lsl + (initial_mean - input$mean_cold), input$lsl)
      plot_frame <- seg_2 %>% mutate(
        UCL = UCL,
        LCL = LCL,
        USL = input$usl,
        LSL = input$lsl,
        UStL = quantile(temp, 0.975),
        LStL = quantile(temp, 0.025),
        total_mean = mean(temp)
      )
      print(head(plot_frame))
      rm(seg_2)
      gc()
      ##plot 2 object
      cached_data$p2 <- ggplot(data = plot_frame, aes(
        x = t_stamp,
        ymax = 10,
        ymin = 0
      )) +
        geom_line(aes(
          y = temp, 
          color = "Temperature",
          linetype = "Temperature")) +
        geom_line(aes(
          y = USL,
          color = "Specification Limits",
          linetype = "Specification Limits"
        )) +
        geom_line(aes(
          y = LSL,
          color = "Specification Limits",
          linetype = "Specification Limits"
        )) +
        geom_line(aes(
          y = UCL,
          color = "Control Limits",
          linetype = "Control Limits"
        )) +
        geom_line(aes(
          y = LCL,
          color = "Control Limits",
          linetype = "Control Limits"
        )) +
        geom_line(aes(
          y = UStL,
          color = "Statistical Limits",
          linetype = "Statistical Limits"
        )) +
        geom_line(aes(
          y = LStL,
          color = "Statistical Limits",
          linetype = "Statistical Limits"
        )) +
        geom_line(aes(
          y = total_mean,
          color = "Mean",
          linetype = "Mean"
        )) +
        scale_color_manual(name = "Legend", values = c(
          "Temperature" = "red",  # Example color (default ggplot2 palette)
          "Specification Limits" = "magenta2",
          "Control Limits" = "orange",
          "Statistical Limits" = "blue",
          "Mean" = "black"
        )) +
        scale_linetype_manual(name = "Legend", values = c(
          "Temperature" = "solid",
          "Specification Limits" = "solid",
          "Control Limits" = "dotted",
          "Statistical Limits" = "dashed",
          "Mean" = "solid"
        )) +
        labs(
          title = paste0(input$asset_id, " RTM ", format(plot_frame$t_stamp[1], "%d-%b-%Y"), "-", format(plot_frame$t_stamp[nrow(plot_frame)], "%d-%b-%Y")),
          x = "Date",
          y = "Hourly Temperature (\u00B0C)"
        ) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y") +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))
      
      print("OOS test")
      if (sum(plot_frame$temp > UCL) > 0 |
          sum(plot_frame$temp < LCL) > 0) {
        OOS <- TRUE
      }
      print(OOS)
      print(c(plot_frame$UStL[1],UCL, plot_frame$LStL[1], LCL))
      if (plot_frame$UStL[1] > UCL | plot_frame$LStL[1] < LCL) {
        risk <- TRUE
      }
      print(risk)
      ##renderTable object
      cached_data$table_2 <- data.frame(
        Parameter = c(
          "Asset ID",
          "Start Date",
          "End Date",
          "Operating Range",
          "Chamber Mean",
          "Upper Control Limit",
          "Lower Control Limit",
          "Upper Statistical Limit",
          "Lower Statistical Limit",
          "Investigation Required?",
          "Risk of OOC?",
          "Date Generated"
        ),
        Value = c(
          input$asset_id,
          format(plot_frame$t_stamp[1], "%d-%b-%Y"),
          format(plot_frame$t_stamp[nrow(plot_frame)], "%d-%b-%Y"),
          paste0(input$lsl, "\u00B0C", " - ", input$usl, "\u00B0C"),
          paste0(round(plot_frame$total_mean[1], 1), "\u00B0C"),
          paste0(round(plot_frame$UCL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$LCL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$UStL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$LStL[1], 1), "\u00B0C"),
          ifelse(OOS, "Yes", "No"),
          ifelse(risk, "Yes", "No"),
          format(Sys.Date(), "%d-%b-%Y")
        )
      )
      
      ################ Handle initial validation and reval w/o probe movement the same #######
      
    } else {
      cached_data$p1 <- NULL
      cached_data$table_1 <- NULL
      initial_mean <- mean(head(hourly$temp, 100))
      UCL <- max(input$usl - (input$mean_warm - initial_mean), input$usl)
      LCL <- min(input$lsl + (initial_mean - input$mean_cold), input$lsl)
      plot_frame <- hourly %>% mutate(
        UCL = UCL,
        LCL = LCL,
        USL = input$usl,
        LSL = input$lsl,
        UStL = quantile(temp, 0.975),
        LStL = quantile(temp, 0.025),
        total_mean = mean(temp)
      )
      ##plot 1 object
      cached_data$p2 <- ggplot(data = plot_frame, aes(
        x = t_stamp,
        ymax = 10,
        ymin = 0
      )) +
        geom_line(aes(
          y = temp, 
          color = "Temperature",
          linetype = "Temperature")) +
        geom_line(aes(
          y = USL,
          color = "Specification Limits",
          linetype = "Specification Limits"
        )) +
        geom_line(aes(
          y = LSL,
          color = "Specification Limits",
          linetype = "Specification Limits"
        )) +
        geom_line(aes(
          y = UCL,
          color = "Control Limits",
          linetype = "Control Limits"
        )) +
        geom_line(aes(
          y = LCL,
          color = "Control Limits",
          linetype = "Control Limits"
        )) +
        geom_line(aes(
          y = UStL,
          color = "Statistical Limits",
          linetype = "Statistical Limits"
        )) +
        geom_line(aes(
          y = LStL,
          color = "Statistical Limits",
          linetype = "Statistical Limits"
        )) +
        geom_line(aes(
          y = total_mean,
          color = "Mean",
          linetype = "Mean"
        )) +
        scale_color_manual(name = "Legend", values = c(
          "Temperature" = "red",  # Example color (default ggplot2 palette)
          "Specification Limits" = "magenta2",
          "Control Limits" = "orange",
          "Statistical Limits" = "blue",
          "Mean" = "black"
        )) +
        scale_linetype_manual(name = "Legend", values = c(
          "Temperature" = "solid",
          "Specification Limits" = "solid",
          "Control Limits" = "dotted",
          "Statistical Limits" = "dashed",
          "Mean" = "solid"
        )) +
        labs(
          title = paste0(input$asset_id," RTM ", format(plot_frame$t_stamp[1], "%d-%b-%Y"), "-", format(plot_frame$t_stamp[nrow(plot_frame)], "%d-%b-%Y")),
          x = "Date",
          y = "Hourly Temperature (\u00B0C)"
        ) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y") +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))
      
      if (sum(plot_frame$temp > UCL) > 0 |
          sum(plot_frame$temp < LCL) > 0) {
        OOS <- TRUE
      }
      if (plot_frame$UStL[1] > UCL | plot_frame$LStL[1] < LCL) {
        risk <- TRUE
      }
      ##renderTable object
      cached_data$table_2 <- data.frame(
        Parameter = c(
          "Asset ID",
          "Start Date",
          "End Date",
          "Operating Range",
          "Chamber Mean",
          "Upper Control Limit",
          "Lower Control Limit",
          "Upper Statistical Limit",
          "Lower Statistical Limit",
          "Investigation Required?",
          "Risk of OOC?",
          "Date Generated"
        ),
        Value = c(
          input$asset_id,
          format(plot_frame$t_stamp[1], "%d-%b-%Y"),
          format(plot_frame$t_stamp[nrow(plot_frame)], "%d-%b-%Y"),
          paste0(input$lsl, "\u00B0C", " - ", input$usl, "\u00B0C"),
          paste0(round(plot_frame$total_mean[1], 1), "\u00B0C"),
          paste0(round(plot_frame$UCL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$LCL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$UStL[1], 1), "\u00B0C"),
          paste0(round(plot_frame$LStL[1], 1), "\u00B0C"),
          ifelse(OOS, "Yes", "No"),
          ifelse(risk, "Yes", "No"),
          format(Sys.Date(), "%d-%b-%Y")
        )
      )
    }
    
    rm(hourly)
    gc()
  })
  
  ##### SHOW TABLE 1 IF !IS.NULL############
  output$parameters1 <- renderTable({
    if (is.null(cached_data$table_1)) {
      return(NULL)
    }
    
    cached_data$table_1
    
  }) %>% bindEvent(input$show_data)
  
  
  output$parameters2 <- renderTable({
    cached_data$table_2
    
  }) %>% bindEvent(input$show_data)
  
  output$plot1 <- renderPlot({
    if (is.null(cached_data$p1)) {
      return(NULL)
    }
    
    cached_data$p1
    
  }) %>% bindEvent(input$show_data)
  
  output$plot2 <- renderPlot({
    cached_data$p2
    
  }) %>% bindEvent(input$show_data)
  
  # match to report
  output$report <- downloadHandler(
    filename = function() {
      paste("RTM - ", input$asset_id, Sys.Date(), ".zip")
    },
    
    content = function(zipfile) {
      req(input$user)
      
      temp_dir <- tempdir()
      
      zipped_files <- c()
      
      
      ################## YEARLY TEMPERATURE #################################################################
      
      #plot "2"
      if(!is.null(cached_data$move_date)){
        pdf_file <- file.path(temp_dir,
                              paste0(
                                input$asset_id, "_",
                                "RTM", "_",
                                cached_data$move_date, "_",
                                cached_data$end_date, "_",
                                "gen_",
                                Sys.Date(),
                                ".pdf"
                              ))
        zipped_files <- c(zipped_files, pdf_file)
        print(zipped_files)
        
        pdf(pdf_file)
      } else {
        pdf_file <- file.path(temp_dir,
                              paste0(
                                input$asset_id, "_",
                                "RTM", "_",
                                cached_data$start_date, "_",
                                cached_data$end_date, "_",
                                "gen_",
                                Sys.Date(),
                                ".pdf"
                              ))
        zipped_files <- c(zipped_files, pdf_file)
        print(zipped_files)
        
        pdf(pdf_file)
      }
      
      p <- cached_data$p2

      final_p <- p + labs(caption = paste("Generated", Sys.Date(), "by", input$user))
      
      final_p
      
      print(final_p)
      
      dev.off()
      
      ################ generate table "2"###################
      
      if(!is.null(cached_data$move_date)){
        csv_file <- file.path(temp_dir,
                              paste0(
                                input$asset_id, "_",
                                "RTM_Eval", "_",
                                cached_data$move_date, "_",
                                cached_data$end_date, "_",
                                "gen_",
                                Sys.Date(),
                                ".csv"
                              ))
        zipped_files <- c(zipped_files, csv_file)
        
        df <- cached_data$table_2
        
        data.table::fwrite(df, csv_file)
        
      } else {
        csv_file <- file.path(temp_dir,
                              paste0(
                                input$asset_id, "_",
                                "RTM_Eval", "_",
                                cached_data$start_date, "_",
                                cached_data$end_date, "_",
                                "gen_",
                                Sys.Date(),
                                ".csv"
                              ))
        zipped_files <- c(zipped_files, csv_file)
        
        df <- cached_data$table_2
        
        data.table::fwrite(df, csv_file)
      }
      
     
      
      #######plot 1 ########################################################
      
      if (!is.null(cached_data$p1)) {
        pdf_file <- file.path(temp_dir,
                              paste0(
                                input$asset_id, "_",
                                "RTM", "_",
                                cached_data$start_date, "_",
                                cached_data$move_date, "_",
                                "gen_",
                                Sys.Date(),
                                ".pdf"
                              ))
        zipped_files <- c(zipped_files, pdf_file)
        print(zipped_files)
        
        pdf(pdf_file)
        
        p <- cached_data$p1

        final_p <- p + labs(caption = paste("Generated", Sys.Date(), "by", input$user))
        
        final_p
        
        print(final_p)
        
        dev.off()
      }
      
      ##### table 1 ############################################################
      
      if (!is.null(cached_data$table_1)) {
        csv_file <- file.path(temp_dir,
                              paste0(
                                input$asset_id, "_",
                                "RTM_Eval", "_",
                                cached_data$start_date, "_",
                                cached_data$move_date, "_",
                                "gen_",
                                Sys.Date(),
                                ".csv"
                              ))
        
        zipped_files <- c(zipped_files, csv_file)
        
        df <- cached_data$table_2
        
        data.table::fwrite(df, csv_file)
      }
      
      #########Zip files##############
      zip::zipr(zipfile, files = zipped_files)
    }
    
    
    
    
  )
  
}

shinyApp(ui = ui, server = server)
