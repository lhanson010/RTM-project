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

# pull hourly function

pull_hourly <- function(df){#, asset_id){
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
  if (length(end_dst) > 0){
    print(end_dst)
    #identify problematic hour
    problem_hour<- round(tstamps[end_dst], "hours")
    print(problem_hour)
    #work on time after DST ends (will only be encountered once a year)
    #could handle edge case where rtm starts on the day of DST, but not for now
    std_time_times <- tstamps[end_dst:length(tstamps)]
    #of the times following the end of dst, which match the duplicate hour. Then add
    #back the dst days so indexing is correct when we remove these indeces
    #floor so everything up to 1:59 duplicate is removed
    rm_indeces <- which(lubridate::floor_date(std_time_times, unit = "hour") == problem_hour)+end_dst-1
    #remove duplicate DST entries
    tstamps <- tstamps[-rm_indeces]
  }
  #create key of hours to round to 
  hours_key <- seq(from = round(tstamps[1], "hours"), 
                   to = round(tstamps[length(tstamps)], "hours"),
                   by = 3600)
  # findInterval to identify which intervals between recording timestamps contain HH:00 values
  new_index <- findInterval(hours_key,tstamps)
  
  # create and return trimmed data
  hourly_data <- df[new_index,]
  return(hourly_data)
}


options(shiny.maxRequestSize = 500*1024^2)

ui <- fluidPage(
  titlePanel("Routine Trend Monitoring (RTM) tool"),
  sidebarLayout(
    sidebarPanel(
      textInput("user", "Enter your full name"),
      textInput("asset_id", "Enter the unit asset ID"),
      textInput("start_year", "Enter the year this trend review starts"),
      textInput("end_year", "Enter the year this trend review ends"),
      #numericInput("mean_warm", "Mean Warmest Location Average Temperature from the most recent protocol", value = 1),
      #numericInput("mean_cold", "Mean Coolest Location Average Temperature from the most recent protocol", value = 0),
      numericInput("usl", "Upper Specification Limit", value = 0),
      numericInput("lsl", "Lower Specification Limit", value = 0),
      fileInput("upload", NULL, buttonLabel = "Upload Trend Data", multiple = TRUE, accept = ".csv"), #c(".xlsx", ".xls")),
      #textOutput("text")
      #tableOutput("head"),
      actionButton("show_data", "Analyze Data"),
      downloadButton("report", "Download Data (zip file)")
    ),
    mainPanel(
      tableOutput("parameters"),
      plotOutput("plot1", width = "100%"),
      plotOutput("plot2", width = "100%"),
      plotOutput("plot3", width = "100%"),
    )
  )
)
server <- function(input, output, session) {
  # temp var to store data
  cached_data <- reactiveValues()
  
  data <- observeEvent(input$upload, {
    req(input$upload)
    file_amount <- nrow(input$upload)
    processed_data <- list()
    for (i in 1:file_amount){
      csv_data <- fread(input$upload$datapath[i])
      processed_data[[i]] <- pull_hourly(csv_data)
      rm(csv_data)
      gc()
    }
    #data_list <- lapply(input$upload$datapath, read_excel)
    combined_data <- bind_rows(processed_data) %>% arrange(t_stamp)
    cached_data$data <- combined_data
    rm(combined_data)
    gc()
  })
  
  #store hourly trend data
  output$parameters <- renderTable({
    req(input$asset_id)
    #req(input$mean_cold)
    #req(input$mean_warm)
    req(input$lsl)
    req(input$usl)
    
    gt_length <- 100
    
    #initial
    ground_truth <- cached_data$data[[2]][1:gt_length]
    print(ground_truth)
    groupings <- ceiling(seq_along( cached_data$data[[2]])/gt_length)
    print(groupings)
    rolling_mean <- tapply( cached_data$data[[2]], groupings, mean)
    rolling_sd <- tapply( cached_data$data[[2]], groupings, sd)
    print(rolling_mean)
    group_times <- as.POSIXct(tapply(cached_data$data[[1]], groupings, function(x) x[1]))
    std_bs <- replicate(n = 1000, sample(ground_truth, gt_length, replace = TRUE))
    SDs <- apply(std_bs, 2, sd)
    rm(std_bs)
    gc()
    sd_of_sd <- sd(SDs)
    std_err <- sd(ground_truth)/sqrt(gt_length)
    center_line <- mean(ground_truth)
    gt_sd <- sd(ground_truth)
    ucl_x <- center_line + 3*std_err
    lcl_x <- center_line - 3*std_err
    ucl_s <- gt_sd + 3*sd_of_sd 
    
    #OOC variable initializations
    mean_OOC <- FALSE
    sd_OOC <- FALSE
    CAPA <- FALSE
    
    #new potential ground truth
    current_state <- tail(cached_data$data[[2]], gt_length)
    std_bs_cs <- replicate(n = 1000, sample(current_state, gt_length, replace = TRUE))
    SDs_cs <- apply(std_bs_cs, 2, sd)
    rm(std_bs_cs)
    gc()
    sd_of_sd_cs <- sd(SDs_cs)
    std_err_new <- sd(current_state)/sqrt(gt_length)
    center_line_new <- mean(current_state)
    gt_sd_new <- sd(current_state)
    ucl_x_new <- center_line_new + 3*std_err_new
    lcl_x_new <- center_line_new - 3*std_err_new
    ucl_s_new <- gt_sd_new + 3*sd_of_sd_cs 
    
    #sd ooc checker
    if (gt_sd_new > ucl_s){
      sd_OOC <- TRUE
      if((lcl_x_new - 3*(ucl_s_new/sqrt(gt_length))) < input$lsl | (ucl_x_new + 3*(ucl_s_new/sqrt(gt_length))) > input$usl){
        CAPA <- TRUE
      }
    }
    
    #mean ooc checker
    if (center_line_new > ucl_x | center_line_new < lcl_x){
      mean_OOC <- TRUE
      #see if new mean control limit plus standard error as calculated with the new lcl_s is OOS
      if((lcl_x_new - 3*(ucl_s_new/sqrt(gt_length))) < input$lsl | (ucl_x_new + 3*(ucl_s_new/sqrt(gt_length))) > input$usl){ #
        CAPA <- TRUE 
      }
    }
    
    
    
    data.frame(Parameter = c("Asset ID", 
                             "Start Date", 
                             "End Date", 
                             "Operating Range", 
                             "Mean of first 100 hours of data",
                             "Standard Deviation of first 100 hours of data",
                             "UCL of Mean",
                             "LCL of Mean",
                             "UCL of S.D.",
                             "Mean out-of-control?",
                             "S.D. out-of-control?",
                             "New Mean UCL",
                             "New Mean LCL",
                             "New S.D. UCL",
                             "CAPA Required?",
                             "Date Generated"),
               Value = c(input$asset_id,
                         format(cached_data$data[[1]][1],"%d-%b-%Y"),
                         format(cached_data$data[[1]][nrow(cached_data$data)], "%d-%b-%Y"),
                         paste0(input$lsl,"\u00B0C"," - ", input$usl,"\u00B0C"),
                         paste0(round(center_line, 1), "\u00B0C"),
                         paste0(round(gt_sd, 1), "\u00B0C"),
                         paste0(round(ucl_x, 1), "\u00B0C"), 
                         paste0(round(lcl_x, 1), "\u00B0C"),
                         paste0(round(ucl_s, 1), "\u00B0C"),
                         ifelse(mean_OOC, "Yes", "No"),
                         ifelse(sd_OOC, "Yes", "No"),
                         ifelse(mean_OOC, paste0(round(ucl_x_new,1),"\u00B0C"), "N/A"),
                         ifelse(mean_OOC, paste0(round(lcl_x_new,1),"\u00B0C"), "N/A"),
                         ifelse(sd_OOC, paste0(round(ucl_s_new,1), "\u00B0C"), "N/A"),
                         ifelse(CAPA, "Yes", "No"),
                         format(Sys.Date(),"%d-%b-%Y")
               )
    )
  }) %>% bindEvent(input$show_data)
  
  output$plot1 <- renderPlot({
    req(input$asset_id)
    req(input$lsl)
    req(input$usl)
    req(input$start_year)
    req(input$end_year)
    
    ######## CALCULATIONS ##################
    gt_length <- 100
    ground_truth <- cached_data$data[[2]][1:gt_length]
    print(ground_truth)
    groupings <- ceiling(seq_along( cached_data$data[[2]])/gt_length)
    print(groupings)
    rolling_mean <- tapply( cached_data$data[[2]], groupings, mean)
    rolling_sd <- tapply( cached_data$data[[2]], groupings, sd)
    print(rolling_mean)
    group_times <- as.POSIXct(tapply(cached_data$data[[1]], groupings, function(x) x[1]))
    std_bs <- replicate(n = 1000, sample(ground_truth, gt_length, replace = TRUE))
    SDs <- apply(std_bs, 2, sd)
    rm(std_bs)
    gc()
    sd_of_sd <- sd(SDs)
    std_err <- sd(ground_truth)/sqrt(gt_length)
    center_line <- mean(ground_truth)
    gt_sd <- sd(ground_truth)
    ucl_x <- center_line + 3*std_err
    lcl_x <- center_line - 3*std_err
    ucl_s <- gt_sd + 3*sd_of_sd  
    yhi <- input$usl + 3
    ylo <- input$lsl - 3
    #plot(mean_times, rolling_mean)
    x_labels <- as.POSIXct(cached_data$data[[1]][seq(1, length(cached_data$data[[1]]), by = 200)])
    
    ########### YEARLY TEMPERATURE #######################
    par(mar = c(6, 4.1, 4.1, 2.1))
    plot(cached_data$data,
         main = paste0(input$asset_id, " Temperature "),
         #sub = paste(input$start_year, " - ", input$end_year),
         ylim = c(ylo, yhi),
         ylab = "Temperature (\u00B0C)",
         xlab = " ",
         type = "l",
         col = "red",
         xaxt = "n",
         cex.main = 2,
         font.main = 1)
    abline(h = c(center_line,  input$usl, input$lsl),
           col = c("black","magenta2","magenta2")
    )
    axis(1, at = x_labels, labels = format(x_labels,"%d-%b-%Y"), las = 2)
    legend(x = "topright",
           legend = c("Initial Mean",
                      "Specification Limit"),
           lty = c(1, 1),
           col = c("black", "magenta"),
           cex = 0.8)
  })%>% bindEvent(input$show_data)
  ################ X-BAR CHART ###################
  output$plot2 <- renderPlot({
    req(input$asset_id)
    req(input$lsl)
    req(input$usl)
    req(input$start_year)
    req(input$end_year)
    
    ######## CALCULATIONS ##################
    gt_length <- 100
    ground_truth <- cached_data$data[[2]][1:gt_length]
    print(ground_truth)
    groupings <- ceiling(seq_along( cached_data$data[[2]])/gt_length)
    print(groupings)
    rolling_mean <- tapply( cached_data$data[[2]], groupings, mean)
    #rolling_sd <- tapply( cached_data$data[[2]], groupings, sd)
    print(rolling_mean)
    group_times <- as.POSIXct(tapply(cached_data$data[[1]], groupings, function(x) x[1]))
    #std_bs <- replicate(n = 1000, sample(ground_truth, gt_length, replace = TRUE))
    #SDs <- apply(std_bs, 2, sd)
    #rm(std_bs)
    #gc()
    #sd_of_sd <- sd(SDs)
    std_err <- sd(ground_truth)/sqrt(gt_length)
    center_line <- mean(ground_truth)
    gt_sd <- sd(ground_truth)
    ucl_x <- center_line + 3*std_err
    lcl_x <- center_line - 3*std_err
    #ucl_s <- gt_sd + 3*sd_of_sd  
    yhi <- input$usl + 3
    ylo <- input$lsl - 3
    #plot(mean_times, rolling_mean)
    x_labels <- as.POSIXct(cached_data$data[[1]][seq(1, length(cached_data$data[[1]]), by = 200)])
    par(mar = c(6, 4.1, 4.1, 2.1))
    plot(group_times, rolling_mean, 
         main = paste0(input$asset_id, " Temperature X-Bar Chart"),
         #sub = paste(input$start_year, " - ", input$end_year),
         ylim = c(input$lsl, input$usl),
         ylab = "Average 100-Hour Temperature (\u00B0C)",
         xlab = " ",
         type = "l",
         col = "navy",
         lwd = 3,
         xaxt = "n",
         cex.main = 2,
         font.main = 1)
    abline(h = c(center_line,  ucl_x, lcl_x),
           col = c("black","red","red"),
           lty = c(1,2,2)
    )
    
    axis(1, at = x_labels, labels = format(x_labels,"%d-%b-%Y"), cex.axis = 0.7, las = 2)
    legend(x = "topright",
           legend = c("Initial Mean",
                      "Control Limit - Mean"),
           col = c("black", "red"),
           lty = c(1,2),
           cex = 0.8)
  }) %>% bindEvent(input$show_data)
  ################### s-CHART ##################################################
  output$plot3 <- renderPlot({
    req(input$asset_id)
    req(input$lsl)
    req(input$usl)
    req(input$start_year)
    req(input$end_year)
    
    ######## CALCULATIONS ##################
    gt_length <- 100
    ground_truth <- cached_data$data[[2]][1:gt_length]
    #print(ground_truth)
    groupings <- ceiling(seq_along( cached_data$data[[2]])/gt_length)
    #print(groupings)
    #rolling_mean <- tapply(cached_data$data[[2]], groupings, mean)
    rolling_sd <- tapply(cached_data$data[[2]], groupings, sd)
    #print(rolling_mean)
    group_times <- as.POSIXct(tapply(cached_data$data[[1]], groupings, function(x) x[1]))
    std_bs <- replicate(n = 1000, sample(ground_truth, gt_length, replace = TRUE))
    SDs <- apply(std_bs, 2, sd)
    rm(std_bs)
    gc()
    sd_of_sd <- sd(SDs)
    #std_err <- sd(ground_truth)/sqrt(gt_length)
    #center_line <- mean(ground_truth)
    gt_sd <- sd(ground_truth)
    #ucl_x <- center_line + 3*std_err
    #lcl_x <- center_line - 3*std_err
    ucl_s <- gt_sd + 3*sd_of_sd  
    #yhi <- input$usl + 3
    #ylo <- input$lsl - 3
    #plot(mean_times, rolling_mean)
    x_labels <- as.POSIXct(cached_data$data[[1]][seq(1, length(cached_data$data[[1]]), by = 200)])
    par(mar = c(6, 4.1, 4.1, 2.1))
    plot(group_times, rolling_sd, 
         main = paste0(input$asset_id, " Temperature S Chart"),
         #sub = paste(input$start_year, " - ", input$end_year),
         ylim = c(0, 4*ucl_s),
         ylab = "Standard Deviation of 100-Hour Temperature (\u00B0C)",
         xlab = " ",
         type = "l",
         col = "limegreen",
         lwd = 3,
         xaxt = "n",
         cex.main = 2,
         font.main = 1)
    abline(h = c(gt_sd,  ucl_s ),
           col = c("black", "red"),
           lty = c(1,2)
    )
    axis(1, at = x_labels, labels = format(x_labels,"%d-%b-%Y"), las = 2, cex.axis = 0.7)
    legend(x = "topright",
           legend = c("Initial Standard Deviation",
                      "Control Limit - Standard Deviation"),
           col = c("black", "red"),
           lty = c(1,2),
           cex = 0.8)  
    par(mfrow = c(1, 1))
  }) %>% bindEvent(input$show_data)
  
  # match to report
  output$report <- downloadHandler(
    
    filename = function() {
      paste("RTM - ", input$asset_id, Sys.Date(), ".zip")
    },
    
    content = function(zipfile) {
      req(input$asset_id)
      req(input$lsl)
      req(input$usl)
      req(input$start_year)
      req(input$end_year)
      req(input$user)
      
      temp_dir <- tempdir()
      
      zipped_files <- c()
      
      
      ##################### CALCULATIONS #############################################
      gt_length <- 100
      ground_truth <- cached_data$data[[2]][1:gt_length]
      print(ground_truth)
      groupings <- ceiling(seq_along( cached_data$data[[2]])/gt_length)
      print(groupings)
      rolling_mean <- tapply( cached_data$data[[2]], groupings, mean)
      rolling_sd <- tapply( cached_data$data[[2]], groupings, sd)
      print(rolling_mean)
      group_times <- as.POSIXct(tapply(cached_data$data[[1]], groupings, function(x) x[1]))
      std_bs <- replicate(n = 1000, sample(ground_truth, gt_length, replace = TRUE))
      SDs <- apply(std_bs, 2, sd)
      rm(std_bs)
      gc()
      sd_of_sd <- sd(SDs)
      std_err <- sd(ground_truth)/sqrt(gt_length)
      center_line <- mean(ground_truth)
      gt_sd <- sd(ground_truth)
      ucl_x <- center_line + 3*std_err
      lcl_x <- center_line - 3*std_err
      ucl_s <- gt_sd + 3*sd_of_sd  
      yhi <- input$usl + 3
      ylo <- input$lsl - 3
      #plot(mean_times, rolling_mean)
      x_labels <- as.POSIXct(cached_data$data[[1]][seq(1, length(cached_data$data[[1]]), by = 200)])
      par(mar = c(6, 4.1, 4.1, 2.1))
      
      
      ################## YEARLY TEMPERATURE #################################################################
      pdf_file <- file.path(temp_dir, paste(input$asset_id, 
                                            "Temperature", 
                                            input$start_year,
                                            input$end_year, ".pdf"))
      zipped_files <- c(zipped_files, pdf_file)
      print(zipped_files)
      pdf(pdf_file)
      par(mar = c(6, 4.1, 4.1, 2.1))
      plot(cached_data$data,
           main = paste0(input$asset_id, " Temperature "),
           #sub = paste(input$start_year, " - ", input$end_year),
           ylim = c(ylo, yhi),
           ylab = "Temperature (\u00B0C)",
           xlab = " ",
           type = "l",
           col = "red",
           xaxt = "n",
           cex.main = 2,
           font.main = 1)
      abline(h = c(center_line,  input$usl, input$lsl),
             col = c("black","magenta2","magenta2")
      )
      axis(1, at = x_labels, labels = format(x_labels,"%d-%b-%Y"), las = 2, cex.axis = 0.7)
      legend(x = "topright",
             legend = c("Initial Mean",
                        "Specification Limit"),
             lty = c(1,1),
             col = c("black", "magenta"),
             cex = 0.8)
      mtext(paste("Generated", Sys.Date(), "by", input$user), side = 4, line = 1, adj = 0, cex = 0.7)
      mtext("Note: Control Limits and Initial mean are calculated based on the first 100 hours of data following the unit release", 
            side = 1, line = 5, adj = 0, cex = 0.7)
      dev.off()
      
      ################################ X-BAR CHART ###################################################
      pdf_file <- file.path(temp_dir, paste(input$asset_id, 
                                            "Xbar-Chart", 
                                            input$start_year,
                                            input$end_year, ".pdf"))
      zipped_files <- c(zipped_files, pdf_file)
      print(zipped_files)
      pdf(pdf_file)
      par(mar = c(6, 4.1, 4.1, 2.1))
      plot(group_times, rolling_mean, 
           main = paste0(input$asset_id, " Temperature X-Bar Chart"),
           #sub = paste(input$start_year, " - ", input$end_year),
           ylim = c(ylo, yhi),
           ylab = "Average 100-Hour Temperature (\u00B0C)",
           xlab = " ",
           type = "l",
           col = "navy",
           lwd = 3,
           xaxt = "n",
           cex.main = 2,
           font.main = 1)
      abline(h = c(center_line,  ucl_x, lcl_x),
             col = c("black","red","red"),
             lty = c(1,2,2)
      )
      
      axis(1, at = x_labels, labels = format(x_labels,"%d-%b-%Y"), las = 2, cex.axis = 0.7)
      legend(x = "topright",
             legend = c("Initial Mean",
                        "Control Limit - Mean"),
             col = c("black", "red"),
             lty = c(1,2),
             cex = 0.8)
      mtext(paste("Generated", Sys.Date(), "by", input$user), side = 4, line = 1, adj = 0, cex = 0.7)
      mtext("Note: Control Limits and Initial mean are calculated based on the first 100 hours of data following the unit release", 
            side = 1, line = 5, adj = 0, cex = 0.7)
      dev.off()
      
      ###################### s-CHART ###############################################################
      pdf_file <- file.path(temp_dir, paste(input$asset_id, 
                                            "S-Chart", 
                                            input$start_year,
                                            input$end_year, ".pdf"))
      zipped_files <- c(zipped_files, pdf_file)
      print(zipped_files)
      pdf(pdf_file)
      par(mar = c(6, 4.1, 4.1, 2.1))
      plot(group_times, rolling_sd, 
           main = paste0(input$asset_id, " Temperature S Chart"),
           #sub = paste(input$start_year, " - ", input$end_year),
           ylim = c(ylo, yhi),
           ylab = "Standard Deviation of 100-Hour Temperature (\u00B0C)",
           xlab = " ",
           type = "l",
           col = "limegreen",
           lwd = 3,
           xaxt = "n",
           cex.main = 2,
           font.main = 1)
      abline(h = c(gt_sd,  ucl_s ),
             col = c("black", "red"),
             lty = c(1,2)
      )
      axis(1, at = x_labels, labels = format(x_labels,"%d-%b-%Y"), las = 2, cex.axis = 0.7)
      legend(x = "topright",
             legend = c("Initial Standard Deviation",
                        "Control Limit - Standard Deviation"),
             col = c("black", "red"),
             lty = c(1,2),
             cex = 0.8)  
      mtext(paste("Generated", Sys.Date(), "by", input$user), side = 4, line = 1, adj = 0, cex = 0.7)
      mtext("Note: Control Limits and Initial S.D. are calculated based on the first 100 hours of data following the unit release", 
            side = 1, line = 5, adj = 0, cex = 0.7)
      dev.off()
      ################### Generate info table ########################################
      
      #OOC variable initializations
      mean_OOC <- FALSE
      sd_OOC <- FALSE
      CAPA <- FALSE
      
      #new potential ground truth
      current_state <- tail(cached_data$data[[2]], gt_length)
      std_bs_cs <- replicate(n = 1000, sample(current_state, gt_length, replace = TRUE))
      SDs_cs <- apply(std_bs_cs, 2, sd)
      rm(std_bs_cs)
      gc()
      sd_of_sd_cs <- sd(SDs_cs)
      std_err_new <- sd(current_state)/sqrt(gt_length)
      center_line_new <- mean(current_state)
      gt_sd_new <- sd(current_state)
      ucl_x_new <- center_line_new + 3*std_err_new
      lcl_x_new <- center_line_new - 3*std_err_new
      ucl_s_new <- gt_sd_new + 3*sd_of_sd_cs
      
      #sd ooc checker
      if (gt_sd_new > ucl_s){
        sd_OOC <- TRUE
        if((lcl_x_new - 3*(ucl_s_new/sqrt(gt_length))) < input$lsl | (ucl_x_new + 3*(ucl_s_new/sqrt(gt_length))) > input$usl){
          CAPA <- TRUE
        }
      }
      
      #mean ooc checker
      if (center_line_new > ucl_x | center_line_new < lcl_x){
        mean_OOC <- TRUE
        #see if new mean control limit plus standard error as calculated with the new lcl_s is OOS
        if((lcl_x_new - 3*(ucl_s_new/sqrt(gt_length))) < input$lsl | (ucl_x_new + 3*(ucl_s_new/sqrt(gt_length))) > input$usl){ #
          CAPA <- TRUE
        }
      }
      
      
      
      df = data.frame(Parameter = c("Asset ID",
                                    "Start Date",
                                    "End Date",
                                    "Operating Range",
                                    "Mean of first 100 hours of data",
                                    "Standard Deviation of first 100 hours of data",
                                    "UCL of Mean",
                                    "LCL of Mean",
                                    "UCL of S.D.",
                                    "Mean out-of-control?",
                                    "S.D. out-of-control?",
                                    "New Mean UCL",
                                    "New Mean LCL",
                                    "New S.D. UCL",
                                    "CAPA Required?",
                                    "Date Generated"),
                      Value = c(input$asset_id,
                                format(cached_data$data[[1]][1],"%d-%b-%Y"),
                                format(cached_data$data[[1]][nrow(cached_data$data)], "%d-%b-%Y"),
                                paste0(input$lsl,"\u00B0C"," - ", input$usl,"\u00B0C"),
                                paste0(round(center_line, 1), "\u00B0C"),
                                paste0(round(gt_sd, 1), "\u00B0C"),
                                paste0(round(ucl_x, 1), "\u00B0C"),
                                paste0(round(lcl_x, 1), "\u00B0C"),
                                paste0(round(ucl_s, 1), "\u00B0C"),
                                ifelse(mean_OOC, "Yes", "No"),
                                ifelse(sd_OOC, "Yes", "No"),
                                ifelse(mean_OOC, paste0(round(ucl_x_new,1),"\u00B0C"), "N/A"),
                                ifelse(mean_OOC, paste0(round(lcl_x_new,1),"\u00B0C"), "N/A"),
                                ifelse(sd_OOC, paste0(round(ucl_s_new,1), "\u00B0C"), "N/A"),
                                ifelse(CAPA, "Yes", "No"),
                                format(Sys.Date(),"%d-%b-%Y")
                      )
      )
      
      csv_file <- file.path(temp_dir, paste(input$asset_id,
                                            "RTM Evaluation",
                                            input$start_year,
                                            input$end_year, ".csv"))
      
      zipped_files <- c(zipped_files, csv_file)
      
      data.table::fwrite(df, csv_file)
      
      #########Zip files##############
      zip::zipr(zipfile, files = zipped_files)
    }
    
    
    
    
  )
  #   filename = function() {
  #     paste0(input$asset_id, " - RTM Report - ", Sys.Date(), ".pdf")
  #   }
  #   content = function(file){
  #     tempReport <- file.path(tempdir(),"report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  # 
  #     params <- list(
  #       name = input$user,
  #       asset = input$asset_id,
  #       start_year = input$start_year,
  #       end_year = input$end_year,
  #       mean_warm = input$mean_warm,
  #       mean_cold = input$mean_cold,
  #       usl = input$usl,
  #       lsl = input$lsl,
  #       data = cached_data$data
  #     )
  # 
  #     rmarkdown:render(tempReport,
  #                      output_file = file,
  #                      params = params,
  #                      envir = new.env(parent = globalenv())
  #                      )
  #   }
  # )
  # output$head <- renderTable(
  #   head(data()[[1]], input$n))
}

shinyApp(ui = ui, server = server)
