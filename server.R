library(shiny)
library(shinyFiles)
library(rhandsontable)
library(rdrop2)

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

BlankDF <<- data.frame(Message = "Please fill out the above information and click Input Data to continue")
TestDF <<- data.frame(Message = "Please double click the Input Data button")

Checker <<- "No"
shinyServer(
  function(input, output, session){
    
    #Creates a function for plotting pictures
    plot_jpeg_map = function(path, add=FALSE)
    {
      require('jpeg')
      jpg = readJPEG(path, native=T) # read the file
      res = dim(jpg)[1:2] # get the resolution
      if (!add) # initialize an empty plot area if add==FALSE
        plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
      rasterImage(jpg,150,1,res[1]-150,res[2])
    }
    
    plot_jpeg_para = function(path, add=FALSE)
    {
      require('jpeg')
      jpg = readJPEG(path, native=T) # read the file
      res = dim(jpg)[1:2] # get the resolution
      if (!add) # initialize an empty plot area if add==FALSE
        plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
      rasterImage(jpg,75,1,res[1]-75,res[2])
    }
    
    observe({

      if (input$DataProcessButton == 0 | input$Catchdata == "")
        return(NULL)
      isolate({
        
        PrimaryDF <- data.frame(species = factor(levels = c("Please Choose","Chinook (King)", "Coho (Silver)", "Chum (Dog)", "Pink (Humpy)",
                                                            "Sockeye", "Steelhead", "Cutthroat")), 
                                size = factor(levels = c("Please Choose","Less than 12 inches", "Greater than or equal to 12 inches but less than 16 inches", 
                                                         "Greater than or equal to 16 inches but less than 20 inches", "Greater than or equal to 20 inches but less than 28 inches",
                                                         "Greater than or equal to 28 inches")),
                                numArgu =as.integer(), numCope = as.integer(), numUnk = as.integer())
        for (i in 1:input$Catchdata){
          NewRow <- data.frame(species = factor("Please Choose", levels = c("Please Choose","Chinook (King)", "Coho (Silver)", "Chum (Dog)", "Pink (Humpy)",
                                                                            "Sockeye", "Steelhead", "Cutthroat"), ordered = TRUE), 
                               size = factor("Please Choose", levels = c("Please Choose","Less than 12 inches", "Greater than or equal to 12 inches but less than 16 inches", 
                                                                          "Greater than or equal to 16 inches but less than 20 inches", "Greater than or equal to 20 inches but less than 28 inches",
                                                                          "Greater than or equal to 28 inches"), ordered = TRUE),
                               numArgu = "", numCope = "", numUnk = "")
          PrimaryDF <- rbind(PrimaryDF, NewRow)
        }
        
        colnames(PrimaryDF)[1] <- "Species"
        colnames(PrimaryDF)[2] <- "Size Category"
        colnames(PrimaryDF)[3] <- "Argulid Count"
        colnames(PrimaryDF)[4] <- "Copepod Count"
        colnames(PrimaryDF)[5] <- "Unknown Parasite Count"
        PrimaryDF <<- PrimaryDF
        Checker <<- "Yes"
      })
    })
    
    observe({
      
      if(input$DataSendButton == 0)
        return(NULL)
      isolate({
        InputDF <<- hot_to_r(input$table)
        
        colnames(InputDF)[1] <- "Species"
        colnames(InputDF)[2] <- "Size"
        colnames(InputDF)[3] <- "ArgCount"
        colnames(InputDF)[4] <- "CopeCount"
        colnames(InputDF)[5] <- "UnkCount"
        
        #This checks and makes sure that all data was filled out in the first form
        if (input$AngFirstName == ""| input$AngLastName == "" | input$AngContact == "" | input$MarineArea == "Please Select" |
            input$Month == "Please Select" | input$Day == "Please Select" | input$Year == "" | input$Hours == "" |
            input$Catchdata == "" | input$Method == "Please Select" | input$NumAng == ""){
          showModal(modalDialog(
            title = "Error message",
            "Please fill out all information on the left hand panel to continue"
          ))
        }

        else{
          
          ErrorCheck = FALSE
          for(i in 1:nrow(InputDF)){
            if(InputDF$Species[i] == "Please Choose" | InputDF$Size[i] == "Please Choose"){
              ErrorCheck = TRUE
            }
          }
          
          if(ErrorCheck == TRUE){
            showModal(modalDialog(
              title = "Error message",
              "Please fill out all information in the table to continue"
            ))
          }
          else{
            InputDF$AnglerFirst <- input$AngFirstName
            InputDF$AnglerLast <- input$AngLastName
            InputDF$AngContact <- input$AngContact
            InputDF$MarineArea <- input$MarineArea
            InputDF$Hours <- input$Hours
            InputDF$TotalCaught <- input$Catchdata
            InputDF$Date <- paste(input$Month, input$Day, input$Year, sep = "-")
            InputDF$TripNum <- -88
            InputDF$Verified <- "NO"
            InputDF$Shore <- input$Method
            InputDF$Contacted <- "NO"
        
            InputDF <<- InputDF

            DropDF<-drop_read_csv("CutthroatParasiteData2.csv",sep=",",dtoken=token)
            
            InputDF$TripNum <- max(DropDF$TripNum)+1
            
            InputDF$Timestamp <- format(Sys.time())
            
            InputDF$AngNum <- input$NumAng
        
            DropDF <- DropDF [-1]

            DropDFComp <<- rbind (DropDF, InputDF)

            filePath <- file.path(tempdir(), "CutthroatParasiteData2.csv")
            write.csv(DropDFComp,filePath)

            drop_upload(filePath)
        
            showModal(modalDialog(
              title = "Submission message",
              "Your data has been successfully submitted, thank you for using this reporting tool!"
            ))
          }
        }
        
        
        
        
      })
    })
    
    
    #Plot1 refers to the plot in the ui.R, main panel section
    
    output$Plot1 <- renderPlot({
      #If the the user has not yet provided a number of trout caught, ask them to do so

        #This is set by the user in ui.R from the select input button in the side panel
        graphic <- input$graphic
        
        
        if (graphic == "Instructions"){
          
          WelcomeText <- "Welcome to the Coastal Cutthroat Coalition Parasite Reporting Tool!

        Please fill out the angler name, capture date, marine area,  
        hours fished, and total trout count

        After these sections are filled out, please double click the 
        Input Data button. Fill out a row of data for each trout, 
        even those caught without parasites

        For an image to help identify a copepod and argulid, or a map of
        catch areas, please toggle the figure to display below

        Once all data is filled out, please hit the Send Data button
        
        Thanks for reporting parasites on your catch! 
        Please email James Losee at james.losee@dfw.wa.gov
        If you have any questions related to the tool"
          par(mar = c(0,0,0,0))
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.5, WelcomeText, 
               cex = 1.6, col = "black")
        }
        else if (graphic == "Parasite Identification Image"){
          plot_jpeg_para('argulid copepod.jpg')
        }
        else if (graphic == "Catch Area Map"){
          par(mar = c(0,0,0,0))
          plot_jpeg_map('MarineAreaMap.jpg')
        }
    })
    output$table <- renderRHandsontable({
      if(input$DataProcessButton == 0 | input$Catchdata == ""){
        rhandsontable(BlankDF, stretchH = "all", readOnly = TRUE)
      }
      else if (Checker == "No"){
        rhandsontable(TestDF, stretchH = "all", readOnly = TRUE)
      }
      else {
        rhandsontable(PrimaryDF, width = 1150, height = 500) %>%
          hot_cols(colWidths = c(150, 400, 150, 150, 200), rowHeights = 75) %>%
          hot_col(col = "Species", type = "dropdown", allowInvalid = FALSE, source = c("Please Choose","Chinook (King)", "Coho (Silver)", "Chum (Dog)", "Pink (Humpy)",
                                                                                       "Sockeye", "Steelhead", "Cutthroat")) %>%
          hot_col(col = "Size Category", type = "dropdown", allowInvalid = FALSE, source = c("Please Choose","Less than 12 inches", "Greater than or equal to 12 inches but less than 16 inches", 
                                                                          "Greater than or equal to 16 inches but less than 20 inches", "Greater than or equal to 20 inches but less than 28 inches",
                                                                                             "Greater than or equal to 28 inches")) 
      }
    })
  })
