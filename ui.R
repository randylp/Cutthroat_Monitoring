library(shiny)
library(shinyFiles)
library(rhandsontable)
library(rdrop2)

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

shinyServer(
  #Sets up our page, this one has a header
  #Side bar (inputs)
  #Main panel (graphics)
  pageWithSidebar(
    #Adds a header at the top of the page
    headerPanel("Cutthroat Parasite Reporting Tool"),
    
    
    #This adds a side bar that we'll use to put inputs in
    sidebarPanel(
      selectInput("graphic", "Please Choose a Figure to Display",
                  choices = c("Instructions", "Parasite Identification Image", "Catch Area Map")),
      textInput("AngFirstName", "Angler First Name", ""),
      textInput("AngLastName", "Angler Last Name", ""),
      textInput("AngContact", "Contact Info (Email or Phone)", ""),
      selectInput("MarineArea", "Catch Area",
                  choices = c("Please Select","1", "2", "3", "4", "5","6","7","8-1","8-2","9","10","11","12","13","Alaska","Canada","Oregon","California")),
      selectInput("Method", "Angling Method",
                  choices = c("Please Select","From Shore", "From Boat")),
      selectInput("Month", "Capture Month",
                  choices = c("Please Select","1", "2", "3", "4", "5","6","7","8","9","10","11","12")),
      selectInput("Day", "Capture Day",
                  choices = c("Please Select","1", "2", "3", "4", "5","6","7","8","9","10","11","12",
                              "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23",
                              "24", "25", "26", "27", "28", "29", "30", "31")),
      textInput("Year", "Capture Year", ""),
      textInput("Hours", "Hours Fished", ""),
      textInput("NumAng", "Number of Anglers", ""),
      textInput("Catchdata", "Total Fish Count", ""),
      #Adds a dropdown box
      actionButton("DataProcessButton",label = "Click Here To Input Data"),
      actionButton("DataSendButton",label = "Click Here To Send Data")

      
    ),
    
    #Main panel is for graphics
    mainPanel(
      #outputs a plot
      plotOutput("Plot1", width = "600px", height = "600px"),
      rHandsontableOutput("table")
    )
  )
  
)
