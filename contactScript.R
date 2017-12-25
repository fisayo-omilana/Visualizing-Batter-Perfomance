importData <- function() {
  #Loading csv data.
  # install.packages("readr")
  library(readr)
  contactData <- read_csv("contactData.csv")
  
  #Altering variable names and adding default right handed batter variable for visuals
  colnames(contactData)[3] <- "px"
  colnames(contactData)[4] <- "pz"
  contactData$stand <- "R"
  return(contactData)
}

createMatrix <- function(contactData, pitchType) {
  
  #Creating a matrix of zeroes to fill accordingly with pitches of each type
  #that the batter made contact with, number of pitches of each type, and the
  #percentage of pitches made contact with of each type.
  contactMatrix <- matrix(0L, ncol = length(pitchType), nrow = 3)
  colnames(contactMatrix) <- pitchType
  rownames(contactMatrix) <- c("numContact", "numPitches", "contact%")
  
  #Looping through csv and adding plays where the batter makes contact with the pitch.
  #Organized based on the unique id for pitch type.
  for (i in 1:nrow(contactData)) {
    if (contactData$event_type[i] == "hit_into_play" | contactData$event_type[i] == "foul") {
      contactMatrix["numContact", contactData$pitch_name[i]] <- contactMatrix["numContact", contactData$pitch_name[i]] + 1
    }
    contactMatrix["numPitches", contactData$pitch_name[i]] <- contactMatrix["numPitches", contactData$pitch_name[i]] + 1
  }
  
  #Looping through now organzied matrix to calcuate the percentages for each pitch type
  #where the batter made contact
  for(i in 1:ncol(contactMatrix)) {
    contactMatrix["contact%", i] <- contactMatrix["numContact", i]/contactMatrix["numPitches", i]
  }
  return(contactMatrix)
}

contactData <- importData()

#Finding unique pitch types.
pitchType <- unique(contactData$pitch_name)

contactMatrix <- createMatrix(contactData, pitchType)

#In case the R shiny app does not work, here is the code for the screenshotted visual on my questionnaire:
#strikeFX(contactData, point.size = 1, point.alpha = .5, draw_zones = TRUE, color = "event_type") + scale_color_manual(values = c("blue", "green", "red")) + facet_grid(~pitch_type)

#Shiny interactive app that plots pitch locations, batter outcome, and strike zones for each pitch type
# install.packages("shiny")
library(shiny)
# install.packages("pitchRx", dependencies = TRUE)
library(pitchRx)

ui <- fluidPage(
  titlePanel("Batter Performance vs Various Pitches"),
  selectInput(inputId = "pitch", label = "Type of pitch:", choices = pitchType),
  plotOutput("batterViz"),
  textOutput("percentages")
)

contactData
contactMatrix

server <- function(input, output) {
  output$batterViz <- renderPlot({
    strikeFX(subset(contactData, pitch_name == input$pitch), point.size = 2, point.alpha = .5, draw_zones = TRUE, color = "event_type") + scale_color_manual(values = c("blue", "green", "red")) + ggtitle(input$pitch)
  })
  output$percentages <- renderText({
    paste("Makes contact with", round(contactMatrix["contact%", which(pitchType == input$pitch)], 3), "of", input$pitch, "pitches")
  })
}

shinyApp(ui = ui, server = server)
