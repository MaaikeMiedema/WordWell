library(shiny)
source("initapp_ip.r")


predicted <-list() #at start of app empty prediction list
predictedChoice <- character()
predictedValue <- character()

m <- 5 # nr of predictions showed 

ui <- navbarPage("WordWell ",
                tabPanel("Predict",
                         tags$body(background = "vague_letters.jpg"),
                         splitLayout(
                               cellWidths = c("85%", "15%"),
                               textInput("inText", 
                               label= NULL, 
                               placeholder = "Type or insert text and hit space bar",
                               width = "100%"
                               ),
                               actionButton("btClear",
                                            "Clear"
                                     )
                               ),
                         radioButtons("rbPrediction", 
                                      label= NULL,
                                      choiceNames = predictedChoice,
                                      choiceValues = predictedValue,     
                                      selected = character(0)
                                      ),
                         p("How it works:",br(),"Type or insert text in text field followed by a space and 
                           select one of the five suggestions or type your own 
                           next word followed by a space. The next predictions will appear. To start again: 
                           hit the \"clear\"button")
                         ),
                tabPanel("About", 
                         p("WordWell", br(),
                           "WordWell has been developed as part of the Data Science Specialization
                           of Johns Hopkins University at Coursera.",  
                           br(), "Code and more info can be found at my GitHub account:",
                           a("https://github.com/MaaikeMiedema/WordWell", 
                             href ="https://github.com/MaaikeMiedema/WordWell",
                             target="_blank" )),
                         p("Suggestions", br(),
                           "Next word sugggestions appear in descending order of likelihood. 
                           The percentages indicate the scores relative to the score of the best match. 
                           The first suggestion thus has a percentage of 100%, 
                           unless no prediction has been found and the algorithm suggests the most frequent words"),
                         p("Algorithm", br(),
                           "The prediction score is calculated by a linear interpolation of relative frequencies of word combinations. 
                           It uses data from different sources: news, twitter and blogs. Data is provided by Coursera. 
                           The data has been cleaned and the words that cover 97.5%
                           of the texts are included in the prediction model"),
                         p("Accuracy",br(), 
                           "Accuracy tests show that in about 16% of the cases the provided suggestions contain the correct word. 
                           In the future accuracy may be improved by choosing another algorithm, 
                           or by adjusting the prediction realtime, while the user is typing." )
                )
)

server <- function(input, output, session) {
      
      # when type blank
      observeEvent(input$inText, {
            if (str_trunc(input$inText,1, side= "left", ellipsis = "") == " "){
                  predicted<-predictIP(input$inText, ngramProbs,
                                    rmWords, vocabulary, naSub, m)
                  predictedValue<- paste0(predicted[,1]," ") # next prediction follows directly
                  predictedChoice<- paste0(predicted[,1]," ... ", 
                                             as.character(100* round(predicted[,2],3)),"%")
                  updateRadioButtons(session,
                                     inputId = "rbPrediction",
                                     label = "Suggestions",
                                     choiceNames = predictedChoice,
                                     choiceValues = predictedValue,  
                                     selected = character(0)
                  )
            }
      })
      # when click radio buttons
      observeEvent(input$rbPrediction,{
            updateTextInput(session,
                            inputId = "inText", 
                            value = paste0(input$inText, input$rbPrediction)
                            )
            }
      )
      #when click "clear" button
      observeEvent(input$btClear, {
            updateTextInput(session, 
                            inputId = "inText", 
                            value = ""
                            )
            updateRadioButtons(session, 
                               inputId = "rbPrediction", 
                               choices = list(),
                               selected = character(0)
                               )
            }
      )
}

shinyApp(ui, server)

