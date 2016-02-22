library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Prediction Application for the 2016 Six Nations Rugby Championship"),
  
             tabsetPanel(
               tabPanel("Instructions", 
                        br(),
                        p("This is an application that makes match prediction for the 2016 Six Nations Rugby Championship."),
                        p("The application uses a Generalized Linear Model (glm with poisson count and log link function) initially trained using the 2015 championship results shown below:"),
                        tableOutput("Result2015Table"),
                        #tags$style(HTML("#Result2015Table table{ 
                        #          margin: left;
                        #          width: 60%;
                        #           }")),
                        br(),
                        p("Four tab panels are available for you to prepare and run the prediction of matches to play, they should be used in the order given below:"),
                        tags$ol(
                          tags$li(h5("2016 Fixtures"),"Use this panel to enter the latest match results (so far Week 1 and Week 2 results are defined by default). There are two convenience buttons which you can use to either ",
                                  br(),"reset all score values to '-' (e.g. unplayed or to predict) or to the default scores for the two first weeks(played so far). This should allow you to experiment with the prediction model ",
                                  br(),"with arbitriraly chosen championship scores. For reference, click ",a("here",href="https://en.wikipedia.org/wiki/2016_Six_Nations_Championship")," to get the latest 2016 championship fixture results (wikipedia link)."),
                          tags$li(h5("2016 Championship Table"),"Use this panel to check the updated championship table based on your own inputs."),
                          tags$li(h5("2016 Training Dataset"),"Use this panel to check the updated training dataset which should contain the 2015 match results and any of the fixture results set in tab panel 1."),
                          tags$li(h5("2016 Unplayed Match Predictions"),"Use the 'Update Predictions!' button on this panel to train the prediction model with the updated training dataset and see the updated prediction output table and ",
                                  br(), "plots for the remaining 2016 championship matches.")),
                        br(),"The use of tab panels 2 and 3 is optional, you can start from panel 1 and directly jump to panel 4 to run predictions.",
                        br(),"The GLM model is retrained to incorporate the latest fixture results only when the 'Update Predictions!' button is pressed.",
                        br(),"The predicted outomes for the remaining 2016 championship matches will then reactively be updated and displayed in both graphical and tabular formats.",
                        br(),br()
               ),
                        tabPanel("1 - 2016 Fixtures",
                        fixedPage(
                          fixedRow(
                                     br(),
                                     tags$head(
                                       tags$style(HTML('#remove{color:#006bb3; border: 1px solid #006bb3; font-weight: bold;}'))
                                     ),
                                     tags$head(
                                       tags$style(HTML('#reset{color:#006bb3; border: 1px solid #006bb3; font-weight: bold;}'))
                                     ),
                                     actionButton("remove", "Reset all scores to '-'"),
                                     actionButton("reset", "Reset to Week 1 and Week 2 Scores"),
                                     hr()
                                     ,
                                     h3("Week 1"),
                                     column(width = 2, h4("06/02/2016 : ")),
                                     column(width = 2, h4("France")),
                                     column(width = 2, selectInput("FranceHome1", label=NULL, c("-", seq(0,60)), 23)),
                                     column(width = 2, selectInput("ItalyAway1", label=NULL, c("-",seq(0,60)), 21)),
                                     column(width = 2, h4("Italy"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("06/02/2016 : ")),
                                     column(width = 2, h4("Scotland")),
                                     column(width = 2, selectInput("ScotlandHome1", label=NULL,  c("-",seq(0,60)), 9)),
                                     column(width = 2, selectInput("EnglandAway1", label=NULL,  c("-",seq(0,60)), 15)),
                                     column(width = 2, h4("England"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("07/02/2016 : ")),
                                     column(width = 2, h4("Ireland")),
                                     column(width = 2, selectInput("IrelandHome1", label=NULL,  c("-",seq(0,60)), 16)),
                                     column(width = 2, selectInput("WalesAway1", label=NULL,  c("-",seq(0,60)), 16)),
                                     column(width = 2, h4("Wales"))
                                   ),
                          fixedRow(
                                     h3("Week 2"),
                                     column(width = 2, h4("13/02/2016 : ")),
                                     column(width = 2, h4("France")),
                                     column(width = 2, selectInput("FranceHome2", label=NULL,  c("-",seq(0,60)), 10)),
                                     column(width = 2, selectInput("IrelandAway2", label=NULL,  c("-",seq(0,60)), 9)),
                                     column(width = 2, h4("Ireland"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("13/02/2016 : ")),
                                     column(width = 2, h4("Wales")),
                                     column(width = 2, selectInput("WalesHome2", label=NULL,  c("-",seq(0,60)), 27)),
                                     column(width = 2, selectInput("ScotlandAway2", label=NULL,  c("-",seq(0,60)), 23)),
                                     column(width = 2, h4("Scotland"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("14/02/2016 : ")),
                                     column(width = 2, h4("Italy")),
                                     column(width = 2, selectInput("ItalyHome2", label=NULL,  c("-",seq(0,60)), 9)),
                                     column(width = 2, selectInput("EnglandAway2", label=NULL,  c("-",seq(0,60)), 40)),
                                     column(width = 2, h4("England"))
                                   ),
                          fixedRow(
                                     h3("Week 3"),
                                     column(width = 2, h4("26/02/2016 : ")),
                                     column(width = 2, h4("Wales")),
                                     column(width = 2, selectInput("WalesHome3", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("FranceAway3", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("France"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("27/02/2016 : ")),
                                     column(width = 2, h4("Italy")),
                                     column(width = 2, selectInput("ItalyHome3", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("ScotlandAway3", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("Scotland"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("27/02/2016 : ")),
                                     column(width = 2, h4("England")),
                                     column(width = 2, selectInput("EnglandHome3", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("IrelandAway3", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("Ireland"))
                                   ),
                          fixedRow(
                                     h3("Week 4"),
                                     column(width = 2, h4("12/03/2016 : ")),
                                     column(width = 2, h4("Ireland")),
                                     column(width = 2, selectInput("IrelandHome4", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("ItalyAway4", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("Italy"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("12/03/2016 : ")),
                                     column(width = 2, h4("England")),
                                     column(width = 2, selectInput("EnglandHome4", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("WalesAway4", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("Wales"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("13/03/2016 : ")),
                                     column(width = 2, h4("Scotland")),
                                     column(width = 2, selectInput("ScotlandHome4", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("FranceAway4", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("France"))
                                   ),
                          fixedRow(
                                     h3("Week 5"),
                                     column(width = 2, h4("19/03/2016 : ")),
                                     column(width = 2, h4("Wales")),
                                     column(width = 2, selectInput("WalesHome5", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("ItalyAway5", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("Italy"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("19/03/2016 : ")),
                                     column(width = 2, h4("Ireland")),
                                     column(width = 2, selectInput("IrelandHome5", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("ScotlandAway5", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("Scotland"))
                                   ),
                          fixedRow(
                                     column(width = 2, h4("19/03/2016 : ")),
                                     column(width = 2, h4("France")),
                                     column(width = 2, selectInput("FranceHome5", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, selectInput("EnglandAway5", label=NULL,  c("-",seq(0,60)), "-")),
                                     column(width = 2, h4("England"))
                                   ),
                                   hr()
                            )
               ),
               tabPanel("2 - 2016 Championship Table", 
                        fixedPage(
                          fixedRow(
                            br(),
                            p("The table below contains the latest fixture updates."),
                            br(),
                            tableOutput("ChampionshipTable"),
                            hr()
                          )
                        )
               ),
               tabPanel("3 - 2016 Training Dataset",
                        fixedPage(
                          fixedRow(
                            br(),
                            textOutput("ThirdPanelMessageValue"),
                            br(),
                            tableOutput("TrainingTable"),
                            hr()
                          )
                        )
               ),
               tabPanel("4 - 2016 Unplayed Match Predictions", 
                        fixedPage(
                          fixedRow(
                              br(),
                              textOutput("FourthPanelMessageValue"),
                              br(),
                              tags$head(
                                tags$style(HTML('#PredictButton{color:#006bb3; border: 1px solid #006bb3; font-weight: bold;}'))
                              ),
                              actionButton("PredictButton", "Update Predictions!"),
                              hr(),
                              plotOutput("PredictionPlots"),
                              hr(),
                              br(),
                              tags$style(HTML("#PredictionTableMessage { 
                                        margin: auto;
                                              }")),
                              
                              p(textOutput("PredictionTableMessage")),
                              br(),
                              tableOutput("PredictionTable"),
                              tags$style(HTML("#PredictionTable table{ 
                                        margin: auto;
                                         }")),
                              hr(),
                              br()
                            )
                        )
                )
             )
  )
)


