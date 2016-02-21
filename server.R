library(shiny)
library(dplyr)
library(xtable)
library(reshape2)
library(ggplot2)
library(gridExtra)


#--- 2015 Championship results
rugby_results_2015_txt = 'Date,HomeTeam,AwayTeam,HomeScore,AwayScore
6 February 2015,Wales,England,16,21
7 February 2015,Italy,Ireland,3,26
7 February 2015,France,Scotland,15,8
14 February 2015,England,Italy,47,17
14 February 2015,Ireland,France,18,11
15 February 2015,Scotland,Wales,23,26
28 February 2015,Scotland,Italy,19,22
28 February 2015,France,Wales,13,20
1 March 2015,Ireland,England,19,9
14 March 2015,Wales,Ireland,23,16
14 March 2015,England,Scotland,25,13
15 March 2015,Italy,France,0,29
21 March 2015,Italy,Wales,20,61
21 March 2015,Scotland,Ireland,10,40
21 March 2015,England,France,55,35'


rugby_results_2015_df <- read.csv(text = rugby_results_2015_txt,header = T,row.names = NULL)

rugby_results_2015_df$HomeScore <- as.numeric(rugby_results_2015_df$HomeScore)
rugby_results_2015_df$AwayScore <- as.numeric(rugby_results_2015_df$AwayScore)

rugby_results_2015_df$HomeResult <- as.factor(with(rugby_results_2015_df, {ifelse( HomeScore > AwayScore, "W", ifelse( HomeScore == AwayScore,"D","L"))}))
rugby_results_2015_df$AwayResult <- as.factor(with(rugby_results_2015_df, {ifelse( HomeScore > AwayScore, "L", ifelse( HomeScore == AwayScore,"D","W"))}))


#--- 2015 Championship Table creation
training_results_2015 <- data.frame(Date=rugby_results_2015_df$Date,Team=as.factor(c(as.character(rugby_results_2015_df$HomeTeam),
                                        as.character(rugby_results_2015_df$AwayTeam))),
                       Opponent=as.factor(c(as.character(rugby_results_2015_df$AwayTeam),
                                            as.character(rugby_results_2015_df$HomeTeam))),
                       Score=c(rugby_results_2015_df$HomeScore, rugby_results_2015_df$AwayScore),
                       Home=c(as.integer(rep(1, dim(rugby_results_2015_df)[1])), as.integer(rep(0, dim(rugby_results_2015_df)[1]))),
                       Result=factor(c(rugby_results_2015_df$HomeResult,rugby_results_2015_df$AwayResult), labels=c("L","W")))

table_2015_results <- tbl_df(training_results_2015)  %>% group_by(Team) %>% summarise(Played=n())
table_2015_results <- merge(table_2015_results,tbl_df(training_results_2015) %>% group_by(Team) %>% summarise(Won=as.integer(sum(Result=="W"))))
table_2015_results <- merge(table_2015_results,tbl_df(training_results_2015) %>% group_by(Team) %>% summarise(Drawn=as.integer(sum(Result=="D"))))
table_2015_results <- merge(table_2015_results,tbl_df(training_results_2015) %>% group_by(Team) %>% summarise(Lost=as.integer(sum(Result=="L"))))
table_2015_results <- merge(table_2015_results,tbl_df(training_results_2015) %>% group_by(Team) %>% summarise(For=as.integer(sum(Score))))
table_2015_results <- merge(table_2015_results,tbl_df(training_results_2015) %>% group_by(Opponent) %>% summarise(Against=as.integer(sum(Score))), by.x="Team", by.y="Opponent")
table_2015_results <- table_2015_results %>% mutate(Diff=as.integer(For-Against), Points=as.integer(Won*2+Drawn)) %>% arrange(desc(Points),desc(Diff))



#--- Functions 
update_training <- function(df, Date, HomeTeam, AwayTeam, HomeScore, AwayScore) {
  if(HomeScore=='-' | AwayScore=='-')
  {
    home_score = NA
    away_score = NA
    home_result = "NA"
    away_result = "NA"
  } else {
    home_score <- as.integer(HomeScore)
    away_score <- as.integer(AwayScore)
    home_result <- ifelse(home_score>away_score,"W",ifelse(home_score<away_score,"L","D"))
    away_result <- ifelse(away_score>home_score,"W",ifelse(away_score<home_score,"L","D"))
  }

  df <- rbind(df, data.frame(Date=Date,Team=HomeTeam,Opponent=AwayTeam,Score=home_score,Home=1,Result=home_result))
  df <- rbind(df, data.frame(Date=Date,Team=AwayTeam,Opponent=HomeTeam,Score=away_score,Home=0,Result=away_result))
  return(df)
}
extract_inputs <- function(updated_training, input, output) {
  #Round 1
  updated_training <- update_training(updated_training,"6 February 2016","France","Italy",input$FranceHome1,input$ItalyAway1)
  updated_training <- update_training(updated_training,"6 February 2016","Scotland","England",input$ScotlandHome1,input$EnglandAway1)
  updated_training <- update_training(updated_training,"7 February 2016","Ireland","Wales",input$IrelandHome1,input$WalesAway1)
  #Round 2
  updated_training <- update_training(updated_training,"13 February 2016","France","Ireland",input$FranceHome2,input$IrelandAway2)
  updated_training <- update_training(updated_training,"13 February 2016","Wales","Scotland",input$WalesHome2,input$ScotlandAway2)
  updated_training <- update_training(updated_training,"14 February 2016","Italy","England",input$ItalyHome2,input$EnglandAway2)
  #Round 3
  updated_training <- update_training(updated_training,"26 February 2016","Wales","France",input$WalesHome3,input$FranceAway3)
  updated_training <- update_training(updated_training,"27 February 2016","Italy","Scotland",input$ItalyHome3,input$ScotlandAway3)
  updated_training <- update_training(updated_training,"27 February 2016","England","Ireland",input$EnglandHome3,input$IrelandAway3)
  #Round 4
  updated_training <- update_training(updated_training,"12 March 2016","Ireland","Italy",input$IrelandHome4,input$ItalyAway4)
  updated_training <- update_training(updated_training,"12 March 2016","England","Wales",input$EnglandHome4,input$WalesAway4)
  updated_training <- update_training(updated_training,"13 March 2016","Scotland","France",input$ScotlandHome4,input$FranceAway4)
  #Round 5
  updated_training <- update_training(updated_training,"19 March 2016","Wales","Italy",input$WalesHome5,input$ItalyAway5)
  updated_training <- update_training(updated_training,"19 March 2016","Ireland","Scotland",input$IrelandHome5,input$ScotlandAway5)
  updated_training <- update_training(updated_training,"19 March 2016","France","England",input$FranceHome5,input$EnglandAway5)
  
  class(updated_training$Score) <- "integer"
  class(updated_training$Home) <- "integer"
  class(updated_training$Score) <- "integer"
  class(updated_training$Home) <- "integer"
  
  return(updated_training)
}

#--- Functions to reset the selectInput widgets
# Send an update message to a Select input on the client.
# This update message can change the value and/or label.
updateSelectInput <- function(session, inputId,
                           label = NULL, value = NULL) {
  
  message <- dropNulls(list(label = label, value = value))
  session$sendInputMessage(inputId, message)
}


# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

shinyServer(
  function(input, output, session) {
    observe({
      # Run whenever reset button is pressed
      if(input$remove)
      {
        # Send an update to my_url, resetting its value
        updateSelectInput(session, "FranceHome1", value = "-")
        updateSelectInput(session, "FranceHome2", value = "-")
        updateSelectInput(session, "FranceHome3", value = "-")
        updateSelectInput(session, "FranceHome4", value = "-")
        updateSelectInput(session, "FranceHome5", value = "-")
        updateSelectInput(session, "FranceAway1", value = "-")
        updateSelectInput(session, "FranceAway2", value = "-")
        updateSelectInput(session, "FranceAway3", value = "-")
        updateSelectInput(session, "FranceAway4", value = "-")
        updateSelectInput(session, "FranceAway5", value = "-")
        updateSelectInput(session, "ItalyHome1", value = "-")
        updateSelectInput(session, "ItalyHome2", value = "-")
        updateSelectInput(session, "ItalyHome3", value = "-")
        updateSelectInput(session, "ItalyHome4", value = "-")
        updateSelectInput(session, "ItalyHome5", value = "-")
        updateSelectInput(session, "ItalyAway1", value = "-")
        updateSelectInput(session, "ItalyAway2", value = "-")
        updateSelectInput(session, "ItalyAway3", value = "-")
        updateSelectInput(session, "ItalyAway4", value = "-")
        updateSelectInput(session, "ItalyAway5", value = "-")
        updateSelectInput(session, "EnglandHome1", value = "-")
        updateSelectInput(session, "EnglandHome2", value = "-")
        updateSelectInput(session, "EnglandHome3", value = "-")
        updateSelectInput(session, "EnglandHome4", value = "-")
        updateSelectInput(session, "EnglandHome5", value = "-")
        updateSelectInput(session, "EnglandAway1", value = "-")
        updateSelectInput(session, "EnglandAway2", value = "-")
        updateSelectInput(session, "EnglandAway3", value = "-")
        updateSelectInput(session, "EnglandAway4", value = "-")
        updateSelectInput(session, "EnglandAway5", value = "-")
        updateSelectInput(session, "IrelandHome1", value = "-")
        updateSelectInput(session, "IrelandHome2", value = "-")
        updateSelectInput(session, "IrelandHome3", value = "-")
        updateSelectInput(session, "IrelandHome4", value = "-")
        updateSelectInput(session, "IrelandHome5", value = "-")
        updateSelectInput(session, "IrelandAway1", value = "-")
        updateSelectInput(session, "IrelandAway2", value = "-")
        updateSelectInput(session, "IrelandAway3", value = "-")
        updateSelectInput(session, "IrelandAway4", value = "-")
        updateSelectInput(session, "IrelandAway5", value = "-")
        updateSelectInput(session, "WalesHome1", value = "-")
        updateSelectInput(session, "WalesHome2", value = "-")
        updateSelectInput(session, "WalesHome3", value = "-")
        updateSelectInput(session, "WalesHome4", value = "-")
        updateSelectInput(session, "WalesHome5", value = "-")
        updateSelectInput(session, "WalesAway1", value = "-")
        updateSelectInput(session, "WalesAway2", value = "-")
        updateSelectInput(session, "WalesAway3", value = "-")
        updateSelectInput(session, "WalesAway4", value = "-")
        updateSelectInput(session, "WalesAway5", value = "-")
        updateSelectInput(session, "ScotlandHome1", value = "-")
        updateSelectInput(session, "ScotlandHome2", value = "-")
        updateSelectInput(session, "ScotlandHome3", value = "-")
        updateSelectInput(session, "ScotlandHome4", value = "-")
        updateSelectInput(session, "ScotlandHome5", value = "-")
        updateSelectInput(session, "ScotlandAway1", value = "-")
        updateSelectInput(session, "ScotlandAway2", value = "-")
        updateSelectInput(session, "ScotlandAway3", value = "-")
        updateSelectInput(session, "ScotlandAway4", value = "-")
        updateSelectInput(session, "ScotlandAway5", value = "-")  
      }
    })
    
    observe({
      # Run whenever reset button is pressed
      if(input$reset)
      {
        # Send an update to my_url, resetting its value
        updateSelectInput(session, "FranceHome1", value = "23")
        updateSelectInput(session, "FranceHome2", value = "10")
        updateSelectInput(session, "FranceHome3", value = "-")
        updateSelectInput(session, "FranceHome4", value = "-")
        updateSelectInput(session, "FranceHome5", value = "-")
        updateSelectInput(session, "FranceAway1", value = "-")
        updateSelectInput(session, "FranceAway2", value = "-")
        updateSelectInput(session, "FranceAway3", value = "-")
        updateSelectInput(session, "FranceAway4", value = "-")
        updateSelectInput(session, "FranceAway5", value = "-")
        updateSelectInput(session, "ItalyHome1", value = "-")
        updateSelectInput(session, "ItalyHome2", value = "9")
        updateSelectInput(session, "ItalyHome3", value = "-")
        updateSelectInput(session, "ItalyHome4", value = "-")
        updateSelectInput(session, "ItalyHome5", value = "-")
        updateSelectInput(session, "ItalyAway1", value = "21")
        updateSelectInput(session, "ItalyAway2", value = "-")
        updateSelectInput(session, "ItalyAway3", value = "-")
        updateSelectInput(session, "ItalyAway4", value = "-")
        updateSelectInput(session, "ItalyAway5", value = "-")
        updateSelectInput(session, "EnglandHome1", value = "-")
        updateSelectInput(session, "EnglandHome2", value = "-")
        updateSelectInput(session, "EnglandHome3", value = "-")
        updateSelectInput(session, "EnglandHome4", value = "-")
        updateSelectInput(session, "EnglandHome5", value = "-")
        updateSelectInput(session, "EnglandAway1", value = "15")
        updateSelectInput(session, "EnglandAway2", value = "40")
        updateSelectInput(session, "EnglandAway3", value = "-")
        updateSelectInput(session, "EnglandAway4", value = "-")
        updateSelectInput(session, "EnglandAway5", value = "-")
        updateSelectInput(session, "IrelandHome1", value = "16")
        updateSelectInput(session, "IrelandHome2", value = "-")
        updateSelectInput(session, "IrelandHome3", value = "-")
        updateSelectInput(session, "IrelandHome4", value = "-")
        updateSelectInput(session, "IrelandHome5", value = "-")
        updateSelectInput(session, "IrelandAway1", value = "16")
        updateSelectInput(session, "IrelandAway2", value = "9")
        updateSelectInput(session, "IrelandAway3", value = "-")
        updateSelectInput(session, "IrelandAway4", value = "-")
        updateSelectInput(session, "IrelandAway5", value = "-")
        updateSelectInput(session, "WalesHome1", value = "-")
        updateSelectInput(session, "WalesHome2", value = "27")
        updateSelectInput(session, "WalesHome3", value = "-")
        updateSelectInput(session, "WalesHome4", value = "-")
        updateSelectInput(session, "WalesHome5", value = "-")
        updateSelectInput(session, "WalesAway1", value = "16")
        updateSelectInput(session, "WalesAway2", value = "-")
        updateSelectInput(session, "WalesAway3", value = "-")
        updateSelectInput(session, "WalesAway4", value = "-")
        updateSelectInput(session, "WalesAway5", value = "-")
        updateSelectInput(session, "ScotlandHome1", value = "9")
        updateSelectInput(session, "ScotlandHome2", value = "-")
        updateSelectInput(session, "ScotlandHome3", value = "-")
        updateSelectInput(session, "ScotlandHome4", value = "-")
        updateSelectInput(session, "ScotlandHome5", value = "-")
        updateSelectInput(session, "ScotlandAway1", value = "-")
        updateSelectInput(session, "ScotlandAway2", value = "23")
        updateSelectInput(session, "ScotlandAway3", value = "-")
        updateSelectInput(session, "ScotlandAway4", value = "-")
        updateSelectInput(session, "ScotlandAway5", value = "-") 
      }
    })
    
    
    #--- First Panel Table output
    output$Result2015Table <- renderTable(table_2015_results)

    updated_training_dataset <- reactive({return(extract_inputs(as.data.frame(training_results_2015),input,output))})

    #--- Second Panel Table output
    output$ChampionshipTable <- renderTable({
      #--- 2016 Championship Table creation
      is_na_idx <- which(is.na(updated_training_dataset()$Score))
      training_results_2016 <- updated_training_dataset()[-is_na_idx,]
      training_results_2016 <- training_results_2016[grep('2016',training_results_2016$Date),]


      table_2016_results <- tbl_df(training_results_2016)  %>% group_by(Team) %>% summarise(Played=n())
      table_2016_results <- merge(table_2016_results,tbl_df(training_results_2016) %>% group_by(Team) %>% summarise(Won=as.integer(sum(Result=="W"))))
      table_2016_results <- merge(table_2016_results,tbl_df(training_results_2016) %>% group_by(Team) %>% summarise(Drawn=as.integer(sum(Result=="D"))))
      table_2016_results <- merge(table_2016_results,tbl_df(training_results_2016) %>% group_by(Team) %>% summarise(Lost=as.integer(sum(Result=="L"))))
      table_2016_results <- merge(table_2016_results,tbl_df(training_results_2016) %>% group_by(Team) %>% summarise(For=as.integer(sum(Score))))
      table_2016_results <- merge(table_2016_results,tbl_df(training_results_2016) %>% group_by(Opponent) %>% summarise(Against=as.integer(sum(Score))), by.x="Team", by.y="Opponent")
      table_2016_results <- table_2016_results %>% mutate(Diff=as.integer(For-Against), Points=as.integer(Won*2+Drawn)) %>% arrange(desc(Points),desc(Diff))
      return(table_2016_results)
    })
    
    
    #--- Third Panel Table output
    output$ThirdPanelMessageValue <- renderText({
      is_na_idx <- which(is.na(updated_training_dataset()$Score))
      new_training_df <- updated_training_dataset()[-is_na_idx,]
      n <- nrow(new_training_df)
      return(sprintf("The training dataset contains %d rows.", n))
    })
    
    #--- Third Panel Table output
    output$TrainingTable <- renderTable({
      is_na_idx <- which(is.na(updated_training_dataset()$Score))
      new_training_df <- updated_training_dataset()[-is_na_idx,]
      rownames(new_training_df) <- NULL
      return(new_training_df)
    })
    
    #--- Fourth Panel Message output
    output$FourthPanelMessageValue <- renderText({
      is_na_idx <- which(is.na(updated_training_dataset()$Score))
      new_testing_df <- updated_training_dataset()[is_na_idx,] 
      rownames(new_testing_df) <- NULL
      if(is.null(new_testing_df)) {
        return("Press 'Update Prediction!' to define the training and test datasets for the predictions.")
      } else if(nrow(new_testing_df)==0) {
        return("There is no more matches remaining in the fixtures to predict")
      } else {
        n <- nrow(new_testing_df)
        return(sprintf("There are %d match outcome%s to predict.", n/2, ifelse(n>0,"s","")))
      }
    })
    #--- Fourth panel prediction table message
    output$PredictionTableMessage <- renderText({if(input$PredictButton) {
      isolate({"The table below shows the predicted home team and away team scoring rates (e.g. Lambda, Mu mean values predicted from the GLM Poisson model), the win/draw/lose' prediction probability and decimal odd values."})
    }})
    
    #--- Fourth panel prediction table
    pred_tbl <- NULL
    model <- NULL
    output$PredictionTable <- renderTable({
      if(input$PredictButton) {
        isolate({
          ### - Header Message
          
          is_na_idx <- which(is.na(updated_training_dataset()$Score))
          
          ### Training
          new_training_df <- updated_training_dataset()[-is_na_idx,]
          new_training_df <- new_training_df %>% arrange(Home)
          model <<- glm(Score ~ Home + Team + Opponent, data=new_training_df, family=poisson(link="log"))
          
          ### Prediction with the Home==1 rows
          new_testing_df <- updated_training_dataset()[is_na_idx,] %>% filter(Home==1)
          
          maxScore <- 65
          prob_matrix <- matrix(0.0, nrow=maxScore+1, ncol=maxScore+1)
          lambda <- 0.0
          mu <- 0.0
          
          predict_match <- function(date_, team, opponent) {
            #-Expected Home Scores
            lambda <<- predict(model, data.frame(Home=1, Team=team, Opponent=opponent), type="response")
            #-Expected Away Scores
            mu <<- predict(model, data.frame(Home=0, Team=opponent, Opponent=team), type="response")
            prob_matrix <<- dpois(0:maxScore, lambda) %*% t(dpois(0:maxScore, mu))
            HomeWinPr <- sum(prob_matrix[lower.tri(prob_matrix)])
            DrawPr <- sum(diag(prob_matrix))
            AwayWinPr <- sum(prob_matrix[upper.tri(prob_matrix)])
            #return(data.frame(Date=date_,Team=team,Opponent=opponent,WinPr=HomeWinPr,DrawPr=DrawPr,LosePr=AwayWinPr))
            return(c(HomeWinPr,DrawPr,AwayWinPr,lambda,mu))
          }
          
          #fixtures <- data.frame(HomeTeam="Italy", AwayTeam="France", Win=0, Draw=0, Lose=0)
          match_result_probs <- apply(new_testing_df, 1, function(x) predict_match(x['Date'],x['Team'],x['Opponent']))
          match_result_dec_odds<- round(1/match_result_probs,4)
          
          tbl_output <- data.frame(Date=new_testing_df$Date,`Home Team`=new_testing_df$Team,`Away Team`=new_testing_df$Opponent)
          tbl_output["lambda"] <- match_result_probs[4,]
          tbl_output["mu"] <- match_result_probs[5,]
          
          tbl_output["Win"] <- match_result_probs[1,]
          tbl_output["Draw"] <- match_result_probs[2,]
          tbl_output["Lose"] <- match_result_probs[3,]

          tbl_output["Win Odd"] <- match_result_dec_odds[1,]
          tbl_output["Draw Odd"] <- match_result_dec_odds[2,]
          tbl_output["Lose Odd"] <- match_result_dec_odds[3,]
          names(tbl_output) <- c('Date','Home Team','Away Team','lambda','mu','Win','Draw','Lose','Win Odd','Draw Odd','Lose Odd')
          
          pred_tbl <<- tbl_output
          return(tbl_output)
        })
      }
    })
    
    #--- Fourth Panel Prediction Plots
    output$PredictionPlots <- renderPlot({
      if(input$PredictButton) {
        isolate({
          #--- Plot 1
          fixtures <- pred_tbl
          mplt <- melt(fixtures,id.vars = c("Date","Home Team","Away Team"),measure.vars = c("Win","Draw","Lose"), variable.name = "Home Result")
          mplt["match"] <- with(mplt,{paste(Date,`Home Team`,`Away Team`,sep=" | ")})
          g1 <- ggplot(data=mplt, aes(x=factor(match,levels=rev(match)), y= value, fill=`Home Result`))
          g1 <- g1 + geom_bar(stat = "identity")+coord_flip()
          g1 <- g1 + theme_bw(base_family = "Avenir", base_size = 15)
          g1 <- g1 + xlab("") + ylab("Probability") + ggtitle("Match Prediction")
          g1 <- g1 + theme(legend.position = "bottom", plot.margin=unit(c(0.5,1,1,1), "cm"))  + labs(aesthetic='custom text') 
          
          #--- Plot 2
          coeff <- coef(model)
          coefOff <- coeff[grep('Team',names(coeff))]
          coefDef <- coeff[grep('Opponent',names(coeff))]
          names(coefOff) <- gsub('Team','',names(coefOff))
          names(coefDef) <- gsub('Opponent','',names(coefDef))
          
          # England is missing, so it is added there
          coeffDf <- rbind(cbind(coefOff,coefDef), matrix(c(0,0), nrow=1,dimnames=list('England',c('coefOff','coefDef'))))
          
          # scaled to show relative strength 
          coeffDf <- as.data.frame(scale(coeffDf,scale=FALSE)) 
          coeffDf["Team"] <- row.names(coeffDf)
          row.names(coeffDf) <- NULL
          
          #
          g2 <- ggplot(data=coeffDf,aes(x=coefOff,y=-coefDef,label=Team,color=Team)) 
          g2 <- g2 + theme_bw(base_family = "Avenir", base_size = 15)
          g2 <- g2 + xlab('Offensive Strength') + ylab('Defensive Strength') + ggtitle("Relative Strength (Scaled From GLM Coefficients)")
          g2 <- g2 + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
          g2 <- g2 + geom_text(size = 6) 
          g2 <- g2 + theme(legend.position="none", plot.margin=unit(c(0.5,1,1,1), "cm"))
          
          
          grid.arrange( g1, g2, nrow=1, widths=c(1,1))
        })
      }
    }, height = 400, width = 1000)
  }
)