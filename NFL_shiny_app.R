library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(zoo)
library(reshape2)


# getData--------------
pbp         <- readRDS("data/pbp2018.rds")
games       <- readRDS("data/games2018.rds")


# enrichData------------

# add the week and final score data to the play-by-play
pbp <- merge(pbp, games[,c("game_id","week","home_score","away_score")], by.x = "GameID",by.y = "game_id", all.x = TRUE)


# setStatics-------------
teams <- unique(pbp$HomeTeam); teams <- teams[order(teams)]
weeks <- unique(pbp$week)    ; weeks <- weeks[order(weeks)]
pages <- c("Game","Offense","Defense")




# Define UI for app ----
ui <- dashboardPage(
  
  dashboardHeader(title = "NFL Game Analysis"),
  dashboardSidebar(
    
    selectInput("inputTeam", label = h3("Team"), 
                choices = teams, 
                selected = teams[1]),
    
    selectInput("inputWeek", label = h3("Week"), 
                choices = weeks, 
                selected = weeks[1]),
    
    sidebarMenu(
    
    menuItem("Game Info", icon = icon("th"), tabName = "gameInfo"),
    menuItem("Offense", icon = icon("th"), tabName = "offense"),
    menuItem("Defense", icon = icon("th"), tabName = "defense")
    
    )
    
  ),
  dashboardBody(
    
    
    tabItems(
      tabItem(tabName = "gameInfo",
              
              fluidRow(
                
                column(width = 6,
                       valueBoxOutput("homeFinalScore", width = NULL),
                       valueBoxOutput("awayFinalScore", width = NULL),
                       box(dataTableOutput("gameStoryTable"),width = NULL, title = "Game Story")
                       ),

                column(width = 6,
                       box(plotOutput(outputId = "scoreEvolPlot"),width = NULL, title = "Score Difference"),
                       box(plotOutput(outputId = "winPercPlot"),width = NULL, title = "Win % by Team")
                       )

              )
              
              
      ),
      
      tabItem(tabName = "offense",
              
              fluidRow(
                 box(plotOutput(outputId = "offenseEvolPlot"), width = 5, title = "Yards Gained Breakdown"),
                 box(plotOutput(outputId = "receivingYardsPlot"), width = 4, title = "Receiving (Total & Air) Yards by Receiver"),
                 box(plotOutput(outputId = "rushingYardsPlot"), width = 3,  title = "Rushing Yards (+ Touches) by Rusher")
               ),
              
              fluidRow(
                 box(plotOutput(outputId = "offenseDriveEvolPlot"), width = 5, title = "Drive Evolution"),
                 box(plotOutput(outputId = "yardsAfterCatch"), width = 4, title = "Air + Ground Yards on Passing Plays"),
                 box(plotOutput(outputId = "offenseRunPassPie"), width = 3, title = "Run/Pass Chosen % ")
              )
      ),
      
      tabItem(tabName = "defense",
              fluidRow(
                box(plotOutput(outputId = "defenseTimeline"), width = 5, title = "Defensive Events by Quarter"),
                box(plotOutput(outputId = "tacklerBar"), width = 7, title = "Tackles by player")
              ),
              
              fluidRow(
                box(plotOutput(outputId = "defenseDriveEvolPlot"), width = 5, title = "Drive Evolution"),
                box(plotOutput(outputId = "yardsAllowed"), width = 4, title = "Yards Allowed per Drive"),
                box(plotOutput(outputId = "defenseRunPassPie"), width = 3, title = "Run/Pass Faced %")
              )
      )
      
      
    )
    

    
  )
  
)


# Define server logic ----
server <- function(input, output) {
  
  
  # -------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------
  # GAME INFO
  # -------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------
  
  # --------------------------------
  # Home team score box
  # --------------------------------
  output$homeFinalScore <- renderValueBox({
    
    #get the data
    score_final <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam)
    
    homeTeam  = score_final$HomeTeam[1]
    homeScore = score_final$home_score[1]
    
    col = "orange"
    if(homeScore > score_final$away_score[1]){
      col = "olive"
    }
    if(homeScore < score_final$away_score[1]){
      col = "red"
    }
    
    valueBox(
      value = homeScore, subtitle = paste(homeTeam, "Home",sep = ", "), color = col
    )
  })
  
  # --------------------------------
  # Away team score box
  # --------------------------------
  output$awayFinalScore <- renderValueBox({
    
    #get the data
    score_final <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam)
    
    awayTeam  = score_final$AwayTeam[1]
    awayScore = score_final$away_score[1]
    
    col = "orange"
    if(awayScore > score_final$home_score[1]){
      col = "olive"
    }
    if(awayScore < score_final$home_score[1]){
      col = "red"
    }
    
    valueBox(
     value = awayScore, subtitle =  paste(awayTeam, "Away",sep = ", "), color = col
    )
  })
  
  # --------------------------------
  # Plot the score evolution
  # --------------------------------
  output$scoreEvolPlot <- renderPlot({
    
    #get the data
    score_timeline <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam)
    
    #compute the time (secs) through the game
    score_timeline$Game.time.total <- max(score_timeline$TimeSecs) - score_timeline$TimeSecs
    
    #LOCF the abs score diff
    score_timeline$AbsScoreDiff <- na.locf(score_timeline$AbsScoreDiff)
     
    #plot
    ggplot()+
      geom_line(data = score_timeline, aes(x = Game.time.total, y = AbsScoreDiff, colour = -AbsScoreDiff ), size = 1.5) +
      geom_vline(xintercept = 0, linetype="dotted", color = "grey", size = 1.5) +
      geom_vline(xintercept = 15*60, linetype="dotted", color = "grey", size = 1.5) +
      geom_vline(xintercept = 30*60, linetype="dotted", color = "grey", size = 1.5) +
      geom_vline(xintercept = 45*60, linetype="dotted", color = "grey", size = 1.5) +
      geom_vline(xintercept = 60*60, linetype="dotted", color = "grey", size = 1.5) +
      xlab("Game time (secs)") + ylab("Score Difference")
    
  })
  
  # --------------------------------
  # Game story table
  # --------------------------------
  output$gameStoryTable <- renderDataTable(
    
    #get the data
    pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% select(desc)
  
  )
  
  
  
  # --------------------------------
  # Plot the win percentage evolution
  # --------------------------------
  output$winPercPlot <- renderPlot({
    
    #get the data
    win_perc_timeline <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam)
    
    #compute the time (secs) through the game
    win_perc_timeline$Game.time.total <- max(win_perc_timeline$TimeSecs) - win_perc_timeline$TimeSecs
    
    #get the data in tall format to plot
    filtered <-  melt(win_perc_timeline[,c("Home_WP_post","Away_WP_post","Game.time.total")] , id.vars = c("Game.time.total"))
    
    #plot
    ggplot(data = filtered, aes(x = Game.time.total,y = value, col = variable))+
      geom_line(size = 0.8, alpha = 0.5 ) +
      geom_point() +
      geom_vline(xintercept = 0, linetype="dotted", color = "grey", size = 1.5) +
      geom_vline(xintercept = 15*60, linetype="dotted", color = "grey", size = 1.5) +
      geom_vline(xintercept = 30*60, linetype="dotted", color = "grey", size = 1.5) +
      geom_vline(xintercept = 45*60, linetype="dotted", color = "grey", size = 1.5) +
      geom_vline(xintercept = 60*60, linetype="dotted", color = "grey", size = 1.5) +
      xlab("Game time (secs)") + ylab("Win %") +
      theme(legend.position="right")
    
  })
  
  # -------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------
  # OFFENSE
  # -------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------
  
  
  # --------------------------------
  # Plot the offense yeards gained evolution
  # --------------------------------
  output$offenseEvolPlot <- renderPlot({
    
    #get the data
    yards_quarter_play <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(PlayType == "Run" | PlayType == "Pass") %>%
      filter(posteam == input$inputTeam) %>% filter( is.na(PassOutcome) | PassOutcome == "Complete")
    
    #compute the time (secs) through the quarter
    yards_quarter_play$Game.time = abs(as.numeric(substr(yards_quarter_play$time,1,2))*60 + as.numeric(substr(yards_quarter_play$time,4,5)) - 15*60)
    
    #if a touchdown is scores, get the scorer name
    yards_quarter_play$Scorer.Name = mapply(
      function(td, rush, catch){
        if(td == 1){
          if(!is.na(rush)) return(rush)
          else if(!is.na(catch)) return(catch)
          else return(NA)
        }else{
          return(NA)
        }
      },
      yards_quarter_play$Touchdown,
      yards_quarter_play$Rusher,
      yards_quarter_play$Receiver
    )
    
    #plot
    ggplot() +
      geom_point(data = yards_quarter_play, 
                 aes(x = Game.time,y = Yards.Gained, shape = factor(PlayType), color = PlayType, size = Touchdown == 1 ) 
      ) + 
      scale_size_manual(values=c(2,5)) +
      facet_wrap(vars(qtr)) +
      geom_text(data=subset(yards_quarter_play, !is.na(Scorer.Name) ), aes(x = Game.time,y = Yards.Gained,label = Scorer.Name), size = 3) +
      ggtitle(paste(yards_quarter_play$posteam[1], "Offense vs", yards_quarter_play$DefensiveTeam[1])) +
      xlab("Time through Q (secs)") + ylab("Yards")
    
    
  })
  
  # --------------------------------
  # Bar plot of receiving yards by receiver (with overlaid Air Yards)
  # --------------------------------
  output$receivingYardsPlot <- renderPlot({
    
    #get the data
    yards_receiving_player <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(PlayType == "Run" | PlayType == "Pass") %>%
      filter(posteam == input$inputTeam) %>% filter( is.na(PassOutcome) | PassOutcome == "Complete")
    
    #compute the time (secs) through the quarter
    yards_receiving_player$Game.time = abs(as.numeric(substr(yards_receiving_player$time,1,2))*60 + as.numeric(substr(yards_receiving_player$time,4,5)) - 15*60)
    
    #only pass plays and group by receiver
    yards_receiving_player <- yards_receiving_player %>% filter(PlayType == "Pass") %>% group_by(Receiver)
    
    #count the total yards, air yards, and number of catches
    yards_receiving_player_to_plot <- yards_receiving_player %>% summarise(rec_yards = sum(Yards.Gained),
                                                                           AirYards = sum(AirYards),
                                                                           catches = n())
    #plot
    ggplot(data = yards_receiving_player_to_plot) +
      geom_col(aes(x = reorder(Receiver, -rec_yards), y = rec_yards,fill = Receiver)) +
      geom_col(aes(x = reorder(Receiver, -rec_yards), y = AirYards ),fill = "grey", colour = "black", alpha = 0.5) +
      xlab("Receiver") + ylab("Yards") + theme(legend.position = "none")

  })
  
  # --------------------------------
  # Bar plot of rush yards by rusher
  # --------------------------------
  output$rushingYardsPlot <- renderPlot({
    
    #get the data
    yards_rushing_player <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(PlayType == "Run" | PlayType == "Pass") %>%
      filter(posteam == input$inputTeam) %>% filter( is.na(PassOutcome) | PassOutcome == "Complete")
    
    #compute the time (secs) through the quarter
    yards_rushing_player$Game.time = abs(as.numeric(substr(yards_rushing_player$time,1,2))*60 + as.numeric(substr(yards_rushing_player$time,4,5)) - 15*60)
    
    #only rush plays and group by player
    yards_rushing_player <- yards_rushing_player %>% filter(PlayType == "Run") %>% group_by(Rusher)
    
    #count rushing yards and touches by player
    yards_rushing_player_to_plot <- yards_rushing_player %>% summarise(rush_yards = sum(Yards.Gained),
                                                                       touches = n()  )
    #plot
    ggplot(data = yards_rushing_player_to_plot) +
      geom_col(aes(x = reorder(Rusher, -rush_yards), y = rush_yards, fill = Rusher), alpha = 0.8) +
      geom_point(aes(x = reorder(Rusher, -rush_yards),y = touches), size = 4, shape = 12,color = "darkblue" )+
      xlab("Rusher") + ylab("Yards") + theme(legend.position = "none")
    
  })
  
  # --------------------------------
  # Offense: Line plots of drives by quarter
  # --------------------------------
  output$offenseDriveEvolPlot <- renderPlot({
    
    #get the data
    drive_evolution <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(posteam == input$inputTeam)
    
    #make sure our data is only downs 1,2,3,4
    drive_evolution <- drive_evolution[!is.na(drive_evolution$down),]
    
    #compute the time (secs) through the quarter
    drive_evolution$Game.time = abs(as.numeric(substr(drive_evolution$time,1,2))*60 + as.numeric(substr(drive_evolution$time,4,5)) - 15*60)
    
    #we need to uniform the yard line to 0-100 (rather than TEAM 0-50)
    drive_evolution$yrdline100 <- mapply(
      function(team, line, yards){
        if(input$inputTeam == team) return(line + yards)
        else return(100-line + yards)
      },
      drive_evolution$SideofField,
      drive_evolution$yrdln,
      drive_evolution$Yards.Gained
    )
    
    #if a touchdown is scored, extract the scorer
    drive_evolution$Scorer.Name = mapply(
      function(td, rush, catch){
        if(td == 1){
          if(!is.na(rush)) return(rush)
          else if(!is.na(catch)) return(catch)
          else return(NA)
        }else{
          return(NA)
        }
      },
      drive_evolution$Touchdown,
      drive_evolution$Rusher,
      drive_evolution$Receiver
    )
    
    #extract the pick sixes so we can overlay them
    pick_six <- drive_evolution[!is.na(drive_evolution$Interceptor) & drive_evolution$Touchdown == 1,]
    
    #remove pick sixes from main data set
    drive_evolution <- drive_evolution[!(!is.na(drive_evolution$Interceptor) & drive_evolution$Touchdown == 1),]
    
    #plot
    ggplot(data = drive_evolution, aes(x = Game.time, y = yrdline100,group = Drive, colour = PlayType, shape = down,size = Touchdown == 1)) +
      scale_size_manual(values=c(2,5)) +
      geom_line(size = 1, color = "black") +
      geom_point() +
      geom_point(data = pick_six, color = "black", size = 6) + 
      geom_text(data = pick_six, label = "p6", color = "red", size = 5) +
      facet_wrap(vars(qtr)) +
      geom_text(data=subset(drive_evolution, !is.na(Scorer.Name) ), aes(x = Game.time,y = yrdline100,label = Scorer.Name), size = 3, colour = "black") +
      xlab("Time through Q (secs)") + ylab("Yard Line") +
      ggtitle(paste(drive_evolution$posteam[1], "Offense vs", drive_evolution$DefensiveTeam[1]))
    
    
  })
  
  # --------------------------------
  # Pass play air yards and total yards
  # --------------------------------
  output$yardsAfterCatch <- renderPlot({
    
    #get the data
    yards_after_catch <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(PlayType == "Run" | PlayType == "Pass") %>%
      filter(posteam == input$inputTeam) %>% filter( is.na(PassOutcome) | PassOutcome == "Complete")
    
    #only pass plays
    yards_after_catch <- yards_after_catch %>% filter(PlayType == "Pass")
    
    #get the data into tall format to plot
    filtered <- melt(yards_after_catch, id.vars = c("AirYards","YardsAfterCatch","qtr","Passer","Receiver"),measure.vars = c("play_id"))
    
    #plot
    ggplot(data = filtered,aes(x = 1:nrow(filtered))) + 
      geom_col(aes( y = AirYards + YardsAfterCatch)) + 
      geom_col(aes(y = AirYards, fill = qtr), size = 2) +
      geom_text(aes(y = AirYards + YardsAfterCatch,label = paste(Passer,Receiver, sep = "/") ), size = 3, angle = 90, nudge_y = 5)+
      xlab("Pass Play Count") + ylab("Yards")
    
  })
  
  # --------------------------------
  # Offense: Run/Pass pie chart
  # --------------------------------
  output$offenseRunPassPie <- renderPlot({
    
    #get the data
    run_pass_pie <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(PlayType == "Run" | PlayType == "Pass") %>%
      filter(posteam == input$inputTeam)
    
    #count the number of run/pass plays
    filtered <- run_pass_pie %>% group_by(PlayType) %>% summarize(plays = n())
    
    #plot
    ggplot(filtered, aes(x="", y=plays, fill=PlayType))+
      geom_bar(width = 1, stat = "identity") + 
      coord_polar("y", start=0) 

  })
  
  
  # -------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------
  # DEFENSE
  # -------------------------------------------------------------------------------
  # -------------------------------------------------------------------------------
  
  
  # --------------------------------
  # Defensive Events Timeline
  # --------------------------------
  output$defenseTimeline <- renderPlot({
    
    #get the data
    def_timeline <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(DefensiveTeam == input$inputTeam)

    #compute the time (secs) through the quarter
    def_timeline$Game.time = abs(as.numeric(substr(def_timeline$time,1,2))*60 + as.numeric(substr(def_timeline$time,4,5)) - 15*60)

    # count the number of QB hits, interceptions, sacks and safetys
    filtered <- def_timeline %>% group_by(qtr) %>% summarize(QBHit = sum(QBHit),
                                                             Interception = sum(InterceptionThrown),
                                                             Sack = sum(Sack),
                                                             Safety = sum(Safety)
                                                             )
    #get data into tall format to plot
    filtered_to_plot <- melt(filtered, measure.vars = c("QBHit","Interception","Sack","Safety"))
    
    #plot
    ggplot(data = filtered_to_plot) +
      geom_col( aes(x = variable, y = value, fill = variable) ) +
      facet_wrap(vars(qtr)) +
      xlab("Event") + ylab("Count")

  })
  
  
  # --------------------------------
  # Tackler Bar Chart
  # --------------------------------
  output$tacklerBar <- renderPlot({
    
    #get the data
    tackler_pie <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(DefensiveTeam == input$inputTeam)
    
    #count the number of tackles by main tackler in play
    filtered <- tackler_pie %>% group_by(Tackler1) %>% summarize(tackles = n())
    
    #exclude plays with no tackles
    filtered <- filtered[!is.na(filtered$Tackler1),]
    
    #plot
    ggplot( data = filtered )+
      geom_col( aes(x=reorder(Tackler1, -tackles), y=tackles, fill=-tackles) ) + 
      theme(axis.text.x = element_text(angle = 90)) +
      ylab("Tackles") + xlab("Tackler")
  })
  
  # --------------------------------
  # Yards Allowed per Drive Timeline
  # --------------------------------
  output$yardsAllowed <- renderPlot({
    
    #get the data
    yards_allowed <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(PlayType == "Run" | PlayType == "Pass") %>%
      filter(DefensiveTeam == input$inputTeam)
    
    #make a separate column for rushYards, so we can sum over drives
    yards_allowed$rushYards <- mapply(
      function(yards, type) if(type == "Run") return(yards) else return(0),
      yards_allowed$Yards.Gained,
      yards_allowed$PlayType
    )
    
    #summarize total yards allowed per drive, and of those yards, how many were rush yards
    filtered <- yards_allowed %>% group_by(Drive) %>% summarize(yardsAllowed = sum(Yards.Gained),
                                                                rushYardsAllowed = sum(rushYards))
    #index the drive numbers
    filtered$Drive <- 1:nrow(filtered)

    #plot
    ggplot(data = filtered) +
      geom_col( aes(x = Drive, y = yardsAllowed, fill = -yardsAllowed) ) +
      geom_col( aes(x = Drive, y = rushYardsAllowed ),fill = "darkred") +
      xlab("Drive") + ylab("Yards Allowed (red = rush sum)")
    
  })
  
  # --------------------------------
  # Defense: Line plots of drives by quarter
  # --------------------------------
  output$defenseDriveEvolPlot <- renderPlot({
    
    #get the data
    drive_evolution <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(DefensiveTeam == input$inputTeam)
    
    #make sure our data is only downs 1,2,3,4
    drive_evolution <- drive_evolution[!is.na(drive_evolution$down),]
    
    #compute the time (secs) through the quarter
    drive_evolution$Game.time = abs(as.numeric(substr(drive_evolution$time,1,2))*60 + as.numeric(substr(drive_evolution$time,4,5)) - 15*60)
    
    #we need to uniform the yard line to 0-100 (rather than TEAM 0-50)
    drive_evolution$yrdline100 <- mapply(
      function(team, line, yards){
        if(input$inputTeam == team) return(line - yards)
        else return(100-line - yards)
      },
      drive_evolution$SideofField,
      drive_evolution$yrdln,
      drive_evolution$Yards.Gained
    )
    
    #if a touchdown is scored, extract the scorer
    drive_evolution$Scorer.Name = mapply(
      function(td, rush, catch){
        if(td == 1){
          if(!is.na(rush)) return(rush)
          else if(!is.na(catch)) return(catch)
          else return(NA)
        }else{
          return(NA)
        }
      },
      drive_evolution$Touchdown,
      drive_evolution$Rusher,
      drive_evolution$Receiver
    )
    
    #extract the pick sixes so we can overlay them
    pick_six <- drive_evolution[!is.na(drive_evolution$Interceptor) & drive_evolution$Touchdown == 1,]
    
    #remove pick sixes from main data set
    drive_evolution <- drive_evolution[!(!is.na(drive_evolution$Interceptor) & drive_evolution$Touchdown == 1),]
    
    #plot
    ggplot(data = drive_evolution, aes(x = Game.time, y = yrdline100,group = Drive, colour = PlayType, shape = down,size = Touchdown == 1)) +
      scale_size_manual(values=c(2,5)) +
      geom_line(size = 1, color = "black") +
      geom_point() +
      geom_point(data = pick_six, color = "black", size = 6) + 
      geom_text(data = pick_six, label = "p6", color = "red", size = 5) +
      facet_wrap(vars(qtr)) +
      geom_text(data=subset(drive_evolution, !is.na(Scorer.Name) ), aes(x = Game.time,y = yrdline100,label = Scorer.Name), size = 3, colour = "black") +
      xlab("Time through Q (secs)") + ylab("Yard Line") +
      ggtitle(paste(drive_evolution$posteam[1], "Offense vs", drive_evolution$DefensiveTeam[1]))
  })
  
  # --------------------------------
  # Defense: Run/Pass pie chart
  # --------------------------------
  output$defenseRunPassPie <- renderPlot({
    
    #get the data
    run_pass_pie <- pbp %>% filter(week == as.numeric(input$inputWeek)) %>% filter(HomeTeam == input$inputTeam | AwayTeam == input$inputTeam) %>% filter(PlayType == "Run" | PlayType == "Pass") %>%
      filter(DefensiveTeam == input$inputTeam)
    
    #count number of run and pass plays (includes incomplete)
    filtered <- run_pass_pie %>% group_by(PlayType) %>% summarize(plays = n())
    
    #plot
    ggplot(filtered, aes(x="", y=plays, fill=PlayType))+
      geom_bar(width = 1, stat = "identity") + 
      coord_polar("y", start=0) 

  })
  
}

# Run the app
shinyApp(ui = ui, server = server)