#### load libraries ####
library(shiny)
library(tidyverse)
library(xtable)
library(zoo)
library(bslib)
library(ggsci)
library(dplyr)
library(ggplot2)
library(mosaic)

#### load and preproccess data ####

player_game_data <- read.csv('data/player_game_data.csv')
team_game_data <- read.csv('data/team_game_data.csv')
player_info <- read.csv('data/player_info.csv')
player_pics <- read.csv('data/PlayersPics.csv')

colnames(player_game_data)[1] <- "Starters"
colnames(team_game_data)[1] <- "Team"
colnames(player_info)[1] <- "Num"

# create stat_select data frame
stat_select <- player_game_data %>%
  select(BPM, PTS, FG, FGA, FG., X3P, X3PA, X3P., FT, FTA, FT., ORB, DRB, TRB, AST, STL, BLK, TOV, PF, TS., eFG., X3PAr, FTr, ORB., DRB., TRB., AST., STL., BLK., TOV., USG., ORtg, DRtg)

# add game_type column for playoff select
player_game_data$game_type[player_game_data$Playoff==0] <- "Regular Season"
player_game_data$game_type[player_game_data$Playoff==1] <- "Playoffs"
team_game_data$game_type[team_game_data$Playoff==0] <- "Regular Season"
team_game_data$game_type[team_game_data$Playoff==1] <- "Playoffs"

# add attendance_type for attendance select
player_game_data$attendance_type[player_game_data$empty==0] <- "Non-Empty Arenas"
player_game_data$attendance_type[player_game_data$empty==1] <- "Empty Arenas"
team_game_data$attendance_type[team_game_data$empty==0] <- "Non-Empty Arenas"
team_game_data$attendance_type[team_game_data$empty==1] <- "Empty Arenas"

# add bubble_type for attendance select
player_game_data$bubble_type[player_game_data$BubbleGame==0] <- "Non-Bubble Games"
player_game_data$bubble_type[player_game_data$BubbleGame==1] <- "Bubble Games"
team_game_data$bubble_type[team_game_data$BubbleGame==0] <- "Non-Bubble Games"
team_game_data$bubble_type[team_game_data$BubbleGame==1] <- "Bubble Games"

# create team_select_list
team_select <- sort(team_game_data %>% 
                      select(Team) %>% 
                      distinct(Team) %>% 
                      pull(Team))

# create player_select list
player_select <- sort(player_game_data %>%
                        select(Starters) %>%
                        distinct(Starters) %>%
                        pull(Starters))

# team logos
logos <- c("https://content.sportslogos.net/logos/6/220/thumbs/22081902021.gif",
           "https://content.sportslogos.net/logos/6/213/thumbs/slhg02hbef3j1ov4lsnwyol5o.gif",
           "https://content.sportslogos.net/logos/6/3786/thumbs/hsuff5m3dgiv20kovde422r1f.gif",
           "https://content.sportslogos.net/logos/6/5120/thumbs/512019262015.gif",
           "https://content.sportslogos.net/logos/6/221/thumbs/hj3gmh82w9hffmeh3fjm5h874.gif",
           "https://content.sportslogos.net/logos/6/222/thumbs/22269212018.gif",
           "https://content.sportslogos.net/logos/6/228/thumbs/22834632018.gif",
           "https://content.sportslogos.net/logos/6/229/thumbs/22989262019.gif",
           "https://content.sportslogos.net/logos/6/223/thumbs/22321642018.gif",
           "https://content.sportslogos.net/logos/6/235/thumbs/23531522020.gif",
           "https://content.sportslogos.net/logos/6/230/thumbs/23068302020.gif",
           "https://content.sportslogos.net/logos/6/224/thumbs/22448122018.gif",
           "https://content.sportslogos.net/logos/6/236/thumbs/23637762019.gif",
           "https://content.sportslogos.net/logos/6/237/thumbs/uig7aiht8jnpl1szbi57zzlsh.gif",
           "https://content.sportslogos.net/logos/6/231/thumbs/23143732019.gif",
           "https://content.sportslogos.net/logos/6/214/thumbs/burm5gh2wvjti3xhei5h16k8e.gif",
           "https://content.sportslogos.net/logos/6/225/thumbs/22582752016.gif",
           "https://content.sportslogos.net/logos/6/232/thumbs/23296692018.gif",
           "https://content.sportslogos.net/logos/6/4962/thumbs/496226812014.gif",
           "https://content.sportslogos.net/logos/6/216/thumbs/2nn48xofg0hms8k326cqdmuis.gif",
           "https://content.sportslogos.net/logos/6/2687/thumbs/khmovcnezy06c3nm05ccn0oj2.gif",
           "https://content.sportslogos.net/logos/6/217/thumbs/wd9ic7qafgfb0yxs7tem7n5g4.gif",
           "https://content.sportslogos.net/logos/6/218/thumbs/21870342016.gif",
           "https://content.sportslogos.net/logos/6/238/thumbs/23843702014.gif",
           "https://content.sportslogos.net/logos/6/239/thumbs/23997252018.gif",
           "https://content.sportslogos.net/logos/6/240/thumbs/24040432017.gif",
           "https://content.sportslogos.net/logos/6/233/thumbs/23325472018.gif",
           "https://content.sportslogos.net/logos/6/227/thumbs/22770242021.gif",
           "https://content.sportslogos.net/logos/6/234/thumbs/23467492017.gif",
           "https://content.sportslogos.net/logos/6/219/thumbs/21956712016.gif")

# define boxScoreData function
boxScoreData <- function(player, data){
  
  nonempty_data <- filter(data,
                          Starters == player &
                            empty==0)
  
  empty_data <- filter(data,
                       Starters == player &
                         empty==1)
  
  nonempty_df <- data.frame(Environment = "Non-Empty",
                            G = nrow(nonempty_data),
                            MP = mean(nonempty_data$MP, na.rm = T),
                            PTS = mean(nonempty_data$PTS, na.rm = T),
                            TRB = mean(nonempty_data$TRB, na.rm = T),
                            AST = mean(nonempty_data$AST, na.rm = T),
                            FGP = sum(nonempty_data$FG)/sum(nonempty_data$FGA),
                            X3PP = sum(nonempty_data$X3P)/sum(nonempty_data$X3PA),
                            FTP = sum(nonempty_data$FT)/sum(nonempty_data$FTA),
                            BPM = mean(nonempty_data$BPM, na.rm = T),
                            BLK = mean(nonempty_data$BLK, na.rm = T),
                            STL = mean(nonempty_data$STL, na.rm = T),
                            PF = mean(nonempty_data$PF, na.rm = T),
                            TOV = mean(nonempty_data$TOV, na.rm = T)
                            )
  
  empty_df <- data.frame(Environment = "Empty",
                            G = nrow(empty_data),
                            MP = mean(empty_data$MP, na.rm = T),
                            PTS = mean(empty_data$PTS, na.rm = T),
                            TRB = mean(empty_data$TRB, na.rm = T),
                            AST = mean(empty_data$AST, na.rm = T),
                            FGP = sum(empty_data$FG)/sum(empty_data$FGA),
                            X3PP = sum(empty_data$X3P)/sum(empty_data$X3PA),
                            FTP = sum(empty_data$FT)/sum(empty_data$FTA),
                            BPM = mean(empty_data$BPM, na.rm = T),
                            BLK = mean(empty_data$BLK, na.rm = T),
                            STL = mean(empty_data$STL, na.rm = T),
                            PF = mean(empty_data$PF, na.rm = T),
                            TOV = mean(empty_data$TOV, na.rm = T)
                            )
  
  r_df <- rbind(nonempty_df, empty_df)
  
  return(r_df)
}



# define statAnalyzedData function
statAnalyzedData <- function(player, stat, data){
  nonempty_data <- filter(data,
                           Starters == player &
                             empty == 0) 
  
  empty_data <- filter(data,
                       Starters == player &
                         empty == 1) 
  
  if (stat %in% c("FT.", "FG.", "X3P.")) {
    r_df <- data.frame(Environment = c("Non-Empty", 
                                       "Empty"),
                       stat = c(sum(nonempty_data[, which(colnames(nonempty_data) == substr(stat,
                                                                                            start = 1, 
                                                                                            stop = 2)
                                                          )], na.rm = T)/
                                  sum(nonempty_data[, which(colnames(nonempty_data) == paste0(substr(stat,
                                                                                                    start = 1, 
                                                                                                    stop = 2),
                                                                                              "A"))], na.rm = T), 
                                sum(empty_data[, which(colnames(empty_data) == substr(stat,
                                                                                      start = 1, 
                                                                                      stop = 2)
                                )], na.rm = T)/
                                  sum(empty_data[, which(colnames(empty_data) == paste0(substr(stat,
                                                                                              start = 1, 
                                                                                              stop = 2),
                                                                                        "A"))], na.rm = T)
                       )
    )
  }
  else {
  r_df <- data.frame(Environment = c("Non-Empty", 
                                     "Empty"),
                     stat = c(mean(nonempty_data[, which(colnames(nonempty_data) == stat)], na.rm = T), 
                                        mean(empty_data[, which(colnames(empty_data) == stat)], na.rm = T))
                     )
  }
  
  colnames(r_df) <- c("Environment", paste0("Mean(", stat, ")"))
  
  return(r_df)
}

# define playerPerformance function
playerPerformance <- function(player, stat, data){
  
  # create data frame of selected player
  player_df <- filter(data, Starters==player)
  
  # explore distribution of selected stat
  dist <- hist(player_df[, which(colnames(player_df) == stat)], 
                  main = paste(stat, "Distribution"), 
                  xlab = paste(stat), col = "#00468BFF" )
  
  # analyze BPM trends
  stat_ts <- na.omit(player_df[, which(colnames(player_df) == stat)])
  stat_ts <- ts(rollmean(stat_ts, k = 7, fill = NA))
  ts_plot <- stat_ts
  
  # perform t-test
  t_test <- t.test(player_df[, which(colnames(player_df) == stat)] ~ player_df$empty)
  
  tstat <- round(t_test$statistic[[1]], 2)
  pval <- round(t_test$p.value, 3)
  conf_int <- round(t_test$conf.int[1:2], 3)
  
  result <- list(dist,
                 ts_plot,
                 tstat,
                 pval,
                 conf_int)
  
  return(result)
}


#### UI ####
ui <- navbarPage(
  
  title = "DATA 501 Project",
  id = "navbar",
  theme = bs_theme(primary = "#0028A8", secondary = "#D0CDCD", 
                   font_scale = NULL, bootswatch = "united"),
 
####### Title Page ############################  
tabPanel(
  title = "About",
  imageOutput("nbalogo"),
  h1(style = "text-align:center", strong("Assessing the Impact of Crowds on NBA Player and Team Performance")),
  h2(style = "text-align:center ", "By: Abdo Ahmed and Alexander Sewell"),
  h4(style = "text-align:center ", strong("Project Description")),
  p(style="text-align: justify; font-size = 15px",
    "Due to the Covid Pandemic, people have been restricted from attending many events", 
    em("including sporting events."), 
    "Because of this, we are presented with the unique oppurtunity to evaluate how crowds affect the performance of NBA players
    and teams performance in a controlled environment. This leads us to ask", strong("How can we use data science techniques to determine the impact that the fans in attendance have on NBA players and teams?"),
    "To accompish this, we want to see the differences in performance that the following environments have on performance:",
    br(""),
    br("-Empty vs Non-Empty Games"),
    br("-Home vs Away Games"),
    br("-Playoff Vs Regular Season Games."))),
  
  
  
  
  
  
  
   ##### Home Court Advantage #####
  tabPanel(
    title = "Home Court Advantage",
    # team image & select
    fixedRow(
      column(
        width = 4,
        offset = 0,
        selectInput(
          inputId = "team_select",
          label = "Select a Team",
          choices = team_select,
          selected = "Los Angeles Lakers"
        ),
        actionButton(
          inputId = "team_update",
          label = "Update Data"
        )
      ),
      column(
        width = 4,
        offset = 4,
        uiOutput(
          outputId = "team_logo"
        )
      )
    ),
    # Pane 1 and 2
    fixedRow(
      column(
        width = 4,
        offset = 1,
        textOutput(
          outputId = "env_1_title"
        ),
        plotOutput(
          outputId = "env_1_barplot",
          height = "300px"
        ),
        tableOutput(
          outputId = "env_1_table"
        )
      ),
      column(
        width = 4,
        offset = 2,
        textOutput(
          outputId = "env_2_title"
        ),
        plotOutput(
          outputId = "env_2_barplot",
          height = "300px"
        ),
        tableOutput(
          outputId = "env_2_table"
        )
      )
    ),
    # pane 1 and 2 controls
    fixedRow(
      column(
        width = 1,
        offset = 1,
        checkboxGroupInput(
          inputId = "env_1_game_type",
          label = "Games",
          choices = list(
            "Regular Season",
            "Playoffs"
          ),
          selected = list(
            "Regular Season",
            "Playoffs"
          )
        )
      ),
      column(
        width = 2,
        checkboxGroupInput(
          inputId = "env_1_attendance_type",
          label = "Attendance Type",
          choices = list(
            "Empty Arenas",
            "Non-Empty Arenas"
          ),
          selected = "Non-Empty Arenas"
        )
      ),
      column(
        width = 3,
        checkboxGroupInput(
          inputId = "env_1_bubble_type",
          label = "Bubble Games",
          choices = list(
            "Bubble Games",
            "Non-Bubble Games"
          ),
          selected = list(
            "Bubble Games",
            "Non-Bubble Games"
          ),
        )
      ),
      column(
        width = 1,
        checkboxGroupInput(
          inputId = "env_2_game_type",
          label = "Games",
          choices = list(
            "Regular Season",
            "Playoffs"
          ),
          selected = list(
            "Regular Season",
            "Playoffs"
          )
        )
      ),
      column(
        width = 2,
        checkboxGroupInput(
          inputId = "env_2_attendance_type",
          label = "Attendance Type",
          choices = list(
            "Empty Arenas",
            "Non-Empty Arenas"
          ),
          selected = "Empty Arenas"
        )
      ),
      column(
        width = 2,
        checkboxGroupInput(
          inputId = "env_2_bubble_type",
          label = "Bubble Games",
          choices = list(
            "Bubble Games",
            "Non-Bubble Games"
          ),
          selected = list(
            "Bubble Games",
            "Non-Bubble Games"
          )
        )
      ),
    )
  ),
  
  ##### Player Impact Analysis #####
  tabPanel(
    title = "Player Impact Analysis",
    fixedRow(
      column(
        width = 12,
        offset = 0,
        uiOutput(
          outputId = "main_title",
          inline = F
        )
      )
    ),
    # team image & select
    fixedRow(
      column(
        width = 3,
        offset = 0,
        selectInput(
          inputId = "player_entry",
          label = "Select a Player",
          choices = player_select,
          selected = "LeBron James"
        ),
        # selectInput(
        #   inputId = "env_select",
        #   label = "Select Comparison Environemnt",
        #   choices = c("Empty vs Non-Empty",
        #               "Regular Season vs Playoffs",
        #               "Bubble Playoffs vs Normal Playoffs"),
        #   selected = "Empty vs Non-Empty"
        # ),
        selectInput(
          inputId = "stat_select",
          label = "Select Comparison Statistic",
          choices = c("BPM",
                      "PTS",
                      "FT.",
                      "FG."),
          selected = "BPM"
        ),
        selectInput(
          inputId = "season_select",
          label = "Select Seasons to Analyze",
          choices = c("All Seasons" = 1,
                      "Only 2019-20 and 2020-21" = 2),
          selected = "All Seasons"
        ),
        actionButton(
          inputId = "player_update",
          label = "Analyze"
        ),
        uiOutput(
          outputId = "player_picture"
        )
      ),
      column(
        width = 9,
        offset = 0,
        uiOutput(
          outputId = "summ_title"
        ),
        tableOutput(
          outputId = "player_info"
        ),
        tableOutput(
          outputId = "box_score_stats"
        ),
        plotOutput(
          outputId = "games_played",
          height = "200px"
        )
      )
    ),
    fixedRow(
      column(
        width = 10,
        offset = 2,
        uiOutput(
          outputId = "player_title"
        )
      ),
    ),
    fixedRow(
      column(
        width = 4,
        offset = 0,
        plotOutput(
          outputId = "stat_dist"
        )
      ),
      column(
        width = 5,
        offset = 0,
        plotOutput(
          outputId = "stat_trend"
        )
      ),
      column(
        width = 3,
        offset = 0,
        tableOutput(
          outputId = "stat_analyzed"
        ),
        textOutput(
          outputId = "test_stat"
        ),
        textOutput(
          outputId = "test_p"
        ),
        textOutput(
          outputId = "test_conf"
        )
      )
    )
  )
  #### UI End #### 
)





#### Server ####
server <- function(input, output, session) {
  
  updateNavbarPage(inputId = "navbar")
  
  
  
  
  output$nbalogo <- renderImage({
    
    list(src = "nbalogo.png",
         width = "40%",
         style="display: block; margin-left: auto; margin-right: auto;",
         deletefile = FALSE)     })
  #### Home Court Advantage Server ####
  
  env_1_data <- eventReactive(input$team_update, {
    data.frame(xtabs(~WinLoss + HomeAway, 
                     data = team_game_data %>%
                              filter(Team==input$team_select &
                                       game_type %in% input$env_1_game_type &
                                       attendance_type %in% input$env_1_attendance_type &
                                       bubble_type %in% input$env_1_bubble_type)))
  })
  
  env_2_data <- eventReactive(input$team_update, {
    data.frame(xtabs(~WinLoss + HomeAway, 
                     data = team_game_data %>%
                       filter(Team==input$team_select &
                                game_type %in% input$env_2_game_type &
                                attendance_type %in% input$env_2_attendance_type &
                                bubble_type %in% input$env_2_bubble_type)))
  })
  
  logo_link <- eventReactive(input$team_update, {
    logos[match(input$team_select, team_select)]
  })
  
  output$team_logo <- renderUI({
    tags$img(src = logo_link())
  })
  
  output$env_1_barplot <- renderPlot({
    ggplot(data = env_1_data(), aes(x=HomeAway, y=Freq, fill=WinLoss)) +
      geom_bar(position = "dodge", stat = "identity", show.legend = F) +
      geom_text(aes(label = WinLoss), vjust = -0.5, position = position_dodge(0.9)) +
      ggtitle("Home Court Advantage Analysis #1") +
      xlab("Away vs Home") +
      ylab("Games") + theme_minimal()+scale_fill_lancet()
  })
  
  output$env_2_barplot <- renderPlot({
    ggplot(data = env_2_data(), aes(x=HomeAway, y=Freq, fill=WinLoss)) +
      geom_bar(position = "dodge", stat = "identity", show.legend = F) +
      geom_text(aes(label = WinLoss), vjust = -0.5, position = position_dodge(0.9)) +
      ggtitle("Home Court Advantage Analysis #2") +
      xlab("Away vs Home") +
      ylab("Games") + theme_minimal()+scale_fill_lancet()
  })
  
  output$env_1_table <- renderTable({
    data.frame(`Away Win Perc` = env_1_data()$Freq[2]/sum(env_1_data()$Freq[1:2]),
               `Home Win Perc` = env_1_data()$Freq[4]/sum(env_1_data()$Freq[3:4]),
               `P-Value` = prop.test(c(env_1_data()$Freq[2], env_1_data()$Freq[4]), 
                                     c(sum(env_1_data()$Freq[1:2]), sum(env_1_data()$Freq[3:4])))$p.value)
  })

  output$env_2_table <- renderTable({
    data.frame(`Away Win Perc` = env_2_data()$Freq[2]/sum(env_2_data()$Freq[1:2]),
               `Home Win Perc` = env_2_data()$Freq[4]/sum(env_2_data()$Freq[3:4]),
               `P-Value` = prop.test(c(env_2_data()$Freq[2], env_2_data()$Freq[4]), 
                                     c(sum(env_2_data()$Freq[1:2]), sum(env_2_data()$Freq[3:4])))$p.value)
  })
  
  #### Player Impact Analysis Server ####
  box_score_data <- eventReactive(input$player_update, {
    if (input$season_select == 1){
      data <- player_game_data
    } else {
      data <- filter(player_game_data, Season %in% c("2019-2020", 
                                                     "2020-2021")
                     )
    }
    return(boxScoreData(input$player_entry, data))
  })
  
  stat_analyzed_data <- eventReactive(input$player_update, {
    if (input$season_select == 1){
      data <- player_game_data
    } else {
      data <- filter(player_game_data, Season %in% c("2019-2020", 
                                                     "2020-2021")
      )
    }
    return(statAnalyzedData(input$player_entry, input$stat_select, data))
  })
  
  player_info_data <- eventReactive(input$player_update, {
    df1 <- filter(player_game_data,
                      Starters == input$player_entry) %>%
                      distinct(Team) %>%
                      pull(Team)
          
    #if (length(df1)>1){
      df1 <- data.frame(Played_For = paste(df1, collapse = " & "))
    #}
    
    
    df2 <- player_info %>% 
      filter(Player == input$player_entry) %>%
      select(Num, Pos, HT, WT, Age, Draft.Status)
    
    return(cbind(df1, df2))
  })
  
  plot_data <- eventReactive(input$player_update, {
    if (input$season_select == 1){
      data <- player_game_data
    } else {
      data <- filter(player_game_data, Season %in% c("2019-2020", 
                                                     "2020-2021")
      )
    }
    return(playerPerformance(input$player_entry, input$stat_select, data))
  })
  
  player_pic_link <- eventReactive(input$player_update, {
    filter(player_pics, names == input$player_entry)
  })
  
  # main_title_data <- reactive({paste0("Player Impact Analysis")})
  
  
  summ_title_data <- eventReactive(input$player_update, {
    paste0(input$player_entry, " Summary")
  })
  
  player_title_data <- eventReactive(input$player_update, {
    paste0("Analysis of ", input$player_entry, " ", input$stat_select, " Performance")
  })
  
  ts_plot_y <- eventReactive(input$player_update, {
    input$stat_select
  })
  
  pval <- eventReactive(input$player_update, {
    if (input$season_select == 1){
      data <- player_game_data
    } else {
      data <- filter(player_game_data, Season %in% c("2019-2020", 
                                                     "2020-2021")
      )
    }
    ttest_data <- playerPerformance(input$player_entry, input$stat_select, data)[4]
    return(t.test(ttest_data[,1] ~ ttest_data[,2])$p.value)
  })
  
  output$player_picture <- renderUI({
    tags$img(src = player_pic_link()[, 2])
  })
  
  output$box_score_stats <- renderTable({
    box_score_data()
  })
  
  # output$box_score_stats2 <- renderTable({
  #   box_score_data()[c(1, 7:11)]
  # })
  
  output$games_played <- renderPlot({
    ggplot(data=box_score_data(), aes(x = G, y = Environment)) +
      geom_col(aes(fill = Environment)) +
      ggtitle("Games Played in each Environment") + theme_minimal()+scale_fill_lancet()
  })
  
  # output$main_title <- renderUI({
  #   h2("Player Impact Analysis")
  # })
  
  output$summ_title <- renderUI({
    h3(summ_title_data())
  })
  
  output$player_title <- renderUI({
    h3(player_title_data())
  })
  
  output$stat_analyzed <- renderTable({
    stat_analyzed_data()
  })
  
  output$player_info <- renderTable({
    player_info_data()
  })
  
  output$stat_dist <- renderPlot({
    plot_data()$dist
  })
  
  output$stat_trend <- renderPlot({
    plot(plot_data()[[2]], main = "Trend", xlab = "Games", ylab = ts_plot_y())
  })
  
  output$test_stat <- renderText({
    paste0("Test Statistic: ", plot_data()[3])
  })
  
  output$test_p <- renderText({
    paste0("P-Value: ", plot_data()[4])
  })
  
  output$test_conf <- renderText({
    paste0("95% Confidence Interval: ", plot_data()[5][1])
  })
}


#### Server End ####


shinyApp(ui = ui, server = server)










