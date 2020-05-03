#one column

library(purrr)
library(shiny)
source('C:/Users/mattd/Desktop/RnDnD/RnDnD.R')
#####UI#####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 12,
                 fluidRow(
                   numericInput("n", "Number of Players", value = 2, min = 2),
                   uiOutput('player'),
                   numericInput('fights','How many Fights?', 100, width = '10%'),
                   actionButton('fight', 'FIGHT!')
                 )
    ),
    mainPanel(
        splitLayout(cellWidths = c("50%", "50%"), 
                    plotOutput("fight.length"), 
                    plotOutput("winner.rate"))
    )
  )
)
#####SERVER#####
server <- function(input, output, session) {
  #name of stat
  #player
  armor_classes <- reactive(paste0("ac", seq_len(input$n)))
  hit_points <- reactive(paste0("hp", seq_len(input$n)))
  initiative <- reactive(paste0("init", seq_len(input$n)))
  playable_character <- reactive(paste0("pc", seq_len(input$n)))
  #weapon
  weapon_atk <- reactive(paste0("wpn.atk", seq_len(input$n)))
  weapon_dice <- reactive(paste0("wpn.dice", seq_len(input$n)))
  weapon_sides <- reactive(paste0("wpn.sides", seq_len(input$n)))
  weapon_bonus <- reactive(paste0("wpn.bonus", seq_len(input$n)))

  #map each text to column 
  output$player <- renderUI({
    #player stats
    ac <- map(armor_classes(), ~ textInput(.x, 
                                           'AC', value = isolate(input[[.x]])) %||% "")
    hp <- map(hit_points(), ~ textInput(.x, 
                                        'HP', value = isolate(input[[.x]])) %||% "")
    init <- map(initiative(), ~ textInput(.x, 
                                          'Inititative', value = isolate(input[[.x]])) %||% "")
    pc <- map(playable_character(), ~ radioButtons(.x, 
                                                   'Playable', choices = c('PC', 'NPC')))
    #weapon stats
    atk <- map(weapon_atk(), ~ textInput(.x, 
                                         'ATK', value = isolate(input[[.x]])) %||% "")
    dice <- map(weapon_dice(), ~ textInput(.x, 
                                          'Die', value = isolate(input[[.x]])) %||% "")
    sides <- map(weapon_sides(), ~ textInput(.x, 
                                         'Sides', value = isolate(input[[.x]])) %||% "")
    bonus <- map(weapon_bonus(), ~ textInput(.x, 
                                          'Bonus', value = isolate(input[[.x]])) %||% "")
    
    
    #player stats UI
      fluidRow(
        column(2, ac),
        column(2, hp),
        column(2, init),
        column(2, pc),
        #weapon stats
        column(1, atk),
        column(1, dice),
        column(1, sides),
        column(1, bonus)
    )
      
  })
  
  #fight after button is pressed
  fight <- eventReactive(input$fight, {
    #list of armor classes
    acs <- map_chr(armor_classes(), ~ input[[.x]])
    acs[acs == ""] <- NA
    #list of hps
    hps <- map_chr(hit_points(), ~ input[[.x]])
    hps[hps == ""] <- NA
    #list of initiatives
    inits <- map_chr(initiative(), ~ input[[.x]])
    inits[inits == ""] <- NA
    #list of playable
    pcs <- map_chr(playable_character(), ~ input[[.x]])
    pcs[pcs == "PC"] <- T
    pcs[pcs == "NPC"] <- F
    #list of ATKs
    atk <- map_chr(weapon_atk(), ~ input[[.x]])
    atk[atk == ""] <- NA
    #list of dice
    dice <- map_chr(weapon_bonus(), ~ input[[.x]])
    dice[dice == ""] <- NA
    #list of sides
    sides <- map_chr(weapon_sides(), ~ input[[.x]])
    sides[sides == ""] <- NA
    #list of bonuses
    bonus <- map_chr(weapon_bonus(), ~ input[[.x]])
    bonus[bonus == ""] <- NA
    
    #list of player characters
    players <- list()
    
    #TODO run on action button
    for (i in seq_len(input$n)) {
      players[[i]] <- new("Character", playable=as.logical(pcs[i]), 
                          init=as.integer(inits[i]), 
                          hp=as.integer(hps[i]), 
                          ac=as.integer(acs[i]), 
                          weapons=c(
                            new("Weapon", atk=as.integer(atk[i]), 
                                          dice=as.integer(dice[i]), 
                                          sides=as.integer(sides[i]), 
                                          bonus=as.integer(bonus[i]), 
                                          favor=1)
                            ))
    }
    
    turns <- data.frame()
    for (i in c(1:input$fights)) {
      turns <- rbind(turns, battle(list(pc1, npc1)))
    }
    turns
  })
  
  #generate plot  
  output$fight.length <- renderPlot({
    fight <- fight()
    hist(fight$turns)
    abline(v = mean(fight$turns, col='red'))
  })
  
  output$winner.rate <- renderPlot({
    fight <- fight()
    barplot(table(fight$winner))  
  })
}
#####SHINY APP#####
shinyApp(ui, server)