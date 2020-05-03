#when X>1 and len==1 X behaves as :X which throws off turn order on a 1v1 fight
#this amends that
trueSample <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}

#die function
die <- function(sides, dice=1, mod=0){
  return(sum(sample(sides, dice)+mod))
}

#weapon class
#atk-attack bonus, damage - die damage, favor - how often weapon is used
setClass("Weapon", slots=c(atk="numeric", dice="numeric", sides="numeric", bonus="numeric",
                           favor="numeric"))

#player class
setClass("Character", slots=c(playable="ANY",init="numeric", hp="numeric", 
                              ac="numeric", weapons="ANY"))

#TEST PLAYERS
pc1 <- new("Character", playable=T, init = 0, hp=100, ac=14, 
            weapons=c(
              new("Weapon", atk=4, dice=2, sides=8, bonus=1, favor=1),
              new("Weapon", atk=1, dice=1, sides=10, bonus=1, favor=0))
)

npc1 <- new("Character", playable=F, init = 0, hp=100, ac=14, 
            weapons=c(
                      new("Weapon", atk=4, dice=2, sides=8, bonus=1, favor=1),
                      new("Weapon", atk=1, dice=1, sides=10, bonus=1, favor=0))
            )


#player will pick a weapon based on favoritism ratio
setGeneric("wpnChoice", function(object) standardGeneric("wpnChoice"))
setMethod("wpnChoice", "Character", function(object){
  fvr <- c()
  for(wpn in 1:length(object@weapons)){
    fvr[wpn] <- object@weapons[[wpn]]@favor
  }
  #TODO: dual weilding
  return(sample(object@weapons, 1, prob=c(fvr))[[1]])
})

#attack method rolls die from character class
setGeneric("attack", function(atckr, dfndr) standardGeneric("attack"))
setMethod('attack','Character',function(atckr, dfndr){
  #choose weapon
  wpn <- wpnChoice(atckr)
  
  #roll attack die
  atckr.atk <- die(20, mod=wpn@atk)
  
  #if attacker atk > defender AC continue with damage
  if (atckr.atk >= dfndr@ac) {
    dfndr@hp <- dfndr@hp - (die(dice = wpn@dice, sides = wpn@sides)+wpn@bonus)
    return(dfndr)
  } else {
    return(dfndr)
  }
})


#function to determine order of initiative
roll.init <- function(plyrs){
  plyr.order <- matrix(nrow=length(plyrs), ncol=2)
  #roll for initiative for each player
  for (i in c(1:length(plyrs))) {
    plyr.order[i,1] <- i
    plyr.order[i,2] <- die(20, mod = plyrs[[i]]@ac)
  }
  plyr.order <- plyr.order[order(plyr.order[,2]),]
  
  return(list(plyrs[plyr.order[,1]])[[1]])
}

#FIGHT!
battle <- function(players){
  battle.log <- data.frame()
  
  #battle length
  b.len = 0
  
  #player order
  players <- roll.init(players)

  #list of player index in players, playable, and what their turn order is
  npcs <- which(lapply(players, function(players) which(players@playable==F))==1)
  pcs <- which(lapply(players, function(players) which(players@playable==T))==1)
  
  fighting = T
  #while someone is left to fight, the battle rages on
  while (fighting) {
    b.len = b.len + 1
    
    #rotate between player initiatives
    for (attacker in players) {
      #print(paste('attacker',attacker@playable))
      #if the player is a pc it chooses a random npc to attack, and vise verse
      
      if (attacker@playable) {
        dfndr <- npcs[trueSample(1:length(npcs))]
        defender <- players[dfndr][[1]]
        
        battle.log <- rbind(battle.log, 'PC')
        
        #print(defender@playable)
      } else {
        dfndr <- pcs[trueSample(1:length(pcs))]
        defender <- players[dfndr][[1]]
        
        
        battle.log <- rbind(battle.log, 'NPC')
        
        #print(attacker@playable)
      }
      
      
      
      battle.log <- rbind(battle.log, attacker@hp)
      
      #new stats for defender
      defender <- attack(attacker, defender)

      battle.log <- rbind(battle.log, attacker@hp)
      
      #check health of defender, if less than 0 remove them from the list
      if (defender@hp <= 0) {
        if (defender@playable) {
          pcs <- pcs[-dfndr]
          #stop fighting
          if (length(pcs)<=0){
            fighting = F
          }
        } else {
          npcs <- npcs[-dfndr]
          #stop fighting
          if (length(npcs)<=0){
            fighting = F
          }
        }
      }
      
      #check health of attacker
      if (attacker@hp <= 0) {
        if (attacker@playable) {
          pcs <- pcs[-dfndr]
          #stop fighting
          if (length(pcs)<=0){
            fighting = F
          }
        } else {
          npcs <- npcs[-dfndr]
          #stop fighting
          if (length(npcs)<=0){
            fighting = F
          }
        }
      }
      
      #update the players 
      players[[dfndr]] <- defender
    } 
  }
  
  #who won? who's next? YOU DECIIIDDDEEEEEE!
  winner <- if (length(pcs)>length(npcs)) {
    #print('PC WINNER')
    #print(battle.log)
    "PCs"
  } else {
    #print(battle.log)
    "NPCs"
  }
  
  return(data.frame(
    turns = c(b.len),
    pcs.left = c(length(pcs)),
    npcs.left = c(length(npcs)),
    winner
    )
  )
}



battle(list(pc1, npc1))

#turn counter testing
#turns <- data.frame()
#for (i in c(1:1000)) {
#  turns <- rbind(turns, battle(list(pc1, npc1)))
#}
#table(turns$winner)


