options(scipen=999)
######################################################
#                   ________________                 #
#                  |  Taipan        |                #
#                  |   Taipan       |                #
#                  |    Taipan      |                #
#                  |________________|                #
#                          |                         #
#          ________________|_______________          #
#          \  O   O   O   O   O   O   O   /          #
#           \   USS RA Fisher            /           #
#            \__________________________/            #
#                                                    #
######################################################



#Menu Function
#draws and displays menu
menuMaker<-function(menuOptions){
  
  while(TRUE){
    
    #List menu options
    writeLines("Your options are:")  
    for(i in 1:length(menuOptions[,1])){
      writeLines(paste(i,menuOptions[i,1]))
    }
    writeLines(paste((i+1),"Return to previous menu."))
    
    #request User Input
    menuInput<-readline("[Enter a number]")
    
    #Error Handling
    #inverse logic, if FALSE then TRUE
    if((menuInput %in% 1:(i+1))==FALSE){writeLines("Input not recognized. Please Try Again.")}
    
    #Return menu options
    else{
      cat("\014")  
      #Run menu option
      menuOptions1<- rbind(menuOptions,c('x',glob.return))
      menuOptions1[[as.numeric(menuInput),2]]()
    
      #break into outerloop
      if(menuInput==i+1){break}
    }
  }
}

glob.return<-function(){
    #filler
}

################################################################################
################################################################################
#Main Menu
#Code to create a custom menu
#Function for calling functions and returning to menu
################################################################################
################################################################################
#Port Menu
main.port<- function(){
  writeLines(paste("You are at the city port of ",loc[,1][loc[,2]=='1'],", where would like to go?",sep=''))
  menuMaker(sail.menu)
}

  # Market Menu
  main.market<-function(){
    writeLines(
"You arrive at a busy market, with people selling various goods...
You look up to see a giant bulletin board, with today's prices:")
    writeLines(paste(
      "   The price for ham is:     $",market[(loc$cityid[loc$status==1]),'ham'],
      "\n   The price for turkey is:  $",market[(loc$cityid[loc$status==1]),'turkey'],     
      "\n   The price for beets is:   $",market[(loc$cityid[loc$status==1]),'beets'],      
      "\n   The price for steak is:   $",market[(loc$cityid[loc$status==1]),'steak'],      
      "\n   The price for pork is:    $",market[(loc$cityid[loc$status==1]),'pork'],       
      "\n   The price for hotdogs is: $",market[(loc$cityid[loc$status==1]),'hotdogs'],
      "\n",
      "\n You have $",ship$money," to spend.",
      "\n",sep=''))    
    
    menuMaker(market.menu)
  }
  #Rumor Menu
  main.rumor<-function(){
    menuMaker(rumor.menu)
  }
  #Ship Status
  main.status<-function(){
    writeLines(paste(
      "You have $", ship$money," in your coffers, and your ship, 'The ", ship$name,"' has:\n",
      
      "Armor: ", ship$armor,"/",ship$armor.max,"\n",
      "Cannons: ", ship$guns,"\n",
      "Cargo: ", ship$cargo.n,"/",ship$cargo,"\n",
      
      "    Ham     ", ship$cargo.ham    ,"\n",
      "    Turkey  ", ship$cargo.turkey ,"\n",
      "    Beets   ", ship$cargo.beets  ,"\n",
      "    Steak   ", ship$cargo.steak  ,"\n",
      "    Pork    ", ship$cargo.pork   ,"\n",
      "    Hotdogs ", ship$cargo.hotdogs,"\n", sep=""))
    pause<-readline("Press 'Enter' to continue")
    cat("\014")
  }
#Stores a String name and corresponding function that it should call if selected.
names<-c("Visit the port",
         "Visit the market",
         "Check rumors",
         "Check ship status")
funcs<-c(main.port,
         main.market,
         main.rumor,
         main.status)
main.menu<-cbind(names,funcs)

################################################################################
################################################################################
#Sailing City Selection Gate:

#Sailing to different city functions
sail.loc<-function(i){
  if(loc[i,2]=="1"){
    writeLines("You are already in this city! Try going somewhere else.")
    pause<-readline("Press 'Enter' to continue")
  }
  else{
    loc[2]<<-"0"
    loc[i,2]<<-"1" 
    market.cycle()
    
    #Check to see if there is a pirate fight!!!
    fight()
    
    writeLines(paste("You arrive at the Port of",loc[,1][loc[,2]=='1']))
    pause<-readline("Press 'Enter' to continue")
    cat("\014") 
    writeLines(paste("Welcome to ",loc[,1][loc[,2]=='1'],"!!!! Home of the world's best ",sample(nouns,1),"!",sep=''))
    }
}
################################################################################

sail.loc.1<-function(){sail.loc(1)}
sail.loc.2<-function(){sail.loc(2)}
sail.loc.3<-function(){sail.loc(3)}
sail.loc.4<-function(){sail.loc(4)}
sail.loc.5<-function(){sail.loc(5)}

names<-c("Juno",
         "Hong Kong",
         "Sydney",
         "Seattle",
         "London")
funcs<-c(sail.loc.1, 
         sail.loc.2, 
         sail.loc.3,
         sail.loc.4,
         sail.loc.5)
#this variable can be updated as well.
sail.location<-cbind(names,funcs)
#Sailing Menu Interface
#sail.XXXX
#Taipain Based fighting
sail.out<-function(){menuMaker(sail.location)}
#sail.out()
################################################################################
################################################################################
#Shipyard fix yo'ship

shipyard.repair<-function(){
  repair.cost<-10
  while(TRUE){
    if((ship$armor.max-ship$armor)==0){
      writeLines("There is such a thing as too many repairs. If it ain't broke, don't fix it. Ya' know?")
      break
    }
    
    while(TRUE){
    z<-readline(
      paste("How much of the ship do you want to repair? Currently your ship has, ",
            ship$armor,"/", ship$armor.max," armor.",sep=''))
      z<-as.numeric(z)
      if(is.na(z)){
        writeLines("Let's try that agian...")
      } else { break
      }
    }
    
    if(z>(ship$armor.max-ship$armor)){
      writeLines("There is such a thing as too many repairs. If it ain't broke, don't fix it. Ya' know?")
    } else if((z * repair.cost)> ship$money){writeLines(paste("It will cost you, $", round(z * repair.cost),
                                                              " to repair that much of your ship. You have $",ship$money,
                                                              ". That is not enough money",sep=''))
      pause<-readline("Press 'Enter' to continue")
    }else{
      z1<-readline(paste("It will cost you, $", z * repair.cost,
                         " to repair that much of your ship. You have $",ship$money,". Is that okay?[Yes/No]",sep=''))
      cat("\014")
      #If yes and want to repair
      if(tolower(z1)=="yes"){
        ship$armor<<-ship$armor+z
        ship$money<<-ship$money-(z*repair.cost)
        writeLines("*Clink!* *Clank!* *Clink!* *Clank!*")
        pause<-readline("Press 'Enter' to continue")
        cat("\014")
        
        writeLines("*Boooom!*")
        pause<-readline("Press 'Enter' to continue")
        cat("\014")
        
        writeLines("Whew! That was quite the undertaking but we finished working on your ship")
        writeLines(paste("Your ship regained ",z," points of armor. Your ship has ", ship$armor,"/",ship$armor.max," armor",sep=''))
        pause<-readline("Press 'Enter' to continue")
        break
      } else if(tolower(z1)=="no"){
        writeLines("Okay, what would you like to do?")
        pause<-readline("Press 'Enter' to continue")
        cat("\014")
      } else{writeLines("Let's try that agian...")
        break
      }
    }   
  }
}
#shipyard.repair()

###Buying max armor
shipyard.armor<-function(){
  armor.cost<-10
  z<-as.numeric(readline(
    paste("At this shipyard, it costs $",armor.cost," per steel plate. You have $",ship$money," to spend.", 
          "\nHow much armor do you want to add to your ship? Currently your ship has a max of ",ship$armor.max," armor.",sep='')))
        
        while(TRUE){
 
          if(armor.cost*z > ship$money){
            writeLines("You cannot afford that much armor... It will cost you, $", round(z * armor.cost),
                        " to improve your ship. You have $",ship$money,
                        ". That is not enough money",sep='')
            pause<-readline("Press 'Enter' to continue")
          }else{
            z1<-readline(paste("It will cost you, $", z * armor.cost,
                               " to improve your ship. You have $",ship$money,". Is that okay?[Yes/No]",sep=''))
            cat("\014")
            #If yes and want to repair
            if(tolower(z1)=="yes"){
              ship$armor<<-ship$armor+z
              ship$armor.max<<-ship$armor.max+z
              ship$money<<-ship$money-(z*armor.cost)
              writeLines("*Clink!* *Clank!* *Clink!* *Clank!*")
              pause<-readline("Press 'Enter' to continue")
              cat("\014")
              
              writeLines("*Boooom!*")
              pause<-readline("Press 'Enter' to continue")
              cat("\014")
              
              writeLines("Whew! That was quite the undertaking but we finished working on your ship")
              writeLines(paste("Your ship added ",z," points of armor. Your ship has ", ship$armor,"/",ship$armor.max," armor",sep=''))
              pause<-readline("Press 'Enter' to continue")
              break
            } else{writeLines("Okay. Let's try that again...")
              break
            }
          }   
        }
}

#shipyard.armor()

###########################
#Buying Armor for your Ship
###########################
shipyard.cargo<-function(){
  cargo.cost<-10
  z<-as.numeric(readline(
    paste("At this shipyard, it costs $",cargo.cost," per cargo hold. You have $",ship$money," to spend.", 
          "\nHow many cargo holds do you want to add to your ship? Currently your ship has ",ship$cargo," cargo holds.",sep='')))
  
  while(TRUE){
    
    if(cargo.cost*z > ship$money){
      writeLines("You cannot afford that many cargo holds... It will cost you, $", round(z * cargo.cost),
                 " to improve your ship. You have $",ship$money,
                 ". That is not enough money",sep='')
      pause<-readline("Press 'Enter' to continue")
    }else{
      z1<-readline(paste("It will cost you, $", z * cargo.cost,
                         " to improve your ship. You have $",ship$money,". Is that okay?[Yes/No]",sep=''))
      cat("\014")
      #If yes and want to repair
      if(tolower(z1)=="yes"){
        ship$cargo<<-ship$cargo+z
        ship$cargo.max<<-ship$cargo.max+z
        ship$money<<-ship$money-(z*cargo.cost)
        writeLines("*Clink!* *Clank!* *Clink!* *Clank!*")
        pause<-readline("Press 'Enter' to continue")
        cat("\014")
        
        writeLines("*Boooom!*")
        pause<-readline("Press 'Enter' to continue")
        cat("\014")
        
        writeLines("Whew! That was quite the undertaking but we finished working on your ship")
        writeLines(paste("Your ship has an extra ",z," holds. Your ship has ",ship$cargo," cargo holds",sep=''))
        pause<-readline("Press 'Enter' to continue")
        break
      } else{writeLines("Okay. Let's try that again...")
        break
      }
    }   
  }#while loop end
  
}#function end
#shipyard.cargo()


###########################
#Buying Guns for your Ship
###########################

shipyard.guns<-function(){
  gun.cost<-10
  z<-as.numeric(readline(
    paste("At this shipyard, it costs $",gun.cost," per cannon. You have $",ship$money," to spend.", 
          "\nHow many cannons do you want to add to your ship? Currently your ship has ",ship$guns," cannons.",sep='')))
  
  while(TRUE){
    if(z == 0 ){
      writeLines("Alright...")
      pause<-readline("Press 'Enter' to continue")
      break
    }
    if(gun.cost*z > ship$money){
      writeLines("You cannot afford that much armor... It will cost you, $", round(z * gun.cost),
                 " to improve your ship. You have $",ship$money,
                 ". That is not enough money",sep='')
      pause<-readline("Press 'Enter' to continue")
    }else{
      z1<-readline(paste("It will cost you, $", z * gun.cost,
                         " to improve your ship. You have $",ship$money,". Is that okay?[Yes/No]",sep=''))
      cat("\014")
      #If yes and want to repair
      if(tolower(z1)=="yes"){
        ship$guns <<-ship$guns+z
        ship$money<<-ship$money-(z*ship$guns)
        writeLines("*Pew!* *Pew!* *Pew!* *Pew!*")
        pause<-readline("Press 'Enter' to continue")
        cat("\014")
        
        writeLines("*Clank!*")
        pause<-readline("Press 'Enter' to continue")
        cat("\014")
        
        writeLines("Whew! That was quite the undertaking but we finished working on your ship")
        writeLines(paste("Your ship added ",z," cannons. Your ship has ", ship$guns,"  now.",sep=''))
        pause<-readline("Press 'Enter' to continue")
        break
      } else{writeLines("Okay. Let's try that again...")
        break
      }
    }   
  } #end of while loop
}#end of function
#shipyard.guns()
  
  
###############################################
#Ship Yard Menu
###############################################
names<-c("Repair ship hull",
         "Buy armor",
         "Buy cannons",
         "Expand cargohold")
funcs<-c(shipyard.repair, 
         shipyard.armor, 
         shipyard.guns,
         shipyard.cargo)
#this variable can be updated as well.
shipyard.options<-cbind(names,funcs)

#Menu for the shipyard
shipyard.menu<-function(){
  menuMaker(shipyard.options)
}
#Stores a String name and corresponding function that it should call if selected.
names<-c("Sail away from port", "Visit the shipyard")
funcs<-c(sail.out,shipyard.menu)
#this variable can be updated as well.
sail.menu<-cbind(names,funcs)
#menuMaker(sail.menu)
################################################################################
################################################################################





################################################################################
################################################################################
#Market Pricing
#Current and future pricing
#Future required to be calculated for Rumor and News to work.

################################################################################
################################################################################
#Function to update market prices, Tomorrow's -> Today's and generates new Tomorrow prices.
market.cycle<-function(){
market<<-market1
    ham    <-round(runif(5,min=15,max=25))
    turkey <-round(runif(5,min=35,max=50))
    beets  <-round(runif(5,min=25,max=80))
    steak  <-round(runif(5,min=120,max=300))
    pork   <-round(runif(5,min=60,max=75))
    hotdogs<-round(runif(5,min=100,max=200))
market1<<-cbind(ham, turkey, beets, steak,pork, hotdogs)
}

#market.cycle()
################################################################################
################################################################################
market.check<-function(){
  cat("\014")
  writeLines(paste(
  "The price for ham is:     $",market[(loc$cityid[loc$status==1]),'ham'],
  "\nThe price for turkey is:  $",market[(loc$cityid[loc$status==1]),'turkey'],     
  "\nThe price for beets is:   $",market[(loc$cityid[loc$status==1]),'beets'],      
  "\nThe price for steak is:   $",market[(loc$cityid[loc$status==1]),'steak'],      
  "\nThe price for pork is:    $",market[(loc$cityid[loc$status==1]),'pork'],       
  "\nThe price for hotdogs is: $",market[(loc$cityid[loc$status==1]),'hotdogs'],sep=''))    
  pause<-readline("Press 'Enter' to continue")
}
market.buy<-function(){
  while(TRUE){
    writeLines(paste(
      "You currently have, $",ship$money," to spend."," Select a good you wish to buy:
      1 Ham     (Current Price: $",market[(loc$cityid[loc$status==1]),1],",  Amount in Cargo: ",ship$cargo.ham,")
      2 Turkey  (Current Price: $",market[(loc$cityid[loc$status==1]),2],",  Amount in Cargo: ",ship$cargo.turkey,")
      3 Beets   (Current Price: $",market[(loc$cityid[loc$status==1]),3],",  Amount in Cargo: ",ship$cargo.beets,")
      4 Steak   (Current Price: $",market[(loc$cityid[loc$status==1]),4],", Amount in Cargo: ",ship$cargo.steak,")
      5 Pork    (Current Price: $",market[(loc$cityid[loc$status==1]),5],",  Amount in Cargo: ",ship$cargo.pork,")
      6 Hotdogs (Current Price: $",market[(loc$cityid[loc$status==1]),6],", Amount in Cargo: ",ship$cargo.hotdogs,")
      7 Go to the previous Menu",sep=''))
    x<-readline("[Enter a number]")
    while(!(x %in% c(1,2,3,4,5,6,7))){ x<-readline("That is not a valid response, try again [Enter a number]")}
    if(x==7){break}
    
    buy.name<-names(market[(loc$cityid[loc$status==1]),as.numeric(x)])
    buy.cost<-market[(loc$cityid[loc$status==1]),as.numeric(x)]
    
    #Confirm total amount
    cat("\014")
    y1<-readline(
      paste("The ",buy.name," is currently $", buy.cost," per bushel, how much ",buy.name," do you want to buy? [Enter a number]" 
      ,sep=''))
    y<-as.numeric(y1)
    
    #Confirm total cost
    cat("\014")
    
    try(if(ship$money>=buy.cost*y & ship$cargo>(ship$cargo.n+y)){
      z<-readline(paste("That is ",y," bushel of ",buy.name," which will total $", buy.cost*y," is that okay?[Yes/No]" 
      ,sep=''))})
      
     if(ship$cargo<=(ship$cargo.n+y)){
      z<-'no'
      writeLines("I'm sorry you cannot fit that many goods in your cargo.")
      }
     if(ship$money<buy.cost*y){
      z<-'no'
      writeLines("I'm sorry you cannot affort that.")
    }
    

    #Buying calculation
    if(z==tolower("yes")){
      ship$money<<-ship$money-buy.cost*y
      ship$cargo.n<<-ship$cargo.n+y
      ship[paste('cargo.',buy.name,sep='')]<<-y
      writeLines("Transaction completed. Thank you!")
      pause<-readline("Press 'Enter' to continue")
    }
    
  }
}

market.sell<-function(){  
  while(TRUE){
    writeLines(paste(
      "You currently have, $",ship$money," to spend and ",ship$cargo.n,"/",ship$cargo," space taken up"," Select a good you wish to sell:
      1 Ham     (Current Price: $",market[(loc$cityid[loc$status==1]),1],",  Amount in Cargo: ",ship$cargo.ham,")
      2 Turkey  (Current Price: $",market[(loc$cityid[loc$status==1]),2],",  Amount in Cargo: ",ship$cargo.turkey,")
      3 Beets   (Current Price: $",market[(loc$cityid[loc$status==1]),3],",  Amount in Cargo: ",ship$cargo.beets,")
      4 Steak   (Current Price: $",market[(loc$cityid[loc$status==1]),4],", Amount in Cargo: ",ship$cargo.steak,")
      5 Pork    (Current Price: $",market[(loc$cityid[loc$status==1]),5],",  Amount in Cargo: ",ship$cargo.pork,")
      6 Hotdogs (Current Price: $",market[(loc$cityid[loc$status==1]),6],", Amount in Cargo: ",ship$cargo.hotdogs,")
      7 Go to the previous Menu",sep=''))
    x<-readline("[Enter a number]")
    while(!(x %in% c(1,2,3,4,5,6,7))){ x<-readline("That is not a valid response, try again [Enter a number]")}
    if(x==7){break}
    sell.name<-names(market[(loc$cityid[loc$status==1]),as.numeric(x)])
    sell.cost<-market[(loc$cityid[loc$status==1]),as.numeric(x)]
    sell.amount<-ship[paste('cargo.',sell.name,sep='')]
    
    #must have good to sell to proceed
    if(sell.amount<1){
      writeLines(paste("I'm sorry, it does not appear that you have any ",sell.name,sep=''))
      writeLines(paste(
        "You currently have, $",ship$money," to spend.","Select a good you wish to sell:
        1 Ham     (Current Price: $",market[(loc$cityid[loc$status==1]),1],",  Amount in Cargo: ",ship$cargo.ham,")
        2 Turkey  (Current Price: $",market[(loc$cityid[loc$status==1]),2],",  Amount in Cargo: ",ship$cargo.turkey,")
        3 Beets   (Current Price: $",market[(loc$cityid[loc$status==1]),3],",  Amount in Cargo: ",ship$cargo.beets,")
        4 Steak   (Current Price: $",market[(loc$cityid[loc$status==1]),4],", Amount in Cargo: ",ship$cargo.steak,")
        5 Pork    (Current Price: $",market[(loc$cityid[loc$status==1]),5],",  Amount in Cargo: ",ship$cargo.pork,")
        6 Hotdogs (Current Price: $",market[(loc$cityid[loc$status==1]),6],", Amount in Cargo: ",ship$cargo.hotdogs,")
        7 Go to the previous Menu",sep=''))
      x<-readline("Please select another good you want to sell [Enter a number]")
    }
    
    #Confirm total amount
    cat("\014")
    y1<-readline(
      paste(
"The ",sell.name," is currently $", sell.cost," per bushel. 
You have ",sell.amount," bushels, how much ",sell.name," do you want to sell? [Enter a number]" 
            ,sep=''))
    y<-as.numeric(y1)
    
    #Confirm total cost
    cat("\014")
    z<-readline(paste("That is ",y," bushel of ",sell.name," which will total $", sell.cost*y," is that okay?[Yes/No]" 
                        ,sep=''))
    #Buying calculation
    if(z==tolower("yes")){
      ship$money<<-ship$money+sell.cost*y
      ship$cargo.n<<-ship$cargo.n-y
      ship[paste('cargo.',sell.name,sep='')]<<-(ship[[paste('cargo.',sell.name,sep='')]]-y)
    writeLines("Transaction completed. Thank you!")
    pause<-readline("Press 'Enter' to continue")
    }
  }
}


names<-c("Check City Prices on Goods","Buy Goods", "Sell Goods")
funcs<-c(market.check, market.buy, market.sell)
#this variable can be updated as well.
market.menu<-cbind(names,funcs)
#menuMaker(market.menu)






################################################################################
################################################################################
################################################################################
################################################################################
#Rumor: Representing sampling
#pay to get prices from other cities
#returns sampled dataset from a random normal distribution;
#as n>0 more certainty

rumor.pollster<-function(){
  writeLines("You meet a shady pollster in a dark alley looking to make a deal...")
  rumorMill()
}
rumor.yourself<-function(){
  cat("\014") 
  writeLines("You steel yourself against the uncertain result as you ask 
your ship's crew what they think of you...")
  pause<-readline("Press 'Enter' to continue")
  cat("\014") 
  writeLines(paste("The crew thinks you are ",sample(adjectives,1)," and ",sample(adjectives,1),"...",sep=''))
  writeLines(paste("However, the crew says the people of ",loc[,1][loc[,2]=='1']," think of you as the '",sample(adjectives,1)," ",sample(nouns,1),".'",sep=''))
  pause<-readline("Press 'Enter' to continue")
  cat("\014") 
}
rumor.news<-function(){
  cat("\014") 
  writeLines("You pick up a newspaper and realize the sayings are true.... there are lies, damn lies, and statistics")
  writeLines("You chuckle at how orginal you are...")
  pause<-readline("Press 'Enter' to continue")
}



names<-c("Go into a dark alley","Talk with your crew", "Check the local news")
funcs<-c(rumor.pollster, rumor.yourself,rumor.news )
#this variable can be updated as well.
rumor.menu<-cbind(names,funcs)
#menuMaker(rumor.menu)
################################################################################

rumorMill<-function(){
  gate<-readline("Do you want to get the rumor on the street on pricing?[Yes/No]")
  if(tolower(gate)=="yes"){
  cat("\014") 
  writeLines("Which city do you want us to survey?
    1 Juno
    2 Hong Kong
    3 Sydney
    4 Seattle
    5 London")
  x<-as.numeric(readline("[Enter a number]"))
  cat("\014") 
    rumor.n<-as.numeric(readline("How many people do you want to interview?"))
    writeLines(paste("You currently have, $",ship$money," to spend. Interviewing this many people will cost $",
                     rumor.n*.75,".",sep=''))
    gate2<-readline("Is that okay?[Yes/No]")
    
    if(tolower(gate2)=="yes"){
      ship$money<<-(ship$money-rumor.n*.75)
    writeLines(paste(
      "From talking to ",rumor.n," people, we think tomorrows prices at ",loc[,1][loc[,2]=='1']," will be:
      Ham     Estimated Price: $",round(mean(rnorm(rumor.n,market1[x,1], sd = 10))),"
      Turkey  Estimated Price: $",round(mean(rnorm(rumor.n,market1[x,2], sd = 10))),"
      Beets   Estimated Price: $",round(mean(rnorm(rumor.n,market1[x,3], sd = 10))),"
      Steak   Estimated Price: $",round(mean(rnorm(rumor.n,market1[x,4], sd = 10))),"
      Pork    Estimated Price: $",round(mean(rnorm(rumor.n,market1[x,5], sd = 10))),"
      Hotdogs Estimated Price: $",round(mean(rnorm(rumor.n,market1[x,6], sd = 10))),sep=''))
    pause<-readline("Press 'Enter' to continue")
    }
  }
}
#rumorMill()

################################################################################
################################################################################
################################################################################
################################################################################
#Read the news: Representing Regression.
#Learn about leading indicators
#
#Pirate attack function:
pirate.attack<-function(){
  #Then the pirates attack!
  for(i in 1:pirate.n){
    print(pirate.n)
    
    if(runif(1)>.50){
      cat("\014")
      #print(pirate.n)
      ship$armor<<-ship$armor-1
      writeLines(paste("Pirate #",i," attack hit!! Your ship takes 1 damage to its hull. You have ", ship$armor, "HP left.",sep=''))
      
      
        if(ship$armor<=0){
          cat("\014")
          writeLines("Your ship is about to sink. In a brave move, you raise the white flag and allow the pirates to board your ship. They search your ship and take what goods they find valuable.")
          writeLines(paste("You lose half of everything on your ship.....",sep=''))
          writeLines(paste(
            "You currently have, $",ship$money," in your purse and the following goods in your ship's cargo hold:",
            "\n  Ham     Amount in Cargo: ",ship$cargo.ham,
            "\n  Turkey  Amount in Cargo: ",ship$cargo.turkey,
            "\n  Beets   Amount in Cargo: ",ship$cargo.beets,
            "\n  Steak   Amount in Cargo: ",ship$cargo.steak,
            "\n  Pork    Amount in Cargo: ",ship$cargo.pork,
            "\n  Hotdogs Amount in Cargo: ",ship$cargo.hotdogs,sep=''))
          pause<-readline("Press 'Enter' to continue")
          break
        }
    }
    else{
      cat("\014")
      writeLines(paste("Pirate #",i," attack missed. You escape this attack unscathed.",sep=""))
      pause<-readline("Press 'Enter' to continue")
    }
      
  }
}#end of function
           

#fight()
fight<-function(){

#test parameters:
  #ship$cargo.n=3000
  #ship$guns=5
  #ship$armor<-20
  #pirate.prob<-1
#As cargo increase, p of fight increases
  pirate.prob<<-ship$cargo.n/200
  pirate.adjective<-sample(adjectives,1)
  pirate.name<-sample(nouns,1)
  pirate.rawn<-min((ship$cargo.n)/5,20)
  #select random number around the mean of z, if <0 then 1
  pirate.n<-max(c(round(rnorm(1,mean=pirate.rawn,sd=2)),1))
  pirate.reward<-round(sum(runif(pirate.n)*25))
  if(runif(1)<pirate.prob){
    pirate.n1<-5
      while(TRUE){
        
        writeLines(paste("Your path has been blocked by the dreaded '", simpleCap(pirate.adjective)," ", simpleCap(pirate.name),
                         "' pirates! There are ", pirate.n," pirate ships blocking your path",sep=''))
        writeLines(paste("What do you want to do?",
                   "\n  1 Attack",
                   "\n  2 Run",
                   "\n  3 Surrender",sep=''))
      
        x<-readline("[Enter a number]")
        
        #attack logic
        if(x==1){
          #player gets to shoot first.
          for(i in 1:ship$guns){
            pirate.n<-pirate.n-1
            pirate.n<<-pirate.n
            if(runif(1)>.20){
                           
              if(pirate.n>0){
                cat("\014")
                writeLines(paste("Cannon #",i," hit! There are ",pirate.n," pirates left.", sep=""))
                pause<-readline("Press 'Enter' to continue")
              }
              else{
                cat("\014") 
                writeLines(paste("Cannon #",i," hit! There are no more pirates left.", sep=""))
                pause<-readline("Press 'Enter' to continue")
                
                break
              }
            }
            else{cat("\014") 
                  writeLines(paste("Cannon #",i," missed! There are ",pirate.n," pirates left.", sep=""))
                 pause<-readline("Press 'Enter' to continue")
                 
            }
          }
          #Break loop if you killed them all
          if(pirate.n<=0){
            cat("\014")
            writeLines(paste("Congratulations, you survived the encounter with the dreaded '",
                             simpleCap(pirate.adjective)," ", simpleCap(pirate.name),"' Pirates.",
                             " Looting the wreckage you find, $",pirate.reward,
                             sep=''))
            ship$money<<-ship$money+pirate.reward
            pause<-readline("Press 'Enter' to continue")
             
            break
          }
          
          pirate.attack()
          
        }
        
    
        #run logic
        if(x==2){
            #player gets to RUN first.
            for(i in 1:ship$sails){
              if(runif(1)>.10){
                pirate.n<-pirate.n-1
                pirate.n<<-pirate.n
                if(pirate.n>0){
                  cat("\014") 
                  writeLines(paste("Your sails catch the wind and you outrun one of the ships. There are ",pirate.n," pirates left.", sep=""))
                  pause<-readline("Press 'Enter' to continue")
                  
                }
                else{
                  cat("\014") 
                  writeLines(paste("You've escaped them all. There are no more pirates left.", sep=""))
                  pause<-readline("Press 'Enter' to continue")
                  
                  break
                }
              }
              else{
                cat("\014")
                writeLines(paste("They keep pursute! There are ",pirate.n," pirates left.", sep=""))
                pause<-readline("Press 'Enter' to continue")
                 
              }
            }
            #Break loop if you killed them all
            if(pirate.n<=0){
              cat("\014")
              writeLines(paste("Congratulations, you survived the encounter with the dreaded '",
                               simpleCap(pirate.adjective)," ", simpleCap(pirate.name),"' Pirates.",
                               sep=''))
              pause<-readline("Press 'Enter' to continue")
              break
            }
          #then the ppirates attack
          pirate.attack()
        }
        
        #surrender logic
        if(x==3){
          
          ship$money <<-ship$money/2
          ship$cargo.ham <<-ship$cargo.ham/2
          ship$cargo.turkey <<-ship$cargo.turkey/2
          ship$cargo.beets <<-ship$cargo.beets/2
          ship$cargo.steak <<-ship$cargo.steak/2
          ship$cargo.pork <<-ship$cargo.pork/2
          ship$cargo.hotdogs <<-ship$cargo.hotdogs/2
          
          writeLines("In a brave move, you raise the white flag and allow the pirates to board your ship. They search your ship and take what goods they find valuable.")
          writeLines(paste("You lose half of everything on your ship.....",sep=''))
          writeLines(paste(
            "You currently have, $",ship$money," in your purse and the following goods in your ship's cargo hold:",
            "\n  Ham     Amount in Cargo: ",ship$cargo.ham,
            "\n  Turkey  Amount in Cargo: ",ship$cargo.turkey,
            "\n  Beets   Amount in Cargo: ",ship$cargo.beets,
            "\n  Steak   Amount in Cargo: ",ship$cargo.steak,
            "\n  Pork    Amount in Cargo: ",ship$cargo.pork,
            "\n  Hotdogs Amount in Cargo: ",ship$cargo.hotdogs,sep=''))
          

          pause<-readline("Press 'Enter' to continue")
          break
        }
        
        #Loser logic
        if(ship$armor<=0){
          
          ship$money <<-ship$money/2
          ship$cargo.ham <<-ship$cargo.ham/2
          ship$cargo.turkey <<-ship$cargo.turkey/2
          ship$cargo.beets <<-ship$cargo.beets/2
          ship$cargo.steak <<-ship$cargo.steak/2
          ship$cargo.pork <<-ship$cargo.pork/2
          ship$cargo.hotdogs <<-ship$cargo.hotdogs/2
          ship$cargo.n <<-ship$cargo.n/2
          
          writeLines("You cannot fight any longer, the pirates to board your ship. They search your ship and take what goods they find valuable.")
          writeLines(paste("You lose half of everything on your ship.....",sep=''))
          writeLines(paste(
            "You currently have, $",ship$money," in your purse and the following goods in your ship's cargo hold:",
            "\n  Ham     Amount in Cargo: ",ship$cargo.ham,
            "\n  Turkey  Amount in Cargo: ",ship$cargo.turkey,
            "\n  Beets   Amount in Cargo: ",ship$cargo.beets,
            "\n  Steak   Amount in Cargo: ",ship$cargo.steak,
            "\n  Pork    Amount in Cargo: ",ship$cargo.pork,
            "\n  Hotdogs Amount in Cargo: ",ship$cargo.hotdogs,sep=''))
          
          
          pause<-readline("Press 'Enter' to continue")
          break
          }
    }
  }
  else{writeLines("Nothing eventful to report on your travels... ")
    }
}#function end parenthesis
#fight()
#ship$cargo
#attack
#run

#help("<<-")
#Shipwrights: Represent Monty Carlo Simulations 
#Simulate Battle Survival Percentages

#Ship Profile

#Ship Upgrades
#Ship Repair
#cat("\014")
#Ship intialization



#

#Government Tariff Collectors: Bandit Algorithm Learning
#Searches Ship for goods: Select different places to hide goods to avoid taxes

########################
#Start Screen!!!!!    
########################

start.opening<- function(){
  writeLines(
"Welcome to the broken world of Taipan in R! 
Try not to enter unexpected values,
error handling is not fully developed.

")
  menuMaker(start.menu)
}
start.newgame<-function(){    
  ship <<- NULL
  ship.start()
  menuMaker(main.menu)
}
start.savegame<-function(){
  savename<-readline("Enter file name for your saved game (saves to working directory):")
  saveRDS(ship, paste(tolower(savename),".rds",sep=""))
  menuMaker(main.menu)
}
start.loadgame<-function(){
  loadname<-readline("Enter file name of your saved game (in working directory):")
  ship<<-readRDS(paste(tolower(loadname),".rds",sep=""))
  menuMaker(main.menu)
}
start.contgame<-function(){
  menuMaker(main.menu)
}

#Stores a String name and corresponding function that it should call if selected.
names<-c("New Game (deletes current game)",
         "Save Game",
         "Load Game",
         "Continue Game")
funcs<-c(start.newgame,
         start.savegame,
         start.loadgame,
         start.contgame)
start.menu<-cbind(names,funcs)

################################################################################
#Utility functions:
#Title Caps function#
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
################################################################################

################################################################################
#Intializeing Cities
################################################################################
################################################################################
city<-c("Juno","Hong Kong", "Sydney","Seattle","London")
status<-c(0,0,0,1,0)
cityid<-c(1,2,3,4,5)
loc<-as.data.frame(cbind(city,status,cityid))
################################################################################
################################################################################
#intializing prices
#today's prices'
ham    <-round(runif(5,min=15,max=25))
turkey <-round(runif(5,min=35,max=50))
beets  <-round(runif(5,min=25,max=80))
steak  <-round(runif(5,min=120,max=300))
pork   <-round(runif(5,min=60,max=75))
hotdogs<-round(runif(5,min=100,max=200))
market<-cbind(ham, turkey, beets, steak,pork, hotdogs)

#tomorrows prices
ham    <-round(runif(5,min=15,max=25))
turkey <-round(runif(5,min=35,max=50))
beets  <-round(runif(5,min=25,max=80))
steak  <-round(runif(5,min=120,max=300))
pork   <-round(runif(5,min=60,max=75))
hotdogs<-round(runif(5,min=100,max=200))
market1<-cbind(ham, turkey, beets, steak,pork, hotdogs)


################################################################################
#Ship Starting function
################################################################################
ship.start<-function(){
  #If Ship is missing, intialize!
  if(is.null(ship)){
    ship$name<-""
    ship$armor<-100
    ship$armor.max<-100
    ship$guns<-5
    ship$sails<-1
    ship$money<-10000
    ship$cargo<-500
    ship$cargo.n<-0
    ship$cargo.ham    <-0   
    ship$cargo.turkey <-0   
    ship$cargo.beets  <-0   
    ship$cargo.steak  <-0   
    ship$cargo.pork   <-0   
    ship$cargo.hotdogs<-0   
  }
  
  #Name Ship
  if(ship$name==''){
    cat("\014") 
    writeLines("Ahoy there! It looks like you have a new ship. It can't sail with out a new name... That is bad luck!")
    writeLines(paste("It used to be called 'The ",simpleCap(sample(adjectives,1))," ",simpleCap(sample(nouns,1)),"', but you can call her anything you want.",sep=''))
    ship$name<-readline("What do you want to name her?(do not include 'the'):")
    writeLines(paste("Ship Wright: The ",simpleCap(ship$name),"... that is a beautiful name...",sep=''))
    ship<<-ship
    pause<-readline("Press 'Enter' to continue")
  }
}
ship<-NULL
################################################################################
# Pirate ship characteristics
################################################################################


#setwd("D:/work/R workspace/Taipan")
#write.table(nouns)
#nouns2<-as.list(read.table("nouns.txt"))
#head(nouns2$x)

#intialize game
nouns<-c('people',
         'history',
         'way',
         'art',
         'world',
         'information',
         'map',
         'two',
         'family',
         'government',
         'health',
         'system',
         'computer',
         'meat',
         'year',
         'thanks',
         'music',
         'person',
         'reading',
         'method',
         'data',
         'food',
         'understanding',
         'theory',
         'law',
         'bird',
         'literature',
         'problem',
         'software',
         'control',
         'knowledge',
         'power',
         'ability',
         'economics',
         'love',
         'internet',
         'television',
         'science',
         'library',
         'nature',
         'fact',
         'product',
         'idea',
         'temperature',
         'investment',
         'area',
         'society',
         'activity',
         'story',
         'industry',
         'media',
         'thing',
         'oven',
         'community',
         'definition',
         'safety',
         'quality',
         'development',
         'language',
         'management',
         'player',
         'variety',
         'video',
         'week',
         'security',
         'country',
         'exam',
         'movie',
         'organization',
         'equipment',
         'physics',
         'analysis',
         'policy',
         'series',
         'thought',
         'basis',
         'boyfriend',
         'direction',
         'strategy',
         'technology',
         'army',
         'camera',
         'freedom',
         'paper',
         'environment',
         'child',
         'instance',
         'month',
         'truth',
         'marketing',
         'university',
         'writing',
         'article',
         'department',
         'difference',
         'goal',
         'news',
         'audience',
         'fishing',
         'growth',
         'income',
         'marriage',
         'user',
         'combination',
         'failure',
         'meaning',
         'medicine',
         'philosophy',
         'teacher',
         'communication',
         'night',
         'chemistry',
         'disease',
         'disk',
         'energy',
         'nation',
         'road',
         'role',
         'soup',
         'advertising',
         'location',
         'success',
         'addition',
         'apartment',
         'education',
         'math',
         'moment',
         'painting',
         'politics',
         'attention',
         'decision',
         'event',
         'property',
         'shopping',
         'student',
         'wood',
         'competition',
         'distribution',
         'entertainment',
         'office',
         'population',
         'president',
         'unit',
         'category',
         'cigarette',
         'context',
         'introduction',
         'opportunity',
         'performance',
         'driver',
         'flight',
         'length',
         'magazine',
         'newspaper',
         'relationship',
         'teaching',
         'cell',
         'dealer',
         'finding',
         'lake',
         'member',
         'message',
         'phone',
         'scene',
         'appearance',
         'association',
         'concept',
         'customer',
         'death',
         'discussion',
         'housing',
         'inflation',
         'insurance',
         'mood',
         'woman',
         'advice',
         'blood',
         'effort',
         'expression',
         'importance',
         'opinion',
         'payment',
         'reality',
         'responsibility',
         'situation',
         'skill',
         'statement',
         'wealth',
         'application',
         'city',
         'county',
         'depth',
         'estate',
         'foundation',
         'grandmother',
         'heart',
         'perspective',
         'photo',
         'recipe',
         'studio',
         'topic',
         'collection',
         'depression',
         'imagination',
         'passion',
         'percentage',
         'resource',
         'setting',
         'ad',
         'agency',
         'college',
         'connection',
         'criticism',
         'debt',
         'description',
         'memory',
         'patience',
         'secretary',
         'solution',
         'administration',
         'aspect',
         'attitude',
         'director',
         'personality',
         'psychology',
         'recommendation',
         'response',
         'selection',
         'storage',
         'version',
         'alcohol',
         'argument',
         'complaint',
         'contract',
         'emphasis',
         'highway',
         'loss',
         'membership',
         'possession',
         'preparation',
         'steak',
         'union',
         'agreement',
         'cancer',
         'currency',
         'employment',
         'engineering',
         'entry',
         'interaction',
         'mixture',
         'preference',
         'region',
         'republic',
         'tradition',
         'virus',
         'actor',
         'classroom',
         'delivery',
         'device',
         'difficulty',
         'drama',
         'election',
         'engine',
         'football',
         'guidance',
         'hotel',
         'owner',
         'priority',
         'protection',
         'suggestion',
         'tension',
         'variation',
         'anxiety',
         'atmosphere',
         'awareness',
         'bath',
         'bread',
         'candidate',
         'climate',
         'comparison',
         'confusion',
         'construction',
         'elevator',
         'emotion',
         'employee',
         'employer',
         'guest',
         'height',
         'leadership',
         'mall',
         'manager',
         'operation',
         'recording',
         'sample',
         'transportation',
         'charity',
         'cousin',
         'disaster',
         'editor',
         'efficiency',
         'excitement',
         'extent',
         'feedback',
         'guitar',
         'homework',
         'leader',
         'mom',
         'outcome',
         'permission',
         'presentation',
         'promotion',
         'reflection',
         'refrigerator',
         'resolution',
         'revenue',
         'session',
         'singer',
         'tennis',
         'basket',
         'bonus',
         'cabinet',
         'childhood',
         'church',
         'clothes',
         'coffee',
         'dinner',
         'drawing',
         'hair',
         'hearing',
         'initiative',
         'judgment',
         'lab',
         'measurement',
         'mode',
         'mud',
         'orange',
         'poetry',
         'police',
         'possibility',
         'procedure',
         'queen',
         'ratio',
         'relation',
         'restaurant',
         'satisfaction',
         'sector',
         'signature',
         'significance',
         'song',
         'tooth',
         'town',
         'vehicle',
         'volume',
         'wife',
         'accident',
         'airport',
         'appointment',
         'arrival',
         'assumption',
         'baseball',
         'chapter',
         'committee',
         'conversation',
         'database',
         'enthusiasm',
         'error',
         'explanation',
         'farmer',
         'gate',
         'girl',
         'hall',
         'historian',
         'hospital',
         'injury',
         'instruction',
         'maintenance',
         'manufacturer',
         'meal',
         'perception',
         'pie',
         'poem',
         'presence',
         'proposal',
         'reception',
         'replacement',
         'revolution',
         'river',
         'son',
         'speech',
         'tea',
         'village',
         'warning',
         'winner',
         'worker',
         'writer',
         'assistance',
         'breath',
         'buyer',
         'chest',
         'chocolate',
         'conclusion',
         'contribution',
         'cookie',
         'courage',
         'dad',
         'desk',
         'drawer',
         'establishment',
         'examination',
         'garbage',
         'grocery',
         'honey',
         'impression',
         'improvement',
         'independence',
         'insect',
         'inspection',
         'inspector',
         'king',
         'ladder',
         'menu',
         'penalty',
         'piano',
         'potato',
         'profession',
         'professor',
         'quantity',
         'reaction',
         'requirement',
         'salad',
         'sister',
         'supermarket',
         'tongue',
         'weakness',
         'wedding',
         'affair',
         'ambition',
         'analyst',
         'apple',
         'assignment',
         'assistant',
         'bathroom',
         'bedroom',
         'beer',
         'birthday',
         'celebration',
         'championship',
         'cheek',
         'client',
         'consequence',
         'departure',
         'diamond',
         'dirt',
         'ear',
         'fortune',
         'friendship',
         'funeral',
         'gene',
         'girlfriend',
         'hat',
         'indication',
         'intention',
         'lady',
         'midnight',
         'negotiation',
         'obligation',
         'passenger',
         'pizza',
         'platform',
         'poet',
         'pollution',
         'recognition',
         'reputation',
         'shirt',
         'sir',
         'speaker',
         'stranger',
         'surgery',
         'sympathy',
         'tale',
         'throat',
         'trainer',
         'uncle',
         'youth',
         'time',
         'work',
         'film',
         'water',
         'money',
         'example',
         'while',
         'business',
         'study',
         'game',
         'life',
         'form',
         'air',
         'day',
         'place',
         'number',
         'part',
         'field',
         'fish',
         'back',
         'process',
         'heat',
         'hand',
         'experience',
         'job',
         'book',
         'end',
         'point',
         'type',
         'home',
         'economy',
         'value',
         'body',
         'market',
         'guide',
         'interest',
         'state',
         'radio',
         'course',
         'company',
         'price',
         'size',
         'card',
         'list',
         'mind',
         'trade',
         'line',
         'care',
         'group',
         'risk',
         'word',
         'fat',
         'force',
         'key',
         'light',
         'training',
         'name',
         'school',
         'top',
         'amount',
         'level',
         'order',
         'practice',
         'research',
         'sense',
         'service',
         'piece',
         'web',
         'boss',
         'sport',
         'fun',
         'house',
         'page',
         'term',
         'test',
         'answer',
         'sound',
         'focus',
         'matter',
         'kind',
         'soil',
         'board',
         'oil',
         'picture',
         'access',
         'garden',
         'range',
         'rate',
         'reason',
         'future',
         'site',
         'demand',
         'exercise',
         'image',
         'case',
         'cause',
         'coast',
         'action',
         'age',
         'bad',
         'boat',
         'record',
         'result',
         'section',
         'building',
         'mouse',
         'cash',
         'class',
         'nothing',
         'period',
         'plan',
         'store',
         'tax',
         'side',
         'subject',
         'space',
         'rule',
         'stock',
         'weather',
         'chance',
         'figure',
         'man',
         'model',
         'source',
         'beginning',
         'earth',
         'program',
         'chicken',
         'design',
         'feature',
         'head',
         'material',
         'purpose',
         'question',
         'rock',
         'salt',
         'act',
         'birth',
         'car',
         'dog',
         'object',
         'scale',
         'sun',
         'note',
         'profit',
         'rent',
         'speed',
         'style',
         'war',
         'bank',
         'craft',
         'half',
         'inside',
         'outside',
         'standard',
         'bus',
         'exchange',
         'eye',
         'fire',
         'position',
         'pressure',
         'stress',
         'advantage',
         'benefit',
         'box',
         'frame',
         'issue',
         'step',
         'cycle',
         'face',
         'item',
         'metal',
         'paint',
         'review',
         'room',
         'screen',
         'structure',
         'view',
         'account',
         'ball',
         'discipline',
         'medium',
         'share',
         'balance',
         'bit',
         'black',
         'bottom',
         'choice',
         'gift',
         'impact',
         'machine',
         'shape',
         'tool',
         'wind',
         'address',
         'average',
         'career',
         'culture',
         'morning',
         'pot',
         'sign',
         'table',
         'task',
         'condition',
         'contact',
         'credit',
         'egg',
         'hope',
         'ice',
         'network',
         'north',
         'square',
         'attempt',
         'date',
         'effect',
         'link',
         'post',
         'star',
         'voice',
         'capital',
         'challenge',
         'friend',
         'self',
         'shot',
         'brush',
         'couple',
         'debate',
         'exit',
         'front',
         'function',
         'lack',
         'living',
         'plant',
         'plastic',
         'spot',
         'summer',
         'taste',
         'theme',
         'track',
         'wing',
         'brain',
         'button',
         'click',
         'desire',
         'foot',
         'gas',
         'influence',
         'notice',
         'rain',
         'wall',
         'base',
         'damage',
         'distance',
         'feeling',
         'pair',
         'savings',
         'staff',
         'sugar',
         'target',
         'text',
         'animal',
         'author',
         'budget',
         'discount',
         'file',
         'ground',
         'lesson',
         'minute',
         'officer',
         'phase',
         'reference',
         'register',
         'sky',
         'stage',
         'stick',
         'title',
         'trouble',
         'bowl',
         'bridge',
         'campaign',
         'character',
         'club',
         'edge',
         'evidence',
         'fan',
         'letter',
         'lock',
         'maximum',
         'novel',
         'option',
         'pack',
         'park',
         'plenty',
         'quarter',
         'skin',
         'sort',
         'weight',
         'baby',
         'background',
         'carry',
         'dish',
         'factor',
         'fruit',
         'glass',
         'joint',
         'master',
         'muscle',
         'red',
         'strength',
         'traffic',
         'trip',
         'vegetable',
         'appeal',
         'chart',
         'gear',
         'ideal',
         'kitchen',
         'land',
         'log',
         'mother',
         'net',
         'party',
         'principle',
         'relative',
         'sale',
         'season',
         'signal',
         'spirit',
         'street',
         'tree',
         'wave',
         'belt',
         'bench',
         'commission',
         'copy',
         'drop',
         'minimum',
         'path',
         'progress',
         'project',
         'sea',
         'south',
         'status',
         'stuff',
         'ticket',
         'tour',
         'angle',
         'blue',
         'breakfast',
         'confidence',
         'daughter',
         'degree',
         'doctor',
         'dot',
         'dream',
         'duty',
         'essay',
         'father',
         'fee',
         'finance',
         'hour',
         'juice',
         'limit',
         'luck',
         'milk',
         'mouth',
         'peace',
         'pipe',
         'seat',
         'stable',
         'storm',
         'substance',
         'team',
         'trick',
         'afternoon',
         'bat',
         'beach',
         'blank',
         'catch',
         'chain',
         'consideration',
         'cream',
         'crew',
         'detail',
         'gold',
         'interview',
         'kid',
         'mark',
         'match',
         'mission',
         'pain',
         'pleasure',
         'score',
         'screw',
         'sex',
         'shop',
         'shower',
         'suit',
         'tone',
         'window',
         'agent',
         'band',
         'block',
         'bone',
         'calendar',
         'cap',
         'coat',
         'contest',
         'corner',
         'court',
         'cup',
         'district',
         'door',
         'east',
         'finger',
         'garage',
         'guarantee',
         'hole',
         'hook',
         'implement',
         'layer',
         'lecture',
         'lie',
         'manner',
         'meeting',
         'nose',
         'parking',
         'partner',
         'profile',
         'respect',
         'rice',
         'routine',
         'schedule',
         'swimming',
         'telephone',
         'tip',
         'winter',
         'airline',
         'bag',
         'battle',
         'bed',
         'bill',
         'bother',
         'cake',
         'code',
         'curve',
         'designer',
         'dimension',
         'dress',
         'ease',
         'emergency',
         'evening',
         'extension',
         'farm',
         'fight',
         'gap',
         'grade',
         'holiday',
         'horror',
         'horse',
         'host',
         'husband',
         'loan',
         'mistake',
         'mountain',
         'nail',
         'noise',
         'occasion',
         'package',
         'patient',
         'pause',
         'phrase',
         'proof',
         'race',
         'relief',
         'sand',
         'sentence',
         'shoulder',
         'smoke',
         'stomach',
         'string',
         'tourist',
         'towel',
         'vacation',
         'west',
         'wheel',
         'wine',
         'arm',
         'aside',
         'associate',
         'bet',
         'blow',
         'border',
         'branch',
         'breast',
         'brother',
         'buddy',
         'bunch',
         'chip',
         'coach',
         'cross',
         'document',
         'draft',
         'dust',
         'expert',
         'floor',
         'god',
         'golf',
         'habit',
         'iron',
         'judge',
         'knife',
         'landscape',
         'league',
         'mail',
         'mess',
         'native',
         'opening',
         'parent',
         'pattern',
         'pin',
         'pool',
         'pound',
         'request',
         'salary',
         'shame',
         'shelter',
         'shoe',
         'silver',
         'tackle',
         'tank',
         'trust',
         'assist',
         'bake',
         'bar',
         'bell',
         'bike',
         'blame',
         'boy',
         'brick',
         'chair',
         'closet',
         'clue',
         'collar',
         'comment',
         'conference',
         'devil',
         'diet',
         'fear',
         'fuel',
         'glove',
         'jacket',
         'lunch',
         'monitor',
         'mortgage',
         'nurse',
         'pace',
         'panic',
         'peak',
         'plane',
         'reward',
         'row',
         'sandwich',
         'shock',
         'spite',
         'spray',
         'surprise',
         'till',
         'transition',
         'weekend',
         'welcome',
         'yard',
         'alarm',
         'bend',
         'bicycle',
         'bite',
         'blind',
         'bottle',
         'cable',
         'candle',
         'clerk',
         'cloud',
         'concert',
         'counter',
         'flower',
         'grandfather',
         'harm',
         'knee',
         'lawyer',
         'leather',
         'load',
         'mirror',
         'neck',
         'pension',
         'plate',
         'purple',
         'ruin',
         'ship',
         'skirt',
         'slice',
         'snow',
         'specialist',
         'stroke',
         'switch',
         'trash',
         'tune',
         'zone',
         'anger',
         'award',
         'bid',
         'bitter',
         'boot',
         'bug',
         'camp',
         'candy',
         'carpet',
         'cat',
         'champion',
         'channel',
         'clock',
         'comfort',
         'cow',
         'crack',
         'engineer',
         'entrance',
         'fault',
         'grass',
         'guy',
         'hell',
         'highlight',
         'incident',
         'island',
         'joke',
         'jury',
         'leg',
         'lip',
         'mate',
         'motor',
         'nerve',
         'passage',
         'pen',
         'pride',
         'priest',
         'prize',
         'promise',
         'resident',
         'resort',
         'ring',
         'roof',
         'rope',
         'sail',
         'scheme',
         'script',
         'sock',
         'station',
         'toe',
         'tower',
         'truck',
         'witness',
         'one',
         'many',
         'most',
         'other',
         'use',
         'make',
         'good',
         'look',
         'help',
         'go',
         'great',
         'being',
         'few',
         'might',
         'still',
         'public',
         'read',
         'keep',
         'start',
         'give',
         'human',
         'local',
         'general',
         'she',
         'specific',
         'long',
         'play',
         'feel',
         'high',
         'tonight',
         'put',
         'common',
         'set',
         'change',
         'simple',
         'past',
         'big',
         'possible',
         'particular',
         'today',
         'major',
         'personal',
         'current',
         'national',
         'cut',
         'natural',
         'physical',
         'show',
         'try',
         'check',
         'second',
         'call',
         'move',
         'pay',
         'let',
         'increase',
         'single',
         'individual',
         'turn',
         'ask',
         'buy',
         'guard',
         'hold',
         'main',
         'offer',
         'potential',
         'professional',
         'international',
         'travel',
         'cook',
         'alternative',
         'following',
         'special',
         'working',
         'whole',
         'dance',
         'excuse',
         'cold',
         'commercial',
         'low',
         'purchase',
         'deal',
         'primary',
         'worth',
         'fall',
         'necessary',
         'positive',
         'produce',
         'search',
         'present',
         'spend',
         'talk',
         'creative',
         'tell',
         'cost',
         'drive',
         'green',
         'support',
         'glad',
         'remove',
         'return',
         'run',
         'complex',
         'due',
         'effective',
         'middle',
         'regular',
         'reserve',
         'independent',
         'leave',
         'original',
         'reach',
         'rest',
         'serve',
         'watch',
         'beautiful',
         'charge',
         'active',
         'break',
         'negative',
         'safe',
         'stay',
         'visit',
         'visual',
         'affect',
         'cover',
         'report',
         'rise',
         'walk',
         'white',
         'beyond',
         'junior',
         'pick',
         'unique',
         'anything',
         'classic',
         'final',
         'lift',
         'mix',
         'private',
         'stop',
         'teach',
         'western',
         'concern',
         'familiar',
         'fly',
         'official',
         'broad',
         'comfortable',
         'gain',
         'maybe',
         'rich',
         'save',
         'stand',
         'young',
         'fail',
         'heavy',
         'hello',
         'lead',
         'listen',
         'valuable',
         'worry',
         'handle',
         'leading',
         'meet',
         'release',
         'sell',
         'finish',
         'normal',
         'press',
         'ride',
         'secret',
         'spread',
         'spring',
         'tough',
         'wait',
         'brown',
         'deep',
         'display',
         'flow',
         'hit',
         'objective',
         'shoot',
         'touch',
         'cancel',
         'chemical',
         'cry',
         'dump',
         'extreme',
         'push',
         'conflict',
         'eat',
         'fill',
         'formal',
         'jump',
         'kick',
         'opposite',
         'pass',
         'pitch',
         'remote',
         'total',
         'treat',
         'vast',
         'abuse',
         'beat',
         'burn',
         'deposit',
         'print',
         'raise',
         'sleep',
         'somewhere',
         'advance',
         'anywhere',
         'consist',
         'dark',
         'double',
         'draw',
         'equal',
         'fix',
         'hire',
         'internal',
         'join',
         'kill',
         'sensitive',
         'tap',
         'win',
         'attack',
         'claim',
         'constant',
         'drag',
         'drink',
         'guess',
         'minor',
         'pull',
         'raw',
         'soft',
         'solid',
         'wear',
         'weird',
         'wonder',
         'annual',
         'count',
         'dead',
         'doubt',
         'feed',
         'forever',
         'impress',
         'nobody',
         'repeat',
         'round',
         'sing',
         'slide',
         'strip',
         'whereas',
         'wish',
         'combine',
         'command',
         'dig',
         'divide',
         'equivalent',
         'hang',
         'hunt',
         'initial',
         'march',
         'mention',
         'smell',
         'spiritual',
         'survey',
         'tie',
         'adult',
         'brief',
         'crazy',
         'escape',
         'gather',
         'hate',
         'prior',
         'repair',
         'rough',
         'sad',
         'scratch',
         'sick',
         'strike',
         'employ',
         'external',
         'hurt',
         'illegal',
         'laugh',
         'lay',
         'mobile',
         'nasty',
         'ordinary',
         'respond',
         'royal',
         'senior',
         'split',
         'strain',
         'struggle',
         'swim',
         'train',
         'upper',
         'wash',
         'yellow',
         'convert',
         'crash',
         'dependent',
         'fold',
         'funny',
         'grab',
         'hide',
         'miss',
         'permit',
         'quote',
         'recover',
         'resolve',
         'roll',
         'sink',
         'slip',
         'spare',
         'suspect',
         'sweet',
         'swing',
         'twist',
         'upstairs',
         'usual',
         'abroad',
         'brave',
         'calm',
         'concentrate',
         'estimate',
         'grand',
         'male',
         'mine',
         'prompt',
         'quiet',
         'refuse',
         'regret',
         'reveal',
         'rush',
         'shake',
         'shift',
         'shine',
         'steal',
         'suck',
         'surround',
         'anybody',
         'bear',
         'brilliant',
         'dare',
         'dear',
         'delay',
         'drunk',
         'female',
         'hurry',
         'inevitable',
         'invite',
         'kiss',
         'neat',
         'pop',
         'punch',
         'quit',
         'reply',
         'representative',
         'resist',
         'rip',
         'rub',
         'silly',
         'smile',
         'spell',
         'stretch',
         'stupid',
         'tear',
         'temporary',
         'tomorrow',
         'wake',
         'wrap',
         'yesterday')
adjectives<-c('different',
              'used',
              'important',
              'every',
              'large',
              'available',
              'popular',
              'able',
              'basic',
              'known',
              'various',
              'difficult',
              'several',
              'united',
              'historical',
              'hot',
              'useful',
              'mental',
              'scared',
              'additional',
              'emotional',
              'old',
              'political',
              'similar',
              'healthy',
              'financial',
              'medical',
              'traditional',
              'federal',
              'entire',
              'strong',
              'actual',
              'significant',
              'successful',
              'electrical',
              'expensive',
              'pregnant',
              'intelligent',
              'interesting',
              'poor',
              'happy',
              'responsible',
              'cute',
              'helpful',
              'recent',
              'willing',
              'nice',
              'wonderful',
              'impossible',
              'serious',
              'huge',
              'rare',
              'technical',
              'typical',
              'competitive',
              'critical',
              'electronic',
              'immediate',
              'aware',
              'educational',
              'environmental',
              'global',
              'legal',
              'relevant',
              'accurate',
              'capable',
              'dangerous',
              'dramatic',
              'efficient',
              'powerful',
              'foreign',
              'hungry',
              'practical',
              'psychological',
              'severe',
              'suitable',
              'numerous',
              'sufficient',
              'unusual',
              'consistent',
              'cultural',
              'existing',
              'famous',
              'pure',
              'afraid',
              'obvious',
              'careful',
              'latter',
              'unhappy',
              'acceptable',
              'aggressive',
              'boring',
              'distinct',
              'eastern',
              'logical',
              'reasonable',
              'strict',
              'administrative',
              'automatic',
              'civil',
              'former',
              'massive',
              'southern',
              'unfair',
              'visible',
              'alive',
              'angry',
              'desperate',
              'exciting',
              'friendly',
              'lucky',
              'realistic',
              'sorry',
              'ugly',
              'unlikely',
              'anxious',
              'comprehensive',
              'curious',
              'impressive',
              'informal',
              'inner',
              'pleasant',
              'sexual',
              'sudden',
              'terrible',
              'unable',
              'weak',
              'wooden',
              'asleep',
              'confident',
              'conscious',
              'decent',
              'embarrassed',
              'guilty',
              'lonely',
              'mad',
              'nervous',
              'odd',
              'remarkable',
              'substantial',
              'suspicious',
              'tall',
              'tiny',
              'more',
              'some',
              'one',
              'all',
              'many',
              'most',
              'other',
              'such',
              'even',
              'new',
              'just',
              'good',
              'any',
              'each',
              'much',
              'own',
              'great',
              'another',
              'same',
              'few',
              'free',
              'right',
              'still',
              'best',
              'public',
              'human',
              'both',
              'local',
              'sure',
              'better',
              'general',
              'specific',
              'enough',
              'long',
              'small',
              'less',
              'high',
              'certain',
              'little',
              'common',
              'next',
              'simple',
              'hard',
              'past',
              'big',
              'possible',
              'particular',
              'real',
              'major',
              'personal',
              'current',
              'left',
              'national',
              'least',
              'natural',
              'physical',
              'short',
              'last',
              'single',
              'individual',
              'main',
              'potential',
              'professional',
              'international',
              'lower',
              'open',
              'according',
              'alternative',
              'special',
              'working',
              'TRUE',
              'whole',
              'clear',
              'dry',
              'easy',
              'cold',
              'commercial',
              'full',
              'low',
              'primary',
              'worth',
              'necessary',
              'positive',
              'present',
              'close',
              'creative',
              'green',
              'late',
              'fit',
              'glad',
              'proper',
              'complex',
              'content',
              'due',
              'effective',
              'middle',
              'regular',
              'fast',
              'independent',
              'original',
              'wide',
              'beautiful',
              'complete',
              'active',
              'negative',
              'safe',
              'visual',
              'wrong',
              'ago',
              'quick',
              'ready',
              'straight',
              'white',
              'direct',
              'excellent',
              'extra',
              'junior',
              'pretty',
              'unique',
              'classic',
              'final',
              'overall',
              'private',
              'separate',
              'western',
              'alone',
              'familiar',
              'official',
              'perfect',
              'bright',
              'broad',
              'comfortable',
              'flat',
              'rich',
              'warm',
              'young',
              'heavy',
              'valuable',
              'correct',
              'leading',
              'slow',
              'clean',
              'fresh',
              'normal',
              'secret',
              'tough',
              'brown',
              'cheap',
              'deep',
              'objective',
              'secure',
              'thin',
              'chemical',
              'cool',
              'extreme',
              'exact',
              'fair',
              'fine',
              'formal',
              'opposite',
              'remote',
              'total',
              'vast',
              'lost',
              'smooth',
              'dark',
              'double',
              'equal',
              'firm',
              'frequent',
              'internal',
              'sensitive',
              'constant',
              'minor',
              'previous',
              'raw',
              'soft',
              'solid',
              'weird',
              'amazing',
              'annual',
              'busy',
              'dead',
              'pretty',
              'round',
              'sharp',
              'thick',
              'wise',
              'equivalent',
              'initial',
              'narrow',
              'nearby',
              'proud',
              'spiritual',
              'wild',
              'adult',
              'apart',
              'brief',
              'crazy',
              'prior',
              'rough',
              'sad',
              'sick',
              'strange',
              'external',
              'illegal',
              'loud',
              'mobile',
              'nasty',
              'ordinary',
              'royal',
              'senior',
              'super',
              'tight',
              'upper',
              'yellow',
              'dependent',
              'funny',
              'gross',
              'ill',
              'spare',
              'sweet',
              'upstairs',
              'usual',
              'brave',
              'calm',
              'dirty',
              'downtown',
              'grand',
              'honest',
              'loose',
              'male',
              'quiet',
              'brilliant',
              'dear',
              'drunk',
              'empty',
              'female',
              'inevitable',
              'neat',
              'ok',
              'representative',
              'silly',
              'slight',
              'smart',
              'stupid',
              'temporary',
              'weekly',
              'that',
              'this',
              'what',
              'which',
              'time',
              'these',
              'work',
              'no',
              'only',
              'then',
              'first',
              'money',
              'over',
              'business',
              'his',
              'game',
              'think',
              'after',
              'life',
              'day',
              'home',
              'economy',
              'away',
              'either',
              'fat',
              'key',
              'training',
              'top',
              'level',
              'far',
              'fun',
              'house',
              'kind',
              'future',
              'action',
              'live',
              'period',
              'subject',
              'mean',
              'stock',
              'chance',
              'beginning',
              'upset',
              'chicken',
              'head',
              'material',
              'salt',
              'car',
              'appropriate',
              'inside',
              'outside',
              'standard',
              'medium',
              'choice',
              'north',
              'square',
              'born',
              'capital',
              'shot',
              'front',
              'living',
              'plastic',
              'express',
              'feeling',
              'otherwise',
              'plus',
              'savings',
              'animal',
              'budget',
              'minute',
              'character',
              'maximum',
              'novel',
              'plenty',
              'select',
              'background',
              'forward',
              'glass',
              'joint',
              'master',
              'red',
              'vegetable',
              'ideal',
              'kitchen',
              'mother',
              'party',
              'relative',
              'signal',
              'street',
              'connect',
              'minimum',
              'sea',
              'south',
              'status',
              'daughter',
              'hour',
              'trick',
              'afternoon',
              'gold',
              'mission',
              'agent',
              'corner',
              'east',
              'neither',
              'parking',
              'routine',
              'swimming',
              'winter',
              'airline',
              'designer',
              'dress',
              'emergency',
              'evening',
              'extension',
              'holiday',
              'horror',
              'mountain',
              'patient',
              'proof',
              'west',
              'wine',
              'expert',
              'native',
              'opening',
              'silver',
              'waste',
              'plane',
              'leather',
              'purple',
              'specialist',
              'bitter',
              'incident',
              'motor',
              'pretend',
              'prize',
              'resident')

########################
#Run Game
########################
while(TRUE){
  cat("\014") 
  writeLines("  
  ######################################################
  #                   ________________                 #
  #                  |  Welcome to    |                #
  #                  |    Taipan      |                #
  #                  |       in R     |                #
  #                  |________________|                #
  #                          |                         #
  #          ________________|_______________          #
  #          \\  O   O   O   O   O   O   O   /          #
  #           \\   Programmed by Adamishere /           #
  #            \\__________________________/            #
  #                                                    #
  #   github: Adamishere                               #
  ######################################################
")
  pause<-readline("Press 'Enter' to continue")
  cat("\014") 
  start.opening()
}


