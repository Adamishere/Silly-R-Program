#MARTINGALE Betting System Simulation

step_max<-NULL
wallet_max<-NULL
wallet_now<-NULL

for( i in 1:1000){
  min_bet<- 2       #Min Bet
  max_bet <-9000    #Max Bet
  rb_prob <-18/38   #Win probability
  wallet  <-200     #Current Funds
  m<-2              #Win Multiplier
  cur_bet <-min_bet #Current Bet
  
  #Quitting Conditions
  #If you reach max attemps and you have more money that you stated with
  #Take profit
  
  max_step<-40 #Max Attempts before reset
  break_even <-wallet #Break even point
    
  #initialize
  step<-0           #Step Counter
  max_wallet<-0     #Max Win Counter
  
  while(TRUE){
    print(paste("Step:",step,"Current Bet",cur_bet," Wallet:", wallet, "Max Win:", max_wallet))
    
    if(cur_bet<wallet){
      wallet<-wallet-cur_bet
    }else{
      print("Out of funds")
      break
    }
    if(cur_bet>max_bet){
      print("Max Bet")
      break
    }
    
    if(runif(1)<rb_prob){
      print("Win")
      wallet<-wallet+m*cur_bet
      cur_bet<-min_bet
    }else{
      print("Lose")
      cur_bet<-2*cur_bet
    }
    
    #Step counter
    step<-step+1
    
    #If you reach max attemps and you have more money that you stated with
    #Take profit
    if(step>max_step & wallet>break_even){
      break
      }
    #Max win amount
    max_wallet<-max(max_wallet,wallet)
  }
  wallet_now<-c(wallet_now,wallet)
  step_max<-c(step_max,step)
  wallet_max<-c(wallet_max,max_wallet)
}

#number of steps before you quit
hist(step_max)
summary(step_max)

#Maximum you can make given conditions
hist(wallet_max)
summary(wallet_max)

#Money you leave with given conditions
hist(wallet_now)
summary(wallet_now)
#Average amount of money left:
sum(wallet_now)/length(wallet_now)

