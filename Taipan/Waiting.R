

looper<-for( i in 1:1000){
  #cat("\014") 
  writeLines(paste("Some may call me the '",
                   capitalize(sample(adjectives,1))," ", capitalize(sample(nouns,1)),
                     ",' but you are truly the '",
                   capitalize(sample(adjectives,1))," ", capitalize(sample(nouns,1)),".'", sep = ''))
  Sys.sleep(10)
  #print(i)
}

Sys.sleep(1)

sample
?tolower
library(Hmisc)
capitalize()