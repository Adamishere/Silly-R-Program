#install.packages("rcorpora")
library(rcorpora)
library(dplyr)
library(tools)

tarot<-as.data.frame(corpora("divination/tarot_interpretations")$tarot_interpretations)

drawme<-function(){
  drawcard<-sample_n(tarot,1)
  
  writeLines(
    paste(
        "You chose the:",toTitleCase(unlist(drawcard$name)),"\n\n",
        "Light Meaning:\n", paste(unlist(drawcard$meaning$light),collapse = ', '),"\n\n",
        "Shadow Meaning:\n", paste(unlist(drawcard$meaning$shadow),collapse = ', '),"\n\n",
        "This card is associated with:", paste(unlist(drawcard$keywords),collapse = ', ')
        )
  )
}
      
drawme()

