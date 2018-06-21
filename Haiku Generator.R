#install.packages("qdapDictionaries")
library(qdapDictionaries)

dict<-DICTIONARY
table(dict$syllables)

for(i in c(5,7,5)){
  syll_cnt<-i
  line1<-NULL

    while(TRUE){
      tempdict<-dict[dict$syllables<=syll_cnt,]
      rowid<-sample(1:nrow(tempdict),1)
      selectword<-tempdict[rowid,]
      syll_cnt<-syll_cnt-selectword$syllables
      line1<-c(line1,tempdict[rowid,"word"])
      if(syll_cnt <= 0){break}
    }
  print(paste(line1, collapse = ' '))
}


