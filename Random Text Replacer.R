library(pps)

#Probability of each word
letterfreq.vowels<-("
letter pct
'E' 	12.02
'A' 	8.12
'I' 	7.31
'U' 	2.88
'O' 	7.68
'Y' 	2.11")
letterfreq.consonance<-("
letter pct
'T' 	9.10
'N' 	6.95
'S' 	6.28
'R' 	6.02
'H' 	5.92
'D' 	4.32
'L' 	3.98
'C' 	2.71
'M' 	2.61
'F' 	2.30
'Y' 	2.11
'W' 	2.09
'G' 	2.03
'P' 	1.82
'B' 	1.49
'V' 	1.11
'K' 	0.6
'X' 	0.1
'Q' 	0.1
'J' 	0.1
'Z' 	0.07")

data.v <- read.table(text=letterfreq.vowels, 
                     header = TRUE, 
                     stringsAsFactors = FALSE)
data.c <- read.table(text=letterfreq.consonance, 
                     header = TRUE,
                     stringsAsFactors = FALSE)

#Letter Transposer
#initialize tables and functions
#alpha-numeric table
vowels<-c('a','e','i','o','u','y')
consonance<-c(
  'b','c','d','f','g','h','j',
  'k','l','m','n','p','q','r',
  's','t','v','w','x','y','z')

plain.text<-"The quick brown fox jumped over the lazy dog"
#Convert text to dataframe
message<-unlist(strsplit(tolower(plain.text),''))
n<-length(message)
msg.df<-as.data.frame(message,stringsAsFactors = FALSE)
msg.df$message

out.message <- NULL
for(i in msg.df$message){
  if(i  %in% vowels){
    out.message<-paste(out.message,
                       data.v[ppss(data.v$pct,1),"letter"],
                       sep = "")
  } else if(i %in% consonance){
    out.message<-paste(out.message,
                       data.c[ppss(data.c$pct,1),"letter"],
                       sep = "")
  } else {
    out.message<-paste(out.message,
                       i,
                       sep = "")
  }
}
out.message


num<-as.data.frame(1:nrow(let))
rot0<-cbind(let,num)
names(rot0)<-c('let','num')

#returns the numeric equivelent of the letter
converter<-function(x){
  z<-rot0[rot0$let==x,2]
  return(z)
}
#returns the string equivelent of the number
converter2<-function(x){
  z<-rot0[rot0$num==x,1]
  return(z)
}