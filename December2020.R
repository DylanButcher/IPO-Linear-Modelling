library(readxl)

december <- readxl::read_excel("IPOSHEET.xlsx",sheet="Dec2020PC",col_names = TRUE,row.names(TRUE))
december <-t(december)
december <- december[-c(2,3), ]
december.names <- december[1,]
colnames(december) <- december.names
december <- december[-c(1),]

count <- c(1:29)

pdf("plottings1.pdf")

for (x in december.names){
  plot.new()
  print(x)
  lm = lm(december[,x]~count)
  summary(lm)
  plot(december[,x]~count,pch=19,xlab="Days after IPO",ylab="Percentage Change",main=x)
  abline(lm)
}
dev.off()
  

positiveCount = 0
for (x in december.names){
  print(x)
  lm = lm(december[,x]~count)
  print(coef(lm)[2])
  if(coef(lm)[2]>=0){
    positiveCount = positiveCount + 1
  }
}
print(paste("Positive: ",positiveCount))
print(positiveCount/length(december.names))
print(paste("Negative: ",length(december.names)-positiveCount))
