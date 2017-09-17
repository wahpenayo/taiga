# John Alan McDonald 2015-12-10
#-------------------------------------------------------------------------------
setwd('e:/workplace/tu-projects/taiga')
source('src/scripts/r/functions.r')
#-------------------------------------------------------------------------------
namespace <- 'probability'
ndata <- 32768
nterms <- 128
mincount <- 1
mtry <- 3
#-------------------------------------------------------------------------------
by.nterms <- function (package) {
 fname <- paste('by-nterms',namespace,ndata,nterms,mincount,mtry,sep='-');
 folder <- file.path('tst','taiga','test',package);
 inf <- file.path(folder,paste(fname,'tsv','gz',sep='.'));
 data <- read.delim(inf);
 dev.on(file.path(folder,paste('mad',fname,sep='-')),
   aspect=(900/1800),width=1800);
 p <- ggplot(data,aes(x=nterms,y=mad,colour=trainTest)) + geom_line() + 
   ggtitle(paste('MAD:',fname));
 print(p);
 dev.off(); 
 dev.on(file.path(folder,paste('trueNegatives',fname,sep='-')),
   aspect=(900/1800),width=1800);
 p <- ggplot(data,aes(x=nterms,y=trueNegatives,colour=trainTest)) + geom_line() + 
   ggtitle(paste('trueNegatives:',fname));
 print(p);
 dev.off(); 
 dev.on(file.path(folder,paste('falseNegatives',fname,sep='-')),
   aspect=(900/1800),width=1800);
 p <- ggplot(data,aes(x=nterms,y=falseNegatives,colour=trainTest)) + geom_line() + 
   ggtitle(paste('falseNegatives:',fname));
 print(p);
 dev.off(); 
 dev.on(file.path(folder,paste('falsePositives',fname,sep='-')),
   aspect=(900/1800),width=1800);
 p <- ggplot(data,aes(x=nterms,y=falsePositives,colour=trainTest)) + geom_line() + 
   ggtitle(paste('falsePositives:',fname));
 print(p);
 dev.off(); 
 dev.on(file.path(folder,paste('truePositives',fname,sep='-')),
   aspect=(900/1800),width=1800);
 p <- ggplot(data,aes(x=nterms,y=truePositives,colour=trainTest)) + geom_line() + 
   ggtitle(paste('truePositives:',fname));
 print(p);
 dev.off(); 
 train <- data[data$trainTest=='train',]
 dev.on(file.path(folder,paste('train-confusion',fname,sep='-')),
   aspect=(900/1800),width=1800);
 p <- ggplot(train,aes(x=nterms)) + 
   geom_line(aes(y=trueNegatives,colour="trueNegatives")) + 
   geom_line(aes(y=falseNegatives,colour="falseNegatives")) + 
   geom_line(aes(y=falsePositives,colour="falsePositives")) + 
   geom_line(aes(y=truePositives,colour="truePositives")) + 
   ggtitle(paste('train confusion:',fname));
 print(p);
 dev.off(); 
 dev.on(file.path(folder,paste('test-confusion',fname,sep='-')),
   aspect=(900/1800),width=1800);
 test <- data[data$trainTest=='test',]
 p <- ggplot(test,aes(x=nterms)) + 
   geom_line(aes(y=trueNegatives,colour="trueNegatives")) + 
   geom_line(aes(y=falseNegatives,colour="falseNegatives")) + 
   geom_line(aes(y=falsePositives,colour="falsePositives")) + 
   geom_line(aes(y=truePositives,colour="truePositives")) + 
   ggtitle(paste('test confusion:',fname));
 print(p);
 dev.off(); 
}
#-------------------------------------------------------------------------------
by.nterms('step')
by.nterms('pyramid')
#-------------------------------------------------------------------------------
