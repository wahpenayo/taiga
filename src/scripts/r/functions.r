# John Alan McDonald
# 2015-12-11
#-----------------------------------------------------------------
# Load the necessary add-on packages, downloading and installing (in the user's
# R_LIBS_USER folder) if necessary.  
#remove.packages(c("ggplot2", "data.table"))
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)
load.packages <- function () {
 user.libs <- Sys.getenv('R_LIBS_USER')
 dir.create(user.libs,showWarnings=FALSE,recursive=TRUE)
 repos <- c('http://cran.fhcrc.org')
 packages <- 
   c('foreign', 
     'classInt',
     'getopt', 
     'RColorBrewer',
     'data.table',
     'reshape',
     'grid', 
     'hexbin',
     'lattice',
     'latticeExtra',
     'plyr',
     'stringr',
     'reshape2',
     'ggplot2',
     'rgeos',
     'sp',
     'rgdal',
     'maptools',
     'randomForest',
     'quantregForest',
     'rpart',
     #'randomSurvivalForest',
     'mlbench',
     'datasets')
 for (package in packages) {
  found <- eval(call('require',package,quietly=TRUE))
  if (! found) { 
   install.packages(c(package),user.libs,repos=repos,
     dependencies=TRUE)
   eval(call('library',package)) } } }
#-----------------------------------------------------------------
load.packages()
#-----------------------------------------------------------------
my.theme <- function () {
 n <- 7
 text.color <- 'black'
 dark <- brewer.pal(n,'Dark2')
 pastel <- brewer.pal(n,'Pastel2')
 list(
   background = list(col="transparent"),
   #fontsize='16',
   axis.line=list(col='gray'),
   axis.text=list(col=text.color,cex=1.5),
   box.rectangle = list(col="darkgreen"),
   box.umbrella = list(col="darkgreen"),
   dot.line = list(col="#e8e8e8"),
   dot.symbol = list(col="darkgreen"),
   par.xlab.text=list(col=text.color,cex=1.5),
   par.xlab.text=list(col=text.color,cex=1.5),
   par.ylab.text=list(col=text.color,cex=1.5),
   par.main.text=list(col=text.color,cex=2),
   par.sub.text=list(col=text.color,cex=1.5),
   plot.line = list(col="darkgreen"),
   plot.symbol = list(col="darkgreen"),
   plot.polygon = list(col="darkgreen"),
   reference.line = list(col="#e8e8e8"),
   regions = list(
     col=colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(100)),
   shade.colors = list(
     palette=colorRampPalette(rev(brewer.pal(11,'RdYlGn')))),
   strip.border = list(col='gray'),
   strip.shingle = list(col=dark),
   strip.background = list(col='gray'),
   superpose.line = list(col = dark,lty = 1:n,lwd=1),
   superpose.polygon = 
     list(col=dark,border=rep('#DDDDDD44',n),alpha=rep(0.3,n)),
   superpose.symbol = 
     list(pch=c(1,3,6,0,5,16,17),cex=rep(1, n),col=dark,
       fontface='bold')) }
lattice.options(default.theme = my.theme)
lattice.options(lattice.theme = my.theme)
#-----------------------------------------------------------------
theme_set(theme_bw())
#-----------------------------------------------------------------
# Open a png graphics device.
# aspect ratio is height/width

dev.on <- function (
  filename=NULL,
  aspect=(1050/1400),
  width=16,
  theme=my.theme) {
 
  stopifnot(!is.null(filename))
  
 # make sure the folder is there
 dir.create(
   path=dirname(filename),
   showWarnings=FALSE,
   recursive=TRUE)
 
 # often the graphics device is stuck from the last failed run
 options(show.error.messages = FALSE)
 options(warn = -1)
 try( dev.off() )
 options(warn = 0)
 options(show.error.messages = TRUE)
 
 w <- width
 h <- aspect*w
 
 # The png device doesn't work under my R installation on linux.
 # The bitmap device doesn't work on my windows laptop.
 # At least they both produce png files.
 plotF <- paste(filename,'png',sep='.')
 print(plotF)
 print(paste('size:',w,'x',h))
 if ('windows'==.Platform$OS.type) {
  trellis.device(
    device='png',
    theme=theme,
    filename=plotF,
    width=w,
    height=h,
    pointsize=10,
    units='cm',
    res=96) }
 else { 
  trellis.device(
    device='bitmap',
    theme=theme,
    file=plotF,
    width=w,
    height=h,
    pointsize=10,
    units='cm',
    res=96) } }
#-----------------------------------------------------------------
write.tsv <- function (data, file) {
 write.table(
   x=data, 
   quote=FALSE, 
   sep='\t', 
   row.names=FALSE, 
   file=file) }
#-----------------------------------------------------------------
write.ssv <- function (data, file) {
 write.table(
   x=data, 
   quote=FALSE, 
   sep=' ', 
   row.names=FALSE, 
   file=file) }
#-----------------------------------------------------------------
# rf dummy example
#-----------------------------------------------------------------
parabola <- function (x0,x1) { 
  return( ((x0+x1)^2)/2 + (x0-x1)^2 ); }
#-----------------------------------------------------------------
parabola.data <- function (n) {
 x0 <- runif(n,-1,1);
 x1 <- runif(n,-1,1);
 y <- parabola(x0,x1);
 return( data.frame(x0=x0,x1=x1,y=y) ); }
#-----------------------------------------------------------------
add.noise <- function (frame,sigma) {
 frame$y <- frame$y + rnorm(length(frame$y),mean=0,sd=sigma); 
 return( frame ); }
#-----------------------------------------------------------------
make.data <- function (name,width=16,aspect=0.9) {
 
 steps <- seq(-1,1,by=0.05)
 test <- expand.grid(x0=steps,x1=steps)
 test$y <- parabola(test$x0,test$x1)
 sigma <- 0.5
 train <- add.noise(parabola.data(10000),sigma)
 exp <- expression(y == {(x[0]+x[1])^2}/2 + (x[0]-x[1])^2)
 ncuts <- 16
 palette <- colorRampPalette(rev(brewer.pal(11,'RdYlGn')))
 
 dev.on(paste('fig/',name,'-true-levelplot',sep=''),
   width=width,
   aspect=aspect)
 print(
   levelplot(y ~ x0 + x1, 
     test, 
     cex=0.5, 
     main=exp, 
     col.regions=palette))
 dev.off()      
 
 dev.on(paste('fig/',name,'-true-wireframe',sep=''),
   width=width,
   aspect=aspect)
 print(
   wireframe(y ~ x0 + x1, 
     test,
     #cex=0.5, 
     main=exp, 
     col='#AAAAAA44',
     drape=TRUE, 
     cuts=ncuts, 
     col.regions=palette(ncuts+1)))
 dev.off()      
 
 dev.on('fig/rpart-training-data',
   width=width,aspect=aspect)
 print(
   cloud(y ~ x0 + x1, train, 
     #cex=0.5, 
     col='blue', 
     pch='.',
     main=expression(y[i] + sigma*epsilon[i])))
 dev.off()      
 
 list(
   test=test,
   train=train,
   exp=exp,
   palette=palette,
   ncuts=ncuts) }

#-----------------------------------------------------------------

illustrate.rpart <- function (
  name,
  d,
  maxdepth=30,
  cp=0.0001,
  minsplit=8,
  width=16,
  aspect=0.9) {
 
 tree <- rpart(
   y ~ x0 + x1, 
   d$train, 
   maxdepth=maxdepth, 
   minsplit=minsplit, 
   method='anova', 
   cp=0.0001, 
   maxcompete=0,
   maxsurrogate=0)
 
 if ((cp <= 0.0005) & (nrow(tree$frame) > 32)) {
  treep <- prune(tree,cp=0.0002)
  dev.on(
    filename=paste('fig/rpart',name,'plotcp',sep='-'),
    width=width,
    aspect=0.75*aspect)
  plotcp(treep,pch='.',main=NULL)
  dev.off() }
 
 tree <- prune(tree,cp=cp)
 
 dev.on(
   filename=paste('fig/rpart',name,'tree',sep='-'),
   width=0.75*width,
   aspect=aspect)
 plot(
   x=tree, 
   uniform=TRUE, 
   #main=paste(name,'CART tree'), 
   branch=0.1)
 if (nrow(tree$frame) < 64) { 
   text(tree, use.n=FALSE, fancy=FALSE) }
# post(tree0,filename='')
 dev.off()
 
 test0 <- d$test
 test0$yhat <- predict(tree,newdata=test0)
 
 dev.on(
   filename=paste('fig/rpart',name,'levelplot',sep='-'),
   width=width,
   aspect=aspect)
 print(
   levelplot(
     x=yhat ~ x0 + x1, 
     data=test0, 
     cex=0.5, 
     #main=paste(name,'fit'),
     col.regions=d$palette))
 dev.off()      
 
 dev.on(
   filename=paste('fig/rpart',name,'wireframe',sep='-'),
   width=width,
   aspect=aspect)
 print(
   wireframe(
     x=yhat ~ x0 + x1, 
     data=test0, 
     cex=0.5, 
     #main=paste(name,'fit'), 
     col='#AAAAAA44', 
     drape=TRUE, 
     cuts=d$ncuts, 
     col.regions=d$palette(d$ncuts+1)))
 dev.off()      
 
 NULL }

#-----------------------------------------------------------------

illustrate.rf <- function (
  name,
  d,
  ntree=128,
  minsplit=8,
  width=16,
  aspect=0.9) {
 
  stopifnot(
    !is.null(name),
    !is.null(d))
  
 forest <- 
   randomForest(y ~ x0 + x1, 
     data=d$train, 
     ytest=d$test$y, 
     xtest=d$test[ , c('x0','x1') ],
     ntree=ntree, 
     nodesize=minsplit,
     keep.forest=TRUE) 
 
 if (ntree > 32) {
  dev.on(
    filename=paste('fig/rf',name,'forest',sep='-'),
    width=width,
    aspect=0.75) 
  plot(x=forest,main=NULL) 
  dev.off() }
 
 test0 <- d$test
 test0$yhat <- predict(forest,newdata=test0)
 
 dev.on(
   filename=paste('fig/rf',name,'levelplot',sep='-'),
   width=width,
   aspect=aspect)
 print(
   levelplot(
     x=yhat ~ x0 + x1, 
     data=test0, 
     cex=0.5, 
     #main=paste(name,'fit'),
     col.regions=d$palette))
 dev.off()      
 
 dev.on(
   filename=paste('fig/rf',name,'wireframe',sep='-'),
   width=width,
   aspect=aspect)
 print(
   wireframe(
     x=yhat ~ x0 + x1, 
     data=test0, 
     cex=0.5, 
     #main=paste(name,'fit'), 
     col='#AAAAAA44', 
     drape=TRUE, 
     cuts=d$ncuts, 
     col.regions=d$palette(d$ncuts+1)))
 dev.off()      
 
 NULL }

#-----------------------------------------------------------------
# synthetic data tests
#-----------------------------------------------------------------
synthetic.prediction.data <- function (package=NULL,
  namespace=NULL,
  ndata=32768,
  nterms=128,
  mincount=127,
  mtry=3) {
 folder <- file.path('tst','taiga','test',package);
 fname <- paste('predictions','train',namespace,ndata,nterms,mincount,mtry,sep='-');
 inf <- file.path(folder,paste(fname,'tsv','gz',sep='.'));
 if (file.exists(inf)) { 
  train <- read.delim(inf);
  train$trainTest <- 'train'
  fname <- paste('predictions','test',namespace,ndata,nterms,mincount,mtry,sep='-');
  inf <- file.path(folder,paste(fname,'tsv','gz',sep='.'));
  test <- read.delim(inf);
  test$trainTest <- 'test'
  data <- rbind(train,test)
  data$true.class <- factor(data$true.class,levels=c(0.0,1.0),labels=c('true class 0','true class 1'),ordered=TRUE)
  data$trainTest <- factor(data$trainTest,levels=c('train','test'),ordered=TRUE)
  data$residual.probability <- data$true.probability - data$predicted.probability
  return(data); 
 } else {
  return (NULL); 
 } }
#-----------------------------------------------------------------
synthetic.prediction.plots <- function (package=NULL,
  namespace=NULL,
  data=NULL,
  prefix=NULL,
  ndata=32768,
  nterms=128,
  mincount=127,
  mtry=3) {
 folder <- file.path('tst','taiga','test',package);
 pname <- paste(prefix,namespace,ndata,nterms,mincount,mtry,sep='-');
# dev.on(file.path(folder,paste('pred-true',pname,sep='-')),aspect=0.5,width=1600);
# p <- ggplot(data,aes(x=predicted.probability,y=true.probability,
#       colour=trainTest,group=trainTest)) +
#   scale_colour_discrete(drop=FALSE) + 
#   facet_grid(. ~ true.class) +
#   geom_point(alpha=0.2,size=4) + 
#   xlim(0,1) + ylim(0,1) +
#   geom_abline(intercept=0,slope=1,color='black',alpha=0.5) + 
#   ggtitle(pname);
# print(p);
# dev.off(); 
 dev.on(file.path(folder,paste('true-pred',pname,sep='-')),aspect=0.5,width=1600);
 lim <- range(data$true.probability,data$predicted.probability)
 p <- ggplot(data,aes(x=true.probability,y=predicted.probability,
       colour=trainTest,group=trainTest)) +
   scale_colour_discrete(drop=FALSE) + 
   facet_grid(. ~ true.class) +
   geom_point(alpha=0.2,size=4) + 
   coord_fixed(xlim=lim,ylim=lim) + 
   geom_abline(intercept=0,slope=1,color='black',alpha=0.5) + 
   ggtitle(pname);
 print(p);
 dev.off(); 
# dev.on(file.path(folder,paste('pred-residuals',pname,sep='-')),aspect=0.5,width=1600);
# p <- ggplot(data,aes(x=predicted.probability,y=residual.probability,
#       colour=trainTest,group=trainTest)) + 
#   scale_colour_discrete(drop=FALSE) + 
#   facet_grid(. ~ true.class) +
#   geom_point(alpha=0.2,size=4) + 
#   xlim(0,1) + ylim(-1,1) +
#   geom_abline(intercept=0,slope=0,color='black',alpha=0.5) + 
#   ggtitle(pname);
# print(p);
# dev.off(); 
# dev.on(file.path(folder,paste('true-residuals',pname,sep='-')),aspect=0.5,width=1600);
# p <- ggplot(data,aes(x=true.probability,y=residual.probability,
#       colour=trainTest,group=trainTest)) + 
#   scale_colour_discrete(drop=FALSE) + 
#   facet_grid(. ~ true.class) +
#   geom_point(alpha=0.2,size=4) + 
#   xlim(0,1) + ylim(-1,1) +
#   geom_abline(intercept=0,slope=0,color='black',alpha=0.5) + 
#   ggtitle(pname);
# print(p);
# dev.off(); 
}
#-----------------------------------------------------------------

