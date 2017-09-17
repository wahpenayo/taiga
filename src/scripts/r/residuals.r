# John Alan McDonald 2015-12-11
#-------------------------------------------------------------------------------
setwd('e:/workplace/tu-projects/taiga')
source('src/scripts/r/functions.r')
#-------------------------------------------------------------------------------
for (n in (1024*c(32,64,128,256,512,1024))) {
 for (nt in c(128,256,512)) {
  for (pkg in c('spikes','pyramid')) {
  for (mc in c(15,31,63,127)) {
  for (ns in c(
    #'bias-weight-majority-vote-probability',
    #'bias-weight-positive-fraction-probability',
    #'bias-majority-vote-probability',
    #'bias-positive-fraction-probability'
    #,
    #'majority-vote-probability'
    # , 
    'positive-fraction-probability'
    , 'weight-majority-vote-probability'
    , 'weight-positive-fraction-probability'
      )) {
   predictions <- synthetic.prediction.data(
     package=pkg,namespace=ns,mincount=mc,ndata=n,nterms=nt)
   if (! is.null(predictions)) {
    synthetic.prediction.plots(package=pkg, namespace=ns, mincount=mc, ndata=n,
       nterms=nt, prefix='all', data=predictions)
# synthetic.prediction.plots(package=pkg, namespace=ns,mincount=mc,
#   prefix='train', data=predictions[predictions$trainTest=='train',])
# synthetic.prediction.plots(package=pkg,namespace=ns,mincount=mc,
#   prefix='test', data=predictions[predictions$trainTest=='test',]) 
   } } } } } }
#-------------------------------------------------------------------------------
