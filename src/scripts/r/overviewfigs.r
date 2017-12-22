# wahpenayo at gmail dot com
# 2017-12-21
#-----------------------------------------------------------------
if (file.exists('e:/porta/projects/taiga')) {
  setwd('e:/porta/projects/taiga/docs/overview')
} else {
  setwd('c:/porta/projects/taiga/docs/overview')
}
#-----------------------------------------------------------------
source('../../src/scripts/r/functions.r')
#-----------------------------------------------------------------
d <- make.data('reg')

illustrate.rpart(name='1-split',d=d,maxdepth=1)
illustrate.rpart(name='3-split',d=d,maxdepth=2)

illustrate.rpart(name='unpruned',d=d,cp=0.00001)

illustrate.rpart(name='pruned',d=d,cp=0.0005)

illustrate.rf(name='1-tree',d=d,ntree=1)
illustrate.rf(name='4-tree',d=d,ntree=4)
illustrate.rf(name='1024-tree',d=d,ntree=1024)
#-----------------------------------------------------------------
