# wahpenayo at gmail dot com
# 2017-12-19
#-----------------------------------------------------------------
if (file.exists('e:/porta/projects/taiga')) {
  setwd('e:/porta/projects/taiga')
} else {
  setwd('c:/porta/projects/taiga')
}
#-----------------------------------------------------------------
source('src/scripts/r/functions.r')
#-----------------------------------------------------------------
d <- make.data('reg')
illustrate.rpart('reg',d)
illustrate.rf('reg',d)
#-----------------------------------------------------------------
