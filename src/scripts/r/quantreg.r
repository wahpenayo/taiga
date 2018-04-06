# wahpenayo at gmail dot com
# 2018-04-05
#-----------------------------------------------------------------
if (file.exists('e:/porta/projects/taiga')) {
  setwd('e:/porta/projects/taiga')
} else {
  setwd('c:/porta/projects/taiga')
}
#source('src/main/r/functions.r')
#-----------------------------------------------------------------
library(quantreg)
#example(rq)
data(stackloss)
data(engel)
