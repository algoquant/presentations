rm(list=ls())

# coerce mts object into zoo
zoo.eustx <- as.zoo(EuStockMarkets)
# coerce index into class 'Dates'
index(zoo.eustx) <- as.Date(365*(index(zoo.eustx)-1970))

# ggplot2 in multiple panes
autoplot(zoo.eustx, facets=Series ~ .)

library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)

# ggplot2 in single pane
ggp.zoo1 <- autoplot(zoo.eustx, main="Eu Stox", 
                     facets=NULL) + xlab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.background=element_blank(),
        plot.background=element_rect(colour='purple', fill='pink', size=3, linetype='dashed')
  )

# ggplot2 in multiple panes
ggp.zoo2 <- autoplot(zoo.eustx, main="Eu Stox", 
                     facets=Series ~ .) + xlab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.background=element_blank(),
        plot.background=element_rect(colour='red', fill='blue', size=3, linetype='dashed')
  )



n.globvar <- 11  # define a global variable
ls(environment())  # get all variables in environment
MyFunc <- function() {  # function for exploring environments
  n.locvar <- 1  # define a local variable
  cat('objects in parent environment:\t', 
      ls(parent.env(environment())), '\n')
  cat('objects in function environment:\t', 
      ls(environment()), '\n')
  cat('n.locvar:\t', n.locvar, 'n.globvar:\t', n.globvar, '\n')
  cat('this is the parent environment:')
  parent.env(environment())  # return parent environment
}  # end MyFunc
MyFunc()
environment(MyFunc)


FuncPower <- function(n.exp) {  # wrapper function
  # a power function factory
  function(n.arg) {  # anonymous closure
    cat(ls(environment()), '\n')
    var <- n.arg^n.exp
    cat(ls(environment()), '\n')
    var
  }
}  # end FuncPower

f.square <- FuncPower(2)

f.square(2)

funcTestFunc <- function(inputFunc, ...) {
  inputFunc(...)
}


