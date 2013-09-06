suppressMessages(library(knitr))
suppressMessages(library(markdown))
suppressMessages(library(getopt))
suppressMessages(library(xts))
suppressMessages(library(PerformanceAnalytics))
suppressMessages(library(ROneTick))
suppressMessages(library(foreach))
suppressMessages(library(quantmod))
suppressMessages(library(car))
suppressMessages(library(urca))
suppressMessages(library(compiler))
source("tsdPlotCoint.R")
enableJIT(3)


options(scipen=15,digits.secs=6)

spec = matrix(c(
  'output' , 'o', 1, "character"
), byrow=TRUE, ncol=4);
opt = getopt(spec);


outputDir <- '.'
if(!is.null(opt$output)){
  outputDir <<- opt$output
}

print(paste(outputDir,'\\all_index.html',sep=''))
print(paste(outputDir,'\\cor_index18.html',sep=''))
print(paste(outputDir,'\\cor_index.html',sep=''))

source('GraphBase.R') 
source('GraphData.R')

maxIndex <- length(resultCor[,1])

symbolNames <- resultCor[,1][1:50]
html <- paste(outputDir,'/cor_index_1.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- resultCor[,1][51:100]
html <- paste(outputDir,'/cor_index_2.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- resultCor[,1][101:150]
html <- paste(outputDir,'/cor_index_3.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- resultCor[,1][151:200]
html <- paste(outputDir,'/cor_index_4.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- resultCor[,1][201:250]
html <- paste(outputDir,'/cor_index_5.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- resultCor[,1][251:300]
html <- paste(outputDir,'/cor_index_6.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- resultCor[,1][301:maxIndex]
html <- paste(outputDir,'/cor_index_7.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

##### sorted by alphabet
maxIndex <- length(instruments)

symbolNames <- instruments[1:50]
html <- paste(outputDir,'/all_index_1.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- instruments[51:100]
html <- paste(outputDir,'/all_index_2.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- instruments[101:150]
html <- paste(outputDir,'/all_index_3.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- instruments[151:200]
html <- paste(outputDir,'/all_index_4.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- instruments[201:250]
html <- paste(outputDir,'/all_index_5.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- instruments[251:300]
html <- paste(outputDir,'/all_index_6.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

symbolNames <- instruments[301:maxIndex]
html <- paste(outputDir,'/all_index_7.html',sep='')
knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)

#html <- paste(outputDir,'/cor_index18.html',sep='')
#knit2html(input="CorOrderSimpleEquityGraph.Rmd", output=html)
# 
# html <- paste(outputDir,'/cor_index18_3.html',sep='')
# knit(input="CorOrderSimpleEquityGraph.Rmd",envir=new.env())
# 
# html <- paste(outputDir,'/cor_index18_4.html',sep='')
# knit(input="CorOrderSimpleEquityGraph.Rmd",envir=new.env())
# 
# html <- paste(outputDir,'/cor_index18_5.html',sep='')
# knit(input="CorOrderSimpleEquityGraph.Rmd",envir=new.env())




#knit2html("CorOrderSimpleEquityGraph.Rmd",output=html, envir=parent.frame())

#knit("CorOrderSimpleEquityGraph.Rmd",output="graph.md")
#html <- paste(outputDir,'/cor_index.html',sep='')
#knit2html("CorOrderSimpleEquityGraph.Rmd",output=html,envir=parent.frame())

#html <- paste(outputDir,'/all_index.html',sep='')
#knit(input="AlphaOrderSimpleEquityGraph.Rmd")
#knit2html(input="AlphaOrderSimpleEquityGraph.Rmd",output=html)


