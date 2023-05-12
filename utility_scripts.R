########################
### Various utility scripts
########################

###############
# Get all file names with *.Rnw in the lecture_slides directory
filen <- Sys.glob("/Users/jerzy/Develop/lecture_slides/*.Rnw")
# Get all *.Rnw files in the lecture_slides directory, except those that contain "FRE"
filen <- filen[-grep("FRE", filen)]



###############
# Render *.Rnw files into *.pdf files.
# Render single file
knitr::knit2pdf("/Users/jerzy/Develop/lecture_slides/data_management.Rnw", bib_engine="biber")

# Loop over all the *.Rnw files in the cwd, and render them into *.pdf files.
setwd("/Users/jerzy/Develop/lecture_slides")
# Render files without using error handler.
sapply(filen, knitr::knit2pdf, bib_engine="biber")

## Render files using error handler.
# Create a Boolean vector of names already processed.
nameproc <- NULL
isproc <- filen %in% nameproc
# Loop over the filen and render them into *.pdf files.
sapply(filen[!isproc], function(file_name) {
  tryCatch(  # With error handler
    {
      cat("Processing: ", file_name, "\n")
      knitr::knit2pdf(file_name, bib_engine="biber", quiet=TRUE)
      nameproc <<- c(nameproc, file_name)
    },
    # Error handler captures error condition
    error=function(error_cond) {
      # Reference to file_name produces error and tryCatch() fails
      # cat("Error in: ", file_name, "\n")
      print(paste("error handler: ", error_cond))
    }  # end error handler
    # finally=print(paste("file_name=", file_name))
  )  # end tryCatch
})  # end sapply
# See files that have not been processed
isproc <- filen %in% nameproc
filen[!isproc]


###############
# Extract R chunks from single *.Rnw file
knitr::purl("/Users/jerzy/Develop/lecture_slides/FRE6871_Lecture_1.Rnw", documentation=0, quiet=TRUE)

# Extract R chunks from all *.Rnw files, except those that contain "FRE".
sapply(filen, knitr::purl, documentation=0, quiet=TRUE)



###############
# Render all the *.Rmd files in the cwd into *.md and *.html files.
# Loop over all the *.Rmd files in the cwd, and render them into *.md and *.html files.
sapply(Sys.glob("*.Rmd"), 
       function(x) rmarkdown::render(input=file.path(getwd(), x), clean=FALSE)
)  # end sapply



###############
# Read the RData file sp500.RData, and write all the time series into 
# separate CSV files using function data.table::fwrite()

dirn <- "/Users/jerzy/Develop/lecture_slides/data/SP500/"
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

dirn <- "/Users/jerzy/Develop/data/"
# Using lapply() and zoo::write.zoo()
filen <- lapply(ls(etfenv), function(symbol) {
  xtes <- get(symbol, envir=etfenv)
  zoo::write.zoo(xtes, file=paste0(dirn, symbol, ".csv"))
  symbol
})  # end lapply
unlist(filen)

# Or using lapply() and data.table::fwrite()
filen <- lapply(ls(sp500env), function(symbol) {
  xtes <- get(symbol, envir=sp500env)
  data.table::fwrite(data.table::as.data.table(xtes), file=paste0(dirn, symbol, ".csv"))
  symbol
})  # end eapply
names(filen)

# Or using eapply() and data.table::fwrite()
filen <- eapply(sp500env, function(xtes) {
  file_name <- rutils::get_name(colnames(xtes)[1])
  data.table::fwrite(data.table::as.data.table(xtes), file=paste0(dirn, file_name, ".csv"))
  file_name
})  # end eapply
names(filen)

# Or
filen <- lapply(as.list(sp500env), function(xtes) {
  file_name <- rutils::get_name(colnames(xtes)[1])
  data.table::fwrite(data.table::as.data.table(xtes), file=paste0(dirn, file_name, ".csv"))
  file_name
})  # end lapply
names(filen)



###############
# Compare contents of all the files in two different directories, assuming same file names

dir1 <- "/Users/jerzy/Develop/R/statarb/results"
dir2 <- "/Users/jerzy/Users/Jerzy/Downloads/results"
# compare all files in two directories assuming file names are the same
sapply(dir(dir1), function(filev) {
  all.equal(
    readLines(file.path(dir1, filev)), 
    readLines(file.path(dir2, filev)))
})  # end sapply


## Compare contents of all files in two directories, assuming different file names
# extract contents of all files in first directory
foo <- lapply(dir(dir1), function(filev) {
  scan(file=file.path(dir1, filev), what=character())
})  # end sapply
# extract contents of all files in second directory
bar <- lapply(dir(dir2), function(filev) {
  scan(file=file.path(dir2, filev), what=character())
})  # end sapply
sapply(seq_along(foo), function(x) {
  all.equal(foo[x], bar[x])
})  # end sapply



###############
# Extract futures symbols from file names

filen <- Sys.glob("/Users/jerzy/Develop/data_def/hull_data/dec2017/raw/*")

namesv <- sapply(filen, function(x) {
  foo <- strsplit(x, split='/')
  foo <- strsplit(xts::last(foo[[1]]), split='_')
  foo[[1]][1]
})  # end sapply
namesv <- unname(namesv)
namesv <- unique(namesv)



###############
# Read Excel spreadsheets

# Install and load package readxl
install.packages("readxl")
library(readxl)

# Read names of all the sheets from the Excel spreadsheet
filev <- "/Users/jerzy/Develop/R/capstone/Xuewan_Zhao/SP500 5Y Fundamental data.xlsx"
namesv <- readxl::excel_sheets(filev)

# Read all the sheets from the Excel spreadsheet - takes very long time
sheets <- lapply(namesv, readxl::read_xlsx, path=filev)
# Remove first sheet - it's empty
sheets <- sheets[-1]
namesv <- namesv[-1]
# Rename sheets to their stock tickers
namesv <- sapply(namesv, function(x) strsplit(x[1], " ")[[1]][1])
names(sheets) <- namesv

# The sheets are a list of tibbles (data frames)
class(sheets)
tibblev <- sheets[[1]]
class(tibblev)
# Some tibble columns are character strings, not numeric
class(tibblev$'BEst P/E Ratio')

# Coerce tibble to matrix
# Function to coerce tibble columns from character strings to numeric
to_matrix <- function(tibblev) {
  # Coerce columns from strings to numeric
  listv <- lapply(tibblev, as.numeric)
  # Flatten list into matrix
  do.call(cbind, listv)
}  # end to_matrix
matrixv <- to_matrix(tibblev)
colnames(matrixv)
# Calculate number of rows in matrixv
NROW(matrixv)
# Calculate number of NA values in column "P/E Ratio"
sum(is.na(matrixv[, "BEst P/E Ratio"]))

# Calculate number of NA values in column "P/E Ratio"
sum(is.na(as.numeric(tibblev$'BEst P/E Ratio')))
# Function to calculate number of NA values in column P/E Ratio
num_na <- function(tibblev) {
  sum(is.na(as.numeric(tibblev$'BEst P/E Ratio')))
}  # end num_na
num_na(tibblev)

# Calculate number of NA values in column "P/E Ratio" in all the sheets tibbles
num_na_s <- sapply(sheets, num_na)
# Or simply
num_na_s <- sapply(sheets, function(tibblev) 
  sum(is.na(as.numeric(tibblev$'BEst P/E Ratio'))))
median(num_na_s)

# Plot histogram of NA values in column "P/E Ratio" in all the sheets elements
x11()
hist(num_na_s)



###############
# Read binary data files

# Create a connection object to read the file in binary mode using "rb".
filez <- file.info("/Users/jerzy/Users/Jerzy/Downloads/ESH7.bin")$size
connp <- file("/Users/jerzy/Users/Jerzy/Downloads/ESH7.bin", open="rb")

# Reset position of pointer
seek(connp, where=(filez-12), origin="start")
seek(connp, where=0, origin="start")

# First read the column names. n = 3 as we have 3 columns.
# column.names <- readBin(connp, character(), n = 3)

# Read the n, k, and version integer values
datav <- readBin(connp, what="integer", n=3)

datav <- readBin(connp, what="double", n=4)

# seek() gives 
off_set <- seek(connp, origin="end")
seek(connp, where=12, origin="start")

datav <- readBin(connp, what="raw", n=4)
datav <- readBin(connp, what="double", n=4)
datav <- readBin(connp, what="double", n=4, size=4)
datav <- readBin(connp, what="double", n=4, size=4, endian="big")
datav <- readBin(connp, what="numeric", n=4)
datav <- readBin(connp, what="numeric", n=4, size=4)
datav <- readBin(connp, what="numeric", n=4, size=4, endian="big")

close(connp)


# Read compressed files directly

connp <- gzfile("/Users/jerzy/Users/Jerzy/Downloads/ESH820171213.bin.gz", open="rb")
connp <- gzfile("/Users/jerzy/Develop/data/hull_data/20160304/ESH7.bin.gz", open="rb")

colnamev <- c("type", "actn", "posn", "cond", "Px", "Sz", "posixt",
               "pB1r", "sB1r", "pA1r", "sA1r", "pB1c", "sB1c", "pA1c",
               "sA1c")

# read header with format info:  941642 x 15, format 1
headr <- readBin(connp, 'integer', 3)
datav <- readBin(connp, 'double', headr[1]*headr[2])
datav <- matrix(datav, nrow=headr[1], ncol=headr[2], 
                byrow=TRUE, dimnames=list(NULL, colnamev))

close(connp)


datav <- readBin(connp, what=integer(), n=3)
datav <- readBin(connp, what=double(), n=4)

foo <- seek(connp, origin="end")

close(connp)



###############
# install package H2O

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turnbull/2/R")))
library(h2o)
localH2O = h2o.init(nthreads=-1)

# Finally, let's run a demo to see H2O at work.
demo(h2o.kmeans)

