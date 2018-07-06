########################
### Various utility scripts
########################

###############
### Get all file names with *.Rnw in the lecture_slides directory
file_names <- Sys.glob("C:/Develop/R/lecture_slides/*.Rnw")
# Get all *.Rnw files in the lecture_slides directory, except those that contain "FRE"
file_names <- file_names[-grep("FRE", file_names)]



###############
### Render *.Rnw files into *.pdf files.
# loop over all the *.Rnw files in the cwd, and render them into *.pdf files.
setwd("C:/Develop/R/lecture_slides")
sapply(file_names, knitr::knit2pdf)



###############
### Extract R chunks from all *.Rnw files, except those that contain "FRE".
sapply(file_names, knitr::purl, documentation=0)



###############
### Render all the *.Rmd files in the cwd into *.md and *.html files.
# loop over all the *.Rmd files in the cwd, and render them into *.md and *.html files.
sapply(Sys.glob("*.Rmd"), 
       function(x) rmarkdown::render(input=file.path(getwd(), x), clean=FALSE)
)  # end sapply



###############
### Compare contents of all the files in two different directories, assuming same file names

dir_1 <- "C:/Develop/R/statarb/results"
dir_2 <- "C:/Users/Jerzy/Downloads/results"
# compare all files in two directories assuming file names are the same
sapply(dir(dir_1), function(fil_e) {
  all.equal(
    readLines(file.path(dir_1, fil_e)), 
    readLines(file.path(dir_2, fil_e)))
})  # end sapply


## Compare contents of all files in two directories, assuming different file names
# extract contents of all files in first directory
foo <- lapply(dir(dir_1), function(fil_e) {
  scan(file=file.path(dir_1, fil_e), what=character())
})  # end sapply
# extract contents of all files in second directory
bar <- lapply(dir(dir_2), function(fil_e) {
  scan(file=file.path(dir_2, fil_e), what=character())
})  # end sapply
sapply(seq_along(foo), function(x) {
  all.equal(foo[x], bar[x])
})  # end sapply



###############
### Extract futures symbols from file names

file_names <- Sys.glob("C:/Develop/data_def/hull_data/dec_2017/raw/*")

name_s <- sapply(file_names, function(x) {
  foo <- strsplit(x, split='/')
  foo <- strsplit(xts::last(foo[[1]]), split='_')
  foo[[1]][1]
})  # end sapply
name_s <- unname(name_s)
name_s <- unique(name_s)



###############
### Read binary data

# Create a connection object to read the file in binary mode using "rb".
si_ze <- file.info("C:/Users/Jerzy/Downloads/ESH7.bin")$size
connect_ion <- file("C:/Users/Jerzy/Downloads/ESH7.bin", open="rb")

# reset position of pointer
seek(connect_ion, where=(si_ze-12), origin="start")
seek(connect_ion, where=0, origin="start")

# First read the column names. n = 3 as we have 3 columns.
# column.names <- readBin(connect_ion, character(), n = 3)

# Read the n, k, and version integer values
da_ta <- readBin(connect_ion, what="integer", n=3)

da_ta <- readBin(connect_ion, what="double", n=4)

# seek() gives 
off_set <- seek(connect_ion, origin="end")
seek(connect_ion, where=12, origin="start")

da_ta <- readBin(connect_ion, what="raw", n=4)
da_ta <- readBin(connect_ion, what="double", n=4)
da_ta <- readBin(connect_ion, what="double", n=4, size=4)
da_ta <- readBin(connect_ion, what="double", n=4, size=4, endian="big")
da_ta <- readBin(connect_ion, what="numeric", n=4)
da_ta <- readBin(connect_ion, what="numeric", n=4, size=4)
da_ta <- readBin(connect_ion, what="numeric", n=4, size=4, endian="big")

close(connect_ion)


# Read compressed files directly

connect_ion <- gzfile("C:/Users/Jerzy/Downloads/ESH8_20171213.bin.gz", open="rb")
connect_ion <- gzfile("C:/Develop/data/hull_data/20160304/ESH7.bin.gz", open="rb")

col_names <- c("type", "actn", "posn", "cond", "Px", "Sz", "posixt",
               "pB1r", "sB1r", "pA1r", "sA1r", "pB1c", "sB1c", "pA1c",
               "sA1c")

# read header with format info:  941642 x 15, format 1
head_er <- readBin(connect_ion, 'integer', 3)
da_ta <- readBin(connect_ion, 'double', head_er[1]*head_er[2])
da_ta <- matrix(da_ta, nrow=head_er[1], ncol=head_er[2], 
                byrow=TRUE, dimnames=list(NULL, col_names))

close(connect_ion)


da_ta <- readBin(connect_ion, what=integer(), n=3)
da_ta <- readBin(connect_ion, what=double(), n=4)

foo <- seek(connect_ion, origin="end")

close(connect_ion)



###############
### install package H2O

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

