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
### Create table of statistics of student scores
score_s <- read.table("clipboard", header=TRUE, stringsAsFactors=FALSE)
score_stats <- sapply(score_s, 
                      function(x) 
                        c(max=max(x), min=min(x), 
                          mean=mean(x), sd=sd(x), 
                          median=median(x), mad=mad(x)))
score_stats <- t(score_stats)
round(score_stats, 1)

write.table(x=foo, file="clipboard", sep="\t")



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



