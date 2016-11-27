############
### various utility scripts
############

### file names of all *.Rnw files in the cwd, except those that contain "FRE"
file_names <- Sys.glob("*.Rnw")[-grep("FRE", Sys.glob("*.Rnw"))]



### script for rendering *.Rnw files into *.pdf files.
# loop over all the *.Rnw files in the cwd, and render them into *.pdf files.
sapply(file_names, knitr::knit2pdf)



### script for extracting R chunks from all *.Rnw files, except those that contain "FRE".
sapply(file_names, knitr::purl, documentation=0)



### script for rendering all the *.Rmd files in the cwd into *.md and *.html files.
# loop over all the *.Rmd files in the cwd, and render them into *.md and *.html files.
sapply(Sys.glob("*.Rmd"), 
       function(x) rmarkdown::render(input=file.path(getwd(), x), clean=FALSE)
)  # end sapply



### script for creating table of statistics of student scores
score_s <- read.table("clipboard", header=TRUE, stringsAsFactors=FALSE)
score_stats <- sapply(score_s, 
                      function(x) 
                        c(max=max(x), min=min(x), 
                          mean=mean(x), sd=sd(x), 
                          median=median(x), mad=mad(x)))
score_stats <- t(score_stats)
round(score_stats, 1)

write.table(x=foo, file="clipboard", sep="\t")
