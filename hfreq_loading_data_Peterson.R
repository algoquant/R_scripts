#################################
### loading Brian Peterson data using package FinancialInstrument

### set data_dir directory
# data_dir <- "/home/storage/settles/"
# data_dir <- "/home/storage/sec/"
data_dir <- "E:/mktdata/sec/"
scrub_dir <- "E:/scrubdata/"
# data_dir <- "/home/storage/tick/"
# print(data_dir)


###########
# code for loading xts data

# load and save data for a single symbol
save_OHLC("IWF")
# load data for list of symbols
sapply(head(symbolv), save_OHLC)

# load data for a single symbol
load(file="SPY.RData")
chartSeries(SPY["2013"], name=symbol, theme=chartTheme("white"))


###########
# code for loading instruments data

# load list of symbols
symbolv <- read.csv(file="etf_list_hf.csv")
symbolv <- symbolv[[1]]

### load list of instrument definitions: creates .instrument environment
loadInstruments(file_name='E:/mktdata/instruments.rda')

# explore the .instrument environment
# list instrument names in the .instrument environment (character vector)
# ls_instruments()  # vary large list!!!
# ls(FinancialInstrument:::.instrument)
# ls_instruments() bigger than ls(FinancialInstrument:::.instrument)
list.instruments <- ls_instruments()
length(list.instruments)
sample(list.instruments, 11)
tail(list.instruments)
write.csv(list.instruments, file="instruments.txt")


# get tickers for all stocks
# ls_instruments_by('type', 'stock')  # very large list!!!
# find.instrument("stock")  # very large list!!!
# get tickers for all computer stocks
find.instrument("computer")
# get tickers for all bond instruments
find.instrument("bond")


# get contract specs for instrument "First Solar stock"
getInstrument("FSLR")
# explore a few instrument objects
an.instrument <- getInstrument("MSFT")
an.instrument <- getInstrument("BAXH3")
an.instrument$type
an.instrument$series_description



###########
# extra code for parsing list.instruments - you can ignore

convert_NULL_NA <- function(in.var) {
  if (is.null(in.var))
    ret.var <- NA
  else
    ret.var <- in.var
  ret.var
}  # end convert_NULL_NA

attr.instrument <- function(name.instrument) {
  my.instrument <- getInstrument(name.instrument)
  c(name=convert_NULL_NA(my.instrument$primary_id[1]), type=convert_NULL_NA(my.instrument$type[1]), longName=convert_NULL_NA(my.instrument$longName[1]), description=convert_NULL_NA(my.instrument$series_description[1]))
}  # end attr.instrument

# table.instruments <- apply(as.matrix(sample(list.instruments, 5)), 1, attr.instrument)
table.instruments <- aperm(sapply(sample(list.instruments, 11), attr.instrument), c(2,1))
table.instruments <- aperm(sapply(list.instruments, attr.instrument), c(2,1))
write.csv(table.instruments, file="table.instruments.txt")
unique(table.instruments[, "type"])
write.csv(unique(table.instruments[, "longName"]), file="unique.instruments.txt")

### end code for parsing list.instruments



###########
# load ts data using getSymbols.FI

### set defaults for getSymbols
# setDefaults(getSymbols, verbose=FALSE, dir=data_dir, src="rda")

# setDefaults for getSymbols: call getSymbols.FI by default
setDefaults(getSymbols, verbose=FALSE, src="FI")
# setDefaults for getSymbols.FI: load data from local drive
setDefaults(getSymbols.FI,
            extension="RData",
            dir=data_dir,
            days_to_omit="Saturday",
            use_identifier="X.RIC")


### load seconds bar data using getSymbols.FI

# run loadInstruments() first
symbol <- "SPY"
getSymbols(symbol)  # takes very long!!!
dim(SPY)
SPY[10000:10020, ]


### extra legacy code

# load second data for single day - works
load("E:/mktdata/sec/ESM9/2009.04.02.ESM9.rdata")
dim(ESM9)
len_es <- dim(ESM9)[1]
# inspect
ESM9[(len_es-10010):(len_es-10000), ]
# plot
plot(ESM9[((len_es-20000):(len_es-10000)), "Bid.Price"])


# tics data works
getSymbols("ZSK5")

# doesn't work
getSymbols("MSFT", verbose=FALSE, dir=data_dir, src="rda")
file  MSFT.rda  does not exist  in  E:/mktdata/ ....skipping
[1] "MSFT"

# doesn't work
getSymbols("ESH3")
# NULL
# Warning message:
#   In getSymbols.FI(Symbols="ESH3", env=<environment>, verbose=FALSE,  :
#                      No data found.

# doesn't work
getSymbols("ZQZ9")
# Error in FUN(1L[[1L]], ...) : 
#  must define instrument first to call with 'use_identifier'
#  In addition: Warning message:
#  In getInstrument(Symbols[[i]], silent=FALSE) :
#  instrument ZQZ9 not found, please create it first.
getSymbols("QZ9")

# doesn't work
getSymbols("ZSK7")
# NULL
# Warning message:
#   In getSymbols.FI(Symbols="ZSK7", env=<environment>, verbose=FALSE,  :
#                      No data found.

# analyzing getSymbols.FI()
getSymbols.FI <- function (symbols_list, date_from="2010-01-01", to=Sys.Date(), ..., 
                           dir="", return_class="xts", extension="rda", split_method=c("days", "common"), 
                           use_identifier=NA, date_format=NULL, verbose=TRUE, 
                           days_to_omit=c("Saturday", "Sunday"), indexTZ=NA) {
  
  # looks redundant
  if (is.null(date_format)) 
    date_format <- "%Y.%m.%d"
  # what's this?
  if (is.null(days_to_omit)) 
    days_to_omit <- "NULL"
  
  # copy named dots arguments into function environment - not sure why this is needed
  this_env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this_env)
  }
  
  # recursive "rbind" function for list arguments - same as do.call.rbind
  do_call_rbind <- function(list_var) {
    # call lapply in a loop to divide list_var by half, binding neighboring elements
    while (length(list_var) > 1) {
      # index of odd list elements
      odd_index <- seq(from=1, to=length(list_var), by=2)
      # bind neighboring elements and divide list_var by half
      list_var <- lapply(odd_index, function(indeks) {
        if (indeks==length(list_var)) {
          return(list_var[[indeks]])
        }
        return(rbind(list_var[[indeks]], list_var[[indeks+1]]))
      })  # end lapply
    }  # end while
    # list_var has only one element - return it
    list_var[[1]]
  }  # end do_call_rbind
  
  # assign input argument values to hidden '.*' variables
  # the variables hasArg.* are used in pickArg()
  if (hasArg.date_from <- hasArg(date_from)) 
    .date_from <- date_from
  if (hasArg.to <- hasArg(to)) 
    .to <- to
  if (hasArg.dir <- hasArg(dir)) 
    .dir <- dir
  if (hasArg.return_class <- hasArg(return_class)) 
    .return_class <- return_class
  if (hasArg.extension <- hasArg(extension)) 
    .extension <- extension
  if (hasArg.split_method <- hasArg(split_method)) 
    .split_method <- split_method
  if (hasArg.use_identifier <- hasArg(use_identifier)) 
    .use_identifier <- use_identifier
  if (hasArg.date_format <- hasArg(date_format)) 
    .date_format <- date_format
  if (hasArg.verbose <- hasArg(verbose)) 
    .verbose <- verbose
  if (hasArg.days_to_omit <- hasArg(days_to_omit)) 
    .days_to_omit <- days_to_omit
  if (hasArg.indexTZ <- hasArg(indexTZ)) 
    .indexTZ <- indexTZ
  
  importDefaults("getSymbols.FI")
  
  # assign default.* values to those passed through argument list of getSymbols.FI
  # the default.* variables are used in pickArg()
  # the default.* and '.*' variables are duplicates of the same arguments
  default.date_from <- date_from
  default.to <- to
  default.dir <- dir
  default.return_class <- return_class
  default.extension <- extension
  default.split_method <- split_method[1]
  default.use_identifier <- use_identifier
  default.date_format <- date_format
  default.verbose <- verbose
  default.days_to_omit <- days_to_omit
  default.indexTZ <- indexTZ
  # end unused variables
  
  auto.assign <- if (hasArg(auto.assign)) {
    auto.assign
  } else {
    TRUE
  }
  
  env <- if (hasArg(env)) {
    env
  }
  else .GlobalEnv
  
  # get default load parameters for all Symbols
  symbol_lookup <- getSymbolLookup()
  
  # get default load parameters for symbol
  pickArg <- function(x, symbol_name) {
    # check if symbol_name was passed as argument, and get '.*' value
    if (get(paste("hasArg", x, sep="."))) {
      get(paste(".", x, sep=""))
    }
    # get value that was set using setSymbolLookup
    else if (!is.null(symbol_lookup[[symbol_name]][[x]])) {
      symbol_lookup[[symbol_name]][[x]]
    }
    # get 'default.*' value
    else get(paste("default", x, sep="."))
  }  # end pickArg
  
  fr <- NULL  # "fr" is never used
  
  ### huge lapply over list of symbols
  datl <- lapply(1:length(symbols_list),  # load data for list of symbols
                 function(symbol_index) {  # load data for single symbol
                   symbol_i <- symbols_list[[symbol_index]]
                   
                   # get default load parameters for symbol_i
                   from <- pickArg("from", symbol_i)
                   to <- pickArg("to", symbol_i)
                   dir <- pickArg("dir", symbol_i)
                   return_class <- pickArg("return_class", symbol_i)
                   file_extension <- pickArg("extension", symbol_i)
                   split_method <- pickArg("split_method", symbol_i)
                   use_identifier <- pickArg("use_identifier", symbol_i)
                   date_format <- pickArg("date_format", symbol_i)
                   verbose <- pickArg("verbose", symbol_i)
                   days_to_omit <- pickArg("days_to_omit", symbol_i)
                   indexTZ <- pickArg("indexTZ", symbol_i)
                   
                   # if use_identifier is set, then extract identifier from symbol
                   instr_str <- NA
                   if (!is.na(use_identifier)) {
                     tmp_instr <- try(getInstrument(symbol_i, silent=FALSE))
                     if (inherits(tmp_instr, "try-error") || !is.instrument(tmp_instr)) 
                       stop("must define instrument first to call with 'use_identifier'")
                     if (!use_identifier=="primary_id") {
                       instr_str <- make.names(tmp_instr$identifiers[[use_identifier]])
                     }
                     else instr_str <- make.names(tmp_instr[[use_identifier]])
                     if (length(instr_str)==0L) 
                       stop("Could not find instrument. Try with use_identifier=NA")
                   }  # end if
                   
                   # assign symbol name from either identifier or symbol
                   symbol_name <- ifelse(is.na(instr_str), make.names(symbol_i), instr_str)
                   
                   # drop last "/" from dir
                   ndc <- nchar(dir)
                   if (substr(dir, ndc, ndc)=="/")
                     dir <- substr(dir, 1, ndc - 1)
                   # add symbol_name to dir
                   ssd <- strsplit(dir, "/")[[1]]
                   if (identical(character(0), ssd) || 
                       (!identical(character(0), ssd) && ssd[length(ssd)] != symbol_name))
                     dir <- paste(dir, symbol_name, sep="/")
                   
                   # load data for single symbol from its directory
                   if (!dir=="" && !file.exists(dir)) {
                     if (verbose)
                       cat("\ndirectory ", dir, " does not exist, skipping\n")
                   } else {
                     if (verbose)
                       cat("loading ", symbol_i, ".....\n")
                     
                     ### start switch - either "days" or "common" 
                     switch(split_method[1],
                            days={  # load from daily files
                              # create vector of dates and file names
                              StartDate <- as.Date(from)
                              EndDate <- as.Date(to)
                              vec_dates <- as.Date(StartDate:EndDate)
                              vec_dates <- vec_dates[!weekdays(vec_dates) %in% days_to_omit]
                              vec_dates <- format(vec_dates, format=date_format)
                              vec_file_names <- paste(vec_dates, symbol_name, file_extension, sep=".")
                              if (dir!="") vec_file_names <- file.path(dir, vec_file_names)
                              
                              # loop over file names and load data
                              data_list <- lapply(vec_file_names, function(file_name_full) {
                                file_name <- strsplit(file_name_full, "/")[[1]]
                                file_name <- file_name[length(file_name)]
                                if (verbose) cat("Reading ", file_name, "...")
                                if (!file.exists(file_name_full)) {
                                  if (verbose) cat(" failed. File not found in ", dir, " ... skipping\n")
                                } else {
                                  data_name <- load(file_name_full)  # load invisibly and get character string of object names
                                  data_object <- get(data_name)  # get value of named object
                                  if (!is.na(indexTZ) && !is.null(data_object)) indexTZ(data_object) <- indexTZ
                                  if (verbose) cat(" done.\n")
                                  data_object  # return data from loop
                                }  # end if
                              }  # end anon function
                              )  # end lapply
                              
                              if (verbose) cat("rbinding data ... ")
                              data_complete <- do_call_rbind(data_list)
                            },  # end days
                            
                            common={
                              file_name <- paste(symbol_name, file_extension, sep=".")
                              if (dir != "") file_name <- file.path(dir, file_name)
                              if (!file.exists(file_name)) {
                                if (verbose) cat("file ", paste(symbol_name, file_extension, sep="."), " does not exist in ", dir, "....skipping\n")
                              } else {
                                data_name <- load(file_name)
                                data_object <- get(data_name)
                                if (!is.na(indexTZ) && !is.null(data_object)) indexTZ(data_object) <- indexTZ
                                assign("data_complete", data_object)
                                if (verbose) cat("done.\n")
                              }
                            }  # end common
                            
                     )  # end switch
                     
                     data_complete <- quantmod:::convert.time.series(data_complete=data_complete, return_class=return_class)
                     symbol_i <- make.names(symbol_i)
                     data_out <- list()
                     data_out[[symbol_i]] <- data_complete
                     if (verbose) 
                       cat("done.\n")
                     data_out
                   }  # end load data for single symbol
                 }  # end anon function for loading single symbol
                 
  )  # end lapply over list of symbols
  
  
  if (length(Filter("+", lapply(datl, length)))==0) {
    warning("No data found.")
    return(NULL)
  }
  
  datl.names <- do.call(c, lapply(datl, names))
  missing <- symbols_list[!symbols_list %in% datl.names]
  if (length(missing) > 0) 
    warning("No data found for ", paste(missing, collapse=" "))
  if (auto.assign) {
    out <- Filter(function(x) length(x) > 0, datl)
    invisible(lapply(out, function(x) assign(names(x), x[[1]], pos=env)))
    return(datl.names)
  }
  else {
    out <- lapply(datl, function(x) {
      if (length(x) > 0) 
        x[[1]]
    })
    if (length(out)==1) 
      return(out[[1]])
    else {
      names(out) <- symbols_list
      return(out)
    }
  }
}  # end getSymbols.FI
<environment: namespace:FinancialInstrument>
