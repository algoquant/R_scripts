##############################
### Read futures data from binary files, and aggregate and save it

library(HighFreq)

## reading binary data

# Read compressed files directly

# Open a connection to read the file in binary mode
connect_ion <- gzfile("C:/Develop/data/hull_data/dec_2017/raw/ESH8_20171213.bin.gz", open="rb")

# Define column names
col_names <- c("type", "actn", "posn", "cond", "Px", "Sz", "posixt", "pB1r", 
               "sB1r", "pA1r", "sA1r", "pB1c", "sB1c", "pA1c", "sA1c")

# Read header with number of rows, columns, and format info
head_er <- readBin(connect_ion, 'integer', 3)
# Read da_ta and coerce it to matrix
da_ta <- readBin(connect_ion, 'double', head_er[1]*head_er[2])
da_ta <- matrix(da_ta, nrow=head_er[1], ncol=head_er[2], 
                byrow=TRUE, dimnames=list(NULL, col_names))
# Close the connection
close(connect_ion)

class(da_ta)
apply(da_ta, 2, class)

foo <- da_ta[(da_ta[, "posn"] == 0) & (da_ta[, "actn"] == 0) & (da_ta[, "cond"] == 0) & (da_ta[, "type"] == 2), 
             c("posixt", "Px", "Sz")]

foo[, "posixt"] <- round(foo[, "posixt"])

bar <- foo[(foo[, "posixt"] == 1513194128), ]

bar[(bar[, "type"] == 2), ]

