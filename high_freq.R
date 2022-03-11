##############################
### Read futures data from binary files, and aggregate and save it

library(HighFreq)

## reading binary data

# Read compressed files directly

# Open a connection to read the file in binary mode
connect_ion <- gzfile("C:/Develop/data/hull_data/dec2017/raw/ESH820171213.bin.gz", open="rb")

# Define column names
colnamev <- c("type", "actn", "posn", "cond", "Px", "Sz", "posixt", "pB1r", 
               "sB1r", "pA1r", "sA1r", "pB1c", "sB1c", "pA1c", "sA1c")

# Read header with number of rows, columns, and format info
head_er <- readBin(connect_ion, 'integer', 3)
# Read datav and coerce it to matrix
datav <- readBin(connect_ion, 'double', head_er[1]*head_er[2])
datav <- matrix(datav, nrow=head_er[1], ncol=head_er[2], 
                byrow=TRUE, dimnames=list(NULL, colnamev))
# Close the connection
close(connect_ion)

class(datav)
apply(datav, 2, class)

foo <- datav[(datav[, "posn"] == 0) & (datav[, "actn"] == 0) & (datav[, "cond"] == 0) & (datav[, "type"] == 2), 
             c("posixt", "Px", "Sz")]

foo[, "posixt"] <- round(foo[, "posixt"])

bar <- foo[(foo[, "posixt"] == 1513194128), ]

bar[(bar[, "type"] == 2), ]

