################################################
###
### package h5 scripts for managing hdf5 files
### load and save data from hdf5 files
###
################################################

# set options
options(max.print=40)


# load packages
library(HighFreq)
library(h5)


### script #1
# create and save a binary HDF5 data file

## create H5File pointer to empty binary HDF5 file
file_pointer <- h5file("C:/Develop/data/data_test.h5")

## create H5Group pointer to data folder called group1, using subsetting operators
data_pointer <- file_pointer["group1"]

## store mat1 and mat2 data sets in group1 folder using pointer data_pointer
# the mat1 and mat2 data sets don't need to have same dimensions
# use H5Group pointer to data folder
data_pointer["mat1"] <- matrix(1:9, nrow=3)
data_pointer["mat2"] <- matrix(11:22, ncol=4)

## store mat1 and mat2 data sets in group1 folder using H5File pointer to data file
file_pointer["group1/mat1"] <- matrix(1:9, nrow=3)
file_pointer["group1/mat2"] <- matrix(11:22, ncol=4)

## add attribute to group1 folder using H5Group pointer
# create data pointer to folder called group1, using subsetting operators
data_pointer <- file_pointer["group1"]
h5attr(data_pointer, "attr1") <- "group1"
# doesn't work
# h5attr(file_pointer["group1"], "folder_attr") <- "group1"

## add attribute to mat1 data set using H5Group pointer to folder
h5attr(data_pointer["mat1"], "matrix_attr") <- "matrix1"

## add attribute to mat2 data set using H5File pointer to data file
h5attr(file_pointer["group1/mat2"], "matrix_attr") <- "matrix2"

## add group2 data folder with mat3 data set
file_pointer["group2/mat3"] <- matrix(21:30, ncol=2)

## add attribute to mat3 data set using H5File pointer to data file
h5attr(file_pointer["group2/mat3"], "matrix_attr") <- "matrix3"
# doesn't work
# h5attr(data_group2, "attr2") <- "group2 folder"

## print summary of datasets in file_pointer
# print names of datasets
list.datasets(file_pointer)
# print summary of datasets in file_pointer
sapply(list.datasets(file_pointer), function(x) file_pointer[x])

## print the data in group1 folder
file_pointer["group1"]

## print the data in mat1 dataset
# print all the data
file_pointer["group1/mat1"][]
# print the first two rows
file_pointer["group1/mat1"][1:2, ]

## close file pointer - file still locked
h5close(file_pointer)



### script #2
# open file in read/write mode to append extra data
file_pointer <- h5file("C:/Develop/data/data_test.h5", mode="r+")

## add mat4 data set in group2 data folder
file_pointer["group2/mat4"] <- matrix(11:20, ncol=2)

## add mat4 data set in group3 data folder
file_pointer["group3/mat4"] <- matrix(11:20, ncol=2)

## add attribute to mat3 data set using H5File pointer to data file
h5attr(file_pointer["group3/mat4"], "matrix_attr") <- "matrix4"

## print summary of datasets in file_pointer
sapply(list.datasets(file_pointer), function(x) file_pointer[x])

## print the data in mat4 dataset
# print all the data
file_pointer["group3/mat4"][]
# print the first two rows
file_pointer["group3/mat4"][1:2, ]

## print summary of datasets in file_pointer
# print names of datasets
list.datasets(file_pointer)
# print summary of datasets in file_pointer
sapply(list.datasets(file_pointer), function(x) file_pointer[x])

## close file pointer - file still locked
h5close(file_pointer)

# remove file
file.remove("C:/Develop/data/data_test.h5")



### script #3
# create and save a binary HDF5 data file

## create H5File pointer to empty binary HDF5 file
file_pointer <- h5file("C:/Develop/data/data_test.h5", mode="a")

## create group called group_folder and a character vector dataset called vec_tor
file_pointer["group_folder/vec_tor"] <- LETTERS[1:9]

## create pointer to vec_tor dataset
data_pointer <- file_pointer["group_folder/vec_tor"]
# print first 3 elements of vec_tor
data_pointer[1:3]
# add elements to vec_tor dataset
data_pointer <- c(data_pointer, LETTERS[10:26])
# print entire vec_tor dataset
data_pointer[]

## create named integer matrix called mat_rix and add it to group_folder
mat_rix <- matrix(1:9, nrow = 3)
rownames(mat_rix) <- LETTERS[1:3]
colnames(mat_rix) <- c("AY", "BE", "CE")
file_pointer["group_folder/mat_rix"] <- mat_rix

## store rownames and column names of mat_rix as attributes
# create pointer to mat_rix data set
data_pointer <- file_pointer["group_folder/mat_rix"]
# store rownames in attribute dimnames_1
h5attr(data_pointer, "dimnames_1") <- rownames(mat_rix)
# Store columnnames in attribute dimnames_2
h5attr(data_pointer, "dimnames_2") <- colnames(mat_rix)

## add to group_folder an array dataset called ar_ray
file_pointer["group_folder/ar_ray"] <- array(as.numeric(1:45), dim = c(3, 3, 5))

## print summary of datasets in file_pointer
# print names of datasets
list.datasets(file_pointer)
# print summary of datasets in file_pointer
sapply(list.datasets(file_pointer), function(x) file_pointer[x])

## close remove file pointer
h5close(data_pointer)
h5close(file_pointer)



### script #4
### binary HDF5 file containing Two Sigma Kaggle data
# https://www.kaggle.com/c/two-sigma-financial-modeling/data

# create pointer to binary HDF5 file with Kaggle data
file_pointer <- h5file("C:/Develop/data/train.h5")

## print summary of datasets in file_pointer
# print names of datasets
list.datasets(file_pointer)
# print summary of datasets in file_pointer
sapply(list.datasets(file_pointer), function(x) file_pointer[x])

# print dimensions of datasets in file_pointer
sapply(list.datasets(file_pointer), function(x) dim(file_pointer[x]))
# print first columns of datasets in file_pointer
sapply(list.datasets(file_pointer), function(x) file_pointer[x][, 1])


## close remove file pointer
h5close(file_pointer)

