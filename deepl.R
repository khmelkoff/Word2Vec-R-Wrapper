###############################################################################
# Deepl learning with word2vector
# Version: 1
# author: khmelkoff
###############################################################################

# Load data and library #######################################################

library(tm)
#library(SnowballC)
#library(e1071)
#library(caret)
#library(randomForest)


training_data <- read.delim(unz("labeledTrainData.tsv.zip", 
                               "labeledTrainData.tsv"),
                               header = TRUE,
                               sep = "\t",
                               quote = "",
                               as.is=TRUE)

# Data preprocessing ##########################################################

dim(training)
training_data <- training_data$review

# clean HTML charactre
cleanHTML <- function(x) {
    return(gsub("<.*?>", "", x))
}
training_data <- cleanHTML(r1)

# clean special symbols
onlyText <- function(x) {
    x <- gsub("'s", "", x) 
    return(gsub("[^a-zA-Z]", " ", x)) 
}

# Tokenaize
tokenize <- function(x) {
    x <- tolower(x)
    x <- unlist(strsplit(x, split=" "))
}

# Create stopwords list
stopWords <- stopwords("en")

# Process 25000 records
rws <- sapply(1:length(training_data), function(x){
    # Progress indicator
    if(x %% 1000 == 0) print(paste(x, "reviews processed")) 
    
    rw <- training_data[x]
    rw <- cleanHTML(rw)
    rw <- onlyText(rw)
    rw <- tokenize(rw)
    rw <- rw[nchar(rw)>1]
    rw <- rw[!rw %in% stopWords]
    paste(rw, collapse=" ") # paste tokens to text
})

rws <- paste(rws, collapse=" ") #collaps to string

# write string to txt file
con <- file("train_data.txt", "wb")
writeChar(rws, con, eos=NULL)
close(con)

# Train model #################################################################
dyn.load("word2vec")


word2vec <- function(train_file, output_file)
{
    if (!file.exists(train_file)) stop("Can't find the trsin file!")
    train_dir <- dirname(train_file)
    
    if(missing(output_file)) {
        output_file <- gsub(gsub("^.*\\.", "", basename(train_file)), "bin", basename(train_file))
        output_file <- file.path(train_dir, output_file)
    }
    
    outfile_dir <- dirname(output_file)
    if (!file.exists(outfile_dir)) dir.create(outfile_dir, recursive = TRUE)
    
    train_file <- normalizePath(train_file, winslash = "/", mustWork = FALSE)
    output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
    # Whether to output binary, default is 1 means binary.
    binary = 1
    
    OUT <- .C("CWrapper_word2vec", 
              train_file = as.character(train_file), 
              output_file = as.character(output_file),
              binary = as.character(binary))
    
    class(OUT) <- "word2vec"
    names(OUT)[2] <- "model_file"
    cat(paste("The model was generated in '", dirname(output_file), "'!\n", sep = ""))
    return(OUT)
}

# Train model
word2vec("train_data.txt", "model.bin")

# Check model #################################################################
dyn.load("distance")

distance <- function(file_name, word)
{
    if (!file.exists(file_name)) stop("Can't find the model file!")
    N <- 20
    
    OUT <- .C("CWrapper_distance", 
              file_name = as.character(file_name), 
              word = as.character(word),
              returnw = "",
              returnd = as.double(rep(0,N)))
    #return(OUT)
    vword <- strsplit(gsub("^ *", "", OUT$returnw), split = " ")[[1]]
    vdist <- OUT$returnd
    if (length(vword) == 0) vdist <- numeric()
    return(data.frame(Word = vword, CosDist = vdist, stringsAsFactors = FALSE))
}

distance("model.bin", "film")



