###############################################################################
# Deepl learning with word2vector
# Version: 2
# author: khmelkoff
###############################################################################

# Load data and library #######################################################

library(tm) # for stopword list

training_data_1 <- read.delim(unz("unlabeledTrainData.tsv.zip", 
                               "unlabeledTrainData.tsv"),
                               header = TRUE,
                               sep = "\t",
                               quote = "",
                               as.is=TRUE)

training_data_2 <- read.delim(unz("testData.tsv.zip", 
                                "testData.tsv"),
                            header = TRUE,
                            sep = "\t",
                            quote = "",
                            as.is=TRUE)

training_data_3 <- read.delim(unz("labeledTrainData.tsv.zip", 
                                  "labeledTrainData.tsv"),
                              header = TRUE,
                              sep = "\t",
                              quote = "",
                              as.is=TRUE)

# Data preprocessing ##########################################################

training_data <- rbind(training_data_1, training_data_2, training_data_3[,c(1,3)]) 
training_data <- training_data$review

# clean HTML charactre
cleanHTML <- function(x) {
    return(gsub("<.*?>", "", x))
}

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

# Process 50000 records
process <- function(x){
    # Progress indicator
    if(x %% 1000 == 0) print(paste(x, "reviews processed")) 
    
    rw <- training_data[x]
    rw <- cleanHTML(rw)
    rw <- onlyText(rw)
    rw <- tokenize(rw)
    rw <- rw[nchar(rw)>1]
    rw <- rw[!rw %in% stopWords]
    paste(rw, collapse=" ") # paste tokens to text
}

rws <- sapply(1:length(training_data), process)

rws <- paste(rws, collapse=" ") #collaps to string

# write string to txt file
con <- file("train_data.txt", "wb")
writeChar(rws, con, eos=NULL)
close(con)

# Clean Env.
rm(training_data_1)
rm(training_data_2)
rm(training_data_3)
rm(training_data)
rm(rws)


# Train model #################################################################
# dyn.unload("dll/word2vec.dll") # for recompiling
dyn.load("dll/word2vec.dll")

word2vec <- function(train_file, output_file, 
                     binary,
                     cbow,
                     num_threads,
                     num_features,
                     window,
                     min_count,
                     sample,
                     classes)
{
    if (!file.exists(train_file)) stop("Can't find the train file!")
    train_dir <- dirname(train_file)
    
    if(missing(output_file)) {
        output_file <- gsub(gsub("^.*\\.", "", basename(train_file)), "bin", basename(train_file))
        output_file <- file.path(train_dir, output_file)
    }
    
    outfile_dir <- dirname(output_file)
    if (!file.exists(outfile_dir)) dir.create(outfile_dir, recursive = TRUE)
    
    train_file <- normalizePath(train_file, winslash = "/", mustWork = FALSE)
    output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
    
    OUT <- .C("CWrapper_word2vec", 
              train_file = as.character(train_file), 
              output_file = as.character(output_file),
              binary = as.character(binary),
              cbow = as.character(cbow),
              num_threads = as.character(num_threads),
              num_features = as.character(num_features),
              window = as.character(window),
              min_count = as.character(min_count),
              sample = as.character(sample),
              classes = as.character(classes))
    
    class(OUT) <- "word2vec"
    names(OUT)[2] <- "model_file"
    cat(paste("The model was generated in '", dirname(output_file), "'!\n", sep = ""))
    return(OUT)
}

# Train model
tstart <- Sys.time()
word2vec("train_data.txt", "model300.txt", 
         binary=0, # output format, 1-binary, 0-txt
         cbow=0, # skip-gram (0) or continuous bag of words (1)
         num_threads = 6, # num of workers
         num_features = 300, # word vector dimensionality
         window = 10, # context / window size
         min_count = 40, # minimum word count
         sample = 1e-3, # downsampling of frequent words
         classes = 0 # if >0 make k-means clustering 
         )
tend <- Sys.time()
tend - tstart

# Check model #################################################################
# dyn.unload("distance")
dyn.load("dll/distance.dll")

distance <- function(file_name, word, size)
{
    
    if (!file.exists(file_name)) stop("Can't find the model file!")
    
    OUT <- .C("CWrapper_distance", 
              file_name = as.character(file_name), 
              word = as.character(word),
              returnw = "",
              returnd = as.double(rep(0,size)),
              size = as.character(size))
    #return(OUT)
    vword <- strsplit(gsub("^ *", "", OUT$returnw), split = " ")[[1]]
    vdist <- OUT$returnd
    if (length(vword) == 0) vdist <- numeric()
    return(data.frame(Word = vword, CosDist = vdist, stringsAsFactors = FALSE))
}

distance("model300.bin", "bad", 15)
distance("model300.bin", "good", 15)

# Differences #################################################################

model <- read.table("model300.txt", sep=" ", as.is = TRUE, skip=1)
names(model)[1] <- "word"

similarity <- function(word1, word2, model) {
    size <- ncol(model)-1
    vec1 <- model[model$word==word1,2:size]
    vec2 <- model[model$word==word2,2:size]
    sim <- sum(vec1 * vec2)
    sim <- sim/(sqrt(sum(vec1^2))*sqrt(sum(vec2^2)))
    return(sim)
}


difference <- function(string, model) {
    words <- tokenize(string)
    num_words <- length(words)
    diff_mx <- matrix(rep(0,num_words^2), nrow=num_words, ncol=num_words)

    if (num_words<3) {
        return("Need at least three words")
    }
    
    for (i in 1:num_words) {
        if(dim(model[model$word==words[i],])[1]==0) {
            return(paste(words[i], ": No such word in model" ))
        }
    }
    
     
    for (i in 1:num_words) {
        for (j in 1:num_words) {
            sim <- similarity(words[i],words[j],model)
            if(i!=j) {
                diff_mx[i,j]=sim
            }
        }
    }
    return(words[which.min(rowSums(diff_mx))])
}

difference("squirrel deer human dog cat", model)
difference("bad red good nice awful", model)


# Word analogy ################################################################
# dyn.unload("word-analogy.dll")
dyn.load("dll/word-analogy.dll")

analogy <- function(file_name, words, size)
{
    if (!file.exists(file_name)) stop("Can't find the model file!")
    
    OUT <- .C("CWrapper_analogy", 
              file_name = as.character(file_name), 
              words = as.character(words),
              returnw = "",
              returnd = as.double(rep(0,as.integer(size))),
              size = as.character(size))
    
    # return(OUT)
    vword <- strsplit(gsub("^ *", "", OUT$returnw), split = " ")[[1]]
    vdist <- OUT$returnd
    if (length(vword) == 0) vdist <- numeric()
    return(data.frame(Word = vword, CosDist = vdist, stringsAsFactors = FALSE)) 
}    

analogy("model300.bin", "man woman king", 3)
analogy("model300.bin", "man woman husband", 3)
analogy("model300.bin", "man woman boy", 3)

# Word clustering #############################################################
# Clustering
tstart <- Sys.time()
word2vec("train_data.txt", "classes.txt", 
         binary=1, # output format, 1-binary, 0-txt
         cbow=0, # skip-gram (0) or continuous bag of words (1)
         num_threads = 6, # num of workers
         num_features = 300, # word vector dimensionality
         window = 10, # context / window size
         min_count = 40, # minimum word count
         sample = 1e-3, # downsampling of frequent words
         classes = 3000 # if >0 make k-means clustering 
)
tend <- Sys.time()
tend - tstart


clasters <- read.table("classes.txt", sep=" ", as.is = TRUE)
names(clasters) <- c("word", "id")
clasters <- clasters[order(clasters$id),]
max(clasters$id)

clasters[clasters$word=="humor",]
clasters[clasters$id==2952,][1:15,]

clasters[clasters$word=="cat",]
clasters[clasters$id==241,]

clasters[clasters$word=="military",]
clasters[clasters$id==322,]

