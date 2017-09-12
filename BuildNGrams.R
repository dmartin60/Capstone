## Author: Don Martin
## Date  : 09/11/2017
## 
## Based on the Task 1 directives of the Coursera Data Science Capstone this program will:
##
##  1) Download the SwiftKey Sample & Profane words
##
##  2) Select a sample of the following en_US datasets then merge into one sample
##     "./final/en_US/en_US.blogs.txt"
##     "./final/en_US/en_US.news.txt"
##     "./final/en_US/en_US.twitter.txt"
##
##  3) Break samples into sentences then merge into one sample
##
##  4) Clean the merged sample, removing profanity
##
##  5) Create N-Gram Models to be used to predict the next word
##     Select 300000 of the most probable Ngrams for the Shiny app
##
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+

 # Memory Management Settings 
   memory.limit(size=64000)
   options(warn =-1)

 # Clear Memory   
   rm(list = ls(all=TRUE))
   gc()
   
 # Set Working Directory
   setwd("C:/Users/dmartin/COURSERA/10 Capstone")
   
 # SampleSize and Random Sample Seed
   sampleSize <- 0.005
   set.seed   <- 1000
   
 # Create Target Directory for N-Gram Dataframe Models
   unlink("./models", recursive = TRUE)
   if( !file.exists ("./models") ){
     dir.create  ("./models")
   }

##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Step01 Download the SwiftKey Sample & Profane words 
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
 
 Step01 <- as.numeric(Sys.time()) # Time starts.
 
 # Sample Files for the SwiftKey Corpus 
   zipFile   <- "Coursera-SwiftKey.zip"
   unZipDir  <- "./final/en_US/en_US.blogs.txt"
   urlAddr   <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  
 # Execute the download
   if( !file.exists (zipFile) ){ download.file(urlAddr, zipFile) } 
  
 # Unpack the zipFile
   if( !file.exists (unZipDir) ){ unzip(zipFile) }
   
 # Banned Word List for Profanity Filter    
   badFile  <- "./final/swearWords.txt"
   urlAddr <- "http://www.bannedwordlist.com/lists/swearWords.txt"
   
 # Execute the download
   if( !file.exists (badFile) ){ download.file(urlAddr, badFile) } 
   
 # Remove un-needed variables
   rm(zipFile,unZipDir,urlAddr)
   gc()

 Step01 <- round(as.numeric(Sys.time() - Step01), 2) # Time ends.
  
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Step02 Load & select a random sample the en_US data sets  
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
  
 Step02 <- as.numeric(Sys.time()) # Time starts.
   
 # Load "./final/en_US/en_US.blogs.txt"
   blogLines  <- readLines("./final/en_US/en_US.blogs.txt",encoding="UTF-8",warn=FALSE,skipNul = TRUE)
    
 # Load "./final/en_US/en_US.twitter.txt"  
   twitLines  <- readLines("final/en_US/en_US.twitter.txt",encoding="UTF-8",warn=FALSE,skipNul = TRUE)
    
 # Sample "./final/en_US/en_US.twitter.txt"
   twitSample <- sample(twitLines, length(twitLines) * sampleSize)
    
 # Load "./final/en_US/en_US.news.txt" using binary mode as there are special character issues  
   con       <- file("final/en_US/en_US.news.txt",open="rb")
   newsLines <- readLines(con,encoding="UTF-8",warn=FALSE,skipNul = TRUE)
   close(con)
 
 # Sample "./final/en_US/en_US.blogs.txt"
   blogSample <- sample(blogLines, length(blogLines) * sampleSize)
   
 # Sample  "./final/en_US/en_US.news.txt"
   newsSample  <- sample(newsLines, length(newsLines) * sampleSize)
   
 # Sample "./final/en_US/en_US.twitter.txt"
   twitSample <- sample(twitLines, length(twitLines) * (sampleSize/2))

 # Remove un-needed variables   
   rm(blogLines,newsLines,twitLines,con)
   gc()
   
 Step02 <- round(as.numeric(Sys.time() - Step02), 2) # Time ends. 
 
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Step03 Break samples up into sentences & merge into one sample
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
 
 Step03 <- as.numeric(Sys.time()) # Time starts
 
 # Load required libraries 
   suppressMessages( library(qdap))               # For Quantitative analysis of transcripts. (sent_detect)
   
 # Merge blog & news samples 
   textSample <- c(blogSample, newsSample)            
   textSample <- sent_detect(textSample)          # Break into sentences
   
 # Handle Twitter Sample    
   twitSample.nw <- c(NULL)
   for(i in 1:length(twitSample)){
       twitSample.sd <- sent_detect(twitSample[i]) # Break into sentences
       twitSample.nw <- c(twitSample.nw, twitSample.sd)
   }
   rm (i,twitSample.sd)
   twitSample <- twitSample.nw
   
 # Merge blog, news & twitter samples 
   textSample <- c(textSample, twitSample)

 # Remove un-needed variables  
   rm(blogSample,newsSample,twitSample,twitSample.nw)
   gc()

 Step03 <- round(as.numeric(Sys.time() - Step03), 2) # Time ends. 
 
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Step04 Clean the sample 
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
 
 Step04 <- as.numeric(Sys.time()) # Time starts
 
 # Load required libraries 
   suppressMessages( library(tm))            # For basic text-mining
   
 # Remove Non-ASCII Characters  
   textSample  <- iconv(textSample, from="latin1", to="ASCII", sub="")
   #save(textSample, file= "./Sample/Step03.textSample.RData")

 # Create the corpus       
   corpus <- VCorpus(VectorSource(textSample))

 # Supporting tm_mapFunctions
   removeURLs <- function(x) gsub("http[[:alnum:]]*", "", x) 
   toSpace    <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
   
   corpus <- tm_map(corpus, toSpace, "@[^\\s]+")                     # Remove Special characters       
   corpus <- tm_map(corpus, content_transformer(removeURLs))         # Remove URLs
   corpus <- tm_map(corpus, content_transformer(tolower))            # Convert to lower case
   corpus <- tm_map(corpus, content_transformer(removePunctuation))  # Remove Punctuation
   corpus <- tm_map(corpus, content_transformer(removeNumbers))      # Remove Numbers      
   corpus <- tm_map(corpus, stripWhitespace)                         # Remove whitespace
   
 # Remove Profanity
   if( file.exists (badFile) ){
     profanity <- readLines(badFile)
     rm(badFile)
     corpus    <- tm_map(corpus, removeWords, profanity)             # Remove profanity
   }

 # convert corpus to data frame   
   corpusDf <- data.frame(text = unlist(sapply(corpus, '[', 'content')), stringsAsFactors = F)
   
  #save(corpusDf, file= "./Sample/Step03.corpusDf.RData")
   
   rm(textSample,profanity,removeURLs,toSpace)
   gc()
   
 Step04<- round(as.numeric(Sys.time() - Step04), 2) # Time ends. 
 
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Step04 N-Gram Model Processing
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
 Step05 <- as.numeric(Sys.time()) # Time starts
 
 # Load required libraries 
   suppressMessages( library(RWeka))         # For N-gram generation and tokenization
   
 # Create N-Grams  
   uniGram    <- NGramTokenizer(corpusDf, Weka_control(min = 1, max = 1))
   biGram     <- NGramTokenizer(corpusDf, Weka_control(min = 2, max = 2))
   triGram    <- NGramTokenizer(corpusDf, Weka_control(min = 3, max = 3))
   rm(corpusDf)
   
 # Convert N-Grams to DataFrames 
   uniGramDf  <- data.frame(table(uniGram))  # Convert to DataFrame
   biGramDf   <- data.frame(table(biGram))
   triGramDf  <- data.frame(table(triGram))
   rm(uniGram,biGram,triGram)

 # Sort N-Grams by frequency        
   uniGramDf  <- uniGramDf[order(uniGramDf$Freq, decreasing = TRUE), ]
   biGramDf   <- biGramDf[order(biGramDf$Freq,   decreasing = TRUE), ]
   triGramDf  <- triGramDf[order(triGramDf$Freq, decreasing = TRUE), ]

 # Add header lables to the dataframes       
   names(uniGramDf) <- c("Token", "Freq")
   names(biGramDf)  <- c("Token", "Freq")
   names(triGramDf) <- c("Token", "Freq")
   
 # select significant 300000 N-Grams for use in Shiny application
   uniGramDf <- as.character(uniGramDf$Token[1:300000])
   biGramDf  <- as.character(biGramDf$Token[1:300000])
   triGramDf <- as.character(triGramDf$Token[1:300000])
   
 # save significant 300000 N-Grams for use in Shiny application
   save(uniGramDf, file = "./models/uniGramDf.RData")
   save(biGramDf,  file = "./models/biGramDf.RData")
   save(triGramDf, file = "./models/triGramDf.RData")

   gc()
   
 Step05 <- round(as.numeric(Sys.time() - Step05), 2) # Time ends.
 

  
 
  
 
 
  