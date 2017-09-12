## Author: Don Martin
## Date  : 09/05/2017
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Predict using one word
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+

   predict_1 <- function(x, uniGramDf, biGramDf){
  
       se <- grep(paste0("^", x, "\\s"), 
                  biGramDf, 
                  perl  = TRUE, 
                  value = FALSE)
    
       if(length(se) == 0){
      
           print(c(as.character(uniGramDf[1]), 
                   as.character(uniGramDf[2]), 
                   as.character(uniGramDf[3]), 
                   as.character(uniGramDf[4]), 
                   as.character(uniGramDf[5]), 
                   as.character(uniGramDf[6]), 
                   as.character(uniGramDf[7]), 
                   as.character(uniGramDf[8]), 
                   as.character(uniGramDf[9]), 
                   as.character(uniGramDf[10])))
       }else{
              num  <- length(se)
              word <- NULL
              i <- 1
              while(i <= 10 && i <= num){
                    word[i] <- unlist(strsplit(as.character(biGramDf[se][i]), "\\s"))[2]
                    i = i + 1
              }
              return(word)
             }
   }

##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Predict using multiple words
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
   predict_m <- function(x, uniGramDf, biGramDf, triGramDf){
  
       se   <- grep(paste0("^", x, "\\s"), 
                    triGramDf, 
                    perl  = TRUE, 
                    value = FALSE)
    
       num  <- length(se)
       word <- NULL
    
       i <- 1
       while(i <= 10 && i <= num){
           word[i] <- unlist(strsplit(as.character(triGramDf[se][i]), "\\s"))[3]
           i = i + 1
       }
    
       if(length(word) != 10){
      
           second <- strsplit(x, "\\s")[[1]][2]
           se  <- grep(paste0("^", second, "\\s"), 
                       biGramDf, 
                       perl  = TRUE, 
                       value = FALSE)
           num <- length(se)
           len <- length(word)
        
           while(i <= 10 && i <= num + len){
                 word[i] <- unlist(strsplit(as.character(biGramDf[se][i - len]), "\\s"))[2]
                 i = i + 1
           }
        
           if(length(word) != 10){
              j <- 0
              len <- length(word)
              while(i <= 10){
                    word[i] <- as.character(uniGramDf[1 + j])
                    i <- i + 1
                    j <- j + 1
              }
           }
      }
      return(word)
    
  }

##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Predict driver function
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
   predict <- function(x, first, second, third){
           x <- tolower(x)
           y <- unlist(strsplit(as.character(x), "\\s"))
           
           if(length(y) == 0){
              y <- c("", 
                     "", 
                     "", 
                     "", 
                     "")  
           }else if(length(y) == 1){
              y <- predict_1(y, first, second)
              
              
           }else if(length(y) == 2){
              y <- paste(y, collapse = " ")
              y <- predict_m(y, first, second, third)
           }else{
              y <- y[-1:-(length(y) - 2)]
              y <- paste(y, collapse = " ")
              y <- predict_m(y, first, second, third)
           }
          return(y)
   }

   
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
## Capitalize the first character of each word
##--+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+   
   capwords <- function(s, strict = FALSE) {
     cap <- function(s) paste(toupper(substring(s, 1, 1)),
                              {s <- substring(s, 2); if(strict) tolower(s) else s},
                              sep = "", collapse = " " )
     sapply(strsplit(s, split = " "), 
            cap, 
            USE.NAMES = !is.null(names(s)))
   }

