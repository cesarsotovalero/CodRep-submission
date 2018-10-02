####################################
# ----------- PACKAGES ----------- #
####################################

if("tidyverse" %in% rownames(installed.packages()) == F) {
  install.packages("tidyverse")
}

if("tidytext" %in% rownames(installed.packages()) == F) {
  install.packages("tidytext")
}

if("rJava" %in% rownames(installed.packages()) == F) {
  install.packages("rJava")
}

if("stringi" %in% rownames(installed.packages()) == F) {
  install.packages("stringi")
}

if("optparse" %in% rownames(installed.packages()) == F) {
  install.packages("optparse")
}

# turn off warning messages
options(warn=-1)

library(tidyverse, warn.conflicts = F)
library(stringdist, warn.conflicts = F)
library(tidytext, warn.conflicts = F)
options(java.parameters = "-Xmx12g" )
library(rJava, warn.conflicts = F)
library(stringi, warn.conflicts = F)
library(optparse, warn.conflicts = F)
library(methods)

.jinit('.')
.jaddClassPath('MyParser.jar')
checker <- .jnew('ParserChecker')


#####################################
# ----------- FUNCTIONS ----------- #
#####################################

removeComments <- function(sentence) {
  if (grepl("//", sentence) == T) {
    return(unlist(strsplit(sentence, "//")[1])[1])
  }else {
    return(sentence)
  }
} 

preprocessCode <- function(code) {
  for (i in 1:length(code)) {
    line <- code[i]
    if ((startsWith(line, "/*") && !str_detect(line, "\\*/") && endsWith(line, "*/")) ||
        startsWith(line, "*") ||
        nchar(line) < 1) {
      code[i] <- NA
    }
  }
  return(code)
}

isInTrain <- function(subTrain, code) {
  if (length(subTrain) == 0) {
    return(NULL)
  } else{
    for (i in 1:length(subTrain)) {
      for (j in 1:length(code)) {
        if (!is.na(code[j]) &&  subTrain[i] == code[j]) {
          return(j)
        }
      }
    }
  }
  return(NULL)
}

# If the query is an import statement, this function matches 
# the import string in the code that have the same name of the class
# at the end of the import statement in the code
processImports <- function(query, code) {
  sol_line <- amatch(query,
                     code,
                     method = 'jw',
                     maxDist = Inf)
  sol <- code[sol_line]
  
  # save the initial sol
  initial_sol <- sol_line
  
  for (i in 1:length(code)) {
    
    if (is.na(code[sol_line])) {
      # best match
      return(initial_sol)
    }
    
    sol_splitted <- unlist(strsplit(sol, "[.]"))
    query_splitted <- unlist(strsplit(query, "[.]"))
    
    paste(sol_splitted[length(sol_splitted)])
    paste(query_splitted[length(query_splitted)])
    
    if (query_splitted[length(query_splitted)] == "*;") {
      if (paste(sol_splitted[length(sol_splitted) - 1], sol_splitted[length(sol_splitted)], sep = ".") ==
          paste(query_splitted[length(query_splitted) - 1], query_splitted[length(query_splitted)], sep = ".")) {
        # best match
        return(sol_line)
      }
    } else if (sol_splitted[length(sol_splitted)] == query_splitted[length(query_splitted)]) {
      # best match
      return(sol_line)
    }
    code[sol_line] <- NA
    sol_line <- amatch(query,
                       code,
                       method = 'jw',
                       maxDist = Inf)
    sol <- code[sol_line]
  }
  return(initial_sol)
}

#####################################
# ------------ PROGRAM ------------ #
#####################################

# construct the training dataset
train <- bind_rows(
  read.csv("Train/dataset1_df.csv"),
  read.csv("Train/dataset2_df.csv"),
  read.csv("Train/dataset3_df.csv"),
  read.csv("Train/dataset4_df.csv")
)
train <- train %>% select(Query, SolutionStr) %>% distinct()

# dataset for analysis
cat("\nPlease type the path to the \"Tasks/\" folder: ")
path_tasks <- readLines("stdin", n = 1)
cat("\n")
cat("Prediction starting...\n")

file_names_tasks <- dir(path_tasks, pattern = ".txt")

#  main data frame
df <- NULL

for (i in 1:length(file_names_tasks)) {
  # print information about the remainder files
  if ((length(file_names_tasks) - i) %% 20 == 0) {
    print(paste("Remaining files : ", length(file_names_tasks) - i))
  }
  # clear the console
  if ((length(file_names_tasks) - i) %% 100 == 0) {
    cat("\014")
  }
  
  conn_tasks <-
    file(paste(path_tasks, file_names_tasks[i], sep = ""), open = "r")
  lines_tasks <- readLines(conn_tasks, warn = F)
  
  # read the query
  query <- lines_tasks[1]
  # create code vector for parsing check
  code_parsing <- c()
  # create code vector for distance function
  code <- c()
  for (j in 3:length(lines_tasks)) {
    # concatenate with the vector
    code_parsing <- c(code_parsing, lines_tasks[j])
    # remove tabs and spaces
    lines_tasks[j] <- gsub("\t", "", lines_tasks[j], fixed = TRUE)
    lines_tasks[j] <- trimws(lines_tasks[j])
    # concatenate with the vector
    code <- c(code, lines_tasks[j])
  }
  
  # preprocess code
  code <- preprocessCode(code)
  
  # check if query is in train set
  subTrain <- train %>% filter(Query == query)
  subTrain <- subTrain$SolutionStr
  inTrain <- isInTrain(subTrain, code)
  if (!is.null(inTrain)) {
    my_solution_line <- inTrain
  } else if (startsWith(query, "import ")) {
    my_solution_line <- processImports(query, code)
  }else{
    # select the best code match
    my_solution_line <-
      amatch(query, code, method = 'jw', maxDist = Inf)
    initial_match <- my_solution_line
    
    # check the size of the task file
    fileSize <-
      file.info(paste(path_tasks, file_names_tasks[i], sep = ""))$size < 50000
    if (fileSize == TRUE) {
      # write buffer
      fileConn <- file("buffer.txt")
      codeTMP <- code_parsing
      
      # put the query
      codeTMP[my_solution_line] <- query
      
      writeLines(codeTMP, fileConn)
      close(fileConn)
      
      # checking parsing
      passParsing <-
        .jcall(checker,
               'S',
               'checkFile',
               'buffer.txt')
      
      TOL <- 1
      while (removeComments(code[my_solution_line]) == removeComments(query) ||
             passParsing == "FAILED")
      {
        code[my_solution_line] <- NA
        
        my_solution_line <-
          amatch(query, code, method = 'jw', maxDist = Inf)
        
        # put the query
        codeTMP <- code_parsing
        codeTMP[my_solution_line] <- query
        
        # write buffer
        fileConn <- file("buffer.txt")
        writeLines(codeTMP, fileConn)
        close(fileConn)
        
        # checking parsing
        passParsing <-
          .jcall(checker,
                 'S',
                 'checkFile',
                 'buffer.txt')
        
        print(paste(file_names_tasks[i], ":", passParsing))
        
        if (TOL == 30 || is.na(my_solution_line)) {
          break
        }
        TOL <- TOL + 1
      }
      
      if (passParsing == "FAILED") {
        my_solution_line <- initial_match
      }
      
    } else{
      while (removeComments(code[my_solution_line]) == removeComments(query))
      {
        code[my_solution_line] <-  NA
        my_solution_line <-
          amatch(query, code, method = 'jw', maxDist = Inf)
      }
    }
  }
  
  # concatenate with the dataframe
  tmp_df <-
    data_frame(
      File = file_names_tasks[i],
      Query = query,
      MySolutionLine = my_solution_line
    )
  df <- bind_rows(df, tmp_df)
  
  # close files
  close(conn_tasks)
}

# write the my solution files
for (i in 1:dim(df)[1]){
  fileConn <- file(paste("Outputs/", df[i, 1], sep = ""))
  writeLines(paste(paste(path_tasks, df[i,1], sep = ""), df[i,3], sep = " "), fileConn)
  close(fileConn)
}

cat("\nProgram finished with exit code 0\nSee the predictions for each file in the \"Outputs/\" folder\n")




