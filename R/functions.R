# Function to check if layout is battle ready
checkBattleReady <- function(mat){
  # Check number of enem
  matSum <- sum(mat)
  
  # Empty or full matrices are ready
  if (matSum %in% c(0, 48)){
    return(TRUE)
  } else{
    remainder <- matSum %% 4
    nBlocks <- ceiling(matSum / 4)
  }
  
  # First check and remove any full columns
  fullCols <-  which(colSums(mat) == 4)
  nBlocks <- nBlocks - length(fullCols)
  mat[, fullCols] <- 0
  
  # Now look for columns for which rows 3 and 4 aren't both 1
  colsToFind <- which(
    sapply(
      1:12,
      function(i) sum(mat[3:4, i]) < 2
    )
  )
  
  # If there aren't any set the bottom 2 rows to zero
  if (length(colsToFind) == 0){
    nBlocks <- nBlocks - 6
    mat[3:4, ] <- 0
  } else{
    
    # Shift columns so 12 doesn't contain 1s in row 3 and 4
    if (! 12 %in% colsToFind){
      mat <- mat[, (0:11 - (12 - colsToFind[1])) %% 12 + 1]
    }
    
    # Remove any blocks of 4 in the bottom 2 rows in consecutive columns
    for (i in 1:10){
      if (sum(mat[3:4, i:(i + 1)]) == 4){
        nBlocks <- nBlocks - 1
        mat[3:4, i:(i + 1)] <- 0
      }
    }
  }
  
  # If nBlocks == 0 we are good
  if (nBlocks == 0){
    return(TRUE)
    # And if there are more than 1 we are not
  } else if (nBlocks > 1){
    return(FALSE)
  }
  
  # So we know number of blocks left is 1
  # We know there are no blocks of 4 remaining so..
  if (remainder == 0){
    return(FALSE)
    # And if there is only 1 left it is guaranteed to be in its own block
  } else if (remainder == 1){
    return(TRUE)
  }
  
  # So we have 2 or 3 enemies left and need to check they are in the same block
  # Check columns with no enemies
  zeroSumCols <- length(which(colSums(mat) == 0))
  if (zeroSumCols < 10){
    return(FALSE)
  }
  if (zeroSumCols > 10){
    return(TRUE)
  }
  
  # Finally need to check that remaining enemies are in consecutive colums
  lastCols <- which(colSums(mat) > 0)
  if ((max(lastCols) - min(lastCols)) %in% c(1, 11)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

# Function to shift layout by rotating a row
rowMove <- function(mat, row, shift){
  mat[row, ] <- mat[row, (0:11 - shift) %% 12 + 1]
  return(mat)
}

# Function to shift layout by rotating opposite columns
colMove <- function(mat, col, shift){
  mat <- rbind(mat[, 1:6], mat[4:1, 7:12])
  mat[, col] <- mat[(0:7 - shift) %% 8 + 1, col]
  mat <- cbind(mat[1:4, ], mat[8:5, ])
  return(mat)
}

# General move function
move <- function(mat, type, which, shift){
  if (type == "row"){
    rowMove(mat, which, shift)
  } else{
    colMove(mat, which, shift)
  }
}

# Table of all possible moves
getPossibleMoves <- function(){
  possibleMoves <- data.frame(
    type = c(rep("row", 44), rep("col", 42)),
    which = c(ceiling(1:44/11), ceiling(1:42/7)),
    shift = c(rep(1:11, 4), rep(1:7, 6))
  )
  
  createDescription <- function(i){
    type <- possibleMoves$type[i]
    which <- possibleMoves$which[i]
    shift <- possibleMoves$shift[i]
    
    if (type == "row"){
      typeDesc <- "Rotate"
      whichDesc <- c("outermost ring", "3rd ring from centre", "2nd ring from centre", "innermost ring")[which]
      shiftDesc <- ifelse(shift <= 6, paste0("clockwise by ", shift), paste0("anticlockwise by ", 12 - shift))
    } else{
      typeDesc <- "Shift"
      whichDesc <- paste0("column ", which)
      shiftDesc <- ifelse(shift <= 4, paste0("down by ", shift), paste0("up by ", 8 - shift))
    }
    return(paste0(typeDesc, " ", whichDesc, " ", shiftDesc))
  }
  
  possibleMoves$description <- sapply(seq_len(86), createDescription)
  
  return(possibleMoves)
}

# Function to take a matrix and return list of possible next move positions
nextPositions <- function(mat){
  possibleMoves <- getPossibleMoves()
  
  lapply(
    1:86,
    function(i){
      move(mat, possibleMoves$type[i], possibleMoves$which[i], possibleMoves$shift[i])
    }
  )
}

# function to find solution
findSolution <- function(mat, maxDepth = 3, verbose = TRUE){

  ### Check matrix is not already solved (depth 0)
  if (checkBattleReady(mat)){
    if (verbose) cat("Already set in best battle formation.\n")
    return(numeric(0))
  }
  
  ### Check depth 1
  # Get next positions
  nextPos <- nextPositions(mat)
  
  # Find which are solved
  whichReady <- which(sapply(
    nextPos,
    checkBattleReady
  ))
  
  # If we are done, return
  if (length(whichReady) > 0){
    if (verbose) cat(paste0("Found solution in 1 move.\n"))
    return(whichReady[1])
  }
  
  # If at max depth already and no solution return zero length numeric
  if (maxDepth == 1){
    if (verbose) cat("No solution found.\n")
    return(numeric(0))
  }
  
  ### Otherwise go deeper
  # Next to check
  matsToCheck <- which(
    sapply(
      nextPos,
      function(x){
        ! identical(x, mat)
      }
    )
  )
  
  # Loop through possible next moves
  for (depth in seq(1, maxDepth - 1)){
    # if (depth == maxDepth - 1){
    #   if (verbose) cat(paste0(
    #     "No solution in fewer than ",
    #     maxDepth,
    #     " moves.\nSearching through solution space at maximum depth...\n"))
    # }
    for (i in matsToCheck){
      val <- findSolution(nextPos[[i]], maxDepth = depth, verbose = FALSE)
      if (length(val) == 0){
        # if (depth == maxDepth - 1){
        #   if (verbose) cat(paste0(format(100 * round(which(i == matsToCheck)/length(matsToCheck), 3), nsmall = 1), "% complete\n"))
        # }
      } else{
        if (verbose) cat(paste0("Found solution in ", length(val) + 1, " moves.\n"))
        return(c(i, val))
      }
    }
  }
  
  # Otherwise no solution can be found
  if (verbose) cat("No solution found.\n")
  return(numeric(0))
}

paperMarioBattle <- function(enemyPositions = list(), numMoves = 3L){
  if (! is.numeric(numMoves)){
    numMoves <- suppressWarnings(as.numeric(numMoves))
    if (is.na(numMoves)){
      numMoves <- 3L
    }
  } else if (is.na(numMoves)){
    numMoves <- 3
  }
  
  mat <- matrix(rep(0, 48), nrow = 4)
  for (x in enemyPositions){
    mat[x[1], x[2]] <- 1
  }
  
  print(mat)

  str <- capture.output(solution <- findSolution(mat, numMoves, TRUE))
  
  possibleMoves <- getPossibleMoves()
  
  for (i in seq_along(solution)){
    moveDesc <- paste0(i, ") ", possibleMoves$description[solution[i]])
    cat(paste0(moveDesc, "\n"))
    str <- c(str, moveDesc)
  }
  
  return(paste0(str, collapse = "\n"))
}
