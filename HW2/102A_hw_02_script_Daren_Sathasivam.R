## Part 1
# on Rmd file

## Part 2
# Start and end coord for chutes and ladders
rm(list = ls())
show_board <- function(board) {
  # Extract board dimensions and chutes/ladders information
  board_dim <- board$dim
  chutes <- board$chutes
  ladders <- board$ladders
  
  plot.new()
  plot.window(xlim = c(0, board_dim$row), ylim = c(0, board_dim$col), asp = 1)
  # Create tiles
  for (r in 0:board_dim$row) {
    segments(x0 = 0, y0 = r, x1 = board_dim$col, y1 = r)
  }
  for (c in 0:board_dim$col) {
    segments(x0 = c, y0 = 0, x1 = c, y1 = board_dim$row)
  }
  # Number the tiles
  for (r in seq_len(board_dim$row)) {
    if (r %% 2 == 1) { # Odd
      for (c in seq_len(board_dim$col)) {
        text(x = c - 0.5, y = r - 0.5, labels = (r - 1) * board_dim$col + c)
      }
    } else if (r %% 2 == 0) { # Even
      for (c in seq_len(board_dim$col)) {
        text(x = (board_dim$col - c) + 0.5, y = r - 0.5, labels = (r - 1) * board_dim$col + c)
      }
    }
  }
  # Arrows
  for (i in seq_along(chutes$start)) {
    start_row <- (chutes$start[i] - 1) %/% board_dim$col + 1
    start_col <- ifelse(start_row %% 2 == 1, 
                        (chutes$start[i] - 1) %% board_dim$col + 1, 
                        board_dim$col - (chutes$start[i] - 1) %% board_dim$col)
    end_row <- (chutes$end[i] - 1) %/% board_dim$col + 1
    end_col <- ifelse(end_row %% 2 == 1, 
                      (chutes$end[i] - 1) %% board_dim$col + 1, 
                      board_dim$col - (chutes$end[i] - 1) %% board_dim$col)
    
    x_start <- start_col - 0.5
    x_end <- end_col - 0.5
    y_start <- start_row - 0.5
    y_end <- end_row - 0.5
    
    arrows(
      x0 = x_start,
      x1 = x_end,
      y0 = y_start,
      y1 = y_end,
      col = "red", length = 0.1, lwd = 2
    )
  }
  for (i in seq_along(ladders$start)) {
    start_row <- (ladders$start[i] - 1) %/% board_dim$col + 1
    start_col <- ifelse(start_row %% 2 == 1, 
                        (ladders$start[i] - 1) %% board_dim$col + 1, 
                        board_dim$col - (ladders$start[i] - 1) %% board_dim$col)
    end_row <- (ladders$end[i] - 1) %/% board_dim$col + 1
    end_col <- ifelse(end_row %% 2 == 1, 
                      (ladders$end[i] - 1) %% board_dim$col + 1, 
                      board_dim$col - (ladders$end[i] - 1) %% board_dim$col)
    
    x_start <- start_col - 0.5
    x_end <- end_col - 0.5
    y_start <- start_row - 0.5
    y_end <- end_row - 0.5
    
    arrows(
      x0 = x_start,
      x1 = x_end,
      y0 = y_start,
      y1 = y_end,
      col = "green", length = 0.1, lwd = 2
    )
  }
}

## Part 3
# Miniboards on Rmd File

## Part 4
spin <- function() {
  sample(6, 1)
}
play_solo <- function(board, verbose = FALSE) {
  # Initializers for board components
  dim <- board$dim
  chutes <- board$chutes
  ladders <- board$ladders
  # Initializers for game components
  pos <- 0
  max_pos <- dim$row * dim$col
  turns <- 0
  chute_tally <- rep(0, length(chutes$start))
  ladder_tally <- rep(0, length(ladders$start))
  move_log <- c()
  
  # Find chute/ladder(specials)
  find_special <- function(pos, specials) { 
    for (i in seq_along(specials$start)) {
      if (specials$start[i] == pos) {
        return(list(new_pos = specials$end[i], idx = i))
      }
    }
    return(NULL)
  }
  
  # Loop through game
  while (pos != max_pos) {
    turns <- turns + 1
    start_pos <- pos
    move <- spin()
    pos <- pos + move
    if (verbose) {
      cat("Turn", turns, "\nStart at ", start_pos, "\nSpinner: ", move, "\n")
    }
    # Check if move exceeds max pos then set back to pos at start of turn
    if (pos > max_pos) {
      pos <- start_pos
    } else {
      # Check for chute/ladder(special)
      special <- find_special(pos, chutes)
      if (!is.null(special)) {
        chute_tally[special$idx] <- chute_tally[special$idx] + 1
        if (verbose) {
          cat("Landed on:", pos, "\nChute!\n")
        }
        pos <- special$new_pos
      } else {
        special <- find_special(pos, ladders)
        if (!is.null(special)) {
          ladder_tally[special$idx] <- ladder_tally[special$idx] + 1
          if (verbose) {
            cat("Landed on:", pos, "\nLadder!\n")
          } 
          pos <- special$new_pos
        }
      }
    }
    # Move log
    move_log <- c(move_log, pos)
    if (verbose) {
      cat("Turn ends at: ", pos, "\n \n")
    }
  }
  # Output
  return(list(turns = turns, chute_tally = chute_tally, ladder_tally = ladder_tally, move_log = move_log))
}
# Compare seed-10 to example output file
set.seed(10)
play_solo(board, TRUE)
play_solo(board)
# Test seed to match instructions
set.seed(5)
replicate(28, sample(6, 1))


## Part 5
set.seed(5)
simulation <- replicate(10000, play_solo(board), simplify = FALSE)
# Extract turn data
total_turns <- sapply(simulation, function(x){
  x$turns
})
min_turns <- min(total_turns)
max_turns <- max(total_turns)
median_turns <- median(total_turns)
mean_turns <- mean(total_turns)
prop_morethan_100 <- mean(total_turns >= 100)
prop_lessthan_10 <- mean(total_turns <= 10)
# Extract chute/ladder data
chute_tallies <- tapply(unlist(lapply(simulation, function (x) {
  x$chute_tally
})), rep(seq_along(board$chutes$start), times = 10000), sum)
ladder_tallies <- tapply(unlist(lapply(simulation, function (x) {
  x$ladder_tally
})), rep(seq_along(board$ladders$start), times = 10000), sum)
prop_ladder9 <- ladder_tallies[9] / 10000
# Visualization
hist(total_turns, breaks = 50, main = "Histogram of turns to complete a game of Chutes and Ladders", xlab = "Turns")
barplot(chute_tallies / 10000, main = "Relative Frequency of Chute Usage", xlab = "Chute Number", ylab = "Frequency", col = "red")
barplot(ladder_tallies / 10000, main = "Relative Frequncy of Ladder Usage", xlab = "Ladder Number", ylab = "Frequency", col = "green")