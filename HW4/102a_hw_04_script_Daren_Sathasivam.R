library(R6)

# gameboard and decks -----------------------------------------------------
# Do not change this code

gameboard <- data.frame(
  space = 1:40, 
  title = c(
    "Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue",
    "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance",
    "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
    "Electric Company", "States Avenue", "Virginia Avenue",
    "Pennsylvania Railroad", "St. James Place", "Community Chest",
    "Tennessee Avenue", "New York Avenue", "Free Parking",
    "Kentucky Avenue", "Chance", "Indiana Avenue", "Illinois Avenue",
    "B & O Railroad", "Atlantic Avenue", "Ventnor Avenue", "Water Works",
    "Marvin Gardens", "Go to jail", "Pacific Avenue",
    "North Carolina Avenue", "Community Chest", "Pennsylvania Avenue",
    "Short Line Railroad", "Chance", "Park Place", "Luxury Tax",
    "Boardwalk"), stringsAsFactors = FALSE)
# gameboard
# gameboard[8, 2] --> Chance
chancedeck <- data.frame(
  index = 1:15, 
  card = c(
    "Advance to Go", "Advance to Illinois Ave.",
    "Advance to St. Charles Place", "Advance token to nearest Utility",
    "Advance token to the nearest Railroad",
    "Take a ride on the Reading Railroad",
    "Take a walk on the Boardwalk", "Go to Jail", "Go Back 3 Spaces",
    "Bank pays you dividend of $50", "Get out of Jail Free",
    "Make general repairs on all your property", "Pay poor tax of $15",
    "You have been elected Chairman of the Board", 
    "Your building loan matures"), stringsAsFactors = FALSE)
# chancedeck # Moving cards(1, 2, 3, 4, 5, 6, 7, 8, 9)
# chancedeck$card[3]
communitydeck <- data.frame(
  index = 1:16, 
  card = c(
    "Advance to Go", "Go to Jail",
    "Bank error in your favor. Collect $200", "Doctor's fees Pay $50",
    "From sale of stock you get $45", "Get Out of Jail Free",
    "Grand Opera Night Opening", "Xmas Fund matures", "Income tax refund",
    "Life insurance matures. Collect $100", "Pay hospital fees of $100",
    "Pay school tax of $150", "Receive for services $25",
    "You are assessed for street repairs",
    "You have won second prize in a beauty contest",
    "You inherit $100"), stringsAsFactors = FALSE)
# communitydeck # Moving cards(1, 2)
# communitydeck$card[2]

# RandomDice class --------------------------------------------------------
# Do not change this code

RandomDice <- R6Class(
  classname = "RandomDice",
  public = list(
    verbose = NA,
    initialize = function(verbose = FALSE){
      stopifnot(is.logical(verbose))
      self$verbose = verbose
    },
    roll = function() {
      outcome <- sample(1:6, size = 2, replace = TRUE)
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)

# Preset Dice -------------------------------------------------------------
# Do not change this code

PresetDice <- R6Class(
  classname = "PresetDice",
  public = list(
    verbose = NA,
    preset_rolls = double(0),
    position = 1,
    initialize = function(rolls, verbose = FALSE){
      stopifnot(is.logical(verbose))
      stopifnot(is.numeric(rolls))
      self$preset_rolls = rolls
      self$verbose = verbose
    },
    roll = function(){
      if(self$position > length(self$preset_rolls)){
        stop("You have run out of predetermined dice outcomes.")
      }
      outcome <- c(self$preset_rolls[self$position], 
                   self$preset_rolls[self$position + 1])
      self$position <- self$position + 2
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)


# Chance and Community Decks ----------------------------------------------
# Do not change this code

# This R6 class object shuffles the card deck when initialized.
# It has one method $draw(), which will draw a card from the deck.
# If all the cards have been drawn (position = deck length), then it will
# shuffle the cards again.
# The verbose option cats the card that is drawn on to the screen.
CardDeck <- R6Class(
  classname = "CardDeck",
  public = list(
    verbose = NA,
    deck_order = double(0), 
    deck = data.frame(),
    position = 1,
    initialize = function(deck, verbose = FALSE){
      stopifnot(is.data.frame(deck),
                is.numeric(deck[[1]]),
                is.character(deck[[2]]))
      self$deck_order <- sample(length(deck[[1]]))
      self$verbose <- verbose
      self$deck <- deck
    },
    draw = function(){
      if(self$position > length(self$deck_order)){
        # if we run out of cards, shuffle deck
        # and reset the position to 1
        if(self$verbose){
          cat("Shuffling deck.\n")
        }
        self$deck_order <- sample(length(self$deck[[1]]))
        self$position <- 1
      }
      outcome <- c(self$deck_order[self$position]) # outcome is the value at position
      self$position <- self$position + 1 # advance the position by 1
      if(self$verbose){
        cat("Card:", self$deck[outcome, 2], "\n")
      }
      outcome # return the outcome
    }
  )
)


# R6 Class SpaceTracker ---------------------------------------------------
# Do not change this code

SpaceTracker <- R6Class(
  classname = "SpaceTracker",
  public = list(
    counts = rep(0, 40),
    verbose = TRUE,
    tally = function(x){
      self$counts[x] <- self$counts[x] + 1
      if(self$verbose){
        cat("Added tally to ", x, ": ", gameboard$title[x], ".\n", sep = "")
      }
    },
    initialize = function(verbose){
      self$verbose <- verbose
    }
  )
)

# R6 Class Player ---------------------------------------------------------
## You'll need to expand on this

Player <- R6Class(
  classname = "Player",
  public = list(
    pos = 1,
    verbose = TRUE,
    jail_turns = 0, # Counter for turns spent in jail
    doubles_count = 0, # Counter for consecutive doubles rolled
    move_fwd = function(n){
      self$pos <- self$pos + n
      if(self$pos > 40){
        self$pos <- self$pos - 40
      }
      if(self$verbose){
        cat("Player moves forward ", n, ".\n", sep = "") # Player moves forward dice roll val
        cat("Player is now at ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
      }
    },
    # Go to Jail
    go_to_jail = function() {
      self$pos <- 11 # Set position to 11(Jail)
      self$jail_turns <- 1 
      if (self$verbose) {
        cat("Player goes to jail. \n")
      }
    },
    go_to_space = function(n) {
      self$pos <- n
      if (self$verbose) {
        cat("Player moves to ", n, ": ", gameboard$title[n], ".\n", sep = "")
      }
    },
    initialize = function(verbose = FALSE, pos = 1) {
      self$verbose <- verbose
      self$pos <- pos
    }
  )
)


# VERY BASIC turn taking example ------------------------------------------
# You will need to expand this
# You can write helper function if you want
# For cards only implement the ones that move the tokens
take_turn <- function(player, spacetracker) {
  dice_rolls <- dice$roll()
  continue_turn <- TRUE
  in_jail <- player$jail_turns > 0
  just_released <- FALSE
  third_turn <- FALSE
  
  # Check if player is in jail
  if (in_jail) {
    player$jail_turns <- player$jail_turns + 1
    # Escape jail conditions
    if (dice_rolls[1] == dice_rolls[2] || player$jail_turns > 3) {
      # spacetracker$tally(player$pos)
      if (dice_rolls[1] == dice_rolls[2]) {
        if (player$verbose) {
          cat("In jail but rolled doubles. \nPlayer exits jail. \n")
          cat("Player starts at 11: Jail. \n")
        }
        player$move_fwd(sum(dice_rolls))
        just_released <- TRUE
        spacetracker$tally(player$pos)  # Tally the new position after moving
      }
      if (player$jail_turns > 3) {
        if (player$verbose) {
          cat("Player's third turn in jail. Player must exit jail. \nPlayer exits jail. \n")
        }
        if (!dice_rolls[1] == dice_rolls[2]) {
          third_turn <- TRUE
        }
        # Do not tally "Jail" here since player is exiting after third turn without rolling doubles
      }
      if (!just_released && !third_turn){
        spacetracker$tally(player$pos)
      }
      player$jail_turns <- 0
    } else {
      if (player$verbose) {
        cat("Player stays in jail. \n")
      }
      spacetracker$tally(11)  # Tally "Jail" if the player remains
      return
    }
  }
  # Check for consecutive doubles
  if (dice_rolls[1] == dice_rolls[2]) {
    player$doubles_count <- player$doubles_count + 1
    if (!just_released) {
      if (player$verbose) {
        cat("Doubles count is now ", player$doubles_count, ". \n", sep = "")
      }
    }
    if (player$doubles_count >= 3 && player$jail_turns == 0) { # Land on Jail if three consecutive doubles
      player$go_to_jail()
      spacetracker$tally(player$pos) # Tally Jail space
      return # End turn
    }
  } else {
    player$doubles_count <- 0 # Reset doubles_count if not double
  }
  
  if(!just_released && player$jail_turns == 0) {
    if(player$verbose) {
      cat("Player starts at ", player$pos,  ": ", gameboard$title[player$pos], ".\n", sep = "")
    }
    player$move_fwd(sum(dice_rolls))
    if (player$pos != 31) {
      spacetracker$tally(player$pos)
    }
  }
  
  # Handling Chance Cards (pos at 8, 23, 37)
  utilities <- c(13, 29) # Electric Co and Water Works
  railroads <- c(6, 16, 26, 36) # Spaces for Railroads
  if (player$pos %in% c(8, 23, 37)) {
    if (player$verbose) {
      cat("Draw a Chance card. \n")
    }
    chance_card <- chance$draw()
    # If chance card moves player (1-9)
    if (chance_card %in% 1:9) {
      if (chance_card == 1) { # Advance to Go
        player$go_to_space(1)
        spacetracker$tally(player$pos)
      }
      if(chance_card == 2) { # Go to Illinois (25)
        player$go_to_space(25)
        spacetracker$tally(player$pos)
      }
      if (chance_card == 3) { # Advance to St. Charles Place (12)
        player$go_to_space(12)
        spacetracker$tally(player$pos)
      }
      if (chance_card == 4) { # Advance to nearest Utility (13 - Electric Co, 29 - Water Works)
        # find nearest uility
        distances <- sapply(utilities, function(utility_pos) {
          if (utility_pos > player$pos) {
            return(utility_pos - player$pos)  # If ahead, simple subtraction
          } else {
            return(40 - player$pos + utility_pos)  # If behind, wrap around the board
          }
        })
        nearest_utility_pos <- utilities[which.min(distances)]
        player$go_to_space(nearest_utility_pos)
        spacetracker$tally(player$pos)
      }
      if (chance_card == 5) { # Advance to nearest Railroad (6 - Reading, 16 - Penn, 26 - B & O, 36 - Short)
        # find nearest railroad
        distances <- sapply(railroads, function(railroad_pos) {
          if (railroad_pos > player$pos) {
            return(railroad_pos - player$pos)  # If ahead, simple subtraction
          } else {
            return(40 - player$pos + railroad_pos)  # If behind, wrap around the board
          }
        })
        nearest_railroad_pos <- railroads[which.min(distances)]
        player$go_to_space(nearest_railroad_pos)
        spacetracker$tally(player$pos)
      }
      if (chance_card == 6) { # Take a ride on Reading Railroad (6)
        player$go_to_space(6)
        spacetracker$tally(player$pos)
      }
      if (chance_card == 7) { # Take a walk on the Boardwalk (40)
        player$go_to_space(40)
        spacetracker$tally(player$pos)
      }
      if (chance_card == 8) { # Go to jail
        player$go_to_jail()
        spacetracker$tally(player$pos)
        continue_turn <- FALSE
      }
      if (chance_card == 9) { # Go back 3 spaces
        new_pos <- player$pos - 3
        player$pos <- ifelse(new_pos > 0, new_pos, new_pos + 40)
        player$go_to_space(player$pos)
        spacetracker$tally(player$pos)
      }
    }
  }
  # Handling Community Chest Cards (pos at 3, 18, 34)
  if (player$pos %in% c(3, 18, 34)) {
    if (player$verbose) {
      cat("Draw a Community Chest Card. \n")
    }
    community_card <- community$draw()
    # If community card that moves player
    if (community_card %in% 1:2) {
      if (community_card == 1) { # Advance to Go
        player$go_to_space(1)
        spacetracker$tally(player$pos)
      } else if (community_card == 2) { # Go to Jail
        player$go_to_jail()
        spacetracker$tally(player$pos)
        continue_turn <- FALSE
      }
    }
  }
  
  # Go to Jail space
  if (player$pos == 31) { # Land on 31(Go to Jail)
    player$go_to_jail()
    spacetracker$tally(player$pos)
    continue_turn <- FALSE
    return
  }
  
  # Recursive take_turn if doubles rolled and not sent to jail
  if (dice_rolls[1] == dice_rolls[2] && !just_released && player$jail_turns == 0) {
    if (player$verbose) {
      cat("\nPlayer rolled doubles, so they take another turn. \n")
    }
    take_turn(player, spacetracker)
  }
}

