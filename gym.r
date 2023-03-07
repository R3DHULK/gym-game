# Define gym parameters
max_members <- 50
gym_capacity <- 30
equipment <- c("Treadmill", "Stationary bike", "Elliptical machine", "Weightlifting bench")
equipment_count <- length(equipment)
cost_per_month <- 50
revenue_per_member <- 30

# Initialize gym state
members <- 0
equipment_status <- rep(0, equipment_count)
month <- 1
balance <- 0

# Define helper function to print gym status
print_gym_status <- function() {
  cat("\n=========================\n")
  cat(paste("Month:", month, "\n"))
  cat(paste("Members:", members, "/", max_members, "\n"))
  cat(paste("Equipment status:", paste(equipment_status, collapse = ","), "\n"))
  cat(paste("Balance:", balance, "\n"))
  cat("=========================\n\n")
}

# Define helper function to simulate a new month
simulate_month <- function() {
  # Calculate revenue and expenses
  revenue <- members * revenue_per_member
  expenses <- cost_per_month + equipment_count * 10
  
  # Update balance
  balance <<- balance + revenue - expenses
  
  # Update member count
  new_members <- sample(0:gym_capacity, 1)
  members <<- min(members + new_members, max_members)
  
  # Update equipment status
  equipment_status <<- pmax(0, equipment_status - sample(0:1, equipment_count, replace = TRUE))
  new_equipment <- sample(0:1, equipment_count, replace = TRUE)
  equipment_status <<- pmin(1, equipment_status + new_equipment)
  
  # Increment month counter
  month <<- month + 1
}

# Main game loop
while (members < max_members) {
  # Print gym status
  print_gym_status()
  
  # Ask player for action
  cat("What would you like to do?\n")
  cat("1. Buy new equipment\n")
  cat("2. Advertise the gym\n")
  cat("3. Do nothing\n")
  choice <- as.integer(readline(prompt = "Enter your choice: "))
  
  # Perform the chosen action
  if (choice == 1) {
    # Buy new equipment
    if (balance >= 100) {
      new_equipment <- sample(0:1, equipment_count, replace = TRUE)
      equipment_status <<- pmin(1, equipment_status + new_equipment)
      balance <<- balance - 100
    } else {
      cat("Not enough money to buy new equipment.\n")
    }
  } else if (choice == 2) {
    # Advertise the gym
    if (balance >= 50) {
      members_gained <- sample(0:10, 1)
      members <<- min(members + members_gained, max_members)
      balance <<- balance - 50
      cat(paste("Ad campaign successful! Gained", members_gained, "new members.\n"))
    } else {
      cat("Not enough money to advertise the gym.\n")
    }
  } else {
    # Do nothing
    cat("You decide to take a break this month.\n")
  }
  
  # Simulate a new month
  simulate_month()
}

# Print final gym status
print_gym_status()
cat("Congratulations! You have reached maximum capacity for the gym.\n")
