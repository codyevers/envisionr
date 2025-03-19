simulate_fire_area <- function(years = 50, fire_prob = 0.3, small_fire_mean = 100, small_fire_sd = 50, large_fire_scale = 500) {
  # Initialize a vector to store fire areas
  fire_area <- numeric(years)

  # Loop over the years
  for (i in 1:years) {
    # Determine if a fire occurs (30% chance)
    if (runif(1) < fire_prob) {
      # 90% of the time, generate a small fire; 10% of the time, generate a large fire
      if (runif(1) < 0.9) {
        # Small fire: use a normal distribution to simulate fire size
        fire_area[i] <- rnorm(1, mean = small_fire_mean, sd = small_fire_sd)
      } else {
        # Large fire: use an exponential distribution to simulate larger, rarer fires
        fire_area[i] <- rexp(1, rate = 1/large_fire_scale)
      }
    } else {
      # No fire this year
      fire_area[i] <- 0
    }
  }

  # Ensure no negative fire areas (if using normal distribution)
  fire_area <- pmax(fire_area, 0)

  return(fire_area)
}

# Example usage
set.seed(123)  # Set seed for reproducibility
fire_areas <- simulate_fire_area()

# Print the result
fire_areas
plot(fire_areas)


# Define the decay function
apply_decay <- function(fire_areas, years = 50, decay_half_life = 5) {

  # Calculate the decay constant k
  decay_constant <- log(2) / decay_half_life

  # Initialize a vector to store the adjusted fire areas after decay
  adjusted_areas <- numeric(years)

  # Loop over the years and apply the decay
  for (i in 1:years) {
    # Cumulative sum of the past fires, applying decay for each past year
    for (j in 1:i) {
      time_since_fire <- i - j
      adjusted_areas[i] <- adjusted_areas[i] + fire_areas[j] * exp(-decay_constant * time_since_fire)
    }
  }

  return(adjusted_areas)
}

# Generate the fire areas using the previous function
set.seed(123)
fire_areas <- simulate_fire_area()

# Apply the decay to the fire areas
adjusted_fire_areas <- apply_decay(fire_areas)

# Print the original and adjusted fire areas
data.frame(Year = 1:50, Original_Area = fire_areas, Adjusted_Area = adjusted_fire_areas)

plot(adjusted_fire_areas, type='l')
points(fire_areas, pch=16, type='h', col='red', lwd=2)
