library(epiR)

# Fixed inputs
total_n <- 350000 # total sample size need to change for our study
ratio_m <- 6.81   # ratio of unexposed to exposed need to change for our study 

# P0 values from 5% to 50%
p0_range <- c(0.05, 0.15, 0.25, 0.41, 0.50)

# Function to calculate MDOR for each P0
calculate_mdor <- function(p0_val) {
  result <- epi.sscc(
    OR = NA,              
    p0 = p0_val,          
    n = total_n,          
    power = 0.80,         
    r = ratio_m,          
    method = "unmatched"
  )
  return(result$OR)
}

# Generate the results
cat("--- Power Analysis Results ---\n")
for(p in p0_range) {
  mdor_result <- calculate_mdor(p)
  cat(paste0("If P0 is ", p*100, "%, MDOR is: ", round(mdor_result, 3), "\n"))
}