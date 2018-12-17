# point click in circle sector 

# define functions
rad2deg <- function(rad) (rad * 180) / pi
deg2rad <- function(deg) (deg * pi) / 180
point_in_sector <- function(x, y, sectors) {
  # theta_pt <- atan(y / x)
  theta_pt <- ifelse(y >= 0,
                     atan2(y, x),
                     pi + (pi + atan2(y, x)))
  hypot_pt <- sqrt(x^2 + y^2)
  print(paste("point angle:", theta_pt, "point vector len:", hypot_pt))
  for (i in 1L:length(sectors)) {
    print(paste(sectors[[i]]$theta_1_rad, sectors[[i]]$theta_2_rad))
    if (sectors[[i]]$theta_1_rad < theta_pt && 
        theta_pt < sectors[[i]]$theta_2_rad && 
        hypot_pt <= 1)
      return(i)
  }
  return(integer(0))
}

point_in_sector_2 <- function(x, y, sector_vec) {
  
  sector_vec_sum <- sum(sector_vec)
  sector_vec_cumsum <- cumsum(sector_vec)
  sectors <- list()
  theta_1_rad <- 0.00
  theta_2_rad <- 0.00
  
  for (i in seq_along(sector_vec)) {
    sectors[[i]] <- list(
      theta_1_rad = ifelse(i > 1L, sectors[[i-1L]]$theta_2_rad, 0),
      theta_2_rad = sector_vec_cumsum[i] / sector_vec_sum * 2 * pi
    )
  }
  
  pt_theta <- ifelse(y >= 0, atan2(y, x), pi + (pi + atan2(y, x)))
  pt_hypot <- sqrt(x^2 + y^2)
  
  for (i in seq_along(sectors)) {
    if (sectors[[i]]$theta_1_rad < pt_theta && 
        pt_theta < sectors[[i]]$theta_2_rad && 
        pt_hypot <= 1)
      return(i)
  }
  return(integer(0))
  
}

# input
sector_input <- c(1, 2, 5)
sector_input_sums <- cumsum(sector_input)
sector_input_sum <- sum(sector_input)

sectors <- list()
theta_1_rad <- 0.00
theta_2_rad <- 0.00
theta_1_deg <- 0.00
theta_2_deg <- 0.00

for (i in seq_along(sector_input)) {
  print(i)
  sector_sum_curr <- 0
  # for (s in 1:i) {
  #   sector_sum_curr <- sector_sum_curr + sector_input[s]
  # }
  sectors[[i]] <- list(
    theta_1_rad = ifelse(i > 1L, sectors[[i-1]]$theta_2_rad, 0),
    # theta_2_rad = sector_sum_curr / sector_input_sum * 2 * pi,
    theta_2_rad = sector_input_sums[i] / sector_input_sum * 2 * pi,
    theta_1_deg = rad2deg(
      ifelse(i > 1L, sectors[[i-1]]$theta_2_rad, 0)
    ),
    theta_2_deg = rad2deg(
      # sector_sum_curr / sector_input_sum * 2 * pi
      sector_input_sums[i] / sector_input_sum * 2 * pi
    )
  )
}

point_in_sector(0.5, 0.25, sectors) # expect 1
point_in_sector(1, 1, sectors) # expect integer(0)
point_in_sector(-0.25, 0.5, sectors) # expect 2
point_in_sector(-0.5, 0.0, sectors) # expect 3
point_in_sector(1, 0, sectors) # returns integer(0)
point_in_sector(0.5, 0.5, sectors) # returns integer(0)
point_in_sector(-0.5, -0.25, sectors) # expect 3

point_in_sector_2(0.5, 0.25, c(1, 2, 5))
point_in_sector_2(0, 0.99, c(1, 2, 5))
point_in_sector_2(-0.5, 0.25, c(1, 2, 5))


