# Species Overlap Metrics

Overlap metrics based on Carroll et al. (2019). These take the ensemble prediction outputs for two different species, produced by the `ensemble_predictions()` function above. The global index of collocation also requires the center of gravity calculations from `get_cog(what='df')`.

## Schoener's D

#Schoener's D
schoeners <- function(dat1, dat2) {
  p1 <- dat1$ens_est %>% exp()
  p2 <- dat2$ens_est %>% exp()
  p_p1 <- p1/sum(p1, na.rm = T)
  p_p2 <- p2/sum(p2, na.rm = T)
  1 - 0.5 * (sum(abs(p_p1-p_p2), na.rm = T))
}


## Local Index of Collocation

lic <- function(dat1,dat2) {
  p1 <- dat1$ens_est %>% exp()
  p2 <- dat2$ens_est %>% exp()
  p_p1 <- p1/sum(p1, na.rm = T)
  p_p2 <- p2/sum(p2, na.rm = T)
  sum(p_p1*p_p2, na.rm = T)/(sqrt(sum(p_p1^2, na.rm = T)*sum(p_p2^2, na.rm = T)))
}

# if you want the metric to be spatial (grid-scale)
lic_grid <- function(dat1,dat2) {
  p1 <- dat1$ens_est %>% exp()
  p2 <- dat2$ens_est %>% exp()
  p_p1 <- p1/sum(p1, na.rm = T)
  p_p2 <- p2/sum(p2, na.rm = T)
  lic_vec <- (p_p1*p_p2)/(sqrt(sum(p_p1^2)*sum(p_p2^2)))
  lic_vec
}