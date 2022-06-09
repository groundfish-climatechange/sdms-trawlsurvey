# sdmTMB intro vignette
library(ggplot2)
library(dplyr)
library(sdmTMB)

glimpse(pcod)

mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 10)
plot(mesh)
mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 5)
plot(mesh) 

head(pcod$depth)

m <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh, # can be omitted for a non-spatial model
  family = binomial(link = "logit"),
  spatial = "off"
)
predictions0 <- predict(m, newdata = qcs_grid)
plot_map(predictions0,"est")+scale_fill_viridis()


m
AIC(m)

m0 <- glm(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  family = binomial(link = "logit")
)
summary(m0)


m1 <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh,
  family = binomial(link = "logit"),
  spatial = "on"
)
m1
AIC(m1)


m2 <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh,
  family = binomial(link = "logit"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)
m2

m3 <- sdmTMB(
  data = pcod,
  formula = density ~ depth_scaled + depth_scaled2,
  mesh = mesh,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)
m3

tidy(m3, conf.int = TRUE)


pcod$resids <- residuals(m3) # randomized quantile residuals
qqnorm(pcod$resids)
qqline(pcod$resids)


####
glimpse(qcs_grid)
predictions <- predict(m3, newdata = qcs_grid)

plot_map <- function(dat, column) {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    geom_raster() +
    coord_fixed()
}

plot_map(predictions, "exp(est)") +
  scale_fill_viridis_c(
    trans = "sqrt",
    # trim extreme high values to make spatial variation more visible
    na.value = "yellow", limits = c(0, quantile(exp(predictions$est), 0.995))
  ) +
  facet_wrap(~year) +
  ggtitle("Prediction (fixed effects + all random effects)",
          subtitle = paste("maximum estimated biomass density =", round(max(exp(predictions$est))))
  )

plot_map(predictions, "exp(est_non_rf)") +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Prediction (fixed effects only)")

plot_map(predictions, "omega_s") +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")


nd <- data.frame(
  depth_scaled = seq(min(pcod$depth_scaled) + 0.2,
                     max(pcod$depth_scaled) - 0.2,
                     length.out = 100
  ),
  year = 2009L # a chosen year
)
nd$depth_scaled2 <- nd$depth_scaled^2
p <- predict(m3, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(depth_scaled, exp(est),
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se)
)) +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  scale_x_continuous(labels = function(x) round(exp(x * pcod$depth_sd[1] + pcod$depth_mean[1]))) +
  coord_cartesian(expand = F) +
  labs(x = "Depth (m)", y = "Biomass density (kg/km2)")
