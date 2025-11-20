# Data curation re-use tradeoff model

library('tidyverse')

# Number of minutes needed for dataset standardization (submission to re-use),
# discovery, and publication to the repository
t_stdize_min <- 60
t_disc_min <- 0
t_pub_min <- 0

# Data state at t0 (contributor, 0-1)
dstate_t0 <- 0

# Data state at t1 (repository, 0-1)

df <- data.frame(dstate_t1 = seq(0, 1, 0.01))

# Data state at t2 (re-use, 0-1)
dstate_t2 <- 1

# Repository efficiency multiplier (percent of N_min_std saved by repository, 0-1)
repo_eff_mult <- 0.0

# Effort calculations

df['e_curator'] <- ((t_stdize_min * (1-repo_eff_mult)) * df$dstate_t1) - 
  (t_stdize_min * dstate_t0) + t_pub_min
df['e_user'] <- (t_stdize_min * dstate_t2) - (t_stdize_min * df$dstate_t1) +
  t_disc_min + (t_disc_min * (1 - df$dstate_t1))

g <- ggplot(data=df, aes(x=dstate_t1)) +
  geom_point(aes(y=e_curator, color='Data curator (IM)')) +
  geom_point(aes(y=e_user, color='Data user')) +
  ylab('Effort (minutes)') + xlab('FAIR status (analysis-ready -> )') +
  guides(colour = guide_legend("Person")) +
  ggtitle('Unassisted')
g
ggsave('~/Downloads/dc_du_tradeoff_noassist.png', width = 5.5, height = 4)

