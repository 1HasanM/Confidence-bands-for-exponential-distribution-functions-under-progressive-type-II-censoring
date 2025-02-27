library(ggplot2)
x = c(0.19, 0.78, 0.96, 1.31, 2.78, 4.85, 6.50, 7.35)
r = c(0, 0, 3, 0, 3, 0, 0, 5)
m = length(r)
n = sum(r) + m
j1 = 2:m
j2 = 1:(m - 1)
gam = NULL
for (j in 1:m) {
  gam[j] = sum(r[j:m] + 1)
}
muhat = min(x)
sigmahat = 1/m * sum(gam[2:m] * (x[j1] - x[j2]))
p = 0.05
q1 = (1 - sqrt(1 - p))/2
q2 = 1 - q1
s_q1 = 2 * m * sigmahat / qchisq(q1, 2 * m - 2)
s_q2 = 2 * m * sigmahat / qchisq(q2, 2 * m - 2)
sigmaset = c(s_q2, s_q1)
data = NULL
for (s in seq(s_q2, s_q1, 0.1)) {
  mu_alt = muhat + s * log(q1) / n
  mu_ust = muhat + s * log(q2) / n  
  data = rbind(data, c(s, mu_alt, mu_ust))
}
lines_data <- data.frame(
  mu = rep(data[, 2], each = 2),
  sigma = rep(data[, 1], each = 2),
  mu2 = rep(data[, 3], each = 2),
  sigma2 = rep(data[, 1], each = 2)
)
ggplot() +
  geom_segment(data = lines_data,
               aes(x = mu, y = sigma, xend = mu2, yend = sigma2), color = "red") +
  geom_point(aes(x = muhat, y = sigmahat), size = 3, color = "blue") +
  xlim(-10, 5) +
  ylim(0, 30) +
  theme_minimal()  


