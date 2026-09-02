#
rm(list = ls()); cat("\014"); graphics.off(); set.seed(2026)

#
library(metafor)
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)

#
nrep <- 1:50
cnames <- c("i", "nt", "n", "ns",
            "b", 
            "bGLMM", "bGLMMp",
            "bMeta", "bMetap",
            "singular"
)
dd <- data.frame(matrix(NA, ncol = length(cnames)))
colnames(dd) <- cnames

#
nv <- 30
nsv <- seq(5, 30, 5)
b <- .2

#
ii <- 1
for (n in nv) {
  for (ns in nsv) {
    cat(paste0("Doing: \n n: ", n, "; ns: ", ns, ";\n"))
    for (i in nrep) {
      d <- data.frame(X = numeric(), Y = numeric(), S = factor())
      
      tryCatch({
        
        #
        sd_int <- 0.5
        sd_slo <- 0.2
        
        for (nn in 1:ns) {
          set.seed((i * 1000) + (ns * 100) + nn) 
          
          #
          r_int <- rnorm(1, 0, sd_int)
          r_slo <- rnorm(1, 0, sd_slo)
          
          X <- rnorm(n)
          
          #
          Y <- (b + r_slo) * X + r_int + rnorm(n)
          
          S <- nn
          dp <- data.frame(X, Y, S)
          d <- rbind(d, dp)
        }
        
        #
        f1 <- lmer(Y ~ X + (1 + X|S), data = d)
        s1 <- summary(f1)
        bGLMM <- s1$coefficients["X", "Estimate"]
        bGLMMp <- s1$coefficients["X", "Pr(>|t|)"]
        singular <- isSingular(f1)
        
        #
        effs <- data.frame(coeff = numeric(), secoeff = numeric(), s = factor())
        for (s_id in unique(d$S)) {
          dp1 <- d[d$S == s_id, ]
          f_lm <- lm(Y ~ X, data = dp1)
          ss_lm <- summary(f_lm)
          coeff <- ss_lm$coefficients["X", "Estimate"]
          secoeff <- ss_lm$coefficients["X", "Std. Error"]
          effs <- rbind(effs, data.frame(coeff, secoeff, s = s_id))
        }
        
        #
        f2 <- rma(yi = coeff, vi = secoeff^2, data = effs, method = "REML")
        bMeta <- f2$beta[1]
        bMetap <- f2$pval[1]
        
        nt <- n * ns
        dd[ii, ] <- c(i, nt, n, ns,
                      b,
                      bGLMM, bGLMMp,
                      bMeta, bMetap,
                      singular)
      }, error = function(e) {
        dd[ii, ] <<- c(i, n * ns, n, ns,
                       b,
                       rep(NA, 5))
      })
      ii <- ii + 1
    }
  }
}

#
ddd <- dd %>%
  group_by(ns) %>%
  select(-i) %>%
  mutate(powerGLMM = ifelse(bGLMMp < .05, 1, 0),
         powerMeta = ifelse(bMetap < .05, 1, 0)) %>%
  reframe(across(where(is.numeric), ~mean(.x, na.rm = T))) %>%
  as.data.frame()
# print(ddd)

#
p1 <- ggplot(ddd, aes(x = ns)) +
  geom_line(aes(y = bGLMM, color = "GLMM"), linewidth = 1) +
  geom_line(aes(y = bMeta, color = "Meta-analysis"), linewidth = 1) +
  geom_point(aes(y = bGLMM, color = "GLMM"), size = 3) +
  geom_point(aes(y = bMeta, color = "Two-Stage Meta"), size = 3, shape = 17) +
  labs(
    title = "Parameter Recovery: GLMM vs Meta-Analysis",
    subtitle = "Comparing slope estimates across different numbers of clusters (ns)",
    x = "Number of Clusters (ns)",
    y = "Estimated Slope (b)",
    color = "Method"
  ) +
  scale_color_manual(values = c("GLMM" = "#0072B2", "Two-Stage Meta" = "#D55E00")) +
  geom_hline(yintercept = b, color = "black", linetype = "dashed") +
  theme_bw()
print(p1)

#
p2 <- ggplot(ddd, aes(x = ns)) +
  geom_line(aes(y = powerGLMM, color = "GLMM"), linewidth = 1) +
  geom_line(aes(y = powerMeta, color = "Two-Stage Meta"), linewidth = 1) +
  geom_point(aes(y = powerGLMM, color = "GLMM"), size = 3) +
  geom_point(aes(y = powerMeta, color = "Two-Stage Meta"), size = 3, shape = 17) +
  labs(
    title = "Statistical Power: GLMM vs Meta-Analysis",
    subtitle = "Power to detect the true slope (b = 0.1)",
    x = "Number of Clusters (ns)",
    y = "Power",
    color = "Method"
  ) +
  scale_color_manual(values = c("GLMM" = "#0072B2", "Two-Stage Meta" = "#D55E00")) +
  geom_hline(yintercept = .8, color = "black", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw()
print(p2)

#
p3 <- ggplot(ddd, aes(ns, singular)) +
  geom_point(size = 3, color = "#009E73") +
  geom_line(linewidth = 1, color = "#009E73") +
  labs(
    title = "GLMM Singularity Rate",
    subtitle = "Proportion of LMER models resulting in singular fit warnings",
    x = "Number of Clusters (ns)",
    y = "Singularity Rate"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw()
print(p3)