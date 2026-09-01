#
rm(list = ls()); cat("\014"); graphics.off(); set.seed(2025)

#
library(lavaan)
library(tidyLPA)
library(cluster)
library(dplyr)
library(ggplot2)
library(tidyr)

#
n <- 2e2
lf <- rnorm(n)
l <- .5
ni <- 1:10
for (i in ni) {
  assign(paste0("i", i), (l * lf + rnorm(n)))
}
d <- do.call(cbind, mget(ls(pattern = "i\\d")))
d <- data.frame(scale(d))

#
model <- paste0("lf =~ ", paste0(grep("i\\d", names(d), value = T), collapse = " + "))
f <- cfa(model, d, std.lv = T)
s <- summary(f, std = T)
s$pe[s$pe$op == "=~", "std.all"]

#
# tt <- estimate_profiles(d, 1:3)

########################################################################

#
rm(list = ls()); cat("\014"); graphics.off(); set.seed(2025)

#
Root <- getwd()
for (f in c("Outputs", "Scripts")) {dir.create(paste0(Root, "/", f, "/")); assign(paste0(f, "Folder"), paste0(Root, "/", f, "/"))}

#
library(lavaan)
library(tidyLPA)
library(cluster)
library(dplyr)
library(ggplot2)
library(tidyr)

#
nrep <- 1:3
n <- 120*10
g <- as.factor(rep(c(0, 1), each = n/2))
cdv <- seq(.2, .8, .2)
cdv <- seq(.05, .1, .05)
cdv <- c(.2)
ngv <- c(2, 4, 6)
ngv <- c(2)

### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###
cdv <- c(1, 2)
ngv <- c(3)
### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###

#
cnames <- c("ii", 
            "cd", "ng",
            "cd_Est", "LPA", "CL")
dd <- data.frame(matrix(NA, ncol = length(cnames)))
names(dd) <- cnames
ii <- 1
t1 <- Sys.time()
for (ng in ngv) {
  for (cd in cdv) {
    cat(paste0("Completion: ", round(ii / (max(nrep)*length(cdv)*length(ngv))*100, 3), "%; \n"))
    cat(paste0("Time elapsed: ", round(difftime(Sys.time(), t1, units = "mins"), 3), " s; \n"))
    # cat(paste0("Doing: \n > ng: ", ng, "; > cd: ", cd, "; ", r, "/", max(nrep), "; \n"))
    cat(paste0("Doing: \n > ng: ", ng, "; > cd: ", cd, "; \n"))
    cat("########################################################\n")
    for (r in nrep) {
      tryCatch({
        #
        set.seed(r)
        
        #
        lf <- unlist(lapply(0:(ng-1), function(k) rnorm(n/ng, mean = k * cd)))
        
        g <- as.factor(rep(c(0:(ng-1)), each = n/ng))
        
        l <- .5
        ni <- 1:10
        for (i in ni) {
          assign(paste0("i", i), (l * lf + rnorm(n)))
        }
        d <- do.call(cbind, mget(ls(pattern = "i\\d")))
        d <- data.frame(scale(d))
        d$g <- g
        
        #
        model <- paste0("lf =~ ", paste0(grep("i\\d", names(d), value = T), collapse = " + "))
        f <- cfa(model, d, std.lv = T, 
                 group = "g", 
                 group.equal = c("loadings", "intercepts", "residuals", "lv.variances"))
        s <- summary(f, std = T)
        cd_Est <- s$pe[s$pe$lhs == "lf" & s$pe$op == "~1", "std.all"][[1]]
        
        #
        LPA <- tryCatch({
          tmp_LPA <- estimate_profiles(d[, grep("i\\d", names(d))], 1:max(ng), 6)
          tmp_LPA <- compare_solutions(tmp_LPA)$best
          tmp_LPA["LogLik"][[1]]
        }, error = function(e) {
          NA
        })
        
        #
        sil <- sapply(2:max(ng), function(k){
          km <- kmeans(d[, grep("i\\d", names(d))], centers = k)
          mean(silhouette(km$cluster, dist(d[, grep("i\\d", names(d))]))[, 3])
        })
        CL <- (2:max(ng))[which.max(sil)]
        
        dd[ii, ] <- c(ii, 
                      cd, ng,
                      cd_Est, LPA, CL)
      }, error = function(e) {
        dd[ii, ] <<- c(ii, cd, ng, rep(NA, ncol(dd)-3))
      })
      ii <- ii + 1
    }
  }
}
cat(paste0("Time elapsed: ", round(difftime(Sys.time(), t1, units = "mins"), 3), " s; \n"))

#
ddd <- dd %>%
  dplyr::select(-ii) %>%
  group_by(cd, ng) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = T)), .groups = "drop") %>%
  as.data.frame()
ddd

#
ddd$ng_factor <- as.factor(ddd$ng)

#
p1 <- ggplot(ddd, aes(x = cd, y = cd_Est, color = ng_factor, group = ng_factor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray", linewidth = 1) +
  labs(
    title = "CFA Parameter Recovery: Estimated vs True Effect",
    subtitle = "The dashed line represents perfect recovery (y = x)",
    x = "True distance between groups (cd)",
    y = "Distance estimated by MGCFA (cd_Est)",
    color = "N. of Groups (ng)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )
print(p1)
ggsave(paste0(OutputsFolder, "p1.png"))

#
ddd_long <- ddd %>%
  pivot_longer(
    cols = c(LPA, CL), 
    names_to = "Algorithm", 
    values_to = "Estimated_Groups"
  )

#
p2 <- ggplot(ddd_long, aes(x = cd, y = Estimated_Groups, color = Algorithm)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(aes(yintercept = ng), linetype = "dashed", color = "black", linewidth = 0.8) +
  facet_wrap(~ paste("True Groups (ng) =", ng), scales = "free_y") +
  scale_color_manual(values = c("LPA" = "#D55E00", "CL" = "#0072B2")) +
  labs(
    title = "Class Enumeration: LPA vs K-Means",
    subtitle = "The dashed black line indicates the correct target",
    x = "Distance between groups (cd)",
    y = "Average number of groups found",
    color = "Method"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 11)
  )
print(p2)
ggsave(paste0(OutputsFolder, "p2.png"))