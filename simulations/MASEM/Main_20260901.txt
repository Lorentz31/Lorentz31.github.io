#
rm(list = ls()); cat("\014"); graphics.off(); set.seed(2026); options(max.print = 1e3, width = 1e3)

#
library(lavaan)
library(metaSEM)
library(openxlsx)
library(dplyr)
library(foreach)
library(doParallel)
library(doRNG)
library(ggplot2)

#
Root <- getwd()
for (f in c("Outputs", "Scripts")) {dir.create(paste0(Root, "/", f, "/")); assign(paste0(f, "Folder"), paste0(Root, "/", f, "/"))}

#
NIR <- 3:12
N <- seq(50, 300, 50)
NS1 <- seq(20, 100, 20)
TAU <- seq(.1, .6, .2)
REP <- 1:10

### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###
NIR <- 6
N <- seq(50, 100, 50)
NS1 <- c(3, 6)
TAU <- c(.05)
REP <- 1:1e1
### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###### TEST ONLY ###

#
nCores <- detectCores() - 4
registerDoParallel(cores = nCores)
t1 <- Sys.time()
grid <- expand.grid(NIR = NIR, N = N, NS1 = NS1, TAU = TAU)

#
x <- 
  foreach(i = 1:nrow(grid)) %dopar% {
    foreach(rep = REP) %do% {
      
      #
      set.seed((i * 1000) + rep)
      
      nir <- grid[i, "NIR"]
      n <- grid[i, "N"]
      ns1 <- grid[i, "NS1"]
      tau <- grid[i, "TAU"]
      
      nt <- n * ns1
      
      tryCatch({
        #
        lf <- rnorm(n)
        lvec <- sample(40:80, nir, F)/100
        
        #
        d <- data.frame(lapply(lvec, function(l) l * lf + rnorm(n, 0, sqrt(1 - l^2))))
        colnames(d) <- paste0("i", 1:nir)
        
        model <- paste0("lf =~ ", paste0(grep("i\\d", colnames(d), value = T), collapse = " + "))
        
        ct <- cor(d)
        nCorrs <- nir * (nir - 1) / 2
        ctV <- diag(tau^2, nCorrs)
        cts <- rCor(Sigma = ct, V = ctV, n = rep(n, ns1), raw.data = T)$R
        
        fs1 <- tssem1(cts, rep(n, ns1), "REM", RE.type = "Diag")
        stssem1 <- summary(fs1)
        
        #
        RAM0 <- lavaan2RAM(model, obs.variables = colnames(d), std.lv = TRUE)
        
        fs2 <- tssem2(fs1, RAM0, diag.constraints = T, intervals.type = "z")
        stssem <- summary(fs2)
        
        dd <- Cor2DataFrame(cts, rep(n, ns1))
        fosm <- osmasem("Null", RAM0, data = dd, RE.type = "Diag", intervals.type = "z")
        sosmasem <- summary(fosm, fitIndices = T)
        
        #
        tssem_tau2 <- pmax(0, stssem1$coefficients[grep("Tau2", rownames(stssem1$coefficients)), "Estimate"])
        osma_tau2  <- pmax(0, exp(sosmasem$parameters[grep("vecTau1", sosmasem$parameters$matrix), "Estimate"]))
        
        #
        item_names <- paste0("i", 1:nir, "ONlf")
        
        TSSEM_EstLoadings  <- stssem$coefficients[item_names, "Estimate"]
        TSSEM_EstLoadingsP <- stssem$coefficients[item_names, "Pr(>|z|)"]
        
        idx_osma <- match(item_names, sosmasem$parameters$name)
        OSMASEM_EstLoadings  <- sosmasem$parameters$Estimate[idx_osma]
        OSMASEM_EstLoadingsP <- sosmasem$parameters$`Pr(>|z|)`[idx_osma]
        
        df1 <- data.frame(
          i = rep,
          TotalSampleSize = nt,
          nStudies = ns1,
          MeanSampleSize = n,
          nItem = nir,
          Tau = tau,
          OrLoadings = lvec,
          
          isPd = as.numeric(sum(unlist(lapply(cts, function(x) is.pd(x)))) == ns1),
          
          TSSEM_meanTau = mean(atanh(sqrt(tssem_tau2)), na.rm = TRUE),
          TSSEM_sdTau = sd(atanh(sqrt(tssem_tau2)), na.rm = TRUE),
          
          TSSEM_EstLoadings = TSSEM_EstLoadings,
          TSSEM_EstLoadingsP = TSSEM_EstLoadingsP,
          TSSEM_CHI2 = stssem$stat["Chi-square of target model",],
          TSSEM_DF = stssem$stat["DF of target model",],
          TSSEM_RMSEA = stssem$stat["RMSEA",],
          TSSEM_SRMR = stssem$stat["SRMR",],
          TSSEM_CFI = stssem$stat["CFI",],
          TSSEM_TLI = stssem$stat["TLI",],
          TSSEM_AIC = stssem$stat["AIC",],
          TSSEM_BIC = stssem$stat["BIC",],
          
          OSMASEM_meanTau = mean(atanh(sqrt(osma_tau2)), na.rm = TRUE),
          OSMASEM_sdTau = sd(atanh(sqrt(osma_tau2)), na.rm = TRUE),
          
          OSMASEM_EstLoadings = OSMASEM_EstLoadings,
          OSMASEM_EstLoadingsP = OSMASEM_EstLoadingsP,
          OSMASEM_CHI2 = sosmasem$Chi,
          OSMASEM_DF = sosmasem$ChiDoF,
          OSMASEM_RMSEA = sosmasem$RMSEA,
          OSMASEM_SRMR = osmasemSRMR(fosm),
          OSMASEM_CFI = sosmasem$CFI,
          OSMASEM_TLI = sosmasem$TLI,
          OSMASEM_AIC = sosmasem$AIC,
          OSMASEM_BIC = sosmasem$BIC,
          
          stringsAsFactors = FALSE
        )
        
      }, error = function(e7) {
        df1 <<- data.frame(
          i = rep,
          TotalSampleSize = nt,
          nStudies = ns1,
          MeanSampleSize = n,
          nItem = nir,
          Tau = tau,
          OrLoadings = lvec,
          
          isPd = NA,
          
          TSSEM_meanTau = NA,
          TSSEM_sdTau = NA,
          
          TSSEM_EstLoadings = NA,
          TSSEM_EstLoadingsP = NA,
          TSSEM_CHI2 = NA,
          TSSEM_DF = NA,
          TSSEM_RMSEA = NA,
          TSSEM_SRMR = NA,
          TSSEM_CFI = NA,
          TSSEM_TLI = NA,
          TSSEM_AIC = NA,
          TSSEM_BIC = NA,
          
          OSMASEM_meanTau = NA,
          OSMASEM_sdTau = NA,
          OSMASEM_EstLoadings = NA,
          OSMASEM_EstLoadingsP = NA,
          OSMASEM_CHI2 = NA,
          OSMASEM_DF = NA,
          OSMASEM_RMSEA = NA,
          OSMASEM_SRMR = NA,
          OSMASEM_CFI = NA,
          OSMASEM_TLI = NA,
          OSMASEM_AIC = NA,
          OSMASEM_BIC = NA,
          
          stringsAsFactors = FALSE
        )
      })
      return(df1)
    }
  }
difftime(Sys.time(), t1, units = "mins")
stopImplicitCluster()

#
extract <- function(lst) {
  if (is.data.frame(lst)) return(list(lst))
  if (is.list(lst)) return(unlist(lapply(lst, extract), recursive = FALSE))
  return(NULL)
}
df1 <- bind_rows(extract(x))
write.xlsx(df1, paste0(OutputsFolder, "SimulationMASEM_Raw_", gsub(":", " ", format(Sys.time(), "%a %b %d %X %Y")), ".xlsx"), rowNames = FALSE)

#
CFI_cut <- .90
TLI_cut <- .90
RMSEA_cut <- .08
SRMR_cut <- .08

df2 <- df1 %>%
  dplyr::select(-i) %>%
  dplyr::group_by(nItem, MeanSampleSize, nStudies, Tau) %>%
  
  dplyr::mutate(
    TSSEM_Conv = !is.na(TSSEM_EstLoadings),
    OSMASEM_Conv = !is.na(OSMASEM_EstLoadings),
    
    TSSEM_Fit = TSSEM_Conv & (TSSEM_CFI > CFI_cut & TSSEM_TLI > TLI_cut & TSSEM_RMSEA < RMSEA_cut & TSSEM_SRMR < SRMR_cut),
    OSMASEM_Fit = OSMASEM_Conv & (OSMASEM_CFI > CFI_cut & OSMASEM_TLI > TLI_cut & OSMASEM_RMSEA < RMSEA_cut & OSMASEM_SRMR < SRMR_cut),
    
    #
    TSSEM_RawPower = as.numeric(TSSEM_Conv & TSSEM_EstLoadingsP < .05),
    OSMASEM_RawPower = as.numeric(OSMASEM_Conv & OSMASEM_EstLoadingsP < .05)
  ) %>%
  
  dplyr::reframe(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    
    TSSEM_Conv_Rate = mean(TSSEM_Conv),
    OSMASEM_Conv_Rate = mean(OSMASEM_Conv),
    
    TSSEM_Fit_Rate = mean(TSSEM_Fit, na.rm = TRUE),
    OSMASEM_Fit_Rate = mean(OSMASEM_Fit, na.rm = TRUE),
    
    TSSEM_Bias = mean((TSSEM_EstLoadings - OrLoadings) / OrLoadings, na.rm = TRUE),
    OSMASEM_Bias = mean((OSMASEM_EstLoadings - OrLoadings) / OrLoadings, na.rm = TRUE),
    
    #
    TSSEM_Power = mean(TSSEM_RawPower, na.rm = TRUE),
    OSMASEM_Power = mean(OSMASEM_RawPower, na.rm = TRUE)
  )
write.xlsx(df2, paste0(OutputsFolder, "SimulationMASEM_PowerComputed_", gsub(":", " ", format(Sys.time(), "%a %b %d %X %Y")), ".xlsx"), rowNames = FALSE)

#
df_plot <- df2 %>%
  dplyr::select(nItem, MeanSampleSize, nStudies, Tau, TSSEM_Power, OSMASEM_Power) %>%
  tidyr::pivot_longer(
    cols = c(TSSEM_Power, OSMASEM_Power),
    names_to = "Method",
    values_to = "Power"
  ) %>%
  dplyr::mutate(Method = sub("_Power", "", Method))


#
itemsChosen <- max(df_plot$nItem)

p1 <- ggplot(df_plot %>% filter(nItem == itemsChosen), 
             aes(x = nStudies, y = Power, color = Method, linetype = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red", alpha = 0.7) +
  facet_grid(Tau ~ MeanSampleSize, labeller = label_both) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = paste("Power Curves by Number of Studies and Sample Size (Items =", itemsChosen, ")"),
    x = "Number of Studies (K)",
    y = "Statistical Power",
    subtitle = "Rows: Heterogeneity (Tau) | Columns: Mean Sample Size (N)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
print(p1)
ggsave(paste0(OutputsFolder, "p1.png"))

#
tauChosen <- min(df_plot$Tau)
nChosen <- max(df_plot$MeanSampleSize)

p2 <- ggplot(df_plot %>% filter(Tau == tauChosen & MeanSampleSize == nChosen), 
             aes(x = nItem, y = Power, color = Method, group = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red", alpha = 0.7) +
  facet_wrap(~ nStudies, labeller = label_both) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = unique(df_plot$nItem)) +
  labs(
    title = paste("Power vs Model Complexity (Tau =", tauChosen, "| N =", nChosen, ")"),
    x = "Number of Items",
    y = "Statistical Power",
    subtitle = "Panels: Number of Studies (K)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
print(p2)
ggsave(paste0(OutputsFolder, "p2.png"))