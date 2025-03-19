###############################################################################
# 0) Install/Load Packages
###############################################################################
simulate_cv_tests <- function(A = 5,  # Number of replicates
                              num_simulations = 10000,
                              seed = 123) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("evir", quietly = TRUE)) install.packages("evir")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
  if (!requireNamespace("cvequality", quietly = TRUE)) install.packages("cvequality")
  if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
  
  library(data.table)
  library(evir)
  library(ggplot2)
  library(DT)
  library(cvequality)
  library(knitr)
  
  ###############################################################################
  # 1) Basic Setup
  ###############################################################################
  # We'll create 20 companies
  companies <- paste0("Company_", 1:20)
  Years     <- c(2025, 2026, 2027)
  
  # For reproducibility in picking unique combos for heterogeneous
  set.seed(seed)
  
  ###############################################################################
  # 2) Covariate Effects & Base Coefficients
  ###############################################################################
  covariate_effects <- list(
    Lambda = list(
      Location        = c(USA = 0.4, `Non-USA` = -0.4),
      Business_Sector = c(Finance = 0.8, Healthcare = 0, Other = -0.5),
      Revenue         = c(Small = -0.8, Medium = 0, Big = 0.8),
      Employee_Size   = c(Small = -0.4, Medium = 0, Big = 0.4),
      Year            = c(`2025`=0, `2026`=0.1, `2027`=0.2)
    ),
    Mu = list(
      Location        = c(USA = 0.3, `Non-USA` = -0.3),
      Business_Sector = c(Finance = 0.6, Healthcare = 0, Other = -0.4),
      Revenue         = c(Small = -0.7, Medium = 0, Big = 0.7),
      Employee_Size   = c(Small = -0.3, Medium = 0, Big = 0.3),
      Year            = c(`2025`=0, `2026`=0.3, `2027`=0.6)
    ),
    Tau = list(
      Location        = c(USA = -0.1, `Non-USA` = 0.1),
      Business_Sector = c(Finance = -0.05, Healthcare=0, Other=0.05),
      Revenue         = c(Small = -0.1, Medium = 0, Big = 0.2),
      Employee_Size   = c(Small = -0.05, Medium=0, Big=0.05),
      Year            = c(`2025`=0, `2026`=0.03, `2027`=0.1)
    )
  )
  
  loss_type_coefficients <- list(
    `Identity - Fraudulent Use/Account Access` = list(lambda = -6.0, mu = 15, tau = -0.225),
    `Data - Malicious Breach`                  = list(lambda = -6.0, mu = 15, tau = -0.225),
    `Network/Website Disruption`               = list(lambda = -6.0, mu = 15, tau = -0.225)
  )
  
  ###############################################################################
  # 3) Homogeneous Covariates (same for all 20 companies)
  ###############################################################################
  homogeneous_covariates <- data.table(
    Company         = companies,
    Location        = "USA",
    Business_Sector = "Finance",
    Revenue         = "Medium",
    Employee_Size   = "Medium"
  )
  
  ###############################################################################
  # 4) Create All Possible Covariate Combinations for Heterogeneous
  #    Then we will sample 20 *distinct* combos per replicate
  ###############################################################################
  # Possible levels:
  locs   <- c("USA", "Non-USA")
  secs   <- c("Finance", "Healthcare", "Other")
  reves  <- c("Small", "Medium", "Big")
  emps   <- c("Small", "Medium", "Big")
  
  all_combos <- as.data.table(expand.grid(Location = locs,
                                          Business_Sector = secs,
                                          Revenue         = reves,
                                          Employee_Size   = emps,
                                          stringsAsFactors = FALSE))
  # We have 2 x 3 x 3 x 3 = 54 total combos
  # We'll randomly sample 20 distinct combos from these 54 for each replicate
  
  ###############################################################################
  # 5) Parameter + Loss Simulation Functions
  ###############################################################################
  simulate_parameters <- function(covariates) {
    results <- data.table()
    
    for (i in 1:nrow(covariates)) {
      company         <- covariates[i, Company]
      location        <- covariates[i, Location]
      business_sector <- covariates[i, Business_Sector]
      revenue         <- covariates[i, Revenue]
      employee_size   <- covariates[i, Employee_Size]
      
      for (year in Years) {
        for (loss_type in names(loss_type_coefficients)) {
          base_coeffs <- loss_type_coefficients[[loss_type]]
          
          log_lambda <- base_coeffs$lambda +
            covariate_effects$Lambda$Location[location] +
            covariate_effects$Lambda$Business_Sector[business_sector] +
            covariate_effects$Lambda$Revenue[revenue] +
            covariate_effects$Lambda$Employee_Size[employee_size] +
            covariate_effects$Lambda$Year[as.character(year)]
          
          log_mu <- base_coeffs$mu +
            covariate_effects$Mu$Location[location] +
            covariate_effects$Mu$Business_Sector[business_sector] +
            covariate_effects$Mu$Revenue[revenue] +
            covariate_effects$Mu$Employee_Size[employee_size] +
            covariate_effects$Mu$Year[as.character(year)]
          
          log_tau <- base_coeffs$tau +
            covariate_effects$Tau$Location[location] +
            covariate_effects$Tau$Business_Sector[business_sector] +
            covariate_effects$Tau$Revenue[revenue] +
            covariate_effects$Tau$Employee_Size[employee_size] +
            covariate_effects$Tau$Year[as.character(year)]
          
          lambda <- exp(log_lambda)
          mu     <- exp(log_mu)
          tau    <- exp(log_tau)
          tau    <- ifelse(tau >= 1, 0.99, tau)  # cap shape below 1
          
          results <- rbind(
            results,
            data.table(
              Company   = company,
              Year      = year,
              Loss_Type = loss_type,
              Lambda    = lambda,
              Mu        = mu,
              Tau       = tau
            )
          )
        }
      }
    }
    return(results)
  }
  
  simulate_losses <- function(parameters, num_sims) {
    total_losses <- numeric(num_sims)
    unique_companies <- unique(parameters$Company)
    
    for (sim_i in seq_len(num_sims)) {
      sim_loss <- 0
      for (company in unique_companies) {
        sub_params <- parameters[Company == company]
        comp_total <- 0
        
        # For each combination of (Year, Loss_Type) => frequency + severity
        for (row_i in seq_len(nrow(sub_params))) {
          lambda <- sub_params$Lambda[row_i]
          mu     <- sub_params$Mu[row_i]
          tau    <- sub_params$Tau[row_i]
          
          # Poisson # events
          num_events <- rpois(1, lambda)
          if (num_events > 0) {
            event_losses <- rgpd(num_events, xi = tau, mu = 0, beta = mu)
            comp_total <- comp_total + sum(event_losses)
          }
        }
        sim_loss <- sim_loss + comp_total
      }
      total_losses[sim_i] <- sim_loss
    }
    
    return(total_losses)
  }
  
  # Helper for Coefficient of Variation
  calc_cv <- function(x) {
    mu_x <- mean(x)
    sd_x <- sd(x)
    if (mu_x == 0) {
      return(NA_real_)
    } else {
      return(sd_x / mu_x)
    }
  }
  
  ###############################################################################
  # 6) Loop over A Replicates
  ###############################################################################
  # We'll store replicate-level results (CVs, test p-values, etc.)
  results_list <- vector("list", A)
  
  for (rep_i in seq_len(A)) {
    # 6a) Build Heterogeneous Covariates with 20 unique combos
    #     Then assign to the same 20 companies
    chosen_combos <- all_combos[sample(.N, 20)]  # sample 20 distinct combos
    heterogeneous_covariates <- data.table(
      Company         = companies,
      Location        = chosen_combos$Location,
      Business_Sector = chosen_combos$Business_Sector,
      Revenue         = chosen_combos$Revenue,
      Employee_Size   = chosen_combos$Employee_Size
    )
    
    # 6b) Parameter generation
    homo_params <- simulate_parameters(homogeneous_covariates)
    hetero_params <- simulate_parameters(heterogeneous_covariates)
    
    # 6c) Loss simulation
    homo_losses <- simulate_losses(homo_params, num_sims = num_simulations)
    hetero_losses <- simulate_losses(hetero_params, num_sims = num_simulations)
    
    # 6d) Coefficients of Variation
    cv_homo   <- calc_cv(homo_losses)
    cv_hetero <- calc_cv(hetero_losses)
    
    # 6e) Statistical test for difference in CVs
    df_test <- data.frame(
      Group = c(rep("Heterogeneous", length(hetero_losses)),
                rep("Homogeneous",   length(homo_losses))),
      Loss  = c(hetero_losses, homo_losses)
    )
    cv_test_result <- asymptotic_test(x = df_test$Loss, y = df_test$Group)
    p_val <- cv_test_result$p_value
    
    # 6f) Store replicate results, plus the param tables if you like
    #     We can store param tables to examine them later if desired
    results_list[[rep_i]] <- list(
      replicate_id        = rep_i,
      homogeneous_cv      = cv_homo,
      heterogeneous_cv    = cv_hetero,
      p_value_cv_test     = p_val,
      # For deeper analysis, store parameter DataTables or the chosen combos
      # so we can see how each group was built:
      homogeneous_params  = homo_params,
      heterogeneous_params= hetero_params,
      heterogeneous_covs  = heterogeneous_covariates
    )
    
    cat("Replication", rep_i, "complete. CV_Homo=", cv_homo,
        " CV_Hetero=", cv_hetero, " p=", p_val, "\n")
  }
  
  ###############################################################################
  # 7) Summarize All Replicates
  ###############################################################################
  # Build a data.table with replicate-level metrics
  replicate_summary <- rbindlist(lapply(results_list, function(x) {
    data.table(
      Replicate           = x$replicate_id,
      CV_Homogeneous      = x$homogeneous_cv,
      CV_Heterogeneous    = x$heterogeneous_cv,
      P_Value_CVTest      = x$p_value_cv_test
    )
  }))
  
  replicate_summary[, Rejected := (P_Value_CVTest < 0.05)]
  
  # Print final replicate summary
  cat("\n--- Replicate-Level Results ---\n")
  print(replicate_summary)
  
  # Summaries
  total_reps    <- nrow(replicate_summary)
  reject_count  <- sum(replicate_summary$Rejected, na.rm=TRUE)
  fail_count    <- total_reps - reject_count
  
  summary_dt <- data.table(
    Metric = c("Number of Replications",
               "Number of Rejections (p<0.05)",
               "Number of Fails to Reject",
               "Proportion of Rejections"),
    Value  = c(total_reps,
               reject_count,
               fail_count,
               reject_count / total_reps)
  )
  
  cat("\n--- Summary of Replications ---\n")
  print(summary_dt)
  
  # Return everything (the replicate_summary plus the entire list if you need details)
  return(list(
    replicate_summary = replicate_summary,
    detailed_results  = results_list,
    final_summary     = summary_dt
  ))
}

results_out <- simulate_cv_tests(A = 20, num_simulations = 10000, seed = 123)

###############################################################################
# Example Usage
###############################################################################
# We run 5 replicates (A=5). Each replicate uses 10,000 simulations of total losses.
# We ensure the Heterogeneous portfolio has 20 unique covariate combos (no duplicates).
# The Homogeneous portfolio is the same across all 20 companies in each replicate.

# results_out <- simulate_cv_tests(A = 5, num_simulations = 10000, seed = 123)
# str(results_out)

# The object 'results_out' contains:
# $replicate_summary : data.table with replicate-level CVs & p-values
# $detailed_results  : list of length A, each with param tables etc.
# $final_summary     : data.table summarizing how many times we reject CV equality
