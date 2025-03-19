###############################################################################
# Libraries
###############################################################################
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
if (!requireNamespace("evir", quietly = TRUE)) install.packages("evir")

library(data.table)
library(evir)

###############################################################################
# 1) Simulation Parameters
###############################################################################
set.seed(123)
num_simulations <- 20000
r               <- 0.04  # 4% yearly discount rate

# Bond Details
Face_Value      <- 25000000
Years           <- c(2025, 2026, 2027)
Maturity_Length <- length(Years)

# Triggers for coupon and principal
Trigger_0       <- 0
Trigger_1       <- 65000000 
Trigger_2       <- 200000000

# -----------------------------------------------------------------------------
#   Coupon rates under triggers (example):
#     - Full coupon if losses < Trigger_1
#     - 60% coupon if in [Trigger_1, Trigger_2)
#     - 0% coupon if ≥ Trigger_2
# -----------------------------------------------------------------------------
Coupon_Rate_0   <- 0.05          # Full coupon
Coupon_Rate_1   <- 0.05 * 0.60   # Reduced coupon
Coupon_Rate_2   <- 0             # Zero coupon

# -----------------------------------------------------------------------------
#   Principal Repayment rates under triggers:
#     - 100% if losses < Trigger_1
#     - 50%  if [Trigger_1, Trigger_2)
#     - 0%   if ≥ Trigger_2
# -----------------------------------------------------------------------------
Principal_Rate_0 <- 1.0    # 100%
Principal_Rate_1 <- 0.50   # 50%
Principal_Rate_2 <- 0.0    # 0%

# Quarterly schedule (3 years = 12 quarters)
quarter_ends    <- seq(0.25, Maturity_Length, by = 0.25)

# Example companies & loss types
companies       <- paste0("Company_", 1:20)
loss_types      <- c("Identity - Fraudulent Use/Account Access",
                     "Data - Malicious Breach",
                     "Network/Website Disruption")

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

homogeneous_covariates <- data.table(
  Company         = companies,
  Location        = "USA",
  Business_Sector = "Finance",
  Revenue         = "Medium",
  Employee_Size   = "Medium"
)

heterogeneous_covariates <- data.table(
  Company         = companies,
  Location        = sample(c("USA", "Non-USA"), length(companies), replace = TRUE),
  Business_Sector = sample(c("Finance", "Healthcare", "Other"), length(companies), replace = TRUE),
  Revenue         = sample(c("Small", "Medium", "Big"), length(companies), replace = TRUE),
  Employee_Size   = sample(c("Small", "Medium", "Big"), length(companies), replace = TRUE)
)

###############################################################################
# 3) Function to Generate Model Parameters
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
        
        # cap shape (tau) below 1
        tau    <- ifelse(tau >= 1, 0.99, tau)
        
        results <- rbind(
          results,
          data.table(Company=company, Year=year, Loss_Type=loss_type,
                     Lambda=lambda, Mu=mu, Tau=tau)
        )
      }
    }
  }
  
  return(results)
}

heterogeneous_params <- simulate_parameters(heterogeneous_covariates)
homogeneous_params   <- simulate_parameters(homogeneous_covariates)

###############################################################################
# 4) Function to Simulate Arrival Times, allowing multiple events/year
###############################################################################
simulate_arrival_times_df <- function(params, years) {
  # We assume Lambda is the average number of events/year, so we draw N ~ Poisson(lambda)
  # Then each event is assigned a random arrival time U ~ Uniform(0,1) within that year.
  
  arrival_times_df <- data.table(Company=character(), Loss_Type=character(),
                                 Arrival_Time=numeric(), Year=numeric())
  
  for (i in unique(params$Company)) {
    for (lt in unique(params$Loss_Type)) {
      subset_params <- params[Company==i & Loss_Type==lt]
      
      for (yr in years) {
        lambda <- subset_params[Year==yr, Lambda]
        if (length(lambda) > 0 && lambda > 0) {
          # Number of events in [0,1] for that year
          N <- rpois(1, lambda)
          if (N > 0) {
            # arrival times within [0,1]
            times <- sort(runif(N, 0, 1)) + (yr - min(years))  # shift by (year - 2025)
            
            arrival_times_df <- rbind(
              arrival_times_df,
              data.table(Company=i, Loss_Type=lt, Arrival_Time=times, Year=yr)
            )
          }
        }
      }
    }
  }
  
  return(arrival_times_df)
}

###############################################################################
# 5) compute_payments(): Uses triggers to reduce coupon AND principal
###############################################################################
compute_payments <- function(events,
                             Face_Value, quarter_ends,
                             Trigger_1, Trigger_2,
                             Coupon_Rate_0, Coupon_Rate_1, Coupon_Rate_2,
                             Principal_Rate_0, Principal_Rate_1, Principal_Rate_2,
                             r) {
  
  results <- data.table(Quarter = 1:length(quarter_ends))
  setkey(events, Arrival_Time)
  
  # 1) Cumulative Loss by quarter
  results[, Cumulative_Loss := sapply(quarter_ends, function(cutoff) {
    sum(events[Arrival_Time <= cutoff, Loss_Amount])
  })]
  
  # 2) Coupon Rate by triggers
  results[, Coupon_Rate := fcase(
    Cumulative_Loss < Trigger_1,                                  Coupon_Rate_0,
    Cumulative_Loss >= Trigger_1 & Cumulative_Loss < Trigger_2,   Coupon_Rate_1,
    Cumulative_Loss >= Trigger_2,                                 Coupon_Rate_2
  )]
  
  # 3) Coupon Payment each quarter
  results[, Coupon_Payment := (Coupon_Rate * Face_Value) / 4]
  
  # 4) Principal repayment at maturity:
  #    Determine final trigger bucket from the final Cumulative_Loss
  final_cum_loss  <- results[Quarter == length(quarter_ends), Cumulative_Loss]
  principal_rate  <- fcase(
    final_cum_loss < Trigger_1,                                   Principal_Rate_0,
    final_cum_loss >= Trigger_1 & final_cum_loss < Trigger_2,     Principal_Rate_1,
    final_cum_loss >= Trigger_2,                                  Principal_Rate_2
  )
  
  principal_repay <- Face_Value * principal_rate
  
  # 5) Add Principal Repayment row
  results <- rbind(
    results,
    data.table(
      Quarter         = length(quarter_ends) + 1,
      Cumulative_Loss = final_cum_loss,
      Coupon_Rate     = NA_real_,
      Coupon_Payment  = principal_repay
    ),
    fill = TRUE
  )
  results[Quarter == length(quarter_ends) + 1, Quarter_Label := "Principal_Repayment"]
  
  # 6) Discounting each cash flow
  results[, Time := ifelse(Quarter <= length(quarter_ends),
                           Quarter * 0.25,
                           length(quarter_ends) * 0.25)]
  results[, PV := Coupon_Payment / ((1 + r)^Time)]
  
  return(results)
}

###############################################################################
# 6) Main Simulation Loop
###############################################################################
simulation_results <- data.table(
  Simulation            = integer(),
  Homogeneous_Loss      = numeric(),
  Heterogeneous_Loss    = numeric(),
  Homogeneous_PV        = numeric(),
  Heterogeneous_PV      = numeric(),
  Homogeneous_YTM       = numeric(),
  Heterogeneous_YTM     = numeric(),
  Homogeneous_AvgCoupon = numeric(),  # newly stored
  Heterogeneous_AvgCoupon = numeric(),
  Homogeneous_AvgPrincipal = numeric(),
  Heterogeneous_AvgPrincipal = numeric()
)

events_data_dt   <- data.table()
payments_data_dt <- data.table()

for (sim in seq_len(num_simulations)) {
  
  # --- A) Homogeneous ---
  homogeneous_arrival <- simulate_arrival_times_df(homogeneous_params, Years)
  
  if (nrow(homogeneous_arrival) > 0) {
    homogeneous_event_data <- merge(
      homogeneous_arrival,
      homogeneous_params[, .(Company, Year, Loss_Type, Mu, Tau)],
      by = c("Company", "Year", "Loss_Type"),
      all.x = TRUE
    )
    # Loss amounts from GPD
    homogeneous_event_data[, Loss_Amount := rgpd(.N, xi=Tau, mu=0, beta=Mu)]
    total_homogeneous_loss <- sum(homogeneous_event_data$Loss_Amount)
  } else {
    homogeneous_event_data   <- data.table(
      Company=character(), Year=integer(), Loss_Type=character(),
      Arrival_Time=numeric(), Mu=numeric(), Tau=numeric(), Loss_Amount=numeric()
    )
    total_homogeneous_loss   <- 0
  }
  
  # Compute bond cashflows
  homogeneous_payments <- compute_payments(
    homogeneous_event_data, Face_Value, quarter_ends,
    Trigger_1, Trigger_2,
    Coupon_Rate_0, Coupon_Rate_1, Coupon_Rate_2,
    Principal_Rate_0, Principal_Rate_1, Principal_Rate_2,
    r
  )
  homogeneous_pv <- sum(homogeneous_payments$PV, na.rm=TRUE)
  
  # Extract coupon/principal info to compute simulation-level average coupon & principal
  homo_coupons_dt <- homogeneous_payments[is.na(Quarter_Label)]
  homo_princ_dt   <- homogeneous_payments[Quarter_Label=="Principal_Repayment"]
  
  # If there are coupon rows (should be 12), compute average
  if (nrow(homo_coupons_dt) > 0) {
    sim_avg_coupon_homo <- mean(homo_coupons_dt$Coupon_Payment)
  } else {
    sim_avg_coupon_homo <- 0
  }
  # The principal row (should be 1) if it exists
  sim_princ_homo  <- ifelse(nrow(homo_princ_dt) > 0, homo_princ_dt$Coupon_Payment, 0)
  
  # Approx YTM = (AvgCoupon + Principal) / Price
  sim_yield_homo  <- ifelse(homogeneous_pv > 0,
                            (sim_avg_coupon_homo + sim_princ_homo) / homogeneous_pv,
                            0)
  
  # Store event data in a global table
  if (nrow(homogeneous_event_data) > 0) {
    homogeneous_event_data[, `:=`(Simulation=sim, Portfolio="Homogeneous")]
    events_data_dt <- rbind(events_data_dt, homogeneous_event_data, fill=TRUE)
  }
  
  # Store payment data
  hp <- copy(homogeneous_payments)
  hp[, `:=`(Simulation=sim, Portfolio="Homogeneous")]
  payments_data_dt <- rbind(payments_data_dt, hp, fill=TRUE)
  
  # --- B) Heterogeneous ---
  heterogeneous_arrival <- simulate_arrival_times_df(heterogeneous_params, Years)
  
  if (nrow(heterogeneous_arrival) > 0) {
    heterogeneous_event_data <- merge(
      heterogeneous_arrival,
      heterogeneous_params[, .(Company, Year, Loss_Type, Mu, Tau)],
      by = c("Company", "Year", "Loss_Type"),
      all.x=TRUE
    )
    heterogeneous_event_data[, Loss_Amount := rgpd(.N, xi=Tau, mu=0, beta=Mu)]
    total_heterogeneous_loss <- sum(heterogeneous_event_data$Loss_Amount)
  } else {
    heterogeneous_event_data <- data.table(
      Company=character(), Year=integer(), Loss_Type=character(),
      Arrival_Time=numeric(), Mu=numeric(), Tau=numeric(), Loss_Amount=numeric()
    )
    total_heterogeneous_loss <- 0
  }
  
  # Compute bond cashflows
  heterogeneous_payments <- compute_payments(
    heterogeneous_event_data, Face_Value, quarter_ends,
    Trigger_1, Trigger_2,
    Coupon_Rate_0, Coupon_Rate_1, Coupon_Rate_2,
    Principal_Rate_0, Principal_Rate_1, Principal_Rate_2,
    r
  )
  heterogeneous_pv <- sum(heterogeneous_payments$PV, na.rm=TRUE)
  
  # Simulation-level average coupon, principal, yield for the Heterogeneous side
  het_coupons_dt <- heterogeneous_payments[is.na(Quarter_Label)]
  het_princ_dt   <- heterogeneous_payments[Quarter_Label=="Principal_Repayment"]
  
  if (nrow(het_coupons_dt) > 0) {
    sim_avg_coupon_het <- mean(het_coupons_dt$Coupon_Payment)
  } else {
    sim_avg_coupon_het <- 0
  }
  sim_princ_het <- ifelse(nrow(het_princ_dt) > 0, het_princ_dt$Coupon_Payment, 0)
  
  sim_yield_het <- ifelse(heterogeneous_pv > 0,
                          (sim_avg_coupon_het + sim_princ_het) / heterogeneous_pv,
                          0)
  
  # Store event data
  if (nrow(heterogeneous_event_data) > 0) {
    heterogeneous_event_data[, `:=`(Simulation=sim, Portfolio="Heterogeneous")]
    events_data_dt <- rbind(events_data_dt, heterogeneous_event_data, fill=TRUE)
  }
  
  # Store payment data
  hep <- copy(heterogeneous_payments)
  hep[, `:=`(Simulation=sim, Portfolio="Heterogeneous")]
  payments_data_dt <- rbind(payments_data_dt, hep, fill=TRUE)
  
  # --- C) Summaries for This Simulation ---
  simulation_results <- rbind(
    simulation_results,
    data.table(
      Simulation               = sim,
      Homogeneous_Loss         = total_homogeneous_loss,
      Heterogeneous_Loss       = total_heterogeneous_loss,
      Homogeneous_PV           = homogeneous_pv,
      Heterogeneous_PV         = heterogeneous_pv,
      Homogeneous_YTM          = sim_yield_homo,
      Heterogeneous_YTM        = sim_yield_het,
      Homogeneous_AvgCoupon    = sim_avg_coupon_homo,
      Heterogeneous_AvgCoupon  = sim_avg_coupon_het,
      Homogeneous_AvgPrincipal = sim_princ_homo,
      Heterogeneous_AvgPrincipal = sim_princ_het
    )
  )
}

###############################################################################
# 7) Summaries & Approx. YTM
###############################################################################
expected_price_homogeneous   <- mean(simulation_results$Homogeneous_PV)
expected_price_heterogeneous <- mean(simulation_results$Heterogeneous_PV)

cat("Expected Price of Homogeneous Portfolio:   ",
    expected_price_homogeneous, "\n")
cat("Expected Price of Heterogeneous Portfolio: ",
    expected_price_heterogeneous, "\n")

# Principal & coupon payment stats (averaged across all simulations & quarters)
principal_payments <- payments_data_dt[
  Quarter_Label=="Principal_Repayment",
  .(AvgPrincipal=mean(Coupon_Payment)),
  by=Portfolio
]

coupon_payments <- payments_data_dt[
  is.na(Quarter_Label),
  .(AvgCoupon=mean(Coupon_Payment)),
  by=Portfolio
]

# Approx. yield from the *aggregated* perspective
ytm_dt <- merge(coupon_payments, principal_payments, by="Portfolio", all=TRUE)

price_dt <- data.table(
  Portfolio = c("Homogeneous","Heterogeneous"),
  Price     = c(expected_price_homogeneous, expected_price_heterogeneous)
)

ytm_dt <- merge(ytm_dt, price_dt, by="Portfolio", all=TRUE)
ytm_dt[, YTM := (AvgCoupon + AvgPrincipal) / Price]

# ---------------------------------------------------------------------------
# Compute Variance of the distribution of (AvgCoupon, AvgPrincipal, Price)
# across the num_simulations:
# We stored each simulation's "Homogeneous_AvgCoupon" etc. in 'simulation_results'.
# ---------------------------------------------------------------------------
# Homogeneous
var_homo_coupon    <- var(simulation_results$Homogeneous_AvgCoupon, na.rm=TRUE)
var_homo_principal <- var(simulation_results$Homogeneous_AvgPrincipal, na.rm=TRUE)
var_homo_price     <- var(simulation_results$Homogeneous_PV, na.rm=TRUE)

# Heterogeneous
var_het_coupon     <- var(simulation_results$Heterogeneous_AvgCoupon, na.rm=TRUE)
var_het_principal  <- var(simulation_results$Heterogeneous_AvgPrincipal, na.rm=TRUE)
var_het_price      <- var(simulation_results$Heterogeneous_PV, na.rm=TRUE)

# Add these to the ytm_dt table
ytm_dt[, Var_AvgCoupon := fcase(
  Portfolio=="Homogeneous", var_homo_coupon,
  Portfolio=="Heterogeneous", var_het_coupon
)]
ytm_dt[, Var_AvgPrincipal := fcase(
  Portfolio=="Homogeneous", var_homo_principal,
  Portfolio=="Heterogeneous", var_het_principal
)]
ytm_dt[, Var_Price := fcase(
  Portfolio=="Homogeneous", var_homo_price,
  Portfolio=="Heterogeneous", var_het_price
)]

cat("\nApproximate YTM for each Portfolio (based on aggregated coupon & price):\n")
print(ytm_dt)

###############################################################################
# 8) Calculate Penetration Rates
###############################################################################
# For the final losses in each simulation, define trigger outcomes:
#   - Trigger_0: final_loss < Trigger_1
#   - Trigger_1: Trigger_1 ≤ final_loss < Trigger_2
#   - Trigger_2: final_loss ≥ Trigger_2

homog_trigger0 <- sum(simulation_results$Homogeneous_Loss < Trigger_1)
homog_trigger1 <- sum(simulation_results$Homogeneous_Loss >= Trigger_1 &
                        simulation_results$Homogeneous_Loss <  Trigger_2)
homog_trigger2 <- sum(simulation_results$Homogeneous_Loss >= Trigger_2)

het_trigger0   <- sum(simulation_results$Heterogeneous_Loss < Trigger_1)
het_trigger1   <- sum(simulation_results$Heterogeneous_Loss >= Trigger_1 &
                        simulation_results$Heterogeneous_Loss <  Trigger_2)
het_trigger2   <- sum(simulation_results$Heterogeneous_Loss >= Trigger_2)

penetration_rates <- data.table(
  Portfolio        = c("Homogeneous", "Homogeneous", "Homogeneous",
                       "Heterogeneous","Heterogeneous","Heterogeneous"),
  Trigger_Bucket   = c("Trigger_0","Trigger_1","Trigger_2",
                       "Trigger_0","Trigger_1","Trigger_2"),
  Count            = c(homog_trigger0, homog_trigger1, homog_trigger2,
                       het_trigger0,   het_trigger1,   het_trigger2),
  Penetration_Rate = c(
    homog_trigger0 / num_simulations,
    homog_trigger1 / num_simulations,
    homog_trigger2 / num_simulations,
    het_trigger0   / num_simulations,
    het_trigger1   / num_simulations,
    het_trigger2   / num_simulations
  )
)

cat("\nPenetration Rates (Proportion of Simulations in Each Trigger Bucket):\n")
print(penetration_rates)

###############################################################################
# Final Tables:
###############################################################################
# simulation_results: per-simulation summary of losses, PV, YTM, plus per-sim
#                     avg coupon & principal
# events_data_dt:     all loss events (including multiple per year)
# payments_data_dt:   all coupon & principal payments (including PV)
# ytm_dt:             approximate yield from average values + variance
# penetration_rates:  final trigger penetration summary
###############################################################################

simulation_results
events_data_dt
payments_data_dt
ytm_dt
penetration_rates

write.csv(simulation_results, "simulation_results.csv")
write.csv(events_data_dt, "events_data_dt.csv")
write.csv(payments_data_dt, "payments_data_dt.csv")
write.csv(ytm_dt, "ytm_dt.csv")
write.csv(penetration_rates, "penetration_rates.csv")



# For the homogeneous portfolio:
threshold_homo_2  <- quantile(simulation_results$Homogeneous_Loss, 0.98, na.rm = TRUE)
threshold_homo_05 <- quantile(simulation_results$Homogeneous_Loss, 0.995, na.rm = TRUE)

top2_homo  <- simulation_results[Homogeneous_Loss >= threshold_homo_2]
top05_homo <- simulation_results[Homogeneous_Loss >= threshold_homo_05]

# For the heterogeneous portfolio:
threshold_het_2  <- quantile(simulation_results$Heterogeneous_Loss, 0.98, na.rm = TRUE)
threshold_het_05 <- quantile(simulation_results$Heterogeneous_Loss, 0.995, na.rm = TRUE)

top2_het  <- simulation_results[Heterogeneous_Loss >= threshold_het_2]
top05_het <- simulation_results[Heterogeneous_Loss >= threshold_het_05]

threshold_homo_2
threshold_homo_05

threshold_het_2
threshold_het_05


