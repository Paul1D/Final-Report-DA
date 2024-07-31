library(decisionSupport)
library(ggplot2)
library(dplyr)
library(openxlsx)


##### Input Table Agroforestry system & Baseline comparisson


cost_benefits <- read.csv("input_table.csv", header = TRUE, sep = ";")

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,as.numeric(x[1,i]),envir=.GlobalEnv)

} 

make_variables(as.estimate(cost_benefits))

cost_benefits_function <- function(){
  
  # Initialize vector to store profits for each year
  
  profit_annuals <- numeric(operating_years)
  
  # Generate yields and prices for each year based on crop rotation with adjusted yields due to production risks 
  # yield (kg/ha) , price (EUR/kg)
  # Initial risk
  
  
  for (year in 1:operating_years) {
    cycle_year <- (year - 1) %% 4 + 1
    # Calculate the cumulative risk for the current year
    cumulative_risk <- p_production_risk + (year - 1) * 0.005
    
    if (cycle_year == 1) {
      total_yield_wheat <- vv(yield_wheat * hectares_annuals, var_CV, 1)
      adjusted_wheat_yield <- chance_event(
        chance = vv(var_mean = cumulative_risk, var_CV = var_CV, n = 1),
        value_if = total_yield_wheat * vv(p_change_production_risk_AF, var_CV, 1),
        value_if_not = total_yield_wheat,
        n = 1
      )
      price_wheat <- vv(price_wheat, var_CV, 1)
      profit_annuals[year] <- adjusted_wheat_yield * price_wheat
    } else if (cycle_year == 2) {
      total_yield_soy_bean <- vv(yield_soy_bean * hectares_annuals, var_CV, 1)
      adjusted_soy_bean_yield <- chance_event(
        chance = vv(var_mean = cumulative_risk, var_CV = var_CV, n = 1),
        value_if = total_yield_soy_bean * vv(p_change_production_risk_AF, var_CV, 1),
        value_if_not = total_yield_soy_bean,
        n = 1
      )
      price_soy_bean <- vv(price_soy_bean, var_CV, 1)
      profit_annuals[year] <- adjusted_soy_bean_yield * price_soy_bean
    } else if (cycle_year == 3) {
      total_yield_maize <- vv(yield_maize * hectares_annuals, var_CV, 1)
      adjusted_maize_yield <- chance_event(
        chance = vv(var_mean = cumulative_risk, var_CV = var_CV, n = 1),
        value_if = total_yield_maize * vv(p_change_production_risk_AF, var_CV, 1),
        value_if_not = total_yield_maize,
        n = 1
      )
      price_maize <- vv(price_maize, var_CV, 1)
      profit_annuals[year] <- adjusted_maize_yield * price_maize
    } else if (cycle_year == 4) {
      total_yield_landsberger_gemenge <- vv(yield_landsberger_gemenge * hectares_annuals, var_CV, 1)
      adjusted_landsberger_gemenge_yield <- chance_event(
        chance = vv(var_mean = cumulative_risk, var_CV = var_CV, n = 1),
        value_if = total_yield_landsberger_gemenge * vv(p_change_production_risk_AF, var_CV, 1),
        value_if_not = total_yield_landsberger_gemenge,
        n = 1
      )
      price_landsberger_gemenge <- vv(price_landsberger_gemenge, var_CV, 1)
      profit_annuals[year] <- adjusted_landsberger_gemenge_yield * price_landsberger_gemenge
    }
  }
  
  #calculate profits from perennials (kg/tree), one tree cycle goes for 40 years so the as the timespan of the simulation
  
  yield_apple <- gompertz_yield(max_harvest=max_harvest_apple,
                                time_to_first_yield_estimate=time_to_first_yield_estimate_apple,
                                time_to_second_yield_estimate=time_to_second_yield_estimate_apple,
                                first_yield_estimate_percent= first_yield_estimate_percent_apple,
                                second_yield_estimate_percent= second_yield_estimate_percent_apple,
                                n_years = n_years_apple, var_CV = var_CV,
                                no_yield_before_first_estimate = TRUE)
  
  # this function calculates total apple yield by multiplying yield of one tree with total amount of trees)
  total_yield_apple <- (yield_apple*trees_per_ha*hectares_perennials)
  #include production risks into apple yield
  adjusted_apple_yield <- chance_event(chance = vv(var_mean = p_production_risk,
                                                    var_CV = var_CV,
                                                    n = operating_years,
                                                    absolute_trend = 0.005), #Increasing risk of production problems due to climate change
                                                     value_if = total_yield_apple * vv(p_change_production_risk_AF, var_CV, 1),
                                                     value_if_not = total_yield_apple,
                                                     n = operating_years)
  
  # Calculate the yield for plums
  yield_plum <- gompertz_yield(max_harvest=max_harvest_plum,
                               time_to_first_yield_estimate=time_to_first_yield_estimate_plum,
                               time_to_second_yield_estimate=time_to_second_yield_estimate_plum,
                               first_yield_estimate_percent=first_yield_estimate_percent_plum,
                               second_yield_estimate_percent=second_yield_estimate_percent_plum,
                               n_years=n_years_plum, var_CV= var_CV,
                               no_yield_before_first_estimate=TRUE)
  
  # this function calculates total apple yield by multiplying yield of one tree with total amount of trees)
  total_yield_plum <- (yield_plum*trees_per_ha*hectares_perennials)
  
  # include production risks into plum yield
  adjusted_plum_yield <- chance_event(chance =vv(var_mean = p_production_risk,
                                                 var_CV = var_CV,
                                                 n = operating_years,
                                                 absolute_trend = 0.005), #Increasing risk of production problems due to climate change 
                                       value_if = total_yield_plum * vv(p_change_production_risk_AF, var_CV, 1),
                                       value_if_not = total_yield_plum,
                                       n = operating_years)
  
  #calculate profits over time span of 40 years 
  profit_perennials <- (adjusted_apple_yield*price_apple)+(adjusted_plum_yield*price_plum)
  
  # Calculate benefit from subsidies (echo scheme tree): 200€/ ha of trees/ year 
  subsidies <- numeric(operating_years)
  subsidies[1:operating_years] <- (subsidies_eco_scheme_tree*hectares_perennials)
  # Subsidies with the included risk of institutional and policy changes, the risk is increasing over time
  # due to highly uncertain political landscape 
  adjusted_subsidies <-  chance_event(chance = vv(var_mean = p_institutional_policy_risk,
                                                  var_CV = var_CV,
                                                  n = operating_years,
                                                  absolute_trend = 0.004),
                                      value_if = subsidies * vv(var_mean = p_change_institutional_risk_AF,
                                                                var_CV = var_CV,
                                                                n = operating_years),
                                      value_if_not = subsidies,
                                      n = operating_years)
  
  # Calculate ecological benefits(e.g. carbon sequestration, increased bio diversity) EUR/ha/per year 
  eco_benefits <-vv(var_mean = ecological_benefits*(hectares_annuals+hectares_perennials),
                   var_CV = var_CV,
                   n = operating_years)

  # Calculate social moral benefits: the monetary value that incentivizes the farmer to choose the AF system over 
  # the conventional system in EUR/ha/year

  sm_benefits <-vv(var_mean = social_moral_benefits*(hectares_annuals+hectares_perennials),
                   var_CV = var_CV,
                   n = operating_years)
  
  non_yield_profits <- (adjusted_subsidies + eco_benefits + sm_benefits)
  
  
  ## Calulate initial investment costs 
  
  # calculate how many trees are bought and to which cost, area of perennials is divided by 2, since area is shared by plum and apple trees
  invest_costs_apple_trees <- c(apple_tree*(trees_per_ha*(hectares_perennials/2)), rep(0, 39)) 
  invest_costs_plum_trees <- c(plum_tree*(trees_per_ha*(hectares_perennials/2)), rep(0, 39))

  invest_costs_irrigation <- c(rep(irrigation_cost * irrigation_amount * trees_per_ha * hectares_perennials,3),
                               rep(0, operating_years - 3))
  
  invest_costs_infrastructure <- c(new_infrastructure_cost*(hectares_annuals+hectares_perennials), rep(0,39))

    # calulate total initial investments costs
  invest_costs_total <- invest_costs_apple_trees+invest_costs_plum_trees+invest_costs_irrigation+invest_costs_infrastructure
  
  ## Calculate operating cost 
  
  new_machinery_rental_cost <- vv(var_mean = new_machinery_rental_cost*(hectares_annuals+hectares_perennials), #rental cost  in EUR/ha/year
                                  var_CV = var_CV,
                                  n = operating_years)
  
  
  machinery_maintenance_costs <- vv(var_mean = machinery_maintenance_cost,  
                                    var_CV = var_CV, 
                                    n = operating_years)
  fuel_costs <- vv(var_mean = fuel_cost*(hectares_annuals+hectares_perennials), # fuel cost per ha/year
                   var_CV = var_CV, 
                   n = operating_years)
  labour_costs <- vv(var_mean = labour_cost*labour_hours*(hectares_annuals+hectares_perennials), # (EUR/hour)*(hours/ha)*total ha
                      var_CV = var_CV, 
                      n = operating_years)
  # Adjusted labor costs in case farmer gets sick or other personal problems
  adjusted_labour_costs <- chance_event(chance = vv(var_mean = p_personal_risk,
                                                    var_CV = var_CV,
                                                    n = operating_years,
                                                    absolute_trend = 0.003), # increasing over time due to higher chance of becoming sick with age 
                                      value_if = labour_costs * vv(p_change_personal_risk, var_CV, 1),
                                      value_if_not = labour_costs,
                                      n = operating_years)
  
  seed_costs_wheat <- vv(var_mean = seed_cost_wheat * hectares_annuals, # EUR/ha 
                      var_CV = var_CV, 
                      n = operating_years)
  seed_costs_soy_bean <- vv(var_mean = seed_cost_soy_bean*hectares_annuals, # EUR/ha 
                            var_CV = var_CV, 
                            n = operating_years)
  seed_costs_maize <- vv(var_mean = seed_cost_maize*hectares_annuals, # EUR/ha 
                         var_CV = var_CV, 
                         n = operating_years)
  seed_costs_landsberger_gemenge <- vv(var_mean = seed_cost_langsberger_gemenge*hectares_annuals, # EUR/ha 
                                       var_CV = var_CV, 
                                       n = operating_years)
  fertilizer_costs <- vv(var_mean = fertilizer_cost*(hectares_annuals+hectares_perennials),  #EUR/ha
                         var_CV = var_CV, 
                         n = operating_years)
  crop_protection_costs <- vv(var_mean = crop_protection_cost * (hectares_annuals + hectares_perennials), # EUR/ha 
                         var_CV = var_CV, 
                         n = operating_years)
  distribution_costs <- vv(var_mean = distribution_cost * (hectares_annuals+hectares_perennials), # EUR/ha 
                            var_CV = var_CV, 
                            n = operating_years)

  # calculate total operating costs
  operating_costs_total <- new_machinery_rental_cost+machinery_maintenance_costs+fuel_costs+adjusted_labour_costs+seed_costs_wheat+
                           seed_costs_soy_bean+seed_costs_maize+seed_costs_landsberger_gemenge+
                           fertilizer_costs+crop_protection_costs+distribution_cost
 
  
  ## Calculate profits for AF system
  profit_AF_system <- chance_event(
    chance = vv(
      var_mean = p_market_price_risk,
      var_CV = var_CV,
      n = operating_years,
      absolute_trend = 0.004 # increasing risk due to highly uncertain development of markets
    ),
    value_if = (profit_annuals + profit_perennials + non_yield_profits - invest_costs_total - operating_costs_total) * 
      vv(
        var_mean = p_change_market_risk,
        var_CV = var_CV,
        n = operating_years
      ),
    value_if_not = profit_annuals + profit_perennials + non_yield_profits - invest_costs_total - operating_costs_total,
    n = operating_years
  )
  
  
  
  ## Baseline comparisson 
  
  # Calculate profits from annuals in the conventional system for baseline comparisson 
  # Initialize vector to store yields for each year
  # total_hectares_baseline = owned land + leased land 

  profits_baseline <- numeric(operating_years)
  
  
  # Initial risk
  initial_risk <- p_production_risk
  
  # Generate yields and prices for each year based on Rheinische crop rotation
  for (year in 1:operating_years) {
    cycle_year <- (year - 1) %% 3 + 1
    # Calculate the cumulative risk for the current year
    cumulative_risk <- initial_risk + (year - 1) * 0.005
    
    if (cycle_year == 1) {
      total_yield_wheat_baseline <- vv(yield_wheat_baseline * total_hectares_baseline, var_CV, 1)
      adjusted_wheat_yield_baseline <- chance_event(
        chance = vv(var_mean = cumulative_risk, var_CV = var_CV, n = 1),
        value_if = total_yield_wheat_baseline * vv(p_change_production_risk_baseline, var_CV, 1),
        value_if_not = total_yield_wheat_baseline,
        n = 1
      )
      price_wheat <- vv(price_wheat_baseline, var_CV, 1)
      profits_baseline[year] <- adjusted_wheat_yield_baseline * price_wheat
    } else if (cycle_year == 2) {
      total_yield_rape_seed_baseline <- vv(yield_rape_seed_baseline * total_hectares_baseline, var_CV, 1)
      adjusted_yield_rape_seed_baseline <- chance_event(
        chance = vv(var_mean = cumulative_risk, var_CV = var_CV, n = 1),
        value_if = total_yield_rape_seed_baseline * vv(p_change_production_risk_baseline, var_CV, 1),
        value_if_not = total_yield_rape_seed_baseline,
        n = 1
      )
      price_rape_seed <- vv(price_rape_seed_baseline, var_CV, 1)
      profits_baseline[year] <- adjusted_yield_rape_seed_baseline * price_rape_seed
    } else if (cycle_year == 3) {
      total_yield_sugar_beet_baseline <- vv(yield_sugar_beet_baseline * total_hectares_baseline, var_CV, 1)
      adjusted_yield_sugar_beet_baseline <- chance_event(
        chance = vv(var_mean = cumulative_risk, var_CV = var_CV, n = 1),
        value_if = total_yield_sugar_beet_baseline * vv(p_change_production_risk_baseline, var_CV, 1),
        value_if_not = total_yield_sugar_beet_baseline,
        n = 1
      )
      price_sugar_beet <- vv(price_sugar_beet_baseline, var_CV, 1)
      profits_baseline[year] <- adjusted_yield_sugar_beet_baseline * price_sugar_beet
    }
  }
  
  
  ## Calculate costs for conventional system
  
  machinery_maintenance_costs_baseline <- vv(var_mean = machinery_maintenance_baseline, 
                                             var_CV = var_CV, 
                                             n = operating_years)
  
  fuel_costs_baseline <- vv(var_mean = fuel_cost_baseline*total_hectares_baseline, 
                            var_CV = var_CV, 
                            n = operating_years)
  
  labour_costs_baseline <- vv(var_mean = labour_cost_baseline*labour_hours_baseline*total_hectares_baseline, # EUR/hour*hour/ha * ha
                              var_CV = var_CV, 
                              n = operating_years)
  adjusted_labour_costs_baseline <- chance_event(chance = vv(var_mean = p_personal_risk,
                                                             var_CV = var_CV,
                                                             n = operating_years,
                                                             absolute_trend = 0.003), # increasing over time due to higher chance of becoming sick with age ,
                                        value_if = labour_costs_baseline * vv(p_change_personal_risk, var_CV, 1),
                                        value_if_not = labour_costs_baseline,
                                        n = operating_years)
  
  seed_costs_wheat_baseline <- vv(var_mean = seed_cost_wheat_baseline*total_hectares_baseline,
                                  var_CV = var_CV,
                                  n = operating_years)
  seed_costs_rape_seed_baseline <-vv(var_mean = seed_cost_rape_seed_baseline*total_hectares_baseline,
                                     var_CV = var_CV,
                                     n = operating_years)
  seed_costs_sugar_beet_baseline <- vv(var_mean = seed_cost_wheat_baseline*total_hectares_baseline,
                                       var_CV = var_CV,
                                       n = operating_years)
  
  fertilizers_costs_baseline <- vv(var_mean = fertilizer_cost_baseline*total_hectares_baseline,
                                   var_CV = var_CV,
                                   n = operating_years)
  crop_protection_costs_baseline <- vv(var_mean = crop_protection_cost_baseline*total_hectares_baseline,
                                        var_CV = var_CV,
                                        n = operating_years)
  leasing_costs_baseline <- vv(var_mean = leasing_cost_baseline*leased_hectares_baseline,
                              var_CV = var_CV,
                              n = operating_years,
                              absolute_trend = 0.002)
                              
  
  #Calculate total costs baseline
  costs_baseline <- (machinery_maintenance_costs_baseline+fuel_costs_baseline+adjusted_labour_costs_baseline+seed_costs_wheat_baseline+
                       seed_costs_rape_seed_baseline+ seed_costs_sugar_beet_baseline+fertilizers_costs_baseline+crop_protection_costs_baseline+leasing_costs_baseline)

 
  
  # calculate profit with the net with included market risks
  profit_baseline_comparisson <- chance_event(chance = vv(var_mean = p_market_price_risk,
                                                          var_CV = var_CV,
                                                          n = operating_years,
                                                          absolute_trend = 0.004), # increasing risk due to highly uncertain development of markets 
                                              value_if =  (profits_baseline-costs_baseline)*
                                                vv(var_mean = p_change_market_risk ,
                                                   var_CV = var_CV,
                                                   n = operating_years),
                                              value_if_not = (profits_baseline-costs_baseline),
                                              n = operating_years)
  
  # use 'discount' to calculate net present value 
  # 'discount_rate' is expressed in percent
  NPV_AF_system <- discount(profit_AF_system, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_baseline_comparisson <- discount(profit_baseline_comparisson, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  
  # calculate the overall NPV of the decision (do - don't do)
  NPV_decision <- NPV_AF_system - NPV_baseline_comparisson
  
  # Cashflow

  AF_cash_flow <- vv(profit_AF_system, n = operating_years, var_CV = var_CV)
  baseline_cash_flow <-vv(profit_baseline_comparisson, n = operating_years, var_CV = var_CV)
  
  
  
  return(list(NPV_baseline_comparisson =  NPV_baseline_comparisson,
              NPV_AF_system =  NPV_AF_system, 
              NPV_decision = NPV_decision,
              AF_cash_flow = AF_cash_flow,
              #AF_cum_cash_flow = AF_cum_cash_flow,
              baseline_cash_flow = baseline_cash_flow
              #baseline_cum_cash_flow = baseline_cum_cash_flow
              ))

}

# Run the Monte Carlo simulation using the model function
model_mc_simulation <- mcSimulation(estimate = as.estimate(cost_benefits),
                                   model_function = cost_benefits_function, 
                                   numberOfModelRuns = 1000,
                                   functionSyntax = "plainNames")
#plotting results
plot_distributions(mcSimulation_object = model_mc_simulation, 
                   vars = c("NPV_baseline_comparisson", "NPV_AF_system"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

ggsave("mc_distr_plot.png", plot = last_plot(), width = 20, height = 7, units = "cm")

plot_distributions(mcSimulation_object = model_mc_simulation, 
                   vars = c("NPV_decision"),
                   method = 'boxplot_density', 
                   y_axis_name = "Probability",
                   x_axis_name = "Net decisions outcome (NPV in Euro)",
                   base_size = 7)

ggsave("boxplot_mc_plot.png", plot = last_plot(), width = 20, height = 7, units = "cm")


plot_cashflow(mcSimulation_object = model_mc_simulation, 
              cashflow_var_name = c("AF_cash_flow", "baseline_cash_flow"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in Euro",
              color_25_75 = "blue1", color_5_95 = "darkblue",
              color_median = "red", 
              facet_labels = c("AF System", "Conventional system"))

 ggsave("cashflow_mc_plot.png", plot = last_plot(), width = 20, height = 7, units = "cm")

compound_figure(mcSimulation_object = model_mc_simulation, 
                input_table = cost_benefits,
                decision_var_name = "NPV_decision",
                cashflow_var_name = "AF_cash_flow",
                model_runs = 1e2, 
                distribution_method = 'smooth_simple_overlay')

ggsave("compound_mc_plot.png", plot = last_plot(), width = 40, height = 20, units = "cm")


mcSimulation_table_AF <- data.frame(model_mc_simulation$x, 
                                    model_mc_simulation$y[3]) 

mcSimulation_table_AF <- na.omit(mcSimulation_table_AF) 
evpi_AF <- multi_EVPI(mc = mcSimulation_table_AF, 
                      first_out_var = "NPV_decision")

plot_evpi(evpi_AF, decision_vars = "NPV_decision")
