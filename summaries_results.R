library(dplyr)

output_folder <- "C:/test/Bonds/"
result_files_paths <- list.files(output_folder, recursive = TRUE, pattern="results")

###
example_file_path <- result_files_paths[1]
example_file_path <- paste0(output_folder, example_file_path)
example_file <- read.csv(example_file_path)

sectors <- unique(example_file$sector)
total_exposure <- example_file %>% select(sector, portfolio_sector_share) %>% unique()
total_exposure <- sum(total_exposure$portfolio_sector_share)

cut_of_quantile_high <- 0.95
cut_of_quantile_low <- 0.05

colname_high <- paste0('upper_cut_off_', as.character(cut_of_quantile_high))
colname_low <- paste0('lower_cut_off_', as.character(cut_of_quantile_low))

summary =data.frame('asset_tyoe'=character(),
                    'hazard'=character(),
                    'source'=character(),
                    'scenario'=character(),
                    colname_high = numeric(),
                    'mean_risk_increase'=numeric(),
                    'std_risk_increase'=numeric())


colnames(summary)[4]<- colname_high
colnames(summary)[5]<- colname_low

for (sector_ in sectors){

  summary[paste0(sector_,'_risk_increase_mean')]=numeric()
  summary[paste0(sector_,'_risk_increase_std')]=numeric()
}
colnames_ <- colnames(summary)


i=0
for (result_file_path in result_files_paths){
    i=i+1
    result_file_path_ <- paste0(output_folder, result_file_path)
    results <- read.csv(result_file_path_)
    results_path <- dirname(result_file_path)

    results <- results %>% mutate(relative_change = round(relative_change,1)) %>% filter(has_geo_data==TRUE)

    results <- results %>% filter(!is.na(relative_change))
    if (nrow(results)>0){
    results <- results %>% mutate(weight = portfolio_economic_value_share_sector * portfolio_sector_share)
    results <- results %>% mutate(risk_weight = relative_change * weight/total_exposure, risk_weight_2 = relative_change *relative_change * weight/total_exposure)

    mean_risk = sum(results$risk_weight)
    std = abs(mean_risk**2 - sum(results$risk_weight_2))**0.5

    asset_type <- unique(results$asset_type)
    hazard <- unique(results$hazard)
    scenario <- unique(results$scenario)
    source <-unique(results$provider)
    sector_dist <- results %>% group_by(sector, relative_change) %>% summarise(portfolio_economic_value_share_sector = sum(portfolio_economic_value_share_sector))
    sector_dist_ <- sector_dist %>% mutate(risk = relative_change * portfolio_economic_value_share_sector, risk_2 = relative_change* relative_change * portfolio_economic_value_share_sector) %>% group_by(sector) %>% summarise(risk = sum(risk), risk2  = sum(risk_2)) %>% mutate(std=abs(risk**2-risk2)**0.5)%>%select(-risk2)
    sector_dist_ref <- results %>% mutate(risk = relative_change/nrow(results), risk_2 = risk_level* risk_level/nrow(results)) %>% summarise(risk = sum(risk), risk2  = sum(risk_2)) %>% mutate(std=abs(risk**2-risk2)**0.5)%>%select(-risk2)
    sector_dist_ref["sector"]=c("Total")

    sector_dist_ <- sector_dist_ %>% rbind(sector_dist_ref)
    value_indicator_ths <- as.integer(quantile(results$relative_change, 0.95))

    value_indicator <- results %>% filter(relative_change > value_indicator_ths) %>% summarise(value_indicator = sum(weight)) %>% mutate(value_indicator = value_indicator/total_exposure)
    value_indicator <- sum(value_indicator$value_indicator) %>% round(3)


    row = c(asset_type,
            hazard,
            scenario,
            source,
            value_indicator,
            round(sum(sector_dist_ref$risk),3),
            round(sum(sector_dist_ref$std),3))


      for (sector_ in sectors){

        mean <- sum(sector_dist_ %>% filter(sector == sector_) %>% select(risk)) %>% round(3)
        std <- sum(sector_dist_ %>% filter(sector == sector_) %>% select(std)) %>% round(3)

        row <- append(row, c(mean, std))
      }

      summary <- summary %>% rbind(row)
      colnames(summary)<- colnames_}
}

write.csv(summary, paste0(output_folder, "summary_risk_increases.csv"))



summary =data.frame('asset_tyoe'=character(),
                    'hazard'=character(),
                    'source'=character(),
                    'scenario'=character(),
                    'value_above_95'=numeric(),
                    'mean_risk_level'=numeric(),
                    'std_risk_level'=numeric())
for (sector_ in sectors){

  summary[paste0(sector_,'_risk_level_mean')]=numeric()
  summary[paste0(sector_,'_risk_level_std')]=numeric()
}
colnames_ <- colnames(summary)


i=0
for (result_file_path in result_files_paths){
  i=i+1
  result_file_path_ <- paste0(output_folder, result_file_path)
  results <- read.csv(result_file_path_)
  results_path <- dirname(result_file_path)

  results <- results %>% mutate(risk_level = round(risk_level)) %>% filter(has_geo_data==TRUE)

  results <- results %>% filter(!is.na(risk_level))
  if (nrow(results)>0){
    results <- results %>% mutate(weight = portfolio_economic_value_share_sector * portfolio_sector_share)
    results <- results %>% mutate(risk_weight = risk_level * weight/total_exposure, risk_weight_2 = risk_level *risk_level * weight/total_exposure)

    mean_risk = sum(results$risk_weight)
    std = abs(mean_risk**2 - sum(results$risk_weight_2))**0.5

    asset_type <- unique(results$asset_type)
    hazard <- unique(results$hazard)
    scenario <- unique(results$scenario)
    source <-unique(results$provider)
    sector_dist <- results %>% group_by(sector, risk_level) %>% summarise(portfolio_economic_value_share_sector = sum(portfolio_economic_value_share_sector))
    sector_dist_ <- sector_dist %>% mutate(risk = risk_level * portfolio_economic_value_share_sector, risk_2 = risk_level* risk_level * portfolio_economic_value_share_sector) %>% group_by(sector) %>% summarise(risk = sum(risk), risk2  = sum(risk_2)) %>% mutate(std=abs(risk**2-risk2)**0.5)%>%select(-risk2)
    sector_dist_ref <- results %>% mutate(risk = risk_level/nrow(results), risk_2 = risk_level* risk_level/nrow(results)) %>% summarise(risk = sum(risk), risk2  = sum(risk_2)) %>% mutate(std=abs(risk**2-risk2)**0.5)%>%select(-risk2)
    sector_dist_ref["sector"]=c("Total")

    sector_dist_ <- sector_dist_ %>% rbind(sector_dist_ref)
    value_indicator_ths <- as.integer(quantile(results$risk_level, 0.95))

    value_indicator <- results %>% filter(risk_level > value_indicator_ths) %>% summarise(value_indicator = sum(weight)) %>% mutate(value_indicator = value_indicator/total_exposure)
    value_indicator <- sum(value_indicator$value_indicator) %>% round(3)


    row = c(asset_type,
            hazard,
            scenario,
            source,
            value_indicator,
            round(sum(sector_dist_ref$risk),3),
            round(sum(sector_dist_ref$std),3))


    for (sector_ in sectors){

      mean <- sum(sector_dist_ %>% filter(sector == sector_) %>% select(risk)) %>% round(3)
      std <- sum(sector_dist_ %>% filter(sector == sector_) %>% select(std)) %>% round(3)

      row <- append(row, c(mean, std))
    }

    summary <- summary %>% rbind(row)
    colnames(summary)<- colnames_}
}

write.csv(summary, paste0(output_folder, "summary_risk_levels.csv"))

