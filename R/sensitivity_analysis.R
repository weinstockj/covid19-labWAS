compare_estimates = function(summary_one, summary_two) {
    
    summary_one = filter_summary_stats_to_labs(summary_one) %>%
        dplyr::filter(outcome_type == "Hospitalized") %>%
        dplyr::select(ResultName, beta_one = beta)

    summary_two = filter_summary_stats_to_labs(summary_two) %>%
        dplyr::filter(outcome_type == "Hospitalized") %>%
        dplyr::select(ResultName, beta_two = beta)

    merged_summaries = dplyr::inner_join(summary_one, summary_two, by = "ResultName")

    n_labs = length(unique(merged_summaries$ResultName))
    
    correlation_pearson = cor(merged_summaries$beta_one, merged_summaries$beta_two)
    correlation_spearman = cor(merged_summaries$beta_one, merged_summaries$beta_two, method = "spearman")

    futile.logger::flog.info(glue::glue("pearson correlation across {n_labs} labs is {scales::number(correlation_pearson, accuracy = .01)}"))
    futile.logger::flog.info(glue::glue("spearman correlation across {n_labs} labs is {scales::number(correlation_spearman, accuracy = .01)}"))
}
