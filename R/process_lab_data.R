get_spark_yaml = function() {
     "/net/mgi/covid_labWAS/scripts/covidLabWASanalysis/config-template.yml"
}

create_order_lookup = function(labs) {
    labs %>%
        dplyr::distinct(OrderName, ResultName)
}

phewas_plot = function(summary_stats, output_dir) {
    outcome_types = unique(summary_stats$outcome_type)

    purrr::walk(outcome_types, ~phewas_plot_(
                                summary_stats, 
                                .x, 
                                covariates = covariate_list_generator(),
                                output_dir = output_dir
        )
    )
}

forest_plot = function(summary_stats, output_dir) {
    outcome_types = unique(summary_stats$outcome_type)

    purrr::walk(outcome_types, ~forest_plot_(
                                summary_stats, 
                                .x, 
                                covariates = covariate_list_generator(),
                                output_dir = output_dir
        )
    )
}


phewas_plot_ = function(summary_stats, .outcome_type = "ICU", covariates = covariate_list_generator(), output_dir) {
    triangle_up = 24
    triangle_down = 25

    covariate_string = paste("Covariates include:", glue::glue_collapse(covariates, " + "))

    n_labs = length(unique(summary_stats$ResultName))
    n_tests = 3 # ICU / Hospitalized / Deceased
    bonferonni = -log10(.05 / (n_labs * n_tests))

    summary_stats = summary_stats %>%
        dplyr::filter(term == "Value" & outcome_type == .outcome_type) 

    p = summary_stats %>%
        dplyr::mutate(shape = ifelse(beta > 0, triangle_up, triangle_down)) %>%
        ggplot2::ggplot(ggplot2::aes(x = ResultName, y = -log10(pvalue), shape = shape)) +
        ggplot2::geom_hline(yintercept = bonferonni, color = "gray", linetype = "dashed", alpha = .8) +
        ggplot2::geom_point() + 
        cowplot::theme_cowplot(font_size = 9) + 
        ggplot2::scale_shape_identity() + 
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.1, hjust=1),
            axis.title.x = ggplot2::element_blank()
        ) +
        ggplot2::labs(subtitle = covariate_string) +
        ggplot2::ggtitle(glue::glue("LabWAS of {.outcome_type} status"))

    
    date = stringr::str_replace_all(Sys.Date(), "-", "_")
    job_name = return_global_job_name()

    ggplot2::ggsave(
        filename = file.path(output_dir, "figures", glue::glue("labwas_{.outcome_type}_{date}_{job_name}.pdf")),
        plot = p,
        units = "in",
        width = 10,
        height = 6
    )
    ggplot2::ggsave(
        filename = file.path(output_dir, "figures", glue::glue("labwas_{.outcome_type}_{date}_{job_name}.tiff")),
        plot = p,
        dpi = 300,
        units = "in",
        width = 10,
        height = 6,
        type = "cairo"
    )

}

forest_plot_ = function(summary_stats, .outcome_type = "ICU", covariates = covariate_list_generator(), output_dir) {
    covariate_string = paste("Covariates include:", glue::glue_collapse(covariates, " + "))

    n_labs = length(unique(summary_stats$ResultName))
    n_tests = 3 # ICU / Hospitalized / Deceased
    bonferonni = -log10(.05 / (n_labs * n_tests))

    date = stringr::str_replace_all(Sys.Date(), "-", "_")
    job_name = return_global_job_name()

    # labs are on a widly different scale than the others
    exclude_labs = c(
        "Absolute Early Gran Count",
        "Absolute Basophil Count",
        "Specific Gravity (Urine)",
        "Specific Gravity, Urine"
    )

    summary_stats = summary_stats %>%
        dplyr::filter(term == "Value" & outcome_type == .outcome_type) %>%
        dplyr::filter(!(ResultName %in% exclude_labs)) %>%
        dplyr::mutate(ResultName = stringr::str_replace_all(ResultName, " CHOL", ""))

    p = summary_stats %>%
        dplyr::mutate(ResultName = forcats::fct_reorder(ResultName, exp(beta), .desc = FALSE)) %>%
        ggplot2::ggplot(ggplot2::aes(y = ResultName, x = exp(beta), xmin = exp(conf.low), xmax = exp(conf.high))) +
        ggplot2::geom_pointrange() + 
        #ggplot2::coord_flip() +
        cowplot::theme_cowplot(font_size = 14) + 
        ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(color = "black", size = 1)
        ) +
        ggplot2::xlim(0, 6) +
        ggplot2::labs(x = "OR (95% CI)") +
        ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "gray", alpha = .5) 
        # ggplot2::ggtitle(glue::glue("LabWAS of {.outcome_type} status"))

    beta_table = summary_stats %>%
        dplyr::mutate(ResultName = forcats::fct_reorder(ResultName, exp(beta), .desc = FALSE)) %>%
        dplyr::mutate(
           beta = scales::number(exp(beta), accuracy = .01), 
           conf.low = scales::number(exp(conf.low), accuracy = .01), 
           conf.high = scales::number(exp(conf.high), accuracy = .01),
           ci_label = glue::glue("{beta} ({conf.low}-{conf.high})")
        ) %>%
        ggplot2::ggplot(data = ., ggplot2::aes(y = ResultName)) +
            ggplot2::geom_text(ggplot2::aes(x = .01, label = ResultName, hjust = 0)) +
            ggplot2::geom_text(ggplot2::aes(x = 3, label = ci_label)) +
            ggplot2::theme_void() +
            ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")) +
            ggplot2::xlim(0, 4) +
            ggplot2::annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = 2.5, xend = 3.5, size = 1) +
            ggplot2::ggtitle("                                                        OR (95% CI)")
            # ggplot2::annotation_custom(grid::textGrob("OR (95% CI)"), xmin = 3, xmax = 4, ymin = 40, ymax = 40)
            # cowplot::theme_minimal_hgrid()

    pvalue_table = summary_stats %>%
        dplyr::mutate(ResultName = forcats::fct_reorder(ResultName, exp(beta), .desc = FALSE)) %>%
        dplyr::mutate(
            pvalue_label = scales::scientific(pvalue, digits = 2)
        ) %>%
        ggplot2::ggplot(data = ., ggplot2::aes(y = ResultName)) +
            ggplot2::geom_text(ggplot2::aes(x = .2, label = pvalue_label)) +
            ggplot2::theme_void() +
            ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")) +
            ggplot2::xlim(0, .6)  +
            ggplot2::annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = .1, xend = .35, size = 1) +
            ggplot2::ggtitle("          Pvalue")
            # cowplot::theme_minimal_hgrid()

    table_and_plot = beta_table + p + pvalue_table +
        patchwork::plot_annotation(
        title = glue::glue("LabWAS of {.outcome_type} status"),
        subtitle = covariate_string 
        ) +
        patchwork::plot_layout(ncol = 3, widths = c(2, 2, 1))

    ggplot2::ggsave(
        filename = file.path(output_dir, "figures", glue::glue("labwas_forest_plot_and_table_{.outcome_type}_{date}_{job_name}.pdf")),
        plot = table_and_plot,
        units = "in",
        width = 12,
        height = 12
    )
}

summarize_lab_data = function(labs) {
    # takes 2 minutes without spark or previous cache
    assert_not_empty(labs)

    futile.logger::flog.info("now summarizing labs")

    # num_cores = 12

    # config = sparklyr::spark_config(file = get_spark_yaml(), use_default = FALSE)
    # config$`sparklyr.shell.driver-memory` <- "70G"
    # config$`sparklyr.shell.executor-memory` <- "40G"
    # config$spark.executor.memory <- "10GB"
    # config$spark.driver.memory <- "20GB"

    # config$`sparklyr.cores.local` <- num_cores
    # sc = sparklyr::spark_connect(master = "local", config = config, version = "3.0.0")
    # sc$config[['sparklyr.shell.driver-memory']] = "30g"
    # sc$config[['sparklyr.connect.cores.local']] = num_cores
    # sc$config[['spark.sql.shuffle.partitions.local']] = num_cores
    # sc$config[['spark.executor.memory']] = "10g"
    # labs_tbl = sparklyr::copy_to(sc, labs, repartition = num_cores)

    # lab_summary = labs_tbl %>%
    lab_summary = labs %>%
        dplyr::mutate(Value = as.numeric(readr::parse_number(Value))) %>%
        tidyr::drop_na(Value) %>%
        dplyr::group_by(Deid_ID, ResultName) %>%
        dplyr::arrange(desc(CollectionDate_DaysSinceBirth)) %>% # descending order
        dplyr::slice(1) %>% # take most recent value
        dplyr::ungroup(.) %>%
        dplyr::select(Deid_ID, ResultName, Value, Unit, Range, HILONORMAL = HILONORMAL_Flag, days_before_test_or_dx) 
        # sparklyr::collect(.)

    futile.logger::flog.info("done.")

    lab_summary
}

get_rcache = function() {
    "/net/mgi/covid_labWAS/.rcache"
}

summarize_lab_data = memoise::memoise(summarize_lab_data, cache = memoise::cache_filesystem(get_rcache()))

filter_to_labs_with_large_sample_sizes = function(labs, sample_size_threshold) {
    assert_not_empty(labs)

    futile.logger::flog.info("filtering now to most common labs")

    labs_to_use = get_labs_with_large_sample_sizes(labs, sample_size_threshold = sample_size_threshold)

    futile.logger::flog.info(glue::glue("Identifed {length(labs_to_use)} with sample size >= {sample_size_threshold}"))
    
    labs = labs %>%
        dplyr::filter(ResultName %in% labs_to_use)

    futile.logger::flog.info("done.")
    labs
}

filter_labs_based_on_diagnosis_date = function(labs, outcomes, end_of_window = 365 * 5) {

    tested_or_dx_labels = return_tested_or_dx_labels()

    # browser()
    # For test or dx samples, want to exclude missing test_or_dx_date
    # For controls, all samples will have missing test_or_dx_date, so don't want to exclude these
    date_of_diagnosis = outcomes %>%
        dplyr::select(Deid_ID, `Cohort Type`, DaysSinceBirth_EARLIEST_TEST_OR_DX) %>%
        dplyr::filter((!is.na(DaysSinceBirth_EARLIEST_TEST_OR_DX) & `Cohort Type` %in% tested_or_dx_labels) | (is.na(DaysSinceBirth_EARLIEST_TEST_OR_DX) & !(`Cohort Type` %in% tested_or_dx_labels)))

    labs = labs %>%
        dplyr::inner_join(date_of_diagnosis, by = "Deid_ID") 

    labs = labs %>%
        dplyr::mutate(
            days_before_test_or_dx = DaysSinceBirth_EARLIEST_TEST_OR_DX - CollectionDate_DaysSinceBirth
        )
       
    futile.logger::flog.info(glue::glue("before filtering, we have {nrow(labs)} rows in the labs dataframe"))

    minimum_window = 14 # 14 days at least
    labs = labs %>%
        # tidyr::drop_na(days_before_test_or_dx) %>%
        dplyr::filter(days_before_test_or_dx >= minimum_window | is.na(days_before_test_or_dx))


    labs = labs %>%
        dplyr::filter(days_before_test_or_dx <= end_of_window | is.na(days_before_test_or_dx))

    futile.logger::flog.info(glue::glue("after filtering, we have {nrow(labs)} rows in the labs dataframe"))

    return(labs)

}

get_labs_with_large_sample_sizes = function(labs, sample_size_threshold = 5000) {
    assert_not_empty(labs)

    labs %>%
        dplyr::group_by(ResultName) %>%
        dplyr::summarize(n_samples = length(unique(Deid_ID))) %>%
        dplyr::filter(n_samples >= sample_size_threshold) %>%
        dplyr::pull(ResultName)
}

plot_lab_sample_sizes = function(labs) {
    assert_not_empty(labs)

    counts = labs %>%
        dplyr::group_by(ResultName) %>%
        dplyr::summarize(n_samples = length(unique(Deid_ID)))

    p = ggplot2::ggplot(counts, ggplot2::aes(x = n_samples)) +
        ggplot2::geom_histogram() +
        cowplot::theme_cowplot(font_size = 15) +
        #ggplot2::scale_x_continuous(labels = scales::comma, limits = c(0, max(counts$n_samples) + 1000)) + 
        ggplot2::scale_x_continuous(labels = scales::comma) + 
        ggplot2::labs(x = "sample size")

    fname = file.path(get_output_dir(), "figures", "lab_sample_sizes_histogram.png")

    ggplot2::ggsave(fname, p, type = "cairo", width = 7, height = 5, units = "in")

}

get_labs_with_large_sample_sizes = memoise::memoise(get_labs_with_large_sample_sizes, cache = memoise::cache_filesystem(get_rcache()))

return_tested_or_dx_labels = function() {
   c("Diagnosed", "Tested") 
}

filter_controls = function(outcomes) {
    labels = c("Unmatched Controls", return_tested_or_dx_labels())
    futile.logger::flog.info(glue::glue("before control filtering, we have {nrow(outcomes)} rows in the outcomes dataframe"))
    outcomes = outcomes %>%
        dplyr::filter(`Cohort Type` %in% labels) %>%
        exclude_negative_tests()

    futile.logger::flog.info(glue::glue("after control filtering, we have {nrow(outcomes)} rows in the outcomes dataframe"))

    outcomes
}

exclude_negative_tests = function(outcomes) {
    
    futile.logger::flog.info(glue::glue("before negative test filtering, we have {nrow(outcomes)} rows in the outcomes dataframe"))
    outcomes = outcomes %>%
        dplyr::filter(Outcome != "Tested Negative")

    futile.logger::flog.info(glue::glue("after negative test filtering, we have {nrow(outcomes)} rows in the outomes dataframe"))

    outcomes
}

filter_case_only = function(outcomes) {
    labels = return_tested_or_dx_labels()
    futile.logger::flog.info(glue::glue("before tested/dx filtering, we have {nrow(outcomes)} rows in the outcomes dataframe"))
    outcomes = outcomes %>%
        dplyr::filter(`Cohort Type` %in% labels) %>%
        exclude_negative_tests()

    futile.logger::flog.info(glue::glue("after tested/dx filtering, we have {nrow(outcomes)} rows in the outcomes dataframe"))

    outcomes
}

attach_covariates_outcomes = function(labs, outcomes, type = "prognostic") {

    assert_not_empty(labs)
    assert_not_empty(outcomes)

    valid_types = c("prognostic", "susceptibility")
    type = match.arg(type, valid_types)

    covariates = covariate_list_generator()
    
    if(type == "prognostic") {
        outcomes_to_attach = outcomes %>%
            dplyr::select(Deid_ID, covariate_list_generator(), ICU, Deceased, Hospitalized)
    } else {
        outcomes_to_attach = outcomes %>%
            dplyr::select(Deid_ID, covariate_list_generator(), `Test Results`)
    }

    labs %>%
        dplyr::inner_join(outcomes_to_attach, by = "Deid_ID")
}

prepare_glm_formula = function(covariates) {
    stopifnot(is.character(covariates))
    stopifnot(length(covariates) > 1)

    if(!("Value" %in% covariates)) {
        covariates = c("Value", covariates)
    }

    covar_string = glue::glue_collapse(covariates, " + ")
    formula = as.formula(glue::glue("outcome_value  ~ {covar_string}"))
    return(formula)
}

covariate_list_generator = function() {
    return(
        c(
            "Age",
            "Sex",
            "BMI",
            # "Ethnicity",
            "Race",
            "SmokingStatus",
            "Drinker",
            "popden10",
            "NeighborhoodSocioeconomicDisadvantageIndex",
            "ComorbidityScore"
        )
    )
}


#' expects output from attach_covariates_outcomes
associate_labs_with_outcomes = function(labs, covariates = covariate_list_generator(), type = "prognostic", num_cores = 30) {
    valid_types = c("prognostic", "susceptibility")
    type = match.arg(type, valid_types)

    assert_not_empty(labs)

    futile.logger::flog.info("now computing marginal summary stats")

    n_covar = length(covariate_list_generator())

    # config = sparklyr::spark_config(file = get_spark_yaml(), use_default = FALSE)
    # config$`sparklyr.shell.driver-memory` <- "70G"
    # config$`sparklyr.shell.executor-memory` <- "40G"
    # config$spark.executor.memory <- "10GB"
    # config$spark.driver.memory <- "20GB"

    # config$`sparklyr.cores.local` <- num_cores
    # sc = sparklyr::spark_connect(master = "local", config = config, version = "3.0.0")
    # sc$config[['sparklyr.shell.driver-memory']] = "30g"
    # sc$config[['sparklyr.connect.cores.local']] = num_cores
    # sc$config[['spark.sql.shuffle.partitions.local']] = num_cores
    # sc$config[['spark.executor.memory']] = "10g"

    # future::plan(future::multiprocess, workers = num_cores)


    # browser()
    if(type == "prognostic") {
        labs = labs %>%
            tidyr::gather(key = outcome_type, value = outcome_value, ICU:Hospitalized) %>%
            dplyr::mutate(
                outcome_value = ifelse(is.na(outcome_value), FALSE, outcome_value),
                outcome_value = as.integer(outcome_value)
            ) 
    } else {
        labs = labs %>%
            dplyr::rename(outcome_value = `Test Results`) %>%
            dplyr::mutate(
                outcome_type = "susceptibility", 
                outcome_value = ifelse(is.na(outcome_value), 0, outcome_value)
            ) 
    }

    # labs_tbl = sparklyr::copy_to(sc, labs, name = "labs", repartition = num_cores, overwrite = TRUE)

    # lab_summary = labs_tbl %>%
    lab_summary = labs %>%
        dplyr::filter(!is.na(Age) & !is.na(Sex) & !is.na(Value)) %>%
        dplyr::group_by(ResultName, outcome_type) %>%
        dplyr::summarize(n_samples = dplyr::n(), n_cases = sum(outcome_value)) %>%
        dplyr::filter(n_samples >= 300 & n_cases >= 20) %>%
        dplyr::ungroup(.)
        
    futile.logger::flog.info("now filtering by case count")

    # labs_tbl = labs_tbl %>%
    labs_tbl = labs %>%
        dplyr::inner_join(lab_summary, by = c("ResultName", "outcome_type")) 
        # sparklyr::copy_to(sc, ., repartition = num_cores, overwrite = TRUE)

    # summary_stats = labs %>%

    futile.logger::flog.info("now running logistic regression by lab and outcome")

    glm_formula = prepare_glm_formula(covariates)
    possibly_glm = purrr::possibly(glm, otherwise = NULL)
    possibly_tidy = purrr::possibly(broom::tidy, otherwise = NULL)

    # browser()

    # summary_stats = labs_tbl %>%
    #     sparklyr::sdf_repartition(partitions = 20, partition_by = c("ResultName", "outcome_type")) %>%
    #     sparklyr::spark_apply(function(x) {
    #
    #
    #             # model = possibly_glm(outcome_value ~ Value + Age + Sex, data = x, family = binomial(), model = FALSE)
    #
    #             stopifnot(nrow(x) > 10)
    #             print(glue::glue("x has {nrow(x)} rows"))
    #             print(head(x, 1))
    #
    #             # model = glm(outcome_value ~ Value + Age + Sex, data = x, family = binomial(), model = FALSE)
    #             model = glm(glm_formula, data = x, family = binomial(), model = FALSE)
    #             tidy_model = broom::tidy(model, conf.int = FALSE)
    #
    #             tidy_model$n_samples = x$n_samples[1]
    #             tidy_model$n_cases = x$n_cases[1]
    #
    #             return(tidy_model)
    #         },
    #         names = c("term", "estimate", "std.error", "statistic", "p.value", "n_samples", "n_cases"),
    #         group_by = c("ResultName", "outcome_type")
    #     ) %>%
    #     dplyr::collect(.) %>%
    #     dplyr::rename(beta = estimate, se = std.error, pvalue = p.value)
    future::plan(future::multiprocess, workers = num_cores)

    summary_stats = labs_tbl %>%
        dplyr::group_by(ResultName, outcome_type) %>%
        tidyr::nest(.) %>%
        dplyr::mutate(
            n_samples = purrr::map_int(data, nrow),
            n_cases = purrr::map_int(data, ~sum(as.integer(.x$outcome_value))),
            tidy_model = furrr::future_map(data, ~possibly_glm(glm_formula, data = .x, family = binomial()) %>% possibly_tidy(conf.int = TRUE))
            # tidy_model = furrr::future_map(model, ~possibly_tidy(.x, conf.int = TRUE)),
            # tidy_model = purrr::map(model, ~possibly_tidy(.x, conf.int = TRUE)),
        ) %>%
        dplyr::ungroup(.)

    futile.logger::flog.info("now summarizing marginal summary stats")

    future::plan(future::sequential)

    # browser()

    summary_stats = summary_stats %>%
        dplyr::mutate(
            results_are_null = purrr::map_lgl(tidy_model, is.null)
        ) %>%
        dplyr::filter(!results_are_null) %>%
        # dplyr::mutate(result_rows = purrr::map_int(tidy_model, nrow)) %>%
        # dplyr::filter(result_rows == (n_covar + 1)) %>%
        # dplyr::mutate(
        #     beta = purrr::map_dbl(tidy_model, ~{.x %>% dplyr::filter(term == "Value") %>% dplyr::pull(estimate)}),
        #     se = purrr::map_dbl(tidy_model, ~{.x %>% dplyr::filter(term == "Value") %>% dplyr::pull(std.error)}),
        #     pvalue = purrr::map_dbl(tidy_model, ~{.x %>% dplyr::filter(term == "Value") %>% dplyr::pull(p.value)}),
        # )
        tidyr::unnest(tidy_model) %>%
        dplyr::rename(beta = estimate, se = std.error, pvalue = p.value)

    futile.logger::flog.info("done computing marginal summary stats")

    summary_stats
}

get_output_dir = function() {
    "/net/mgi/covid_labWAS/output"
}

get_prognostic_output_dir = function() {
    file.path(get_output_dir(), "prognostic")
}

get_susceptibility_output_dir = function() {
    file.path(get_output_dir(), "susceptibility")
}

write_summary_stats_output = function(df, output_dir) {
    assert_not_empty(df)
    
    job_name = return_global_job_name()

    futile.logger::flog.info("now writing output")

    date = stringr::str_replace_all(Sys.Date(), "-", "_")
    fname = file.path(output_dir, glue::glue("covid_labwas_marginal_summary_stats_{date}_{job_name}.tsv"))
    readr::write_tsv(df, fname)

    futile.logger::flog.info("done")

    invisible(df)
}

return_date_format = function() {
    date = stringr::str_replace_all(Sys.Date(), "-", "_")
    return(date)
}

cache_lab_data_into_rds = function(labs, output_dir) {
    cache_dir = file.path(output_dir, "cache")
    job_name = return_global_job_name()
    date = return_date_format()
    fname = file.path(cache_dir, glue::glue("labs_{date}_{job_name}.rds"))
    saveRDS(labs, fname)
}

write_config = function(output_dir) {
    date = return_date_format()

    config = list(
        "covariates" = covariate_list_generator(),
        "date"       = date,
        "lab_data_file_name" = get_lab_data_location(),
        "outcome_data_file_name" = get_outcome_data_location()
    )

    config_dir = file.path(output_dir, "config")
    
    job_name = return_global_job_name()
    fname = file.path(config_dir, glue::glue("config_{date}_{job_name}.json"))
    jsonlite::write_json(config, fname)
    invisible(config)
}

create_global_job_name = function() {
    ._job_name <<- stringi::stri_rand_strings(1, 5, '[A-Z]')
}

return_global_job_name = function() {
    return(._job_name)
}


main_prognostic = function() {
    
    output_dir = get_prognostic_output_dir()

    create_global_job_name()
    
    outcomes = read_in_outcome_data() %>%
        filter_case_only()

    labs = read_in_lab_data() %>%
        filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * 5) %>%
        summarize_lab_data()

    summary_stats = labs %>%
        attach_covariates_outcomes(outcomes, type = "prognostic") %>%
        tidyr::drop_na(covariate_list_generator()) %>%
        filter_to_labs_with_large_sample_sizes(500) %>%
        associate_labs_with_outcomes(type = "prognostic")

    write_summary_stats_output(
        summary_stats %>% dplyr::select(ResultName, outcome_type, term, n_samples, n_cases, beta, se, pvalue),
        output_dir
    )

    cache_lab_data_into_rds(labs, output_dir)

    write_config(output_dir)

    phewas_plot(summary_stats, output_dir)
    forest_plot(summary_stats, output_dir)


    return(summary_stats)
}   

main_susceptibility = function() {
    output_dir = get_susceptibility_output_dir()

    create_global_job_name()
    
    outcomes = read_in_outcome_data() %>%
        filter_controls()

    labs = read_in_lab_data() %>%
        filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * 5) %>%
        summarize_lab_data()

    summary_stats = labs %>%
        attach_covariates_outcomes(outcomes, type = "susceptibility") %>%
        tidyr::drop_na(covariate_list_generator()) %>%
        filter_to_labs_with_large_sample_sizes(1000) %>%
        associate_labs_with_outcomes(type = "susceptibility")

    write_summary_stats_output(
        summary_stats %>% dplyr::select(ResultName, outcome_type, term, n_samples, n_cases, beta, se, pvalue),
        output_dir
    )

    cache_lab_data_into_rds(labs, output_dir)

    write_config(output_dir)

    phewas_plot(summary_stats, output_dir)
    forest_plot(summary_stats, output_dir)

    return(summary_stats)
}
