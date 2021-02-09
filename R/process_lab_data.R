get_spark_yaml = function() {
     "/net/mgi/covid_labWAS/scripts/covidLabWASanalysis/config-template.yml"
}

create_order_lookup = function(labs) {
    labs %>%
        dplyr::distinct(OrderName, ResultName)
}

create_supplement = function(summary_stats, output_dir) {

    summary_stats = filter_summary_stats_to_labs(summary_stats) %>%   
        dplyr::select(ResultName, outcome_type, term, n_samples, n_cases, beta, se, pvalue) %>%
        dplyr::mutate(
            dplyr::across(c(beta, se), ~scales::number(.x, accuracy = 0.01)),
            pvalue = scales::scientific(pvalue)
        )

    hospitalized = summary_stats %>%
        dplyr::filter(outcome_type == "Hospitalized") %>%
        dplyr::select(-outcome_type, -term)

    icu = summary_stats %>%
        dplyr::filter(outcome_type == "ICU") %>%
        dplyr::select(-outcome_type, -term)

    deceased = summary_stats %>%
        dplyr::filter(outcome_type == "Deceased") %>%
        dplyr::select(-outcome_type, -term)

    readr::write_tsv(hospitalized, file.path(output_dir, "hospitalized_supplement.tsv"))
    readr::write_tsv(icu, file.path(output_dir, "icu_supplement.tsv"))
    readr::write_tsv(deceased, file.path(output_dir, "deceased_supplement.tsv"))

}

summarize_lab_data = function(labs, inverse_normal_transform = TRUE, indicator = FALSE) {
    # takes 2 minutes without spark or previous cache
    assert_not_empty(labs)
    stopifnot(inverse_normal_transform != indicator)

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

    if(indicator) {
        # browser()
        lab_summary = labs %>%
            dplyr::mutate(Value = as.numeric(readr::parse_number(Value))) %>%
            tidyr::drop_na(Value) %>%
            dplyr::group_by(Deid_ID, ResultName) %>%
            dplyr::arrange(desc(CollectionDate_DaysSinceBirth)) %>% # descending order
            dplyr::summarize(
                Value = dplyr::n() >= 1, # at least 1 measurement
                Unit = dplyr::first(Unit),
                Range = dplyr::first(Range),
                HILONORMAL = dplyr::first(HILONORMAL_Flag),
                days_before_test_or_dx = dplyr::first(days_before_test_or_dx)
            ) %>%
            dplyr::ungroup(.)

        lab_summary = dplyr::right_join(lab_summary, tidyr::expand(lab_summary, Deid_ID, ResultName)) %>%
            dplyr::mutate(Value = dplyr::if_else(is.na(Value), FALSE, Value))
    } else {
        lab_summary = labs %>%
            dplyr::mutate(Value = as.numeric(readr::parse_number(Value))) %>%
            tidyr::drop_na(Value) %>%
            dplyr::group_by(Deid_ID, ResultName) %>%
            dplyr::arrange(desc(CollectionDate_DaysSinceBirth)) %>% # descending order
            dplyr::slice(1) %>% # take most recent value
            dplyr::ungroup(.) %>%
            dplyr::select(Deid_ID, ResultName, Value, Unit, Range, HILONORMAL = HILONORMAL_Flag, days_before_test_or_dx) 
        # sparklyr::collect(.)
    }

    # browser()
    if(inverse_normal_transform) {
        lab_summary = lab_summary %>%
            dplyr::group_by(ResultName) %>%
            dplyr::mutate(Value = inverse_normalize(Value)) %>%
            dplyr::ungroup(.)
    }

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

    futile.logger::flog.info(glue::glue("after negative test filtering, we have {nrow(outcomes)} rows in the outcomes dataframe"))

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

    valid_types = c("prognostic", "ordinal_prognostic", "susceptibility")
    type = match.arg(type, valid_types)

    covariates = covariate_list_generator()
    
    if(type %in% c("prognostic", "ordinal_prognostic")) {
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

ordinal_covariate_list_generator = function() {
    return(
        c(
            "Age",
            "Sex",
            "BMI",
            # "Ethnicity",
            "popden10",
            "NeighborhoodSocioeconomicDisadvantageIndex",
            "ComorbidityScore"
        )
    )
}


#' expects output from attach_covariates_outcomes
associate_labs_with_outcomes = function(labs, covariates = covariate_list_generator(), type = "prognostic", num_cores = 30) {
    valid_types = c("prognostic", "susceptibility", "ordinal_prognostic")
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
    } else if(type == "ordinal_prognostic") {
        labs = labs %>%
            dplyr::mutate(
                # outcome_value = case_when(
                #    as.integer(Deceased) == 1 ~ "Deceased", 
                #    as.integer(ICU) == 1 ~ "ICU", 
                #    as.integer(Hospitalized) == 1 ~ "Hospitalized", 
                #    TRUE ~ "None"
                # ),
                outcome_value = case_when(
                   as.integer(Deceased) == 1 | as.integer(ICU) == 1 ~ "ICU or Deceased", 
                   as.integer(Hospitalized) == 1 ~ "Hospitalized", 
                   TRUE ~ "None"
                ),
                # outcome_value = factor(
                #     outcome_value,
                #     ordered = TRUE,
                #     levels = c("None", "Hospitalized", "ICU", "Deceased")
                # ),
                outcome_value = factor(
                    outcome_value,
                    ordered = TRUE,
                    levels = c("None", "Hospitalized", "ICU or Deceased")
                ),
                outcome_type = "ordinal_prognostic"
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
    if(type == "ordinal_prognostic") {
        lab_summary = labs %>%
            dplyr::filter(!is.na(Age) & !is.na(Sex) & !is.na(Value)) %>%
            dplyr::group_by(ResultName, outcome_type) %>%
            dplyr::summarize(n_samples = dplyr::n(), n_cases = sum(outcome_value != "None")) %>%
            dplyr::filter(n_samples >= 300 & n_cases >= 20) %>%
            dplyr::ungroup(.)
    } else {
        lab_summary = labs %>%
            dplyr::filter(!is.na(Age) & !is.na(Sex) & !is.na(Value)) %>%
            dplyr::group_by(ResultName, outcome_type) %>%
            dplyr::summarize(n_samples = dplyr::n(), n_cases = sum(outcome_value)) %>%
            dplyr::filter(n_samples >= 300 & n_cases >= 20) %>%
            dplyr::ungroup(.)
    }
        
    futile.logger::flog.info("now filtering by case count")

    # labs_tbl = labs_tbl %>%
    labs_tbl = labs %>%
        dplyr::inner_join(lab_summary, by = c("ResultName", "outcome_type")) 
        # sparklyr::copy_to(sc, ., repartition = num_cores, overwrite = TRUE)

    # summary_stats = labs %>%

    futile.logger::flog.info("now running logistic regression by lab and outcome")

    glm_formula = prepare_glm_formula(covariates)

    if(type == "ordinal_prognostic") {
        possibly_regression = purrr::possibly(ordinal::clm, otherwise = NULL)
        possibly_regression = purrr::partial(possibly_regression, control = ordinal::clm.control(method = "optim")) # method = "Newton" is the default, frequently does not converge
    } else {
        possibly_regression = purrr::possibly(glm, otherwise = NULL)
        possibly_regression = purrr::partial(possibly_regression, family = binomial())
    }

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
        dplyr::group_by(ResultName, outcome_type, n_samples, n_cases) %>%
        tidyr::nest(.) %>%
        dplyr::mutate(
            # n_samples = purrr::map_int(data, nrow),
            # n_cases = purrr::map_int(data, ~sum(as.integer(.x$outcome_value))),
            tidy_model = furrr::future_map(data, ~possibly_regression(glm_formula, data = .x) %>% possibly_tidy(conf.int = TRUE))
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

possibly_initalize_dir = function(dir) {
    if(!dir.exists(dir)) {
        dir.create(dir)
        dir.create(file.path(dir, "figures"))
        dir.create(file.path(dir, "config"))
        dir.create(file.path(dir, "cache"))
    }
}

get_indicator_prognostic_output_dir = function() {
    file.path(get_output_dir(), "indicator_prognostic")
}

get_ordinal_prognostic_output_dir = function() {
    file.path(get_output_dir(), "ordinal_prognostic")
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

write_config = function(output_dir, covariates = covariate_list_generator(), inverse_normalize, years_back) {
    date = return_date_format()

    config = list(
        "covariates" = covariates,
        "date"       = date,
        "inverse_normal" = inverse_normalize,
        "lab_data_file_name" = get_lab_data_location(),
        "years_back" = years_back,
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


main_prognostic = function(years_back = 5L) {

    stopifnot(is.wholenumber(years_back))

    output_dir = get_prognostic_output_dir()
    INVERSE_NORMALIZE_FLAG = TRUE

    create_global_job_name()
    
    outcomes = read_in_outcome_data() %>%
        filter_case_only()

    labs = read_in_lab_data() %>%
        filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * years_back) %>%
        summarize_lab_data(INVERSE_NORMALIZE_FLAG)

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

    write_config(output_dir, inverse_normalize = INVERSE_NORMALIZE_FLAG, years_back = years_back)

    phewas_plot(summary_stats, output_dir)
    forest_plot(summary_stats, output_dir)

    return(summary_stats)
}   

main_indicator_prognostic = function(years_back = 5L) {

    stopifnot(is.wholenumber(years_back))
    
    output_dir = get_indicator_prognostic_output_dir()
    INVERSE_NORMALIZE_FLAG = FALSE

    create_global_job_name()
    
    outcomes = read_in_outcome_data() %>%
        filter_case_only()

    labs = read_in_lab_data() %>%
        # filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * 5) %>%
        filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * years_back) %>%
        filter_to_labs_with_large_sample_sizes(500) %>% # labs with at least 500 COVID cases, subsetting here to speed things up
        summarize_lab_data(INVERSE_NORMALIZE_FLAG, indicator = TRUE)

    expanded_labs = outcomes %>%
        dplyr::left_join(labs, by = "Deid_ID") %>%
        tidyr::expand(Deid_ID, ResultName) 

    labs = labs %>%
        dplyr::right_join(expanded_labs, by = c("Deid_ID", "ResultName")) %>%
        dplyr::mutate(Value = dplyr::if_else(is.na(Value), FALSE, Value))

    summary_stats = labs %>%
        attach_covariates_outcomes(outcomes, type = "prognostic") %>%
        tidyr::drop_na(covariate_list_generator()) %>%
        filter_to_labs_with_large_sample_sizes(500) %>% # labs with at least 500 COVID cases with complete covariate information, a subset of the above filtering
        associate_labs_with_outcomes(type = "prognostic")

    write_summary_stats_output(
        summary_stats %>% dplyr::select(ResultName, outcome_type, term, n_samples, n_cases, beta, se, pvalue),
        output_dir
    )

    cache_lab_data_into_rds(labs, output_dir)

    write_config(output_dir, inverse_normalize = INVERSE_NORMALIZE_FLAG, years_back = years_back)

    phewas_plot(summary_stats, output_dir)
    forest_plot(summary_stats, output_dir)


    return(summary_stats)
}   

main_susceptibility = function(years_back = 5L) {

    stopifnot(is.wholenumber(years_back))

    output_dir = get_susceptibility_output_dir()
    INVERSE_NORMALIZE_FLAG = TRUE

    create_global_job_name()
    
    outcomes = read_in_outcome_data() %>%
        filter_controls()

    labs = read_in_lab_data() %>%
        filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * years_back) %>%
        summarize_lab_data(INVERSE_NORMALIZE_FLAG)

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

    write_config(output_dir, inverse_normalize = INVERSE_NORMALIZE_FLAG, years_back = years_back)

    phewas_plot(summary_stats, output_dir)
    forest_plot(summary_stats, output_dir)

    return(summary_stats)
}

main_ordinal_prognostic = function() {

    output_dir = get_ordinal_prognostic_output_dir()
    INVERSE_NORMALIZE_FLAG = TRUE

    create_global_job_name()
    
    outcomes = read_in_outcome_data() %>%
        filter_case_only()

    labs = read_in_lab_data() %>%
        filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * 5) %>%
        summarize_lab_data(INVERSE_NORMALIZE_FLAG)

    summary_stats = labs %>%
        attach_covariates_outcomes(outcomes, type = "ordinal_prognostic") %>%
        tidyr::drop_na(ordinal_covariate_list_generator()) %>%
        filter_to_labs_with_large_sample_sizes(500) %>%
        associate_labs_with_outcomes(covariates = ordinal_covariate_list_generator(), type = "ordinal_prognostic")

    write_summary_stats_output(
        summary_stats %>% dplyr::select(ResultName, outcome_type, term, n_samples, n_cases, beta, se, pvalue),
        output_dir
    )

    cache_lab_data_into_rds(labs, output_dir)

    write_config(output_dir, ordinal_covariate_list_generator(), inverse_normalize = INVERSE_NORMALIZE_FLAG)

    phewas_plot(summary_stats, output_dir, ordinal_covariate_list_generator())
    forest_plot(summary_stats, output_dir, ordinal_covariate_list_generator())

    return(summary_stats)

}
