get_spark_yaml = function() {
     "/net/mgi/covid_labWAS/scripts/covidLabWASanalysis/config-template.yml"
}

create_order_lookup = function(labs) {
    labs %>%
        dplyr::distinct(OrderName, ResultName)
}

phewas_plot = function(summary_stats) {
    outcome_types = unique(summary_stats$outcome_type)

    purrr::walk(outcome_types, ~phewas_plot_(summary_stats, .x))
}

phewas_plot_ = function(summary_stats, .outcome_type = "ICU") {
    triangle_up = 24
    triangle_down = 25

    p = summary_stats %>%
        dplyr::filter(term == "Value" & outcome_type == .outcome_type) %>%
        dplyr::mutate(shape = ifelse(beta > 0, triangle_up, triangle_down)) %>%
        ggplot2::ggplot(ggplot2::aes(x = ResultName, y = -log10(pvalue), shape = shape)) +
        ggplot2::geom_point() + 
        cowplot::theme_cowplot(font_size = 9) + 
        ggplot2::scale_shape_identity() + 
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.1, hjust=1),
            axis.title.x = ggplot2::element_blank()
        ) +
        ggplot2::ggtitle(glue::glue("LabWAS of {.outcome_type} status"))

    
    date = stringr::str_replace_all(Sys.Date(), "-", "_")
    ggplot2::ggsave(
        filename = file.path(get_output_dir(), "figures", glue::glue("labwas_{.outcome_type}_{date}.pdf")),
        plot = p,
        units = "in",
        width = 10,
        height = 6
    )
    ggplot2::ggsave(
        filename = file.path(get_output_dir(), "figures", glue::glue("labwas_{.outcome_type}_{date}.tiff")),
        plot = p,
        dpi = 300,
        units = "in",
        width = 10,
        height = 6,
        type = "cairo"
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
        dplyr::select(Deid_ID, ResultName, Value, Unit, Range, HILONORMAL = HILONORMAL_Flag) %>%
        sparklyr::collect(.)

    futile.logger::flog.info("done.")

    lab_summary
}

get_rcache = function() {
    "/net/mgi/covid_labWAS/.rcache"
}

summarize_lab_data = memoise::memoise(summarize_lab_data, cache = memoise::cache_filesystem(get_rcache()))

filter_to_labs_with_large_sample_sizes = function(labs) {
    assert_not_empty(labs)

    futile.logger::flog.info("filtering now to most common labs")

    labs_to_use = get_labs_with_large_sample_sizes(labs)
    
    labs = labs %>%
        dplyr::filter(ResultName %in% labs_to_use)

    futile.logger::flog.info("done.")
    labs
}

get_labs_with_large_sample_sizes = function(labs, sample_size_threshold = 5000) {
    assert_not_empty(labs)

    labs %>%
        dplyr::group_by(ResultName) %>%
        dplyr::summarize(n_samples = length(unique(Deid_ID))) %>%
        dplyr::filter(n_samples >= sample_size_threshold) %>%
        dplyr::pull(ResultName)
}

get_labs_with_large_sample_sizes = memoise::memoise(get_labs_with_large_sample_sizes, cache = memoise::cache_filesystem(get_rcache()))

filter_controls = function(outcomes) {
    label = "Unmatched Controls"
    futile.logger::flog.info(glue::glue("before unmatched control filtering, we have {nrow(outcomes)} samples"))
    outcomes = outcomes %>%
        dplyr::filter(Outcome != label)

    futile.logger::flog.info(glue::glue("after unmatched control filtering, we have {nrow(outcomes)} samples"))

    outcomes
}

attach_covariates_outcomes = function(labs, outcomes) {

    assert_not_empty(labs)
    assert_not_empty(outcomes)
    
    outcomes_to_attach = outcomes %>%
        dplyr::select(Deid_ID, Age, Sex, ICU, Deceased, Hospitalized)

    labs %>%
        dplyr::inner_join(outcomes_to_attach, by = "Deid_ID")
}


#' expects output from attach_covariates_outcomes
associate_labs_with_outcomes = function(labs, num_cores = 30) {

    futile.logger::flog.info("now computing marginal summary stats")

    n_covar = 3

    # config = sparklyr::spark_config()
    # config$`sparklyr.shell.driver-memory` <- "30G"
    # config$`sparklyr.shell.executor-memory` <- "20G"
    # config$`sparklyr.cores.local` <- num_cores
    # config$`sparklyr.shell.driver-memory` <- "70G"
    # config$`sparklyr.shell.executor-memory` <- "40G"
    # config$spark.executor.memory <- "10GB"
    # config$spark.driver.memory <- "30GB"
    # config$sparklyr.connect.timeout <- 60 * 5
    # config$sparklyr.log.console <- TRUE
    # sc = sparklyr::spark_connect(master = "local", config = config, version = "3.0.0")
    config = sparklyr::spark_config(file = get_spark_yaml(), use_default = FALSE)
    # config$`sparklyr.shell.driver-memory` <- "70G"
    # config$`sparklyr.shell.executor-memory` <- "40G"
    # config$spark.executor.memory <- "10GB"
    # config$spark.driver.memory <- "20GB"

    # config$`sparklyr.cores.local` <- num_cores
    sc = sparklyr::spark_connect(master = "local", config = config, version = "3.0.0")
    # sc$config[['sparklyr.shell.driver-memory']] = "30g"
    # sc$config[['sparklyr.connect.cores.local']] = num_cores
    # sc$config[['spark.sql.shuffle.partitions.local']] = num_cores
    # sc$config[['spark.executor.memory']] = "10g"

    # future::plan(future::multiprocess, workers = num_cores)


    labs = labs %>%
        tidyr::gather(key = outcome_type, value = outcome_value, ICU:Hospitalized) %>%
        dplyr::mutate(
            outcome_value = ifelse(is.na(outcome_value), FALSE, outcome_value),
            outcome_value = as.integer(outcome_value)
        ) 

    labs_tbl = sparklyr::copy_to(sc, labs, name = "labs", repartition = num_cores, overwrite = TRUE)

    lab_summary = labs_tbl %>%
        dplyr::filter(!is.na(Age) & !is.na(Sex) & !is.na(Value)) %>%
        dplyr::group_by(ResultName, outcome_type) %>%
        dplyr::summarize(n_samples = dplyr::n(), n_cases = sum(outcome_value)) %>%
        dplyr::filter(n_samples >= 300 & n_cases >= 20) %>%
        dplyr::ungroup(.)
        
    futile.logger::flog.info("now filtering by case count")

    labs_tbl = labs_tbl %>%
        dplyr::inner_join(lab_summary, by = c("ResultName", "outcome_type")) 
        # sparklyr::copy_to(sc, ., repartition = num_cores, overwrite = TRUE)

    # summary_stats = labs %>%

    futile.logger::flog.info("now running logistic regression by lab and outcome")

    summary_stats = labs_tbl %>%
        sparklyr::spark_apply(function(x) {

                # possibly_glm = purrr::possibly(glm, otherwise = NULL)
                # possibly_tidy = purrr::possibly(broom::tidy, otherwise = NULL)

                # model = possibly_glm(outcome_value ~ Value + Age + Sex, data = x, family = binomial(), model = FALSE)
                print(glue::glue("x has {nrow(x)} rows"))

                model = glm(outcome_value ~ Value + Age + Sex, data = x, family = binomial(), model = FALSE)
                tidy_model = broom::tidy(model, conf.int = FALSE)

                tidy_model$n_samples = x$n_samples[1]
                tidy_model$n_cases = x$n_cases[1]

                return(tidy_model)
            },
            names = c("term", "estimate", "std.error", "statistic", "p.value", "n_samples", "n_cases"),
            group_by = c("ResultName", "outcome_type")
        ) %>%
        dplyr::collect(.) %>%
        dplyr::rename(beta = estimate, se = std.error, pvalue = p.value)

        # tidyr::nest(.) %>%
        # dplyr::mutate(
        #     n_samples = purrr::map_int(data, nrow),
        #     n_cases = purrr::map_int(data, ~sum(.x$outcome_value)),
        #     tidy_model = furrr::future_map(data, ~possibly_glm(outcome_value ~ Value + Age + Sex, data = .x, family = binomial(), model = FALSE, x = FALSE, y = FALSE) %>% possibly_tidy(conf.int = TRUE))
        #     # tidy_model = furrr::future_map(model, ~possibly_tidy(.x, conf.int = TRUE)),
        #     # tidy_model = purrr::map(model, ~possibly_tidy(.x, conf.int = TRUE)),
        # ) %>%
        # dplyr::ungroup(.)

    # futile.logger::flog.info("now summarizing marginal summary stats")

    # future::plan(sequential)

    # summary_stats = summary_stats %>%
    #     dplyr::mutate(
    #         results_are_null = purrr::map_lgl(tidy_model, is.null)
    #     ) %>%
    #     dplyr::filter(!results_are_null) %>%
    #     dplyr::mutate(result_rows = purrr::map_int(tidy_model, nrow)) %>%
    #     dplyr::filter(result_rows == (n_covar + 1)) %>%
    #     dplyr::mutate(
    #         beta = purrr::map_dbl(tidy_model, ~{.x %>% dplyr::filter(term == "Value") %>% dplyr::pull(estimate)}),
    #         se = purrr::map_dbl(tidy_model, ~{.x %>% dplyr::filter(term == "Value") %>% dplyr::pull(std.error)}),
    #         pvalue = purrr::map_dbl(tidy_model, ~{.x %>% dplyr::filter(term == "Value") %>% dplyr::pull(p.value)}),
    #     )

    futile.logger::flog.info("done computing marginal summary stats")

    summary_stats
}

get_output_dir = function() {
    "/net/mgi/covid_labWAS/output"
}

write_summary_stats_output = function(df) {
    assert_not_empty(df)
    
    output_dir = get_output_dir()

    futile.logger::flog.info("now writing output")

    date = stringr::str_replace_all(Sys.Date(), "-", "_")
    fname = file.path(output_dir, glue::glue("covid_labwas_marginal_summary_stats_{date}.tsv"))
    readr::write_tsv(df, fname)

    futile.logger::flog.info("done")

    invisible(df)
}


main = function() {
    
    outcomes = read_in_outcome_data() %>%
        filter_controls

    summary_stats = read_in_lab_data() %>%
        summarize_lab_data() %>%
        filter_to_labs_with_large_sample_sizes() %>%
        attach_covariates_outcomes(outcomes) %>%
        associate_labs_with_outcomes

    write_summary_stats_output(
        summary_stats %>% dplyr::select(ResultName, outcome_type, term, n_samples, n_cases, beta, se, pvalue)
    )

    return(summary_stats)
}
