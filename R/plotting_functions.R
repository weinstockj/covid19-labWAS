phewas_plot = function(summary_stats, output_dir, covariates = covariate_list_generator()) {
    outcome_types = unique(summary_stats$outcome_type)

    purrr::walk(outcome_types, ~phewas_plot_(
                                summary_stats, 
                                .x, 
                                covariates = covariates,
                                output_dir = output_dir
        )
    )
}

forest_plot = function(summary_stats, output_dir, covariates = covariate_list_generator()) {
    outcome_types = unique(summary_stats$outcome_type)

    purrr::walk(outcome_types, ~forest_plot_(
                                summary_stats, 
                                .x, 
                                covariates = covariates,
                                output_dir = output_dir
        )
    )
}

phewas_plot_ = function(summary_stats, .outcome_type = "ICU", covariates = covariate_list_generator(), output_dir) {
    triangle_up = 24
    triangle_down = 25

    covariate_string = paste("Covariates include:", glue::glue_collapse(covariates, " + "))

    n_labs = length(unique(summary_stats$ResultName))
    if(.outcome_type == "susceptibility") {
        n_tests = 1
    } else {
        n_tests = 3 # ICU / Hospitalized / Deceased
    }
    bonferonni = -log10(.05 / (n_labs * n_tests))

    summary_stats = summary_stats %>%
        filter_summary_stats_to_labs %>%
        dplyr::filter(outcome_type == .outcome_type)

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

forest_plot_ = function(summary_stats, .outcome_type = "ICU", covariates = covariate_list_generator(), output_dir, filter_significance = TRUE) {
    covariate_string = paste("Covariates include:", glue::glue_collapse(covariates, " + "))

    n_labs = length(unique(summary_stats$ResultName))
    if(.outcome_type == "susceptibility") {
        n_tests = 1
    } else {
        n_tests = 3 # ICU / Hospitalized / Deceased
    }
    bonferroni = -log10(.05 / (n_labs * n_tests))

    date = stringr::str_replace_all(Sys.Date(), "-", "_")
    job_name = return_global_job_name()

    # labs are on a widly different scale than the others
    exclude_labs = c(
        # "Absolute Early Gran Count",
        # "Absolute Basophil Count",
        "Specific Gravity (Urine)",
        "Specific Gravity, Urine",
        # "LIPASE LEVEL",
        "Estimated GFR, Black",
        "Estimated GFR, Non-Black"
    )


    summary_stats = summary_stats %>%
        filter_summary_stats_to_labs %>%
        dplyr::filter(outcome_type == .outcome_type) %>%
        dplyr::filter(!(ResultName %in% exclude_labs)) %>%
        dplyr::mutate(ResultName = stringr::str_replace_all(ResultName, " CHOL", "")) %>%
        dplyr::mutate(`pvalue < bonferroni` = pvalue < .05 / (n_labs * n_tests))

    if(filter_significance) {
        summary_stats = summary_stats %>%
            dplyr::filter(pvalue < .05)
    }

    xlim_max = max(exp(summary_stats$conf.high)) * 1.05

    p = summary_stats %>%
        dplyr::mutate(ResultName = forcats::fct_reorder(ResultName, exp(beta), .desc = FALSE)) %>%
        ggplot2::ggplot(ggplot2::aes(y = ResultName, x = exp(beta), xmin = exp(conf.low), xmax = exp(conf.high))) +
        # ggplot2::ggplot(ggplot2::aes(y = ResultName, x = exp(beta), xmin = exp(conf.low), xmax = exp(conf.high), color = `pvalue < bonferroni`)) +
        #ggplot2::coord_flip() +
        cowplot::theme_cowplot(font_size = 14) + 
        ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(color = "black", size = 1),
            legend.position = "bottom"
        ) +
        cowplot::background_grid() +
        # ggplot2::xlim(0, 6.5) +
        ggplot2::xlim(0.0, log2(xlim_max)) +
        ggplot2::scale_x_continuous(trans = "log2", breaks = scales::pretty_breaks(n = 6)) +
        ggplot2::labs(x = "OR (95% CI)") +
        ggplot2::geom_vline(xintercept = 1, colour = "black", size = 1.2, alpha = .7) +
        ggplot2::geom_pointrange() 
        # ggplot2::ggtitle(glue::glue("LabWAS of {.outcome_type} status"))

    beta_table = summary_stats %>%
        dplyr::mutate(ResultName = forcats::fct_reorder(ResultName, exp(beta), .desc = FALSE)) %>%
        add_CI_label %>%
        ggplot2::ggplot(data = ., ggplot2::aes(y = ResultName)) +
            ggplot2::geom_text(ggplot2::aes(x = .01, label = ResultName, hjust = 0)) +
            ggplot2::geom_text(ggplot2::aes(x = 3.5, label = ci_label)) +
            ggplot2::theme_void() +
            ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")) +
            ggplot2::xlim(0, 4) +
            ggplot2::annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = 3.0, xend = 4.0, size = 1) +
            ggplot2::ggtitle("                                                                    OR (95% CI)")
            # ggplot2::annotation_custom(grid::textGrob("OR (95% CI)"), xmin = 3, xmax = 4, ymin = 40, ymax = 40)
            # cowplot::theme_minimal_hgrid()

    pvalue_table = summary_stats %>%
        dplyr::mutate(ResultName = forcats::fct_reorder(ResultName, exp(beta), .desc = FALSE)) %>%
        dplyr::mutate(
            pvalue_label = scales::scientific(pvalue, digits = 2),
            pvalue_label = dplyr::if_else(`pvalue < bonferroni`, glue::glue("{pvalue_label}*"), pvalue_label)
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
        # height = 12
        # height = 20
        height = as.integer(nrow(summary_stats) * .38)
    )
}

add_CI_label = function(summary_stats) {

    assert_not_empty(summary_stats)

    summary_stats %>%
        dplyr::mutate(
           beta = scales::number(exp(beta), accuracy = .01), 
           conf.low = scales::number(exp(conf.low), accuracy = .01), 
           conf.high = scales::number(exp(conf.high), accuracy = .01),
           ci_label = glue::glue("{beta} ({conf.low}-{conf.high})")
        )
}

filter_summary_stats_to_labs = function(summary_stats) {

    assert_not_empty(summary_stats)
    stopifnot("term" %in% names(summary_stats))
    
    term_labels = c("Value", "ValueTRUE")

    summary_stats %>%
        dplyr::filter(term %in% term_labels)
}

summarize_recency_of_labs = function(labs_to_include, years_back = 5L) {

    outcomes = read_in_outcome_data() %>%
        filter_case_only()

    lab_summary = read_in_lab_data() %>%
        filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * years_back) %>%
        summarize_lab_data(TRUE) %>%
        dplyr::filter(tolower(ResultName) %in% tolower(labs_to_include)) %>%
        dplyr::group_by(ResultName) %>%
        dplyr::summarize(
            mean = mean(days_before_test_or_dx, na.rm = TRUE),
            median = median(days_before_test_or_dx, na.rm = TRUE),
            max = max(days_before_test_or_dx, na.rm = TRUE),
            min = min(days_before_test_or_dx, na.rm = TRUE),
            sd  = sd(days_before_test_or_dx, na.rm = TRUE)
        )

    lab_summary
}
