relabel_outcomes_for_table = function(df) {
    df %>%
        dplyr::mutate(
            outcome_value = case_when(
               as.integer(Deceased) & has_covid == 1 ~ "COVID+ Deceased", 
               as.integer(ICU) & has_covid == 1 ~ "COVID+ ICU", 
               as.integer(Hospitalized) & has_covid == 1 ~ "COVID+ Hospitalized", 
               is.na(Hospitalized) & is.na(Deceased) & has_covid ~ "COVID+ Not Hospitalized or Deceased", 
               `Cohort Type` == "Unmatched Controls" ~ "Controls (No COVID+ test)",
               TRUE ~ "COVID-"
            ),
            outcome_value = factor(
                outcome_value, 
                levels = c(
                    "Controls (No COVID+ test)",
                    "COVID+ Not Hospitalized or Deceased",
                    "COVID+ Hospitalized",
                    "COVID+ ICU",
                    "COVID+ Deceased"
                )
            )
        ) 
}

descriptives_table = function() {
    outcomes = read_in_outcome_data() %>%
        filter_controls %>%
        # tidyr::drop_na(covariate_list_generator()) %>%
        dplyr::select(covariate_list_generator(), Outcome, Deceased, ICU, Hospitalized, `Cohort Type`, `Test Results`) %>%
        exclude_negative_tests %>%
        dplyr::mutate(
            has_covid = `Cohort Type` == "Diagnosed" | (!is.na(`Test Results`) & `Test Results`) == 1
        )

    expanded_outcomes = dplyr::bind_rows(
            outcomes %>% dplyr::filter((has_covid & is.na(Hospitalized) & is.na(ICU) & is.na(Deceased)) | `Cohort Type` == "Unmatched Controls")  ,
            outcomes %>% dplyr::filter(as.integer(Hospitalized) == 1) %>% dplyr::mutate(ICU = 0, Deceased = 0),
            outcomes %>% dplyr::filter(as.integer(ICU) == 1) %>% dplyr::mutate(Hospitalized = 0, Deceased = 0),
            outcomes %>% dplyr::filter(as.integer(Deceased) == 1) %>% dplyr::mutate(Hospitalized = 0, ICU = 0)
        )

    expanded_outcomes %>%
        relabel_outcomes_for_table %>%
        dplyr::select(covariate_list_generator(), outcome_value) %>%
        gtsummary::tbl_summary(by = outcome_value) %>%
        gtsummary::as_flex_table(.) %>%
        flextable::save_as_docx(path = file.path(get_output_dir(), "table1.docx"))
}

lab_sample_size_table = function(labs_to_include, years_back = 5L) {

    stopifnot(is.wholenumber(years_back))
    stopifnot(is.character(labs_to_include))

    outcomes = read_in_outcome_data() %>%
        filter_controls %>%
        # tidyr::drop_na(covariate_list_generator()) %>%
        dplyr::select(Deid_ID, DaysSinceBirth_EARLIEST_TEST_OR_DX, covariate_list_generator(), Outcome, Deceased, ICU, Hospitalized, `Cohort Type`, `Test Results`) %>%
        exclude_negative_tests %>%
        dplyr::mutate(
            has_covid = `Cohort Type` == "Diagnosed" | (!is.na(`Test Results`) & `Test Results`) == 1
        )

    expanded_outcomes = dplyr::bind_rows(
            outcomes %>% dplyr::filter((has_covid & is.na(Hospitalized) & is.na(ICU) & is.na(Deceased)) | `Cohort Type` == "Unmatched Controls")  ,
            outcomes %>% dplyr::filter(as.integer(Hospitalized) == 1) %>% dplyr::mutate(ICU = 0, Deceased = 0),
            outcomes %>% dplyr::filter(as.integer(ICU) == 1) %>% dplyr::mutate(Hospitalized = 0, Deceased = 0),
            outcomes %>% dplyr::filter(as.integer(Deceased) == 1) %>% dplyr::mutate(Hospitalized = 0, ICU = 0)
        ) %>%
        relabel_outcomes_for_table

    outcome_value_counts = expanded_outcomes %>%
        dplyr::count(outcome_value)
    
    labs = read_in_lab_data() %>%
        filter_labs_based_on_diagnosis_date(outcomes, end_of_window = 365 * years_back) %>%
        summarize_lab_data(inverse_normal_transform = FALSE)

    expanded_outcomes = expanded_outcomes %>%
        dplyr::inner_join(labs) %>%
        dplyr::filter(tolower(ResultName) %in% tolower(labs_to_include))

    total_cases = sum(outcomes$has_covid)

    # need a column for all COVID+, i.e., not stratified by outcome status
    case_counts = outcomes %>%
        dplyr::inner_join(labs) %>%
        dplyr::filter(tolower(ResultName) %in% tolower(labs_to_include)) %>%
        dplyr::count(ResultName, has_covid) %>%
        dplyr::filter(has_covid) %>%
        dplyr::mutate(
            prop = scales::percent(n / {{total_cases}}, accuracy = 0.1),
            sample_size_label = glue::glue("{n} ({prop})")
        ) %>%
        dplyr::select(-has_covid, -prop)

    case_means = outcomes %>%
        dplyr::inner_join(labs) %>%
        dplyr::filter(tolower(ResultName) %in% tolower(labs_to_include)) %>%
        dplyr::filter(has_covid) %>%
        dplyr::group_by(ResultName) %>%
        dplyr::summarize(
            mean = scales::number(mean(Value, na.rm = TRUE), accuracy = 0.01),
            sd = scales::number(sd(Value, na.rm = TRUE), accuracy = 0.01),
            mean_label = glue::glue("{mean} ({sd})")
        ) %>%
        dplyr::select(ResultName, mean_label) %>%
        dplyr::ungroup(.)

    case_counts = case_counts %>%
        dplyr::inner_join(case_means, by = "ResultName") %>%
        dplyr::mutate(sample_size_label = glue::glue("{mean_label}, {sample_size_label}")) %>%
        dplyr::select(-mean_label) %>%
        dplyr::rename(`COVID+` = sample_size_label, `COVID+ n` = n) 

    # now calculate stratified by outcome type
    lab_means = expanded_outcomes %>%
        dplyr::inner_join(labs) %>%
        dplyr::filter(tolower(ResultName) %in% tolower(labs_to_include)) %>%
        dplyr::group_by(ResultName, outcome_value) %>%
        dplyr::summarize(
            mean = scales::number(mean(Value, na.rm = TRUE), accuracy = 0.01),
            sd = scales::number(sd(Value, na.rm = TRUE), accuracy = 0.01),
            mean_label = glue::glue("{mean} ({sd})")
        ) %>%
        dplyr::select(ResultName, outcome_value, mean_label) %>%
        dplyr::ungroup(.)

    lab_units = labs %>%
        dplyr::filter(tolower(ResultName) %in% tolower(labs_to_include)) %>%
        dplyr::group_by(ResultName) %>%
        tidyr::drop_na(Unit) %>%
        dplyr::summarize(Unit = Unit[1])

    # browser()

    long_table_format = expanded_outcomes %>%
        dplyr::inner_join(outcome_value_counts, by = "outcome_value") %>%
        dplyr::group_by(ResultName, outcome_value, n) %>%
        dplyr::summarize(nn = dplyr::n()) %>%
        dplyr::ungroup(.) %>%
        dplyr::mutate(
            prop = scales::percent(nn / n, accuracy = 0.1),
            label = glue::glue("{nn} ({prop})")
        ) %>%
        dplyr::inner_join(case_counts, by = "ResultName") %>%
        dplyr::ungroup(.) %>%
        dplyr::select(-n, -nn, -prop) %>%
        dplyr::inner_join(lab_means, by = c("ResultName", "outcome_value")) %>%
        dplyr::left_join(lab_units, by = "ResultName") %>%
        dplyr::mutate(
            ResultName = glue::glue("{ResultName} ({Unit})"),
            label = glue::glue("{mean_label}, {label}")
        ) %>%
        dplyr::select(-mean_label, -Unit) %>%
        dplyr::rename(`Lab trait` = ResultName)

    long_table_format %>%
        tidyr::spread(outcome_value, label) %>%
        dplyr::arrange(desc(`COVID+ n`)) %>%
        dplyr::select(-`COVID+ n`) %>%
        flextable::flextable(.) %>%
        flextable::save_as_docx(path = file.path(get_output_dir(), "table2_with_prop_and_mean.docx"))

}
