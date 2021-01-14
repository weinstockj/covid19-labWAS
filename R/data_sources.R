get_lab_data_location = function() {
    "/net/junglebook/michiganmedicine/HPI_4706/HPI_4706_LabResults.txt"
}

get_unmatched_lab_data_location = function() {
    "/net/junglebook/michiganmedicine/HPI_4705_RandomControls/HPI_4705_unmatched_ctrls_LabResults.txt"
}

get_outcome_data_location = function() {
    "/net/junglebook/michiganmedicine/MichiganMedicine_COVID19DATA_SIMPLIFIED_20200908.txt"
}

lower_case_lab_names = function(df) {
    assert_not_empty(df)
    stopifnot("ResultName" %in% names(df))

    df %>%
        dplyr::mutate(ResultName = tolower(ResultName))
}

get_labs_to_exclude = function() {
    c(
        "Estimated GFR, Non-Black",
        "Estimated GFR, Black"
    )
}

exclude_labs = function(df, labs_to_exclude = get_labs_to_exclude()) {

    assert_not_empty(df)
    stopifnot(is.character(labs_to_exclude))

    futile.logger::flog.info(glue::glue("excluding {length(labs_to_exclude)} labs"))
    
    df %>%
        dplyr::filter(!(tolower(ResultName) %in% tolower(labs_to_exclude)))
}

read_in_lab_data = function() {
    labs = vroom::vroom(c(get_lab_data_location(), get_unmatched_lab_data_location())) # reads and bind rows   

    assert_not_empty(labs)

    labs = lower_case_lab_names(labs)

    labs = exclude_labs(labs)

    return(labs)
}

read_in_lab_data = memoise::memoise(read_in_lab_data)

read_in_outcome_data = function() {
    outcomes = get_outcome_data_location() %>%
        vroom::vroom(.)

    assert_not_empty(outcomes)

    return(outcomes)
}

read_in_outcome_data = memoise::memoise(read_in_outcome_data)
