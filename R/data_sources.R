get_lab_data_location = function() {
    "/net/junglebook/michiganmedicine/HPI_4706/HPI_4706_LabResults.txt"
}

get_unmatched_lab_data_location = function() {
    "/net/junglebook/michiganmedicine/HPI_4705_RandomControls/HPI_4705_unmatched_ctrls_LabResults.txt"
}

get_outcome_data_location = function() {
    "/net/junglebook/michiganmedicine/MichiganMedicine_COVID19DATA_SIMPLIFIED_20200908.txt"
}


read_in_lab_data = function() {
    labs = vroom::vroom(c(get_lab_data_location(), get_unmatched_lab_data_location())) # reads and bind rows   

    assert_not_empty(labs)

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
