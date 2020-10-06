get_lab_data_location = function() {
    "/net/junglebook/michiganmedicine/HPI_4705/HPI_4705_LabResults.txt"
}

get_outcome_data_location = function() {
    "/net/junglebook/michiganmedicine/MichiganMedicine_COVID19DATA_SIMPLIFIED_20200728.txt"
}


read_in_lab_data = function() {
    get_lab_data_location() %>%
        vroom::vroom(.)
}

read_in_lab_data = memoise::memoise(read_in_lab_data)

read_in_outcome_data = function() {
    get_outcome_data_location() %>%
        vroom::vroom(.)
}

read_in_outcome_data = memoise::memoise(read_in_outcome_data)
