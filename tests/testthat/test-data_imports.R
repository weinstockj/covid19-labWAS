context("data sources are correct")

test_that("data sources exist", {
    lab_data = get_lab_data_location()
    outcome_data = get_outcome_data_location()

    expect_equal(file.exists(lab_data), TRUE)
    expect_equal(file.exists(outcome_data), TRUE)
})

test_that("labs have expected values", {
    labs = read_in_lab_data() 
    expected_head_checksum = "3ed6087ca5903aae79618d03e6d42f64"
    expected_tail_checksum = "f88848449dad2beb6c42345f87bd2443"

    n_rows = 100
    expect_equal(digest::digest(head(labs, n = n_rows)), expected_head_checksum)
    expect_equal(digest::digest(tail(labs, n = n_rows)), expected_tail_checksum)
})

test_that("outcomes have expected values", {
    outcomes = read_in_outcome_data() 
    expected_checksum = "ddf03ae3ea07ae4b712a2cb4c8f00792"

    expect_equal(digest::digest(outcomes), expected_checksum)
})
