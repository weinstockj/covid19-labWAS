context("data sources are correct")

test_that("data sources exist", {
    lab_data = get_lab_data_location()
    outcome_data = get_outcome_data_location()

    expect_equal(file.exists(lab_data), TRUE)
    expect_equal(file.exists(outcome_data), TRUE)
})

test_that("labs have expected values", {
    labs = read_in_lab_data() 
    expected_head_checksum = "6273a33fab22255d7ad6935eb4bd1ecc"
    expected_tail_checksum = "337b995274aa578236a8f22a72beca5a"

    n_rows = 100
    expect_equal(digest::digest(head(labs, n = n_rows)), expected_head_checksum)
    expect_equal(digest::digest(tail(labs, n = n_rows)), expected_tail_checksum)
})

test_that("outcomes have expected values", {
    outcomes = read_in_outcome_data() 
    expected_checksum = "606849a7d3e2176b7fb049bafcd73cda"

    expect_equal(digest::digest(outcomes), expected_checksum)
})
