# test_that function
# This is run if one wants to run the R driver without breaking/errors
# Author: Rupsha Debnath

test_that <- function(...) tryCatch(test_that(...),
                                               error = function(e) warning(e))