# Build-time derivations that make NestTrait and DISPERSE consumable by the
# cross-source trait verb: .onehot_to_multi collapses a group of 0/1 flags into
# one pipe-delimited categorical (issue #5); .ascii_bin_label and
# .disperse_bin_mid normalize DISPERSE's typographic ordinal bins and give the
# physical-magnitude ordinals a coarse numeric midpoint (issue #6).

test_that(".onehot_to_multi keeps every set modality in source order", {
  d <- data.frame(
    NestSite_ground     = c(1, 0, 0, NA),
    NestSite_tree       = c(0, 1, 0, 0),
    NestSite_nontree    = c(0, 1, 0, 0),
    NestSite_cliff_bank = c(0, 1, 0, 0),
    stringsAsFactors = FALSE
  )
  cols <- c("NestSite_ground", "NestSite_tree", "NestSite_nontree",
            "NestSite_cliff_bank")
  labs <- c("ground", "tree", "nontree", "cliff_bank")
  out <- .onehot_to_multi(d, cols, labs)

  expect_equal(out[1], "ground")                        # single flag
  expect_equal(out[2], "tree|nontree|cliff_bank")       # multi-flag, source order
  expect_true(is.na(out[3]))                            # no flag set -> NA
  expect_true(is.na(out[4]))                            # NA flags treated as unset
})

test_that(".onehot_to_multi is robust to absent columns", {
  d <- data.frame(NestAtt_basal = c(1, 0), stringsAsFactors = FALSE)
  # Only one of the requested columns exists; the rest are simply skipped.
  out <- .onehot_to_multi(d, c("NestAtt_basal", "NestAtt_forked"),
                          c("basal", "forked"))
  expect_equal(out, c("basal", NA_character_))

  # No requested column present -> all NA, one per row.
  out2 <- .onehot_to_multi(d, c("missing_a", "missing_b"), c("a", "b"))
  expect_equal(out2, c(NA_character_, NA_character_))
})

test_that(".ascii_bin_label maps typographic comparators and tidies whitespace", {
  # Source characters as code points: U+2264 <=, U+2265 >=, U+2013 en-dash.
  le <- "≤"; ge <- "≥"; nd <- "–"
  x <- c(paste0(le, " .25"), paste0(ge, " 10", nd, "15 "), "> 2-4 ",
         "1  pair + halters", NA)
  expect_equal(
    .ascii_bin_label(x),
    c("<= .25", ">= 10-15", "> 2-4", "1 pair + halters", NA)
  )
  # No byte above the ASCII range survives on non-NA values.
  ok <- .ascii_bin_label(x)
  has_high <- vapply(ok[!is.na(ok)],
                     function(s) any(utf8ToInt(s) > 127L), logical(1))
  expect_false(any(has_high))
})

test_that(".disperse_bin_mid follows the closed/bottom-open/top-open rule", {
  expect_equal(.disperse_bin_mid("> 2-4"), 3)        # closed range -> mean
  expect_equal(.disperse_bin_mid(">= 5-10"), 7.5)    # closed range -> mean
  expect_equal(.disperse_bin_mid("<= .25"), 0.125)   # bottom-open -> bound/2
  expect_equal(.disperse_bin_mid("< 100"), 50)       # bottom-open -> bound/2
  expect_equal(.disperse_bin_mid("> 8"), 8)          # top-open -> threshold
  expect_equal(.disperse_bin_mid(">= 3000"), 3000)   # top-open -> threshold
  expect_true(is.na(.disperse_bin_mid(NA)))
  expect_true(is.na(.disperse_bin_mid("no numbers here")))
})

test_that(".disperse_bin_mid is monotonic across a full binned column", {
  body <- c("<= .25", "> .25-.5", "> .5-1", "> 1-2", "> 2-4", "> 4-8", "> 8")
  mids <- .disperse_bin_mid(body)
  expect_false(is.unsorted(mids))
  expect_length(unique(mids), length(body))
})
