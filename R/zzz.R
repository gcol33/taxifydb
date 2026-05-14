# NSE bindings used inside vectra::filter() / vectra::select() chains.
# Declared here so `R CMD check` does not flag them as undefined globals.
utils::globalVariables(c("key_ci", "accepted_name", "n"))
