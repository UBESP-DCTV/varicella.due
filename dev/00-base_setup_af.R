

# Project packages ------------------------------------------------

renv::install(c("usethis", "here", "spelling"))
usethis::use_description(check_name = FALSE)



# Licensing -------------------------------------------------------

# usethis::use_ccby_license() # slide/report/documenti
# usethis::use_mit_license() # software/pacchetti/..
# usethis::use_proprietary_license("Nome & UBEP")



# Package for the analyses ----------------------------------------

renv::install(c("tidyverse", "rms", "here"))  #....





# Utilities -------------------------------------------------------
usethis::use_spell_check()



# Update the renv lockfile ----------------------------------------
renv::status()
renv::snapshot()





# Development cycle run -------------------------------------------

spelling::spell_check_package()
# spelling::update_wordlist()
renv::status()
# renv::snapshot()
