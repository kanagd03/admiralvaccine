library(tibble)
library(dplyr)
library(rlang)
library(admiral)
library(admiraldev)

#testcase-1
testthat::test_that('testcase-1 : checking whether its handling the NA values
                    and cutoff value is working fine',{

                      input<-tribble(
                        ~USUBJID,~FAOBJ,~ATPTREF,~AVAL,~AVALC,~FATEST, ~FATESTCD, ~FASCAT,
                        "1","REDNESS","VAC1",3.5,"3.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","REDNESS","VAC1",4.5,"4.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","REDNESS","VAC1",.5,"3.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","FATIGUE","VAC1",1  ,"MILD","Severity","SEV","SYSTEMIC",
                        "1","FATIGUE","VAC1",2  ,"MODERATE","Severity","SEV","SYSTEMIC",
                        "1","FATIGUE","VAC1",NA_integer_,"NONE","Severity","SEV","SYSTEMIC",
                        "1","REDNESS","VAC2",3.5,"3.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","REDNESS","VAC2",4.5,"4.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","REDNESS","VAC2",1.5,"1.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","FATIGUE","VAC2",1 ,"MILD","Severity","SEV","SYSTEMIC",
                        "1","FATIGUE","VAC2",2 ,NA_character_,"Severity","SEV","SYSTEMIC",
                        "1","FATIGUE","VAC2",0 ,"NONE","Severity","SEV","SYSTEMIC"
                      )

                      actual_output <-derive_vars_event_flag(
                        dataset = input,
                        by_vars = vars(USUBJID,FAOBJ,ATPTREF),
                        aval_cutoff = 2.0,
                        new_var1 = EVENTL,
                        new_var2 = EVENTDL)


                      expected_output <- input %>%
                        group_by(USUBJID,FAOBJ,ATPTREF) %>%
                        mutate(
                          EVENTL = case_when(any(!is.na(AVAL) & AVAL > 2.0 | AVALC %in% c('Y','MILD','MODERATE','SEVERE')) ~ 'Y',
                                             TRUE ~ 'N'),

                          EVENTDL = case_when(!is.na(AVAL) & AVAL > 2.0 | AVALC %in% c('Y','MILD','MODERATE','SEVERE') ~ 'Y',
                                              TRUE ~ 'N')
                        )
                      expect_dfs_equal(
                        expected_output,
                        actual_output,
                        keys = c('USUBJID','FATEST','ATPTREF','AVAL','AVALC','FAOBJ','EVENTL','EVENTDL')
                      )

                    }
)

# testcase-2

testthat::test_that('test case - 2: Checking whether its creating the user input
                    varibales name for both flag',{
                      input<-tribble(
                        ~USUBJID,~FAOBJ,~ATPTREF,~AVAL,~AVALC,~FATEST, ~FATESTCD, ~FASCAT,
                        "1","REDNESS","VAC1",3.5,"3.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","REDNESS","VAC1",4.5,"4.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","REDNESS","VAC1",1.5,"3.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","FATIGUE","VAC1",1  ,"MILD","Severity","SEV","SYSTEMIC",
                        "1","FATIGUE","VAC1",2  ,"MODERATE","Severity","SEV","SYSTEMIC",
                        "1","FATIGUE","VAC1",0  ,"NONE","Severity","SEV","SYSTEMIC",
                        "1","REDNESS","VAC2",3.5,"3.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","REDNESS","VAC2",4.5,"4.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","REDNESS","VAC2",1.5,"1.5","Diameter","DIAMETER","ADMIN-SITE",
                        "1","FATIGUE","VAC2",1 ,"MILD","Severity","SEV","SYSTEMIC",
                        "1","FATIGUE","VAC2",2 ,'MODERATE',"Severity","SEV","SYSTEMIC",
                        "1","FATIGUE","VAC2",0 ,"NONE","Severity","SEV","SYSTEMIC"
                      )

                      expected_output <- input %>%
                        group_by(USUBJID,FAOBJ,ATPTREF) %>%
                        mutate(
                          flag1 = case_when(any(!is.na(AVAL) & AVAL > 2.0 | AVALC %in% c('Y','MILD','MODERATE','SEVERE')) ~ 'Y',
                                            TRUE ~ 'N'),

                          flag2 = case_when(!is.na(AVAL) & AVAL > 2.0 | AVALC %in% c('Y','MILD','MODERATE','SEVERE') ~ 'Y',
                                            TRUE ~ 'N')
                        ) %>% rename(EFL=flag1,EDFL=flag2)


                      actual_output <- derive_vars_event_flag(
                        dataset = input,
                        by_vars = vars(USUBJID,FAOBJ,ATPTREF),
                        aval_cutoff = 2.0,
                        new_var1 = EFL,
                        new_var2 = EDFL)

                      expect_dfs_equal(
                        expected_output,
                        actual_output,
                        keys = c('USUBJID','FATEST','ATPTREF','AVAL','AVALC','FAOBJ','EFL','EDFL')
                      )

                    })

