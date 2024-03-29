# SBSP_restartMetrics

All NHS Scotland screening programmes were halted in March 2020 in response to the COVID-19 pandemic. The Scottish Breast Screening Programme resumed in early August 2020, working through a backlog of patients. The breast restart metrics are calculated monthly to review the numbers of appointments allocated and attended, as well as referrals, slippage, and waiting times for each of the six Scottish breast screening centres and the whole of Scotland for comparison to pre-pandemic levels.


Processing of the SBSS R079 report is done using R. There is one processing script, plus a housekeeping and two archival scripts. The historical script (x_R079_recreate_historical.R) is an archive of the process to create the original R079 database and can be used if, for any reason, this needs to be recreated. The last script (x_compare_reports.R) was used to compare the weekly reports that the R079 replaced in 2022.

