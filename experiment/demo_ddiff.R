getwd()
source('R/ddiff.R')
source('R/diff_info_1.R')
source('R/d_test.R')
d <- d_test()

#form and base content diff
#identical
ddiff_rpt <- ddiff1(d$identical$new, d$identical$old, "id")
ddiff_rpt
#Change in order
ddiff_rpt <- ddiff1(d$order$new, d$order$old, "id")
ddiff_rpt
#Attributes the same but row content change
ddiff_rpt <- ddiff1(d$attrs$same$new, d$attrs$same$old, "id")
ddiff_rpt
#Attributes different colnames same
ddiff_rpt <- ddiff1(d$attrs$different$col_same$new, d$attrs$different$col_same$old, "id")
ddiff_rpt
#Attributes different colnames different
ddiff_rpt <- ddiff1(d$attrs$different$col_diff$new, d$attrs$different$col_diff$old, "id")
ddiff_rpt
#New records added
ddiff_rpt <- ddiff1(d$records_added$new, d$records_added$old, "id")
ddiff_rpt

#information content diff
ddiff_rpt <- ddiff3(d$identical$new, d$identical$old,"id")
ddiff_rpt
data.tree::FromListSimple(ddiff_rpt)
summary.con(d$identical$new, d$identical$old, ddiff_rpt, summary)
summary.cat(d$identical$new, d$identical$old, ddiff_rpt, table)
summary.bin(d$identical$new, d$identical$old, ddiff_rpt, table)
