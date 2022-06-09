source('R/ddiff.R')
source('R/d_test.R')
d <- d_test()
#form or base content ddiff
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
ddiff_rpt <- ddiff1(d$records_added$new, d$records_added$old)
ddiff_rpt

#information content ddiff

