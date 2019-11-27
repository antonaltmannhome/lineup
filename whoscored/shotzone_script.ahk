sleep 500
WinSelect("Firefox")
sleep 500
send {PgDn}
sleep 500
send {Down 4}
sleep 500
click $DETAILED_X$, $DETAILED_Y$ ;click on detailed
sleep 3000
click $ACCUMULATION_X$, $CAT-SUBCAT-ACCUM_Y$ ;click on detailed/accumulation
sleep 1000
click $ACCUMULATION_X$, $ACCUMULATION-TOTAL_Y$ ;click on detailed/accumulation/total
sleep 3000
