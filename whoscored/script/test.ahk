#SingleInstance force
^!b::
sleep 1000
send ^+s
sleep 2000
send ^a
sleep 500
send {Backspace}
sleep 500
send c:\research\lineup\whoscored\data\yrsummary_1415_1_20151008.csv
sleep 500
click 372,405
sleep 1000
send {Down 13}
sleep 500
send {Return}
sleep 2000
send {Return}
sleep 5000
click 450,50
sleep 2000
send {Return}
sleep 2000
send ^n
sleep 1000
ExitApp
Esc::ExitApp
