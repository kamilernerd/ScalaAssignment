:collect
s/"....."><failure.*failure>/"wrong">/
s/"......."><failure.*failure>/"timeout">/
/<failure/N
/<failure/b collect
s/>></>\n</g
s/.*timestamp[^>]*>//
s/<.testcase>//g
s/<testcase[^[]*.//g
#keep the first test case name
s/-1-.../,/g
#remove the repeated test case names
s/"[^"]*-.-.."/,/g
#clean the single test case name
s/]"/,/g
s/time="//g
s/">/\n/g
s/..testsuite.*//