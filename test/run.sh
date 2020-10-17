#!/bin/bash

EXEC="./funEvalExpr"
RESULT_OK="\e[0;32mOK\e[0;0m"
RESULT_KO="\e[0;31mKO\e[0;0m"
RESULT_CRASH="\e[0;35mCRASHED\e[0;0m"
RESULT_TIMEOUT="\e[0;34mTIMED OUT\e[0;0m"
RESULT_FAILED="\e[0;35mFAILED\e[0;0m"
RESULT_WARNING="\e[0;35mWARNING!!!\e[0;0m"
TOTAL=0
PASSED=0
CRASHED=0
TIMEDOUT=0
FAILED=0
TIMEOUT=1

function test {
    NAME="$1"
    EXPRESSION="$2"
    EXPECTED_RESULT="$3"
    EXPECTED_EXIT_CODE="$4"

    printf "%-40s" "  Test $1: "
    TOTAL=$(($TOTAL + 1))
    RESULT=$(timeout $TIMEOUT $EXEC "$EXPRESSION")
    EXIT_CODE=$?

    if [[ $EXIT_CODE != $EXPECTED_EXIT_CODE ]]; then
        if [[ $EXIT_CODE == 139 ]]; then
            printf "$RESULT_CRASH\n    Test crashed!\n"
            CRASHED=$(($CRASHED + 1))
            return
        fi
        if [[ $EXIT_CODE == 124 ]]; then
            printf "$RESULT_TIMEOUT\n    $TIMEOUT sec. timeout exceeded!\n"
            TIMEDOUT=$(($TIMEDOUT + 1))
            return
        fi
        printf "$RESULT_KO\n    Expected return code $EXPECTED_EXIT_CODE, got $EXIT_CODE\n"
        return
    fi
    if [[ $EXIT_CODE != 0 ]]; then

        printf "$RESULT_OK\n"
        PASSED=$(($PASSED + 1))
        return
    fi
    if [[ "$RESULT" != "$EXPECTED_RESULT" ]]; then
        printf "$RESULT_KO\n    Invalid output:\n"
        printf "      Got $RESULT, expected $EXPECTED_RESULT\n"
        return
    fi
    printf "$RESULT_OK\n"
    PASSED=$(($PASSED + 1))
}

echo "[Subject examples]"
test "example_a" "3 + 5.34" "8.34" 0
test "example_b" "(0.345 + 5 )*( -2-1) / 3" "-5.35" 0
echo

echo "[Rigor]"
test "unbalanced_left_bracket" "1+(2+3))" "" 84
test "unbalanced_right_bracket" "1+((2+3)" "" 84
test "unbalanced_both_brackets" "1+(2-3)-0)*1-(2+1)+(0-3))" "" 84
test "zero_div_a" "1+2/0" "" 84
test "zero_div_b" "1*2+3/((2*3-1)-10/2)" "" 84
test "invalid_operator" "1_2" "" 84
test "invalid_order_a" "1*" "" 84
test "invalid_order_b" "*1" "" 84
test "invalid_order_c" "1+*2" "" 84
test "invalid_order_d" "1+()-1" "" 84
test "invalid_order_e" "(+)" "" 84
test "invalid_order_f" "1 2" "" 84
test "invalid_order_g" "1. 5" "" 84
test "invalid_order_h" "(1(2))" "" 84
test "empty" "" "" 84
echo

echo "[Single number]"
test "number_after_blank" "    42" "42.00" 0
test "number_before_blank" "21     " "21.00" 0
test "number_between_blanks" "        32     " "32.00" 0
test "number_in_single_brackets" "(16)" "16.00" 0
test "number_in_multiple_brackets" "((((((-16))))))" "-16.00" 0
test "negative_brackets" "-(64)" "-64.00" 0
test "negative_brackets_reversed" "-(-64)" "64.00" 0
test "mixed_a" "	 -((- 16  		 ))  	" "16.00" 0
test "mixed_b" " 	 	- ( 	 -  (    - 16     ) 	)" "-16.00" 0
test "mixed_c" "	 -(  - 		(     ( - 128) 	 ))" "-128.00" 0
test "mixed_d" " 		 -(  - 		 (   - (	- 128)  	))" "128.00" 0
echo

echo "[Rounding]"
test "rounding_a" "1.99" "1.99" 0
test "rounding_b" "1.999" "2.00" 0
test "rounding_c" "1.001" "1.00" 0
test "rounding_d" "1.0545" "1.05" 0
test "rounding_e" "1.055" "1.05" 0
test "rounding_f" "1.0555" "1.06" 0
test "rounding_f" "1.056" "1.06" 0
test "rounding_g" "-1.055" "-1.05" 0
test "rounding_h" "-1.056" "-1.06" 0
echo

echo "[Simple]"
test "fibonacci_10" "1+1+2+3+5+8+13+21+34+55" "143.00" 0
test "all_ops_a" "1+2-3*4/5^2" "2.52" 0
test "all_ops_b" "1*2+3/4-5^6" "-15622.25" 0
test "all_ops_c" "1*2*3+4+5+4-3-2/1^0" "14.00" 0
test "all_ops_d" "1/2*1+1^9/4-3+6*8+5/7" "46.46" 0
echo

echo "[Advanced expressions]"
test "advanced_a" "(1-4^2+5*6/2-10-(2*34-54+2*32-2*(1+54-2*3^3)-(3-4*2+2^2^2*2+42*2-88/(2*0+1*1+42+1)+3)-2-2*5)*2+3*(1+2*2+1))" "104.00" 0
test "advanced_b" "-(2*(2-4)-(1+4*5-2^(3-1)^3+10/(2^2+1)*2+10^2+3^4)+12*(1-2*(1+3-0)+2-5)*2+2^2^2+35*4)-2*(3*3+2)" "16.00" 0
test "advanced_c" "((((1+3/2*4)+0^0-2^2+(-2)^3*2)-4*2+3-4/2)*2+(1-4*3)/2)/2" "-21.75" 0
test "advanced_d" "(2^2^2^2-42^4)/(21*(1+1-0))-100/(12*3)/2+3*(2-2*3+1)/5" "-72530.81" 0
test "advanced_e" "(((42+2*3^2-10*(5-2)+((1-2*(3-4+2*3*2)-(5*(2-3)+(1-3)))+(10+2^2)/7)+2*(3+7)-(2^((3^2-1)/4))-2)))" "32.00" 0
echo

echo "[Tricky shit]"
test "many_brackets" "-(1)+(1+((1+1)+((1))+1)+((1))+((1)+1))+1" "8.00" 0
test "chained_power" "2^4^2/2^2^3" "256.00" 0
test "negative_power" "(2^(-2)*10^2)^(-1)" "0.04" 0
echo

printf "\nRESULT:\n"
printf "  $PASSED / $TOTAL\n"
printf "  $(printf $(($PASSED * 10000 / $TOTAL)) | sed 's/..$/.&/') %%\n"
if [[ $CRASHED != 0 ]]; then
    printf "\n$RESULT_WARNING $CRASHED test(s) crashed\n"
    exit 1
fi
if [[ $TIMEDOUT != 0 ]]; then
    printf "\n$RESULT_WARNING $TIMEDOUT test(s) timed out\n"
    exit 1
fi
[ $PASSED != $TOTAL ] && exit 1 || exit 0
