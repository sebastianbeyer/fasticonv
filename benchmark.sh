#!/bin/bash

## run different filter implementations and compare runtime



echo "benchmarking"


# clear file
rm compare_n.txt 
rm compare_r.txt


echo "Varying n with constant r"
for n in 100 500 1000 1500 2000 3000 4000 5000
do
    command_naive="./performancetest naivebox $n 3 2>&1 >/dev/null"
    command_fast="./performancetest box      $n 3 2>&1 >/dev/null"
    # redirect stderr to stdout and stdout to /dev/null
    # %R in time is total elapsed time
    time_naive="$( TIMEFORMAT='%R';time ( $command_naive ) 2>&1 1>/dev/null )"
    time_fast="$( TIMEFORMAT='%R';time ( $command_fast ) 2>&1 1>/dev/null )"
    echo $n $time_naive $time_fast 
    echo $n $time_naive $time_fast  >> compare_n.txt
done

echo "Varying r with constant n"
for r in 1 2 3 5 10 20 30 40 50
do
    command_naive="./performancetest naivebox 1000 $r 2>&1 >/dev/null"
    command_fast="./performancetest box       1000 $r 2>&1 >/dev/null"
    # redirect stderr to stdout and stdout to /dev/null
    # %R in time is total elapsed time
    time_naive="$( TIMEFORMAT='%R';time ( $command_naive ) 2>&1 1>/dev/null )"
    time_fast="$( TIMEFORMAT='%R';time ( $command_fast ) 2>&1 1>/dev/null )"
    echo $r $time_naive $time_fast 
    echo $r $time_naive $time_fast  >> compare_r.txt
done
