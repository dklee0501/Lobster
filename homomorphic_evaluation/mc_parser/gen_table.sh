#!/bin/bash
echo " "
echo "Evaluation result will be saved in paper_result directory"
echo " "
echo " Name                        Eval.Time"
echo "              Original     Baseline-opted   Lobster-opted"

./he_base paper_bench/cardio.eqn 10 > paper_result/cardio
./he_base paper_bench/cardio.eqn_opted_result_baseline 9 > paper_result/cardio_opted_result_baseline
./he_base paper_bench/cardio.eqn_opted_result 8 > paper_result/cardio_opted_result

./he_base paper_bench/dsort.eqn 9 > paper_result/dsort
./he_base paper_bench/dsort.eqn_opted_result_baseline 8 > paper_result/dsort_opted_result_baseline
./he_base paper_bench/dsort.eqn_opted_result 8 > paper_result/dsort_opted_result

echo "hd01.eqn             -                  -               -"

echo "hd02.eqn             -                  -               -"

echo "hd03.eqn             -                  -               -"

./he_base paper_bench/hd04.eqn 10 > paper_result/hd04
./he_base paper_bench/hd04.eqn_opted_result_baseline 9 > paper_result/hd04_opted_result_baseline
./he_base paper_bench/hd04.eqn_opted_result 8 > paper_result/hd04_opted_result

echo "hd05.eqn             -                  -               -"

echo "hd06.eqn             -                  -               -"

./he_base paper_bench/hd07.eqn 5 > paper_result/hd07
echo -n "                  -"
./he_base paper_bench/hd07.eqn_opted_result 3 > paper_result/hd07_opted_result

./he_base paper_bench/hd08.eqn 6 > paper_result/hd08
./he_base paper_bench/hd08.eqn_opted_result_baseline 5 > paper_result/hd08_opted_result_baseline
./he_base paper_bench/hd08.eqn_opted_result 5 > paper_result/hd08_opted_result

./he_base paper_bench/hd09.eqn 14 > paper_result/hd09
./he_base paper_bench/hd09.eqn_opted_result_baseline 12 > paper_result/hd09_opted_result_baseline
./he_base paper_bench/hd09.eqn_opted_result 11 > paper_result/hd09_opted_result

./he_base paper_bench/hd10.eqn 6 > paper_result/hd10
./he_base paper_bench/hd10.eqn_opted_result_baseline 5 > paper_result/hd10_opted_result_baseline
./he_base paper_bench/hd10.eqn_opted_result 5 > paper_result/hd10_opted_result

./he_base paper_bench/hd11.eqn 18 > paper_result/hd11
./he_base paper_bench/hd11.eqn_opted_result_baseline 17 > paper_result/hd11_opted_result_baseline
./he_base paper_bench/hd11.eqn_opted_result 16 > paper_result/hd11_opted_result

./he_base paper_bench/hd12.eqn 16 > paper_result/hd12
./he_base paper_bench/hd12.eqn_opted_result_baseline 15 > paper_result/hd12_opted_result_baseline
./he_base paper_bench/hd12.eqn_opted_result 15 > paper_result/hd12_opted_result

./he_base paper_bench/bar.eqn 12 > paper_result/bar
echo -n "                  -"
./he_base paper_bench/bar.eqn_opted_result 11 > paper_result/bar_opted_result

./he_base paper_bench/cavlc.eqn 16 > paper_result/cavlc
./he_base paper_bench/cavlc.eqn_opted_result_baseline 10 > paper_result/cavlc_opted_result_baseline
./he_base paper_bench/cavlc.eqn_opted_result 10 > paper_result/cavlc_opted_result

./he_base paper_bench/ctrl.eqn 8 > paper_result/ctrl
./he_base paper_bench/ctrl.eqn_opted_result_baseline 6 > paper_result/ctrl_opted_result_baseline
./he_base paper_bench/ctrl.eqn_opted_result 5 > paper_result/ctrl_opted_result

echo "dec.eqn              -                  -               -"

./he_base paper_bench/i2c.eqn 15 > paper_result/i2c
./he_base paper_bench/i2c.eqn_opted_result_baseline 9 > paper_result/i2c_opted_result_baseline
./he_base paper_bench/i2c.eqn_opted_result 8 > paper_result/i2c_opted_result

./he_base paper_bench/int2float.eqn 15 > paper_result/int2float
./he_base paper_bench/int2float.eqn_opted_result_baseline 9 > paper_result/int2float_opted_result_baseline
./he_base paper_bench/int2float.eqn_opted_result 8 > paper_result/int2float_opted_result

./he_base paper_bench/router.eqn 19 > paper_result/router
./he_base paper_bench/router.eqn_opted_result_baseline 10 > paper_result/router_opted_result_baseline
./he_base paper_bench/router.eqn_opted_result 10 > paper_result/router_opted_result

./he_base paper_bench/msort.eqn 45 > paper_result/msort
./he_base paper_bench/msort.eqn_opted_result_baseline 41 > paper_result/msort_opted_result_baseline
./he_base paper_bench/msort.eqn_opted_result 36 > paper_result/msort_opted_result

./he_base paper_bench/isort.eqn 45 > paper_result/isort
echo -n "                  -"
./he_base paper_bench/isort.eqn_opted_result 36 > paper_result/isort_opted_result

./he_base paper_bench/bsort.eqn 45 > paper_result/bsort
./he_base paper_bench/bsort.eqn_opted_result_baseline 41 > paper_result/bsort_opted_result_baseline
./he_base paper_bench/bsort.eqn_opted_result 36 > paper_result/bsort_opted_result

./he_base paper_bench/osort.eqn 25 > paper_result/osort
echo -n "                  -"
./he_base paper_bench/osort.eqn_opted_result 20 > paper_result/osort_opted_result



