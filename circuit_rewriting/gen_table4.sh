#!/bin/bash
echo " "
echo "Optimization logs will be saved in paper_result directory"
echo " "
echo "  Name      Old Depth            Carpov.el.al                       Lobster        "
echo "                           New Depth      Opt Time(s)      New Depth      Opt Time(s)"
../baseline/main.native ../baseline/paper_bench/i2c.eqn ../baseline/baseline_cases > ../baseline/paper_result/i2c
./main.native paper_bench/i2c.eqn paper_cases/leave-i2c > paper_result/i2c
../baseline/main.native ../baseline/paper_bench/int2float.eqn ../baseline/baseline_cases > ../baseline/paper_result/int2float
./main.native paper_bench/int2float.eqn paper_cases/leave-int2float > paper_result/int2float
../baseline/main.native ../baseline/paper_bench/router.eqn ../baseline/baseline_cases > ../baseline/paper_result/router
./main.native paper_bench/router.eqn paper_cases/leave-router > paper_result/router


