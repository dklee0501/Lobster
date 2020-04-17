#!/bin/bash
echo " "
echo "Optimization logs will be saved in paper_result directory"
echo " "
echo "  Name      Old Depth            Carpov.el.al                       Lobster        "
echo "                           New Depth      Opt Time(s)      New Depth      Opt Time(s)"

../baseline/main.native ../baseline/paper_bench/osort.eqn ../baseline/baseline_cases > ../baseline/paper_result/osort
./main.native paper_bench/osort.eqn paper_cases/leave-osort > paper_result/osort
../baseline/main.native ../baseline/paper_bench/bsort.eqn ../baseline/baseline_cases > ../baseline/paper_result/bsort
./main.native paper_bench/bsort.eqn paper_cases/leave-bsort > paper_result/bsort





