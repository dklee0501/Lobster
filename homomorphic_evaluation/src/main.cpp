#include "FHE.h"
#include <timing.h>
#include <EncryptedArray.h>
#include <NTL/lzz_pXFactoring.h>
#include <vector>
#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>
#include <string.h>
#include <fstream>
#include <vector>
#include "NTL/ZZ_p.h"
#include <NTL/ZZ_pX.h>
#include <NTL/vec_ZZ_p.h>
#include <assert.h>
#include <stdlib.h>

int main(int argc, const char *argv[])
{
    long m=0, p=127, r=1; // Native plaintext space
    // Computations will be 'modulo p'
    long L;          // Levels
    long c=2;           // Columns in key switching matrix
    long w=64;          // Hamming weight of secret key
    long d=1;
    long security;
    long s;
    long bnd;

    ZZX G;

    return 0;
}
