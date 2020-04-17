#include <iostream>
#include <cstdlib>
#include <cstring>
#include <helib/FHE.h>
#include <NTL/ZZ.h>
#include <NTL/ZZX.h>
#include <map>
#include <string>
#include <ctime>
#include <chrono>
//#include <NTL/lzz_pXFactoring.h>

#include <helib/EncryptedArray.h>
#include <fstream>
#include <sstream>
#include <sys/time.h>

#include "mc_driver.hpp"
NTL_CLIENT

using namespace MC;

int main( const int argc, const char **argv )
{
    /** check for the right # of arguments **/
    if( argc == 3 )
    {
        MC::MC_Driver driver;

        /** example for piping input from terminal, i.e., using cat / 
          if( std::strncmp( argv[ 1 ], "-o", 2 ) == 0 )
          {
          driver.parse( std::cin );
          }
         * simple help menu *
         else if( std::strncmp( argv[ 1 ], "-h", 2 ) == 0 )
         {
         std::cout << "use -o for pipe to std::cin\n";
         std::cout << "just give a filename to count from a file\n";
         std::cout << "use -h to get this menu\n";
         return( EXIT_SUCCESS );
         }
         * example reading input from a file *
         else
         {

         * assume file, prod code, use stat to check **/
        driver.parse( argv[1] );
        vector<string> inputlist = driver.inputlist;
        vector<string> outputlist = driver.outputlist;
        vector<tuple<string, MC::Bexp*>> eqnlist = driver.eqnlist;
        
        
        // init HElib argument
        long m = 0, p = 2, r = 1;
        long depth = atoi(argv[2]);
        long L = depth * 30; //Level
        long c = 2;
        long w = 64;
        long d = 1;
        long security = 128;
        long s = 0;//slot
        ZZX G;
        
        m = FindM(security, L, c, p, d, s, 0);
	cout << "selected m : " << m << endl;
	cerr << m << ", " ;
        FHEcontext context(m, p, r);
        buildModChain(context, L, c);
        FHESecKey sk(context);
        const FHEPubKey& pk = sk;
        
        G = context.alMod.getFactorsOverZZ()[0];

        sk.GenSecKey(w);

        addSome1DMatrices(sk);
        cout << "generated Key : " << endl;

        

        //encrypt inputlist, make memory
        map<string, Ctxt> memory;

        for(vector<string>::size_type i = 0; i < inputlist.size(); i++){
            Ctxt tmp(pk);
            int plaintext = 0;
            //plaintext modification
            if(i == 3 || i ==11)
                plaintext=1;
            cout << "input : " << inputlist[i] << " : " << plaintext << endl;
            pk.Encrypt(tmp, to_ZZX(plaintext));
            memory.insert( make_pair(inputlist[i], tmp) );
        }
        Ctxt true_ctxt(pk);
        pk.Encrypt(true_ctxt, to_ZZX(1));
        memory.insert( make_pair("true", true_ctxt) );

        Ctxt false_ctxt(pk);
        pk.Encrypt(false_ctxt, to_ZZX(0));
        memory.insert( make_pair("false", false_ctxt) );
         
        /* for(auto i = memory.begin(); i!= memory.end(); i++){
            ZZX memory_res;
            sk.Decrypt(memory_res, i->second);
            cout << i->first << " : " << memory_res[0] << endl;
        }*/
	//int start_time = time(0);
        std::chrono::system_clock::time_point StartTime = std::chrono::system_clock::now();

        for(auto i = 0 ; i < eqnlist.size(); i++){
            string lv = get<0>(eqnlist[i]);
            Bexp* bexp = get<1>(eqnlist[i]);
            auto top_op = bexp->head;
            int constant = bexp->constant;
            string var = bexp->var;
            Bexp* l_child = bexp->left;
            Bexp* r_child = bexp->right;
            if(top_op == MC::Bexp::Head::CONST){
                if(constant == 1){
                    memory.insert(make_pair(lv, true_ctxt ));
                }
                else if(constant == 0){
                    memory.insert(make_pair(lv, false_ctxt));
                }
            }
            else if(top_op == MC::Bexp::Head::VAR){
                memory.insert(make_pair(lv, memory.find(var)->second));
            }
            else if(top_op == MC::Bexp::Head::AND){
                auto lchild_op = l_child->head;
                string lchild_var = l_child->var;
                int lchild_const = l_child->constant;
                Ctxt lchild_ctxt(pk);
                if(lchild_op == MC::Bexp::Head::CONST){
                    if(lchild_const == 1){
                        lchild_ctxt = true_ctxt;
                    }
                    else{
                        lchild_ctxt = false_ctxt;
                    }
                }
                else if(lchild_op == MC::Bexp::Head::VAR){
                    lchild_ctxt = memory.find(lchild_var)->second;
                }

                auto rchild_op = r_child->head;
                string rchild_var = r_child->var;
                int rchild_const = r_child->constant;
                Ctxt rchild_ctxt(pk);
                if(rchild_op == MC::Bexp::Head::CONST){
                    if(rchild_const == 1){
                        rchild_ctxt = true_ctxt;
                    }
                    else{
                        rchild_ctxt = false_ctxt;
                    }
                }
                else if(rchild_op == MC::Bexp::Head::VAR){
                    rchild_ctxt = memory.find(rchild_var)->second;
                }

                /* 
                ZZX lchild_res;
                ZZX rchild_res;
                sk.Decrypt(lchild_res, lchild_ctxt);
                sk.Decrypt(rchild_res, rchild_ctxt);
                */

                lchild_ctxt *= rchild_ctxt;
                lchild_ctxt.reLinearize();               
                
                //ZZX and_res;
                //sk.Decrypt(and_res, lchild_ctxt);
                //cout << lchild_var << " * " << rchild_var << " = " << lv << endl;
                //cout << lchild_res[0] << " * " << rchild_res[0] << " = " << and_res[0] << endl;



                memory.insert(make_pair(lv, lchild_ctxt));
                
            }
            else if(top_op == MC::Bexp::Head::XOR){
                auto lchild_op = l_child->head;
                string lchild_var = l_child->var;
                int lchild_const = l_child->constant;
                Ctxt lchild_ctxt(pk);
                if(lchild_op == MC::Bexp::Head::CONST){
                    if(lchild_const == 1){
                        lchild_ctxt = true_ctxt;
                    }
                    else{
                        lchild_ctxt = false_ctxt;
                    }
                }
                else if(lchild_op == MC::Bexp::Head::VAR){
                    lchild_ctxt = memory.find(lchild_var)->second;
                }

                auto rchild_op = r_child->head;
                string rchild_var = r_child->var;
                int rchild_const = r_child->constant;
                Ctxt rchild_ctxt(pk);
                if(rchild_op == MC::Bexp::Head::CONST){
                    if(rchild_const == 1){
                        rchild_ctxt = true_ctxt;
                    }
                    else{
                        rchild_ctxt = false_ctxt;
                    }
                }
                else if(rchild_op == MC::Bexp::Head::VAR){
                    rchild_ctxt = memory.find(rchild_var)->second;
                }

                
                /*
                ZZX lchild_res;
                ZZX rchild_res;
                sk.Decrypt(lchild_res, lchild_ctxt);
                sk.Decrypt(rchild_res, rchild_ctxt);
                */
                

                lchild_ctxt += rchild_ctxt;                

                /*
                ZZX and_res;
                sk.Decrypt(and_res, lchild_ctxt);
                cout << lchild_var << " + " << rchild_var << " = " << lv << endl;
                cout << lchild_res[0] << " + " << rchild_res[0] << " = " << and_res[0] << endl;
                */

                memory.insert(make_pair(lv, lchild_ctxt));
                
            }
            else if(top_op == MC::Bexp::Head::OR){
                auto lchild_op = l_child->head;
                string lchild_var = l_child->var;
                int lchild_const = l_child->constant;
                Ctxt lchild_ctxt(pk);
                if(lchild_op == MC::Bexp::Head::CONST){
                    if(lchild_const == 1){
                        lchild_ctxt = true_ctxt;
                    }
                    else{
                        lchild_ctxt = false_ctxt;
                    }
                }
                else if(lchild_op == MC::Bexp::Head::VAR){
                    lchild_ctxt = memory.find(lchild_var)->second;
                }

                auto rchild_op = r_child->head;
                string rchild_var = r_child->var;
                int rchild_const = r_child->constant;
                Ctxt rchild_ctxt(pk);
                if(rchild_op == MC::Bexp::Head::CONST){
                    if(rchild_const == 1){
                        rchild_ctxt = true_ctxt;
                    }
                    else{
                        rchild_ctxt = false_ctxt;
                    }
                }
                else if(rchild_op == MC::Bexp::Head::VAR){
                    rchild_ctxt = memory.find(rchild_var)->second;
                }

                
                /*
                ZZX lchild_res;
                ZZX rchild_res;
                sk.Decrypt(lchild_res, lchild_ctxt);
                sk.Decrypt(rchild_res, rchild_ctxt);
                */
                


                Ctxt tmp_ctxt1 = lchild_ctxt;
                tmp_ctxt1 += rchild_ctxt;
                lchild_ctxt *= rchild_ctxt;
                lchild_ctxt +=tmp_ctxt1;
                lchild_ctxt.reLinearize();

                
                /*
                ZZX and_res;
                sk.Decrypt(and_res, lchild_ctxt);
                cout << lchild_var << " or " << rchild_var << " = " << lv << endl;
                cout << lchild_res[0] << " or " << rchild_res[0] << " = " << and_res[0] << endl;
                */

                memory.insert(make_pair(lv, lchild_ctxt));
                
            }
            else if(top_op == MC::Bexp::Head::NOT){
                auto rchild_op = r_child->head;
                string rchild_var = r_child->var;
                int rchild_const = r_child->constant;
                Ctxt rchild_ctxt(pk);
                if(rchild_op == MC::Bexp::Head::CONST){
                    if(rchild_const == 1){
                        rchild_ctxt = true_ctxt;
                    }
                    else{
                        rchild_ctxt = false_ctxt;
                    }
                }
                else if(rchild_op == MC::Bexp::Head::VAR){
                    rchild_ctxt = memory.find(rchild_var)->second;
                }
                
                //ZZX rchild_res;
                //sk.Decrypt(rchild_res, rchild_ctxt);

                rchild_ctxt += true_ctxt;

                /*
                ZZX and_res;
                sk.Decrypt(and_res, rchild_ctxt);
                cout << "not " << rchild_var << " = " << lv << endl;
                cout << "not " << rchild_res[0] << " = " << and_res[0] << endl;
                */

                memory.insert(make_pair(lv, rchild_ctxt));
            }
            

        }
        cout << "circuit evaluation finished" << endl;
        for(auto i = 0; i < outputlist.size(); i++){
            ZZX tmp_res;
            sk.Decrypt(tmp_res, memory.find(outputlist[i])->second) ;
            cout << "output : " << outputlist[i] << " : " << tmp_res[0] << endl;   
        }

        std::chrono::system_clock::time_point EndTime = std::chrono::system_clock::now();
        std::chrono::milliseconds mill  = std::chrono::duration_cast<std::chrono::milliseconds>(EndTime - StartTime);

	//int eval_time = time(0) - start_time;
	string circuit_filename = argv[1];
	string depth_string = argv[2];
	circuit_filename += depth_string;
	
	
        cerr << depth_string << ", " <<  mill.count()  << endl;
        /*}*/
        //driver.print( std::cout ) << "\n";
    }
    else
    {
        /** exit with failure condition **/
        return ( EXIT_FAILURE );
    }
    return( EXIT_SUCCESS );
}
