#ifndef __MCDRIVER_HPP__
#define __MCDRIVER_HPP__ 1

#include <string>
#include <cstddef>
#include <istream>
#include <vector>
#include <tuple>

#include "command.hpp"
#include "mc_scanner.hpp"
#include "mc_parser.tab.hh"

using namespace std;

namespace MC
{
	class MC_Driver
	{
		public:
			MC_Driver() = default; 
			virtual ~MC_Driver();

			/** 
			 * parse - parse from a file
			 * @param filename - valid string with input file
			 */
			void parse( const char * const filename );
			/** 
			 * parse - parse from a c++ input stream
			 * @param is - std::istream&, valid input stream
			 */
			void parse( istream &iss );

			void add_word( const string &word );

			void add_input( const string &var );
			void add_output( const string &var );
			void add_bexp( const MC::Bexp &bexp );
			void add_eqn( const tuple<string, MC::Bexp*> &eqn );
			
			void add_eqn( const string &var );
			void add_consta();

			ostream& print(ostream &stream);

			vector<string> inputlist;
			vector<string> outputlist;
			vector<tuple<string, MC::Bexp*>> eqnlist;
			vector<MC::Bexp> bexplist;
		private:

			void parse_helper( istream &stream );

			MC::MC_Parser  *parser  = nullptr;
			MC::MC_Scanner *scanner = nullptr;

            /*
			vector<string> inputlist;
			vector<string> outputlist;
			vector<tuple<string, MC::Bexp*>> eqnlist;
			vector<MC::Bexp> bexplist;
            */
	};

} /* end namespace MC */
#endif /* END __MCDRIVER_HPP__ */
