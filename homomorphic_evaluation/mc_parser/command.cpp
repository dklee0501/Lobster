#include "command.hpp"
#include <iostream>
#include <sstream>


using namespace MC;

using std::cout;
using std::endl;


Bexp::Bexp()
{
	constant = -1;
	var = "";
	left = NULL;
	right = NULL;
}

