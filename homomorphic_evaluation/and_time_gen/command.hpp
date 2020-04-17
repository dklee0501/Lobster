#ifndef COMMAND_H
#define COMMAND_H

#include <string>
#include <vector>
#include <stdint.h>

namespace MC {

	class Bexp
	{
		public:
			enum Head 
			{
				CONST,
				VAR,
				AND,
				XOR,
				OR,
				NOT
			};
			Bexp();

			Head head;
			int constant;
			std::string var;
			Bexp *left;
			Bexp *right;
	};

	class Command
	{
		public:
			Command(const std::string &name, const std::vector<uint64_t> arguments);
			Command(const std::string &name);
			Command();
			~Command();

			std::string str() const;
			std::string name() const;

		private:
			std::string m_name;
			std::vector<uint64_t> m_args;
	};

}

#endif // COMMAND_H
