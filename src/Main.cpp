#include <iostream>
#include <cerrno>
#include "GladosGenerator.hpp"
#include "Error.hpp"

static int help() {
    std::cout << "USAGE" << std::endl
        << "\t./GladosGenerator [-l=lexical_rules] [-s=syntax_rules]" << std::endl << std::endl
        << "DESCRIPTION" << std::endl
        << "\t-l=lexical_rules\tPath to the lexical rules file. If not provided default is \"./lexical_rules\"." << std::endl
        << "\t-s=syntax_rules\tPath to the syntax rules file. If not provided default is \"./syntax_rules\"." << std::endl;
    return 0;
}

int main(int argc, char **argv) {
    try {
        if (argc < 1 || argc > 3)
            throw GG::Error(GG::Error::BAD_ARGUMENTS);
        if (argc >= 2 && std::string(argv[1]) == std::string("-h"))
            return help();
        GG::Data data(argc, argv);
        data.generate();
    } catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
        return errno;
    }
    return 0;
}
