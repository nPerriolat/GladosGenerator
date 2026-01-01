#ifndef GLADOS_GENERATOR
    #define GLADOS_GENERATOR
    #include <string>
    #include <map>

    namespace GG {
        class Data {
        private:
            std::map<std::string, std::string> lexicon;

        public:
            Data(int argc,	char **argv);
            ~Data() = default;

            void generate();
        };
    }
#endif
