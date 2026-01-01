#ifndef GG_CLASS_ERROR
    #define GG_CLASS_ERROR
    #include <exception>
    #include <functional>
    #include <string>

    namespace GG {
        class Error : public std::exception {
        public:
            typedef enum ErrorType {
                UNKNOWN,
                BAD_ARGUMENTS,
                LEXICON_NOT_FOUND,
                SYNTAX_NOT_FOUND,
                LEXICON_BAD_FORMAT,
                SYNTAX_BAD_FORMAT
            } ErrorType_t;

            Error(ErrorType_t type) : type(type) {};

            const char *what(void) const noexcept override {
                switch (this->type) {
                    case Error::BAD_ARGUMENTS: return "GladosGenerator : ERROR : Bad arguments! Try -h instead.";
                    case Error::LEXICON_NOT_FOUND: return "GladosGenerator : ERROR : The path to the lexicon is either invalid or don't point toward a file.";
                    case Error::SYNTAX_NOT_FOUND: return "GladosGenerator : ERROR : The path to the syntax is either invalid or don't point toward a file.";
                    case Error::LEXICON_BAD_FORMAT: return "GladosGenerator : ERROR : The lexicon file is badly formated and cannot be parsed.";
                    case Error::SYNTAX_BAD_FORMAT: return "GladosGenerator : ERROR : The syntax file is badly formated and cannot be parsed.";
                    default: return "GladosGenerator : ERROR : Undefined error";
                }
            }

        private:
            ErrorType_t type;
        };
    }
#endif
