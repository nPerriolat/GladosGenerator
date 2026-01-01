#include <iostream>
#include <fstream>
#include <filesystem>
#include <algorithm>

#include "GladosGenerator.hpp"
#include "Error.hpp"

namespace GG {
    bool doesFileExists(const std::string& path) {
        return std::filesystem::exists(path) && std::filesystem::is_regular_file(path);
    }

    Data::Data(int argc, char **argv) {
        std::string lexiconPath{ "./lexical_rules" };
        std::string syntaxPath{ "./syntax_rules" };
        bool firstMatch = true;
        if (argc >= 2) {
            std::string arg{ argv[1] };
            if (arg.starts_with("-l=\"") && arg.ends_with("\"")) {
                lexiconPath = arg.substr(4, arg.size() - 3);
                firstMatch = true;
            } else if (arg.starts_with("-s=\"") && arg.ends_with("\"")) {
                syntaxPath = arg.substr(4, arg.size() - 3);
                firstMatch = false;
            } else {
                throw GG::Error(GG::Error::BAD_ARGUMENTS);
            }
        }
        if (argc == 3) {
            std::string arg{ argv[2] };
            if (arg.starts_with("-l=\"") && arg.ends_with("\"")) {
                if (firstMatch) throw GG::Error(GG::Error::BAD_ARGUMENTS);
                lexiconPath = arg.substr(4, arg.size() - 3);
            } else if (arg.starts_with("-s=\"") && arg.ends_with("\"")) {
                if (!firstMatch) throw GG::Error(GG::Error::BAD_ARGUMENTS);
                syntaxPath = arg.substr(4, arg.size() - 3);
            } else {
                throw GG::Error(GG::Error::BAD_ARGUMENTS);
            }
        }
        if (!doesFileExists(lexiconPath)) throw GG::Error(GG::Error::LEXICON_NOT_FOUND);
        if (!doesFileExists(syntaxPath)) throw GG::Error(GG::Error::SYNTAX_NOT_FOUND);

        try {
            std::ifstream file(lexiconPath);
            std::string line;
            while (std::getline(file, line)) {
                size_t pos = line.find("::=");
                if (pos != std::string::npos) {
                    std::string key = line.substr(0, pos);
                    key.erase(0, key.find_first_not_of(" \t\n\r\f\v"));
                    key.erase(key.find_last_not_of(" \t\n\r\f\v") + 1);
                    std::string value = line.substr(pos + 3);
                    value.erase(0, value.find_first_not_of(" \t\n\r\f\v"));
                    value.erase(value.find_last_not_of(" \t\n\r\f\v") + 1);
                    for (size_t i = 0; i < value.size() - 1; i++) {
                        if (value[i] == '\\' && value[i + 1] == '\\') {
                            value.erase(i, 1);
                        } else if (value[i] == '\\' && value[i + 1] != '\\') {
                            value.erase(i, 1);
                            i--;
                        }
                    }
                    //std::cout << "key=" << key << "\t" << "value=" << value << std::endl;
                    this->lexicon[key] = value;
                } else {
                    pos = line.find("/* You cannot modify the following rules */");
                    if (pos != std::string::npos) break;
                }
            }
        } catch (std::exception &e) {
            throw GG::Error(GG::Error::LEXICON_BAD_FORMAT);
        }
    }

    void Data::generate() {
        const std::filesystem::path glados{ "./Glados/" };
        const std::filesystem::path output{ "./Output/" };
        const std::string outputLexicon{ "./Output/src/Lexicon.hs" };

        std::filesystem::copy(glados, output,
            std::filesystem::copy_options::recursive |
            std::filesystem::copy_options::overwrite_existing);
        
        std::ifstream in(outputLexicon);
        std::string content((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
        in.close();

        std::vector<std::pair<std::string, std::string>> entries(this->lexicon.begin(), this->lexicon.end());
        std::sort(entries.begin(), entries.end(), [](auto& a, auto& b) { return a.first.size() > b.first.size(); });

        for (const auto& [key, value] : entries) {
            size_t pos = 0;
            
            std::string safeValue;
            for (char c : value) {
                if (c == '"') {
                    safeValue += "\\\"";
                } else {
                    safeValue += c;
                }
            }

            while ((pos = content.find(key, pos)) != std::string::npos) {
                content.replace(pos, key.length(), safeValue);
                pos += safeValue.length();
            }
        }
        std::ofstream out(outputLexicon);
        out << content;
        out.close();
    }
}
