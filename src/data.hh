#ifndef CXX_TOKENISE_DATA_HH
#define CXX_TOKENISE_DATA_HH

#include <string_view>
#include <unordered_set>

enum struct Tk {
    Invalid,
    Eof,
    Unknown,

    CharacterLiteral,
    CharacterOrStringLiteralPrefix,
    Class,
    Comment,
    EscapeSequence,
    Function,
    HeaderName,
    Ident,
    Keyword,
    Namespace,
    Number,
    Punct,
    RawStringDelimiter,
    StringLiteral,
    Typedef,
    Whitespace,
};

extern std::unordered_set<std::string_view> keywords;
extern std::unordered_set<std::string_view> known_namespaces;
extern std::unordered_set<std::string_view> known_std_classes;
extern std::unordered_set<std::string_view> known_std_functions;
extern std::unordered_set<std::string_view> known_std_typedefs;

#endif // CXX_TOKENISE_DATA_HH
