#include <clopts.hh>
#include <data.hh>
#include <unordered_set>
#include <utils.hh>

using namespace std::literals;

template <>
struct fmt::formatter<Tk> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(Tk const& t, FormatContext& ctx) {
        switch (t) {
            case Tk::Invalid: return format_to(ctx.out(), "Invalid");
            case Tk::Eof: return format_to(ctx.out(), "Eof");
            case Tk::Unknown: return format_to(ctx.out(), "Unknown");

            case Tk::CharacterLiteral: return format_to(ctx.out(), "CharacterLiteral");
            case Tk::CharacterOrStringLiteralPrefix: return format_to(ctx.out(), "CharacterOrStringLiteralPrefix");
            case Tk::Class: return format_to(ctx.out(), "Class");
            case Tk::Comment: return format_to(ctx.out(), "Comment");
            case Tk::EscapeSequence: return format_to(ctx.out(), "EscapeSequence");
            case Tk::Function: return format_to(ctx.out(), "Function");
            case Tk::HeaderName: return format_to(ctx.out(), "HeaderName");
            case Tk::Ident: return format_to(ctx.out(), "Ident");
            case Tk::Keyword: return format_to(ctx.out(), "Keyword");
            case Tk::Namespace: return format_to(ctx.out(), "Namespace");
            case Tk::Number: return format_to(ctx.out(), "Number");
            case Tk::Punct: return format_to(ctx.out(), "Punct");
            case Tk::RawStringDelimiter: return format_to(ctx.out(), "RawStringDelimiter");
            case Tk::StringLiteral: return format_to(ctx.out(), "StringLiteral");
            case Tk::Typedef: return format_to(ctx.out(), "Typedef");
            case Tk::Whitespace: return format_to(ctx.out(), "Whitespace");
        }

        die("unreachable");
    }
};

auto Escape(std::string_view s) -> std::string {
    std::string escaped;
    escaped.reserve(s.size());
    for (auto c : s) {
        switch (c) {
            case '\a': escaped += "\\a"; break;
            case '\b': escaped += "\\b"; break;
            case '\f': escaped += "\\f"; break;
            case '\n': escaped += "\\n"; break;
            case '\r': escaped += "\\r"; break;
            case '\t': escaped += "\\t"; break;
            case '\v': escaped += "\\v"; break;
            case '\\': escaped += "\\\\"; break;
            case '\'': escaped += "\\'"; break;
            case '\"': escaped += "\\\""; break;
            default: escaped += c; break;
        }
    }
    return escaped;
}

struct Token {
    Tk type = Tk::Eof;
    std::string_view text;
};

class Parser {
    std::string_view content;
    const char* curr = content.data();
    const char* const end = content.data() + content.size();
    std::vector<Token> tokens;
    Token EofToken{.type = Tk::Eof, .text = {curr, 0}};

public:
    Parser(std::string_view content) : content{content} {
        while (curr != end) Next();
    }

    static bool IsDigit(char c);
    static bool IsIdentifierContinue(char c);
    static bool IsNonDigit(char c);
    static bool IsPunct(char c);
    static bool IsWhitespace(char c);

    void DumpTokens();
    auto ToString() -> std::string;

    void LexCharOrStringLiteral(bool raw);
    void Next();
    auto Prev(usz index) -> Token&;
    void ReadWhile(Token& tok, bool Predicate(char));

    auto Sv() { return std::string_view{curr, usz(end - curr)}; }
};

/// digit: one of
///     0 1 2 3 4 5 6 7 8 9
bool Parser::IsDigit(char c) {
    return c >= '0' and c <= '9';
}

/// identifier-continue:
///     digit
///     nondigit
///     an element of the translation character set with the Unicode property XID_Continue
bool Parser::IsIdentifierContinue(char c) {
    return IsDigit(c) or IsNonDigit(c);
}

/// nondigit: one of
///     a b c d e f g h i j k l m
///     n o p q r s t u v w x y z
///     A B C D E F G H I J K L M
///     N O P Q R S T U V W X Y Z _
bool Parser::IsNonDigit(char c) {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

/// preprocessing-operator: one of
///     #    ##   %:   %:%:
///
/// operator-or-punctuator: one of
///     {    }    [    ]    (    )
///     <:   :>   <%   %>   ;    :    ...
///     ?    ::   .    .*   ->   ->*  ~
///     !    +    -    *    /    %    ^    &    |
///     =    +=   -=   *=   /=   %=   ^=   &=   |=
///     ==   !=   <    >    <=   >=   <=>  &&   ||
///     <<   >>   <<=  >>=  ++   --   ,
bool Parser::IsPunct(char c) {
    switch (c) {
        case '#':
        case '%':
        case ':':
        case '{':
        case '}':
        case '[':
        case ']':
        case '(':
        case ')':
        case '<':
        case '>':
        case ';':
        case '.':
        case '?':
        case '*':
        case '-':
        case '~':
        case '!':
        case '+':
        case '/':
        case '^':
        case '&':
        case '|':
        case '=':
        case ',':
            return true;
    }

    return false;
}

bool Parser::IsWhitespace(char c) {
    return " \t\n\r\v\f"sv.contains(c);
}

void Parser::LexCharOrStringLiteral(const bool raw) {
    const char delim = *curr;

    /// Raw strings get special handling.
    if (raw) {
        /// Delimiter is a string literal token.
        tokens.emplace_back(Tk::StringLiteral, std::string_view{curr, 1});
        curr++;

        /// Read 'foo('.
        auto& start = tokens.emplace_back(Tk::RawStringDelimiter);
        ReadWhile(start, [](char c) { return c != '('; });
        if (curr == end) return;
        start.text = {start.text.data(), start.text.size() + 1};
        curr++;
        if (curr == end) return;

        /// Read the contents of the string.
        auto& str = tokens.emplace_back(Tk::StringLiteral);
        std::string end_text = fmt::format("){}", std::string_view{start.text.data(), start.text.size() - 1});
        auto end_pos = content.find(end_text, usz(curr - content.data()));
        if (end_pos == std::string_view::npos) return;
        str.text = {curr, end_pos - usz(curr - content.data())};
        curr += str.text.size();

        /// Read the end delimiter.
        auto& end = tokens.emplace_back(Tk::RawStringDelimiter);
        end.text = {end_text.data(), end_text.size()};
        curr += end.text.size();

        /// Delimiter is a string literal token.
        if (curr == this->end) return;
        tokens.emplace_back(Tk::StringLiteral, std::string_view{curr, 1});
        curr++;
        return;
    }

    /// Regular char or string literal.
    auto kind = delim == '\'' ? Tk::CharacterLiteral : Tk::StringLiteral;
    auto* tok = &tokens.emplace_back(kind);
    tok->text = {curr, 1};
    if (curr == end) return;
    curr++;
    while (curr != end) {
        if (auto c = *curr; c == '\\') {
            tok = &tokens.emplace_back(Tk::EscapeSequence);
            if (curr + 1 == end) return;
            tok->text = {curr, 2};
            curr++;
            curr++;
        }

        else {
            if (tok->type == Tk::EscapeSequence) tok = &tokens.emplace_back(kind);
            if (tok->text.empty()) tok->text = {curr, 1};
            else tok->text = {tok->text.data(), tok->text.size() + 1};
            curr++;
            if (c == delim) break;
        }
    }
}

void Parser::Next() {
    if (curr == end) {
        tokens.emplace_back();
        return;
    }

    /// Whitespace.
    if (IsWhitespace(*curr)) {
        auto& tok = tokens.emplace_back(Tk::Whitespace);
        ReadWhile(tok, IsWhitespace);
        return;
    }

    /// Identifiers, keywords, and literals w/ prefixes.
    if (IsNonDigit(*curr)) {
        auto sv = Sv();

        /// Character/string literal.
        if (
            sv.starts_with("u8'") or
            sv.starts_with("u'") or
            sv.starts_with("U'") or
            sv.starts_with("L'") or
            sv.starts_with("u8\"") or
            sv.starts_with("u\"") or
            sv.starts_with("U\"") or
            sv.starts_with("L\"") or
            sv.starts_with("R\"") or
            sv.starts_with("u8R\"") or
            sv.starts_with("uR\"") or
            sv.starts_with("UR\"") or
            sv.starts_with("LR\"")
        ) {
            /// Prefix.
            auto& tok = tokens.emplace_back(Tk::CharacterOrStringLiteralPrefix);
            ReadWhile(tok, IsIdentifierContinue);

            /// Literal.
            LexCharOrStringLiteral(tok.text.ends_with("R"));
            return;
        }

        /// Identifier.
        auto& tok = tokens.emplace_back(Tk::Ident);
        ReadWhile(tok, IsIdentifierContinue);
        if (keywords.contains(tok.text)) {
            tok.type = Tk::Keyword;
            return;
        }

        /// Make an educated guess as to what this might be. First, check
        /// if we’re in the standard namespace.
        if (
            Prev(1).text.ends_with("::") and
            (Prev(2).type == Tk::Namespace and Prev(2).text == "std")
        ) {
            if (known_std_classes.contains(tok.text)) {
                tok.type = Tk::Class;
                return;
            }

            if (known_std_functions.contains(tok.text)) {
                tok.type = Tk::Function;
                return;
            }

            if (known_std_typedefs.contains(tok.text)) {
                tok.type = Tk::Typedef;
                return;
            }
        }

        /// Check if this is a namespace.
        if (known_namespaces.contains(tok.text)) {
            tok.type = Tk::Namespace;
            return;
        }

        /// Otherwise, this is just an identifier.
        return;
    }

    /// Handle line comments.
    if (Sv().starts_with("//")) {
        auto line_end = content.find('\n', usz(curr - content.data()));
        if (line_end == std::string_view::npos) return;
        auto& tok = tokens.emplace_back(Tk::Comment);
        tok.text = {curr, line_end - usz(curr - content.data())};
        curr += tok.text.size();
        return;
    }

    /// Punctuators (alternative representations excluded).
    if (IsPunct(*curr)) {
        auto& tok = tokens.emplace_back(Tk::Punct);
        ReadWhile(tok, IsPunct);

        /// Handle preprocessor directives. Note that we allow whitespace,
        /// but not comments, inbetween the '#' and the directive name.
        if (tok.text == "#" and (tokens.size() < 2 or tokens[tokens.size() - 2].text.contains('\n'))) {
            if (IsWhitespace(*curr)) Next();
            if (curr == end or not IsNonDigit(*curr)) return;
            Next();
            if (curr == end) return;

            if (tokens.back().type == Tk::Ident) {
                tokens.back().type = Tk::Keyword;
            }

            /// Handle header-names after #include.
            if (tokens.back().text == "include") {
                if (IsWhitespace(*curr)) Next();
                if (curr == end or (*curr != '<' and *curr != '\"')) return;
                const char delim = *curr;
                auto name_end = content.find(delim == '<' ? '>' : '"', usz(curr - content.data()) + 1);
                if (name_end == std::string_view::npos) return;
                auto& tok = tokens.emplace_back(Tk::HeaderName);
                tok.text = {curr, name_end - usz(curr - content.data()) + 1};
                curr += tok.text.size();
                return;
            }
        }

        return;
    }

    /// Integer literals.
    if (IsDigit(*curr)) {
        auto& tok = tokens.emplace_back(Tk::Number);
        ReadWhile(tok, [](char c) { return IsDigit(c) or "bBoOxXuUlLzZeEpPfF-+'."sv.contains(c); });
        return;
    }

    /// String and character literals.
    if (*curr == '\'' or *curr == '"') {
        LexCharOrStringLiteral(false);
        return;
    }

    /// Unrecognised character.
    tokens.emplace_back(Tk::Unknown, std::string_view{curr, 1});
    curr++;
}

auto Parser::Prev(usz index) -> Token& {
    if (index >= tokens.size()) return EofToken;
    return tokens[tokens.size() - index - 1];
}

void Parser::ReadWhile(Token& tok, bool Predicate(char)) {
    tok.text = {curr, 1};
    curr++;
    while (curr != end and Predicate(*curr)) {
        tok.text = {tok.text.data(), tok.text.size() + 1};
        curr++;
    }
}

void Parser::DumpTokens() {
    for (auto& t : tokens) {
        fmt::print("{}: \"{}\"\n", t.type, Escape(t.text));
    }
}

auto Parser::ToString() -> std::string {
    std::string out;
    for (auto& t : tokens) {
        switch (t.type) {
            case Tk::Invalid: out += "<invalid>";
            case Tk::Eof: break;

            case Tk::Unknown:
                out += fmt::format("\033[m{}", t.text);
                break;

            case Tk::CharacterLiteral:
            case Tk::HeaderName:
            case Tk::StringLiteral:
            case Tk::Typedef:
                out += fmt::format("\033[33m{}\033[m", t.text);
                break;

            case Tk::Function:
                out += fmt::format("\033[32m{}\033[m", t.text);
                break;

            case Tk::Namespace:
                out += fmt::format("\033[34m{}\033[m", t.text);
                break;

            case Tk::CharacterOrStringLiteralPrefix:
            case Tk::EscapeSequence:
            case Tk::RawStringDelimiter:
            case Tk::Class:
                out += fmt::format("\033[36m{}\033[m", t.text);
                break;

            case Tk::Comment:
                out += fmt::format("\033[38;2;155;131;96m{}\033[m", t.text);
                break;

            case Tk::Ident:
                out += fmt::format("\033[38m{}\033[m", t.text);
                break;

            case Tk::Keyword:
            case Tk::Punct:
                out += fmt::format("\033[31m{}\033[m", t.text);
                break;

            case Tk::Number:
                out += fmt::format("\033[35m{}\033[m", t.text);
                break;

            case Tk::Whitespace:
                out += t.text;
                break;
        }
    }

    return out;
}

namespace detail { // clang-format off
using namespace command_line_options;
using options = clopts<
    positional<"file", "The file to process", file_data>,
    flag<"--debug">,
    help<>
>;
} // clang-format on

using detail::options;

int main(int argc, char** argv) {
    auto opts = options::parse(argc, argv);
    Parser p{opts.get<"file">()->contents};
    if (opts.get<"--debug">()) p.DumpTokens();
    else fmt::print("{}", p.ToString());
}
