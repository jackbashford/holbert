{
module Parse.Parser where
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	x { TokenX }

%%

X : x { XItem }

{
data ParseResult = XItem

data Token = TokenX

lexer :: String -> [Token]
lexer = map (const TokenX)

parseError = undefined
}
