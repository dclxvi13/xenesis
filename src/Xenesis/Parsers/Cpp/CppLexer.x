{
module Xenesis.Parsers.Cpp.CppLexer where

--import
}

%wrapper "posn"

$digit      = [0-9]
$nonzero    = [1-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]

@lineterm = [\n\r] | \r\n

-- TODO: this doesn't notice a comment that ends "**/"
@tradcomm = "/*" ( ~[\*] | \*+ (~[\/\*] | \n) | \n )* \*+ "/"
@linecomm = "//" .* @lineterm
@comm = @tradcomm | @linecomm

$cppLetter = [a-zA-Z\_]
$cppDigit = $digit
$cppLetterOrDigit = [a-zA-Z0-9\_]

@expsuffix = [\+\-]? $digit+
@exponent = [eE] @expsuffix
@pexponent = [pP] @expsuffix

@octEscape = [0123]? $octdig{1,2}
@hexEscape = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])


tokens  :-
    -- Whitespaces & comments
    $white+         ;
    @comm           ;

    -- Keywords
    alignas         {\p _ -> L (pos p) $ KW_alignas }
    alignof         {\p _ -> L (pos p) $ KW_alignof }
    asm             {\p _ -> L (pos p) $ KW_asm }
    auto            {\p _ -> L (pos p) $ KW_auto }
    bool            {\p _ -> L (pos p) $ KW_bool }
    break           {\p _ -> L (pos p) $ KW_break }
    case            {\p _ -> L (pos p) $ KW_case }
    catch           {\p _ -> L (pos p) $ KW_catch }
    char            {\p _ -> L (pos p) $ KW_char }
    char16_t        {\p _ -> L (pos p) $ KW_char16_t }
    char32_t        {\p _ -> L (pos p) $ KW_char32_t }
    class           {\p _ -> L (pos p) $ KW_class }
    const           {\p _ -> L (pos p) $ KW_const }
    constexpr       {\p _ -> L (pos p) $ KW_constexpr }
    const_cast      {\p _ -> L (pos p) $ KW_const_cast }
    continue        {\p _ -> L (pos p) $ KW_continue }
    decltype        {\p _ -> L (pos p) $ KW_decltype }
    default         {\p _ -> L (pos p) $ KW_default }
    delete          {\p _ -> L (pos p) $ KW_delete }
    do              {\p _ -> L (pos p) $ KW_do }
    double          {\p _ -> L (pos p) $ KW_double }
    dynamic_cast    {\p _ -> L (pos p) $ KW_dynamic_cast }
    else            {\p _ -> L (pos p) $ KW_else }
    enum            {\p _ -> L (pos p) $ KW_enum }
    explicit        {\p _ -> L (pos p) $ KW_explicit }
    export          {\p _ -> L (pos p) $ KW_export }
    extern          {\p _ -> L (pos p) $ KW_extern }
    float           {\p _ -> L (pos p) $ KW_float }
    for             {\p _ -> L (pos p) $ KW_for }
    friend          {\p _ -> L (pos p) $ KW_friend }
    goto            {\p _ -> L (pos p) $ KW_goto }
    if              {\p _ -> L (pos p) $ KW_if }
    inline          {\p _ -> L (pos p) $ KW_inline }
    int             {\p _ -> L (pos p) $ KW_int }
    long            {\p _ -> L (pos p) $ KW_long }
    mutable         {\p _ -> L (pos p) $ KW_mutable }
    namespace       {\p _ -> L (pos p) $ KW_namespace }
    new             {\p _ -> L (pos p) $ KW_new }
    noexcept        {\p _ -> L (pos p) $ KW_noexcept }
    operator        {\p _ -> L (pos p) $ KW_operator }
    private         {\p _ -> L (pos p) $ KW_private }
    protected       {\p _ -> L (pos p) $ KW_protected }
    public          {\p _ -> L (pos p) $ KW_public }
    register        {\p _ -> L (pos p) $ KW_register }
    reinterpret_cast {\p _ -> L (pos p) $ KW_reinterpret_cast }
    return          {\p _ -> L (pos p) $ KW_return }
    short           {\p _ -> L (pos p) $ KW_short }
    signed          {\p _ -> L (pos p) $ KW_signed }
    sizeof          {\p _ -> L (pos p) $ KW_sizeof }
    static          {\p _ -> L (pos p) $ KW_static }
    static_assert   {\p _ -> L (pos p) $ KW_static_assert }
    static_cast     {\p _ -> L (pos p) $ KW_static_cast }
    struct          {\p _ -> L (pos p) $ KW_struct }
    switch          {\p _ -> L (pos p) $ KW_switch }
    template        {\p _ -> L (pos p) $ KW_template }
    this            {\p _ -> L (pos p) $ KW_this }
    thread_local    {\p _ -> L (pos p) $ KW_thread_local }
    throw           {\p _ -> L (pos p) $ KW_throw }
    try             {\p _ -> L (pos p) $ KW_try }
    typedef         {\p _ -> L (pos p) $ KW_typedef }
    typeid          {\p _ -> L (pos p) $ KW_typeid }
    typename        {\p _ -> L (pos p) $ KW_typename }
    union           {\p _ -> L (pos p) $ KW_union }
    unsigned        {\p _ -> L (pos p) $ KW_unsigned }
    using           {\p _ -> L (pos p) $ KW_using }
    virtual         {\p _ -> L (pos p) $ KW_virtual }
    void            {\p _ -> L (pos p) $ KW_void }
    volatile        {\p _ -> L (pos p) $ KW_volatile }
    wchar_t         {\p _ -> L (pos p) $ KW_wchar_t }
    while           {\p _ -> L (pos p) $ KW_while }

    nullptr         {\p _ -> L (pos p) $ Token_Nullptr }

    true            {\p _ -> L (pos p) $ Lit_boolean True }
    false           {\p _ -> L (pos p) $ Lit_boolean False }

    -- Literals
    0                               { \p _ -> L (pos p) $ Lit_integer 0 }
    $nonzero $digit*                { \p s -> L (pos p) $ Lit_integer (read s) }
    -- TODO float

    ' (@charEscape | ~[\\\']) '               { \p s -> L (pos p) $ Lit_char (readCharTok s) }

    \" (@charEscape | ~[\\\"])* \"            { \p s -> L (pos p) $ Lit_string (readStringTok s) }


    -- Identifier
    $cppLetter $cppLetterOrDigit*   { \p s -> L (pos p) $ IdentifierToken s }

    -- Punctuation & Operators
    \{              {\p _ -> L (pos p) $ Punc_OpenCurly   }
    \}              {\p _ -> L (pos p) $ Punc_CloseCurly  }
    \[              {\p _ -> L (pos p) $ Punc_OpenSquare  }
    \]              {\p _ -> L (pos p) $ Punc_CloseSquare }
    \(              {\p _ -> L (pos p) $ Punc_OpenParen   }
    \)              {\p _ -> L (pos p) $ Punc_CloseParen  }
    \;              {\p _ -> L (pos p) $ Punc_SemiColon   }
    \.              {\p _ -> L (pos p) $ Punc_Period      }
    \,              {\p _ -> L (pos p) $ Punc_Comma       }
    ":"             {\p _ -> L (pos p) $ Punc_Colon       }
--    "?"             {\p _ -> L (pos p) $ Op_ }
    "#"             {\p _ -> L (pos p) $ Punc_Hash }
--    "##"            {\p _ -> L (pos p) $ Op_ }
--    "<:"            {\p _ -> L (pos p) $ Op_ }
--    ":>"            {\p _ -> L (pos p) $ Op_ }
--    "<%"            {\p _ -> L (pos p) $ Op_ }
--    "%>"            {\p _ -> L (pos p) $ Op_ }
--    "%:"            {\p _ -> L (pos p) $ Op_ }
--    "%:%:"          {\p _ -> L (pos p) $ Op_ }
--    "..."           {\p _ -> L (pos p) $ Op_ }
--    "::"            {\p _ -> L (pos p) $ Op_ }
--    ".*"            {\p _ -> L (pos p) $ Op_ }
    "+"             {\p _ -> L (pos p) $ Op_Add             }
    "-"             {\p _ -> L (pos p) $ Op_Sub             }
    "*"             {\p _ -> L (pos p) $ Op_Mul             }
    "/"             {\p _ -> L (pos p) $ Op_Div             }
    "%"             {\p _ -> L (pos p) $ Op_Mod             }
    "^"             {\p _ -> L (pos p) $ Op_BitXor          }
    "&"             {\p _ -> L (pos p) $ Op_BitAnd          }
    "|"             {\p _ -> L (pos p) $ Op_BitOr           }
    "~"             {\p _ -> L (pos p) $ Op_BitNot          }
    "!"             {\p _ -> L (pos p) $ Op_Not             }
    "="             {\p _ -> L (pos p) $ Op_Set             }
    "<"             {\p _ -> L (pos p) $ Op_Less            }
    ">"             {\p _ -> L (pos p) $ Op_Greater         }
    "+="            {\p _ -> L (pos p) $ Op_AddEq           }
    "-="            {\p _ -> L (pos p) $ Op_SubEq           }
    "*="            {\p _ -> L (pos p) $ Op_MulEq           }
    "/="            {\p _ -> L (pos p) $ Op_DivEq           }
    "%="            {\p _ -> L (pos p) $ Op_ModEq           }
    "^="            {\p _ -> L (pos p) $ Op_BitXorEq        }
    "&="            {\p _ -> L (pos p) $ Op_BitAndEq        }
    "|="            {\p _ -> L (pos p) $ Op_BitOrEq         }
    "<<"            {\p _ -> L (pos p) $ Op_LeftShift       }
    ">>"            {\p _ -> L (pos p) $ Op_RightShift      }
    "<<="           {\p _ -> L (pos p) $ Op_LeftShiftEq     }
    ">>="           {\p _ -> L (pos p) $ Op_RightShiftEq    }
    "=="            {\p _ -> L (pos p) $ Op_Eq              }
    "!="            {\p _ -> L (pos p) $ Op_NotEq           }
    "<="            {\p _ -> L (pos p) $ Op_LessEq          }
    ">="            {\p _ -> L (pos p) $ Op_GreaterEq       }
    "&&"            {\p _ -> L (pos p) $ Op_And             }
    "||"            {\p _ -> L (pos p) $ Op_Or              }
    "++"            {\p _ -> L (pos p) $ Op_Incr            }
    "--"            {\p _ -> L (pos p) $ Op_Decr            }
--    "->*"           {\p _ -> L (pos p) $ Op_                }
--    "->"            {\p _ -> L (pos p) $ Op_                }
    and             {\p _ -> L (pos p) $ Op_AndL            }
    and_eq          {\p _ -> L (pos p) $ Op_AndEqL          }
    bitand          {\p _ -> L (pos p) $ Op_BitAndL         }
    bitor           {\p _ -> L (pos p) $ Op_BitOrL  }
    compl           {\p _ -> L (pos p) $ Op_ComplL  }
    not             {\p _ -> L (pos p) $ Op_NotL    }
    not_eq          {\p _ -> L (pos p) $ Op_NotEqL  }
    or              {\p _ -> L (pos p) $ Op_OrL     }
    or_eq           {\p _ -> L (pos p) $ Op_OrEqL   }
    xor             {\p _ -> L (pos p) $ Op_XorL    }
    xor_eq          {\p _ -> L (pos p) $ Op_XorEqL  }

{
data Token
    -- Identifier
    = IdentifierToken String

    -- Keywords
    | KW_alignas
    | KW_alignof
    | KW_asm
    | KW_auto
    | KW_bool
    | KW_break
    | KW_case
    | KW_catch
    | KW_char
    | KW_char16_t
    | KW_char32_t
    | KW_class
    | KW_const
    | KW_constexpr
    | KW_const_cast
    | KW_continue
    | KW_decltype
    | KW_default
    | KW_delete
    | KW_do
    | KW_double
    | KW_dynamic_cast
    | KW_else
    | KW_enum
    | KW_explicit
    | KW_export     -- C++0x - Reserved for future use
    | KW_extern
    | KW_float
    | KW_for
    | KW_friend
    | KW_goto
    | KW_if
    | KW_inline
    | KW_int
    | KW_long
    | KW_mutable
    | KW_namespace
    | KW_new
    | KW_noexcept
    | KW_operator
    | KW_private
    | KW_protected
    | KW_public
    | KW_register
    | KW_reinterpret_cast
    | KW_return
    | KW_short
    | KW_signed
    | KW_sizeof
    | KW_static
    | KW_static_assert
    | KW_static_cast
    | KW_struct
    | KW_switch
    | KW_template
    | KW_this
    | KW_thread_local
    | KW_throw
    | KW_try
    | KW_typedef
    | KW_typeid
    | KW_typename
    | KW_union
    | KW_unsigned
    | KW_using
    | KW_virtual
    | KW_void
    | KW_volatile
    | KW_wchar_t
    | KW_while

    -- nullptr
    | Token_Nullptr

    -- Literals
    | Lit_integer   Integer
    | Lit_char      Char
    | Lit_float     Float
    | Lit_string    String
    | Lit_boolean   Bool
    | Lit_user      -- TODO

    -- Operators & Punctuators
    | Punc_OpenCurly
    | Punc_CloseCurly
    | Punc_OpenSquare
    | Punc_CloseSquare
    | Punc_OpenParen
    | Punc_CloseParen
    | Punc_SemiColon
    | Punc_Period
    | Punc_Comma
    | Punc_Colon
    | Punc_Hash
    | Op_Add
    | Op_Sub
    | Op_Mul
    | Op_Div
    | Op_Mod
    | Op_BitXor
    | Op_BitAnd
    | Op_BitOr
    | Op_BitNot
    | Op_Not
    | Op_Set
    | Op_Less
    | Op_Greater
    | Op_AddEq
    | Op_SubEq
    | Op_MulEq
    | Op_DivEq
    | Op_ModEq
    | Op_BitXorEq
    | Op_BitAndEq
    | Op_BitOrEq
    | Op_LeftShift
    | Op_RightShift
    | Op_LeftShiftEq
    | Op_RightShiftEq
    | Op_Eq
    | Op_NotEq
    | Op_LessEq
    | Op_GreaterEq
    | Op_And
    | Op_Or
    | Op_Incr
    | Op_Decr
    | Op_AndL
    | Op_AndEqL
    | Op_BitAndL
    | Op_BitOrL
    | Op_ComplL
    | Op_NotL
    | Op_NotEqL
    | Op_OrL
    | Op_OrEqL
    | Op_XorL
    | Op_XorEqL
    deriving (Show, Eq)

lexicalError :: String -> a
lexicalError = error . ("lexical error: " ++)

data L a = L Pos a
  deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)

pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = (l,c)

readCharTok :: String -> Char
readCharTok s = head . dropQuotes $ s
readStringTok :: String -> String
readStringTok = dropQuotes

dropQuotes :: String -> String
dropQuotes s = take (length s - 2) (tail s)

lexer :: String -> [L Token]
lexer = alexScanTokens
}