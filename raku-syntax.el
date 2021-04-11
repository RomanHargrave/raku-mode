;;; raku-syntax.el --- Raku Mode: syntaxification -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Roman Hargrave <roman@hargrave.sh>
;; Author: Roman Hargrave <roman@hargrave.sh>
;;; Commentary:
;; Raku Syntax Property Table Functions and lex helpers

;;; Code:

;; * Syntax primitives
;;
;; ** Balanced constructs
;;
;; Raku supports a comprehensive set of nearly all balanced character
;; pairs in Unicode. This formidable alist was derived from
;; ~HLL/Grammar.nqp~ in the NQP (/Not Quite Perl/) source code, and
;; then "massaged" using a simple raku script to create the nicely
;; formatted table you see below. Many of the higher codepoints here
;; are written as escape sequences because their actual glyphs seem to
;; cause some indigestion for certain emacs configurations.

(setq edebug-all-forms t)

(defconst raku-brackets
  ;; Origin: (nqp) HLL/Grammar.nqp - HLL::Grammar, $brackets
  ;; tremble in horror.
  '(("<"      .      ">") ("["      .      "]") ("("      .      ")") ("{"      .      "}")
    ("("      .      ")") ("<"      .      ">") ("["      .      "]") ("{"      .      "}")
    ("«"      .      "»") ("\xF3A"  .  "\xF3B") ("\xF3C"  .  "\xF3D") ("\x169B" . "\x169C")
    ("\x2018" . "\x2019") ("\x201A" . "\x2019") ("\x201B" . "\x2019") ("\x201C" . "\x201D")
    ("\x201E" . "\x201D") ("\x201F" . "\x201D") ("\x2039" . "\x203A") ("\x2045" . "\x2046")
    ("\x207D" . "\x207E") ("\x208D" . "\x208E") ("\x2208" . "\x220B") ("\x2209" . "\x220C")
    ("\x220A" . "\x220D") ("\x2215" . "\x29F5") ("\x223C" . "\x223D") ("\x2243" . "\x22CD")
    ("\x2252" . "\x2253") ("\x2254" . "\x2255") ("\x2264" . "\x2265") ("\x2266" . "\x2267")
    ("\x2268" . "\x2269") ("\x226A" . "\x226B") ("\x226E" . "\x226F") ("\x2270" . "\x2271")
    ("\x2272" . "\x2273") ("\x2274" . "\x2275") ("\x2276" . "\x2277") ("\x2278" . "\x2279")
    ("\x227A" . "\x227B") ("\x227C" . "\x227D") ("\x227E" . "\x227F") ("\x2280" . "\x2281")
    ("\x2282" . "\x2283") ("\x2284" . "\x2285") ("\x2286" . "\x2287") ("\x2288" . "\x2289")
    ("\x228A" . "\x228B") ("\x228F" . "\x2290") ("\x2291" . "\x2292") ("\x2298" . "\x29B8")
    ("\x22A2" . "\x22A3") ("\x22A6" . "\x2ADE") ("\x22A8" . "\x2AE4") ("\x22A9" . "\x2AE3")
    ("\x22AB" . "\x2AE5") ("\x22B0" . "\x22B1") ("\x22B2" . "\x22B3") ("\x22B4" . "\x22B5")
    ("\x22B6" . "\x22B7") ("\x22C9" . "\x22CA") ("\x22CB" . "\x22CC") ("\x22D0" . "\x22D1")
    ("\x22D6" . "\x22D7") ("\x22D8" . "\x22D9") ("\x22DA" . "\x22DB") ("\x22DC" . "\x22DD")
    ("\x22DE" . "\x22DF") ("\x22E0" . "\x22E1") ("\x22E2" . "\x22E3") ("\x22E4" . "\x22E5")
    ("\x22E6" . "\x22E7") ("\x22E8" . "\x22E9") ("\x22EA" . "\x22EB") ("\x22EC" . "\x22ED")
    ("\x22F0" . "\x22F1") ("\x22F2" . "\x22FA") ("\x22F3" . "\x22FB") ("\x22F4" . "\x22FC")
    ("\x22F6" . "\x22FD") ("\x22F7" . "\x22FE") ("\x2308" . "\x2309") ("\x230A" . "\x230B")
    ("\x3008" . "\x3009") ("\x23B4" . "\x23B5") ("\x2768" . "\x2769") ("\x276A" . "\x276B")
    ("\x276C" . "\x276D") ("\x276E" . "\x276F") ("\x2770" . "\x2771") ("\x2772" . "\x2773")
    ("\x2774" . "\x2775") ("\x27C3" . "\x27C4") ("\x27C5" . "\x27C6") ("\x27D5" . "\x27D6")
    ("\x27DD" . "\x27DE") ("\x27E2" . "\x27E3") ("\x27E4" . "\x27E5") ("\x27E6" . "\x27E7")
    ("\x27E8" . "\x27E9") ("\x27EA" . "\x27EB") ("\x2983" . "\x2984") ("\x2985" . "\x2986")
    ("\x2987" . "\x2988") ("\x2989" . "\x298A") ("\x298B" . "\x298C") ("\x298D" . "\x2990")
    ("\x298F" . "\x298E") ("\x2991" . "\x2992") ("\x2993" . "\x2994") ("\x2995" . "\x2996")
    ("\x2997" . "\x2998") ("\x29C0" . "\x29C1") ("\x29C4" . "\x29C5") ("\x29CF" . "\x29D0")
    ("\x29D1" . "\x29D2") ("\x29D4" . "\x29D5") ("\x29D8" . "\x29D9") ("\x29DA" . "\x29DB")
    ("\x29F8" . "\x29F9") ("\x29FC" . "\x29FD") ("\x2A2B" . "\x2A2C") ("\x2A2D" . "\x2A2E")
    ("\x2A34" . "\x2A35") ("\x2A3C" . "\x2A3D") ("\x2A64" . "\x2A65") ("\x2A79" . "\x2A7A")
    ("\x2A7D" . "\x2A7E") ("\x2A7F" . "\x2A80") ("\x2A81" . "\x2A82") ("\x2A83" . "\x2A84")
    ("\x2A8B" . "\x2A8C") ("\x2A91" . "\x2A92") ("\x2A93" . "\x2A94") ("\x2A95" . "\x2A96")
    ("\x2A97" . "\x2A98") ("\x2A99" . "\x2A9A") ("\x2A9B" . "\x2A9C") ("\x2AA1" . "\x2AA2")
    ("\x2AA6" . "\x2AA7") ("\x2AA8" . "\x2AA9") ("\x2AAA" . "\x2AAB") ("\x2AAC" . "\x2AAD")
    ("\x2AAF" . "\x2AB0") ("\x2AB3" . "\x2AB4") ("\x2ABB" . "\x2ABC") ("\x2ABD" . "\x2ABE")
    ("\x2ABF" . "\x2AC0") ("\x2AC1" . "\x2AC2") ("\x2AC3" . "\x2AC4") ("\x2AC5" . "\x2AC6")
    ("\x2ACD" . "\x2ACE") ("\x2ACF" . "\x2AD0") ("\x2AD1" . "\x2AD2") ("\x2AD3" . "\x2AD4")
    ("\x2AD5" . "\x2AD6") ("\x2AEC" . "\x2AED") ("\x2AF7" . "\x2AF8") ("\x2AF9" . "\x2AFA")
    ("\x2E02" . "\x2E03") ("\x2E04" . "\x2E05") ("\x2E09" . "\x2E0A") ("\x2E0C" . "\x2E0D")
    ("\x2E1C" . "\x2E1D") ("\x2E20" . "\x2E21") ("\x2E28" . "\x2E29") ("\x3008" . "\x3009")
    ("\x300A" . "\x300B") ("\x300C" . "\x300D") ("\x300E" . "\x300F") ("\x3010" . "\x3011")
    ("\x3014" . "\x3015") ("\x3016" . "\x3017") ("\x3018" . "\x3019") ("\x301A" . "\x301B")
    ("\x301D" . "\x301E") ("\xFE17" . "\xFE18") ("\xFE35" . "\xFE36") ("\xFE37" . "\xFE38")
    ("\xFE39" . "\xFE3A") ("\xFE3B" . "\xFE3C") ("\xFE3D" . "\xFE3E") ("\xFE3F" . "\xFE40")
    ("\xFE41" . "\xFE42") ("\xFE43" . "\xFE44") ("\xFE47" . "\xFE48") ("\xFE59" . "\xFE5A")
    ("\xFE5B" . "\xFE5C") ("\xFE5D" . "\xFE5E") ("\xFF08" . "\xFF09") ("\xFF1C" . "\xFF1E")
    ("\xFF3B" . "\xFF3D") ("\xFF5B" . "\xFF5D") ("\xFF5F" . "\xFF60") ("\xFF62" . "\xFF63")
    ("\x27EE" . "\x27EF") ("\x2E24" . "\x2E25") ("\x27EC" . "\x27ED") ("\x2E22" . "\x2E23")
    ("\x2E26" . "\x2E27") ("\x2329" . "\x232A"))
  "Exhaustive selection of brackets a/o 2021-04.")

;; ** Syntax table
;;
;; Here, we set up the syntax table. This tells emacs about some of
;; the most basic aspects of Raku syntax. Importantly, it tells
;; emacs about all the balanced constructs we just described above.

;; See [[info:elisp#Syntax Class Table]]
(defvar raku-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; single-quoted string
    (modify-syntax-entry ?' "\"" table)
    ;; punctuation
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?- "_" table)
    ;; since we have information about all the bracket pairs,
    ;; we can fill out the bracket syntax table pretty easily
    (let ((head raku-brackets))
      (while head
        (let* ((pair  (car head))
               (open  (string-to-char (car pair)))
               (close (string-to-char (cdr pair))))
          (setq head (cdr head))
          ;; match the opening brace to the closing brace
          (modify-syntax-entry open (string ?\( close) table)
          ;; and match the closing brace to the opening brace
          (modify-syntax-entry close (string ?\) open) table))))
    table)
  "Top-level syntax table for raku-mode.")

;; ** Syntax regexps
;;
;; Because the most convenient means of describing and matching
;; complex expressions in emacs is the regexp engine, and raku has
;; plenty of syntax it would stand to reason that we will be needing
;; /lots of regexps/.
;;
;; Fortunately, emacs makes this much more approachable in two ways:
;;   1. [(elisp)Rx notation] allows us to express regular expressions as S-expressions
;;   2. [(elisp)Extending Rx] with custom constructs allows us to reuse building blocks in our regular expressions
;;
;; To make this undertaking even simpler, we're going to two a few
;; things. We'll set up an alist of construct definitions that ~rx.el~
;; can understand. 

(defvar raku--rx-forms-alist ()
  "Alist of custom rx constructs used in raku-mode.")

;; To populate this alist, a macro named `raku--defrx' will be
;; employed. This macro has the convenient side-effect of storing in
;; `raku--rx-forms-alist' the name of our construct and its
;; S-expression definition. In addition to curating the construct
;; alist, this macro also emits S-expressions that will then define a
;; variable containing the stringified form of our regular expression
;; for use where convenient (such as with `looking-at').
;;
;; It should be noted that hacks are sometimes used here, and should
;; be replaced with better macroexpand-based hacks to avoid relying on
;; ~rx.el~ internals.

(defmacro raku--defrx (name docstring &rest regexps)
  "Define both an rx constituent and compiled expression string.
NAME is used to derive the string variable name, e.g.
raku--(name)-rx. NAME is also used to define an rx construct that
is available to defrx. FORM is an rx form that is evaluated with
all previously defined constructs available.
DOCSTRING is passed to defvar"
  (declare (indent 1) (doc-string 2))
  ;; lots of this is stolen from rx.el's internals. why, you might
  ;; ask? well, `rx' is a bad macro and it doesn't work the way i want
  ;; it to. or i'm a bad lisp hacker and don't work the way `rx' wants
  ;; me to.
  (let* ((rx--local-definitions raku--rx-forms-alist)
         (seq-form (cons 'seq regexps))
         (expr (rx--to-expr seq-form)))
    (progn
      ;; overrwrite anything that had this name before us
      (setq raku--rx-forms-alist
            (append
             (assq-delete-all name raku--rx-forms-alist)
             (list (list name seq-form))))
      ;; define a var we can use elsewhere, with the compiled expression
      `(defvar ,(intern (concat "raku--" (symbol-name name) "-rx"))
         ,expr
         ,docstring))))

;; *** Construct definitions
;;
;; **** Atomic constructs
;;
;; These constructs match the most basic building blocks of Raku
;; syntax - identifiers, whitespace, and unspace.

(raku--defrx identifier
  "Raku identifier regexp."
  (and (regex "[_[:alpha:]]")
       (0+ (regex "[_[:alnum:]]"))
       (0+ (any "-'") (regex "[_[:alpha:]]") (0+ (regex "[_[:alnum:]]")))))

(raku--defrx metaoperator
  "Raku meta-operator regexp."
  (or (and (regex "[^[:digit:]@%$]")
           (0+ (regex "[^\[\{\('\"[:space:]]")))
      (and (any "@%$")
           (regex "[^.?^=_[:alpha:]]\[\{\('\"[:space:]]")
           (0+ (regex "[^\[\{\('\"[:space:]]")))))

(raku--defrx ws*
  "Eat some or no whitespace."
  (0+ (any whitespace)))

(raku--defrx ws+
  "Eat a minimum of one whitespace."
  (1+ (any whitespace)))

;; /Aside: What is unspace?/ Unspace, simply put, is whitespace that
;; would otherwise be syntactically invalid were not preceded by an
;; escape character (backslash).

(raku--defrx unspace
  "Match expected unspace."
  "\\" ws+)

(raku--defrx unspace?
  "Match optional unspace."
  (? unspace))

;; **** Sigils and Twigils

(raku--defrx sigil
  "Any permissible sigil in the core language."
  (or "$" "@" "%" "&" "::"))

(raku--defrx twigil
  "Any permissible twigil in the core language."
  (or "*" "^" "!" "."))

;; **** Literals

(raku--defrx literal-version
  "Version literal, e.g. ~v1.2.3~."
  "v" (1+ digit) (0+ "." (or "*" (1+ digit))) (opt "+"))

(raku--defrx literal-number
  "Base-10 number literal."
  (group-n 1
           (or (and (1+ digit)
                    (0+ (and "_" (1+ digit)))
                    (opt "."
                         (1+ digit)
                         (0+ (and "_" (1+ digit)))))
               (and "."
                    (1+ digit)
                    (0+ (and "_" (1+ digit))))))
  (opt (group-n 2 (any "Ee"))
       (group-n 3 (opt "-") (1+ digit) (0+ "_" (1+ digit))))
  (opt (group-n 4 "i")))

(raku--defrx literal-base-number
  "Base-N number literal."
  symbol-start
  (group-n 1 "0")
  (or (and
       (group-n 2 "o")
       (group-n 3 (any "0-7") (0+ (any "0-7_"))))
      (and
       (group-n 2 "b")
       (group-n 3 (any "0-1") (0+ (any "0-1_"))))
      (and
       (group-n 2 "x")
       (group-n 3 (regex "[[:xdigit:]]") (0+ (regex "[[:xdigit:]_]"))))
      (and
       (group-n 2 "d")
       (group-n 3 (regex "[[:digit:]]") (0+ (regex "[[:digit:]_]"))))))

;; **** Literals: The dreaded Q-Lang

;; These can be
;; - delimited by balanced chars (syntax "()")
;; - delimited by the same unbalanced char
;; - delimited by ' or () only if space is placed between Q and ( or '

(raku--defrx q-construct
  "Different kinds of Q-construct."
  ;; Q - literal string
  ;; q - escaping
  ;; qq - interpolated
  ;; qw - word quoting (also: < >)
  ;; qww - word quoting with quote protection
  ;; qqw - word quoting with interpolation
  ;; qqww - word quoting with interpolation and quote protection (also: << >>, « »)
  (or "Q" "qw" "qww" "qq" "qqw" "q"
      "qqww" "qx" "qqx"))

(defvar raku--q-adverb-alist
  `(("x"  . "exec")
    ("w"  . "words")
    ("ww" . "quotewords")
    ("q"  . "single")
    ("qq" . "double")
    ("s"  . "scalar")
    ("a"  . "array")
    ("h"  . "hash")
    ("c"  . "closure")
    ("b"  . "backslash")
    ("to" . "heredoc")
    ("v"  . "val"))
  "Alist associating short and long Q-adverb forms.")

(defmacro raku--q-adverb-expand (adverb)
  "Disambiguate a Q-adverb.
Given an abbreviated ADVERB, this will return the corresponding
long form according to `raku--q-adverb-alist'. If no long form is
associated with ADVERB, then return ADVERB."
  `(or (assq ,adverb raku--q-adverb-alist) adverb))

(raku--defrx q-adverb
  "Different Q-adverbs. May be chained.
This is generated from `raku--q-adverb-alist', which associates
adverbs with their abbreviations. If that alist is updated, the
variable form of this rule ~raku-- ... -rx~ must be
re-declared/updated in order to reflect the changes. Because the
rx-construct form of this rule uses the exec construct, any `rx'
use of the construct that is not part of a precompiled
value (such as this variable) will reflect changes immediately;
However, pre-compiled forms should be preferred where performance
is desirable over flexibility."
  ":" (eval (cons 'or (flatten-list raku--q-adverb-alist))))

(raku--defrx q-expression
  "Match the opening portion of a Q-lang expr.
Q-lang is complicated and requires"
  ;; start with a Q construct
  (group-n 1 q-construct)
  ;; permit unspace for tasteless people
  unspace?
  ;; next, adverbs
  (0+ (group-n 2 q-adverb))
  ;; then, if this is () or '-delimited, we need to expect a space. i
  ;; won't yet bother with making the regexp smart enough to know that
  ;; ' and () REQUIRE a space, just that it is appropriate in that case.
  (or (seq ws+ (group-n 3 (any "'(")))
      ;; any opener or punctuation
      (seq unspace? (group-n 3 (not (syntax ?_))))))

;; **** Pair syntax
;;
;; Pairs in :-form appear frequently in Raku

(raku--defrx pair-bool
  "Boolean pair literal."
  ":" (? (group-n 1 "!")) (1+ identifier))

(raku--defrx pair-int
  "Integer pair literal. Only positive integers."
  ":" (group-n 1 (1+ (any digit))) (1+ identifier))

(raku--defrx pair-copy-variable
  "Pair taking its value from a variable in scope with the same name as the pair.")

(raku--defrx pair-delimited
  "Pair with delimited value."
  ":" )

;; **** Documentation and comment constructs
;;
;; These constructs match portions of documentation and comment
;; constructs. Raku supports an extensive (and somewhat syntactically
;; complex) set of documentation constructs that are stored in the
;; parse tree and are accessible at runtime. This can be thought of as
;; a language-level implementation of `org-babel-tangle'-like
;; functionality, as programs can consist both of highly literate and
;; free-form rich-text documentation and live code simultaneously.

(raku--defrx pod6-directive-term
  "Match POD6 directive terms."
  (or "for" "para" "begin"))

(raku--defrx pod6-type
  "Match POD6 type terms."
  (or (seq (or "head" "item") (1+ (any digit))) ;; special case: headN, itemN
      (seq (any upper) (1+ (any upper digit "-"))) ;; special case: Semantic blocks
      "code" "input" "output" "item" "defn" "table" "comment"))

(raku--defrx pod6-format-char
  "Match POD6 format code types."
  (any "BCEIKLNPRTUVXZ"))

(raku--defrx pod6-format-sequence
  "POD6 formatting sequence opener."
  (group-n 1 pod6-format-char) "<")

(raku--defrx pod6-directive
  "Match a POD6 directive.
For delimited blocks, this matches the opening directive."
  ;; Anatomy of a POD directive
  ;;
  ;; =para head1 :k(v)
  ;;  ---^ ----^ ----^
  ;;  Dir. Type. Params.
  ;;
  ;; Dir. - An optional directive (see pod6-directive)
  ;; These directives give hints to the parser about
  ;; where the POD directive ends.
  ;;
  ;; Type - A mandatory type (see pod6-type)
  ;; This is mandatory. When a directive is not specified,
  ;; the type must be directly adjacent to the leading '=',
  ;; (e.g. =head1). If a directive is not specified, it is
  ;; assumed to be `para`.
  ;;
  ;; Params - Optional parameters for the typesetting
  ;; system.
  line-start ws* "="
  (? (group-n 1 pod6-directive-term) ws+) ;; optional block directive, assume para
  (group-n 2 pod6-type) ;; block type
  (0+ nonl) line-end) ;; whatever else (TODO: keywords)

(raku--defrx pod6-end-directive
  "Complement to `pod6-directive' with a term of \"begin\".
Marks the closing of a block."
  line-start ws* "=" (group-n 1 "end") ws+ (group-n 2 pod6-type) ws* line-end)

(raku--defrx pod6-end-para
  "Ending of a non-delimited, non-abbreviated POD6 line."
  (or
   ;; terminates on a blank line
   (seq line-start ws* line-end)
   ;; or the next POD directive
   pod6-directive))

(raku--defrx comment
  "Match opening portion of comment."
  ;; comments in raku have the following form:
  ;;   #12
  ;; where 1 might be some sort of twigil (`, =, |),
  ;; where 2 might be an opener for a multiline comment
  ;; if 1 is `
  ;;   then expect either whitespace or a opener immediately
  ;;   after. if any other character occurs, this is invalid
  ;;   syntax.
  ;; if 1 is = or |
  ;;   then allow an opener, or a character immediately after.
  ;;   if the character immediately after is an opener, this
  ;;   is a multi-line comment.
  ;;
  ;; we should propertize everything from the # to the end
  ;; as comment text, and as syntax-multiline
  "#"
  (? (group-n 1 (any "|=`")))
  (? (group-n 2 (syntax ?\())))

;; **** Basic keyword constructs
;;
;; These are all things that could be thought of as keywords.
;; Truthfully speaking, many are first-class constructs and therefore
;; are not /technically/ keywords; however, they are used in a similar
;; manner to proper language-level keywords in other languages.

(raku--defrx include
  "Match terms that 'include' other code, such as require."
  (or "use" "require unit"))

(raku--defrx pragma
  "Match pragma terms."
  (or "v6" "v6.c" "v6.d" "v6.d.PREVIEW" ;; todo could probably use version literal
      "MONKEY-GUTS" "MONKEY-SEE-NO-EVAL" "MONKEY-TYPING" "MONKEY"
      "dynamic-scope" "experimental" "fatal" "internals" "invocant"
      "isms" "lib" "newline" "nqp" "parameters" "precompilation"
      "soft" "strict" "trace" "variables" "worries"))

(raku--defrx use-pragma
  "Match the application of a pragma."
  (group-n 1 include)
  ws+
  (group-n 2 pragma))

(raku--defrx pre-declare
  "Match terms that modify declarations."
  (or "multi" "proto" "only"))

(raku--defrx declare
  "Match general declarators."
  (or "macro" "sub" "submethod" "method" "category"
      "module" "class" "role" "package" "enum" "grammar"
      "slang" "subset"))

(raku--defrx rule
  "Matches regexp declarators."
  (or "regex" "rule" "token"))

(raku--defrx scope
  "Match scope modifiers."
  (or "let" "my" "our" "state" "temp" "has" "constant"))

(raku--defrx type-constraint
  "Match type constraints."
  (or "does" "as" "but" "trusts" "of" "returns" "handles" "where"
      "augment" "supersede"))

(raku--defrx type-property
  "Match type properties."
  (or "signature" "context" "also" "shape" "prec" "irs" "ofs" "ors"
      "export" "deep" "binary" "unary" "reparsed" "rw" "parsed"
      "cached" "readonly" "defequiv" "will" "ref" "copy" "inline"
      "tighter" "looser" "equiv" "assoc" "required"))

;; * Flow Control and Lifecycle Rules

(raku--defrx conditional
  "Match conditional terms."
  (or "if" "else" "elsif" "unless" "with"
      "orwith" "without" "given"))

(raku--defrx loop
  "Match loop terms."
  (or "for" "loop" "repeat" "while" "until" "gather"))

(raku--defrx flow-control
  "Match other flow-control terms."
  (or "take" "do" "when" "next" "last" "redo" "return" "contend"
      "maybe" "defer" "start" "default" "exit" "make" "continue"
      "break" "goto" "leave" "async" "lift"))

(raku--defrx exception
  "Match exception-raising terms."
  (or "die" "fail" "try" "warn"))

(raku--defrx phaser
  "Match lifecycle subroutines (phasers)."
  (or "BEGIN" "CHECK" "INIT" "END"
      "DOC BEGIN" "DOC CHECK" "DOC INIT"
      "ENTER" "LEAVE" "KEEP" "UNDO"
      "FIRST" "NEXT" "LAST"
      "PRE" "POST"
      "CATCH" "CONTROL"
      "LAST" "QUIT"
      "COMPOSE" "CLOSE"))

;; * Default operators and other provided constructs of interest

(raku--defrx operator-word
  "Wordy operators."
  (or "div" "xx" "x" "mod" "also" "leg" "cmp" "before" "after" "eq"
      "ne" "le" "lt" "not" "gt" "ge" "eqv" "ff" "fff" "and" "andthen"
      "or" "xor" "orelse" "extra" "lcm" "gcd" "o"))

(raku--defrx operator-symbol
  "Symbolic operators."
  (any "-:+/*~?|=^!%&,<>».;\\∈∉∋∌∩∪≼≽⊂⊃⊄⊅⊆⊇⊈⊉⊍⊎⊖∅∘"))

(raku--defrx operator-reduce
  "Reducing operator."
  (and (0+ (any "RSXZ\["))
       (opt (any "RSXZ&"))
       (1+ "\[")
       (opt "\(")
       metaoperator
       (opt "\)")
       (1+ "\]")))

(raku--defrx operator-rsxz
  "I don't know and I can't find documentation."
  symbol-start
  (any "RSXZ")
  (or
   (or
    ;; maybe need to use `operator'?
    (and operator-word symbol-end)
    (any ".,")
    (1+ (regex "[^:\[.,[:space:][:alnum:]]"))) ;; ^identifier?
   symbol-end))

(raku--defrx operator-hyper
  "Hyper (mapping) operators."
  (or (and "«" metaoperator (char "«»"))
      (and "»" metaoperator (opt (char "«»")))
      (and "<<" metaoperator (or "<<" ">>"))
      (and ">>" metaoperator (opt (or "<<" ">>")))
      (and (regex "[^[:digit:]\[\{\('\",:[:space:]]")
           (0+ (regex "[^\[\{\('\",:[:space:]]"))
           (or "«" "<<"))))

(raku--defrx operator-set
  "Another mystery."
  (opt "R")
  "("
  (or (char "-^.+|&")
      (and (char "<>") (opt (char "=+")))
      "cont"
      "elem")
  ")")

(raku--defrx operator
  "Any operator."
  (or operator-word operator-symbol
      operator-reduce operator-rsxz
      operator-hyper operator-set))

;; * Core types

(raku--defrx type-low
  "Low-level core types."
  (or "int" "int1" "int2" "int4" "int8" "int16" "int32" "int64"
      "rat" "rat1" "rat2" "rat4" "rat8" "rat16" "rat32" "rat64"
      "buf" "buf1" "buf2" "buf4" "buf8" "buf16" "buf32" "buf64"
      "uint" "uint1" "uint2" "uint4" "uint8" "uint16" "uint32"
      "uint64" "utf8" "utf16" "utf32" "bit" "bool" "bag" "set"
      "mix" "num" "complex"))

(raku--defrx type-high
  "High-level core types."
  (or "Object" "Any" "Junction" "Whatever" "Capture" "Match"
      "Signature" "Proxy" "Matcher" "Package" "Module" "Class"
      "Grammar" "Scalar" "Array" "Hash" "KeyHash" "KeySet" "KeyBag"
      "Pair" "List" "Seq" "Range" "Set" "Bag" "BagHash" "Mapping" "Void"
      "Undef" "Failure" "Exception" "Code" "Block" "Routine" "Sub"
      "Macro" "Method" "Submethod" "Regex" "Str" "Blob" "Char" "Map"
      "Byte" "Parcel" "Codepoint" "Grapheme" "StrPos" "StrLen"
      "Version" "Num" "Complex" "Bit" "True" "False" "Order" "Same"
      "Less" "More" "Increasing" "Decreasing" "Ordered" "Callable"
      "AnyChar" "Positional" "Associative" "Ordering" "KeyExtractor"
      "Comparator" "OrderingPair" "IO" "KitchenSink" "Role" "Int" "Bool"
      "Rat" "Buf" "UInt" "Abstraction" "Numeric" "Real" "Nil" "Mu"))

(raku--defrx type
  "Any core type."
  (or type-low type-high))

;; :upside-down-smiley:
;;
;; rx-let-eval sets up rx--local-definitions, but rx-let sticks them
;; in macroexpand-all-environment and expects rx to then set
;; rx--local-definitions from *that*. this makes zero sense to me.
;; hack hack hack
(defmacro raku-rx (&rest sexps)
  "Convert SEXPS to regexp with raku forms available.
See `rx', `rx-let', and `rx-let-eval' for more details."
  (let ((rx--local-definitions raku--rx-forms-alist))
    (rx--to-expr (cons 'seq sexps))))

;; end of rx stuff

;; Propertizer/Lexer/Helper funs

(defmacro raku--looking-at-char (char)
  "Is `following-char' equal to CHAR?"
  `(eq ,char (following-char)))

(defmacro raku--put-props (start end &rest properties)
  "Put each property pair in PROPERTIES into text properties for START to END."
  (declare (indent 2) (pure t))
  `(let ((start ,start)
         (end ,end))
     ,@(mapcar (lambda (pair)
                 `(put-text-property
                   start end
                   ,(car pair) ,(cdr pair))) properties)))

(defmacro raku-put-match-props (sexps)
  "Apply sets of properties to match groups.
For SEXPS having the form (N (P1 . V1) (Pn . Vn) ...), apply all
property pairs (as in `raku--put-props') to the match region for
group N."
  (declare (pure t) (indent 0))
  `(progn
     ,@(mapcar (lambda (group-rules)
                 (let ((group-number (car group-rules))
                       (properties   (cdr group-rules)))
                   `(when (match-beginning ,group-number)
                      ,(macroexpand-all
                        `(raku--put-props (match-beginning ,group-number) (match-end ,group-number)
                           ,@properties))))) sexps)))

;; Comments/POD/Markup

(defun raku-syntax-propertize-pod-text (limit default-face)
  "Propertize a region of text containing pod6 markup, bounded by LIMIT.
Note that this function is aware of where pod text should stop,
so you must set LIMIT wisely. Text not surrounded by a formatting
code has its ~font-lock-face~ property set to DEFAULT-FACE."
  ;; TODO: could very efficiently mark non-control as some other face
  ;; TODO: nested format code support
  (let ((last-plain (point)))
    (while (< (point) limit)
      ;; find the next occurence of *<...
      (if (re-search-forward (raku-rx pod6-format-sequence) limit t)
          ;; the above search such place us after the format leader, e.g. `B<|`
          (let ((text-begin (point)))
            ;; fontify control opener as directive
            (raku-put-match-props
              ((0 ('font-lock-face . 'raku-pod6-directive))))
            ;; performance hack to avoid the need to do two passes: we
            ;; "bookmark" the beginning point of the preceding
            ;; plaintext, and then when the plaintext region
            ;; terminates, we set the desired font face for the region
            ;; spanning from that bookmark to the end of the
            ;; plaintext region.
            (put-text-property last-plain (match-beginning 0) 'font-lock-face default-face)
            ;; step back to the < opening the text
            (goto-char (1- text-begin))
            ;; tell forward-list (great name, I know) to skip to closer
            (forward-list)
            ;; fontify closing brace as directive
            (put-text-property (1- (point)) (point) 'font-lock-face 'raku-pod6-directive)
            ;; update our plaintext bookmark
            (setq last-plain (point))
            ;; forward-list will leave us after the closing >, e.g. `B<text>|` so,
            ;; it would follow that (1- (point)) will be the end of text

            (let* ((text-end (1- (point)))
                   (code     (match-string 1))
                   (face     (pcase code
                               ("B" 'bold)
                               ("I" 'italic)
                               ("U" 'underline)
                               ((or "L" "P") 'link)
                               ("T" 'term)
                               ("X" 'raku-label)
                               ("Z" 'raku-comment)
                               (_   'raku-string))))
              (put-text-property text-begin text-end 'font-lock-face face)))
        ;; no POD markup was found from (point) until LIMIT, so just skip all
        ;; text.
        (goto-char limit)
        ;; mark any remaining plain text
        (put-text-property last-plain (point) 'font-lock-face default-face)))))

(defun raku-syntax-propertize-comment (_limit)
  "Propertize a raku comment.
Should be called looking at the comment.
`match-data' should be match data for `raku--comment-rx'."
  (let ((begin    (match-beginning 0))  ;; begin match
        (twigil   (match-beginning 1))  ;; begin twigil match
        (boundary (match-beginning 2))) ;; begin boundary match
    ;; go to the end of the comment
    (if boundary
        (progn
          ;; step to before boundary (e.g. |<...), and then search till matching
          ;; balanced paren found
          (goto-char boundary)
          (forward-list)
          ;; mark as a multiline construct so that
          ;; propertize-function gets called with correct chunk
          ;; boundaries (see `syntax-propertize-multiline')
          (put-text-property begin (point) 'syntax-multiline t))
      (end-of-line))
    ;; propertize the comment from beginning to point
    (put-text-property begin (point) 'font-lock-face 'raku-comment)
    ;; propertize doc/embed twigil if it's here, also propertize POD
    ;; markup, since = and | imply documentation text (see declarator comments)
    (let ((end (point)))
      (save-excursion
        ;; go to pos after boundary open, twigil, or begin
        (goto-char (1+ (or boundary twigil (1+ begin))))
        ;; and then propertize until either just before boundary close or end
        ;; of comment
        (raku-syntax-propertize-pod-text (if boundary (1- end) end) 'raku-comment)))
    (when twigil
      (put-text-property twigil (1+ twigil) 'font-lock-face 'raku-twigil))))

;; POD6 Parser

(defun raku--find+fontify-pod-closer (type limit)
  "Search up to LIMIT for an end directive for pod6 TYPE.
Return `point' if found before LIMIT."
  ;; we do rx-to-string stuff here since we need the flexibility
  (rx-let-eval raku--rx-forms-alist
    (let ((found  nil)
          ;; assemble an expression matching what we expect our end
          ;; directive to look like
          (end-rx (rx-to-string `(seq ws* line-start "=end" ws+ ,type ws* line-end))))
      ;; we can go ahead and skip a line, as this function is to be
      ;; called facing the opening directive
      (forward-line)
      ;; search forward line-by-line until we match END-RX or hit
      ;; LIMIT
      (while (and (< (point) limit) (not found))
        (if (looking-at end-rx)
            (progn
              ;; if we hit the end statement, fontify it
              (raku-put-match-props
                ((0 ('font-lock-face . 'raku-pod6-default))
                 (1 ('font-lock-face . 'raku-pod6-directive))
                 (2 ('font-lock-face . 'raku-pod6-type))))
              ;; and mark this as our ending position. we don't want
              ;; to mark the point /after/ the directive as the end
              ;; position for the /body/, and we can infer for
              ;; delimited POD regions that the entire region,
              ;; including the closing directive, ends at the end of
              ;; the line /following/ the body.
              (setq found (point)))
          ;; if we didn't match the closer, advance one line
          (forward-line)))
      ;; for reasons of convenience and consistency with incomplete
      ;; match behavior (e.g. LIMIT reached), move `point' to the end
      ;; of the POD region if we located the closing portion of the
      ;; body. we'll give the caller the end of the body proper;
      ;; however moving point like this is considered good manners for
      ;; syntax-propertize so we would have to do it anyways
      (when found
        (goto-char (line-end-position 2)))
      ;; hand caller the body end position
      found)))

(defun raku--pod-find-end (limit)
  "Search up to LIMIT for the end to an abbreviated or para pod6 block.
Must be called on the same line that the block is declared.
Return `point' if found before LIMIT."
  (let ((found nil))
    (save-match-data
      ;; expect to be called on the same line as the opening
      (forward-line)
      (while (and (< (point) limit) (not found))
        (cond
         ;; an empty line or new directive terminates this POD
         ((looking-at (raku-rx pod6-end-para))
          ;; since we're now technically looking at something that isn't the
          ;; same POD, we need to step back to the end of the previous line,
          ;; where the current POD ends.
          (setq found (point)))
         ;; if none of the above, move to start of next line
         (t (forward-line)))
      found))))

(defun raku-syntax-propertize-pod (limit)
  "Propertize a region of plain old documentation until LIMIT.
`match-data' must be populated appropriately for `raku--pod-rx'"
  ;; figure out body geometry and then fontify the body
  (let* ((begin     (match-beginning 0))
         (directive (match-string    1))
         (type-end  (match-end       2))
         (type      (match-string    2))
         (body-begin (pcase directive
                       ;; begin and para have different rules about
                       ;; where body content starts, they stipulate
                       ;; that block contents begin on the line
                       ;; following the directive.
                       ((or "begin" "para") (line-beginning-position 2))
                       ;; anything else can have contents on the same
                       ;; line as the directive
                       (_                   (1+ type-end))))
         (body-end   (save-match-data
                       (or (pcase directive
                             ;; delimited blocks require special
                             ;; handling, as they can be nested and must
                             ;; be terminated by name, similar to the
                             ;; here-document Q-Lang.
                             ("begin" (raku--find+fontify-pod-closer type limit))
                             ;; other blocks follow the same rules about
                             ;; termination, which is that a trailing
                             ;; blank line or POD6 directive terminates
                             ;; the block body
                             (_       (raku--pod-find-end limit)))
                           (point)))))
    ;; (message "rspp limit=%d begin=%d directive=%s type=%s body-beg=%d body-end=%d"
    ;;          limit begin directive type body-begin body-end)
    (message "%S" `(rspp (body-end ,body-end) (body-begin ,body-begin)))
    (message "(rspp-body \"%s\")" (buffer-substring body-begin body-end))
    ;; fontify directive and type (this is down here because it's
    ;; aesthetically pleasing to have single form defun BODYs)
    (raku-put-match-props
      ;; mark the whole directive as comment, first. this macro
      ;; propertizes groups in the order specified, so we can be sure
      ;; this won't take precedence
      ((0 ('font-lock-face . 'raku-pod6-default))
       ;; fontify the directive
       (1 ('font-lock-face . 'raku-pod6-directive))
       ;; fontify the type/NAME
       (2 ('font-lock-face . 'raku-pod6-type))))
    ;; TODO: handle delimited block nesting properly
    ;;
    ;; now, we will fontify the body of the POD directive. we don't
    ;; have to worry about =end .. directives, because
    ;; `raku--find+fontify-pod-closer' will have already seen to it
    ;; for us. we'll also make an excursion to return to the first
    ;; position in the body and fontify it now.
    (save-excursion 
      (message "rspp propertize contents")
      (pcase type
        ;; for code blocks, don't assume a format, just strip any face and
        ;; mark as foreign so we can reason about this later (e.g. for
        ;; indent). also apply this treatment to the DATA metatype, as it is
        ;; conventionally used for inclusion of arbitrary data.
        ((or "code" "DATA") (raku--put-props body-begin body-end
                              ('raku-foreign . t)
                              ('font-lock-face . nil)))
        ;; treat whatever else as text
        (_ (goto-char body-begin)
           ;; this function will take care of fontifying format codes
           ;; and default text.
           (raku-syntax-propertize-pod-text body-end 'raku-pod6-default))))
    ;; lastly, we'll mark the whole thing as multiline syntax so that
    ;; we can get more effective update region boundaries
    (put-text-property begin (point) 'syntax-multiline t)))

;; ** Propertizer function and helper macros
;;
;; This set of macros/funs culminating in `raku-syntax-propertize' and
;; with the assistance of the functions and macros defined in the
;; prior section serve the purpose of efficiently and concisely
;; selecting and propertizing text, either with font faces,
;; ~syntax.el~ hints, or other useful properties.
;;
;; *** Match-propertize-advance macros
;;
;; These macros are intended to DRY up code that follows a pattern
;; commonly used in propertizer functions, which is that of testing
;; for a match following `point', applying properties to regions of
;; the match, and then advancing past the entire match. Additionally,
;; these ensure that expressions are evaluated in the raku rx context
;; that was set up earlier in this file.
;;
;; The more general of the two, `raku--propertize-regexp', allows this
;; pattern to be applied such that any number of properties may be
;; attached to any number of match groups.

(defmacro raku--propertize-regexp (regexp &rest rules)
  "Test for `looking-at' REGEXP, apply RULES using `raku-put-match-props' if non-nil.
Intended for use in `cond'."
  (declare (indent 1))
  `((looking-at ,(macroexpand-all `(raku-rx ,regexp)))
    ,(macroexpand-all `(raku-put-match-props ,rules))
    (goto-char (match-end 0))))

;; The second of the two, `raku--fontify-regexp', applies only the
;; ~font-lock-face~ with the given value to the entire match region,
;; allowing for this common sub-type of the aforementioned pattern to
;; be more concisely expressed.

(defmacro raku--fontify-regexp (regexp face)
  "Like `raku--propertize-regexp', but just for font-lock-face.
Apply FACE to all text matching REGEXP at `point' and then move
to match end. Intended for use in `cond'."
  `((looking-at ,(macroexpand-all `(raku-rx ,regexp)))
    (let ((end (match-end 0)))
      (put-text-property (match-beginning 0) end 'font-lock-face ,face)
      (goto-char end))))

;; *** `cond' hack
;;
;; This macro is a hack that lets us dry up `cond' rules

(defmacro raku--mxcond (&rest conds)
  "Produce a `cond' expression with macroexpanded conditions."
  `(cond ,@(macroexpand-all conds)))

;; *** The syntax-propertize-function
;;
;; `raku-syntax-propertize' analyzes text from the beginning of a
;; given chunk until /at least/ the end of that same chunk. While
;; top-level analysis of the text region will not continue after
;; `point' has advanced past the specified end of the chunk, specific
;; analyses run as part of this function (e.g.
;; `raku-syntax-propertize-comment') may analyze and propertize text
;; past that point in order to simplify the propertization of
;; multi-line or line-consuming constructs (such as POD, comments,
;; QLangs, or SLangs).
;;
;; To accomplish this analysis, `point' is first shifted to the
;; beginning of the region and then text is analyzed progessively.

(defun raku-syntax-propertize (chunk-begin chunk-end)
  "`raku-mode's `syntax-propertize-function' for NQP and Raku.
Propertize text from CHUNK-BEGIN to CHUNK-END within `current-buffer'.

Works by stepping through characters in the region until certain
expressions are matched, at which point those expressions are
propertized/fontified as appropriate."
  (goto-char chunk-begin)
  ;; analysis order:
  ;; 1. comments
  ;; 2. pod
  (while (< (point) chunk-end)
    (let ((start (point)))
      (raku--mxcond
       ;; skip empty lines
       ((eq (point) (line-end-position)) (forward-line))
       ;; comments, multiline comments, and documentation comments
       ((looking-at (raku-rx comment))
        ;; TODO - do we want to modify this so that we can font-lock the twigil?
        (raku-syntax-propertize-comment chunk-end))
       ;; POD, POD code blocks (yikes)
       ((looking-at (raku-rx pod6-directive))
        (raku-syntax-propertize-pod chunk-end))

       ;; Core operators, core "keywords"
       (raku--fontify-regexp operator 'raku-operator)
       (raku--propertize-regexp use-pragma
         (1 ('font-lock-face . 'raku-include))
         (2 ('font-lock-face . 'raku-pragma)))
       (raku--fontify-regexp import 'raku-include)
       
       ;; Flow Control, etc...
       (raku--fontify-regexp conditional  'raku-conditional)
       (raku--fontify-regexp loop         'raku-loop)
       (raku--fontify-regexp flow-control 'raku-flow-control)
       (raku--fontify-regexp exception    'raku-exception)
       
       
       ;; inf. loop guard
       ((looking-at "\\S-")
        (put-text-property (point) (1+ (point)) 'font-lock-face nil)
        (forward-char))
       (t
        (let ((begin (point)))
          (skip-syntax-forward " ")
          ;; TODO: needed?
          (put-text-property begin (point) 'font-lock-face nil))))
      ;;(message "%S" `(raku-syntax-propertize-post (chunk ,(buffer-substring-no-properties start (point)))))
      )))

(provide 'raku-syntax)

;;; raku-syntax.el ends here
