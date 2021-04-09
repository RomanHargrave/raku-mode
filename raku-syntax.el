;;; raku-syntax.el --- Raku Mode: syntaxification -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Roman Hargrave <roman@hargrave.sh>
;; Author: Roman Hargrave <roman@hargrave.sh>
;;; Commentary:
;; Raku Syntax Property Table Functions and lex helpers

;;; Code:

;; (nqp) HLL/Grammar.nqp - HLL::Grammar, $brackets
(defconst raku-brackets
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

(defvar raku--rx-forms
  (let ((rx-identifier
         (rx-to-string `(and (regex "[_[:alpha:]]")
                             (0+ (regex "[_[:alnum:]]"))
                             (0+ (any "-'") (regex "[_[:alpha:]]") (0+ (regex "[_[:alnum:]]"))))))
        (rx-metaoperator
         (rx-to-string `(or (and (regex "[^[:digit:]@%$]")
                                 (0+ (regex "[^\[\{\('\"[:space:]]")))
                            (and (any "@%$")
                                 (regex "[^.?^=_[:alpha:]]\[\{\('\"[:space:]]")
                                 (0+ (regex "[^\[\{\('\"[:space:]]")))))))
    ;; rx forms for raku
    ;; these are organized into groups corresponding to their
    ;; function (at least as it makes sense to me)
    `((symbol (form)
              `(and symbol-start ,@(cdr form) symbol-end))
      ;; normal identifier, for things such as variables, subs, etc..
      (identifier ,rx-identifier)
      ;; whitespace stuff
      (ws* ,(rx (0+ (any whitespace))))
      (ws+ ,(rx (1+ (any whitespace))))
      (unspace ,(rx "\\" (1+ (any whitespace))))
      (unspace? ,(rx (? "\\" (1+ (any whitespace)))))
      ;; POD6 stuff
      ;; POD6 block directives (how we will read in the block)
      (pod6-directive
       ,(rx (or "for" "para" "begin")))
      (pod6-type
       ,(rx (or (seq (or "head" "item") (1+ (any digit))) ;; special case: headN, itemN
                (seq (any upper) (1+ (any upper digit))) ; special case: Semantic blocks
                "code" "input" "output" "item" "defn" "table" "comment")))
      (pod6-formatcode
       ,(rx (any "BCEIKLNPRTUVXZ")))
      ;; keywords that imply inclusion of code units
      (include ,(rx (or "use" "require unit")))
      ;; declarator keywords
      ;; things that come before declare
      (pre-declare
       ,(rx (or "multi" "proto" "only")))
      ;; common declarators, plus any custom common declarators
      (declare
       ,(rx (or "macro" "sub" "submethod" "method" "category"
                "module" "class" "role" "package" "enum" "grammar"
                "slang" "subset")))
      ;; regex declarators, ofen used inside grammars
      (rule ,(rx (or "regex" "rule" "token")))
      ;; keywords that indicate declaration scope
      (scope ,(rx (or "let" "my" "our" "state" "temp" "has" "constant")))
      ;; type constraints
      (type-constraint
       ,(rx (or "does" "as" "but" "trusts" "of" "returns" "handles" "where"
                "augment" "supersede")))
      ;; type properties
      (type-property
       ,(rx (or "signature" "context" "also" "shape" "prec" "irs" "ofs" "ors"
                "export" "deep" "binary" "unary" "reparsed" "rw" "parsed"
                "cached" "readonly" "defequiv" "will" "ref" "copy" "inline"
                "tighter" "looser" "equiv" "assoc" "required")))

      ;; flow control keywords
      ;; for conditional expressions
      (conditional ,(rx (or "if" "else" "elsif" "unless" "with"
                            "orwith" "without")))
      ;; for looping constructs
      (loop ,(rx (or "for" "loop" "repeat" "while" "until" "gather" "given")))
      ;; for other flow control constructs
      (flow-control
       ,(rx (or "take" "do" "when" "next" "last" "redo" "return" "contend"
                "maybe" "defer" "start" "default" "exit" "make" "continue"
                "break" "goto" "leave" "async" "lift")))
      ;; for exceptions
      (exception ,(rx (or "die" "fail" "try" "warn")))

      ;; "lower-level" constructs (as if such a thing could exist in raku)
      ;; https://docs.raku.org/language/phasers
      ;; phasers allow hooking program lifecycle events
      (phaser
       ,(rx (or "BEGIN" "CHECK" "INIT" "END"
                "DOC BEGIN" "DOC CHECK" "DOC INIT"
                "ENTER" "LEAVE" "KEEP" "UNDO"
                "FIRST" "NEXT" "LAST"
                "PRE" "POST"
                "CATCH" "CONTROL"
                "LAST" "QUIT"
                "COMPOSE" "CLOSE")))
      ;; https://docs.raku.org/language/pragmas
      ;; this list appears to be things that go after use/no
      (pragma ,(rx (or "v6" "v6.c" "v6.d" "v6.d.PREVIEW"
                       "MONKEY-GUTS" "MONKEY-SEE-NO-EVAL" "MONKEY-TYPING" "MONKEY"
                       "dynamic-scope" "experimental" "fatal" "internals" "invocant"
                       "isms" "lib" "newline" "nqp" "parameters" "precompilation"
                       "soft" "strict" "trace" "variables" "worries")))

      ;; operators
      ;; "wordy" operators
      ;; TODO allow custom
      (operator-word
       ,(rx (or "div" "xx" "x" "mod" "also" "leg" "cmp" "before" "after" "eq"
                "ne" "le" "lt" "not" "gt" "ge" "eqv" "ff" "fff" "and" "andthen"
                "or" "xor" "orelse" "extra" "lcm" "gcd" "o")))
      ;; single-char operators
      ;; TODO allow custom
      (operator-char ,(rx (any "-:+/*~?|=^!%&,<>».;\\∈∉∋∌∩∪≼≽⊂⊃⊄⊅⊆⊇⊈⊉⊍⊎⊖∅∘")))
      ;; reduce operator - really not sure what this is or where it's
      ;; documented. if it is documented, it's under a different
      ;; name.
      (reduce-operator
       ,(rx-to-string
         `(and (0+ (any "RSXZ\["))
               (opt (any "RSXZ&"))
               (1+ "\[")
               (opt "\(")
               (regex ,rx-metaoperator)
               (opt "\)")
               (1+ "\]"))))
      ;; more spooky ops
      (rsxz-operator
       ,(rx
         symbol-start
         (any "RSXZ")
         (or (or (and (or "div" "mod" "gcd" "lcm" "xx" "x" "does" "but" "cmp"
                          "leg" "eq" "ne" "gt" "ge" "lt" "le" "before" "after"
                          "eqv" "min" "max" "not" "so" "andthen" "and" "or"
                          "orelse")
                      symbol-end)
                 (any ".,")
                 (1+ (regex "[^:\[.,[:space:][:alnum:]]")))
             symbol-end)))
      ;; hyper operator - map over a list
      (hyper-operator
       ,(rx-to-string
         `(or (and "«" (regex ,rx-metaoperator) (char "«»"))
              (and "»" (regex ,rx-metaoperator) (opt (char "«»")))
              (and "<<" (regex ,rx-metaoperator) (or "<<" ">>"))
              (and ">>" (regex ,rx-metaoperator) (opt (or "<<" ">>")))
              (and (regex "[^[:digit:]\[\{\('\",:[:space:]]")
                   (0+ (regex "[^\[\{\('\",:[:space:]]"))
                   (or "«" "<<")))))
      ;; operations on sets
      (set-operator
       ,(rx (opt "R")
            "\("
            (or (char "-^.+|&")
                (and (char "<>") (opt (char "=+")))
                "cont"
                "elem")
            "\)"))

      ;; types
      ;; low-level (virtual machine) types, seen in nqp and when
      ;; using NativeCall
      (low-type
       ,(rx (or "int" "int1" "int2" "int4" "int8" "int16" "int32" "int64"
                "rat" "rat1" "rat2" "rat4" "rat8" "rat16" "rat32" "rat64"
                "buf" "buf1" "buf2" "buf4" "buf8" "buf16" "buf32" "buf64"
                "uint" "uint1" "uint2" "uint4" "uint8" "uint16" "uint32"
                "uint64" "utf8" "utf16" "utf32" "bit" "bool" "bag" "set"
                "mix" "num" "complex")))
      ;; language-level types considered to be significant
      ;; TODO: allow custom
      (high-type
       ,(rx (or "Object" "Any" "Junction" "Whatever" "Capture" "Match"
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
                "Rat" "Buf" "UInt" "Abstraction" "Numeric" "Real" "Nil" "Mu")))

      ;; ordinals
      ;; version number (e.g. v6.0)
      (version ,(rx "v" (1+ digit) (0+ "." (or "*" (1+ digit))) (opt "+")))
      ;; base-10 number
      (number
       ,(rx
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
         (opt (group-n 4 "i"))))
      ;; base-n number - group 1 is the leading 0, 2 the base
      ;; indicator, 3 the number.
      (base-number
       ,(rx symbol-start
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
                 (group-n 3 (regex "[[:digit:]]") (0+ (regex "[[:digit:]_]"))))))))))

(rx-let-eval raku--rx-forms
  (defconst raku--comment-rx
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
    (rx-to-string '(seq "#"
                        (? (group-n 1 (any "|=`")))
                        (? (group-n 2 (syntax ?\())))))
  (defconst raku--pod-rx
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
    (rx-to-string '(seq line-start ws* "="
                        (? (group-n 1 pod6-directive) ws+) ;; optional block directive, assume para
                        (group-n 2 pod6-type)))) ;; block type, TODO maybe include label in expr?
  (defconst raku--pod-end-nondelim-rx
    (rx-to-string `(or
                    (seq line-start ws* line-end)
                    (regex ,raku--pod-rx))))
  (defconst raku--pod-format-rx
    (rx-to-string '(seq (group-n 1 pod6-formatcode) "<")))
  (defconst raku--heredoc-rx
    ;; I'm assuming heredoc delims are restricted to syntax class _
    ;; also, heredocs are a special class of q-string it appears
    ;; (which would make sense)
    (rx-to-string '(seq "q" (group-n 1 (? "q"))
                        ":to/" (group-n 2 (syntax ?_)) "/"))))


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

;; :upside-down-smiley:
;;
;; rx-let-eval sets up rx--local-definitions, but rx-let sticks them
;; in macroexpand-all-environment and expects rx to then set
;; rx--local-definitions from *that*. this makes zero sense to me.
;; hack hack hack
(defmacro raku-rx (&rest sexps)
  "Convert SEXPS to regexp with raku forms available.
See `rx', `rx-let', and `rx-let-eval' for more details."
  (let ((rx--local-definitions raku--rx-forms))
    (rx--to-expr (cons 'seq sexps))))

;; end of rx stuff

;; Propertizer/Lexer/Helper funs

(defmacro raku--looking-at-char (char)
  "Is `following-char' equal to CHAR?"
  `(eq ,char (following-char)))

(defun raku--put-props (start end properties)
  "Put each property pair in PROPERTIES into text properties for START to END."
  (while properties
    (let (pair (car properties))
      (setq properties (cdr properties))
      (put-text-property start end (car pair) (cdr pair)))))

(defun raku-put-match-props (&rest sexp)
  "Apply for a sequence of cons cells within SEXP, apply certain properties."
  (while sexp
    (let* ((pair  (car sexp))
           (group (car pair))
           (props (cdr pair)))
      (setq sexp (cdr sexp))
      (raku--put-props
       (match-beginning group)
       (match-end group)
       props))))

(defun raku--find-matching-paren (limit &optional can-escape)
  "From `point', search for balanced parens until LIMIT based on syntax table.
This function will move `point' either to LIMIT, or to the
position /after/ the final paren. `following-char' is used to
determine which paren set will be scanned. If CAN-ESCAPE is
non-nil, \\ will cause the parser to skip over one char."
  ;; read opening char
  (let* ((opener   (following-char))
         (closer   (matching-paren opener))
         (depth    1)
         (found-at nil))
    (when (or (zerop opener) (not closer))
      (error "Call to raku--find-matching paren facing char %d which has no known closer" opener))
    ;; pass first paren
    (forward-char)
    ;; read buffer and count balanced boundary chars
    ;; once we hit CLOSE-CHAR at depth 0, set FOUND-AT
    (while (and (< (point) limit) (> depth 0))
      (cond
       ;; extra advance if we encounter an escape, this way we
       ;; don't analyze whatever comes after
       ((and can-escape (raku--looking-at-char ?\\))
        (forward-char))
       ;; increase depth if we see an opener
       ((raku--looking-at-char opener)
        (setq depth (+ depth 1))
        (forward-char))
       ;; decrease depth if we see a closer
       ((raku--looking-at-char closer)
        (setq depth (- depth 1))
        (when (zerop depth) (setq found-at (point)))
        (forward-char)))
      ;; skip past any non parens/punctuation (we may want to catch escapes)
      (skip-syntax-forward "^.()" limit))
    ;; back up if we leveled out
    (when found-at (goto-char found-at))))

(defun raku-syntax-propertize-comment (end)
  "Propertize a raku comment.
Should be called looking at the comment.
`match-data' should be match data for `raku--comment-rx'.
Will propertize until END or end of comment."
  ;; bad hack to get match data to be consistent
  ;; wil always be redundant when called from `raku-syntax-propertize'
  ;; go to the end of the comment
  (let ((begin    (match-beginning 0))  ;; begin match
        (twigil   (match-beginning 1))  ;; begin twigil match
        (boundary (match-beginning 2))) ;; begin boundary match
    (if boundary
        (progn
          ;; step to before boundary (e.g. |<...), and then search till matching
          ;; balanced paren found
          (goto-char boundary)
          (forward-list)
          ;;(message "bounded text: %s" (buffer-substring-no-properties boundary (point)))
          ;; mark as a multiline construct so that
          ;; propertize-function gets called with correct chunk
          ;; boundaries (see `syntax-propertize-multiline')
          (put-text-property begin (point) 'syntax-multiline t))
      (end-of-line))
    ;; propertize doc/embed twigil if it's here
    (put-text-property twigil (1+ twigil) 'font-lock-face 'raku-twigil)
    ;; propertize the comment from beginning to point
    (put-text-property begin (point) 'font-lock-face 'raku-comment)))

;; POD6 Parsers

(defun raku--pod-find-end-delimited (type limit)
  "Search up to LIMIT for an end directive for pod6 TYPE.
Return non-nil if found before LIMIT."
  (rx-let-eval raku--rx-forms
      (let ((found  nil)
            (end-rx (rx-to-string `(seq ws* line-start "=end" ws+ ,type))))
        (while (and (< (point) limit) (not found))
          (if (looking-at end-rx)
              (progn
                (end-of-line)
                (setq found (point)))
            (forward-line)))
        found)))

(defun raku--pod-find-end (limit)
  "Search up to LIMIT for the end to an abbreviated or para pod6 block."
  (let ((found nil))
    (save-match-data
      (while (and (< (point) limit) (not found))
        (cond
         ;; an empty line or new directive terminates this POD
         ((looking-at raku--pod-end-nondelim-rx)
          ;; since we're now technically looking at something that isn't the
          ;; same POD, we need to step back to the end of the previous line,
          ;; where the current POD ends.
          (forward-char -1)
          (setq found (point)))
         ;; if none of the above, move to start of next line
         (t (forward-line))))
      found)))

(defun raku-syntax-propertize-pod-text (limit)
  "Propertize a region of text support pod6 control chars, bounded by LIMIT.
Note that this function is aware of where pod text should stop,
so you must set LIMIT wisely."
  ;; TODO: nested format code support
  (while (< (point) limit)
    ;; find the next occurence of *<...
    (when (re-search-forward raku--pod-format-rx limit t)
      ;; the above search such place us after the format leader, e.g. `B<|`
      (let ((text-begin (point)))
        ;; step back to the < opening the text
        (goto-char (1- text-begin))
        ;; tell forward-list (great name, I know) to skip to closer
        (forward-list)
        ;; find-matching-paren will leave us after the closing >, e.g.
        ;; `B<text>|` so, it would follow that (1- (point)) will be the end of
        ;; text
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
          (put-text-property text-begin text-end 'font-lock-face face))))))

(defun raku-syntax-propertize-pod (match-data limit)
  "Propertize a region of plain old documentation until LIMIT.
MATCH-DATA should contain integer match boundaries corresponding
to `raku--pod-rx' as per `match-data'."
  (let* ((begin     (nth 0 match-data))
         (dir-beg   (nth 2 match-data))
         (dir-end   (nth 3 match-data))
         (directive (if dir-beg
                        (buffer-substring dir-beg dir-end)
                      nil))
         (typ-beg   (nth 4 match-data))
         (typ-end   (nth 5 match-data))
         (type      (buffer-substring typ-beg typ-end))
         (abbrev    (null directive))
         ;; search the end point and report it, also moves `point'
         ;; will be nil if POD does not terminate
         (found-end  (pcase directive
                       ("begin" (raku--pod-find-end-delimited type limit))
                       (_       (raku--pod-find-end limit)))))
    ;; point is now at end of POD block (incl closing directive if applicable)
    ;; propertize POD text depending on type
    (save-excursion
      ;; if this is abbreviated, text will start after typ-end
      (if abbrev
          (goto-char (1+ typ-end))
        ;; otherwise, text will start on the line after the directive
        (progn
          ;; to get to the next line, seek back to the beginning of the pod
          ;; directive
          (goto-char begin)
          ;; and then go to the next line
          (forward-line)))
      ;; now we are facing the start of the text. we will want to propertize up
      ;; to either text-end or limit.
      (let ((text-end (or found-end limit)))
        (pcase type
          ;; for code blocks, don't assume a format, just strip any face and
          ;; mark as foreign so we can reason about this later (e.g. for
          ;; indent). also apply this treatment to the DATA metatype, as it is
          ;; conventionally used for inclusion of arbitrary data.
          ((or "code" "DATA")
           (raku--put-props (point) text-end `((raku-foreign ,t)
                                               (font-lock-face ,nil))))
          ;; treat whatever else as text
          (_
           (raku-syntax-propertize-pod-text text-end)))))
    (put-text-property begin (point) 'font-lock-face 'raku-comment)
    ;; propertize directive
    (if dir-beg
        (put-text-property dir-beg dir-end 'font-lock-face 'raku-declare))
    ;; propertize type
    (put-text-property typ-beg typ-end 'font-lock-face 'raku-type)))

(defun raku-syntax-propertize (chunk-begin chunk-end)
  "`raku-mode's `syntax-propertize-function' for NQP and Raku.
Propertize text from CHUNK-BEGIN to CHUNK-END within `current-buffer'.

Works by stepping through characters in the region until certain
expressions are matched, at which point those expressions are
propertized/fontified as appropriate."
  (goto-char chunk-begin)
  (setq inhibit-redisplay nil)
  ;; analysis order:
  ;; 1. comments
  ;; 2. pod
  (rx-let-eval raku--rx-forms
    (while (< (point) chunk-end)
      (let ((start (point)))
        (cond
         ;; comments, multiline comments, and documentation comments
         ((looking-at raku--comment-rx)
          ;; TODO - do we want to modify this so that we can font-lock the twigil?
          (raku-syntax-propertize-comment chunk-end))
         ;; POD, POD code blocks (yikes)
         ((looking-at raku--pod-rx)
          (raku-syntax-propertize-pod (match-data t) chunk-end))
         ;; inf. loop guard
         ((looking-at "\\S-") (forward-char))
         (t
          (let ((begin (point)))
            (skip-syntax-forward " ")
            (put-text-property begin (point) 'font-lock-face nil))))
        ;;(message "%S" `(raku-syntax-propertize-post (chunk ,(buffer-substring-no-properties start (point)))))
        ))))

(provide 'raku-syntax)

;;; raku-syntax.el ends here
