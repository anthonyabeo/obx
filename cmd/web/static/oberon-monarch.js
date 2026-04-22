// Oberon+ Monarch grammar and language configuration
// Exposes window.registerOberonLanguage(monaco)
(function(){
  function toCaseVariants(list) {
    const out = [];
    for (const s of list) { out.push(s); out.push(s.toLowerCase()); }
    return out;
  }

  function register(monaco) {
    if (!monaco || !monaco.languages) return;

    const baseKeywords = [
      'ARRAY','BEGIN','BY','CASE','CONST','DEFINITION','DIV','DO','ELSE','ELSIF','END','EXIT','FALSE','FOR','IF','IMPORT','IN','IS','LOOP','MOD','MODULE','NIL','OF','OR','POINTER','PROC','PROCEDURE','RECORD','REPEAT','RETURN','THEN','TO','TRUE','TYPE','UNTIL','VAR','WHILE','WITH'
    ];

    const basePredeclared = [
      'ABS','ANYREC','ASH','ASR','ASSERT','BITAND','BITNOT','BITOR','BITS','BITSHL','BITSHR','BITXOR','BOOLEAN','BYTE','BYTES','CAST','CAP','CHAR','CHR','COPY','DEC','DEFAULT','ENTIER','EXCL','FLOOR','FLT','HALT','INC','INCL','INT8','INT16','INT32','INT64','INTEGER','LEN','LONG','LONGINT','LONGREAL','LSL','MAX','MIN','NEW','NUMBER','ODD','ORD','PACK','PCALL','RAISE','REAL','ROR','SET','SHORT','SHORTINT','SIZE','UNPK','WCHR'
    ];

    const keywords = toCaseVariants(baseKeywords);
    const predeclared = toCaseVariants(basePredeclared);

    monaco.languages.register({ id: 'oberon' });

    monaco.languages.setMonarchTokensProvider('oberon', {
      defaultToken: '',
      tokenPostfix: '.oberon',
      keywords: keywords,
      predeclared: predeclared,
      operators: [':=', ':', '=', '+', '-', '*', '/', '<', '>', '<=', '>=', '<>' ],
      brackets: [ ['{','}','delimiter.curly'], ['[',']','delimiter.bracket'], ['(',')','delimiter.parenthesis'] ],
      symbols: /[=><:+\-*/#^|~]+/,
      tokenizer: {
        root: [
          { include: '@whitespace' },
          [/[A-Za-z_][A-Za-z0-9_]*/, {
            cases: {
              '@predeclared': 'type',
              '@keywords': 'keyword',
              '@default': 'identifier'
            }
          }],
          // numbers: real, hex-integer (trailing H/h), decimal integer (optional suffix L/l/I/i)
          [/\d+\.\d*(?:[eEdDsS][+-]?\d+)?/, 'number.float'],
          [/[0-9A-Fa-f]+[Hh][LlIi]?/, 'number.hex'],
          [/\d+[LlIi]?/, 'number'],
          [/"([^"\\]|\\.)*"/, 'string'],
          [/'([^'\\]|\\.)*'/, 'string'],
          [/:=|\.\.|<=|>=|<>/, 'operator'],
          [/[+\-*/#^=<>:.,;|~]/, 'operator'],
          [/[()\[\]{}]/, 'delimiter']
        ],
        whitespace: [
          [/[ \t\r\n]+/, 'white'],
          [/\(\*/, 'comment', '@comment']
        ],
        comment: [
          [/[^*(]+/, 'comment'],
          [/\*\)/, 'comment', '@pop'],
          [/./, 'comment']
        ]
      }
    });

    // language configuration with improved onEnterRules and completion provider
    monaco.languages.setLanguageConfiguration('oberon', {
      comments: { blockComment: ['(*','*)'] },
      brackets: [['(',')'], ['[',']'], ['{','}']],
      autoClosingPairs: [ { open: '(', close: ')' }, { open: '[', close: ']' }, { open: '{', close: '}' }, { open: '"', close: '"' }, { open: '\'', close: '\'' } ],
      surroundingPairs: [ ['(',')'], ['[',']'], ['{','}'], ['"','"'], ['\'','\''] ],
      indentationRules: {
        increaseIndentPattern: /\b(BEGIN|THEN|DO|REPEAT|MODULE|PROCEDURE|FUNCTION|RECORD|CASE|OF)\b/i,
        decreaseIndentPattern: /\b(END|ELSE|ELSIF|UNTIL)\b/i
      },
      onEnterRules: [
        // IF ... THEN -> indent inside the THEN block
        {
          beforeText: /^.*\bIF\b.*\bTHEN\b.*$/i,
          action: { indentAction: monaco.languages.IndentAction.Indent }
        },
        // ELSIF / ELSE should outdent to match IF block
        {
          beforeText: /^\s*\b(ELSIF|ELSE)\b.*$/i,
          action: { indentAction: monaco.languages.IndentAction.Outdent }
        },
        // CASE / OF -> indent for options
        {
          beforeText: /^.*\bCASE\b.*$/i,
          action: { indentAction: monaco.languages.IndentAction.Indent }
        },
        {
          beforeText: /^.*\bOF\b.*$/i,
          action: { indentAction: monaco.languages.IndentAction.Indent }
        },
        // Outdent before END, UNTIL
        {
          beforeText: /^\s*\b(END|UNTIL)\b.*$/i,
          action: { indentAction: monaco.languages.IndentAction.Outdent }
        }
      ]
    });

    // register basic completion provider for predeclared identifiers
    try {
      monaco.languages.registerCompletionItemProvider('oberon', {
        provideCompletionItems: function(model, position) {
          const suggestions = predeclared.map((word) => ({
            label: word,
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: word,
            detail: 'predeclared',
          }));
          return { suggestions };
        }
      });
    } catch (e) {
      // ignore completion registration errors
    }
  }

  window.registerOberonLanguage = register;
})();

