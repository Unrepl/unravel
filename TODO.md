## Features

- [x] show clojure.repl/doc on current form on shortcut
- [x] welcome message (connected to ...)
- [x] release bundle
- [x] improve docs
- [x] add npm version badge
- [x] add `--version` cli switch
- [x] show current namespace
- [x] handle empty lines
- [x] Tab completion
- [x] use defrecord for tagged values, i.e. ratios, classes etc
- [x] persistent history
- [x] pretty-printing exceptions
- [x] clear on C-C
- [x] update payload
- [x] lazy seq elisions
- [x] restore namespace prompts
- [ ] handle map ellisons
- [ ] refactor to a map?
- [ ] use separate tooling connection to avoid reprompts
- [ ] M-x
- [ ] add screenshot
- [ ] add dumb terminal mode (`-d`)
- [ ] reverse-search in history
- [ ] bump lumo version
- [ ] multi-line input
- [ ] interrupt eval on Control-C
- [ ] highlighting parens (a la lumo)
- [ ] tests
- [ ] gensym unrepl namespace
- [ ] C-C twice to quit
- [ ] print arglist automatically as you're typing a symbol
- [ ] apropos, macroexpand, show-arglist
- [ ] s/_refreshLine/prompt(true)/
- [ ] emojis?
- [ ] 256 colors?

## Bugs

- clear doc lines
- connect before opening readline
- fix completion
- restore unrepl code
- unrepl PR
- Completion timeout even if successful
- read phase exceptions -> unresponsive
