# unravel

Unravel your REPL

Unravel is a simple command-line client for Clojure REPLs. It is based on the [unrepl](https://github.com/cgrand/unrepl) protocol.

## Status

Work in progress

## Usage

Open a Clojure socket server configured without a prompt. For testing you can use `scripts/server`, which starts a no-prompt socket server listening on port 50505.

In another terminal, run

```
scripts/run localhost <socket-server-port>
```

## License

(c) 2017 Paulus Esterhazy

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
