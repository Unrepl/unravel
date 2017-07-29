# unravel

Unravel your REPL

[![npm version](https://badge.fury.io/js/unravel-repl.svg)](https://badge.fury.io/js/unravel-repl)

Unravel is a simple command-line client for Clojure REPLs. It is based on the [unrepl](https://github.com/cgrand/unrepl) protocol, so instead of relying on nREPL, unravel communicates with your Clojure process through a Socket Server REPL.

Unravel provides a richer user experience by:

- showing docstring and arglist of vars as you type them
- eliding long or infinite sequences but allowing the user to request a continuation

## Installation

To use unravel you need Clojure 1.8.0 or above.

### Simple installation using npm

On Linux or macOS you can install (or update) unravel via npm:

```
sudo npm install -g unravel-repl
```

This installs the `unravel` binary and adds it to your PATH.

### Installation from git

For a more up-to-date version, you can check out master:

```
git clone https://github.com/pesterhazy/unravel.git
cd unravel
npm install
scripts/run [--debug] <host> <port>
```

## Usage

Connect to a Clojure [Socket REPL](https://clojure.org/reference/repl_and_main#_launching_a_socket_server):

```
unravel [--debug] <host> <port>
```

In addition to common readline shortcuts, the following keybindings are available:

- `Control-D`: exit the REPL
- `Control-O`: show docstring for symbol under the cursor
- `<Tab>`: complete var or namespace

Lines starting with `#__` are treated as special commands and interpreted by the REPL client. The following specials are available:

- `#__help` shows a help screen
- `#__1`, `#__2`, `#__3` ...: expand the numberd lazy seq ellipsis
- `#__`: expand the most recent lazy seq ellipsis

## Launching a Socket REPL

You can easily enable the Socket REPL feature for your Clojure project, whether you use boot, lein or a plain clojure jar. The Socket REPL does not have any dependencies other than Clojure 1.8.0.

To launch a Clojure process with a Socket REPL listening on port 50505 using boot, use:

```
boot -i "(do (require 'clojure.core.server) (clojure.core.server/start-server {:port 50505 :name :repl :accept 'clojure.core.server/repl}))" wait
```

Using Leiningen:

```
JVM_OPTS='-Dclojure.server.myrepl={:port,50505,:accept,clojure.core.server/repl}' lein repl
```

Using a plain Clojure jar:

```
java -Dclojure.server.myrepl="{:port 50505,:accept,clojure.core.server/repl}" -jar ~/.m2/repository/org/clojure/clojure/1.8.0/clojure-1.8.0.jar
```

You can then connect to the process by running unravel in a separate terminal window:

```
unravel localhost 50505
```

## Changes

### 0.2.1

- Enable lumo caching (3x startup time improvement)
- Interrupt running process on SIGINT
- Fix exceptions when connected to Boot-based socket server

### 0.2.0

- Live docs for vars and namespaces
- Use separate tooling connection for tab completion and docs
- Start in "user" namespace

### 0.1.6

- Fix installation via NPM

### 0.1.5

- Add repl specials
- Expand lazy seq ellipsis
- Fix issue with non-existant namespaces

### 0.1.4

- Tab completion (namespaces and vars)
- Pretty-print exceptions
- Use tagged literals
- Re-prompt on newline or Control-C

### 0.1.3

Persistent history

### 0.1.2

Distribute via npm

### 0.1.0

Show docstring

## Future work

See [TODO.md](TODO.md) for a list of planned improvements. Unravel is developed in conjunction with [unrepl](https://github.com/cgrand/unrepl).

## Resources

- The Ultimate Guide To Clojure REPLs on the [Socket REPL](https://lambdaisland.com/guides/clojure-repls/clojure-repls#orgheadline20)

- [replicant](https://github.com/puredanger/replicant): proof of concept of using Socket REPL for tooling

Join the `#unrepl` channel in the [Clojurians slack](http://clojurians.net/)!

## Copyright

(c) 2017 Paulus Esterhazy

[Unrepl payload.clj](https://github.com/cgrand/unrepl) (c) 2017 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
