# unravel

Unravel your REPL

[![npm version](https://badge.fury.io/js/unravel-repl.svg)](https://badge.fury.io/js/unravel-repl)

Unravel is a simple command-line client for Clojure REPLs. It is based on the [unrepl](https://github.com/cgrand/unrepl) protocol, so instead of relying on nREPL, unravel communicates with your Clojure process through a Socket Server REPL.

Unravel is readline-enabled and aims to provide a smooth REPL user experience. Features like tab completion are planned for a future version.

## Status

Unravel is functional but should be considered a preview release

## Installation

To use unravel you need Clojure 1.8.0 or above.

On Linux or macOS, can install unravel via npm:

```
sudo npm install -g lumo-cljs unravel-repl
```

This installs the `unravel` binary and adds it to your PATH.

Alternatively, clone the git project and type:

```
scripts/run [--debug] <host> <port>
```

## Usage

Connect to a Clojure [Socket REPL] (https://clojure.org/reference/repl_and_main#_launching_a_socket_server):

```
unravel [--debug] <host> <port>
```

In addition to common readline shortcuts, the following keybindings are available:

- `Control-D`: exit the REPL
- `Control-O`: show docstring for symbol under the cursor

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

Using a pain Clojure jar:

```
java -Dclojure.server.myrepl="{:port 50505 :accept clojure.core.server/repl}" -jar ~/.m2/repository/org/clojure/clojure/1.8.0/clojure-1.8.0.jar
```

You can then connect to the process by running unravel in a separate terminal window:

```
unravel localhost 50505
```

## Future work

Unravel is early stages. See [TODO.md](TODO.md) for a list of planned improvements.

Unravel is developed in conjunction with [unrepl](https://github.com/cgrand/unrepl).

## Changes

### 0.1.2

Distribute via npm

### 0.1.0

Show docstring

## Further Reading

The Ultimate Guide To Clojure REPLs on the [Socket REPL](https://lambdaisland.com/guides/clojure-repls/clojure-repls#orgheadline20)

## Copyright

(c) 2017 Paulus Esterhazy

[Unrepl payload.clj](https://github.com/cgrand/unrepl) (c) 2017 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
