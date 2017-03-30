# unravel

Unravel your REPL

Unravel is a simple command-line client for Clojure REPLs. It is based on the [unrepl](https://github.com/cgrand/unrepl) protocol.

## Status

Initial WIP version

## Usage

Open a Clojure [socket server](https://clojure.org/reference/repl_and_main#_launching_a_socket_server):

```
java -Dclojure.server.myrepl="{:port 50505 :accept clojure.main/repl}" -jar ~/.m2/repository/org/clojure/clojure/1.9.0-alpha14/clojure-1.9.0-alpha14.jar
```

In another terminal, run

```
git clone https://github.com/pesterhazy/unravel.git
cd unravel
scripts/run localhost 50505
```

## License

(c) 2017 Paulus Esterhazy

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
