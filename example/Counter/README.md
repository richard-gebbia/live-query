# live-query Counter example

A counter that ticks up every second. The server holds and updates the counter's data model.  The client simply subscribes it and is automatically updated when the server updates itself.

To build and run the server (from this directory):
```
cd Server
npm install
elm make Main.elm --output elm.js
node server.js
```

To build and run the client on macOS (from this directory):
```
cd Client
elm make Main.elm
<open command> index.html
```
where `<open command>` is `open` on macOS or `start` on Windows.