# Test Backpressure

This example can be used to test how different RPC server transports respond to
backpressure, namely `Async_rpc` over TCP, vs `Async_rpc` over websockets.

The example here demonstrates a bug in the `Pipe_transport` module, which is
used in the websocket RPC implementation.  The easiest symptom of the bug to
observe is that `Pipe.write` returns a `Deferred` that completes immediately
rather than completing when the data is actually written.

## Building 

Use this in your jbuild start file to build all the correct targets.
```
(alias ${ROOT}/lib/incr_dom/DEFAULT)
(alias ${ROOT}/lib/incr_dom/javascript-DEFAULT)
```

## Running

Start the server with this command: 

```bash
./server/main.exe server ./web_client/main.bc.js
```

Now, you can test the servers behavior with the native client by running

```bash
./native_client/main.exe run
```

and pointing your browser to the url that the server is hosting.

## The repro

When running the native client, eventually, the server will encounter pushback 
and will stop sending messages.  However, when using the web client, the server 
will keep publishing until a queue somewhere explodes.  This issue is still 
under investigation.
