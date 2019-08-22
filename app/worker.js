// from https://github.com/cpitclaudel/z3.wasm
// MIT licensed
function makeWorker(self, console, queries, responses, performance) {
    var INPUT_FNAME = "input.smt2";

    var solver;
    var ready = false;

    function postMessage(kind, payload) {
        self.postMessage({ kind: kind, payload: payload });
    }

    function progress(message) {
        postMessage(responses.PROGRESS, message);
    }

    function runSolver(input, args) {
        if (!ready) {
            console.error("Cannot run SMT solver yet.");
            postMessage(responses.VERIFICATION_COMPLETE, false);
            return;
        }

        args.push(INPUT_FNAME);
        progress("SolverRunning");
        solver.FS.writeFile(INPUT_FNAME, input, { encoding: "utf8" });
        solver.callMain(args);
        postMessage(responses.VERIFICATION_COMPLETE, true);
        progress("SolverReady");
    }

    function onRuntimeInitialized() {
        ready = true;
        progress("SolverReady");
    }

    var STUB_MSG = "Calling stub instead of signal()"

    function postOutput(channel, output) {
        output = output.replace(STUB_MSG, "");
        if (output != "") {
            postMessage(channel, output);
        }
    }

    function loadSolver() {
        progress("SolverDownloading");
        self.importScripts("z3w.js");
        progress("SolverInitializing");
        solver = Z3({ ENVIRONMENT: "WORKER",
                      onRuntimeInitialized: onRuntimeInitialized,
                      print: function(message) { postOutput(responses.STDOUT, message); },
                      printErr: function(message) { postOutput(responses.STDERR, message); } });
    }

    function onMessage(event) {
        var kind = event.data.kind;
        var payload = event.data.payload;
        switch (kind) {
        case queries.VERIFY:
            runSolver(payload.input, payload.args);
            break;
        }
    }

    function init() {
        loadSolver();
        self.onmessage = onMessage;
    }

    return { init: init };
}

importScripts("protocol.js");
makeWorker(self, console, queries, responses, performance).init();
