addToLibrary({
  wapi_notifyTilesRenderComplete: function wapi_notifyTilesRenderComplete() {
    // The corresponding listener lives on `document` (main thread), so in a
    // worker context we simply skip the dispatch instead of crashing.
    if (typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope) {
      return;
    }
    document.dispatchEvent(new CustomEvent('penpot:wasm:tiles-complete'));
  },

  // The error of a call of WASM (see `#[wasm_error]`), thrown to whoever
  // called it: a JS exception, not a Rust panic, so that Rust is not left
  // panicking and the next error does not abort the module.
  wapi_throwError: function wapi_throwError(ptr, len) {
    throw new Error(UTF8ToString(ptr, len));
  }
});
