#[macro_export]
macro_rules! notify_tiles_render_complete {
    () => {{
        #[cfg(target_arch = "wasm32")]
        unsafe extern "C" {
            pub fn wapi_notifyTilesRenderComplete();
        }

        #[cfg(target_arch = "wasm32")]
        unsafe {
            wapi_notifyTilesRenderComplete()
        };
    }};
}

pub use notify_tiles_render_complete;

/// Throws `message` to the caller of the WASM call that runs, as a JS
/// exception (see `#[wasm_error]`). A Rust panic cannot leave an
/// `extern "C"` function (the module aborts), and one that leaves through
/// `extern "C-unwind"` is never caught by Rust, which then aborts on the
/// next panic as one within a panic. Natively (tests), a panic.
pub fn throw_error(message: &str) -> ! {
    #[cfg(target_arch = "wasm32")]
    {
        unsafe extern "C-unwind" {
            fn wapi_throwError(ptr: *const u8, len: usize) -> !;
        }
        unsafe { wapi_throwError(message.as_ptr(), message.len()) }
    }
    #[cfg(not(target_arch = "wasm32"))]
    panic!("{message}")
}
