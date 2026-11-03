use thiserror::Error;

pub const RECOVERABLE_ERROR: u8 = 0x01;
pub const CRITICAL_ERROR: u8 = 0x02;

// This is not really dead code, #[wasm_error] macro replaces this by something else.
#[allow(dead_code)]
pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("[Recoverable] {0}")]
    RecoverableError(String),
    #[error("[Critical] {0}")]
    CriticalError(String),
}

impl From<Error> for u8 {
    fn from(error: Error) -> Self {
        match error {
            Error::RecoverableError(_) => RECOVERABLE_ERROR,
            Error::CriticalError(_) => CRITICAL_ERROR,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use macros::wasm_error;

    // A call of WASM that fails on purpose: 1 with a recoverable error, 2 with a panic.
    #[wasm_error]
    extern "C" fn fails_as_asked(kind: u8) -> Result<u32> {
        match kind {
            1 => Err(Error::RecoverableError("on purpose".to_string())),
            2 => panic!("on purpose"),
            _ => Ok(7),
        }
    }

    #[test]
    fn an_error_of_a_call_reaches_its_caller_each_time() {
        assert_eq!(fails_as_asked(0), 7);
        assert_eq!(crate::mem::read_error_code(), 0);
        // twice each: the first one leaves nothing panicking for the next one to abort on
        for _ in 0..2 {
            assert!(std::panic::catch_unwind(|| fails_as_asked(1)).is_err());
            assert_eq!(
                crate::mem::read_error_code(),
                RECOVERABLE_ERROR,
                "recoverable"
            );
            assert!(std::panic::catch_unwind(|| fails_as_asked(2)).is_err());
            assert_eq!(
                crate::mem::read_error_code(),
                CRITICAL_ERROR,
                "a panic is critical"
            );
        }
        assert_eq!(fails_as_asked(0), 7, "and it answers after");
        assert_eq!(crate::mem::read_error_code(), 0);
    }
}
