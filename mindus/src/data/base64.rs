use base64::{engine::general_purpose, engine::Engine as _};
pub use base64::{DecodeSliceError as DecodeError, EncodeSliceError as EncodeError};

const BASE64: general_purpose::GeneralPurpose = general_purpose::STANDARD;

pub fn encode(input: &[u8], output: &mut [u8]) -> Result<usize, EncodeError> {
    BASE64.encode_slice(input, output)
}

pub fn decode(input: &[u8], output: &mut [u8]) -> Result<usize, DecodeError> {
    BASE64.decode_slice(input, output)
}
