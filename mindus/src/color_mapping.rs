/// Map a 0xRRGGBB color to a block.
// pub static COLOR2BLOCK: phf::Map<u32, &Block> = include!(concat!(env!("OUT_DIR"), "/4.rs"));
pub static BLOCK2COLOR: [[u8; 3]; u16::MAX as usize + 1] = include!("color_mapped");
