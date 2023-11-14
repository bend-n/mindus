use crate::block::Block;

/// Map a 0xRRGGBB color to a block.
pub static COLOR2BLOCK: phf::Map<u32, &Block> = include!(concat!(env!("OUT_DIR"), "/4.rs"));
pub static BLOCK2COLOR: phf::Map<&str, (u8, u8, u8)> = include!(concat!(env!("OUT_DIR"), "/2.rs"));

pub fn block(col: [u8; 3]) -> Option<&'static Block> {
    COLOR2BLOCK.get(&fimg::Pack::pack(&col)).copied()
}

pub fn color(block: &Block) -> Option<(u8, u8, u8)> {
    BLOCK2COLOR.get(block.name()).copied()
}
