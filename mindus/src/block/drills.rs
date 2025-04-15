//! extraction of raw resources (mine part)
use crate::block::simple::make_simple;
use crate::block::*;

make_simple!(
    DrillBlock,
    |_, name, _, _, rot: Rotation, s| {
        let mut base =
            load!(from name which is ["large-plasma-bore" | "plasma-bore" | "cliff-crusher"], s);
        let mut top = load!(concat "top" => name which is ["large-plasma-bore" | "plasma-bore" | "cliff-crusher"], s);
        unsafe { top.rotate(rot.rotated(false).count()) };
        unsafe { base.overlay(&top) };
        base
    },
    |_, buff: &mut DataRead| read_drill(buff)
);
make_simple!(WallDrillBlock, |_, name, _, _, rot: Rotation, scl| {
    let mut base = load!(from name which is ["cliff-crusher" | "large-cliff-crusher"], scl);
    let mut top =
        load!(concat "top" => name which is ["cliff-crusher" | "large-cliff-crusher"], scl);
    unsafe { top.rotate(rot.rotated(false).count()) };
    unsafe { base.overlay(&top) };
    base
});

/// format:
/// - progress: [`f32`]
/// - warmup: [`f32`]
fn read_drill(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(8)
}
