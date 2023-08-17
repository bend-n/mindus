//! extraction of raw resources (mine part)
use crate::block::simple::make_simple;
use crate::block::*;

make_simple!(
    DrillBlock,
    |_, name, _, _, rot: Rotation, s| {
        let mut base =
            load!(from name which is ["large-plasma-bore" | "plasma-bore" | "cliff-crusher"], s);
        unsafe {
            base.overlay(load!(concat "top" => name which is ["large-plasma-bore" | "plasma-bore" | "cliff-crusher"], s).rotate(rot.rotated(false).count()) )
        };
        base
    },
    |_, buff: &mut DataRead| read_drill(buff)
);
make_simple!(WallDrillBlock, |_, _, _, _, rot: Rotation, scl| {
    let mut base = load!("cliff-crusher", scl);
    unsafe { base.overlay(load!("cliff-crusher-top", scl).rotate(rot.rotated(false).count())) };
    base
});

/// format:
/// - progress: [`f32`]
/// - warmup: [`f32`]
fn read_drill(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(8)
}
