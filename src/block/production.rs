//! the industry part of mindustry
use crate::block::simple::*;
use crate::block::*;
use crate::data::DataRead;

// format: call [`read_production_block`], seed: [`i32`]
make_simple!(SeparatorBlock => |_, buff: &mut DataRead| buff.skip(12));

make_simple!(
    ProductionBlock,
    |_, _, _, _, r: Rotation, s| {
        // electrolyzer exclusive
        // ozone <- e(^) -> hydrogen
        let mut base = load!("electrolyzer", s);
        unsafe {
            base.overlay(
                load!(s -> match r {
                    Rotation::Up | Rotation::Left => "electrolyzer-hydrogen-output1"
                    Rotation::Down | Rotation::Right => "electrolyzer-hydrogen-output2"
                })
                .rotate(r.count()),
            );
            base.overlay(
                load!(s -> match r {
                    Rotation::Down | Rotation::Right => "electrolyzer-ozone-output1"
                    Rotation::Up | Rotation::Left => "electrolyzer-ozone-output2"
                })
                .rotate(r.mirrored(true, true).count()),
            );
        }
        base
    },
    |b: &mut Build<'_>, buff: &mut DataRead| {
        // format:
        // - progress: `f32`
        // - warmup: `f32`
        // (cultivator)
        // `f32`
        buff.skip(8 + if b.name() == "cultivator" { 4 } else { 0 })
    }
);

make_simple!(
    HeatCrafter,
    |_, n, _, _, r: Rotation, s| {
        let mut base = load!(from n which is ["phase-heater" | "electric-heater" | "oxidation-chamber" | "slag-heater" | "heat-source"], s);
        unsafe {
            base.overlay(match r {
            Rotation::Up | Rotation::Right => load!(concat "top1" => n which is ["phase-heater" | "electric-heater" | "oxidation-chamber" | "slag-heater" | "heat-source"], s),
            Rotation::Down | Rotation::Left => load!(concat "top2" => n which is ["phase-heater" | "electric-heater" | "oxidation-chamber" | "slag-heater" | "heat-source"], s)
        }.rotate(r.rotated(false).count()))
        };
        base
    },
    |_, buff: &mut DataRead| {
        // format:
        // - progress: `f32`
        // - warmup: `f32`
        // - heat: f32
        buff.skip(12)?;
        Ok(())
    }
);
make_simple!(HeatConduit, |_, n, _, _, r: Rotation, s| {
    let mut base = load!(from n which is ["heat-router" | "heat-redirector"], s);
    unsafe {
        base.overlay(
            match r {
                Rotation::Up | Rotation::Right => {
                    load!(concat "top1" => n which is ["heat-router" | "heat-redirector"], s)
                }
                Rotation::Down | Rotation::Left => {
                    load!(concat "top2" => n which is ["heat-router" | "heat-redirector"], s)
                }
            }
            .rotate(r.rotated(false).count()),
        )
    };
    base
});
