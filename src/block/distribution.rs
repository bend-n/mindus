//! conveyors ( & ducts )
use crate::block::simple::*;
use crate::block::*;
use crate::content;
use crate::data::autotile::tile;
use crate::data::dynamic::DynType;
use crate::item;

make_simple!(
    ConveyorBlock,
    |_, _, name, _, ctx: Option<&RenderingContext>, rot: Rotation| {
        let ctx = ctx.unwrap(); // we set want_context to true
        Some(tile(ctx, "distribution", "conveyors", name, rot))
    },
    |_, _, _, buff: &mut DataRead| {
        // format:
        // - amount: `i32`
        // - iterate amount:
        //  - val: `i32`
        //  - id = (((val >> 24) as u8) & 0xff) as u16
        //  - x = (val >> 16) as u8) as f32 / 127.0
        //  - y = ((val >> 8) as u8 as f32 + 128.0) / 255.0
        let amount = buff.read_i32()?;
        for _ in 0..amount {
            buff.skip(4)?;
        }
        Ok(())
    },
    true
);

make_simple!(
    DuctBlock,
    |_, _, name, _, ctx: Option<&RenderingContext>, rot| {
        let ctx = ctx.unwrap();
        Some(tile(ctx, "distribution", "ducts", name, rot))
    },
    |_, _, _, buff: &mut DataRead| {
        // format:
        // - rec_dir: `i8`
        buff.skip(1)
    },
    true
);

make_simple!(
    JunctionBlock,
    |_, _, _, _, _, _| None,
    |_, _, _, buff: &mut DataRead| { read_directional_item_buffer(buff) },
    false
);

make_simple!(SimpleDuctBlock, |_, _, name, _, _, rot: Rotation| {
    let mut base = load("distribution/ducts", "duct-base").unwrap().clone();
    let mut top = load("distribution/ducts", name).unwrap().clone();
    top.rotate(rot.rotated(false).count());
    base.overlay(&top, 0, 0);
    Some(ImageHolder::from(base))
});

fn draw_stack(
    _: &StackConveyor,
    _: &str,
    name: &str,
    _: Option<&State>,
    ctx: Option<&RenderingContext>,
    rot: Rotation,
) -> Option<ImageHolder> {
    let ctx = ctx.unwrap();
    let mask = mask(ctx, rot, name);
    // clone to not hold lock
    let edge = load("distribution/stack-conveyors", &format!("{name}-edge"))
        .unwrap()
        .clone();
    let edgify = |skip, to: &mut RgbaImage| {
        for i in 0..4 {
            if i == skip {
                continue;
            }
            let mut edge = edge.clone();
            edge.rotate(i);
            to.overlay(&edge, 0, 0);
        }
    };
    let gimme = |n: u8| {
        load("distribution/stack-conveyors", &format!("{name}-{n}"))
            .unwrap()
            .clone()
    };
    let empty = ctx.cross[rot.count() as usize].map_or(true, |(v, _)| v.name != name);
    // mindustry says fuck this and just draws the arrow convs in schems but im better than that
    Some(ImageHolder::from(
        if rot.mirrored(true, true).mask() == mask && empty && name != "surge-conveyor" {
            // end
            let mut base = gimme(2);
            edgify(rot.mirrored(true, true).rotated(false).count(), &mut base);
            base
        } else if mask == B0000 && empty {
            // single
            let mut base = gimme(0);
            base.rotate(rot.rotated(false).count());
            edgify(5, &mut base);
            base
        } else if mask == B0000 {
            // input
            let mut base = gimme(1);
            edgify(rot.rotated(false).count(), &mut base);
            base
        } else {
            // directional
            let mut base = gimme(0);
            let going = rot.rotated(false).count();
            base.rotate(going);
            for [r, i] in [[3, 0b1000], [0, 0b0100], [1, 0b0010], [2, 0b0001]] {
                if (mask.into_u8() & i) == 0 && (going != r || empty) {
                    let mut edge = edge.clone();
                    edge.rotate(r);
                    base.overlay(&edge, 0, 0);
                }
            }
            base
        },
    ))
}

make_simple!(
    StackConveyor,
    draw_stack,
    // format:
    // - link: `i32`
    // - cooldown: `f32`
    |_, _, _, buff: &mut DataRead| { buff.skip(8) },
    true
);
make_simple!(ControlBlock);

make_register! {
    "conveyor" => ConveyorBlock::new(1, false, cost!(Copper: 1));
    "titanium-conveyor" => ConveyorBlock::new(1, false, cost!(Copper: 1, Lead: 1, Titanium: 1));
    "plastanium-conveyor" => StackConveyor::new(1, false, cost!(Graphite: 1, Silicon: 1, Plastanium: 1));
    "armored-conveyor" => ConveyorBlock::new(1, false, cost!(Metaglass: 1, Thorium: 1, Plastanium: 1));
    "junction" => JunctionBlock::new(1, true, cost!(Copper: 2));
    "bridge-conveyor" => BridgeBlock::new(1, false, cost!(Copper: 6, Lead: 6), 4, true);
    "phase-conveyor" => BridgeBlock::new(1, false, cost!(Lead: 10, Graphite: 10, Silicon: 7, PhaseFabric: 5), 12, true);
    "sorter" => ItemBlock::new(1, true, cost!(Copper: 2, Lead: 2));
    "inverted-sorter" => ItemBlock::new(1, true, cost!(Copper: 2, Lead: 2));
    "router" => ControlBlock::new(1, true, cost!(Copper: 3));
    "distributor" => ControlBlock::new(2, true, cost!(Copper: 4, Lead: 4));
    "overflow-gate" => ControlBlock::new(1, true, cost!(Copper: 4, Lead: 2));
    "underflow-gate" => ControlBlock::new(1, true, cost!(Copper: 4, Lead: 2));
    "mass-driver" => BridgeBlock::new(3, true, cost!(Lead: 125, Titanium: 125, Thorium: 50, Silicon: 75), 55, false);
    "duct" => DuctBlock::new(1, false, cost!(Beryllium: 1));
    "armored-duct" => DuctBlock::new(1, false, cost!(Beryllium: 2, Tungsten: 1));
    "duct-router" => ItemBlock::new(1, true, cost!(Beryllium: 10));
    "overflow-duct" => SimpleDuctBlock::new(1, true, cost!(Graphite: 8, Beryllium: 8));
    "underflow-duct" => SimpleDuctBlock::new(1, true, cost!(Graphite: 8, Beryllium: 8));
    "duct-bridge" => BridgeBlock::new(1, true, cost!(Beryllium: 20), 3, true);
    "duct-unloader" => ItemBlock::new(1, true, cost!(Graphite: 20, Silicon: 20, Tungsten: 10));
    "surge-conveyor" => StackConveyor::new(1, false, cost!(SurgeAlloy: 1, Tungsten: 1));
    "surge-router" => ControlBlock::new(1, false, cost!(SurgeAlloy: 5, Tungsten: 1)); // not symmetric
    "unit-cargo-loader" => ControlBlock::new(3, true, cost!(Silicon: 80, SurgeAlloy: 50, Oxide: 20));
    "unit-cargo-unload-point" => ItemBlock::new(2, true, cost!(Silicon: 60, Tungsten: 60));
    // sandbox only
    "item-source" => ItemBlock::new(1, true, &[]);
    "item-void" => ControlBlock::new(1, true, &[]);
}

pub struct ItemBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl ItemBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> Self {
        assert!(size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub Option<item::Type>);
}

impl BlockLogic for ItemBlock {
    impl_block!();

    fn data_from_i32(&self, config: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        if config < 0 || config > i32::from(u16::MAX) {
            return Err(DataConvertError::Custom(Box::new(ItemConvertError(config))));
        }
        Ok(DynData::Content(content::Type::Item, config as u16))
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(None))),
            DynData::Content(content::Type::Item, id) => Ok(Some(Self::create_state(Some(
                ItemDeserializeError::forward(item::Type::try_from(id))?,
            )))),
            DynData::Content(have, ..) => Err(DeserializeError::Custom(Box::new(
                ItemDeserializeError::ContentType(have),
            ))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Content,
            }),
        }
    }

    fn clone_state(&self, state: &State) -> State {
        let state = Self::get_state(state);
        Box::new(Self::create_state(*state))
    }

    fn mirror_state(&self, _: &mut State, _: bool, _: bool) {}

    fn rotate_state(&self, _: &mut State, _: bool) {}

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        match Self::get_state(state) {
            None => Ok(DynData::Empty),
            Some(item) => Ok(DynData::Content(content::Type::Item, (*item).into())),
        }
    }

    fn draw(
        &self,
        _: &str,
        name: &str,
        state: Option<&State>,
        _: Option<&RenderingContext>,
        rot: Rotation,
    ) -> Option<ImageHolder> {
        let mut p = load(
            match name {
                "unloader" => "storage",
                "duct-router" | "duct-unloader" => "distribution/ducts",
                _ => "distribution",
            },
            name,
        )
        .unwrap()
        .clone();
        if let Some(state) = state {
            if let Some(s) = Self::get_state(state) {
                let mut top = load(
                    match name {
                        "unloader" => "storage",
                        _ => "distribution",
                    },
                    match name {
                        "unit-cargo-unload-point" => "unit-cargo-unload-point-top",
                        _ => "center",
                    },
                )
                .unwrap()
                .clone();
                p.overlay(top.tint(s.color()), 0, 0);
                return Some(ImageHolder::from(p));
            }
        }
        if matches!(name, "unloader" | "unit-cargo-unload-point") {
            return Some(ImageHolder::from(p));
        }
        if matches!(name, "duct-unloader" | "duct-router") {
            let mut null = load("distribution/ducts", "top").unwrap().to_owned();
            null.rotate(rot.rotated(false).count());
            if name == "duct-unloader" {
                let mut top = load("distribution/ducts", "duct-unloader-top")
                    .unwrap()
                    .to_owned();
                // this rotate call could be omitted if rotation == Right to save a clone
                top.rotate(rot.rotated(false).count());
                null.overlay(&top, 0, 0);
            }
            p.overlay(&null, 0, 0);
            Some(ImageHolder::from(p))
        } else {
            let mut null = load("distribution", "cross-full").unwrap().clone();
            null.overlay(&p, 0, 0);
            Some(ImageHolder::from(null))
        }
    }

    /// format:
    /// (sorter | unloader | duct router | item source)
    /// - item: `i16` as item
    /// (duct-unloader/directional):
    /// - tmp: `i16`
    /// - if tmp != -1: item = tmp as item
    /// - offset: `u16`
    /// (unit-cargo-unload-point)
    /// - item: `u16` as item
    /// - stale: `bool`
    fn read(
        &self,
        b: &mut Build,
        _: &BlockRegistry,
        _: &EntityMapping,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        match b.block.name() {
            "duct-unloader" => {
                let n = buff.read_i16()?;
                if n != -1 {
                    b.state = Some(Self::create_state(item::Type::try_from(n as u16).ok()));
                }
            }
            "unit-cargo-unload-point" => {
                b.state = Some(Self::create_state(
                    item::Type::try_from(buff.read_u16()?).ok(),
                ));
                buff.skip(1)?;
            }
            _ => {
                b.state = Some(Self::create_state(
                    item::Type::try_from(buff.read_u16()?).ok(),
                ));
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("invalid config ({0}) for item")]
pub struct ItemConvertError(pub i32);

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ItemDeserializeError {
    #[error("expected Item but got {0:?}")]
    ContentType(content::Type),
    #[error("target item not found")]
    NotFound(#[from] item::TryFromU16Error),
}

impl ItemDeserializeError {
    pub fn forward<T, E: Into<Self>>(result: Result<T, E>) -> Result<T, DeserializeError> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(DeserializeError::Custom(Box::new(e.into()))),
        }
    }
}

pub struct BridgeBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
    range: u16,
    ortho: bool,
}

type Point2 = (i32, i32);

impl BridgeBlock {
    #[must_use]
    pub const fn new(
        size: u8,
        symmetric: bool,
        build_cost: BuildCost,
        range: u16,
        ortho: bool,
    ) -> Self {
        assert!(size != 0, "invalid size");
        assert!(range != 0, "invalid range");
        Self {
            size,
            symmetric,
            build_cost,
            range,
            ortho,
        }
    }

    state_impl!(pub Option<Point2>);
}

impl BlockLogic for BridgeBlock {
    impl_block!();

    fn data_from_i32(&self, config: i32, pos: GridPos) -> Result<DynData, DataConvertError> {
        let (x, y) = ((config >> 16) as i16, config as i16);
        if x < 0 || y < 0 {
            return Err(DataConvertError::Custom(Box::new(BridgeConvertError {
                x,
                y,
            })));
        }
        let dx = i32::from(x) - pos.0 as i32;
        let dy = i32::from(y) - pos.1 as i32;
        Ok(DynData::Point2(dx, dy))
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(None))),
            DynData::Point2(dx, dy) => {
                if self.ortho {
                    // the game uses (-worldX, -worldY) to indicate no target
                    // likely because the absolute target being (0, 0) means it's unlinked
                    if dx != 0 && dy != 0 {
                        return Ok(Some(Self::create_state(None)));
                    }
                    if dx > i32::from(self.range) || dx < -i32::from(self.range) {
                        return Ok(Some(Self::create_state(None)));
                    }
                }
                // can't check range otherwise, it depends on the target's size
                Ok(Some(Self::create_state(Some((dx, dy)))))
            }
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Point2,
            }),
        }
    }

    fn clone_state(&self, state: &State) -> State {
        let state = Self::get_state(state);
        Box::new(Self::create_state(*state))
    }

    fn mirror_state(&self, state: &mut State, horizontally: bool, vertically: bool) {
        match Self::get_state_mut(state) {
            None => (),
            Some((dx, dy)) => {
                if horizontally {
                    *dx = -*dx;
                }
                if vertically {
                    *dy = -*dy;
                }
            }
        }
    }

    fn rotate_state(&self, state: &mut State, clockwise: bool) {
        match Self::get_state_mut(state) {
            None => (),
            Some((dx, dy)) => {
                let (cdx, cdy) = (*dx, *dy);
                *dx = if clockwise { cdy } else { -cdy };
                *dy = if clockwise { -cdx } else { cdx };
            }
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        match Self::get_state(state) {
            None => Ok(DynData::Empty),
            Some((dx, dy)) => Ok(DynData::Point2(*dx, *dy)),
        }
    }

    /// format:
    /// (item bridge)
    /// - become [`read_buffered_item_bridge`]
    /// (buffered brige)
    /// - become [`read_item_buffer`]
    /// (mass driver) (9b)
    /// - link: `i32`
    /// - rotation: `f32`
    /// - state: `i8`
    fn read(
        &self,
        t: &mut Build,
        _: &super::BlockRegistry,
        _: &crate::data::map::EntityMapping,
        buff: &mut crate::data::DataRead,
    ) -> Result<(), crate::data::ReadError> {
        match t.block.name() {
            "bridge-conveyor" => read_buffered_item_bridge(buff)?,
            "phase-conveyor" | "phase-conduit" | "bridge-conduit" => read_item_bridge(buff)?,
            "mass-driver" => buff.skip(9)?,
            // no state?
            "duct-bridge" | "reinforced-bridge-conduit" => {}
            _ => unreachable!(), // surely no forget
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("invalid coordinates ({x}, {y}) for bridge")]
pub struct BridgeConvertError {
    pub x: i16,
    pub y: i16,
}

/// format;
/// - call [`read_item_bridge`]
/// - become [`read_item_buffer`]
fn read_buffered_item_bridge(buff: &mut DataRead) -> Result<(), DataReadError> {
    read_item_bridge(buff)?;
    read_item_buffer(buff)
}

/// format:
/// - index: `u8`
/// - iter `u8`
///     l: `i64`
fn read_item_buffer(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(1)?;
    let n = buff.read_u8()? as usize;
    buff.skip(n * 8)
}

/// format:
/// - link: `i32`
/// - warmup: `f32`
/// - iterate `u8`
///     - incoming: `i32`
/// - moved: `bool`
fn read_item_bridge(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(8)?;
    let n = buff.read_u8()? as usize;
    buff.skip((n * 4) + 1)
}

/// format:
/// - iterate 4
///     - u8
///     - iterate u8
///         - i64
fn read_directional_item_buffer(buff: &mut DataRead) -> Result<(), DataReadError> {
    for _ in 0..4 {
        let _ = buff.read_u8()?;
        let n = buff.read_u8()? as usize;
        buff.skip(n * 8)?;
    }
    Ok(())
}
