//! conveyors ( & ducts )
use crate::block::simple::*;
use crate::block::*;
use crate::content;
use crate::data::autotile::tile;
use crate::data::dynamic::DynType;
use crate::item;

make_simple!(
    ConveyorBlock,
    |_, name, _, ctx: Option<&RenderingContext>, rot, s| tile(ctx.unwrap(), name, rot, s),
    |_, buff: &mut DataRead| {
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
    }
);

make_simple!(
    DuctBlock,
    |_, name, _, ctx: Option<&RenderingContext>, rot, s| tile(ctx.unwrap(), name, rot, s),
    |_, buff: &mut DataRead| {
        // format:
        // - rec_dir: `i8`
        buff.skip(1)
    }
);

make_simple!(JunctionBlock => |_, buff| { read_directional_item_buffer(buff) });
make_simple!(SimpleDuctBlock, |_, name, _, _, rot: Rotation, s| {
    let mut base = load!("duct-base", s);
    let mut top = load!(from name which is ["overflow-duct" "underflow-duct"], s);
    // SAFETY: any load!() is square
    unsafe { top.rotate(rot.rotated(false).count()) };
    // SAFETY: same size
    unsafe { base.overlay(&top) };
    base
});

fn draw_stack(
    _: &StackConveyor,
    name: &str,
    _: Option<&State>,
    ctx: Option<&RenderingContext>,
    rot: Rotation,
    s: Scale,
) -> ImageHolder<4> {
    let ctx = ctx.unwrap();
    let mask = mask(ctx, rot, name);
    #[rustfmt::skip]
    let edge = |n: u8| {
        match n {
            0 => load!(concat "edge-0" => name which is ["surge-conveyor" | "plastanium-conveyor"], s),
            1 => load!(concat "edge-1" => name which is ["surge-conveyor" | "plastanium-conveyor"], s),
            2 => load!(concat "edge-2" => name which is ["surge-conveyor" | "plastanium-conveyor"], s),
            _ => load!(concat "edge-3" => name which is ["surge-conveyor" | "plastanium-conveyor"], s)
        }
    };
    let edgify = |skip, to: &mut ImageHolder<4>| {
        for i in 0..4 {
            if i == skip {
                continue;
            }
            unsafe { to.overlay(&edge(i)) };
        }
    };
    let gimme = |n: u8| match n {
        0 => load!(concat 0 => name which is ["surge-conveyor" | "plastanium-conveyor"], s),
        1 => load!(concat 1 => name which is ["surge-conveyor" | "plastanium-conveyor"], s),
        _ => load!("plastanium-conveyor-2", s),
    };
    let empty = ctx.cross[rot.count() as usize].map_or(true, |(v, _)| v.name != name);
    // mindustry says fuck this and just draws the arrow convs in schems but im better than that
    if rot.mirrored(true, true).mask() == mask && empty && name != "surge-conveyor" {
        // end
        let mut base = gimme(2);
        edgify(rot.mirrored(true, true).rotated(false).count(), &mut base);
        base
    } else if mask == B0000 && empty {
        // single
        let mut base = gimme(0);
        unsafe { base.rotate(rot.rotated(false).count()) };
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
        unsafe { base.rotate(going) };
        for [r, i] in [[3, 0b1000], [0, 0b0100], [1, 0b0010], [2, 0b0001]] {
            if (mask.into_u8() & i) == 0 && (going != r || empty) {
                unsafe { base.overlay(&edge(r)) };
            }
        }
        base
    }
}

make_simple!(
    StackConveyor,
    draw_stack,
    // format:
    // - link: `i32`
    // - cooldown: `f32`
    |_, buff: &mut DataRead| buff.skip(8)
);
make_simple!(
    SurgeRouter,
    |_, _, _, _, r: Rotation, s| {
        let mut base = load!("surge-router", s);
        let mut top = load!("top", s);
        unsafe { top.rotate(r.rotated(false).count()) };
        unsafe { base.overlay(&top) };
        base
    },
    |_, buff: &mut DataRead| buff.skip(2)
);
// format: id: [`i32`]
make_simple!(UnitCargoLoader => |_, buff: &mut DataRead| buff.skip(4));

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

    fn mirror_state(&self, _: &mut State, _: bool, _: bool) {}

    fn rotate_state(&self, _: &mut State, _: bool) {}

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(Self::get_state(state).map_or(DynData::Empty, |item| {
            DynData::Content(content::Type::Item, item.into())
        }))
    }

    fn draw(
        &self,
        name: &str,
        state: Option<&State>,
        _: Option<&RenderingContext>,
        rot: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        let mut p = load!(from name which is ["sorter" | "inverted-sorter" | "duct-router" | "duct-unloader" | "unit-cargo-unload-point" | "unloader" | "item-source"], s);
        if let Some(state) = state
            && let Some(item) = Self::get_state(state)
        {
            let mut top = load!(s -> match name {
                "unit-cargo-unload-point" => "unit-cargo-unload-point-top",
                "unloader" => "unloader-center",
                _ => "center",
            });
            unsafe { p.overlay(top.tint(item.color())) };
            if name == "duct-unloader" {
                unsafe {
                    p.overlay(&load!("duct-unloader-arrows", s).rotate(rot.rotated(false).count()));
                }
            }
            return p;
        }
        if matches!(name, "duct-router" | "duct-unloader") {
            let mut top = load!(s -> match name {
                "duct-router" => "top",
                "duct-unloader" => "duct-unloader-top",
            });
            unsafe { top.rotate(rot.rotated(false).count()) };
            unsafe { p.overlay(&top) };
        }
        p
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
    fn read(&self, b: &mut Build, buff: &mut DataRead) -> Result<(), DataReadError> {
        match b.block.name() {
            "duct-unloader" => {
                let n = buff.read_i16()?;
                if n != -1 {
                    b.state = Some(Self::create_state(item::Type::try_from(n as u16).ok()));
                }
                buff.skip(2)?;
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
            None => Ok(DynData::Point2(-1, -1)),
            Some((dx, dy)) => Ok(DynData::Point2(*dx, *dy)),
        }
    }

    /// format:
    /// (item bridge)
    /// - become [`read_buffered_item_bridge`]
    /// (buffered brige)
    /// - become [`read_item_buffer`]
    /// (mass driver) (9b)
    /// - link: [`i32`]
    /// - rotation: [`f32`]
    /// - state: [`i8`]
    /// (payload mass driver) (19b)
    /// - call [`read_payload_block`](crate::block::payload::read_payload_block)
    /// - link: [`i32`]
    /// - rotation: [`f32`]
    /// - state: [`i8`]
    /// - reload: [`f32`]
    /// - charge: [`f32`]
    /// - loaded: [`bool`]
    /// - charging: [`bool`]
    fn read(
        &self,
        t: &mut Build,
        buff: &mut crate::data::DataRead,
    ) -> Result<(), crate::data::ReadError> {
        match t.block.name() {
            "bridge-conveyor" => read_buffered_item_bridge(buff)?,
            "phase-conveyor" | "phase-conduit" | "bridge-conduit" => read_item_bridge(buff)?,
            "mass-driver" => buff.skip(9)?,
            "payload-mass-driver" | "large-payload-mass-driver" => {
                crate::block::payload::read_payload_block(buff)?;
                buff.skip(19)?;
            }
            // no state?
            "duct-bridge" | "reinforced-bridge-conduit" => {}
            n => unreachable!("{n}"), // surely no forget
        }
        Ok(())
    }

    fn draw(
        &self,
        name: &str,
        _: Option<&State>,
        _: Option<&RenderingContext>,
        r: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        match name {
            "mass-driver" => {
                let mut base = load!("mass-driver-base", s);
                unsafe { base.overlay(&load!("mass-driver", s)) };
                base
            }
            "duct-bridge" | "reinforced-bridge-conduit" => {
                let mut base =
                    load!(from name which is ["duct-bridge" | "reinforced-bridge-conduit"], s);
                let mut arrow = load!(
                    s -> match name {
                        "duct-bridge" => "duct-bridge-dir",
                        _ => "reinforced-bridge-conduit-dir",
                    }
                );
                unsafe { arrow.rotate(r.rotated(false).count()) };
                unsafe { base.overlay(&arrow) };
                base
            }
            // "bridge-conveyor" | "phase-conveyor" | "bridge-conduit" | "phase-conduit" | "payload-mass-driver" | "large-payload-mass-driver"
            _ => unreachable!(),
        }
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
