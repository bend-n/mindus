//! conveyors ( & ducts )
use std::borrow::BorrowMut;

use crate::block::simple::*;
use crate::block::*;
use crate::content;
use crate::data::dynamic::DynType;
use crate::item;
use bobbin_bits::U4;
use image::imageops::{flip_horizontal_in_place as flip_h, flip_vertical_in_place as flip_v};
#[cfg(test)]
macro_rules! dir {
    (^) => {
        crate::block::Rotation::Up
    };
    (v) => {
        crate::block::Rotation::Down
    };
    (<) => {
        crate::block::Rotation::Left
    };
    (>) => {
        crate::block::Rotation::Right
    };
}
#[cfg(test)]
macro_rules! conv {
    (_) => {
        None
    };
    ($dir:tt) => {
        Some((
            &crate::block::distribution::CONVEYOR,
            crate::block::distribution::dir!($dir),
        ))
    };
}
#[cfg(test)]
macro_rules! define {
    ($a:tt,$b:tt,$c:tt,$d:tt) => {
        [
            crate::block::distribution::conv!($a),
            crate::block::distribution::conv!($b),
            crate::block::distribution::conv!($c),
            crate::block::distribution::conv!($d),
        ]
    };
}
#[cfg(test)]
pub(crate) use conv;
#[cfg(test)]
pub(crate) use define;
#[cfg(test)]
pub(crate) use dir;

#[test]
fn test_mask() {
    macro_rules! assert {
        ($a:tt,$b:tt,$c:tt,$d:tt => $rot: tt => $expect: expr) => {
            assert_eq!(mask!(define!($a, $b, $c, $d), $rot), $expect)
        };
    }
    macro_rules! mask {
        ($cross:expr, $rot: tt) => {
            mask(
                &RenderingContext {
                    position: PositionContext {
                        position: GridPos(5, 5),
                        width: 10,
                        height: 10,
                    },
                    cross: $cross,
                    rotation: dir!($rot),
                },
                "conveyor",
            )
        };
    }
    assert!(_,_,_,_ => ^ => U4::B0000);
    assert!(v,_,_,_ => > => U4::B1000);
    assert!(v,v,_,_ => v => U4::B1000);
    assert!(_,v,>,_ => > => U4::B0000);
    assert!(v,>,<,> => ^ => U4::B0001);
    assert!(v,>,>,_ => > => U4::B1000);
}

fn mask(ctx: &RenderingContext, n: &str) -> U4 {
    macro_rules! c {
        ($in: expr, $srot: expr, $name: expr, $at: expr) => {{
            if let Some((b, rot)) = $in {
                if b.name() == $name {
                    // if they go down, we must not go up
                    (rot == $at && rot.mirrored(true, true) != $srot) as u8
                } else {
                    0
                }
            } else {
                0
            }
        }};
    }
    use Rotation::*;
    let mut x = 0b0000;

    // println!("{:?}, {ctx}", ctx.cross);
    x |= 8 * c!(ctx.cross[0], ctx.rotation, n, Down);
    x |= 4 * c!(ctx.cross[1], ctx.rotation, n, Left);
    x |= 2 * c!(ctx.cross[2], ctx.rotation, n, Up);
    x |= c!(ctx.cross[3], ctx.rotation, n, Right);
    U4::from(x)
}

fn tile(ctx: &RenderingContext<'_>, name: &str, rot: Rotation) -> ImageHolder {
    mask2tile(mask(ctx, name), rot, name)
}

const FLIP_X: u8 = 1;
const FLIP_Y: u8 = 2;

/// TODO figure out if a flip is cheaper than a rotate_270
fn mask2tile(mask: U4, rot: Rotation, name: &str) -> ImageHolder {
    use U4::*;
    // let lo = |index: u8| {
    //     load("distribution/conveyors", &format!("{name}-{index}"))
    //         .unwrap()
    //         .value()
    // };
    // r == 5 => flip_v + r - 1
    macro_rules! p {
        ($image:literal, $rotation:literal) => {
            ($image, $rotation, None)
        };
        ($image:literal, $rotation:literal, $flipping:expr) => {
            ($image, $rotation, Some($flipping))
        };
    }

    let (index, r, flip) = match mask {
        // from left
        B0001 => match rot {
            Rotation::Down => p!(1, 1, FLIP_Y), // ┐
            Rotation::Right => p!(0, 0),        // ─
            Rotation::Up => p!(1, 3),           // ┘
            _ => unreachable!(),
        },
        // from below
        B0010 => match rot {
            Rotation::Left => p!(1, 2),  // ┐
            Rotation::Right => p!(1, 1), // ┌
            Rotation::Up => p!(0, 3),    // │
            _ => unreachable!(),
        },
        // from bottom + left
        B0011 => match rot {
            Rotation::Right => p!(2, 0),               // ┬
            Rotation::Up => p!(2, 3, FLIP_Y | FLIP_X), // ┤
            _ => unreachable!(),
        },
        // from right
        B0100 => match rot {
            Rotation::Left => p!(0, 2),       // ─
            Rotation::Down => p!(1, 1),       // ┌
            Rotation::Up => p!(1, 1, FLIP_X), // └
            _ => unreachable!(),
        },
        // from sides
        B0101 => match rot {
            Rotation::Up => p!(4, 3),   // ┴
            Rotation::Down => p!(4, 1), // ┬
            _ => unreachable!(),
        },
        // from right + down
        B0110 => match rot {
            Rotation::Up => p!(2, 3),           // ├,
            Rotation::Left => p!(2, 0, FLIP_X), // ┬
            _ => unreachable!(),
        },
        // from right + down + left
        B0111 => match rot {
            Rotation::Up => p!(3, 3), // ┼
            _ => unreachable!(),
        },
        // from above
        B1000 => match rot {
            Rotation::Down => p!(0, 1),         // │
            Rotation::Left => p!(1, 0, FLIP_X), // ┘
            Rotation::Right => p!(1, 0),        // └
            _ => unreachable!(),
        },
        // from top and left
        B1001 => match rot {
            Rotation::Right => p!(2, 0, FLIP_Y), // ┴
            Rotation::Down => p!(2, 1),          // ┤
            _ => unreachable!(),
        },
        // from top sides
        B1010 => match rot {
            Rotation::Right => p!(4, 0), // ├
            Rotation::Left => p!(4, 3),  // ┤
            _ => unreachable!(),
        },
        // from top, left, bottom
        B1011 => match rot {
            Rotation::Right => p!(3, 0), // ┼
            _ => unreachable!(),
        },
        // from top and right
        B1100 => match rot {
            Rotation::Down => p!(2, 3, FLIP_X), // ├
            Rotation::Left => p!(2, 2),         // ┴
            _ => unreachable!(),
        },
        // from top, left, right
        B1101 => match rot {
            Rotation::Down => p!(3, 1), // ┼
            _ => unreachable!(),
        },
        // from top, right, bottom
        B1110 => match rot {
            Rotation::Left => p!(3, 0, FLIP_X), // ┼
            _ => unreachable!(),
        },
        B0000 => (
            0,
            match rot {
                Rotation::Left => 2,
                Rotation::Right => 0,
                Rotation::Down => 1,
                Rotation::Up => 3,
            },
            None,
        ),
        // B0000 => (0, wrap(rot.count() as i8 - 1, 0, 3) as u8, None),
        B1111 => unreachable!(),
    };
    let mut p = ImageHolder::from(load("distribution/conveyors", &format!("{name}-{index}")));
    if let Some(op) = flip {
        let re: &mut RgbaImage = p.borrow_mut();
        if (op & FLIP_X) != 0 {
            flip_h(re);
        }
        if (op & FLIP_Y) != 0 {
            flip_v(re);
        }
    }
    if r == 0 {
        return p;
    }
    let mut p = p.own();
    p.rotate(r);
    ImageHolder::from(p)
}

make_simple!(
    ConveyorBlock,
    |_, _, name, _, ctx: Option<&RenderingContext>| {
        if let Some(ctx) = ctx {
            return Some(tile(ctx, name, ctx.rotation));
        }
        None
    },
    true
);
make_simple!(
    JunctionBlock,
    |_, _, _, _, _| None,
    |_, _, _, _, _, buff: &mut crate::data::DataRead| {
        // format:
        // - iterate 4
        //     - u8
        //     - iterate u8
        //         - i64
        for _ in 0..4 {
            let _ = buff.read_u8()?;
            let n = buff.read_u8()? as usize;
            buff.skip(n * 8)?;
        }
        Ok(())
    },
    false
);

make_simple!(ControlBlock);

make_register! {
    "conveyor" => ConveyorBlock::new(1, false, cost!(Copper: 1));
    "titanium-conveyor" => ConveyorBlock::new(1, false, cost!(Copper: 1, Lead: 1, Titanium: 1));
    "plastanium-conveyor" => ControlBlock::new(1, false, cost!(Graphite: 1, Silicon: 1, Plastanium: 1));
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
    "duct" => ConveyorBlock::new(1, false, cost!(Beryllium: 1));
    "armored-duct" => ConveyorBlock::new(1, false, cost!(Beryllium: 2, Tungsten: 1));
    "duct-router" => ItemBlock::new(1, true, cost!(Beryllium: 10));
    "overflow-duct" => ControlBlock::new(1, true, cost!(Graphite: 8, Beryllium: 8));
    "underflow-duct" => ControlBlock::new(1, true, cost!(Graphite: 8, Beryllium: 8));
    "duct-bridge" => BridgeBlock::new(1, true, cost!(Beryllium: 20), 3, true);
    "duct-unloader" => ItemBlock::new(1, true, cost!(Graphite: 20, Silicon: 20, Tungsten: 10));
    "surge-conveyor" => ControlBlock::new(1, false, cost!(SurgeAlloy: 1, Tungsten: 1));
    "surge-router" => ControlBlock::new(1, false, cost!(SurgeAlloy: 5, Tungsten: 1)); // not symmetric
    "unit-cargo-loader" => ControlBlock::new(3, true, cost!(Silicon: 80, SurgeAlloy: 50, Oxide: 20));
    "unit-cargo-unload-point" => ItemBlock::new(2, true, cost!(Silicon: 60, Tungsten: 60));
    // sandbox only
    "item-source" => ItemBlock::new(1, true, &[]);
    "item-void" => ConveyorBlock::new(1, true, &[]);
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
        category: &str,
        name: &str,
        state: Option<&State>,
        _: Option<&RenderingContext>,
    ) -> Option<ImageHolder> {
        if !matches!(
            name,
            "unloader" | "item-source" | "sorter" | "inverted-sorter"
        ) {
            return None;
        }
        let mut p = load(category, name).unwrap().clone();
        if let Some(state) = state {
            if let Some(s) = Self::get_state(state) {
                let mut top = load(category, "center").unwrap().clone();
                p.overlay(top.tint(s.color()), 0, 0);
                return Some(ImageHolder::from(p));
            }
        }
        if name == "unloader" {
            return Some(ImageHolder::from(p));
        }
        let mut null = load("distribution", "cross-full").unwrap().clone();
        null.overlay(&p, 0, 0);
        Some(ImageHolder::from(null))
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
    /// - out: `i32`
    /// - warmup: `f32`
    /// - iterate `links<u8>`
    ///     - in+: `i32`
    /// - moved: `bool`
    fn read(
        &self,
        _: &str,
        _: &str,
        _: &super::BlockRegistry,
        _: &crate::data::map::EntityMapping,
        buff: &mut crate::data::DataRead,
    ) -> Result<(), crate::data::ReadError> {
        buff.read_i32()?;
        buff.read_f32()?;
        for _ in 0..buff.read_u8()? {
            buff.read_i32()?;
        }
        buff.read_bool()?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("invalid coordinates ({x}, {y}) for bridge")]
pub struct BridgeConvertError {
    pub x: i16,
    pub y: i16,
}
