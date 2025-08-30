//! schematic drawing
use super::GridPos;
pub(crate) use super::autotile::*;
use super::schematic::Schematic;
use crate::block::State;
use crate::block::content::Type;
use crate::color_mapping::BLOCK2COLOR;
use crate::data::map::Registrar;
use crate::team::Team;
pub(crate) use crate::utils::*;
use crate::{Map, content::Content};
use crate::{
    block::Rotation,
    data::map::{ThinBloc, ThinMapData},
};
use atools::ArrayTools;
use either::Either;
use fimg::{BlendingOverlay, BlendingOverlayAt, uninit};
use std::hint::unlikely;
use std::iter::successors;
use std::ops::Coroutine;
use std::pin::Pin;

include!(concat!(env!("OUT_DIR"), "/full.rs"));
include!(concat!(env!("OUT_DIR"), "/quar.rs"));
include!(concat!(env!("OUT_DIR"), "/eigh.rs"));

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Scale {
    Full,
    // Half,
    Quarter,
    Eigth,
}

impl Scale {
    #[must_use]
    pub const fn px(self) -> u8 {
        match self {
            Self::Full => 32,
            Self::Quarter => 32 / 4,
            Self::Eigth => 32 / 8,
        }
    }
}

impl std::ops::Mul<u32> for Scale {
    type Output = u32;
    fn mul(self, rhs: u32) -> u32 {
        self.px() as u32 * rhs
    }
}

#[macro_export]
macro_rules! load {
	(raw $name: literal, $scale:expr) => {
		paste::paste! { match $scale {
            $crate::data::renderer::Scale::Quarter => $crate::data::renderer::quar::[<$name:snake:upper>],
            $crate::data::renderer::Scale::Eigth => $crate::data::renderer::eigh::[<$name:snake:upper>],
            $crate::data::renderer::Scale::Full => $crate::data::renderer::full::[<$name:snake:upper>],
        }
    } };
    (8x $name:literal) => { paste::paste! {
        car::map!([
            load!([<$name 1>]),
            load!([<$name 2>]),
            load!([<$name 3>]),
            load!([<$name 4>]),
            load!([<$name 5>]),
            load!([<$name 6>]),
            load!([<$name 7>]),
            load!([<$name 8>]),
        ], |x| car::map!(x, DynImage::from))
    } };
    ($name:literal, $scale:expr) => { paste::paste! {
        #[allow(unused_unsafe)] unsafe { match $scale {
            $crate::data::renderer::Scale::Quarter => &$crate::data::renderer::quar::[<$name:snake:upper>],
            $crate::data::renderer::Scale::Eigth => &$crate::data::renderer::eigh::[<$name:snake:upper>],
            $crate::data::renderer::Scale::Full => &$crate::data::renderer::full::[<$name:snake:upper>],
        }.mapped($crate::utils::Cow::Ref) }
    } };
    ($name: ident) => { paste::paste! {
        [$crate::data::renderer::full::[<$name:snake:upper>].copy(), $crate::data::renderer::quar::[<$name:snake:upper>].copy(), $crate::data::renderer::eigh::[<$name:snake:upper>].copy()]
    } };
    ($name: literal) => { paste::paste! {
        [$crate::data::renderer::full::[<$name:snake:upper>].copy(), $crate::data::renderer::quar::[<$name:snake:upper>].copy(), $crate::data::renderer::eigh::[<$name:snake:upper>].copy()]
    } };
    (from $v:ident which is [$($k:literal $(|)?)+], $scale: ident) => {
        $crate::data::renderer::load!($scale -> match $v {
            $($k => $k,)+
        })
    };
    // turn load!(s -> match x { "v" => "y" }) into match x { "v" => load!("y", s) }
    ($scale:ident -> match $v:ident { $($k:pat => $nam:literal $(,)?)+ }) => {
        match $v {
            $($k => $crate::data::renderer::load!($nam, $scale),)+
            #[allow(unreachable_patterns)]
            n => unreachable!("{n:?}"),
        }
    };
    (concat $x:literal => $v:ident which is [$($k:literal $(|)?)+], $scale: ident) => { paste::paste! {
        match $v {
            $($k =>
                #[allow(unused_unsafe)] unsafe { (match $scale {
                    $crate::data::renderer::Scale::Quarter => &$crate::data::renderer::quar::[<$k:snake:upper _ $x:snake:upper>],
                    $crate::data::renderer::Scale::Eigth => &$crate::data::renderer::eigh::[<$k:snake:upper _ $x:snake:upper>],
                    $crate::data::renderer::Scale::Full => &$crate::data::renderer::full::[<$k:snake:upper _ $x:snake:upper>],
                }.mapped($crate::utils::Cow::Ref)) },
            )+
            #[allow(unreachable_patterns)]
            n => unreachable!("{n:?}"),
        }
    } };
}
pub(crate) use load;

/// trait for renderable objects
pub trait Renderable {
    /// create a picture
    #[must_use = "i did so much work for you"]
    fn render(&self) -> Image<Vec<u8>, 3>;
}

impl Renderable for Schematic {
    /// creates a picture of a schematic. Bridges and node connections are not drawn.
    /// ```
    /// # use mindus::*;
    /// # use mindus::block::*;
    /// let mut s = Schematic::new(2, 3);
    /// s.put(0, 0, &DISTRIBUTOR);
    /// s.put(0, 2, &ROUTER);
    /// s.put(1, 2, &COPPER_WALL);
    /// let output /*: Image */ = s.render();
    /// ```
    fn render(&self) -> Image<Vec<u8>, 3> {
        let scale = if self.width + self.height > 500 {
            Scale::Quarter
        } else {
            Scale::Full
        };
        // fill background
        // SAFETY: metal-floor is scalexscale, the output is a multiple of scale
        let x_fac = cfg!(feature = "square") as u32
            * self.height.checked_sub(self.width).unwrap_or(0) as u32
            + 2;
        let y_fac = cfg!(feature = "square") as u32
            * self.width.checked_sub(self.height).unwrap_or(0) as u32
            + 2;
        let mut bg = unsafe {
            load!("metal-floor", scale).repeated(
                scale * (self.width + x_fac as usize) as u32,
                scale * (self.height + y_fac as usize) as u32,
            )
        };
        let mut canvas = Image::alloc(bg.width(), bg.height());
        for (GridPos(x, y), tile) in self.block_iter() {
            let ctx = tile.block.wants_context().then(|| {
                let pctx = PositionContext {
                    position: GridPos(x, y),
                    width: self.width,
                    height: self.height,
                };
                let (cross, corners) = self.cross(&pctx);
                RenderingContext {
                    cross,
                    corners,
                    position: pctx,
                }
            });
            let x = x as u32 - ((tile.block.get_size() - 1) / 2) as u32;
            let y = self.height as u32 - y as u32 - ((tile.block.get_size() / 2) + 1) as u32;
            unsafe {
                canvas.as_mut().overlay_at(
                    &tile.image(
                        ctx.as_ref(),
                        tile.get_rotation().unwrap_or(Rotation::Up),
                        scale,
                    ),
                    scale * (x + x_fac / 2),
                    scale * (y + y_fac / 2),
                )
            };
        }
        for (p, b) in self.block_iter() {
            let Some(&State::Point(mut relative)) = b.get_state() else {
                continue;
            };
            let directional = matches!(b.block.name(), "duct-bridge" | "reinforced-bridge-conduit");
            if false {
                let offset = match b.rot {
                    Rotation::Right => (1, 0),
                    Rotation::Down => (0, -1),
                    Rotation::Left => (-1, 0),
                    Rotation::Up => (0, 1),
                };
                relative = successors(Some(offset), |&(x, y)| Some((x + offset.0, y + offset.1)))
                    .take(4)
                    .find(|x| {
                        self.get((p.0 as i32 + x.0) as _, (p.1 as i32 + x.1) as _)
                            .ok()
                            .flatten()
                            .is_some_and(|x| x.block == b.block)
                    });
            }
            if let Some(relative) = relative
                && relative != (0, 0)
                && let n @ ("bridge-conveyor" | "bridge-conduit" | "phase-conveyor"
                | "phase-conduit") = b.block.name()
                && let Ok(Some(x)) = self.get(
                    (p.0 as i32 + relative.0) as _,
                    (p.1 as i32 + relative.1) as _,
                )
                && x.block.name() == n
            {
                let mut bridge = load!(concat "bridge" => n which is ["bridge-conveyor" | "bridge-conduit" | "phase-conveyor" | "phase-conduit" | "duct-bridge" | "reinforced-bridge-conduit"], scale);

                /*
                let mut bridge = bridge.own();
                bridge.chunked_mut().for_each(|x| match &mut x[3] {
                    0 => (),
                    y => *y = (*y as f32 * 0.20) as u8,
                });
                bridge.save(format!("{n}-bridge.png"));
                let mut bridge = ImageHolder::from(bridge);
                */
                if relative.1 != 0 {
                    // continue;
                    bridge =
                        Image::build(bridge.height(), bridge.width()).buf(bridge.take_buffer());
                }
                let arrow = load!(concat "arrow" => n which is ["bridge-conveyor"| "bridge-conduit" | "phase-conveyor" | "phase-conduit" | "duct-bridge" | "reinforced-bridge-conduit"], scale);
                /*
                let mut arrow = arrow.own();
                arrow.chunked_mut().for_each(|x| match &mut x[3] {
                    0 => (),
                    y => *y = (*y as f32 * 0.20) as u8,
                });
                arrow.save(format!("{n}-arrow.png"));
                let mut arrow = ImageHolder::from(arrow);
                */

                for index in if relative.0 > 0 {
                    Either::Right(
                        scale.px() as i32 * 2..scale.px() as i32 * relative.0 + scale.px() as i32,
                    )
                } else {
                    Either::Left(
                        relative.0 * scale.px() as i32 + scale.px() as i32 * 2..scale.px() as i32,
                    )
                } {
                    unsafe {
                        canvas.overlay_blended_at(
                            &bridge,
                            (p.0 as i32 * scale.px() as i32 + index) as u32,
                            scale * (self.height as u32 - p.1 as u32 - 1 + y_fac / 2),
                        )
                    };
                }

                for index in if relative.1 > 0 {
                    Either::Right(
                        -(scale.px() as i32) - scale.px() as i32 * (relative.1 - 1)
                            ..-(scale.px() as i32),
                    )
                } else {
                    Either::Left(0..(-relative.1 - 1) * scale.px() as i32)
                } {
                    let y =
                        ((self.height as i32 - p.1 as i32 + 1) * scale.px() as i32 + index) as u32;

                    unsafe {
                        canvas.overlay_blended_at(&bridge, scale * (p.0 as u32 + x_fac / 2), y)
                    };
                }
                if relative.0 != 0 {
                    unsafe {
                        canvas.overlay_blended_at(
                            &arrow.rotated(if directional {
                                b.rot.rotated(false).count()
                            } else if relative.0 < 0 {
                                2
                            } else {
                                0
                            }),
                            scale.px() as u32
                                + ((scale * p.0 as u32).cast_signed()
                                    + (scale.px() as i32 * relative.0 / 2))
                                    as u32,
                            (y_fac / 2 + scale * (self.height as u32 - p.1 as u32)) - 1,
                        )
                    };
                } else {
                    unsafe {
                        canvas.overlay_blended_at(
                            &arrow.rotated(if directional {
                                b.rot.rotated(false).count()
                            } else if relative.1 < 0 {
                                1
                            } else {
                                3
                            }),
                            (scale * p.0 as u32) + (x_fac / 2 + scale.px() as u32) - 1,
                            scale.px() as u32
                                + ((scale * (self.height as u32 - p.1 as u32 - 1)).cast_signed()
                                    + (scale.px() as i32 * -relative.1 / 2))
                                    as u32,
                        );
                    }
                }
            }
        }

        if matches!(scale, Scale::Full) {
            ImageUtils::shadow(&mut canvas);
            unsafe { bg.overlay_blended(&canvas) };
        } else {
            unsafe { bg.overlay(&canvas) };
        }
        bg
    }
}

impl Renderable for Map {
    /// Draws a map
    #[implicit_fn::implicit_fn]
    fn render(&self) -> Image<Vec<u8>, 3> {
        let scale = if self.width + self.height < 600 {
            Scale::Full
        } else if self.width + self.height < 4000 {
            Scale::Quarter
        } else {
            Scale::Eigth
        };
        use crate::data::map::table;
        let mut img = uninit::Image::<_, 3>::new(
            (scale * self.width as u32).try_into().unwrap(),
            (scale * self.height as u32).try_into().unwrap(),
        );
        // loop1 draws the floor
        for y in 0..self.height {
            for x in 0..self.width {
                // Map::new() allocates w*h items
                let j = x + self.width * y;
                let tile = unsafe { self.tiles.get_unchecked(j) };
                let y = self.height - y - 1;
                // println!("draw {tile:?} ({x}, {y})");

                if [
                    Type::ColoredFloor,
                    Type::MetalTiles1,
                    Type::MetalTiles2,
                    Type::MetalTiles3,
                    Type::MetalTiles4,
                    Type::MetalTiles5,
                    Type::MetalTiles6,
                    Type::MetalTiles7,
                    Type::MetalTiles8,
                    Type::MetalTiles9,
                    Type::MetalTiles10,
                    Type::MetalTiles11,
                    Type::MetalTiles12,
                ]
                .contains(&tile.floor)
                {
                    let mask = crate::data::autotile::nbors(
                        self.corners(j).map(_.map(_.floor)),
                        self.cross(j).map(_.map(_.floor)),
                        tile.floor,
                    );
                    let i =
                        &crate::data::autotile::select(tile.floor.get_name(), mask)[scale as usize];
                    if tile.floor == Type::ColoredFloor {
                        let mut i = i.boxed();
                        unsafe {
                            img.overlay_at(
                                &i.as_mut()
                                    .tint(tile.nd.skip::<3>().take::<3>().into())
                                    .as_ref(),
                                scale * x as u32,
                                scale * y as u32,
                            )
                        };
                    } else {
                        unsafe { img.overlay_at(i, scale * x as u32, scale * y as u32) };
                    }
                } else {
                    unsafe {
                        img.overlay_at(
                            &table(tile.floor, scale),
                            scale * x as u32,
                            scale * y as u32,
                        );
                    }
                }
                macro_rules! f {
                    ($($x: literal)+) => { paste::paste!{
                        ([$(load!([<rune _ overlay $x>] ),)+], [$(load!([<rune _ overlay _ crux $x>] ),)+])
                    }};
                }
                const RUNES: ([[Image<&[u8], 4>; 3]; 109], [[Image<&[u8], 4>; 3]; 109]) = f![0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108];

                if tile.has_ore() {
                    match tile.ore {
                        Type::RuneOverlay => unsafe {
                            img.overlay_at(
                                &RUNES.0[tile.nd[2] as usize][scale as usize],
                                scale * x as u32,
                                scale * y as u32,
                            );
                        },
                        Type::RuneOverlayCrux => unsafe {
                            img.overlay_at(
                                &RUNES.1[tile.nd[2] as usize][scale as usize],
                                scale * x as u32,
                                scale * y as u32,
                            );
                        },
                        Type::CharacterOverlay | Type::CharacterOverlayWhite => {
                            macro_rules! f {
                                ($($x: literal)+) => { paste::paste!{
                                    [$(load!([<character _ overlay $x>] ),)+]
                                }};
                            }

                            const LETTERS: [[Image<&[u8], 4>; 3]; 64] = f![0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63];
                            unsafe {
                                img.overlay_at(
                                    LETTERS[(tile.nd[2] & 0x3f) as usize][scale as usize]
                                        .mapped(image::Cow::Ref)
                                        .rotate(4 - (tile.nd[2] >> 6)),
                                    scale * x as u32,
                                    scale * y as u32,
                                )
                            };
                        }
                        ore => unsafe {
                            img.overlay_at(&table(ore, scale), scale * x as u32, scale * y as u32);
                        },
                    }
                }
            }
        }
        let mut img = unsafe { img.assume_init() };
        // loop2 draws the buildings
        for y in 0..self.height {
            for x in 0..self.width {
                let j = x + self.width * y;
                let tile = unsafe { self.tiles.get_unchecked(j) };
                let y = self.height - y - 1;
                if let Some(build) = tile.build() {
                    let s = build.block.get_size();
                    let x = x
                        - (match s {
                            1 | 2 => 0,
                            3 | 4 => 1,
                            5 | 6 => 2,
                            7 | 8 => 3,
                            9 => 4,
                            // SAFETY: no block too big
                            _ => unsafe { std::hint::unreachable_unchecked() },
                        }) as usize;
                    let y = y
                        - (match s {
                            1 => 0,
                            2 | 3 => 1,
                            4 | 5 => 2,
                            6 | 7 => 3,
                            8 | 9 => 4,
                            // SAFETY: no block too big
                            _ => unsafe { std::hint::unreachable_unchecked() },
                        }) as usize;
                    if unlikely(matches!(
                        build.block.name,
                        Type::ColoredWall | Type::MetalWall1 | Type::MetalWall2 | Type::MetalWall3
                    )) {
                        let mask = crate::data::autotile::nbors(
                            self.corners(j)
                                .map(|x| x.and_then(|x| x.build().map(|b| b.block.name))),
                            self.cross(j)
                                .map(|x| x.and_then(|x| x.build().map(|b| b.block.name))),
                            build.block.name,
                        );
                        let i =
                            crate::data::autotile::select(build.block.name(), mask)[scale as usize];
                        if build.block.name == Type::ColoredWall {
                            unsafe {
                                img.overlay_at(
                                    &i.boxed()
                                        .as_mut()
                                        .tint(tile.nd.skip::<3>().take::<3>().into())
                                        .as_ref(),
                                    scale * x as u32,
                                    scale * y as u32,
                                )
                            };
                        } else {
                            unsafe { img.overlay_at(&i, scale * x as u32, scale * y as u32) };
                        }
                        continue;
                    }
                    let ctx = build.block.wants_context().then(|| {
                        let pctx = PositionContext {
                            position: GridPos(x, y),
                            width: self.width,
                            height: self.height,
                        };
                        RenderingContext {
                            cross: self
                                .cross(j)
                                .map(|b| b.and_then(|b| Some((b.get_block()?, b.get_rotation()?)))),
                            corners: Default::default(),
                            position: pctx,
                        }
                    });
                    unsafe {
                        img.as_mut().overlay_at(
                            &tile.build_image(ctx.as_ref(), scale),
                            scale * x as u32,
                            scale * y as u32,
                        )
                    };
                }
            }
        }
        // loop3 draws the units
        for entity in &self.entities {
            // bounds checks
            let (x, y) = (
                entity.state.position.0 as u32,
                self.height as u32 - entity.state.position.1 as u32 - 1,
            );
            if x < 10 || x as usize > self.width - 10 || y < 10 || y as usize > self.height - 10 {
                continue;
            }
            unsafe {
                img.as_mut()
                    .overlay_at(&entity.draw(scale), scale * x, scale * y)
            };
        }
        img
    }
}

#[test]
fn all_blocks() {
    use crate::block::content::Type;
    use crate::content::Content;
    for t in 19..Type::WorldMessage as u16 {
        let t = Type::try_from(t).unwrap();
        if matches!(t, |Type::Empty| Type::SlagCentrifuge
            | Type::HeatReactor
            | Type::LegacyMechPad
            | Type::LegacyUnitFactory
            | Type::LegacyUnitFactoryAir
            | Type::LegacyUnitFactoryGround
            | Type::CommandCenter)
        {
            continue;
        }
        let name = t.get_name();
        let t = crate::block::BLOCK_REGISTRY.get(name).unwrap();
        let _ = t.image(
            None,
            Some(&RenderingContext {
                corners: [None; 4],
                cross: [None; 4],
                position: PositionContext {
                    position: GridPos(0, 0),
                    width: 5,
                    height: 5,
                },
            }),
            Rotation::Up,
            Scale::Quarter,
        );
    }
}

pub fn draw_units(
    map: &mut crate::data::map::MapReader,
    mut img: Image<&mut [u8], 3>,
    size: (u16, u16),
) -> Result<(), super::map::ReadError> {
    use std::ops::CoroutineState::*;
    let scale = if size.0 + size.1 < 2000 {
        Scale::Quarter
    } else {
        Scale::Eigth
    };

    let mut co = map.entities()?;
    let n = match Pin::new(&mut co).resume(()) {
        Yielded(crate::data::map::EntityData::Length(x)) => x,
        Complete(Err(e)) => return Err(e),
        _ => unreachable!(),
    };
    'out: {
        for _ in 0..n {
            match Pin::new(&mut co).resume(()) {
                Yielded(crate::data::map::EntityData::Data(entity)) => {
                    // bounds checks
                    let (x, y) = (
                        entity.state.position.0 as u32,
                        size.1 as u32 - entity.state.position.1 as u32 - 1,
                    );
                    if x < 10
                        || x as usize > size.0 as usize - 10
                        || y < 10
                        || y as usize > size.1 as usize - 10
                    {
                        continue;
                    }
                    unsafe {
                        img.as_mut()
                            .overlay_at(&entity.draw(scale), scale * x, scale * y)
                    };
                }
                Complete(Err(e)) => return Err(e),
                Complete(Ok(())) => break 'out,
                x => unreachable!("{x:?}"),
            }
        }
        match Pin::new(&mut co).resume(()) {
            Complete(Ok(())) => (),
            _ => unreachable!(),
        };
    }
    Ok(())
}

/// Draws a map in a single pass.
/// This is quite fast, but, unfortunately, has no memory, so conveyors will be drawing imporperly. As will sorters.
///
/// Reader must have read to the map section.
/// Will walk through the map section. use [`draw_units`] after, if you like.
pub fn draw_map_single(
    map: &mut crate::data::map::MapReader,
    r: Registrar,
) -> Result<(Image<Box<[u8]>, 3>, (u16, u16)), super::map::ReadError> {
    use std::ops::CoroutineState::*;
    let mut co = map.thin_map(r)?;
    let (w, h) = match Pin::new(&mut co).resume(()) {
        Yielded(ThinMapData::Init { width, height }) => (width, height),
        Complete(Err(x)) => return Err(x),
        _ => unreachable!(),
    };
    let scale = if w + h < 2000 {
        Scale::Quarter
    } else {
        Scale::Eigth
    };
    let mut img = uninit::Image::<_, 3>::new(
        (scale * w as u32).try_into().unwrap(),
        (scale * h as u32).try_into().unwrap(),
    );
    use crate::data::map::table;
    // loop1 draws the floor
    for y in 0..h {
        for x in 0..w {
            let (floor, ore) = match Pin::new(&mut co).resume(()) {
                Yielded(ThinMapData::Tile { floor, ore }) => (floor, ore),
                Complete(Err(x)) => return Err(x),
                _ => unreachable!(),
            };
            let y = h - y - 1;
            // println!("draw tile {floor} {ore} @ {x} {y}");
            unsafe { img.overlay_at(&table(floor, scale), scale * x as u32, scale * y as u32) };
            if ore != Type::Air {
                unsafe {
                    img.overlay_at(&table(ore, scale), scale * x as u32, scale * y as u32);
                }
            }
        }
    }
    let mut img = unsafe { img.assume_init() }.boxed();
    let mut i = 0;
    while i < (w as usize * h as usize) {
        let mut draw = |i, r, b: &'static crate::block::Block| {
            let x = i % w as usize;
            let y = i / w as usize;
            let y = h as usize - y - 1;
            let s = b.get_size();
            let x = x
                - (match s {
                    1 | 2 => 0,
                    3 | 4 => 1,
                    5 | 6 => 2,
                    7 | 8 => 3,
                    9 => 4,
                    // SAFETY: no block too big
                    _ => unsafe { std::hint::unreachable_unchecked() },
                }) as usize;
            let y = y
                - (match s {
                    1 => 0,
                    2 | 3 => 1,
                    4 | 5 => 2,
                    6 | 7 => 3,
                    8 | 9 => 4,
                    // SAFETY: no block too big
                    _ => unsafe { std::hint::unreachable_unchecked() },
                }) as usize;
            let ctx = b.wants_context().then(|| {
                let pctx = PositionContext {
                    position: GridPos(x, y),
                    width: w as usize,
                    height: h as usize,
                };
                RenderingContext {
                    corners: [None; 4],
                    cross: [None; 4], // woe
                    position: pctx,
                }
            });
            unsafe {
                img.as_mut().overlay_at(
                    &b.image(None, ctx.as_ref(), r, scale),
                    scale * x as u32,
                    scale * y as u32,
                )
            };
        };
        match Pin::new(&mut co).resume(()) {
            Yielded(ThinMapData::Bloc(ThinBloc::Many(None, n))) => {
                i += n as usize;
            }
            Yielded(ThinMapData::Bloc(ThinBloc::Build(r, bloc, _))) => {
                draw(i, r, bloc);
            }
            Yielded(ThinMapData::Bloc(ThinBloc::Many(Some(bloc), n))) => {
                for i in i..=i + n as usize {
                    draw(i, Rotation::Up, bloc);
                }
                i += n as usize;
            }
            Complete(Err(x)) => return Err(x),
            x => unreachable!("{x:?}"),
        }
        i += 1;
    }
    match Pin::new(&mut co).resume(()) {
        Complete(Ok(())) => (),
        f => unreachable!("{f:?}"),
    };

    Ok((img, (w, h)))
}

/// Draws a map in a single pass. Uses the silly team color thing that mindustry has.
/// probably broken- if you want to use this ask me to update the block2color
pub fn draw_map_simple(
    map: &mut crate::data::map::MapReader,
    r: Registrar,
) -> Result<(Image<Box<[u8]>, 3>, (u16, u16)), super::map::ReadError> {
    use std::ops::CoroutineState::*;
    let mut co = map.thin_map(r)?;
    let (w, h) = match Pin::new(&mut co).resume(()) {
        Yielded(ThinMapData::Init { width, height }) => (width, height),
        Complete(Err(x)) => return Err(x),
        _ => unreachable!(),
    };
    let mut img = uninit::Image::<u8, 3>::new(
        (w as u32).try_into().unwrap(),
        (h as u32).try_into().unwrap(),
    );
    // loop1 draws the floor
    for y in 0..h {
        for x in 0..w {
            let (floor, ore) = match Pin::new(&mut co).resume(()) {
                Yielded(ThinMapData::Tile { floor, ore }) => (floor, ore),
                Complete(Err(x)) => return Err(x),
                _ => unreachable!(),
            };
            let t = (ore != Type::Air).then_some(ore).unwrap_or(floor);
            let y = h - y - 1;
            let i1 = img.at(x as u32, y as u32) as usize;
            unsafe {
                img.slice(i1..i1 + 3)
                    .write_copy_of_slice(&BLOCK2COLOR[t as u16 as usize][..])
            };
        }
    }
    let mut img = unsafe { img.assume_init() }.boxed();
    let mut i = 0;
    while i < (w as usize * h as usize) {
        let mut draw = |i, b: &'static crate::block::Block, team: Team| {
            let x = i % w as usize;
            let y = i / w as usize;
            let y = h as usize - y - 1;
            let s = b.get_size();
            let x = x
                - (match s {
                    1 | 2 => 0,
                    3 | 4 => 1,
                    5 | 6 => 2,
                    7 | 8 => 3,
                    9 => 4,
                    // SAFETY: no block too big
                    _ => unsafe { std::hint::unreachable_unchecked() },
                }) as usize;
            let y = y
                - (match s {
                    1 => 0,
                    2 | 3 => 1,
                    4 | 5 => 2,
                    6 | 7 => 3,
                    8 | 9 => 4,
                    // SAFETY: no block too big
                    _ => unsafe { std::hint::unreachable_unchecked() },
                }) as usize;
            for x in x..(x as usize + s as usize).min(w as usize) {
                for y in y..(y as usize + s as usize).min(h as usize) {
                    unsafe {
                        img.set_pixel(x as u32, y as u32, <[u8; 3]>::from(team.color()));
                    }
                }
            }
        };
        match Pin::new(&mut co).resume(()) {
            Yielded(ThinMapData::Bloc(ThinBloc::Many(None, n))) => {
                i += n as usize;
            }
            Yielded(ThinMapData::Bloc(ThinBloc::Build(_, bloc, t))) => {
                draw(i, bloc, t);
            }
            Yielded(ThinMapData::Bloc(ThinBloc::Many(Some(bloc), n))) => {
                for i in i..=i + n as usize {
                    draw(i, bloc, Team::DERELICT);
                }
                i += n as usize;
            }
            Complete(Err(x)) => return Err(x),
            x => unreachable!("{x:?}"),
        }
        i += 1;
    }
    match Pin::new(&mut co).resume(()) {
        Complete(Ok(())) => (),
        f => unreachable!("{f:?}"),
    };

    Ok((img, (w, h)))
}
