//! schematic drawing
pub(crate) use super::autotile::*;
use super::schematic::Schematic;
use super::GridPos;
use crate::block::Rotation;
pub(crate) use crate::utils::*;
use crate::Map;
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
    ($name:literal, $scale:expr) => { paste::paste! {
        $crate::utils::image::ImageHolder::from(match $scale {
            $crate::data::renderer::Scale::Quarter => &$crate::data::renderer::quar::[<$name:snake:upper>],
            $crate::data::renderer::Scale::Eigth => &$crate::data::renderer::eigh::[<$name:snake:upper>],
            $crate::data::renderer::Scale::Full => &$crate::data::renderer::full::[<$name:snake:upper>],
        }.copy())
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
                ImageHolder::from(match $scale {
                    $crate::data::renderer::Scale::Quarter => &$crate::data::renderer::quar::[<$k:snake:upper _ $x:snake:upper>],
                    $crate::data::renderer::Scale::Eigth => &$crate::data::renderer::eigh::[<$k:snake:upper _ $x:snake:upper>],
                    $crate::data::renderer::Scale::Full => &$crate::data::renderer::full::[<$k:snake:upper _ $x:snake:upper>],
                }.copy()),
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

impl Renderable for Schematic<'_> {
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
        // fill background
        // SAFETY: metal-floor is 32x32, the output is a multiple of 32
        let mut bg = unsafe {
            load!("metal-floor", Scale::Full).borrow().repeated(
                ((self.width + 2) * 32) as u32,
                ((self.height + 2) * 32) as u32,
            )
        };
        let mut canvas = Image::alloc(
            ((self.width + 2) * 32) as u32,
            ((self.height + 2) * 32) as u32,
        );
        for (GridPos(x, y), tile) in self.block_iter() {
            let ctx = tile.block.wants_context().then(|| {
                let pctx = PositionContext {
                    position: GridPos(x, y),
                    width: self.width,
                    height: self.height,
                };
                RenderingContext {
                    cross: self.cross(&pctx),
                    position: pctx,
                }
            });
            let x = x as u32 - ((tile.block.get_size() - 1) / 2) as u32;
            let y = self.height as u32 - y as u32 - ((tile.block.get_size() / 2) + 1) as u32;
            unsafe {
                canvas.as_mut().overlay_at(
                    &tile
                        .image(
                            ctx.as_ref(),
                            tile.get_rotation().unwrap_or(Rotation::Up),
                            Scale::Full,
                        )
                        .borrow(),
                    (x + 1) * 32,
                    (y + 1) * 32,
                )
            };
        }
        canvas.as_mut().shadow();
        for x in 0..canvas.width() {
            for y in 0..canvas.height() {
                // canvas has a shadow
                let p2 = unsafe { canvas.pixel(x, y) };
                let p = unsafe { bg.pixel_mut(x, y) };
                let mut p3 = [p[0], p[1], p[2], 255];
                crate::utils::image::blend(&mut p3, p2);
                p.copy_from_slice(&p3[..3]);
            }
        }
        bg
    }
}

impl Renderable for Map<'_> {
    /// Draws a map
    fn render(&self) -> Image<Vec<u8>, 3> {
        let scale = if self.width + self.height < 2000 {
            Scale::Quarter
        } else {
            Scale::Eigth
        };
        // todo combine these (beware of floor drawing atop buildings) (planned solution:? ptr blocks)
        let mut floor: Image<_, 3> =
            Image::alloc(scale * self.width as u32, scale * self.height as u32);
        let mut top: Image<_, 4> =
            Image::alloc(scale * self.width as u32, scale * self.height as u32);
        for y in 0..self.height {
            for x in 0..self.width {
                // Map::new() allocates w*h items
                let j = x + self.width * y;
                let tile = unsafe { self.tiles.get_unchecked(j) };
                let y = self.height - y - 1;
                // draw the floor first.
                // println!("draw {tile:?} ({x}, {y})");
                unsafe {
                    floor.as_mut().overlay_at(
                        &tile.floor(scale).borrow(),
                        scale * x as u32,
                        scale * y as u32,
                    )
                };
                if tile.has_ore() {
                    unsafe {
                        floor.as_mut().overlay_at(
                            &tile.ore(scale).borrow(),
                            scale * x as u32,
                            scale * y as u32,
                        )
                    };
                }

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
                    let ctx = build.block.wants_context().then(|| {
                        let pctx = PositionContext {
                            position: GridPos(x, y),
                            width: self.width,
                            height: self.height,
                        };
                        RenderingContext {
                            cross: self.cross(j, &pctx),
                            position: pctx,
                        }
                    });
                    unsafe {
                        top.as_mut().overlay_at(
                            &tile.build_image(ctx.as_ref(), scale).borrow(),
                            scale * x as u32,
                            scale * y as u32,
                        )
                    };
                }
            }
        }
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
                top.as_mut()
                    .overlay_at(&entity.draw(scale).borrow(), scale * x, scale * y)
            };
        }
        unsafe { floor.as_mut().overlay(&top.as_ref()) };
        floor
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
