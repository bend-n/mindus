//! schematic drawing
use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use image::codecs::png::PngDecoder;
pub(crate) use image::{DynamicImage, RgbaImage};
use std::io::{BufReader, Cursor};
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use zip::ZipArchive;

pub(crate) use super::autotile::*;
use crate::block::environment::METAL_FLOOR;
use crate::block::Rotation;
use crate::team::SHARDED;
pub(crate) use crate::utils::ImageUtils;
use crate::Map;
pub(crate) use std::borrow::{Borrow, BorrowMut};

use super::schematic::Schematic;
use super::GridPos;

type Cache = DashMap<PathBuf, RgbaImage>;
fn cache() -> &'static Cache {
    CACHE.get_or_init(Cache::new)
}

pub enum ImageHolder {
    Borrow(Ref<'static, PathBuf, RgbaImage>),
    Own(RgbaImage),
}

impl ImageHolder {
    pub fn own(self) -> RgbaImage {
        match self {
            Self::Own(x) => x,
            Self::Borrow(x) => x.clone(),
        }
    }
}

impl Borrow<RgbaImage> for ImageHolder {
    fn borrow(&self) -> &RgbaImage {
        match self {
            Self::Own(x) => x,
            Self::Borrow(x) => x.value(),
        }
    }
}

impl BorrowMut<RgbaImage> for ImageHolder {
    fn borrow_mut(&mut self) -> &mut RgbaImage {
        match self {
            Self::Own(x) => x,
            Self::Borrow(_) => {
                *self = Self::from(std::mem::replace(self, Self::from(RgbaImage::new(0, 0))).own());
                self.borrow_mut()
            }
        }
    }
}

impl Deref for ImageHolder {
    type Target = RgbaImage;
    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl DerefMut for ImageHolder {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.borrow_mut()
    }
}

impl From<Option<Ref<'static, PathBuf, RgbaImage>>> for ImageHolder {
    fn from(value: Option<Ref<'static, PathBuf, RgbaImage>>) -> Self {
        Self::Borrow(value.unwrap())
    }
}

impl From<Ref<'static, PathBuf, RgbaImage>> for ImageHolder {
    fn from(value: Ref<'static, PathBuf, RgbaImage>) -> Self {
        Self::Borrow(value)
    }
}

impl From<RgbaImage> for ImageHolder {
    fn from(value: RgbaImage) -> Self {
        Self::Own(value)
    }
}

static CACHE: OnceLock<Cache> = OnceLock::new();
pub(crate) fn load(category: &str, name: &str) -> Option<Ref<'static, PathBuf, RgbaImage>> {
    let key = Path::new(category).join(name);
    use dashmap::mapref::entry::Entry::*;
    Some(match cache().entry(key) {
        Occupied(v) => v.into_ref().downgrade(),
        Vacant(entry) => {
            let mut p = Path::new("blocks").join(category).join(name);
            p.set_extension("png");
            let Some(i) = load_raw(p) else {
                return None;
            };
            entry.insert(i).downgrade()
        }
    })
}

#[cfg(not(unix))]
const P: &str = "target/out";
#[cfg(unix)]
const P: &str = "/tmp/mindus-tmp";

fn load_raw(f: impl AsRef<Path>) -> Option<RgbaImage> {
    let f = std::fs::File::open(Path::new(P).join(f)).ok()?;
    let r = PngDecoder::new(BufReader::new(f)).unwrap();
    let p = DynamicImage::from_decoder(r).unwrap().into_rgba8();
    assert!(p.width() != 0);
    assert!(p.height() != 0);
    Some(p)
}

fn load_zip() {
    if !Path::new(P).exists() {
        let mut zip = ZipArchive::new(Cursor::new(
            include_bytes!(concat!(env!("OUT_DIR"), "/asset")).to_vec(),
        ))
        .unwrap();
        zip.extract(P).unwrap();
    }
}
pub const TOP: &str = "-top";
const SUFFIXES: &[&str; 9] = &[
    "-bottom", "-mid", "-base", "", "-left", "-right", TOP, "-over", "-team",
];
pub(crate) fn read<S>(category: &str, name: &str, size: S) -> RgbaImage
where
    S: Into<u32> + Copy,
{
    read_with(category, name, SUFFIXES, size)
}

pub(crate) fn read_with<S>(
    category: &str,
    name: &str,
    suffixes: &'static [&'static str],
    size: S,
) -> RgbaImage
where
    S: Into<u32> + Copy,
{
    let mut c = RgbaImage::new(size.into() * 32, size.into() * 32);
    for suffix in suffixes {
        if let Some(p) = load(category, &format!("{name}{suffix}")) {
            if suffix == &"-team" {
                c.overlay(p.clone().tint(SHARDED.color()), 0, 0);
                continue;
            }
            c.overlay(&p, 0, 0);
        }
    }
    c
}

/// trait for renderable objects
pub trait Renderable {
    /// creates a picture of a schematic. Bridges and node connections are not drawn.
    fn render(&self) -> RgbaImage;
}

impl Renderable for Schematic<'_> {
    /// ```
    /// use mindus::*;
    /// let mut s = Schematic::new(2, 3);
    /// s.put(0, 0, &block::distribution::DISTRIBUTOR);
    /// s.put(0, 2, &block::distribution::ROUTER);
    /// s.put(1, 2, &block::walls::COPPER_WALL);
    /// let output /*: RgbaImage */ = s.render();
    /// ```
    fn render(&self) -> RgbaImage {
        load_zip();
        // fill background
        let mut bg = RgbaImage::new(
            ((self.width + 2) * 32) as u32,
            ((self.height + 2) * 32) as u32,
        );
        bg.repeat(METAL_FLOOR.image(None, None, Rotation::Up).borrow());
        let mut canvas = RgbaImage::new(
            ((self.width + 2) * 32) as u32,
            ((self.height + 2) * 32) as u32,
        );
        for (GridPos(x, y), tile) in self.block_iter() {
            let ctx = if tile.block.wants_context() {
                let pctx = PositionContext {
                    position: GridPos(x, y),
                    width: self.width,
                    height: self.height,
                };
                Some(RenderingContext {
                    cross: self.cross(&pctx),
                    position: pctx,
                })
            } else {
                None
            };
            #[cfg(debug_assertions)]
            println!("rendering {tile:?} ({x}, {y}) [+{}]", tile.block.get_size());
            let x = x as u32 - ((tile.block.get_size() - 1) / 2) as u32;
            let y = self.height as u32 - y as u32 - ((tile.block.get_size() / 2) + 1) as u32;
            canvas.overlay(
                tile.image(ctx.as_ref(), tile.get_rotation().unwrap_or(Rotation::Up))
                    .borrow(),
                (x + 1) * 32,
                (y + 1) * 32,
            );
            // canvas.save("tmp.png").unwrap();
        }
        #[cfg(debug_assertions)]
        println!("finishing up");
        image::imageops::overlay(&mut bg, canvas.shadow(), 0, 0);
        bg
    }
}

impl Renderable for Map<'_> {
    fn render(&self) -> RgbaImage {
        load_zip();
        let scale = if self.width + self.height < 2000 {
            8
        } else {
            4
        };
        let mut floor = RgbaImage::new(self.width as u32 * scale, self.height as u32 * scale);
        let mut top = RgbaImage::new(self.width as u32 * scale, self.height as u32 * scale);
        for (x, y, j, tile) in self.tiles.iter().enumerate().map(|(j, t)| {
            (
                (j % self.width),
                // flip y
                (self.height - (j / self.width)) - 1,
                j,
                t,
            )
        }) {
            // draw the floor first.
            floor.overlay(
                // SAFETY: [`load_raw`] forces nonzero image size
                unsafe { &tile.floor_image(None).own().scale(scale) },
                x as u32 * scale,
                y as u32 * scale,
            );
            if let Some(build) = tile.build() {
                let s = build.block.get_size();
                let x = x - ((s - 1) / 2) as usize;
                let y = y - (s / 2) as usize;
                let ctx = (|| {
                    if !build.block.wants_context() {
                        return None;
                    }
                    let pctx = PositionContext {
                        position: GridPos(x, y),
                        width: self.width,
                        height: self.height,
                    };
                    let rctx = RenderingContext {
                        cross: self.cross(j, &pctx),
                        position: pctx,
                    };
                    Some(rctx)
                })();
                top.overlay(
                    // SAFETY: tile.size can never be 0, and [`load_raw`] forces nonzero.
                    unsafe {
                        &tile
                            .build_image(ctx.as_ref())
                            .own()
                            .scale(tile.size() as u32 * scale)
                    },
                    x as u32 * scale,
                    y as u32 * scale,
                );
            }
        }
        image::imageops::overlay(&mut floor, top.shadow(), 0, 0);
        floor
    }
}

#[test]
fn all_blocks() {
    load_zip();
    use crate::block::content::Type;
    use crate::content::Content;
    let reg = crate::block::build_registry();
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

        let t = reg.get(dbg!(t.get_name())).unwrap();
        t.image(
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
        );
    }
}
