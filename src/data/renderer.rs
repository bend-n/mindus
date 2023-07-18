//! schematic drawing
use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use image::codecs::png::PngDecoder;
pub(crate) use image::{DynamicImage, RgbaImage};
use std::io::{BufReader, Cursor};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use zip::ZipArchive;

use crate::block::environment::METAL_FLOOR;
use crate::block::{Block, Rotation};
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

pub type Cross<'l> = [Option<(&'l Block, Rotation)>; 4];
/// holds the 4 bordering blocks
#[derive(Copy, Clone)]
pub struct RenderingContext<'l> {
    pub cross: Cross<'l>,
    pub rotation: Rotation,
    pub position: PositionContext,
}

/// holds positions
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct PositionContext {
    pub position: GridPos,
    pub width: usize,
    pub height: usize,
}

impl std::fmt::Debug for PositionContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "PC<{:?} ({}/{})>",
            self.position, self.width, self.height
        )
    }
}

impl std::fmt::Debug for RenderingContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for RenderingContext<'_> {
    /// this display impl shows RC<$directions=+own rotation>
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RC<")?;
        macro_rules! f {
            ($f:expr, $z:expr, $x:literal, $at: expr, $srot: expr) => {
                if let Some((_, rot)) = $z {
                    if (rot == $at && rot.mirrored(true, true) != $srot) {
                        $f.write_str($x)?;
                    }
                }
            };
        }
        f!(f, self.cross[0], "N = ", Rotation::Down, self.rotation);
        f!(f, self.cross[1], "E = ", Rotation::Left, self.rotation);
        f!(f, self.cross[2], "S = ", Rotation::Up, self.rotation);
        f!(f, self.cross[3], "W = ", Rotation::Right, self.rotation);

        write!(f, "{:?}>", self.rotation)
    }
}

static CACHE: OnceLock<Cache> = OnceLock::new();
pub(crate) fn load(category: &str, name: &str) -> Option<Ref<'static, PathBuf, RgbaImage>> {
    let key = Path::new("blocks").join(category).join(name);
    let mut p = key.clone();
    use dashmap::mapref::entry::Entry::*;
    Some(match cache().entry(key) {
        Occupied(v) => v.into_ref().downgrade(),
        Vacant(entry) => {
            p.set_extension("png");
            let Some(i) = load_raw(p) else {
                return None;
            };
            entry.insert(i).downgrade()
        }
    })
}

fn load_raw(f: impl AsRef<Path>) -> Option<RgbaImage> {
    let f = std::fs::File::open(Path::new("target/out").join(f)).ok()?;
    let r = PngDecoder::new(BufReader::new(f)).unwrap();
    let p = DynamicImage::from_decoder(r).unwrap().into_rgba8();
    assert!(p.width() != 0);
    assert!(p.height() != 0);
    Some(p)
}

fn load_zip() {
    if !Path::new("target/out").exists() {
        let mut zip = ZipArchive::new(Cursor::new(
            include_bytes!(concat!(env!("OUT_DIR"), "/asset")).to_vec(),
        ))
        .unwrap();
        zip.extract("target/out").unwrap();
    }
}
pub const TOP: &str = "-top";
const SUFFIXES: &[&str; 9] = &[
    "-bottom", "-mid", "", "-base", "-left", "-right", TOP, "-over", "-team",
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

pub trait RotationState {
    fn get_rotation(&self) -> Option<Rotation>;
}
pub trait BlockState<'l> {
    fn get_block(&'l self) -> Option<&'l Block>;
}
pub(crate) trait Crossable {
    fn cross(&self, j: usize, c: &PositionContext) -> Cross;
}

// pub(crate) trait Darray {
//     type Output;
//     fn n(&self, j: usize, c: &PositionContext) -> Option<&Self::Output>;
//     fn e(&self, j: usize, c: &PositionContext) -> Option<&Self::Output>;
//     fn s(&self, j: usize, c: &PositionContext) -> Option<&Self::Output>;
//     fn w(&self, j: usize, c: &PositionContext) -> Option<&Self::Output>;
// }

#[cfg(test)]
fn print_crosses(v: Vec<Cross<'_>>, height: usize) -> String {
    let mut s = String::new();
    for c in v.chunks(height) {
        for c in c {
            s.push(c[0].map_or('_', |(_, r)| r.ch()));
            for c in c[1..].iter() {
                s.push(',');
                s.push(c.map_or('_', |(_, r)| r.ch()));
            }
            s.push(' ');
        }
        s.push('\n');
    }
    s
}

#[test]
fn test_cross() {
    use crate::block::distribution::define;
    let mut reg = crate::block::BlockRegistry::default();
    crate::block::distribution::register(&mut reg);
    let mut ss = super::schematic::SchematicSerializer(&reg);
    macro_rules! test {
        ($schem: literal => $($a:tt,$b:tt,$c:tt,$d:tt)*) => {
            let s = ss.deserialize_base64($schem).unwrap();
            let mut c = vec![];
                println!("{:#?}", s.blocks);

            for (position, _) in s.block_iter() {
                let pctx = PositionContext {
                    position,
                    width: s.width,
                    height: s.height,
                };
                c.push(s.cross(&pctx));
            }
            let n = s.tags.get("name").map_or("<unknown>", |x| &x);
            let cc: Vec<Cross> = vec![
                $(define!($a,$b,$c,$d),)*
            ];
            if cc != c {
                let a = print_crosses(cc, s.height as usize);
                let b = print_crosses(c, s.height as usize);
                for diff in diff::lines(&a, &b) {
                    match diff {
                        diff::Result::Left(l)    => println!("\x1b[38;5;1m{}", l),
                        diff::Result::Right(r)   => println!("\x1b[38;5;2m{}", r),
                        diff::Result::Both(l, _) => println!("\x1b[0m{}", l),
                    }
                }
                print!("\x1b[0m");
                /*
                for diff in diff::slice(&c.into_iter().enumerate().collect::<Vec<_>>(), &cc.into_iter().enumerate().collect::<Vec<_>>()) {
                    match diff {
                        diff::Result::Left((i, l))    => println!("\x1b[38;5;1m- {l:?} at {i}"),
                        diff::Result::Right((i, r))   => println!("\x1b[38;5;2m+ {r:?} at {i}"),
                        diff::Result::Both((i, l), _) => println!("\x1b[0m  {l:?} at {i}"),
                    }
                }
                */
                panic!("test {n} \x1b[38;5;1mfailed\x1b[0m")
            }
            println!("test {n} \x1b[38;5;2mpassed\x1b[0m");
        };
    }
    // crosses go from bottom left -> top left -> bottom left + 1 -> top left + 1...
    // the symbols are directions (> => Right...), which mean the neighbors pointing direction
    // _ = no block

    // the basic test
    // ─┐
    // ─┤
    test!("bXNjaAF4nGNgYmBiZmDJS8xNZWBNSizOTGbgTkktTi7KLCjJzM9jYGBgy0lMSs0pZmCNfr9gTSwjA0dyfl5ZamV+EVCOhQEBGGEEM4hiZGAGAOb+EWA=" =>
    //  (0, 0)  (0, 1)
    //  n e s w borders (west void for first row)
        >,v,_,_ _,v,>,_
    //  (1, 0)  (1, 1)
        v,_,_,> _,_,v,>
    );
    // the loop test
    // ─│─
    // ─┼┐
    // ─└┘
    test!("bXNjaAF4nDWK4QqAIBCDd6dE0SNGP8zuh2CeaAS9fZk0xvjGBgNjYJM7BDaqy5h3qb6EfAZNAIboNokVvKyE0Wu65NbyDhM+cQv6mTtTM/WFYfqLm6m3lx9MAg7n" =>
        >,^,_,_ <,>,<,_ _,v,>,_
        >,<,_,< v,v,^,> _,>,>,<
        v,_,_,^ >,_,<,> _,_,v,v
    );
    // the snek test
    // └┐
    // ─┘
    test!("bXNjaAF4nGNgYmBiZmDJS8xNZWApzkvNZuBOSS1OLsosKMnMz2NgYGDLSUxKzSlmYIqOZWTgSM7PK0utzC8CSrAwIAAjEIIQhGJkYAIARA0Ozg==" =>
        ^,^,_,_ _,<,>,_
        <,_,_,> _,_,^,^
    );

    // the notile test
    test!("bXNjaAF4nCWJQQqAIBREx69E0Lq994oWph8STEMj6fZpzcDjDQMCSahoDsZsdN1TYB25aucz28uniMlxsdmf3wCGYDYOBbSsAqNN8eYn5XYofJEdAtSB31tfaoIVGw==" =>
        <,>,_,_ _,^,v,_
        ^,_,_,v _,_,>,<
    );
    // the asymmetrical test
    // <───
    // ───>
    test!("bXNjaAF4nEXJwQqAIBAE0HGVCPrE6GC2B0HdcCPw78MKnMMwj4EFWbjiM8N5bRnLwRpqPK8oBcCU/M5JQetmMAcpNzep/cCIAfX69yv6RF0PFy0O4Q==" =>
        <,>,_,_ _,<,>,_
        <,>,_,> _,<,>,<
        // <,_,_,> _,_,>,<
        <,_,_,> _,_,>,<
    );

    // the complex test
    // ─┬─│││─
    // ─┤─┘─┘─
    // ─┤┌─│─┐
    // ─┼┘─┴─│
    test!("bXNjaAF4nEWOUQ7CIBBEh2VZTbyCx/A2xg9a+WiC0LTGxNvb7Wjk5wEzb7M4QCO05UdBqj3PF5zuZR2XaX5OvQGwmodSV8j1FnAce3uVd1+24Iz/CYQQ8fcVHYEQIjqEXWEm9LwgX9kR+PLSbm2BMlN6Sk/3LhJnJu6S6CVmxl2MntEzv38AchUPug==" =>
        >,v,_,_ >,v,>,_ >,v,>,_ _,v,>,_
        v,<,_,> v,v,v,> v,>,v,> _,<,v,>
        v,>,_,v >,<,<,v <,^,v,v _,v,>,v
        <,>,_,< ^,v,>,v v,>,<,> _,^,^,<
        v,<,_,> >,>,>,< ^,^,v,^ _,^,>,v
        >,v,_,> ^,v,<,v ^,>,>,> _,>,^,^
        // <,_,_,< ^,_,>,v v,_,<,> _,_,^,<
        // v,_,_,> >,_,>,< ^,_,v,^ _,_,>,v
        // >,_,_,> ^,_,<,v ^,_,>,> _,_,^,^
        v,_,_,< >,_,v,> >,_,v,^ _,_,>,^
    );
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
        dbg!(&self.blocks.clone());
        load_zip();
        // fill background
        let mut bg = RgbaImage::new(
            ((self.width + 2) * 32) as u32,
            ((self.height + 2) * 32) as u32,
        );
        bg.repeat(METAL_FLOOR.image(None, None).borrow());
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
                    rotation: tile.rot,
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
                tile.image(ctx.as_ref()).borrow(),
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
        let mut floor = RgbaImage::new(self.width as u32 * 8, self.height as u32 * 8);
        let mut top = RgbaImage::new(self.width as u32 * 8, self.height as u32 * 8);
        for (x, y, j, tile) in self.tiles.iter().enumerate().map(|(j, t)| {
            (
                (j % self.width),
                // flip y
                (self.height - (j / self.width)) - 1,
                j,
                t,
            )
        }) {
            if tile.build().is_none() {
                floor.overlay(
                    // SAFETY: [`load_raw`] forces nonzero image size
                    unsafe { &tile.image(None).own().scale(8) },
                    x as u32 * 8,
                    y as u32 * 8,
                );
            } else {
                let s = if let Some(build) = &tile.build() {
                    build.block.get_size()
                } else {
                    1
                };
                let x = x - ((s - 1) / 2) as usize;
                let y = y - (s / 2) as usize;
                let ctx = (|| {
                    let b = tile.build()?;
                    if !b.block.wants_context() {
                        return None;
                    }
                    let pctx = PositionContext {
                        position: GridPos(x, y),
                        width: self.width,
                        height: self.height,
                    };
                    let rctx = RenderingContext {
                        cross: self.cross(j, &pctx),
                        rotation: b.rotation,
                        position: pctx,
                    };
                    Some(rctx)
                })();
                top.overlay(
                    // SAFETY: tile.size can never be 0, and [`load_raw`] forces nonzero.
                    unsafe { &tile.image(ctx.as_ref()).own().scale(tile.size() as u32 * 8) },
                    x as u32 * 8,
                    y as u32 * 8,
                );
            }
        }
        image::imageops::overlay(&mut floor, top.shadow(), 0, 0);
        floor
    }
}
