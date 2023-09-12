use super::{get_num, Flow, LInstruction};
use crate::{
    executor::{Display, DisplayState, ExecutorContext},
    memory::{LAddress, LRegistry, LVar},
};
use enum_dispatch::enum_dispatch;
use fimg::Image;

#[enum_dispatch]
pub trait DrawInstruction<'v> {
    #[allow(unused_variables)]
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    );
}

#[derive(Debug)]
#[enum_dispatch(DrawInstruction)]
pub enum DrawInstr<'v> {
    DrawLine(DrawLine<'v>),
    DrawRectBordered(DrawRectBordered<'v>),
    DrawRectFilled(DrawRectFilled<'v>),
    DrawTriangle(DrawTriangle<'v>),
    Clear(Clear<'v>),
    SetColorDyn(SetColorDyn<'v>),
    SetColorConst(SetColorConst),
    SetStroke(SetStroke<'v>),
}

#[derive(Debug)]
pub struct Clear<'v> {
    pub r: LAddress<'v>,
    pub g: LAddress<'v>,
    pub b: LAddress<'v>,
    pub a: LAddress<'v>,
}

impl<'v> DrawInstruction<'v> for Clear<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, image: &mut Image<&mut [u8], 4>, _: &mut DisplayState) {
        macro_rules! u8 {
            ($v:ident) => {
                match mem.get(self.$v) {
                    LVar::Num(n) => n.round() as u8,
                    _ => return,
                }
            };
        }
        let (r, g, b, a) = (u8!(r), u8!(g), u8!(b), u8!(a));
        for [r2, g2, b2, a2] in image.chunked_mut() {
            (*r2, *b2, *g2, *a2) = (r, g, b, a);
        }
    }
}

#[derive(Debug)]
pub struct SetColorDyn<'v> {
    pub r: LAddress<'v>,
    pub g: LAddress<'v>,
    pub b: LAddress<'v>,
    pub a: LAddress<'v>,
}
impl<'v> DrawInstruction<'v> for SetColorDyn<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, _: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        macro_rules! u8 {
            ($v:ident) => {
                match mem.get(self.$v) {
                    LVar::Num(n) => n.round() as u8,
                    _ => return,
                }
            };
        }
        state.color = (u8!(r), u8!(g), u8!(b), u8!(a));
    }
}

#[derive(Debug)]
pub struct SetColorConst {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}
impl DrawInstruction<'_> for SetColorConst {
    fn draw(&self, _: &mut LRegistry<'_>, _: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        state.color = (self.r, self.g, self.b, self.a);
    }
}

#[derive(Debug)]
pub struct SetStroke<'v> {
    pub size: LAddress<'v>,
}
impl<'v> DrawInstruction<'v> for SetStroke<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, _: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        if let LVar::Num(n) = mem.get(self.size) {
            state.stroke = n;
        }
    }
}

pub type Point<'v> = (LAddress<'v>, LAddress<'v>);
#[rustfmt::skip]
macro_rules! point {
    ($mem:ident@$point:expr) => {{
        let LVar::Num(a) = $mem.get($point.0) else { return; };
        let LVar::Num(b) = $mem.get($point.1) else { return; };
        (a,b)
    }}
}

macro_rules! map {
    ($tup:expr, $fn:expr) => {{
        let (a, b) = $tup;
        ($fn(a), $fn(b))
    }};
}
#[derive(Debug)]
pub struct DrawLine<'v> {
    pub point_a: Point<'v>,
    pub point_b: Point<'v>,
}
impl<'v> DrawInstruction<'v> for DrawLine<'v> {
    #[allow(unused_variables)]
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        // i will happily ignore that stroke specifys the stroke of lines
        let a = map!(point!(mem@self.point_a), |n| n as i32);
        let b = map!(point!(mem@self.point_b), |n| n as i32);
        image.line(a, b, state.col());
    }
}

macro_rules! unbounded {
    ($img:ident @ $x:expr => $y:expr) => {
        $img.width() < $x || $img.height() < $y
    };
}

#[derive(Debug)]
pub struct DrawRectFilled<'v> {
    pub position: Point<'v>,
    pub width: LAddress<'v>,
    pub height: LAddress<'v>,
}
impl<'v> DrawInstruction<'v> for DrawRectFilled<'v> {
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let pos = map!(point!(mem@self.position), |n| n as u32);
        let width = get_num!(mem.get(self.width), or ret) as u32;
        let height = get_num!(mem.get(self.height), or ret) as u32;
        if unbounded!(image @ pos.0 + width => pos.1 + height) {
            return;
        }
        // SAFETY: bounds checked above
        unsafe { image.filled_box(pos, width, height, state.col()) };
    }
}

#[derive(Debug)]
pub struct DrawRectBordered<'v> {
    pub position: Point<'v>,
    pub width: LAddress<'v>,
    pub height: LAddress<'v>,
}

impl<'v> DrawInstruction<'v> for DrawRectBordered<'v> {
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        // happily ignoring that state specifies box stroke width
        let pos = map!(point!(mem@self.position), |n| n as u32);
        let width = get_num!(mem.get(self.width), or ret) as u32;
        let height = get_num!(mem.get(self.height), or ret) as u32;
        if unbounded!(image @ pos.0 + width => pos.1 + height) {
            return;
        }
        // SAFETY: bounds checked above
        unsafe { image.r#box(pos, width, height, state.col()) };
    }
}

#[derive(Debug)]
pub struct DrawTriangle<'v> {
    pub points: (Point<'v>, Point<'v>, Point<'v>),
}
impl<'v> DrawInstruction<'v> for DrawTriangle<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, i: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        let to32 = |n| n as f32;
        let (a, b, c) = (
            map!(point!(mem@self.points.0), to32),
            map!(point!(mem@self.points.1), to32),
            map!(point!(mem@self.points.2), to32),
        );
        if unbounded!(i @ a.0 as u32 => a.1 as u32)
            || unbounded!(i @ b.0 as u32 => b.1 as u32)
            || unbounded!(i @ c.0 as u32 => c.1 as u32)
        {
            return;
        }
        // SAFETY: bounds are checked
        unsafe { i.tri(a, b, c, state.col()) };
    }
}

#[derive(Debug)]
pub struct DrawFlush {
    pub(crate) display: Display,
}
impl LInstruction<'_> for DrawFlush {
    fn run<W: std::io::Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.flush(self.display);
        Flow::Continue
    }
}
