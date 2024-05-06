use std::borrow::Cow;
use std::collections::HashMap;

use super::State;
use crate::unit::Type::*;
#[macro_export]
macro_rules! ratios {
    ([$($input:ident: $icnt:literal),*] => [$($output:ident: $ocnt:literal),*]) => {{
        #[allow(unused_imports)]
        use $crate::{fluid::Type::*, item::Type::*, block::ConstFrom};
        const I: &[($crate::block::ratios::Resource, ::std::primitive::f32)] = &[$(($crate::block::ratios::Resource::fro($input), $icnt as ::std::primitive::f32),)*];
        const O: &[($crate::block::ratios::Resource, ::std::primitive::f32)] = &[$(($crate::block::ratios::Resource::fro($output), $ocnt as ::std::primitive::f32),)*];
        $crate::block::ratios::Io::new(I, O)
    }};
    [$($input:ident: $cnt:literal),*] => {{
        #[allow(unused_imports)]
        use $crate::{fluid::Type::*, item::Type::*, block::ConstFrom};
        const I: &[($crate::block::ratios::Resource, ::std::primitive::f32)] = &[$(($crate::block::ratios::Resource::fro($input), $cnt as ::std::primitive::f32),)*];
        $crate::block::ratios::Io::new(I, &[][..])
    }}
}
pub use ratios;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Resource {
    Item(crate::item::Type),
    Fluid(crate::fluid::Type),
}

impl const super::ConstFrom<crate::item::Type> for Resource {
    fn fro(value: crate::item::Type) -> Self {
        Self::Item(value)
    }
}

impl const super::ConstFrom<crate::fluid::Type> for Resource {
    fn fro(value: crate::fluid::Type) -> Self {
        Self::Fluid(value)
    }
}

#[derive(Debug, Clone)]
pub struct Io {
    pub input: Cow<'static, [(Resource, f32)]>,
    pub output: Cow<'static, [(Resource, f32)]>,
}

impl PartialEq for Io {
    fn eq(&self, other: &Self) -> bool {
        let sort = |mut x: Vec<_>| {
            x.sort_by_key(|(x, _)| *x);
            x
        };
        sort(self.input.to_vec()) == sort(other.input.to_vec())
            && sort(self.output.to_vec()) == sort(other.output.to_vec())
    }
}

impl Io {
    fn none() -> Self {
        Self {
            input: Cow::from(&[][..]),
            output: Cow::from(&[][..]),
        }
    }

    pub fn new(input: &'static [(Resource, f32)], output: &'static [(Resource, f32)]) -> Self {
        Self {
            input: Cow::from(input),
            output: Cow::from(output),
        }
    }
}

#[derive(Default)]
pub(crate) struct IoBuilder {
    pub net: HashMap<Resource, f32>,
}

impl std::ops::AddAssign<Io> for IoBuilder {
    fn add_assign(&mut self, rhs: Io) {
        for &(res, n) in &*rhs.output {
            *self.net.entry(res).or_default() += n;
        }
        for &(res, n) in &*rhs.input {
            *self.net.entry(res).or_default() -= n;
        }
    }
}

impl From<IoBuilder> for Io {
    fn from(value: IoBuilder) -> Self {
        let (mut input, output): (Vec<_>, Vec<_>) = value
            .net
            .into_iter()
            .filter(|(_, n)| n.abs() > 0.001)
            .map(|(r, n)| (r, (n * 100.0 + 0.5).floor() / 100.0))
            .partition(|&(_, n)| n < 0.0);
        input.iter_mut().for_each(|(_, v)| *v = v.abs());
        Self {
            input: Cow::Owned(input),
            output: Cow::Owned(output),
        }
    }
}

pub trait Ratios {
    #[inline]
    #[must_use]
    #[allow(unused_variables)]
    fn io(&self, state: Option<&State>, name: &str) -> Io {
        Io::none()
    }
}

macro_rules! just {
    ($for:ident, [$($input:ident: $icnt:literal),*] => [$($output:ident: $ocnt:literal),*]) => {
        impl Ratios for super::$for {
            fn io(&self, _: Option<&State>, _: &str) -> Io {
                ratios!([$($input : $icnt),*] => [$($output : $ocnt),*])
            }
        }
    }
}

macro_rules! rats {
    ($for:ident { $($name:literal: [$($input:ident: $icnt:literal),*] => [$($output:ident: $ocnt:literal),*])+ }) => {
        impl Ratios for super::$for {
            fn io(&self, _: Option<&State>, name: &str) -> Io {
                $(_ = crate::data::renderer::load!($name);)+ // string validation
                match name {
                    $($name => ratios!([$($input : $icnt),*] => [$($output : $ocnt),*]),)+
                    _ => Io::none()
                }
            }
        }
    };
    ($for:ident { none }) => {
        impl Ratios for super::$for {}
    }
}

rats!(ConveyorBlock { none });
rats!(JunctionBlock { none });
rats!(StackConveyor { none });
rats!(BridgeBlock { none });
rats!(ItemBlock { none });
rats!(DuctBlock { none });
rats!(SimpleDuctBlock { none });
rats!(FluidBlock { none });
rats!(SurgeRouter { none });
rats!(ProductionBlock {
    "cultivator": [Water: 18] => [SporePod: 0.6]
    "graphite-press": [Coal: 1.33] => [Graphite: 0.66]
    "multi-press": [Coal: 6, Water: 6] => [Graphite: 4]
    "silicon-smelter": [Coal: 1.5, Sand: 3] => [Silicon: 1.5]
    "silicon-crucible": [Coal: 2.66, Sand: 4, Pyratite: 0.66] => [Silicon: 5.33]
    "kiln": [Lead: 2, Sand: 2] => [Metaglass: 2]
    "plastanium-compressor": [Oil: 15, Titanium: 2] => [Plastanium: 1]
    "phase-weaver": [Thorium: 2, Sand: 5] => [PhaseFabric: 0.5]
    "surge-smelter": [Copper: 2.4, Lead: 3.2, Titanium: 1.6, Silicon: 2.4] => [SurgeAlloy: 0.8]
    "cryofluid-mixer": [Titanium: 0.5, Water: 12] => [Cryofluid: 12]
    "pyratite-mixer": [Coal: 0.75, Lead: 1.5, Sand: 1.5] => [Pyratite: 0.75]
    "blast-mixer": [Pyratite: 0.75, SporePod: 0.75] => [BlastCompound: 0.75]
    "melter": [Scrap: 6] => [Slag: 12]
    "spore-press": [SporePod: 3] => [Oil: 18]
    "pulverizer": [Scrap: 1.5] => [Sand: 1.5]
    "coal-centrifuge": [Oil: 6] => [Coal: 2]
    "silicon-arc-furnace": [Graphite: 1.2, Sand: 4.8] => [Silicon: 4.8]
    "electrolyzer": [Water: 10] => [Ozone: 4, Hydrogen: 6]
    "atmospheric-concentrator": [] => [Nitrogen: 4]
    "carbide-crucible": [Tungsten: 0.88, Graphite: 1.33] => [Carbide: 0.44]
    "cyanogen-synthesizer": [Arkycite: 40, Graphite: 0.75] => [Cyanogen: 3]
    "phase-synthesizer": [Thorium: 1, Sand: 3, Ozone: 2] => [PhaseFabric: 0.5]
    "vent-condenser": [] => [Water: 30]
});
rats!(SeparatorBlock {
    "separator": [Slag: 4] => [Copper: 0.71, Lead: 0.43, Graphite: 0.28, Titanium: 0.28]
    "disassembler": [Slag: 7.2, Scrap: 4] => [Sand: 1.6, Graphite: 0.8, Titanium: 0.8, Thorium: 0.8]
});
rats!(HeatConduit { none });
rats!(HeatCrafter {
    "oxidation-chamber": [Ozone: 2, Beryllium: 0.5] => [Oxide: 0.5]
    "phase-heater": [PhaseFabric: 0.12] => []
});
rats!(WallBlock { none });
rats!(DoorBlock { none });
rats!(HeatedBlock { none }); // these arent erekir, btw
rats!(ConduitBlock { none });
impl Ratios for super::UnitFactory {
    fn io(&self, state: Option<&State>, name: &str) -> Io {
        state.map_or(Io::none(), |s| {
            Self::get_state(s).map_or(Io::none(), |t| match (name, t) {
                ("ground-factory", Dagger) => ratios![Silicon: 0.66, Lead: 0.66],
                ("ground-factory", Crawler) => ratios![Silicon: 0.8, Coal: 1],
                ("ground-factory", Nova) => ratios![Silicon: 0.75, Lead: 0.5, Titanium: 0.5],
                ("air-factory", Flare) => ratios![Silicon: 1],
                ("air-factory", Mono) => ratios![Silicon: 0.85, Lead: 0.42],
                ("naval-factory", Risso) => ratios![Silicon: 0.44, Metaglass: 0.77],
                ("naval-factory", Retusa) => ratios![Silicon: 0.3, Metaglass: 0.5, Titanium: 0.4],
                ("tank-fabricator", _) => ratios![Beryllium: 1.14, Silicon: 1.42],
                ("ship-fabricator", _) => ratios![Graphite: 1.25, Silicon: 1.75],
                ("mech-fabricator", _) => ratios![Beryllium: 1.25, Silicon: 1.75],
                (f, t) => unreachable!("{f}, {t:?}"),
            })
        })
    }
}
rats!(ConstructorBlock {
    "additive-reconstructor": [Silicon: 4, Graphite: 4] => []
    "multiplicative-reconstructor": [Silicon: 4.33, Titanium: 2.66, Metaglass: 1.33] => []
    "exponential-reconstructor": [Silicon: 9.44, Titanium: 8.33, Plastanium: 7.22, Cryofluid: 60] => []
    "tetrative-reconstructor": [Silicon: 4.16, Plastanium: 2.5, SurgeAlloy: 2.08, PhaseFabric: 1.45, Cryofluid: 180] => []
    "tank-refabricator": [Hydrogen: 3, Silicon: 1.33, Titanium: 1] => []
    "mech-refabricator": [Hydrogen: 3, Silicon: 1.11, Tungsten: 0.88] => []
    "ship-refabricator": [Hydrogen: 3, Silicon: 1.2, Tungsten: 0.8] => []
    "prime-refabricator": [Nitrogen: 10, Thorium: 1.33, Silicon: 1.66] => []
});
rats!(BasicBlock {
    "water-extractor": [] => [Water: 6.6]
    "unit-repair-tower": [Ozone: 3] => []
    "oil-extractor": [Water: 9, Sand: 1] => [] // *cough*
});
rats!(WallDrillBlock { none }); // more cough
rats!(DrillBlock {
    "laser-drill": [Water: 4.8] => []
    "blast-drill": [Water: 6] => []
    "large-plasma-bore": [Nitrogen: 3] => []
    "impact-drill": [Water: 12] => []
    "eruption-drill": [Hydrogen: 4] => []
});
rats!(MessageLogic { none });
rats!(SwitchLogic { none });
rats!(ProcessorLogic { "hyper-processor": [Cryofluid: 4.8] => [] });
rats!(MemoryBlock { none });
rats!(CanvasBlock { none });
rats!(LampBlock { none });
rats!(ConnectorBlock { none });
rats!(DiodeBlock { none });
just!(NuclearGeneratorBlock, [Thorium: 0.16, Cryofluid: 2.4] => []); // thoreactor
just!(ImpactReactorBlock, [BlastCompound: 0.42, Cryofluid: 15] => []);
just!(Neoplasia, [Arkycite: 80, Water: 10, PhaseFabric: 0.33] => [Neoplasm: 20]);
rats!(GeneratorBlock {
    "differential-generator": [Pyratite: 0.27, Cryofluid: 6] => []
    "turbine-condenser": [] => [Water: 5]
    "chemical-combustion-chamber": [Ozone: 2, Arkycite: 40] => []
    "pyrolysis-generator": [Slag: 20, Arkycite: 40] => [Water: 20]
    "flux-reactor": [Cyanogen: 9] => []
    "rtg-generator": [Thorium: 0.07] => []
});
rats!(ItemTurret { none }); // eh
rats!(TractorBeamTurret { none });
rats!(PointDefenseTurret { none });
rats!(ContinousTurret { none });
rats!(Turret { none });
rats!(ShieldBlock { none });
rats!(PayloadBlock { none });
rats!(PayloadConveyor { none });
rats!(SimplePayloadBlock { none });
rats!(PayloadRouter { none });
rats!(RadarBlock { none });
rats!(UnitCargoLoader { "unit-cargo-loader": [Nitrogen: 10] => [] });
rats!(AssemblerBlock { none });
rats!(RepairTurret { none });
rats!(AssemblerModule { none });
