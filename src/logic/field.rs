use crate::content::numeric_enum;

numeric_enum! {
    pub enum LogicField for u8 | TryFromU8Error
    {
        TotalItems, FirstItem, TotalLiquids, TotalPower, ItemCapacity, LiquidCapacity, PowerCapacity, PowerNetCapacity, PowerNetStored, PowerNetIn,
        PowerNetOut, Ammo, AmmoCapacity, Health, MaxHealth, Heat, Efficiency, Progress, Timescale, Rotation, PosX, PosY, ShootX, ShootY, Size, Dead, Range,
        Shooting, Boosting, MineX, MineY, Mining, Speed, Team, Type, Flag, Controlled, Controller, Name, PayloadCount, PayloadType, Enabled, Shoot, ShootP,
        Config, Color
    }
}

impl LogicField {
    #[must_use]
    pub const fn is_readable(self) -> bool {
        use LogicField::{
            Ammo, AmmoCapacity, Boosting, Color, Controlled, Controller, Dead, Efficiency, Enabled,
            FirstItem, Flag, Health, Heat, ItemCapacity, LiquidCapacity, MaxHealth, MineX, MineY,
            Mining, Name, PayloadCount, PayloadType, PosX, PosY, PowerCapacity, PowerNetCapacity,
            PowerNetIn, PowerNetOut, PowerNetStored, Progress, Range, Rotation, ShootX, ShootY,
            Shooting, Size, Speed, Team, Timescale, TotalItems, TotalLiquids, TotalPower, Type,
        };
        matches!(
            self,
            TotalItems
                | FirstItem
                | TotalLiquids
                | TotalPower
                | ItemCapacity
                | LiquidCapacity
                | PowerCapacity
                | PowerNetCapacity
                | PowerNetStored
                | PowerNetIn
                | PowerNetOut
                | Ammo
                | AmmoCapacity
                | Health
                | MaxHealth
                | Heat
                | Efficiency
                | Progress
                | Timescale
                | Rotation
                | PosX
                | PosY
                | ShootX
                | ShootY
                | Size
                | Dead
                | Range
                | Shooting
                | Boosting
                | MineX
                | MineY
                | Mining
                | Speed
                | Team
                | Type
                | Flag
                | Controlled
                | Controller
                | Name
                | PayloadCount
                | PayloadType
                | Enabled
                | Color
        )
    }

    #[must_use]
    pub const fn is_writable(self) -> bool {
        use LogicField::{Color, Config, Enabled, Shoot, ShootP};
        matches!(self, Enabled | Shoot | ShootP | Config | Color)
    }
}
