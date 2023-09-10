//! contains types of types
use std::error::Error;

macro_rules! numeric_enum {
	($vis:vis enum $tname:ident for $numeric:ty | $error:ident {$($name:ident $(= $val:literal)?),* $(,)?}) =>
	{
		crate::content::numeric_enum!($vis enum $tname for $numeric | $error* {$($name $(= $val)?),*});

		impl std::fmt::Display for $error {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				write!(f, "no variant of {} for value {}", stringify!($tname), self.0)
			}
		}

		impl std::error::Error for $error {}
	};
	($vis:vis enum $tname:ident for $numeric:ty | $error:ident* {$($name:ident $(= $val:literal)?),* $(,)?}) =>
	{
		#[repr($numeric)]
		#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
		$vis enum $tname { $($name $(= $val)?,)+ }

		#[derive(Copy, Clone, Debug, Eq, PartialEq)]
		$vis struct $error($vis $numeric);

		impl TryFrom<$numeric> for $tname {
			type Error = $error;

			#[allow(non_upper_case_globals)]
			fn try_from(value: $numeric) -> Result<Self, $error> {
				$(const $name: $numeric = $tname::$name as $numeric;)+
				match value {
					$($name => Ok(Self::$name),)+
					_ => Err($error(value)),
				}
			}
		}

		impl From<$tname> for $numeric { fn from(value: $tname) -> $numeric { value as $numeric } }
	};
}

pub(crate) use numeric_enum;

macro_rules! content_enum {
	($vis:vis enum $tname:ident / $ctype:ident for u16 | $error:ident {$($val:literal),* $(,)?}) =>
	{
		paste::paste! {
		$crate::content::numeric_enum!($vis enum $tname for u16 | $error* {
			$([<$val:camel>]),*,
		});

		impl $crate::content::Content for $tname {
			fn get_type(&self) -> $crate::content::Type {
				$crate::content::Type::$ctype
			}

			fn get_id(&self) -> u16 {
				*self as u16
			}

			fn get_name(&self) -> &'static str {
				match self {
					$(Self::[<$val:camel>] => $val,)*
				}
			}
		}

		impl std::fmt::Display for $error {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				write!(f, "no content of type {} for value {}", stringify!($ctype), self.0)
			}
		}

		impl std::fmt::Display for $tname {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      	match self {
					$(Self::[<$val:camel>] => f.write_str(strconv::kebab2title!($val)),)*
				}
			}
	}

		impl std::error::Error for $error {}
	}
	};
}
pub(crate) use content_enum;

macro_rules! color_content_enum {
	($vis:vis enum $tname:ident / $ctype:ident for u16 | $error:ident {$($val:literal: $col:literal),* $(,)?}) =>
	{
		paste::paste! {
		$crate::content::content_enum!($vis enum $tname / $ctype for u16 | $error {
			$($val),*,
		});

        impl Type {
            #[must_use]
            pub const fn color(&self) -> (u8, u8, u8) {
                match &self {
                    $(Self::[<$val:camel>] => {
												let v = color_hex::color_from_hex!($col);
												(v[0], v[1], v[2])
                    },)*
                }
            }
        }
    }}
}
pub(crate) use color_content_enum;

numeric_enum! {
    pub enum Type for u8 | TryFromU8Error
    {
        Item = 0,
        Block = 1,
        // Mech = 2,
        Bullet = 3,
        Fluid = 4,
        Modifier = 5,
        Unit = 6,
        Weather = 7,
        // Effect = 8,
        Sector = 9,
        // Loadout = 10,
        // TypeId = 11,
        // Error = 12,
        Planet = 13,
        // Ammo = 14,
        Team = 15,
    }
}

macro_rules! gen_by_id {
    ($target:path, $id:expr) => {
        match <$target>::try_from($id) {
            Ok(v) => Ok(Box::new(v)),
            Err(e) => Err(Box::new(e)),
        }
    };
}

impl Type {
    pub fn get(self, id: u16) -> Result<Box<dyn Content>, Box<dyn Error>> {
        match self {
            Self::Item => gen_by_id!(crate::item::Type, id),
            Self::Block => gen_by_id!(crate::block::content::Type, id),
            Self::Fluid => gen_by_id!(crate::fluid::Type, id),
            Self::Modifier => gen_by_id!(crate::modifier::Type, id),
            Self::Unit => gen_by_id!(crate::unit::Type, id),
            Self::Team => gen_by_id!(crate::team::Team, id),
            _ => Ok(Box::new(Generic(self, id))),
        }
    }
}

pub trait Content {
    fn get_type(&self) -> Type;

    fn get_id(&self) -> u16;

    fn get_name(&self) -> &str;
}

struct Generic(Type, u16);

impl Content for Generic {
    fn get_type(&self) -> Type {
        self.0
    }

    fn get_id(&self) -> u16 {
        self.1
    }

    fn get_name(&self) -> &str {
        "<unknown>"
    }
}
