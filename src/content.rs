macro_rules!numeric_enum
{
	($vis:vis enum $tname:ident for $numeric:ty | $error:ident {$($name:ident $(= $val:literal)?),* $(,)?}) =>
	{
		#[repr($numeric)]
		#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
		$vis enum $tname
		{
			$($name $(= $val)?,)+
		}
		
		#[derive(Copy, Clone, Debug, Eq, PartialEq)]
		$vis struct $error($vis $numeric);
		
		impl TryFrom<$numeric> for $tname
		{
			type Error = $error;
			
			#[allow(non_upper_case_globals)]
			fn try_from(value: $numeric) -> Result<$tname, $error>
			{
				$(const $name: $numeric = $tname::$name as $numeric;)+
				match value
				{
					$($name => Ok($tname::$name),)+
					_ => Err($error(value)),
				}
			}
		}
		
		impl From<$tname> for $numeric
		{
			fn from(value: $tname) -> $numeric
			{
				value as $numeric
			}
		}
	};
}
pub(crate) use numeric_enum;

macro_rules!content_enum
{
	($vis:vis enum $tname:ident / $ctype:ident for u16 | $error:ident {$($name:ident $(= $val:literal)? => $vname:expr),* $(,)?}) =>
	{
		$crate::content::numeric_enum!($vis enum $tname for u16 | $error {$($name $(= $val)?),*});
		
		impl $crate::content::Content for $tname
		{
			fn get_type(&self) -> $crate::content::Type
			{
				$crate::content::Type::$ctype
			}
			
			fn get_id(&self) -> u16
			{
				*self as u16
			}
			
			fn get_name(&self) -> &'static str
			{
				match self
				{
					$($tname::$name => $vname,)*
				}
			}
		}
	};
}
pub(crate) use content_enum;

numeric_enum!
{
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

pub trait Content
{
	fn get_type(&self) -> Type;
	
	fn get_id(&self) -> u16;
	
	fn get_name(&self) -> &str;
}
