pub mod info;
pub mod printable;

/// kill me
pub fn ff(f: f64) -> String {
    let mut s = f.to_string().into_bytes();
    if let Some((dot, _)) = s.iter().enumerate().find(|&(_, b)| *b == b'.') {
        let mut real = 0;
        for b in &mut s[dot..].iter_mut().skip(1) {
            match b {
                _ if real > 4 => {
                    s.truncate(dot + real);
                    break;
                }
                b'1'..=b'9' => real += 1,
                _ => {
                    s.truncate(dot + real);
                    break;
                }
            }
        }
    }
    String::from_utf8(s).unwrap()
}
