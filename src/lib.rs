extern crate core;

use core::ops::{Add, Mul, Rem, Sub, Shl, Shr, BitOr};
use core::cmp::Ordering;
use core::fmt;
use core::num::ParseIntError;

macro_rules! make_uN {
    ($uN:ident, $bits:expr) => {
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct $uN {
        buf: [u8; $uN::BYTES]
    }

    impl $uN {
        const BYTES: usize = $bits / 8;
        const BITS: usize = $bits;

        fn test_bit(&self, bit: usize) -> bool {
            self.buf[bit / 8] & ((1 << (bit % 8)) as u8) == 1
        }

        fn set_bit(mut self, bit: usize) -> Self {
            self.buf[bit / 8] |= (1 << (bit % 8)) as u8;
            self
        }

        pub fn from_hex(mut lit: &str) -> Result<Self, ParseIntError> {
            let mut out: $uN = 0.into();
            if let Some(pos) = lit.find(|c| c != '0') {
                lit = &lit[pos..]
            }

            let bytes = (lit.len() + 1) / 2;
            assert!(bytes <= $uN::BYTES);
            let mut i = bytes;
            if lit.len() % 2 == 1 {
                i -= 1;
                out.buf[i] = u8::from_str_radix(&lit[..1], 16)?;
                lit = &lit[1..];
            }
            while !lit.is_empty() {
                i -= 1;
                out.buf[i] = u8::from_str_radix(&lit[..2], 16)?;
                lit = &lit[2..];
            }
            Ok(out)
        }

        pub fn leading_zeros(&self) -> usize {
            let leading_zero_bytes = self.buf.iter().rev().take_while(|&b| *b == 0).count();
            if leading_zero_bytes == $uN::BYTES { return Self::BITS }
            let msbyte_pos = $uN::BYTES - leading_zero_bytes - 1;
            leading_zero_bytes * 8 + (self.buf[msbyte_pos].leading_zeros() as usize)
        }
    }

    impl fmt::UpperHex for $uN {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let mut iter = self.buf.iter().rev().skip_while(|&x| *x == 0);
            if let Some(x) = iter.next() {
                write!(f, "{:X}", x)?;
            } else {
                write!(f, "0")?;
            }
            for b in iter {
                write!(f, "{:02X}", b)?;
            }
            Ok(())
        }
    }

    impl fmt::LowerHex for $uN {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let mut iter = self.buf.iter().rev().skip_while(|&x| *x == 0);
            if let Some(x) = iter.next() {
                write!(f, "{:x}", x)?;
            } else {
                write!(f, "0")?;
            }
            for b in iter {
                write!(f, "{:02x}", b)?;
            }
            Ok(())
        }
    }

    impl fmt::Debug for $uN {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            <Self as fmt::LowerHex>::fmt(self, f)
        }
    }

    impl From<usize> for $uN {
        fn from(mut val: usize) -> Self {
            let mut out = $uN { buf: [0; $uN::BYTES] };
            let mut byte = 0;
            while val != 0 {
                out.buf[byte] = val as u8;
                byte += 1;
                val >>= 8;
            }
            out
        }
    }

    impl Add for $uN {
        type Output = $uN;

        fn add(self, other: $uN) -> $uN {
            let mut out = $uN {
                buf: [0; $uN::BYTES]
            };
            let mut carry = 0u16;
            for i in 0..$uN::BYTES {
                let a = self.buf[i];
                let b = other.buf[i];
                let word = (a as u16) + (b as u16) + carry;
                out.buf[i] = word as u8;
                carry = word >> 8;
            }
            out
        }
    }

    impl Sub for $uN {
        type Output = $uN;

        fn sub(self, other: $uN) -> $uN {
            let mut out = $uN {
                buf: [0; $uN::BYTES]
            };
            let mut carry = 0;
            for i in 0..$uN::BYTES {
                let a = self.buf[i];
                let b = other.buf[i];
                let (subbed, carry_bool) = (a as u16).overflowing_sub((b as u16) + carry);
                out.buf[i] = subbed as u8;
                carry = if carry_bool {1} else {0};
            }
            out
        }
    }

    impl Mul for $uN {
        type Output = $uN;

        fn mul(self, other: $uN) -> $uN {
            if self == 0.into() || other == 0.into() {
                return 0.into();
            }

            let cmp: $uN = (!0 >> 8*(::core::mem::size_of::<usize>() / 2)).into();
            if self < cmp && other < cmp {
                // Small numbers can be computed directly
                use core::mem::transmute_copy;
                unsafe {
                    let a: usize = transmute_copy(&self.buf);
                    let b: usize = transmute_copy(&other.buf);
                    return (a * b).into()
                }
            }

            let half_size = |num: $uN| {
                let leading_zeros = num.buf.iter().rev().take_while(|&x| *x == 0).count();
                let sig_bytes = $uN::BYTES - leading_zeros;
                sig_bytes / 2
            };

            let split = |num: $uN, size: usize| {
                let mut top: $uN = 0.into();
                let mut bot: $uN = 0.into();
                top.buf[0..$uN::BYTES - size].copy_from_slice(&num.buf[size..]);
                bot.buf[0..size].copy_from_slice(&num.buf[..size]);
                (bot, top)
            };

            let byteshift_left = |num: $uN, bytes: usize| {
                let mut new: $uN = 0.into();
                new.buf[bytes..].copy_from_slice(&num.buf[..$uN::BYTES-bytes]);
                new
            };

            let size = half_size(self.max(other));
            let (a0, a1) = split(self, size);
            let (b0, b1) = split(other, size);
            let z0 = a0 * b0;
            let z2 = a1 * b1;
            let z1 = (a0 + a1) * (b0 + b1) - z0 - z2;

            byteshift_left(z2, size * 2) + byteshift_left(z1, size) + z0
        }
    }

    impl BitOr for $uN {
        type Output = $uN;
        fn bitor(mut self, other: $uN) -> $uN {
            for i in 0..$uN::BYTES {
                self.buf[i] |= other.buf[i];
            }
            self
        }
    }

    impl Shl<usize> for $uN {
        type Output = $uN;

        fn shl(mut self, mut amount: usize) -> $uN {
            if amount % 8 == 0 {
                amount /= 8;
                let mut new: $uN = 0.into();
                new.buf[amount..].copy_from_slice(&self.buf[..$uN::BYTES - amount]);
                new
            } else {
                self = self << (amount >> 3 << 3);
                amount %= 8;
                for i in (1..$uN::BYTES).rev() {
                    let mut word = (self.buf[i-1] as u16) | ((self.buf[i] as u16) << 8);
                    self.buf[i] = (word << amount >> 8) as u8;
                }
                self.buf[0] <<= amount;
                self
            }
        }
    }

    impl Shr<usize> for $uN {
        type Output = $uN;

        fn shr(mut self, mut amount: usize) -> $uN {
            if amount % 8 == 0 {
                amount /= 8;
                let mut new: $uN = 0.into();
                new.buf[..$uN::BYTES - amount].copy_from_slice(&self.buf[amount..]);
                new
            } else {
                self = self >> (amount >> 3 << 3);
                amount %= 8;
                for i in 0..($uN::BYTES-1) {
                    let mut word = (self.buf[i] as u16) | ((self.buf[i+1] as u16) << 8);
                    self.buf[i] = (word >> amount) as u8;
                }
                self.buf[$uN::BYTES-1] >>= amount;
                self
            }
        }
    }

    impl Rem for $uN {
        type Output = $uN;

        fn rem(mut self, modulus: $uN) -> $uN {
            for _ in 0..8 {
                if self < modulus {
                    return self
                }
                self = self - modulus;
            }

            loop {
                if self < modulus {
                    return self
                }
                self = self - modulus;

                let self_sig_bits = $uN::BITS - self.leading_zeros();
                let mod_sig_bits = $uN::BITS - modulus.leading_zeros();
                if mod_sig_bits + 2 > self_sig_bits {
                    continue
                }
                let top_shift = self_sig_bits - (mod_sig_bits + 2);
                // extract mod_sig_bits + 2 bit from the top
                let top = (self >> top_shift) % modulus;
                let bot = self << ($uN::BITS - top_shift) >> ($uN::BITS - top_shift);

                self = (top << top_shift) | bot;
            }
        }
    }

    impl PartialEq for $uN {
        fn eq(&self, other: &Self) -> bool {
            for i in 0..$uN::BYTES {
                if self.buf[i] != other.buf[i] {
                    return false
                }
            }
            return true
        }
    }

    impl Eq for $uN {}

    impl Ord for $uN {
        fn cmp(&self, other: &Self) -> Ordering {
            for b in (0..$uN::BYTES).rev() {
                match self.buf[b].cmp(&other.buf[b]) {
                    Ordering::Equal => continue,
                    found_order => return found_order
                }
            }
            Ordering::Equal
        }
    }

    impl PartialOrd for $uN {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }
}}


make_uN!(u256, 256);
make_uN!(u512, 512);
make_uN!(u1024, 1024);
make_uN!(u2048, 2048);
make_uN!(u4096, 4096);

fn modmul2048(mut a: u2048, b: u2048, m: u2048) -> u2048 { 
    let mut exta: u4096 = 0.into();
    let mut extb: u4096 = 0.into();
    let mut extm: u4096 = 0.into();
    exta.buf[..u2048::BYTES].copy_from_slice(&a.buf);
    extb.buf[..u2048::BYTES].copy_from_slice(&b.buf);
    extm.buf[..u2048::BYTES].copy_from_slice(&m.buf);

    let res = (exta * extb) % extm;
    a.buf[..].copy_from_slice(&res.buf[..u2048::BYTES]);
    a
} 

// pub fn modexp2048(base: u2048, exp: &u2048, md: &u2048) -> u2048 {
//     let mut prod: u2048 = 1.into();
//     let mut bit_exp = base;
//     if exp.test_bit(0) {
//         prod = modmul2048(prod, bit_exp, *md);
//     }
//     for bit in 1..u2048::BITS {
//         bit_exp = modmul2048(bit_exp, bit_exp, *md);
//         if exp.test_bit(bit) {
//             prod = modmul2048(prod, bit_exp, *md);
//         }
//     }
//     prod
// }

pub fn modexp2048(mut base: u2048, exp: &u2048, md: u2048) -> u2048 {
    let mut prod: u2048 = 1.into();
    base = base % md;
    for bit in 1..u2048::BITS {
        if exp.test_bit(bit) {
           prod = modmul2048(prod, base, md);
        }
        base = modmul2048(base, base, md)
    }
    prod
}


#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_display() {
        assert_eq!(format!("{:X}", u2048::from(0)), "0");
        assert_eq!(format!("{:X}", u2048::from(10)), "A");
        assert_eq!(format!("{:X}", u2048::from(0x1010)), "1010");
        assert_eq!(format!("{:X}", u2048::from(0x010101)), "10101");
    }

    #[test]
    fn test_from_hex() {
        assert_eq!(u2048::from_hex("1AB5932C").unwrap(), 0x1ab5932c.into());
        assert_eq!(u2048::from_hex("AB5932C").unwrap(), 0xab5932c.into());
        assert_eq!(u2048::from_hex("0").unwrap(), 0.into());
    }

    #[test]
    fn test_multiply() {
        let a = u2048::from_hex("8907415908475034928712983710").unwrap();
        let b = u2048::from_hex("0927096871290384928634897651").unwrap();
        assert_eq!(a * b, u2048::from_hex("4e626703eab9a08bfcd04c1a645adb0d493136e95907053c31acc10").unwrap())
    }

    #[test]
    fn test_rem() {
        let a = u2048::from_hex("8907415908475034928712983710").unwrap();
        let b = u2048::from_hex("0927096871290384928634897651").unwrap();
        assert_eq!(a % b, u2048::from_hex("8e4bda2d8091ef48f303313bea2").unwrap());

        let a = u2048::from_hex("8907415908475034928712983710").unwrap();
        let b = u2048::from_hex("908234").unwrap();
        assert_eq!(a % b, 0x2b7d8c.into());

        let a: u2048 = 0.into();
        let b: u2048 = 32.into();
        assert_eq!(a % b, 0.into());

        let a: u2048 = 32.into();
        let b: u2048 = 32.into();
        assert_eq!(a % b, 0.into());
    }

    #[test]
    fn test_modmul() {
        let a = u2048::from_hex("31ea7606c2db2fdbb6e01aedfbb7d4143a5a7c6593fa116f87a7b5f72ffff0e01\
                                    b7d728dc64d981203a43c20d459d90ee3e74d0dd79ba1c535bc4d2396708b7380\
                                    bb60ba1d3eb261b968374b87cb5af1cc008303bdf465654509bce8fddad1c64dd\
                                    c3fbd411c7828949e2d1228218d9d1cb0b568af365760b21d369ac1d3678a").unwrap();
        let b = u2048::from_hex("7f6fea75cf31df573172fb15a80ee3a817dde7f87432bb9a2d258af688c45c141\
                                    45cebd06564c9af9525b641bb34708d738a1f49bab32eb0e74f8ae02ebfe89397\
                                    6a220e1b8df88445fb1e9c2fe4f11fd8c6cd42fef697c22bf3c2dd9dab6333aef\
                                    9b1cb4dc7dd4c371015f103d36561a5ce65f4ac5055403301a94c920f1401").unwrap();
        let m = u2048::from_hex("cfc733d3d62af11a935cbba777e3bf08262629284d84095aecf7dfc63efe38dd0\
                                    680a6972d677ff04cc3d2e7c84cb4463c528ebc87680623db37792f0315447b2a\
                                    5da8d229afa229af95b5249eba3c4b3ea726f54989b76cca9002ee10a383de28e\
                                    fe84ebc6f5b74e9f201fd1b9543679e4ef022bf728270b9687beb10599d55").unwrap();
        let expected = u2048::from_hex("3d82fe65b23afda644d20a778ab9f99bc6030333d7800921b49d66dc22809a2f4bec9\
                                           fb9dbd96add5286bc36694cd412dfd869fbde5d349c1073bbf89478fa521389dc7d27\
                                           87a1a23e2ba2eade4a4ad08e1eb4b01b87fb0fe72382a9868ec66bf96191d0f329d36\
                                           8ea3f43e51e054961cc4cfcc6fce7859ae268ba67fba889d4").unwrap();
        assert_eq!(modmul2048(a, b, m), expected);
    }

    #[test]
    fn test_shift() {
        let a = u2048 { buf: [0xFF; u2048::BYTES] };
        let out = a << 1023;
        assert!(out.buf[128..].iter().all(|&x| x == 0xFF));
        assert_eq!(out.buf[127], 0x80);
        assert!(out.buf[..127].iter().all(|&x| x == 0));

        let out = out >> 1025;
        assert!(out.buf[128..].iter().all(|&x| x == 0));
        assert_eq!(out.buf[127], 0x7F);
        assert!(out.buf[..127].iter().all(|&x| x == 0xFF));
    }

    #[test]
    fn test_modexp() {
        let a = u2048::from_hex("31ea7606c2db2fdbb6e01aedfbb7d4143a5a7c6593fa116f87a7b5f72ffff0e01\
                                    b7d728dc64d981203a43c20d459d90ee3e74d0dd79ba1c535bc4d2396708b7380\
                                    bb60ba1d3eb261b968374b87cb5af1cc008303bdf465654509bce8fddad1c64dd\
                                    c3fbd411c7828949e2d1228218d9d1cb0b568af365760b21d369ac1d3678a").unwrap();
        let b = u2048::from_hex("7f6fea75cf31df573172fb15a80ee3a817dde7f87432bb9a2d258af688c45c141\
                                    45cebd06564c9af9525b641bb34708d738a1f49bab32eb0e74f8ae02ebfe89397\
                                    6a220e1b8df88445fb1e9c2fe4f11fd8c6cd42fef697c22bf3c2dd9dab6333aef\
                                    9b1cb4dc7dd4c371015f103d36561a5ce65f4ac5055403301a94c920f1401").unwrap();
        let m = u2048::from_hex("cfc733d3d62af11a935cbba777e3bf08262629284d84095aecf7dfc63efe38dd0\
                                    680a6972d677ff04cc3d2e7c84cb4463c528ebc87680623db37792f0315447b2a\
                                    5da8d229afa229af95b5249eba3c4b3ea726f54989b76cca9002ee10a383de28e\
                                    fe84ebc6f5b74e9f201fd1b9543679e4ef022bf728270b9687beb10599d55").unwrap();
        assert_eq!(modexp2048(a, &b, m), u2048::from_hex("61747461636b206174206461776e").unwrap());
    }
}

