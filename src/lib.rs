#![no_std]

use core::ops::*;
use core::cmp::Ordering;
use core::fmt;
use core::num::ParseIntError;
use core::mem::transmute;

pub trait ModExp {
    fn modmul_assign(&mut self, b: &Self, m: &Self);
    fn modmul(&self, b: &Self, m: &Self) -> Self;
    fn modexp(&self, b: &Self, m: &Self) -> Self;
}

macro_rules! make_uN {
($uN:ident, $bits:expr) => {

    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    #[repr(align(16))]
    pub struct $uN {
        buf: [u128; $uN::QWORDS]
    }

    impl $uN {
        const QWORDS: usize = $bits / 128;
        const DWORDS: usize = $bits / 64;
        const BYTES: usize = $bits / 8;
        const BITS: usize = $bits;

        pub const ZERO: Self = $uN { buf: [0; $uN::QWORDS] };

        fn test_bit(&self, bit: usize) -> bool {
            self.buf()[bit / 8] & ((1 << (bit % 8)) as u8) != 0
        }

        fn _set_bit(mut self, bit: usize) -> Self {
            self.buf_mut()[bit / 8] |= (1 << (bit % 8)) as u8;
            self
        }

        fn buf(&self) -> &[u8; Self::BYTES] {
            unsafe { transmute(&self.buf) }
        }
        fn buf_mut(&mut self) -> &mut [u8; Self::BYTES] {
            unsafe { transmute(&mut self.buf) }
        }
        fn buf64(&self) -> &[u64; Self::DWORDS] {
            unsafe { transmute(&self.buf) }
        }
        fn buf64_mut(&mut self) -> &mut [u64; Self::DWORDS] {
            unsafe { transmute(&mut self.buf) }
        }
        fn buf128(&self) -> &[u128; Self::DWORDS] {
            unsafe { transmute(&self.buf) }
        }
        fn buf128_mut(&mut self) -> &mut [u128; Self::DWORDS] {
            unsafe { transmute(&mut self.buf) }
        }

        pub fn from_hex(mut lit: &str) -> Result<Self, ParseIntError> {
            let mut out: $uN = Self::ZERO;
            if let Some(pos) = lit.find(|c| c != '0') {
                lit = &lit[pos..]
            }

            let bytes = (lit.len() + 1) / 2;
            assert!(bytes <= $uN::BYTES);
            let mut i = bytes;
            if lit.len() % 2 == 1 {
                i -= 1;
                out.buf_mut()[i] = u8::from_str_radix(&lit[..1], 16)?;
                lit = &lit[1..];
            }
            while !lit.is_empty() {
                i -= 1;
                out.buf_mut()[i] = u8::from_str_radix(&lit[..2], 16)?;
                lit = &lit[2..];
            }
            Ok(out)
        }

        pub fn leading_zeros(&self) -> usize {
            let self_buf = self.buf64();
            let leading_zero_dwords = self.buf64().iter().rev().take_while(|&b| *b == 0).count();
            if leading_zero_dwords == $uN::DWORDS { return Self::BITS }

            let msdword_pos = $uN::DWORDS - leading_zero_dwords - 1;
            leading_zero_dwords * 64 + (self_buf[msdword_pos].leading_zeros() as usize)
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

    impl From<u64> for $uN {
        fn from(val: u64) -> Self {
            let mut out = $uN::ZERO;
            out.buf128_mut()[0] = val as u128;
            out
        }
    }
    
    impl From<u128> for $uN {
        fn from(val: u128) -> Self {
            let mut out = $uN::ZERO;
            out.buf128_mut()[0] = val;
            out
        }
    }
    
    impl From<usize> for $uN {
        fn from(val: usize) -> Self {
            let mut out = $uN::ZERO;
            out.buf128_mut()[0] = val as u128;
            out
        }
    }

    impl AddAssign<&Self> for $uN {
        fn add_assign(&mut self, other: &$uN) {
            let mut carry = false;
            
            for i in 0..$uN::QWORDS {
                let self_buf = self.buf128_mut();
                let other_buf = other.buf128();

                let a = self_buf[i];
                let b = other_buf[i];
                let (word1, carry1) = a.overflowing_add(b);
                let (word2, carry2) = word1.overflowing_add(carry as u128);
                self_buf[i] = word2;
                carry = carry1 | carry2;
            }
        }
    }

    impl Add<&Self> for $uN {
        type Output = $uN;

        fn add(mut self, other: &$uN) -> $uN {
            self += other;
            self
        }
    }

    impl SubAssign<&Self> for $uN {
        fn sub_assign(&mut self, other: &$uN) {
            let mut carry = false;

            for i in 0..$uN::QWORDS {
                let self_buf = self.buf128_mut();
                let other_buf = other.buf128();

                let a = self_buf[i];
                let b = other_buf[i];
                let (subbed1, carry_bool1) = a.overflowing_sub(b);
                let (subbed2, carry_bool2) = subbed1.overflowing_sub(carry as u128);
                self_buf[i] = subbed2;
                carry = carry_bool1 | carry_bool2;
            }
        }
    }

    impl Sub<&Self> for $uN {
        type Output = $uN;

        fn sub(mut self, other: &$uN) -> $uN {
            self -= other;
            self
        }
    }

    impl MulAssign<&Self> for $uN {
        fn mul_assign(&mut self, other: &$uN) {
            if self == &Self::ZERO || other == &Self::ZERO {
                *self = Self::ZERO;
                return
            }

            if self.buf64()[1..].iter().all(|x| *x == 0) && other.buf64()[1..].iter().all(|x| *x == 0) {
                // Small numbers can be computed directly
                let a = self.buf64()[0] as u128;
                let b = other.buf64()[0] as u128;
                let mut out = Self::ZERO;
                out.buf[0] = a * b;
                *self = out;
                return
            }

            let half_size = |num: &$uN| {
                let leading_zeros = num.buf().iter().rev().take_while(|&x| *x == 0).count();
                let sig_bytes = $uN::BYTES - leading_zeros;
                sig_bytes / 2
            };

            let split = |num: &$uN, size: usize| {
                let mut top: $uN = Self::ZERO;
                let mut bot: $uN = Self::ZERO;
                top.buf_mut()[0..$uN::BYTES - size].copy_from_slice(&num.buf()[size..]);
                bot.buf_mut()[0..size].copy_from_slice(&num.buf()[..size]);
                (bot, top)
            };

            let byteshift_left = |num: &$uN, bytes: usize| {
                let mut new: $uN = Self::ZERO;
                new.buf_mut()[bytes..].copy_from_slice(&num.buf()[..$uN::BYTES-bytes]);
                new
            };

            let size = half_size((&*self).max(other));
            let (a0, a1) = split(self, size);
            let (b0, b1) = split(other, size);

            // let z1 = (a0 + a1) * (b0 + b1) - (a0 * b0) - (a1 * b1);
            // let out = z2 at size*2 + z1 at size + z0
            let mut z1 = a0;
            let mut acc = b0 + &b1;
            z1 += &a1;
            z1 *= &acc;
            acc = a1 * &b1;
            z1 -= &acc;
            *self = byteshift_left(&acc, size * 2);
            acc = a0 * &b0;
            z1 -= &acc;
            *self += &byteshift_left(&z1, size);
            *self += &acc;
        }
    }

    impl Mul<&Self> for $uN {
        type Output = $uN;

        fn mul(mut self, other: &$uN) -> $uN {
            self *= other;
            self
        }
    }

    impl BitAnd for $uN {
        type Output = $uN;
        fn bitand(mut self, other: $uN) -> $uN {
            for i in 0..$uN::QWORDS {
                let self_buf = self.buf128_mut();
                let other_buf = other.buf128();

                self_buf[i] &= other_buf[i];
            }
            self
        }
    }

    impl BitOr for $uN {
        type Output = $uN;
        fn bitor(mut self, other: $uN) -> $uN {
            for i in 0..$uN::QWORDS {
                let self_buf = self.buf128_mut();
                let other_buf = other.buf128();

                self_buf[i] |= other_buf[i];
            }
            self
        }
    }

    impl ShlAssign<usize> for $uN {
        fn shl_assign(&mut self, mut amount: usize) {
            if amount % 8 == 0 {
                amount /= 8;
                let self_buf = self.buf_mut();
                unsafe {
                    use core::ptr;
                    ptr::copy(self_buf.as_ptr(), self_buf[amount..].as_mut_ptr(), $uN::BYTES - amount);
                    ptr::write_bytes(self_buf.as_mut_ptr(), 0, amount);
                }
            } else {
                *self <<= amount >> 3 << 3;

                let self_buf = self.buf128_mut();
                amount %= 8;
                for i in (1..$uN::QWORDS).rev() {
                    let dword = (self_buf[i-1] >> (128 - amount)) | (self_buf[i] << amount);
                    self_buf[i] = dword;
                }
                self_buf[0] <<= amount;
            }
        }
    }

    impl Shl<usize> for $uN {
        type Output = $uN;

        fn shl(mut self, amount: usize) -> $uN {
            self <<= amount;
            self
        }
    }

    impl ShrAssign<usize> for $uN {
        fn shr_assign(&mut self, mut amount: usize) {
            if amount % 8 == 0 {
                amount /= 8;
                let self_buf = self.buf_mut();
                unsafe {
                    use core::ptr;
                    ptr::copy(self_buf[amount..].as_ptr(), self_buf.as_mut_ptr(), $uN::BYTES - amount);
                    ptr::write_bytes(self_buf[$uN::BYTES - amount..].as_mut_ptr(), 0, amount);
                }
            } else {
                *self >>= amount >> 3 << 3;

                let self_buf = self.buf128_mut();
                amount %= 8;
                for i in 0..($uN::QWORDS-1) {
                    let dword = (self_buf[i] >> amount) | (self_buf[i+1] << (128 - amount));
                    self_buf[i] = dword;
                }
                self_buf[$uN::QWORDS-1] >>= amount;
            }
        }
    }

    impl Shr<usize> for $uN {
        type Output = $uN;

        fn shr(mut self, amount: usize) -> $uN {
            self >>= amount;
            self
        }
    }

    impl RemAssign<&Self> for $uN {
        fn rem_assign(&mut self, modulus: &$uN) {
            for _ in 0..8 {
                if &*self < &modulus {
                    return
                }
                *self -= modulus;
            }

            loop {
                if &*self < &modulus {
                    return
                }
                *self -= modulus;

                let self_sig_bits = $uN::BITS - self.leading_zeros();
                let mod_sig_bits = $uN::BITS - modulus.leading_zeros();
                if mod_sig_bits + 2 > self_sig_bits {
                    continue
                }
                let top_shift = self_sig_bits - (mod_sig_bits + 2);
                // extract mod_sig_bits + 2 bit from the top
                let top = (*self >> top_shift) % modulus;
                let bot = *self << ($uN::BITS - top_shift) >> ($uN::BITS - top_shift);

                *self = (top << top_shift) | bot;
            }
        }
    }

    impl Rem<&Self> for $uN {
        type Output = $uN;

        fn rem(mut self, modulus: &$uN) -> $uN {
            self %= modulus;
            self
        }
    }

    impl PartialEq for $uN {
        fn eq(&self, other: &Self) -> bool {
            let self_buf = self.buf64();
            let other_buf = other.buf64();

            for dw in 0..$uN::DWORDS {
                if self_buf[dw] != other_buf[dw] {
                    return false
                }
            }
            return true
        }
    }

    impl Eq for $uN {}

    impl Ord for $uN {
        fn cmp(&self, other: &Self) -> Ordering {
            let self_buf = self.buf128();
            let other_buf = other.buf128();

            for dw in (0..$uN::QWORDS).rev() {
                match self_buf[dw].cmp(&other_buf[dw]) {
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

}; ($uN:ident, $size:expr, expands to $next:ident) => {

    make_uN!($uN, $size);

    impl ModExp for $uN {
        fn modmul_assign(&mut self, b: &$uN, m: &$uN) { 
            let mut acc = $next::ZERO;
            acc.buf[..$uN::QWORDS].copy_from_slice(&self.buf);
            
            let mut op = $next::ZERO;
            // let acc = (exta * extb) % extm;
            op.buf[..$uN::QWORDS].copy_from_slice(&b.buf);
            acc *= &op;            

            op.buf[..$uN::QWORDS].copy_from_slice(&m.buf);
            acc %= &op;
            
            self.buf[..].copy_from_slice(&acc.buf[..$uN::QWORDS]);
        }

        fn modmul(&self, b: &$uN, m: &$uN) -> $uN {
            let mut out = *self;
            out.modmul_assign(b, m);
            out
        }

        fn modexp(&self, exp: &$uN, md: &$uN) -> $uN {
            let mut prod: $uN = 1usize.into();
            let mut base = *self;
            base %= md;
            for bit in 0..$uN::BITS {
                if exp.test_bit(bit) {
                    prod.modmul_assign(&base, md);
                }
                let base_old = base;
                base.modmul_assign(&base_old, md)
            }
            prod
        }
    }

}}


make_uN!(u256, 256, expands to u512);
make_uN!(u512, 512, expands to u1024);
make_uN!(u1024, 1024, expands to u2048);
make_uN!(u2048, 2048, expands to u4096);
make_uN!(u4096, 4096);


#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_display() {
        assert_eq!(format!("{:X}", u2048::from(0u64)), "0");
        assert_eq!(format!("{:X}", u2048::from(10u64)), "A");
        assert_eq!(format!("{:X}", u2048::from(0x1010u64)), "1010");
        assert_eq!(format!("{:X}", u2048::from(0x010101u64)), "10101");
    }

    #[test]
    fn test_from_hex() {
        assert_eq!(u2048::from_hex("1AB5932C").unwrap(), 0x1ab5932cu64.into());
        assert_eq!(u2048::from_hex("AB5932C").unwrap(), 0xab5932cu64.into());
        assert_eq!(u2048::from_hex("0").unwrap(), u2048::ZERO);
    }

    #[test]
    fn test_multiply() {
        let a = u2048::from_hex("8907415908475034928712983710").unwrap();
        let b = u2048::from_hex("0927096871290384928634897651").unwrap();
        assert_eq!(a * &b, u2048::from_hex("4e626703eab9a08bfcd04c1a645adb0d493136e95907053c31acc10").unwrap())
    }

    #[test]
    fn test_rem() {
        let a = u2048::from_hex("8907415908475034928712983710").unwrap();
        let b = u2048::from_hex("0927096871290384928634897651").unwrap();
        assert_eq!(a % &b, u2048::from_hex("8e4bda2d8091ef48f303313bea2").unwrap());

        let a = u2048::from_hex("8907415908475034928712983710").unwrap();
        let b = u2048::from_hex("908234").unwrap();
        assert_eq!(a % &b, 0x2b7d8cu64.into());

        let a: u2048 = u2048::ZERO;
        let b: u2048 = 32u64.into();
        assert_eq!(a % &b, u2048::ZERO);

        let a: u2048 = 32u64.into();
        let b: u2048 = 32u64.into();
        assert_eq!(a % &b, u2048::ZERO);
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
        assert_eq!(a.modmul(&b, &m), expected);
    }

    #[test]
    fn test_shift() {
        let a = u2048::ZERO;
        let a = a - &1u64.into();

        let out = a << 1023;
        assert!(out.buf()[128..].iter().all(|&x| x == 0xFF));
        assert_eq!(out.buf()[127], 0x80);
        assert!(out.buf()[..127].iter().all(|&x| x == 0));

        let out = out >> 1025;
        assert!(out.buf()[128..].iter().all(|&x| x == 0));
        assert_eq!(out.buf()[127], 0x7F);
        assert!(out.buf()[..127].iter().all(|&x| x == 0xFF));
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
        assert_eq!(a.modexp(&b, &m), u2048::from_hex("61747461636b206174206461776e").unwrap());
    }
}
