//! OVER: the best data format.
#![feature
(
    
)]

#![allow
(
    unused_attributes,
    unused_imports,
    unused_macros,
    unused_unsafe,
)]
/*
pub mod _
{
    pub use std::_::{ * };
}

pub mod __
{
    use ::
    {
        *,
    };
    /*
    */
}
*/

pub mod arch
{
    pub use std::arch::{ * };
}

pub mod f32
{
    pub use std::f32::{ * };
}

pub mod f64
{
    pub use std::f64::{ * };
}

pub mod num
{
    pub use std::num::{ * };

    pub mod traits
    {
        use ::
        {
            *,
        };
        /*
        */

    }

    pub mod integers
    {
        use ::
        {
            *,
        };
        /*
        */

    }

    pub mod big
    {
        use ::
        {
            *,
        };
        /*
        */

    }

    pub mod rational
    {
        use ::
        {
            *,
        };
        /*
        */

    }
    
    
}

pub mod math
{
    /*!
    libm v0.2.0 */
    use ::
    {
        *,
    };
    /*
    //! libm in pure Rust
    #![deny(warnings)]
    #![no_std]
    #![cfg_attr(
        all(target_arch = "wasm32", feature = "unstable"),
        feature(core_intrinsics)
    )]
    #![allow(clippy::unreadable_literal)]
    #![allow(clippy::many_single_char_names)]
    #![allow(clippy::needless_return)]
    #![allow(clippy::int_plus_one)]
    #![allow(clippy::deprecated_cfg_attr)]
    #![allow(clippy::mixed_case_hex_literals)]
    #![allow(clippy::float_cmp)]
    #![allow(clippy::eq_op)]
    #![allow(clippy::assign_op_pattern)]

    mod math;

    use core::{f32, f64};

    pub use self::math::*;
    */
    pub mod lib
    {
        use ::
        {
            *,
        };
        /*
        */
        macro_rules! force_eval
        {
            ($e:expr) =>
            {
                unsafe
                {
                    ::ptr::read_volatile(&$e);
                }
            };
        }
        
        macro_rules! i
        {
            ($array:expr, $index:expr) => {
                unsafe { *$array.get_unchecked($index) }
            };
            ($array:expr, $index:expr, = , $rhs:expr) => {
                unsafe {
                    *$array.get_unchecked_mut($index) = $rhs;
                }
            };
            ($array:expr, $index:expr, += , $rhs:expr) => {
                unsafe {
                    *$array.get_unchecked_mut($index) += $rhs;
                }
            };
            ($array:expr, $index:expr, -= , $rhs:expr) => {
                unsafe {
                    *$array.get_unchecked_mut($index) -= $rhs;
                }
            };
            ($array:expr, $index:expr, &= , $rhs:expr) => {
                unsafe {
                    *$array.get_unchecked_mut($index) &= $rhs;
                }
            };
            ($array:expr, $index:expr, == , $rhs:expr) => {
                unsafe { *$array.get_unchecked_mut($index) == $rhs }
            };
        }

        macro_rules! llvm_intrinsically_optimized
        {
            (#[cfg($($clause:tt)*)] $e:expr) => {
                #[cfg(all(feature = "unstable", $($clause)*))]
                {
                    if true { // thwart the dead code lint
                        $e
                    }
                }
            };
        }

        // Public modules
        mod acos
        {
            use ::
            {
                *,
            };

            use super::sqrt;
            /*            
            */
            const PIO2_HI: f64 = 1.57079632679489655800e+00;
            const PIO2_LO: f64 = 6.12323399573676603587e-17;
            const PS0: f64 = 1.66666666666666657415e-01;
            const PS1: f64 = -3.25565818622400915405e-01;
            const PS2: f64 = 2.01212532134862925881e-01;
            const PS3: f64 = -4.00555345006794114027e-02;
            const PS4: f64 = 7.91534994289814532176e-04;
            const PS5: f64 = 3.47933107596021167570e-05;
            const QS1: f64 = -2.40339491173441421878e+00;
            const QS2: f64 = 2.02094576023350569471e+00;
            const QS3: f64 = -6.88283971605453293030e-01;
            const QS4: f64 = 7.70381505559019352791e-02;

            fn r(z: f64) -> f64
            {
                let p: f64 = z * (PS0 + z * (PS1 + z * (PS2 + z * (PS3 + z * (PS4 + z * PS5)))));
                let q: f64 = 1.0 + z * (QS1 + z * (QS2 + z * (QS3 + z * QS4)));
                p / q
            }

            /// Arccosine (f64) Computes the inverse cosine (arc cosine) of the input value.
            pub fn acos(x: f64) -> f64
            {
                let x1p_120f = f64::from_bits(0x3870000000000000); // 0x1p-120 === 2 ^ -120
                let z: f64;
                let w: f64;
                let s: f64;
                let c: f64;
                let df: f64;
                let hx: u32;
                let ix: u32;

                hx = (x.to_bits() >> 32) as u32;
                ix = hx & 0x7fffffff;
                /* |x| >= 1 or nan */
                if ix >= 0x3ff00000 {
                    let lx: u32 = x.to_bits() as u32;

                    if ((ix - 0x3ff00000) | lx) == 0 {
                        /* acos(1)=0, acos(-1)=pi */
                        if (hx >> 31) != 0 {
                            return 2. * PIO2_HI + x1p_120f;
                        }
                        return 0.;
                    }
                    return 0. / (x - x);
                }
                /* |x| < 0.5 */
                if ix < 0x3fe00000 {
                    if ix <= 0x3c600000 {
                        /* |x| < 2**-57 */
                        return PIO2_HI + x1p_120f;
                    }
                    return PIO2_HI - (x - (PIO2_LO - x * r(x * x)));
                }
                /* x < -0.5 */
                if (hx >> 31) != 0 {
                    z = (1.0 + x) * 0.5;
                    s = sqrt(z);
                    w = r(z) * s - PIO2_LO;
                    return 2. * (PIO2_HI - (s + w));
                }
                /* x > 0.5 */
                z = (1.0 - x) * 0.5;
                s = sqrt(z);
                // Set the low 4 bytes to zero
                df = f64::from_bits(s.to_bits() & 0xff_ff_ff_ff_00_00_00_00);

                c = (z - df * df) / (s + df);
                w = r(z) * s + c;
                2. * (df + w)
            }
        }

        mod acosf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::sqrtf::sqrtf;

            const PIO2_HI: f32 = 1.5707962513e+00; /* 0x3fc90fda */
            const PIO2_LO: f32 = 7.5497894159e-08; /* 0x33a22168 */
            const P_S0: f32 = 1.6666586697e-01;
            const P_S1: f32 = -4.2743422091e-02;
            const P_S2: f32 = -8.6563630030e-03;
            const Q_S1: f32 = -7.0662963390e-01;

            fn r(z: f32) -> f32 {
                let p = z * (P_S0 + z * (P_S1 + z * P_S2));
                let q = 1. + z * Q_S1;
                p / q
            }

            /// Arccosine (f32) | Computes the inverse cosine (arc cosine) of the input value.
            pub fn acosf(x: f32) -> f32
            {
                let x1p_120 = f32::from_bits(0x03800000); // 0x1p-120 === 2 ^ (-120)

                let z: f32;
                let w: f32;
                let s: f32;

                let mut hx = x.to_bits();
                let ix = hx & 0x7fffffff;
                /* |x| >= 1 or nan */
                if ix >= 0x3f800000 {
                    if ix == 0x3f800000 {
                        if (hx >> 31) != 0 {
                            return 2. * PIO2_HI + x1p_120;
                        }
                        return 0.;
                    }
                    return 0. / (x - x);
                }
                /* |x| < 0.5 */
                if ix < 0x3f000000 {
                    if ix <= 0x32800000 {
                        /* |x| < 2**-26 */
                        return PIO2_HI + x1p_120;
                    }
                    return PIO2_HI - (x - (PIO2_LO - x * r(x * x)));
                }
                /* x < -0.5 */
                if (hx >> 31) != 0 {
                    z = (1. + x) * 0.5;
                    s = sqrtf(z);
                    w = r(z) * s - PIO2_LO;
                    return 2. * (PIO2_HI - (s + w));
                }
                /* x > 0.5 */
                z = (1. - x) * 0.5;
                s = sqrtf(z);
                hx = s.to_bits();
                let df = f32::from_bits(hx & 0xfffff000);
                let c = (z - df * df) / (s + df);
                w = r(z) * s + c;
                2. * (df + w)
            }
        }

        mod acosh
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{log, log1p, sqrt};
            const LN2: f64 = 0.693147180559945309417232121458176568; /* 0x3fe62e42,  0xfefa39ef*/
            /// Inverse hyperbolic cosine (f64) | Calculates the inverse hyperbolic cosine of `x`.
            pub fn acosh(x: f64) -> f64
            {
                let u = x.to_bits();
                let e = ((u >> 52) as usize) & 0x7ff;

                /* x < 1 domain error is handled in the called functions */

                if e < 0x3ff + 1 {
                    /* |x| < 2, up to 2ulp error in [1,1.125] */
                    return log1p(x - 1.0 + sqrt((x - 1.0) * (x - 1.0) + 2.0 * (x - 1.0)));
                }
                if e < 0x3ff + 26 {
                    /* |x| < 0x1p26 */
                    return log(2.0 * x - 1.0 / (x + sqrt(x * x - 1.0)));
                }
                /* |x| >= 0x1p26 or nan */
                return log(x) + LN2;
            }
        }

        mod acoshf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{log1pf, logf, sqrtf};

            const LN2: f32 = 0.693147180559945309417232121458176568;

            /// Inverse hyperbolic cosine (f32) | Calculates the inverse hyperbolic cosine of `x`.
            pub fn acoshf(x: f32) -> f32
            {
                let u = x.to_bits();
                let a = u & 0x7fffffff;

                if a < 0x3f800000 + (1 << 23) {
                    /* |x| < 2, invalid if x < 1 or nan */
                    /* up to 2ulp error in [1,1.125] */
                    return log1pf(x - 1.0 + sqrtf((x - 1.0) * (x - 1.0) + 2.0 * (x - 1.0)));
                }
                if a < 0x3f800000 + (12 << 23) {
                    /* |x| < 0x1p12 */
                    return logf(2.0 * x - 1.0 / (x + sqrtf(x * x - 1.0)));
                }
                /* x >= 0x1p12 */
                return logf(x) + LN2;
            }
        }

        mod asin
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{fabs, get_high_word, get_low_word, sqrt, with_set_low_word};
            const PIO2_HI: f64 = 1.57079632679489655800e+00; /* 0x3FF921FB, 0x54442D18 */
            const PIO2_LO: f64 = 6.12323399573676603587e-17; /* 0x3C91A626, 0x33145C07 */
            /* coefficients for R(x^2) */
            const P_S0: f64 = 1.66666666666666657415e-01; /* 0x3FC55555, 0x55555555 */
            const P_S1: f64 = -3.25565818622400915405e-01; /* 0xBFD4D612, 0x03EB6F7D */
            const P_S2: f64 = 2.01212532134862925881e-01; /* 0x3FC9C155, 0x0E884455 */
            const P_S3: f64 = -4.00555345006794114027e-02; /* 0xBFA48228, 0xB5688F3B */
            const P_S4: f64 = 7.91534994289814532176e-04; /* 0x3F49EFE0, 0x7501B288 */
            const P_S5: f64 = 3.47933107596021167570e-05; /* 0x3F023DE1, 0x0DFDF709 */
            const Q_S1: f64 = -2.40339491173441421878e+00; /* 0xC0033A27, 0x1C8A2D4B */
            const Q_S2: f64 = 2.02094576023350569471e+00; /* 0x40002AE5, 0x9C598AC8 */
            const Q_S3: f64 = -6.88283971605453293030e-01; /* 0xBFE6066C, 0x1B8D0159 */
            const Q_S4: f64 = 7.70381505559019352791e-02; /* 0x3FB3B8C5, 0xB12E9282 */

            fn comp_r(z: f64) -> f64
            {
                let p = z * (P_S0 + z * (P_S1 + z * (P_S2 + z * (P_S3 + z * (P_S4 + z * P_S5)))));
                let q = 1.0 + z * (Q_S1 + z * (Q_S2 + z * (Q_S3 + z * Q_S4)));
                p / q
            }
            /// Arcsine (f64) | Computes the inverse sine (arc sine) of the argument `x`.
            pub fn asin(mut x: f64) -> f64
            {
                let z: f64;
                let r: f64;
                let s: f64;
                let hx: u32;
                let ix: u32;

                hx = get_high_word(x);
                ix = hx & 0x7fffffff;
                /* |x| >= 1 or nan */
                if ix >= 0x3ff00000 {
                    let lx: u32;
                    lx = get_low_word(x);
                    if ((ix - 0x3ff00000) | lx) == 0 {
                        /* asin(1) = +-pi/2 with inexact */
                        return x * PIO2_HI + f64::from_bits(0x3870000000000000);
                    } else {
                        return 0.0 / (x - x);
                    }
                }
                /* |x| < 0.5 */
                if ix < 0x3fe00000 {
                    /* if 0x1p-1022 <= |x| < 0x1p-26, avoid raising underflow */
                    if ix < 0x3e500000 && ix >= 0x00100000 {
                        return x;
                    } else {
                        return x + x * comp_r(x * x);
                    }
                }
                /* 1 > |x| >= 0.5 */
                z = (1.0 - fabs(x)) * 0.5;
                s = sqrt(z);
                r = comp_r(z);
                if ix >= 0x3fef3333 {
                    /* if |x| > 0.975 */
                    x = PIO2_HI - (2. * (s + s * r) - PIO2_LO);
                } else {
                    let f: f64;
                    let c: f64;
                    /* f+c = sqrt(z) */
                    f = with_set_low_word(s, 0);
                    c = (z - f * f) / (s + f);
                    x = 0.5 * PIO2_HI - (2.0 * s * r - (PIO2_LO - 2.0 * c) - (0.5 * PIO2_HI - 2.0 * f));
                }
                if hx >> 31 != 0 {
                    -x
                } else {
                    x
                }
            }
        }

        mod asinf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::fabsf::fabsf;
            use super::sqrt::sqrt;

            const PIO2: f64 = 1.570796326794896558e+00;

            /* coefficients for R(x^2) */
            const P_S0: f32 = 1.6666586697e-01;
            const P_S1: f32 = -4.2743422091e-02;
            const P_S2: f32 = -8.6563630030e-03;
            const Q_S1: f32 = -7.0662963390e-01;

            fn r(z: f32) -> f32 {
                let p = z * (P_S0 + z * (P_S1 + z * P_S2));
                let q = 1. + z * Q_S1;
                p / q
            }

            /// Arcsine (f32) | Computes the inverse sine (arc sine) of the argument `x`.
            pub fn asinf(mut x: f32) -> f32
            {
                let x1p_120 = f64::from_bits(0x3870000000000000); // 0x1p-120 === 2 ^ (-120)

                let hx = x.to_bits();
                let ix = hx & 0x7fffffff;

                if ix >= 0x3f800000 {
                    /* |x| >= 1 */
                    if ix == 0x3f800000 {
                        /* |x| == 1 */
                        return ((x as f64) * PIO2 + x1p_120) as f32; /* asin(+-1) = +-pi/2 with inexact */
                    }
                    return 0. / (x - x); /* asin(|x|>1) is NaN */
                }

                if ix < 0x3f000000 {
                    /* |x| < 0.5 */
                    /* if 0x1p-126 <= |x| < 0x1p-12, avoid raising underflow */
                    if (ix < 0x39800000) && (ix >= 0x00800000) {
                        return x;
                    }
                    return x + x * r(x * x);
                }

                /* 1 > |x| >= 0.5 */
                let z = (1. - fabsf(x)) * 0.5;
                let s = sqrt(z as f64);
                x = (PIO2 - 2. * (s + s * (r(z) as f64))) as f32;
                if (hx >> 31) != 0 {
                    -x
                } else {
                    x
                }
            }
        }

        mod asinh
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{log, log1p, sqrt};

            const LN2: f64 = 0.693147180559945309417232121458176568; /* 0x3fe62e42,  0xfefa39ef*/

            /* asinh(x) = sign(x)*log(|x|+sqrt(x*x+1)) ~= x - x^3/6 + o(x^5) */
            /// Inverse hyperbolic sine (f64) | Calculates the inverse hyperbolic sine of `x`.
            pub fn asinh(mut x: f64) -> f64
            {
                let mut u = x.to_bits();
                let e = ((u >> 52) as usize) & 0x7ff;
                let sign = (u >> 63) != 0;

                /* |x| */
                u &= (!0) >> 1;
                x = f64::from_bits(u);

                if e >= 0x3ff + 26 {
                    /* |x| >= 0x1p26 or inf or nan */
                    x = log(x) + LN2;
                } else if e >= 0x3ff + 1 {
                    /* |x| >= 2 */
                    x = log(2.0 * x + 1.0 / (sqrt(x * x + 1.0) + x));
                } else if e >= 0x3ff - 26 {
                    /* |x| >= 0x1p-26, up to 1.6ulp error in [0.125,0.5] */
                    x = log1p(x + x * x / (sqrt(x * x + 1.0) + 1.0));
                } else {
                    /* |x| < 0x1p-26, raise inexact if x != 0 */
                    let x1p120 = f64::from_bits(0x4770000000000000);
                    force_eval!(x + x1p120);
                }

                if sign {
                    -x
                } else {
                    x
                }
            }
        }

        mod asinhf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{log1pf, logf, sqrtf};
            const LN2: f32 = 0.693147180559945309417232121458176568;
            /* asinh(x) = sign(x)*log(|x|+sqrt(x*x+1)) ~= x - x^3/6 + o(x^5) */
            /// Inverse hyperbolic sine (f32) | Calculates the inverse hyperbolic sine of `x`.
            pub fn asinhf(mut x: f32) -> f32
            {
                let u = x.to_bits();
                let i = u & 0x7fffffff;
                let sign = (u >> 31) != 0;

                /* |x| */
                x = f32::from_bits(i);

                if i >= 0x3f800000 + (12 << 23) {
                    /* |x| >= 0x1p12 or inf or nan */
                    x = logf(x) + LN2;
                } else if i >= 0x3f800000 + (1 << 23) {
                    /* |x| >= 2 */
                    x = logf(2.0 * x + 1.0 / (sqrtf(x * x + 1.0) + x));
                } else if i >= 0x3f800000 - (12 << 23) {
                    /* |x| >= 0x1p-12, up to 1.6ulp error in [0.125,0.5] */
                    x = log1pf(x + x * x / (sqrtf(x * x + 1.0) + 1.0));
                } else {
                    /* |x| < 0x1p-12, raise inexact if x!=0 */
                    let x1p120 = f32::from_bits(0x7b800000);
                    force_eval!(x + x1p120);
                }

                if sign {
                    -x
                } else {
                    x
                }
            }
        }

        mod atan
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::fabs;

            const ATANHI: [f64; 4] =
            [
                4.63647609000806093515e-01, /* atan(0.5)hi 0x3FDDAC67, 0x0561BB4F */
                7.85398163397448278999e-01, /* atan(1.0)hi 0x3FE921FB, 0x54442D18 */
                9.82793723247329054082e-01, /* atan(1.5)hi 0x3FEF730B, 0xD281F69B */
                1.57079632679489655800e+00, /* atan(inf)hi 0x3FF921FB, 0x54442D18 */
            ];

            const ATANLO: [f64; 4] = [
                2.26987774529616870924e-17, /* atan(0.5)lo 0x3C7A2B7F, 0x222F65E2 */
                3.06161699786838301793e-17, /* atan(1.0)lo 0x3C81A626, 0x33145C07 */
                1.39033110312309984516e-17, /* atan(1.5)lo 0x3C700788, 0x7AF0CBBD */
                6.12323399573676603587e-17, /* atan(inf)lo 0x3C91A626, 0x33145C07 */
            ];

            const AT: [f64; 11] = [
                3.33333333333329318027e-01,  /* 0x3FD55555, 0x5555550D */
                -1.99999999998764832476e-01, /* 0xBFC99999, 0x9998EBC4 */
                1.42857142725034663711e-01,  /* 0x3FC24924, 0x920083FF */
                -1.11111104054623557880e-01, /* 0xBFBC71C6, 0xFE231671 */
                9.09088713343650656196e-02,  /* 0x3FB745CD, 0xC54C206E */
                -7.69187620504482999495e-02, /* 0xBFB3B0F2, 0xAF749A6D */
                6.66107313738753120669e-02,  /* 0x3FB10D66, 0xA0D03D51 */
                -5.83357013379057348645e-02, /* 0xBFADDE2D, 0x52DEFD9A */
                4.97687799461593236017e-02,  /* 0x3FA97B4B, 0x24760DEB */
                -3.65315727442169155270e-02, /* 0xBFA2B444, 0x2C6A6C2F */
                1.62858201153657823623e-02,  /* 0x3F90AD3A, 0xE322DA11 */
            ];

            /// Arctangent (f64) | Computes the inverse tangent (arc tangent) of the input value.
            pub fn atan(x: f64) -> f64
            {
                let mut x = x;
                let mut ix = (x.to_bits() >> 32) as u32;
                let sign = ix >> 31;
                ix &= 0x7fff_ffff;
                if ix >= 0x4410_0000 {
                    if x.is_nan() {
                        return x;
                    }

                    let z = ATANHI[3] + f64::from_bits(0x0380_0000); // 0x1p-120f
                    return if sign != 0 { -z } else { z };
                }

                let id = if ix < 0x3fdc_0000 {
                    /* |x| < 0.4375 */
                    if ix < 0x3e40_0000 {
                        /* |x| < 2^-27 */
                        if ix < 0x0010_0000 {
                            /* raise underflow for subnormal x */
                            force_eval!(x as f32);
                        }

                        return x;
                    }

                    -1
                } else {
                    x = fabs(x);
                    if ix < 0x3ff30000 {
                        /* |x| < 1.1875 */
                        if ix < 0x3fe60000 {
                            /* 7/16 <= |x| < 11/16 */
                            x = (2. * x - 1.) / (2. + x);
                            0
                        } else {
                            /* 11/16 <= |x| < 19/16 */
                            x = (x - 1.) / (x + 1.);
                            1
                        }
                    } else if ix < 0x40038000 {
                        /* |x| < 2.4375 */
                        x = (x - 1.5) / (1. + 1.5 * x);
                        2
                    } else {
                        /* 2.4375 <= |x| < 2^66 */
                        x = -1. / x;
                        3
                    }
                };

                let z = x * x;
                let w = z * z;
                /* break sum from i=0 to 10 AT[i]z**(i+1) into odd and even poly */
                let s1 = z * (AT[0] + w * (AT[2] + w * (AT[4] + w * (AT[6] + w * (AT[8] + w * AT[10])))));
                let s2 = w * (AT[1] + w * (AT[3] + w * (AT[5] + w * (AT[7] + w * AT[9]))));

                if id < 0 {
                    return x - x * (s1 + s2);
                }

                let z = i!(ATANHI, id as usize) - (x * (s1 + s2) - i!(ATANLO, id as usize) - x);

                if sign != 0 {
                    -z
                } else {
                    z
                }
            }
        }

        mod atan2
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::atan;
            use super::fabs;

            const PI: f64 = 3.1415926535897931160E+00; /* 0x400921FB, 0x54442D18 */
            const PI_LO: f64 = 1.2246467991473531772E-16; /* 0x3CA1A626, 0x33145C07 */

            /// Arctangent of y/x (f64) | Computes the inverse tangent (arc tangent) of `y/x`.
            pub fn atan2(y: f64, x: f64) -> f64
            {
                if x.is_nan() || y.is_nan() {
                    return x + y;
                }
                let mut ix = (x.to_bits() >> 32) as u32;
                let lx = x.to_bits() as u32;
                let mut iy = (y.to_bits() >> 32) as u32;
                let ly = y.to_bits() as u32;
                if ((ix.wrapping_sub(0x3ff00000)) | lx) == 0 {
                    /* x = 1.0 */
                    return atan(y);
                }
                let m = ((iy >> 31) & 1) | ((ix >> 30) & 2); /* 2*sign(x)+sign(y) */
                ix &= 0x7fffffff;
                iy &= 0x7fffffff;

                /* when y = 0 */
                if (iy | ly) == 0 {
                    return match m {
                        0 | 1 => y, /* atan(+-0,+anything)=+-0 */
                        2 => PI,    /* atan(+0,-anything) = PI */
                        _ => -PI,   /* atan(-0,-anything) =-PI */
                    };
                }
                /* when x = 0 */
                if (ix | lx) == 0 {
                    return if m & 1 != 0 { -PI / 2.0 } else { PI / 2.0 };
                }
                /* when x is INF */
                if ix == 0x7ff00000 {
                    if iy == 0x7ff00000 {
                        return match m {
                            0 => PI / 4.0,        /* atan(+INF,+INF) */
                            1 => -PI / 4.0,       /* atan(-INF,+INF) */
                            2 => 3.0 * PI / 4.0,  /* atan(+INF,-INF) */
                            _ => -3.0 * PI / 4.0, /* atan(-INF,-INF) */
                        };
                    } else {
                        return match m {
                            0 => 0.0,  /* atan(+...,+INF) */
                            1 => -0.0, /* atan(-...,+INF) */
                            2 => PI,   /* atan(+...,-INF) */
                            _ => -PI,  /* atan(-...,-INF) */
                        };
                    }
                }
                /* |y/x| > 0x1p64 */
                if ix.wrapping_add(64 << 20) < iy || iy == 0x7ff00000 {
                    return if m & 1 != 0 { -PI / 2.0 } else { PI / 2.0 };
                }

                /* z = atan(|y/x|) without spurious underflow */
                let z = if (m & 2 != 0) && iy.wrapping_add(64 << 20) < ix {
                    /* |y/x| < 0x1p-64, x<0 */
                    0.0
                } else {
                    atan(fabs(y / x))
                };
                match m {
                    0 => z,                /* atan(+,+) */
                    1 => -z,               /* atan(-,+) */
                    2 => PI - (z - PI_LO), /* atan(+,-) */
                    _ => (z - PI_LO) - PI, /* atan(-,-) */
                }
            }
        }

        mod atan2f
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::atanf;
            use super::fabsf;

            const PI: f32 = 3.1415927410e+00; /* 0x40490fdb */
            const PI_LO: f32 = -8.7422776573e-08; /* 0xb3bbbd2e */

            /// Arctangent of y/x (f32) | Computes the inverse tangent (arc tangent) of `y/x`.
            pub fn atan2f(y: f32, x: f32) -> f32
            {
                if x.is_nan() || y.is_nan() {
                    return x + y;
                }
                let mut ix = x.to_bits();
                let mut iy = y.to_bits();

                if ix == 0x3f800000 {
                    /* x=1.0 */
                    return atanf(y);
                }
                let m = ((iy >> 31) & 1) | ((ix >> 30) & 2); /* 2*sign(x)+sign(y) */
                ix &= 0x7fffffff;
                iy &= 0x7fffffff;

                /* when y = 0 */
                if iy == 0 {
                    return match m {
                        0 | 1 => y,   /* atan(+-0,+anything)=+-0 */
                        2 => PI,      /* atan(+0,-anything) = pi */
                        3 | _ => -PI, /* atan(-0,-anything) =-pi */
                    };
                }
                /* when x = 0 */
                if ix == 0 {
                    return if m & 1 != 0 { -PI / 2. } else { PI / 2. };
                }
                /* when x is INF */
                if ix == 0x7f800000 {
                    return if iy == 0x7f800000 {
                        match m {
                            0 => PI / 4.,           /* atan(+INF,+INF) */
                            1 => -PI / 4.,          /* atan(-INF,+INF) */
                            2 => 3. * PI / 4.,      /* atan(+INF,-INF)*/
                            3 | _ => -3. * PI / 4., /* atan(-INF,-INF)*/
                        }
                    } else {
                        match m {
                            0 => 0.,      /* atan(+...,+INF) */
                            1 => -0.,     /* atan(-...,+INF) */
                            2 => PI,      /* atan(+...,-INF) */
                            3 | _ => -PI, /* atan(-...,-INF) */
                        }
                    };
                }
                /* |y/x| > 0x1p26 */
                if (ix + (26 << 23) < iy) || (iy == 0x7f800000) {
                    return if m & 1 != 0 { -PI / 2. } else { PI / 2. };
                }

                /* z = atan(|y/x|) with correct underflow */
                let z = if (m & 2 != 0) && (iy + (26 << 23) < ix) {
                    /*|y/x| < 0x1p-26, x < 0 */
                    0.
                } else {
                    atanf(fabsf(y / x))
                };
                match m {
                    0 => z,                /* atan(+,+) */
                    1 => -z,               /* atan(-,+) */
                    2 => PI - (z - PI_LO), /* atan(+,-) */
                    _ => (z - PI_LO) - PI, /* case 3 */ /* atan(-,-) */
                }
            }
        }

        mod atanf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::fabsf;

            const ATAN_HI: [f32; 4] = [
                4.6364760399e-01, /* atan(0.5)hi 0x3eed6338 */
                7.8539812565e-01, /* atan(1.0)hi 0x3f490fda */
                9.8279368877e-01, /* atan(1.5)hi 0x3f7b985e */
                1.5707962513e+00, /* atan(inf)hi 0x3fc90fda */
            ];

            const ATAN_LO: [f32; 4] = [
                5.0121582440e-09, /* atan(0.5)lo 0x31ac3769 */
                3.7748947079e-08, /* atan(1.0)lo 0x33222168 */
                3.4473217170e-08, /* atan(1.5)lo 0x33140fb4 */
                7.5497894159e-08, /* atan(inf)lo 0x33a22168 */
            ];

            const A_T: [f32; 5] = [
                3.3333328366e-01,
                -1.9999158382e-01,
                1.4253635705e-01,
                -1.0648017377e-01,
                6.1687607318e-02,
            ];

            /// Arctangent (f32) | Computes the inverse tangent (arc tangent) of the input value.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn atanf(mut x: f32) -> f32
            {
                let x1p_120 = f32::from_bits(0x03800000); // 0x1p-120 === 2 ^ (-120)

                let z: f32;

                let mut ix = x.to_bits();
                let sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;

                if ix >= 0x4c800000 {
                    /* if |x| >= 2**26 */
                    if x.is_nan() {
                        return x;
                    }
                    z = ATAN_HI[3] + x1p_120;
                    return if sign { -z } else { z };
                }
                let id = if ix < 0x3ee00000 {
                    /* |x| < 0.4375 */
                    if ix < 0x39800000 {
                        /* |x| < 2**-12 */
                        if ix < 0x00800000 {
                            /* raise underflow for subnormal x */
                            force_eval!(x * x);
                        }
                        return x;
                    }
                    -1
                } else {
                    x = fabsf(x);
                    if ix < 0x3f980000 {
                        /* |x| < 1.1875 */
                        if ix < 0x3f300000 {
                            /*  7/16 <= |x| < 11/16 */
                            x = (2. * x - 1.) / (2. + x);
                            0
                        } else {
                            /* 11/16 <= |x| < 19/16 */
                            x = (x - 1.) / (x + 1.);
                            1
                        }
                    } else if ix < 0x401c0000 {
                        /* |x| < 2.4375 */
                        x = (x - 1.5) / (1. + 1.5 * x);
                        2
                    } else {
                        /* 2.4375 <= |x| < 2**26 */
                        x = -1. / x;
                        3
                    }
                };
                /* end of argument reduction */
                z = x * x;
                let w = z * z;
                /* break sum from i=0 to 10 aT[i]z**(i+1) into odd and even poly */
                let s1 = z * (A_T[0] + w * (A_T[2] + w * A_T[4]));
                let s2 = w * (A_T[1] + w * A_T[3]);
                if id < 0 {
                    return x - x * (s1 + s2);
                }
                let id = id as usize;
                let z = ATAN_HI[id] - ((x * (s1 + s2) - ATAN_LO[id]) - x);
                if sign {
                    -z
                } else {
                    z
                }
            }
        }

        mod atanh
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::log1p;

            /// Inverse hyperbolic tangent (f64) | Calculates the inverse hyperbolic tangent of `x`.
            pub fn atanh(x: f64) -> f64
            {
                let u = x.to_bits();
                let e = ((u >> 52) as usize) & 0x7ff;
                let sign = (u >> 63) != 0;

                /* |x| */
                let mut y = f64::from_bits(u & 0x7fff_ffff_ffff_ffff);

                if e < 0x3ff - 1 {
                    if e < 0x3ff - 32 {
                        /* handle underflow */
                        if e == 0 {
                            force_eval!(y as f32);
                        }
                    } else {
                        /* |x| < 0.5, up to 1.7ulp error */
                        y = 0.5 * log1p(2.0 * y + 2.0 * y * y / (1.0 - y));
                    }
                } else {
                    /* avoid overflow */
                    y = 0.5 * log1p(2.0 * (y / (1.0 - y)));
                }

                if sign {
                    -y
                } else {
                    y
                }
            }
        }

        mod atanhf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::log1pf;
            /// Inverse hyperbolic tangent (f32) | Calculates the inverse hyperbolic tangent of `x`.
            pub fn atanhf(mut x: f32) -> f32
            {
                let mut u = x.to_bits();
                let sign = (u >> 31) != 0;

                /* |x| */
                u &= 0x7fffffff;
                x = f32::from_bits(u);

                if u < 0x3f800000 - (1 << 23) {
                    if u < 0x3f800000 - (32 << 23) {
                        /* handle underflow */
                        if u < (1 << 23) {
                            force_eval!((x * x) as f32);
                        }
                    } else {
                        /* |x| < 0.5, up to 1.7ulp error */
                        x = 0.5 * log1pf(2.0 * x + 2.0 * x * x / (1.0 - x));
                    }
                } else {
                    /* avoid overflow */
                    x = 0.5 * log1pf(2.0 * (x / (1.0 - x)));
                }

                if sign {
                    -x
                } else {
                    x
                }
            }
        }

        mod cbrt
        {
            use ::
            {
                *,
            };
            /*
            */
            const B1: u32 = 715094163; /* B1 = (1023-1023/3-0.03306235651)*2**20 */
            const B2: u32 = 696219795; /* B2 = (1023-1023/3-54/3-0.03306235651)*2**20 */

            /* |1/cbrt(x) - p(x)| < 2**-23.5 (~[-7.93e-8, 7.929e-8]). */
            const P0: f64 = 1.87595182427177009643; /* 0x3ffe03e6, 0x0f61e692 */
            const P1: f64 = -1.88497979543377169875; /* 0xbffe28e0, 0x92f02420 */
            const P2: f64 = 1.621429720105354466140; /* 0x3ff9f160, 0x4a49d6c2 */
            const P3: f64 = -0.758397934778766047437; /* 0xbfe844cb, 0xbee751d9 */
            const P4: f64 = 0.145996192886612446982; /* 0x3fc2b000, 0xd4e4edd7 */

            // Cube root (f64) | Computes the cube root of the argument.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn cbrt(x: f64) -> f64
            {
                let x1p54 = f64::from_bits(0x4350000000000000); // 0x1p54 === 2 ^ 54

                let mut ui: u64 = x.to_bits();
                let mut r: f64;
                let s: f64;
                let mut t: f64;
                let w: f64;
                let mut hx: u32 = (ui >> 32) as u32 & 0x7fffffff;

                if hx >= 0x7ff00000 {
                    /* cbrt(NaN,INF) is itself */
                    return x + x;
                }
                
                if hx < 0x00100000 {
                    /* zero or subnormal? */
                    ui = (x * x1p54).to_bits();
                    hx = (ui >> 32) as u32 & 0x7fffffff;
                    if hx == 0 {
                        return x; /* cbrt(0) is itself */
                    }
                    hx = hx / 3 + B2;
                } else {
                    hx = hx / 3 + B1;
                }
                ui &= 1 << 63;
                ui |= (hx as u64) << 32;
                t = f64::from_bits(ui);
                
                r = (t * t) * (t / x);
                t = t * ((P0 + r * (P1 + r * P2)) + ((r * r) * r) * (P3 + r * P4));
                
                ui = t.to_bits();
                ui = (ui + 0x80000000) & 0xffffffffc0000000;
                t = f64::from_bits(ui);

                /* one step Newton iteration to 53 bits with error < 0.667 ulps */
                s = t * t; /* t*t is exact */
                r = x / s; /* error <= 0.5 ulps; |r| < |t| */
                w = t + t; /* t+t is exact */
                r = (r - t) / (w + r); /* r-t is exact; w+r ~= 3*t */
                t = t + t * r; /* error <= 0.5 + 0.5/3 + epsilon */
                t
            }
        }

        mod cbrtf
        {
            use ::
            {
                *,
            };
            /*
            */
            const B1: u32 = 709958130; /* B1 = (127-127.0/3-0.03306235651)*2**23 */
            const B2: u32 = 642849266; /* B2 = (127-127.0/3-24/3-0.03306235651)*2**23 */

            /// Cube root (f32) | Computes the cube root of the argument.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn cbrtf(x: f32) -> f32 {
                let x1p24 = f32::from_bits(0x4b800000); // 0x1p24f === 2 ^ 24

                let mut r: f64;
                let mut t: f64;
                let mut ui: u32 = x.to_bits();
                let mut hx: u32 = ui & 0x7fffffff;

                if hx >= 0x7f800000 {
                    /* cbrt(NaN,INF) is itself */
                    return x + x;
                }

                /* rough cbrt to 5 bits */
                if hx < 0x00800000 {
                    /* zero or subnormal? */
                    if hx == 0 {
                        return x; /* cbrt(+-0) is itself */
                    }
                    ui = (x * x1p24).to_bits();
                    hx = ui & 0x7fffffff;
                    hx = hx / 3 + B2;
                } else {
                    hx = hx / 3 + B1;
                }
                ui &= 0x80000000;
                ui |= hx;

                /*
                * First step Newton iteration (solving t*t-x/t == 0) to 16 bits.  In
                * double precision so that its terms can be arranged for efficiency
                * without causing overflow or underflow.
                */
                t = f32::from_bits(ui) as f64;
                r = t * t * t;
                t = t * (x as f64 + x as f64 + r) / (x as f64 + r + r);

                /*
                * Second step Newton iteration to 47 bits.  In double precision for
                * efficiency and accuracy.
                */
                r = t * t * t;
                t = t * (x as f64 + x as f64 + r) / (x as f64 + r + r);

                /* rounding to 24 bits is perfect in round-to-nearest mode */
                t as f32
            }
        }

        mod ceil
        {
            use ::
            {
                *,
            };
            /*
            */
            const TOINT: f64 = 1. / f64::EPSILON;

            /// Ceil (f64) | Finds the nearest integer greater than or equal to `x`.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn ceil(x: f64) -> f64
            {
                let u: u64 = x.to_bits();
                let e: i64 = (u >> 52 & 0x7ff) as i64;
                let y: f64;

                if e >= 0x3ff + 52 || x == 0. {
                    return x;
                }
                // y = int(x) - x, where int(x) is an integer neighbor of x
                y = if (u >> 63) != 0 {
                    x - TOINT + TOINT - x
                } else {
                    x + TOINT - TOINT - x
                };
                // special case because of non-nearest rounding modes
                if e < 0x3ff {
                    force_eval!(y);
                    return if (u >> 63) != 0 { -0. } else { 1. };
                }
                if y < 0. {
                    x + y + 1.
                } else {
                    x + y
                }
            }
        }

        mod ceilf
        {
            use ::
            {
                *,
            };
            /*
            */
            /// Ceil (f32) | Finds the nearest integer greater than or equal to `x`.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn ceilf(x: f32) -> f32
            {
                let mut ui = x.to_bits();
                let e = (((ui >> 23) & 0xff).wrapping_sub(0x7f)) as i32;

                if e >= 23 {
                    return x;
                }
                if e >= 0 {
                    let m = 0x007fffff >> e;
                    if (ui & m) == 0 {
                        return x;
                    }
                    force_eval!(x + f32::from_bits(0x7b800000));
                    if ui >> 31 == 0 {
                        ui += m;
                    }
                    ui &= !m;
                } else {
                    force_eval!(x + f32::from_bits(0x7b800000));
                    if ui >> 31 != 0 {
                        return -0.0;
                    } else if ui << 1 != 0 {
                        return 1.0;
                    }
                }
                f32::from_bits(ui)
            }
        }

        mod copysign
        {
            use ::
            {
                *,
            };
            /*
            */
            /// Sign of Y, magnitude of X (f64).
            pub fn copysign(x: f64, y: f64) -> f64
             {
                let mut ux = x.to_bits();
                let uy = y.to_bits();
                ux &= (!0) >> 1;
                ux |= uy & (1 << 63);
                f64::from_bits(ux)
            }
        }

        mod copysignf
        {
            use ::
            {
                *,
            };
            /*
            */
            /// Sign of Y, magnitude of X (f32).
            pub fn copysignf(x: f32, y: f32) -> f32
            {
                let mut ux = x.to_bits();
                let uy = y.to_bits();
                ux &= 0x7fffffff;
                ux |= uy & 0x80000000;
                f32::from_bits(ux)
            }
        }

        mod cos
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{k_cos, k_sin, rem_pio2};

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn cos(x: f64) -> f64
            {
                let ix = (f64::to_bits(x) >> 32) as u32 & 0x7fffffff;

                /* |x| ~< pi/4 */
                if ix <= 0x3fe921fb {
                    if ix < 0x3e46a09e {
                        /* if x < 2**-27 * sqrt(2) */
                        /* raise inexact if x != 0 */
                        if x as i32 == 0 {
                            return 1.0;
                        }
                    }
                    return k_cos(x, 0.0);
                }

                /* cos(Inf or NaN) is NaN */
                if ix >= 0x7ff00000 {
                    return x - x;
                }

                /* argument reduction needed */
                let (n, y0, y1) = rem_pio2(x);
                match n & 3 {
                    0 => k_cos(y0, y1),
                    1 => -k_sin(y0, y1, 1),
                    2 => -k_cos(y0, y1),
                    _ => k_sin(y0, y1, 1),
                }
            }
        }

        mod cosf
        {
            use ::
            {
                f64::consts::FRAC_PI_2,
                *,
            };
            /*
            */
            use super::{k_cosf, k_sinf, rem_pio2f};
            /* Small multiples of pi/2 rounded to double precision. */
            const C1_PIO2: f64 = 1. * FRAC_PI_2; /* 0x3FF921FB, 0x54442D18 */
            const C2_PIO2: f64 = 2. * FRAC_PI_2; /* 0x400921FB, 0x54442D18 */
            const C3_PIO2: f64 = 3. * FRAC_PI_2; /* 0x4012D97C, 0x7F3321D2 */
            const C4_PIO2: f64 = 4. * FRAC_PI_2; /* 0x401921FB, 0x54442D18 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn cosf(x: f32) -> f32
            {
                let x64 = x as f64;

                let x1p120 = f32::from_bits(0x7b800000); // 0x1p120f === 2 ^ 120

                let mut ix = x.to_bits();
                let sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;

                if ix <= 0x3f490fda {
                    /* |x| ~<= pi/4 */
                    if ix < 0x39800000 {
                        /* |x| < 2**-12 */
                        /* raise inexact if x != 0 */
                        force_eval!(x + x1p120);
                        return 1.;
                    }
                    return k_cosf(x64);
                }
                if ix <= 0x407b53d1 {
                    /* |x| ~<= 5*pi/4 */
                    if ix > 0x4016cbe3 {
                        /* |x|  ~> 3*pi/4 */
                        return -k_cosf(if sign { x64 + C2_PIO2 } else { x64 - C2_PIO2 });
                    } else if sign {
                        return k_sinf(x64 + C1_PIO2);
                    } else {
                        return k_sinf(C1_PIO2 - x64);
                    }
                }
                if ix <= 0x40e231d5 {
                    /* |x| ~<= 9*pi/4 */
                    if ix > 0x40afeddf {
                        /* |x| ~> 7*pi/4 */
                        return k_cosf(if sign { x64 + C4_PIO2 } else { x64 - C4_PIO2 });
                    } else if sign {
                        return k_sinf(-x64 - C3_PIO2);
                    } else {
                        return k_sinf(x64 - C3_PIO2);
                    }
                }

                /* cos(Inf or NaN) is NaN */
                if ix >= 0x7f800000 {
                    return x - x;
                }

                /* general argument reduction needed */
                let (n, y) = rem_pio2f(x);
                match n & 3 {
                    0 => k_cosf(y),
                    1 => k_sinf(-y),
                    2 => -k_cosf(y),
                    _ => k_sinf(y),
                }
            }
        }

        mod cosh
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::exp;
            use super::expm1;
            use super::k_expo2;

            /// Hyperbolic cosine (f64)
            ///
            /// Computes the hyperbolic cosine of the argument x.
            /// Is defined as `(exp(x) + exp(-x))/2`
            /// Angles are specified in radians.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn cosh(mut x: f64) -> f64 {
                /* |x| */
                let mut ix = x.to_bits();
                ix &= 0x7fffffffffffffff;
                x = f64::from_bits(ix);
                let w = ix >> 32;

                /* |x| < log(2) */
                if w < 0x3fe62e42 {
                    if w < 0x3ff00000 - (26 << 20) {
                        let x1p120 = f64::from_bits(0x4770000000000000);
                        force_eval!(x + x1p120);
                        return 1.;
                    }
                    let t = expm1(x); // exponential minus 1
                    return 1. + t * t / (2. * (1. + t));
                }

                /* |x| < log(DBL_MAX) */
                if w < 0x40862e42 {
                    let t = exp(x);
                    /* note: if x>log(0x1p26) then the 1/t is not needed */
                    return 0.5 * (t + 1. / t);
                }

                /* |x| > log(DBL_MAX) or nan */
                k_expo2(x)
            }
        }

        mod coshf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::expf;
            use super::expm1f;
            use super::k_expo2f;

            /// Hyperbolic cosine (f64) | Computes the hyperbolic cosine of the argument x.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn coshf(mut x: f32) -> f32
            {
                let x1p120 = f32::from_bits(0x7b800000); // 0x1p120f === 2 ^ 120

                /* |x| */
                let mut ix = x.to_bits();
                ix &= 0x7fffffff;
                x = f32::from_bits(ix);
                let w = ix;

                /* |x| < log(2) */
                if w < 0x3f317217 {
                    if w < (0x3f800000 - (12 << 23)) {
                        force_eval!(x + x1p120);
                        return 1.;
                    }
                    let t = expm1f(x);
                    return 1. + t * t / (2. * (1. + t));
                }

                /* |x| < log(FLT_MAX) */
                if w < 0x42b17217 {
                    let t = expf(x);
                    return 0.5 * (t + 1. / t);
                }

                /* |x| > log(FLT_MAX) or nan */
                k_expo2f(x)
            }
        }

        mod erf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{exp, fabs, get_high_word, with_set_low_word};

            const ERX: f64 = 8.45062911510467529297e-01; /* 0x3FEB0AC1, 0x60000000 */
            /*
            * Coefficients for approximation to  erf on [0,0.84375]
            */
            const EFX8: f64 = 1.02703333676410069053e+00; /* 0x3FF06EBA, 0x8214DB69 */
            const PP0: f64 = 1.28379167095512558561e-01; /* 0x3FC06EBA, 0x8214DB68 */
            const PP1: f64 = -3.25042107247001499370e-01; /* 0xBFD4CD7D, 0x691CB913 */
            const PP2: f64 = -2.84817495755985104766e-02; /* 0xBF9D2A51, 0xDBD7194F */
            const PP3: f64 = -5.77027029648944159157e-03; /* 0xBF77A291, 0x236668E4 */
            const PP4: f64 = -2.37630166566501626084e-05; /* 0xBEF8EAD6, 0x120016AC */
            const QQ1: f64 = 3.97917223959155352819e-01; /* 0x3FD97779, 0xCDDADC09 */
            const QQ2: f64 = 6.50222499887672944485e-02; /* 0x3FB0A54C, 0x5536CEBA */
            const QQ3: f64 = 5.08130628187576562776e-03; /* 0x3F74D022, 0xC4D36B0F */
            const QQ4: f64 = 1.32494738004321644526e-04; /* 0x3F215DC9, 0x221C1A10 */
            const QQ5: f64 = -3.96022827877536812320e-06; /* 0xBED09C43, 0x42A26120 */
            /*
            * Coefficients for approximation to  erf  in [0.84375,1.25]
            */
            const PA0: f64 = -2.36211856075265944077e-03; /* 0xBF6359B8, 0xBEF77538 */
            const PA1: f64 = 4.14856118683748331666e-01; /* 0x3FDA8D00, 0xAD92B34D */
            const PA2: f64 = -3.72207876035701323847e-01; /* 0xBFD7D240, 0xFBB8C3F1 */
            const PA3: f64 = 3.18346619901161753674e-01; /* 0x3FD45FCA, 0x805120E4 */
            const PA4: f64 = -1.10894694282396677476e-01; /* 0xBFBC6398, 0x3D3E28EC */
            const PA5: f64 = 3.54783043256182359371e-02; /* 0x3FA22A36, 0x599795EB */
            const PA6: f64 = -2.16637559486879084300e-03; /* 0xBF61BF38, 0x0A96073F */
            const QA1: f64 = 1.06420880400844228286e-01; /* 0x3FBB3E66, 0x18EEE323 */
            const QA2: f64 = 5.40397917702171048937e-01; /* 0x3FE14AF0, 0x92EB6F33 */
            const QA3: f64 = 7.18286544141962662868e-02; /* 0x3FB2635C, 0xD99FE9A7 */
            const QA4: f64 = 1.26171219808761642112e-01; /* 0x3FC02660, 0xE763351F */
            const QA5: f64 = 1.36370839120290507362e-02; /* 0x3F8BEDC2, 0x6B51DD1C */
            const QA6: f64 = 1.19844998467991074170e-02; /* 0x3F888B54, 0x5735151D */
            /*
            * Coefficients for approximation to  erfc in [1.25,1/0.35]
            */
            const RA0: f64 = -9.86494403484714822705e-03; /* 0xBF843412, 0x600D6435 */
            const RA1: f64 = -6.93858572707181764372e-01; /* 0xBFE63416, 0xE4BA7360 */
            const RA2: f64 = -1.05586262253232909814e+01; /* 0xC0251E04, 0x41B0E726 */
            const RA3: f64 = -6.23753324503260060396e+01; /* 0xC04F300A, 0xE4CBA38D */
            const RA4: f64 = -1.62396669462573470355e+02; /* 0xC0644CB1, 0x84282266 */
            const RA5: f64 = -1.84605092906711035994e+02; /* 0xC067135C, 0xEBCCABB2 */
            const RA6: f64 = -8.12874355063065934246e+01; /* 0xC0545265, 0x57E4D2F2 */
            const RA7: f64 = -9.81432934416914548592e+00; /* 0xC023A0EF, 0xC69AC25C */
            const SA1: f64 = 1.96512716674392571292e+01; /* 0x4033A6B9, 0xBD707687 */
            const SA2: f64 = 1.37657754143519042600e+02; /* 0x4061350C, 0x526AE721 */
            const SA3: f64 = 4.34565877475229228821e+02; /* 0x407B290D, 0xD58A1A71 */
            const SA4: f64 = 6.45387271733267880336e+02; /* 0x40842B19, 0x21EC2868 */
            const SA5: f64 = 4.29008140027567833386e+02; /* 0x407AD021, 0x57700314 */
            const SA6: f64 = 1.08635005541779435134e+02; /* 0x405B28A3, 0xEE48AE2C */
            const SA7: f64 = 6.57024977031928170135e+00; /* 0x401A47EF, 0x8E484A93 */
            const SA8: f64 = -6.04244152148580987438e-02; /* 0xBFAEEFF2, 0xEE749A62 */
            /*
            * Coefficients for approximation to  erfc in [1/.35,28]
            */
            const RB0: f64 = -9.86494292470009928597e-03; /* 0xBF843412, 0x39E86F4A */
            const RB1: f64 = -7.99283237680523006574e-01; /* 0xBFE993BA, 0x70C285DE */
            const RB2: f64 = -1.77579549177547519889e+01; /* 0xC031C209, 0x555F995A */
            const RB3: f64 = -1.60636384855821916062e+02; /* 0xC064145D, 0x43C5ED98 */
            const RB4: f64 = -6.37566443368389627722e+02; /* 0xC083EC88, 0x1375F228 */
            const RB5: f64 = -1.02509513161107724954e+03; /* 0xC0900461, 0x6A2E5992 */
            const RB6: f64 = -4.83519191608651397019e+02; /* 0xC07E384E, 0x9BDC383F */
            const SB1: f64 = 3.03380607434824582924e+01; /* 0x403E568B, 0x261D5190 */
            const SB2: f64 = 3.25792512996573918826e+02; /* 0x40745CAE, 0x221B9F0A */
            const SB3: f64 = 1.53672958608443695994e+03; /* 0x409802EB, 0x189D5118 */
            const SB4: f64 = 3.19985821950859553908e+03; /* 0x40A8FFB7, 0x688C246A */
            const SB5: f64 = 2.55305040643316442583e+03; /* 0x40A3F219, 0xCEDF3BE6 */
            const SB6: f64 = 4.74528541206955367215e+02; /* 0x407DA874, 0xE79FE763 */
            const SB7: f64 = -2.24409524465858183362e+01; /* 0xC03670E2, 0x42712D62 */

            fn erfc1(x: f64) -> f64 {
                let s: f64;
                let p: f64;
                let q: f64;

                s = fabs(x) - 1.0;
                p = PA0 + s * (PA1 + s * (PA2 + s * (PA3 + s * (PA4 + s * (PA5 + s * PA6)))));
                q = 1.0 + s * (QA1 + s * (QA2 + s * (QA3 + s * (QA4 + s * (QA5 + s * QA6)))));

                1.0 - ERX - p / q
            }

            fn erfc2(ix: u32, mut x: f64) -> f64 {
                let s: f64;
                let r: f64;
                let big_s: f64;
                let z: f64;

                if ix < 0x3ff40000 {
                    /* |x| < 1.25 */
                    return erfc1(x);
                }

                x = fabs(x);
                s = 1.0 / (x * x);
                if ix < 0x4006db6d {
                    /* |x| < 1/.35 ~ 2.85714 */
                    r = RA0 + s * (RA1 + s * (RA2 + s * (RA3 + s * (RA4 + s * (RA5 + s * (RA6 + s * RA7))))));
                    big_s = 1.0
                        + s * (SA1
                            + s * (SA2 + s * (SA3 + s * (SA4 + s * (SA5 + s * (SA6 + s * (SA7 + s * SA8)))))));
                } else {
                    /* |x| > 1/.35 */
                    r = RB0 + s * (RB1 + s * (RB2 + s * (RB3 + s * (RB4 + s * (RB5 + s * RB6)))));
                    big_s =
                        1.0 + s * (SB1 + s * (SB2 + s * (SB3 + s * (SB4 + s * (SB5 + s * (SB6 + s * SB7))))));
                }
                z = with_set_low_word(x, 0);

                exp(-z * z - 0.5625) * exp((z - x) * (z + x) + r / big_s) / x
            }

            /// Error function (f64)
            ///
            /// Calculates an approximation to the “error function”, which estimates
            /// the probability that an observation will fall within x standard
            /// deviations of the mean (assuming a normal distribution).
            pub fn erf(x: f64) -> f64 {
                let r: f64;
                let s: f64;
                let z: f64;
                let y: f64;
                let mut ix: u32;
                let sign: usize;

                ix = get_high_word(x);
                sign = (ix >> 31) as usize;
                ix &= 0x7fffffff;
                if ix >= 0x7ff00000 {
                    /* erf(nan)=nan, erf(+-inf)=+-1 */
                    return 1.0 - 2.0 * (sign as f64) + 1.0 / x;
                }
                if ix < 0x3feb0000 {
                    /* |x| < 0.84375 */
                    if ix < 0x3e300000 {
                        /* |x| < 2**-28 */
                        /* avoid underflow */
                        return 0.125 * (8.0 * x + EFX8 * x);
                    }
                    z = x * x;
                    r = PP0 + z * (PP1 + z * (PP2 + z * (PP3 + z * PP4)));
                    s = 1.0 + z * (QQ1 + z * (QQ2 + z * (QQ3 + z * (QQ4 + z * QQ5))));
                    y = r / s;
                    return x + x * y;
                }
                if ix < 0x40180000 {
                    /* 0.84375 <= |x| < 6 */
                    y = 1.0 - erfc2(ix, x);
                } else {
                    let x1p_1022 = f64::from_bits(0x0010000000000000);
                    y = 1.0 - x1p_1022;
                }

                if sign != 0 {
                    -y
                } else {
                    y
                }
            }

            /// Error function (f64)
            ///
            /// Calculates the complementary probability.
            /// Is `1 - erf(x)`. Is computed directly, so that you can use it to avoid
            /// the loss of precision that would result from subtracting
            /// large probabilities (on large `x`) from 1.
            pub fn erfc(x: f64) -> f64 {
                let r: f64;
                let s: f64;
                let z: f64;
                let y: f64;
                let mut ix: u32;
                let sign: usize;

                ix = get_high_word(x);
                sign = (ix >> 31) as usize;
                ix &= 0x7fffffff;
                if ix >= 0x7ff00000 {
                    /* erfc(nan)=nan, erfc(+-inf)=0,2 */
                    return 2.0 * (sign as f64) + 1.0 / x;
                }
                if ix < 0x3feb0000 {
                    /* |x| < 0.84375 */
                    if ix < 0x3c700000 {
                        /* |x| < 2**-56 */
                        return 1.0 - x;
                    }
                    z = x * x;
                    r = PP0 + z * (PP1 + z * (PP2 + z * (PP3 + z * PP4)));
                    s = 1.0 + z * (QQ1 + z * (QQ2 + z * (QQ3 + z * (QQ4 + z * QQ5))));
                    y = r / s;
                    if sign != 0 || ix < 0x3fd00000 {
                        /* x < 1/4 */
                        return 1.0 - (x + x * y);
                    }
                    return 0.5 - (x - 0.5 + x * y);
                }
                if ix < 0x403c0000 {
                    /* 0.84375 <= |x| < 28 */
                    if sign != 0 {
                        return 2.0 - erfc2(ix, x);
                    } else {
                        return erfc2(ix, x);
                    }
                }

                let x1p_1022 = f64::from_bits(0x0010000000000000);
                if sign != 0 {
                    2.0 - x1p_1022
                } else {
                    x1p_1022 * x1p_1022
                }
            }
        }

        mod erff
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{expf, fabsf};

            const ERX: f32 = 8.4506291151e-01; /* 0x3f58560b */
            /*
            * Coefficients for approximation to  erf on [0,0.84375]
            */
            const EFX8: f32 = 1.0270333290e+00; /* 0x3f8375d4 */
            const PP0: f32 = 1.2837916613e-01; /* 0x3e0375d4 */
            const PP1: f32 = -3.2504209876e-01; /* 0xbea66beb */
            const PP2: f32 = -2.8481749818e-02; /* 0xbce9528f */
            const PP3: f32 = -5.7702702470e-03; /* 0xbbbd1489 */
            const PP4: f32 = -2.3763017452e-05; /* 0xb7c756b1 */
            const QQ1: f32 = 3.9791721106e-01; /* 0x3ecbbbce */
            const QQ2: f32 = 6.5022252500e-02; /* 0x3d852a63 */
            const QQ3: f32 = 5.0813062117e-03; /* 0x3ba68116 */
            const QQ4: f32 = 1.3249473704e-04; /* 0x390aee49 */
            const QQ5: f32 = -3.9602282413e-06; /* 0xb684e21a */
            /*
            * Coefficients for approximation to  erf  in [0.84375,1.25]
            */
            const PA0: f32 = -2.3621185683e-03; /* 0xbb1acdc6 */
            const PA1: f32 = 4.1485610604e-01; /* 0x3ed46805 */
            const PA2: f32 = -3.7220788002e-01; /* 0xbebe9208 */
            const PA3: f32 = 3.1834661961e-01; /* 0x3ea2fe54 */
            const PA4: f32 = -1.1089469492e-01; /* 0xbde31cc2 */
            const PA5: f32 = 3.5478305072e-02; /* 0x3d1151b3 */
            const PA6: f32 = -2.1663755178e-03; /* 0xbb0df9c0 */
            const QA1: f32 = 1.0642088205e-01; /* 0x3dd9f331 */
            const QA2: f32 = 5.4039794207e-01; /* 0x3f0a5785 */
            const QA3: f32 = 7.1828655899e-02; /* 0x3d931ae7 */
            const QA4: f32 = 1.2617121637e-01; /* 0x3e013307 */
            const QA5: f32 = 1.3637083583e-02; /* 0x3c5f6e13 */
            const QA6: f32 = 1.1984500103e-02; /* 0x3c445aa3 */
            /*
            * Coefficients for approximation to  erfc in [1.25,1/0.35]
            */
            const RA0: f32 = -9.8649440333e-03; /* 0xbc21a093 */
            const RA1: f32 = -6.9385856390e-01; /* 0xbf31a0b7 */
            const RA2: f32 = -1.0558626175e+01; /* 0xc128f022 */
            const RA3: f32 = -6.2375331879e+01; /* 0xc2798057 */
            const RA4: f32 = -1.6239666748e+02; /* 0xc322658c */
            const RA5: f32 = -1.8460508728e+02; /* 0xc3389ae7 */
            const RA6: f32 = -8.1287437439e+01; /* 0xc2a2932b */
            const RA7: f32 = -9.8143291473e+00; /* 0xc11d077e */
            const SA1: f32 = 1.9651271820e+01; /* 0x419d35ce */
            const SA2: f32 = 1.3765776062e+02; /* 0x4309a863 */
            const SA3: f32 = 4.3456588745e+02; /* 0x43d9486f */
            const SA4: f32 = 6.4538726807e+02; /* 0x442158c9 */
            const SA5: f32 = 4.2900814819e+02; /* 0x43d6810b */
            const SA6: f32 = 1.0863500214e+02; /* 0x42d9451f */
            const SA7: f32 = 6.5702495575e+00; /* 0x40d23f7c */
            const SA8: f32 = -6.0424413532e-02; /* 0xbd777f97 */
            /*
            * Coefficients for approximation to  erfc in [1/.35,28]
            */
            const RB0: f32 = -9.8649431020e-03; /* 0xbc21a092 */
            const RB1: f32 = -7.9928326607e-01; /* 0xbf4c9dd4 */
            const RB2: f32 = -1.7757955551e+01; /* 0xc18e104b */
            const RB3: f32 = -1.6063638306e+02; /* 0xc320a2ea */
            const RB4: f32 = -6.3756646729e+02; /* 0xc41f6441 */
            const RB5: f32 = -1.0250950928e+03; /* 0xc480230b */
            const RB6: f32 = -4.8351919556e+02; /* 0xc3f1c275 */
            const SB1: f32 = 3.0338060379e+01; /* 0x41f2b459 */
            const SB2: f32 = 3.2579251099e+02; /* 0x43a2e571 */
            const SB3: f32 = 1.5367296143e+03; /* 0x44c01759 */
            const SB4: f32 = 3.1998581543e+03; /* 0x4547fdbb */
            const SB5: f32 = 2.5530502930e+03; /* 0x451f90ce */
            const SB6: f32 = 4.7452853394e+02; /* 0x43ed43a7 */
            const SB7: f32 = -2.2440952301e+01; /* 0xc1b38712 */

            fn erfc1(x: f32) -> f32 {
                let s: f32;
                let p: f32;
                let q: f32;

                s = fabsf(x) - 1.0;
                p = PA0 + s * (PA1 + s * (PA2 + s * (PA3 + s * (PA4 + s * (PA5 + s * PA6)))));
                q = 1.0 + s * (QA1 + s * (QA2 + s * (QA3 + s * (QA4 + s * (QA5 + s * QA6)))));
                return 1.0 - ERX - p / q;
            }

            fn erfc2(mut ix: u32, mut x: f32) -> f32 {
                let s: f32;
                let r: f32;
                let big_s: f32;
                let z: f32;

                if ix < 0x3fa00000 {
                    /* |x| < 1.25 */
                    return erfc1(x);
                }

                x = fabsf(x);
                s = 1.0 / (x * x);
                if ix < 0x4036db6d {
                    /* |x| < 1/0.35 */
                    r = RA0 + s * (RA1 + s * (RA2 + s * (RA3 + s * (RA4 + s * (RA5 + s * (RA6 + s * RA7))))));
                    big_s = 1.0
                        + s * (SA1
                            + s * (SA2 + s * (SA3 + s * (SA4 + s * (SA5 + s * (SA6 + s * (SA7 + s * SA8)))))));
                } else {
                    /* |x| >= 1/0.35 */
                    r = RB0 + s * (RB1 + s * (RB2 + s * (RB3 + s * (RB4 + s * (RB5 + s * RB6)))));
                    big_s =
                        1.0 + s * (SB1 + s * (SB2 + s * (SB3 + s * (SB4 + s * (SB5 + s * (SB6 + s * SB7))))));
                }
                ix = x.to_bits();
                z = f32::from_bits(ix & 0xffffe000);

                expf(-z * z - 0.5625) * expf((z - x) * (z + x) + r / big_s) / x
            }

            /// Error function (f32)
            ///
            /// Calculates an approximation to the “error function”, which estimates
            /// the probability that an observation will fall within x standard
            /// deviations of the mean (assuming a normal distribution).
            pub fn erff(x: f32) -> f32 {
                let r: f32;
                let s: f32;
                let z: f32;
                let y: f32;
                let mut ix: u32;
                let sign: usize;

                ix = x.to_bits();
                sign = (ix >> 31) as usize;
                ix &= 0x7fffffff;
                if ix >= 0x7f800000 {
                    /* erf(nan)=nan, erf(+-inf)=+-1 */
                    return 1.0 - 2.0 * (sign as f32) + 1.0 / x;
                }
                if ix < 0x3f580000 {
                    /* |x| < 0.84375 */
                    if ix < 0x31800000 {
                        /* |x| < 2**-28 */
                        /*avoid underflow */
                        return 0.125 * (8.0 * x + EFX8 * x);
                    }
                    z = x * x;
                    r = PP0 + z * (PP1 + z * (PP2 + z * (PP3 + z * PP4)));
                    s = 1.0 + z * (QQ1 + z * (QQ2 + z * (QQ3 + z * (QQ4 + z * QQ5))));
                    y = r / s;
                    return x + x * y;
                }
                if ix < 0x40c00000 {
                    /* |x| < 6 */
                    y = 1.0 - erfc2(ix, x);
                } else {
                    let x1p_120 = f32::from_bits(0x03800000);
                    y = 1.0 - x1p_120;
                }

                if sign != 0 {
                    -y
                } else {
                    y
                }
            }

            /// Error function (f32)
            ///
            /// Calculates the complementary probability.
            /// Is `1 - erf(x)`. Is computed directly, so that you can use it to avoid
            /// the loss of precision that would result from subtracting
            /// large probabilities (on large `x`) from 1.
            pub fn erfcf(x: f32) -> f32 {
                let r: f32;
                let s: f32;
                let z: f32;
                let y: f32;
                let mut ix: u32;
                let sign: usize;

                ix = x.to_bits();
                sign = (ix >> 31) as usize;
                ix &= 0x7fffffff;
                if ix >= 0x7f800000 {
                    /* erfc(nan)=nan, erfc(+-inf)=0,2 */
                    return 2.0 * (sign as f32) + 1.0 / x;
                }

                if ix < 0x3f580000 {
                    /* |x| < 0.84375 */
                    if ix < 0x23800000 {
                        /* |x| < 2**-56 */
                        return 1.0 - x;
                    }
                    z = x * x;
                    r = PP0 + z * (PP1 + z * (PP2 + z * (PP3 + z * PP4)));
                    s = 1.0 + z * (QQ1 + z * (QQ2 + z * (QQ3 + z * (QQ4 + z * QQ5))));
                    y = r / s;
                    if sign != 0 || ix < 0x3e800000 {
                        /* x < 1/4 */
                        return 1.0 - (x + x * y);
                    }
                    return 0.5 - (x - 0.5 + x * y);
                }
                if ix < 0x41e00000 {
                    /* |x| < 28 */
                    if sign != 0 {
                        return 2.0 - erfc2(ix, x);
                    } else {
                        return erfc2(ix, x);
                    }
                }

                let x1p_120 = f32::from_bits(0x03800000);
                if sign != 0 {
                    2.0 - x1p_120
                } else {
                    x1p_120 * x1p_120
                }
            }
        }

        mod exp
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::scalbn;

            const HALF: [f64; 2] = [0.5, -0.5];
            const LN2HI: f64 = 6.93147180369123816490e-01; /* 0x3fe62e42, 0xfee00000 */
            const LN2LO: f64 = 1.90821492927058770002e-10; /* 0x3dea39ef, 0x35793c76 */
            const INVLN2: f64 = 1.44269504088896338700e+00; /* 0x3ff71547, 0x652b82fe */
            const P1: f64 = 1.66666666666666019037e-01; /* 0x3FC55555, 0x5555553E */
            const P2: f64 = -2.77777777770155933842e-03; /* 0xBF66C16C, 0x16BEBD93 */
            const P3: f64 = 6.61375632143793436117e-05; /* 0x3F11566A, 0xAF25DE2C */
            const P4: f64 = -1.65339022054652515390e-06; /* 0xBEBBBD41, 0xC5D26BF1 */
            const P5: f64 = 4.13813679705723846039e-08; /* 0x3E663769, 0x72BEA4D0 */

            /// Exponential, base *e* (f64)
            ///
            /// Calculate the exponential of `x`, that is, *e* raised to the power `x`
            /// (where *e* is the base of the natural system of logarithms, approximately 2.71828).
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn exp(mut x: f64) -> f64 {
                let x1p1023 = f64::from_bits(0x7fe0000000000000); // 0x1p1023 === 2 ^ 1023
                let x1p_149 = f64::from_bits(0x36a0000000000000); // 0x1p-149 === 2 ^ -149

                let hi: f64;
                let lo: f64;
                let c: f64;
                let xx: f64;
                let y: f64;
                let k: i32;
                let sign: i32;
                let mut hx: u32;

                hx = (x.to_bits() >> 32) as u32;
                sign = (hx >> 31) as i32;
                hx &= 0x7fffffff; /* high word of |x| */

                /* special cases */
                if hx >= 0x4086232b {
                    /* if |x| >= 708.39... */
                    if x.is_nan() {
                        return x;
                    }
                    if x > 709.782712893383973096 {
                        /* overflow if x!=inf */
                        x *= x1p1023;
                        return x;
                    }
                    if x < -708.39641853226410622 {
                        /* underflow if x!=-inf */
                        force_eval!((-x1p_149 / x) as f32);
                        if x < -745.13321910194110842 {
                            return 0.;
                        }
                    }
                }

                /* argument reduction */
                if hx > 0x3fd62e42 {
                    /* if |x| > 0.5 ln2 */
                    if hx >= 0x3ff0a2b2 {
                        /* if |x| >= 1.5 ln2 */
                        k = (INVLN2 * x + HALF[sign as usize]) as i32;
                    } else {
                        k = 1 - sign - sign;
                    }
                    hi = x - k as f64 * LN2HI; /* k*ln2hi is exact here */
                    lo = k as f64 * LN2LO;
                    x = hi - lo;
                } else if hx > 0x3e300000 {
                    /* if |x| > 2**-28 */
                    k = 0;
                    hi = x;
                    lo = 0.;
                } else {
                    /* inexact if x!=0 */
                    force_eval!(x1p1023 + x);
                    return 1. + x;
                }

                /* x is now in primary range */
                xx = x * x;
                c = x - xx * (P1 + xx * (P2 + xx * (P3 + xx * (P4 + xx * P5))));
                y = 1. + (x * c / (2. - c) - lo + hi);
                if k == 0 {
                    y
                } else {
                    scalbn(y, k)
                }
            }
        }

        mod exp10
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{exp2, modf, pow};

            const LN10: f64 = 3.32192809488736234787031942948939;
            const P10: &[f64] = &[
                1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1,
                1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15,
            ];

            pub fn exp10(x: f64) -> f64 {
                let (mut y, n) = modf(x);
                let u: u64 = n.to_bits();
                /* fabs(n) < 16 without raising invalid on nan */
                if (u >> 52 & 0x7ff) < 0x3ff + 4 {
                    if y == 0.0 {
                        return P10[((n as isize) + 15) as usize];
                    }
                    y = exp2(LN10 * y);
                    return y * P10[((n as isize) + 15) as usize];
                }
                return pow(10.0, x);
            }
        }

        mod exp10f
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{exp2, exp2f, modff};

            const LN10_F32: f32 = 3.32192809488736234787031942948939;
            const LN10_F64: f64 = 3.32192809488736234787031942948939;
            const P10: &[f32] = &[
                1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7,
            ];

            pub fn exp10f(x: f32) -> f32 {
                let (mut y, n) = modff(x);
                let u = n.to_bits();
                /* fabsf(n) < 8 without raising invalid on nan */
                if (u >> 23 & 0xff) < 0x7f + 3 {
                    if y == 0.0 {
                        return P10[((n as isize) + 7) as usize];
                    }
                    y = exp2f(LN10_F32 * y);
                    return y * P10[((n as isize) + 7) as usize];
                }
                return exp2(LN10_F64 * (x as f64)) as f32;
            }
        }

        mod exp2
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::scalbn;

            const TBLSIZE: usize = 256;

            #[cfg_attr(rustfmt, rustfmt_skip)]
            static TBL: [u64; TBLSIZE * 2] =
            [
                0x3fe6a09e667f3d5d, 0x3d39880000000000,
                0x3fe6b052fa751744, 0x3cd8000000000000,
                0x3fe6c012750bd9fe, 0xbd28780000000000,
                0x3fe6cfdcddd476bf, 0x3d1ec00000000000,
                0x3fe6dfb23c651a29, 0xbcd8000000000000,
                0x3fe6ef9298593ae3, 0xbcbc000000000000,
                0x3fe6ff7df9519386, 0xbd2fd80000000000,
                0x3fe70f7466f42da3, 0xbd2c880000000000,
                0x3fe71f75e8ec5fc3, 0x3d13c00000000000,
                0x3fe72f8286eacf05, 0xbd38300000000000,
                0x3fe73f9a48a58152, 0xbd00c00000000000,
                0x3fe74fbd35d7ccfc, 0x3d2f880000000000,
                0x3fe75feb564267f1, 0x3d03e00000000000,
                0x3fe77024b1ab6d48, 0xbd27d00000000000,
                0x3fe780694fde5d38, 0xbcdd000000000000,
                0x3fe790b938ac1d00, 0x3ce3000000000000,
                0x3fe7a11473eb0178, 0xbced000000000000,
                0x3fe7b17b0976d060, 0x3d20400000000000,
                0x3fe7c1ed0130c133, 0x3ca0000000000000,
                0x3fe7d26a62ff8636, 0xbd26900000000000,
                0x3fe7e2f336cf4e3b, 0xbd02e00000000000,
                0x3fe7f3878491c3e8, 0xbd24580000000000,
                0x3fe80427543e1b4e, 0x3d33000000000000,
                0x3fe814d2add1071a, 0x3d0f000000000000,
                0x3fe82589994ccd7e, 0xbd21c00000000000,
                0x3fe8364c1eb942d0, 0x3d29d00000000000,
                0x3fe8471a4623cab5, 0x3d47100000000000,
                0x3fe857f4179f5bbc, 0x3d22600000000000,
                0x3fe868d99b4491af, 0xbd32c40000000000,
                0x3fe879cad931a395, 0xbd23000000000000,
                0x3fe88ac7d98a65b8, 0xbd2a800000000000,
                0x3fe89bd0a4785800, 0xbced000000000000,
                0x3fe8ace5422aa223, 0x3d33280000000000,
                0x3fe8be05bad619fa, 0x3d42b40000000000,
                0x3fe8cf3216b54383, 0xbd2ed00000000000,
                0x3fe8e06a5e08664c, 0xbd20500000000000,
                0x3fe8f1ae99157807, 0x3d28280000000000,
                0x3fe902fed0282c0e, 0xbd1cb00000000000,
                0x3fe9145b0b91ff96, 0xbd05e00000000000,
                0x3fe925c353aa2ff9, 0x3cf5400000000000,
                0x3fe93737b0cdc64a, 0x3d17200000000000,
                0x3fe948b82b5f98ae, 0xbd09000000000000,
                0x3fe95a44cbc852cb, 0x3d25680000000000,
                0x3fe96bdd9a766f21, 0xbd36d00000000000,
                0x3fe97d829fde4e2a, 0xbd01000000000000,
                0x3fe98f33e47a23a3, 0x3d2d000000000000,
                0x3fe9a0f170ca0604, 0xbd38a40000000000,
                0x3fe9b2bb4d53ff89, 0x3d355c0000000000,
                0x3fe9c49182a3f15b, 0x3d26b80000000000,
                0x3fe9d674194bb8c5, 0xbcec000000000000,
                0x3fe9e86319e3238e, 0x3d17d00000000000,
                0x3fe9fa5e8d07f302, 0x3d16400000000000,
                0x3fea0c667b5de54d, 0xbcf5000000000000,
                0x3fea1e7aed8eb8f6, 0x3d09e00000000000,
                0x3fea309bec4a2e27, 0x3d2ad80000000000,
                0x3fea42c980460a5d, 0xbd1af00000000000,
                0x3fea5503b23e259b, 0x3d0b600000000000,
                0x3fea674a8af46213, 0x3d38880000000000,
                0x3fea799e1330b3a7, 0x3d11200000000000,
                0x3fea8bfe53c12e8d, 0x3d06c00000000000,
                0x3fea9e6b5579fcd2, 0xbd29b80000000000,
                0x3feab0e521356fb8, 0x3d2b700000000000,
                0x3feac36bbfd3f381, 0x3cd9000000000000,
                0x3fead5ff3a3c2780, 0x3ce4000000000000,
                0x3feae89f995ad2a3, 0xbd2c900000000000,
                0x3feafb4ce622f367, 0x3d16500000000000,
                0x3feb0e07298db790, 0x3d2fd40000000000,
                0x3feb20ce6c9a89a9, 0x3d12700000000000,
                0x3feb33a2b84f1a4b, 0x3d4d470000000000,
                0x3feb468415b747e7, 0xbd38380000000000,
                0x3feb59728de5593a, 0x3c98000000000000,
                0x3feb6c6e29f1c56a, 0x3d0ad00000000000,
                0x3feb7f76f2fb5e50, 0x3cde800000000000,
                0x3feb928cf22749b2, 0xbd04c00000000000,
                0x3feba5b030a10603, 0xbd0d700000000000,
                0x3febb8e0b79a6f66, 0x3d0d900000000000,
                0x3febcc1e904bc1ff, 0x3d02a00000000000,
                0x3febdf69c3f3a16f, 0xbd1f780000000000,
                0x3febf2c25bd71db8, 0xbd10a00000000000,
                0x3fec06286141b2e9, 0xbd11400000000000,
                0x3fec199bdd8552e0, 0x3d0be00000000000,
                0x3fec2d1cd9fa64ee, 0xbd09400000000000,
                0x3fec40ab5fffd02f, 0xbd0ed00000000000,
                0x3fec544778fafd15, 0x3d39660000000000,
                0x3fec67f12e57d0cb, 0xbd1a100000000000,
                0x3fec7ba88988c1b6, 0xbd58458000000000,
                0x3fec8f6d9406e733, 0xbd1a480000000000,
                0x3feca3405751c4df, 0x3ccb000000000000,
                0x3fecb720dcef9094, 0x3d01400000000000,
                0x3feccb0f2e6d1689, 0x3cf0200000000000,
                0x3fecdf0b555dc412, 0x3cf3600000000000,
                0x3fecf3155b5bab3b, 0xbd06900000000000,
                0x3fed072d4a0789bc, 0x3d09a00000000000,
                0x3fed1b532b08c8fa, 0xbd15e00000000000,
                0x3fed2f87080d8a85, 0x3d1d280000000000,
                0x3fed43c8eacaa203, 0x3d01a00000000000,
                0x3fed5818dcfba491, 0x3cdf000000000000,
                0x3fed6c76e862e6a1, 0xbd03a00000000000,
                0x3fed80e316c9834e, 0xbd0cd80000000000,
                0x3fed955d71ff6090, 0x3cf4c00000000000,
                0x3feda9e603db32ae, 0x3cff900000000000,
                0x3fedbe7cd63a8325, 0x3ce9800000000000,
                0x3fedd321f301b445, 0xbcf5200000000000,
                0x3fede7d5641c05bf, 0xbd1d700000000000,
                0x3fedfc97337b9aec, 0xbd16140000000000,
                0x3fee11676b197d5e, 0x3d0b480000000000,
                0x3fee264614f5a3e7, 0x3d40ce0000000000,
                0x3fee3b333b16ee5c, 0x3d0c680000000000,
                0x3fee502ee78b3fb4, 0xbd09300000000000,
                0x3fee653924676d68, 0xbce5000000000000,
                0x3fee7a51fbc74c44, 0xbd07f80000000000,
                0x3fee8f7977cdb726, 0xbcf3700000000000,
                0x3feea4afa2a490e8, 0x3ce5d00000000000,
                0x3feeb9f4867ccae4, 0x3d161a0000000000,
                0x3feecf482d8e680d, 0x3cf5500000000000,
                0x3feee4aaa2188514, 0x3cc6400000000000,
                0x3feefa1bee615a13, 0xbcee800000000000,
                0x3fef0f9c1cb64106, 0xbcfa880000000000,
                0x3fef252b376bb963, 0xbd2c900000000000,
                0x3fef3ac948dd7275, 0x3caa000000000000,
                0x3fef50765b6e4524, 0xbcf4f00000000000,
                0x3fef6632798844fd, 0x3cca800000000000,
                0x3fef7bfdad9cbe38, 0x3cfabc0000000000,
                0x3fef91d802243c82, 0xbcd4600000000000,
                0x3fefa7c1819e908e, 0xbd0b0c0000000000,
                0x3fefbdba3692d511, 0xbcc0e00000000000,
                0x3fefd3c22b8f7194, 0xbd10de8000000000,
                0x3fefe9d96b2a23ee, 0x3cee430000000000,
                0x3ff0000000000000, 0x0,
                0x3ff00b1afa5abcbe, 0xbcb3400000000000,
                0x3ff0163da9fb3303, 0xbd12170000000000,
                0x3ff02168143b0282, 0x3cba400000000000,
                0x3ff02c9a3e77806c, 0x3cef980000000000,
                0x3ff037d42e11bbca, 0xbcc7400000000000,
                0x3ff04315e86e7f89, 0x3cd8300000000000,
                0x3ff04e5f72f65467, 0xbd1a3f0000000000,
                0x3ff059b0d315855a, 0xbd02840000000000,
                0x3ff0650a0e3c1f95, 0x3cf1600000000000,
                0x3ff0706b29ddf71a, 0x3d15240000000000,
                0x3ff07bd42b72a82d, 0xbce9a00000000000,
                0x3ff0874518759bd0, 0x3ce6400000000000,
                0x3ff092bdf66607c8, 0xbd00780000000000,
                0x3ff09e3ecac6f383, 0xbc98000000000000,
                0x3ff0a9c79b1f3930, 0x3cffa00000000000,
                0x3ff0b5586cf988fc, 0xbcfac80000000000,
                0x3ff0c0f145e46c8a, 0x3cd9c00000000000,
                0x3ff0cc922b724816, 0x3d05200000000000,
                0x3ff0d83b23395dd8, 0xbcfad00000000000,
                0x3ff0e3ec32d3d1f3, 0x3d1bac0000000000,
                0x3ff0efa55fdfa9a6, 0xbd04e80000000000,
                0x3ff0fb66affed2f0, 0xbd0d300000000000,
                0x3ff1073028d7234b, 0x3cf1500000000000,
                0x3ff11301d0125b5b, 0x3cec000000000000,
                0x3ff11edbab5e2af9, 0x3d16bc0000000000,
                0x3ff12abdc06c31d5, 0x3ce8400000000000,
                0x3ff136a814f2047d, 0xbd0ed00000000000,
                0x3ff1429aaea92de9, 0x3ce8e00000000000,
                0x3ff14e95934f3138, 0x3ceb400000000000,
                0x3ff15a98c8a58e71, 0x3d05300000000000,
                0x3ff166a45471c3df, 0x3d03380000000000,
                0x3ff172b83c7d5211, 0x3d28d40000000000,
                0x3ff17ed48695bb9f, 0xbd05d00000000000,
                0x3ff18af9388c8d93, 0xbd1c880000000000,
                0x3ff1972658375d66, 0x3d11f00000000000,
                0x3ff1a35beb6fcba7, 0x3d10480000000000,
                0x3ff1af99f81387e3, 0xbd47390000000000,
                0x3ff1bbe084045d54, 0x3d24e40000000000,
                0x3ff1c82f95281c43, 0xbd0a200000000000,
                0x3ff1d4873168b9b2, 0x3ce3800000000000,
                0x3ff1e0e75eb44031, 0x3ceac00000000000,
                0x3ff1ed5022fcd938, 0x3d01900000000000,
                0x3ff1f9c18438cdf7, 0xbd1b780000000000,
                0x3ff2063b88628d8f, 0x3d2d940000000000,
                0x3ff212be3578a81e, 0x3cd8000000000000,
                0x3ff21f49917ddd41, 0x3d2b340000000000,
                0x3ff22bdda2791323, 0x3d19f80000000000,
                0x3ff2387a6e7561e7, 0xbd19c80000000000,
                0x3ff2451ffb821427, 0x3d02300000000000,
                0x3ff251ce4fb2a602, 0xbd13480000000000,
                0x3ff25e85711eceb0, 0x3d12700000000000,
                0x3ff26b4565e27d16, 0x3d11d00000000000,
                0x3ff2780e341de00f, 0x3d31ee0000000000,
                0x3ff284dfe1f5633e, 0xbd14c00000000000,
                0x3ff291ba7591bb30, 0xbd13d80000000000,
                0x3ff29e9df51fdf09, 0x3d08b00000000000,
                0x3ff2ab8a66d10e9b, 0xbd227c0000000000,
                0x3ff2b87fd0dada3a, 0x3d2a340000000000,
                0x3ff2c57e39771af9, 0xbd10800000000000,
                0x3ff2d285a6e402d9, 0xbd0ed00000000000,
                0x3ff2df961f641579, 0xbcf4200000000000,
                0x3ff2ecafa93e2ecf, 0xbd24980000000000,
                0x3ff2f9d24abd8822, 0xbd16300000000000,
                0x3ff306fe0a31b625, 0xbd32360000000000,
                0x3ff31432edeea50b, 0xbd70df8000000000,
                0x3ff32170fc4cd7b8, 0xbd22480000000000,
                0x3ff32eb83ba8e9a2, 0xbd25980000000000,
                0x3ff33c08b2641766, 0x3d1ed00000000000,
                0x3ff3496266e3fa27, 0xbcdc000000000000,
                0x3ff356c55f929f0f, 0xbd30d80000000000,
                0x3ff36431a2de88b9, 0x3d22c80000000000,
                0x3ff371a7373aaa39, 0x3d20600000000000,
                0x3ff37f26231e74fe, 0xbd16600000000000,
                0x3ff38cae6d05d838, 0xbd0ae00000000000,
                0x3ff39a401b713ec3, 0xbd44720000000000,
                0x3ff3a7db34e5a020, 0x3d08200000000000,
                0x3ff3b57fbfec6e95, 0x3d3e800000000000,
                0x3ff3c32dc313a8f2, 0x3cef800000000000,
                0x3ff3d0e544ede122, 0xbd17a00000000000,
                0x3ff3dea64c1234bb, 0x3d26300000000000,
                0x3ff3ec70df1c4ecc, 0xbd48a60000000000,
                0x3ff3fa4504ac7e8c, 0xbd3cdc0000000000,
                0x3ff40822c367a0bb, 0x3d25b80000000000,
                0x3ff4160a21f72e95, 0x3d1ec00000000000,
                0x3ff423fb27094646, 0xbd13600000000000,
                0x3ff431f5d950a920, 0x3d23980000000000,
                0x3ff43ffa3f84b9eb, 0x3cfa000000000000,
                0x3ff44e0860618919, 0xbcf6c00000000000,
                0x3ff45c2042a7d201, 0xbd0bc00000000000,
                0x3ff46a41ed1d0016, 0xbd12800000000000,
                0x3ff4786d668b3326, 0x3d30e00000000000,
                0x3ff486a2b5c13c00, 0xbd2d400000000000,
                0x3ff494e1e192af04, 0x3d0c200000000000,
                0x3ff4a32af0d7d372, 0xbd1e500000000000,
                0x3ff4b17dea6db801, 0x3d07800000000000,
                0x3ff4bfdad53629e1, 0xbd13800000000000,
                0x3ff4ce41b817c132, 0x3d00800000000000,
                0x3ff4dcb299fddddb, 0x3d2c700000000000,
                0x3ff4eb2d81d8ab96, 0xbd1ce00000000000,
                0x3ff4f9b2769d2d02, 0x3d19200000000000,
                0x3ff508417f4531c1, 0xbd08c00000000000,
                0x3ff516daa2cf662a, 0xbcfa000000000000,
                0x3ff5257de83f51ea, 0x3d4a080000000000,
                0x3ff5342b569d4eda, 0xbd26d80000000000,
                0x3ff542e2f4f6ac1a, 0xbd32440000000000,
                0x3ff551a4ca5d94db, 0x3d483c0000000000,
                0x3ff56070dde9116b, 0x3d24b00000000000,
                0x3ff56f4736b529de, 0x3d415a0000000000,
                0x3ff57e27dbe2c40e, 0xbd29e00000000000,
                0x3ff58d12d497c76f, 0xbd23080000000000,
                0x3ff59c0827ff0b4c, 0x3d4dec0000000000,
                0x3ff5ab07dd485427, 0xbcc4000000000000,
                0x3ff5ba11fba87af4, 0x3d30080000000000,
                0x3ff5c9268a59460b, 0xbd26c80000000000,
                0x3ff5d84590998e3f, 0x3d469a0000000000,
                0x3ff5e76f15ad20e1, 0xbd1b400000000000,
                0x3ff5f6a320dcebca, 0x3d17700000000000,
                0x3ff605e1b976dcb8, 0x3d26f80000000000,
                0x3ff6152ae6cdf715, 0x3d01000000000000,
                0x3ff6247eb03a5531, 0xbd15d00000000000,
                0x3ff633dd1d1929b5, 0xbd12d00000000000,
                0x3ff6434634ccc313, 0xbcea800000000000,
                0x3ff652b9febc8efa, 0xbd28600000000000,
                0x3ff6623882553397, 0x3d71fe0000000000,
                0x3ff671c1c708328e, 0xbd37200000000000,
                0x3ff68155d44ca97e, 0x3ce6800000000000,
                0x3ff690f4b19e9471, 0xbd29780000000000,
            ];
            /// Exponential, base 2 (f64) | Calculate `2^x`, that is, 2 raised to the power `x`.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn exp2(mut x: f64) -> f64
            {
                let redux = f64::from_bits(0x4338000000000000) / TBLSIZE as f64;
                let p1 = f64::from_bits(0x3fe62e42fefa39ef);
                let p2 = f64::from_bits(0x3fcebfbdff82c575);
                let p3 = f64::from_bits(0x3fac6b08d704a0a6);
                let p4 = f64::from_bits(0x3f83b2ab88f70400);
                let p5 = f64::from_bits(0x3f55d88003875c74);

                // double_t r, t, z;
                // uint32_t ix, i0;
                // union {double f; uint64_t i;} u = {x};
                // union {uint32_t u; int32_t i;} k;
                let x1p1023 = f64::from_bits(0x7fe0000000000000);
                let x1p52 = f64::from_bits(0x4330000000000000);
                let _0x1p_149 = f64::from_bits(0xb6a0000000000000);

                /* Filter out exceptional cases. */
                let ui = f64::to_bits(x);
                let ix = ui >> 32 & 0x7fffffff;
                if ix >= 0x408ff000 {
                    /* |x| >= 1022 or nan */
                    if ix >= 0x40900000 && ui >> 63 == 0 {
                        /* x >= 1024 or nan */
                        /* overflow */
                        x *= x1p1023;
                        return x;
                    }
                    if ix >= 0x7ff00000 {
                        /* -inf or -nan */
                        return -1.0 / x;
                    }
                    if ui >> 63 != 0 {
                        /* x <= -1022 */
                        /* underflow */
                        if x <= -1075.0 || x - x1p52 + x1p52 != x {
                            force_eval!((_0x1p_149 / x) as f32);
                        }
                        if x <= -1075.0 {
                            return 0.0;
                        }
                    }
                } else if ix < 0x3c900000 {
                    /* |x| < 0x1p-54 */
                    return 1.0 + x;
                }

                /* Reduce x, computing z, i0, and k. */
                let ui = f64::to_bits(x + redux);
                let mut i0 = ui as u32;
                i0 = i0.wrapping_add(TBLSIZE as u32 / 2);
                let ku = i0 / TBLSIZE as u32 * TBLSIZE as u32;
                let ki = ku as i32 / TBLSIZE as i32;
                i0 %= TBLSIZE as u32;
                let uf = f64::from_bits(ui) - redux;
                let mut z = x - uf;

                /* Compute r = exp2(y) = exp2t[i0] * p(z - eps[i]). */
                let t = f64::from_bits(TBL[2 * i0 as usize]); /* exp2t[i0] */
                z -= f64::from_bits(TBL[2 * i0 as usize + 1]); /* eps[i0]   */
                let r = t + t * z * (p1 + z * (p2 + z * (p3 + z * (p4 + z * p5))));

                scalbn(r, ki)
            }
        }

        mod exp2f
        {
            use ::
            {
                *,
            };
            /*
            */
            const TBLSIZE: usize = 16;

            static EXP2FT: [u64; TBLSIZE] =
            [
                0x3fe6a09e667f3bcd,
                0x3fe7a11473eb0187,
                0x3fe8ace5422aa0db,
                0x3fe9c49182a3f090,
                0x3feae89f995ad3ad,
                0x3fec199bdd85529c,
                0x3fed5818dcfba487,
                0x3feea4afa2a490da,
                0x3ff0000000000000,
                0x3ff0b5586cf9890f,
                0x3ff172b83c7d517b,
                0x3ff2387a6e756238,
                0x3ff306fe0a31b715,
                0x3ff3dea64c123422,
                0x3ff4bfdad5362a27,
                0x3ff5ab07dd485429,
            ];
            /// Exponential, base 2 (f32) | Calculate `2^x`, that is, 2 raised to the power `x`.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn exp2f(mut x: f32) -> f32
            {
                let redux = f32::from_bits(0x4b400000) / TBLSIZE as f32;
                let p1 = f32::from_bits(0x3f317218);
                let p2 = f32::from_bits(0x3e75fdf0);
                let p3 = f32::from_bits(0x3d6359a4);
                let p4 = f32::from_bits(0x3c1d964e);

                // double_t t, r, z;
                // uint32_t ix, i0, k;

                let x1p127 = f32::from_bits(0x7f000000);

                /* Filter out exceptional cases. */
                let ui = f32::to_bits(x);
                let ix = ui & 0x7fffffff;
                if ix > 0x42fc0000 {
                    /* |x| > 126 */
                    if ix > 0x7f800000 {
                        /* NaN */
                        return x;
                    }
                    if ui >= 0x43000000 && ui < 0x80000000 {
                        /* x >= 128 */
                        x *= x1p127;
                        return x;
                    }
                    if ui >= 0x80000000 {
                        /* x < -126 */
                        if ui >= 0xc3160000 || (ui & 0x0000ffff != 0) {
                            force_eval!(f32::from_bits(0x80000001) / x);
                        }
                        if ui >= 0xc3160000 {
                            /* x <= -150 */
                            return 0.0;
                        }
                    }
                } else if ix <= 0x33000000 {
                    /* |x| <= 0x1p-25 */
                    return 1.0 + x;
                }

                /* Reduce x, computing z, i0, and k. */
                let ui = f32::to_bits(x + redux);
                let mut i0 = ui;
                i0 += TBLSIZE as u32 / 2;
                let k = i0 / TBLSIZE as u32;
                let ukf = f64::from_bits(((0x3ff + k) as u64) << 52);
                i0 &= TBLSIZE as u32 - 1;
                let mut uf = f32::from_bits(ui);
                uf -= redux;
                let z: f64 = (x - uf) as f64;
                /* Compute r = exp2(y) = exp2ft[i0] * p(z). */
                let r: f64 = f64::from_bits(EXP2FT[i0 as usize]);
                let t: f64 = r as f64 * z;
                let r: f64 = r + t * (p1 as f64 + z * p2 as f64) + t * (z * z) * (p3 as f64 + z * p4 as f64);

                /* Scale by 2**k */
                (r * ukf) as f32
            }
        }

        mod expf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::scalbnf;

            const HALF: [f32; 2] = [0.5, -0.5];
            const LN2_HI: f32 = 6.9314575195e-01; /* 0x3f317200 */
            const LN2_LO: f32 = 1.4286067653e-06; /* 0x35bfbe8e */
            const INV_LN2: f32 = 1.4426950216e+00; /* 0x3fb8aa3b */
            /*
            * Domain [-0.34568, 0.34568], range ~[-4.278e-9, 4.447e-9]:
            * |x*(exp(x)+1)/(exp(x)-1) - p(x)| < 2**-27.74
            */
            const P1: f32 = 1.6666625440e-1; /*  0xaaaa8f.0p-26 */
            const P2: f32 = -2.7667332906e-3; /* -0xb55215.0p-32 */

            /// Exponential, base *e* (f32) | Calculate the exponential of `x`, that is, *e* raised to the power `x`
            /// (where *e* is the base of the natural system of logarithms, approximately 2.71828).
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn expf(mut x: f32) -> f32
            {
                let x1p127 = f32::from_bits(0x7f000000); // 0x1p127f === 2 ^ 127
                let x1p_126 = f32::from_bits(0x800000); // 0x1p-126f === 2 ^ -126  /*original 0x1p-149f    ??????????? */
                let mut hx = x.to_bits();
                let sign = (hx >> 31) as i32; /* sign bit of x */
                let signb: bool = sign != 0;
                hx &= 0x7fffffff; /* high word of |x| */

                /* special cases */
                if hx >= 0x42aeac50 {
                    /* if |x| >= -87.33655f or NaN */
                    if hx > 0x7f800000 {
                        /* NaN */
                        return x;
                    }
                    if (hx >= 0x42b17218) && (!signb) {
                        /* x >= 88.722839f */
                        /* overflow */
                        x *= x1p127;
                        return x;
                    }
                    if signb {
                        /* underflow */
                        force_eval!(-x1p_126 / x);
                        if hx >= 0x42cff1b5 {
                            /* x <= -103.972084f */
                            return 0.;
                        }
                    }
                }

                /* argument reduction */
                let k: i32;
                let hi: f32;
                let lo: f32;
                if hx > 0x3eb17218 {
                    /* if |x| > 0.5 ln2 */
                    if hx > 0x3f851592 {
                        /* if |x| > 1.5 ln2 */
                        k = (INV_LN2 * x + HALF[sign as usize]) as i32;
                    } else {
                        k = 1 - sign - sign;
                    }
                    let kf = k as f32;
                    hi = x - kf * LN2_HI; /* k*ln2hi is exact here */
                    lo = kf * LN2_LO;
                    x = hi - lo;
                } else if hx > 0x39000000 {
                    /* |x| > 2**-14 */
                    k = 0;
                    hi = x;
                    lo = 0.;
                } else {
                    /* raise inexact */
                    force_eval!(x1p127 + x);
                    return 1. + x;
                }

                /* x is now in primary range */
                let xx = x * x;
                let c = x - xx * (P1 + xx * P2);
                let y = 1. + (x * c / (2. - c) - lo + hi);
                if k == 0 {
                    y
                } else {
                    scalbnf(y, k)
                }
            }
        }

        mod expm1
        {
            use ::
            {
                *,
            };
            /*
            */
            const O_THRESHOLD: f64 = 7.09782712893383973096e+02; /* 0x40862E42, 0xFEFA39EF */
            const LN2_HI: f64 = 6.93147180369123816490e-01; /* 0x3fe62e42, 0xfee00000 */
            const LN2_LO: f64 = 1.90821492927058770002e-10; /* 0x3dea39ef, 0x35793c76 */
            const INVLN2: f64 = 1.44269504088896338700e+00; /* 0x3ff71547, 0x652b82fe */
            /* Scaled Q's: Qn_here = 2**n * Qn_above, for R(2*z) where z = hxs = x*x/2: */
            const Q1: f64 = -3.33333333333331316428e-02; /* BFA11111 111110F4 */
            const Q2: f64 = 1.58730158725481460165e-03; /* 3F5A01A0 19FE5585 */
            const Q3: f64 = -7.93650757867487942473e-05; /* BF14CE19 9EAADBB7 */
            const Q4: f64 = 4.00821782732936239552e-06; /* 3ED0CFCA 86E65239 */
            const Q5: f64 = -2.01099218183624371326e-07; /* BE8AFDB7 6E09C32D */

            /// Exponential, base *e*, of x-1 (f64).
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn expm1(mut x: f64) -> f64
            {
                let hi: f64;
                let lo: f64;
                let k: i32;
                let c: f64;
                let mut t: f64;
                let mut y: f64;

                let mut ui = x.to_bits();
                let hx = ((ui >> 32) & 0x7fffffff) as u32;
                let sign = (ui >> 63) as i32;

                /* filter out huge and non-finite argument */
                if hx >= 0x4043687A {
                    /* if |x|>=56*ln2 */
                    if x.is_nan() {
                        return x;
                    }
                    if sign != 0 {
                        return -1.0;
                    }
                    if x > O_THRESHOLD {
                        x *= f64::from_bits(0x7fe0000000000000);
                        return x;
                    }
                }

                /* argument reduction */
                if hx > 0x3fd62e42 {
                    /* if  |x| > 0.5 ln2 */
                    if hx < 0x3FF0A2B2 {
                        /* and |x| < 1.5 ln2 */
                        if sign == 0 {
                            hi = x - LN2_HI;
                            lo = LN2_LO;
                            k = 1;
                        } else {
                            hi = x + LN2_HI;
                            lo = -LN2_LO;
                            k = -1;
                        }
                    } else {
                        k = (INVLN2 * x + if sign != 0 { -0.5 } else { 0.5 }) as i32;
                        t = k as f64;
                        hi = x - t * LN2_HI; /* t*ln2_hi is exact here */
                        lo = t * LN2_LO;
                    }
                    x = hi - lo;
                    c = (hi - x) - lo;
                } else if hx < 0x3c900000 {
                    /* |x| < 2**-54, return x */
                    if hx < 0x00100000 {
                        force_eval!(x);
                    }
                    return x;
                } else {
                    c = 0.0;
                    k = 0;
                }

                /* x is now in primary range */
                let hfx = 0.5 * x;
                let hxs = x * hfx;
                let r1 = 1.0 + hxs * (Q1 + hxs * (Q2 + hxs * (Q3 + hxs * (Q4 + hxs * Q5))));
                t = 3.0 - r1 * hfx;
                let mut e = hxs * ((r1 - t) / (6.0 - x * t));
                if k == 0 {
                    /* c is 0 */
                    return x - (x * e - hxs);
                }
                e = x * (e - c) - c;
                e -= hxs;
                /* exp(x) ~ 2^k (x_reduced - e + 1) */
                if k == -1 {
                    return 0.5 * (x - e) - 0.5;
                }
                if k == 1 {
                    if x < -0.25 {
                        return -2.0 * (e - (x + 0.5));
                    }
                    return 1.0 + 2.0 * (x - e);
                }
                ui = ((0x3ff + k) as u64) << 52; /* 2^k */
                let twopk = f64::from_bits(ui);
                if k < 0 || k > 56 {
                    /* suffice to return exp(x)-1 */
                    y = x - e + 1.0;
                    if k == 1024 {
                        y = y * 2.0 * f64::from_bits(0x7fe0000000000000);
                    } else {
                        y = y * twopk;
                    }
                    return y - 1.0;
                }
                ui = ((0x3ff - k) as u64) << 52; /* 2^-k */
                let uf = f64::from_bits(ui);
                if k < 20 {
                    y = (x - e + (1.0 - uf)) * twopk;
                } else {
                    y = (x - (e + uf) + 1.0) * twopk;
                }
                y
            }
        }

        mod expm1f
        {
            use ::
            {
                *,
            };
            /*
            */
            const O_THRESHOLD: f32 = 8.8721679688e+01; /* 0x42b17180 */
            const LN2_HI: f32 = 6.9313812256e-01; /* 0x3f317180 */
            const LN2_LO: f32 = 9.0580006145e-06; /* 0x3717f7d1 */
            const INV_LN2: f32 = 1.4426950216e+00; /* 0x3fb8aa3b */
            /*
            * Domain [-0.34568, 0.34568], range ~[-6.694e-10, 6.696e-10]:
            * |6 / x * (1 + 2 * (1 / (exp(x) - 1) - 1 / x)) - q(x)| < 2**-30.04
            * Scaled coefficients: Qn_here = 2**n * Qn_for_q (see s_expm1.c):
            */
            const Q1: f32 = -3.3333212137e-2; /* -0x888868.0p-28 */
            const Q2: f32 = 1.5807170421e-3; /*  0xcf3010.0p-33 */

            /// Exponential, base *e*, of x-1 (f32).
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn expm1f(mut x: f32) -> f32
            {
                let x1p127 = f32::from_bits(0x7f000000); // 0x1p127f === 2 ^ 127

                let mut hx = x.to_bits();
                let sign = (hx >> 31) != 0;
                hx &= 0x7fffffff;

                /* filter out huge and non-finite argument */
                if hx >= 0x4195b844 {
                    /* if |x|>=27*ln2 */
                    if hx > 0x7f800000 {
                        /* NaN */
                        return x;
                    }
                    if sign {
                        return -1.;
                    }
                    if x > O_THRESHOLD {
                        x *= x1p127;
                        return x;
                    }
                }

                let k: i32;
                let hi: f32;
                let lo: f32;
                let mut c = 0f32;
                /* argument reduction */
                if hx > 0x3eb17218 {
                    /* if  |x| > 0.5 ln2 */
                    if hx < 0x3F851592 {
                        /* and |x| < 1.5 ln2 */
                        if !sign {
                            hi = x - LN2_HI;
                            lo = LN2_LO;
                            k = 1;
                        } else {
                            hi = x + LN2_HI;
                            lo = -LN2_LO;
                            k = -1;
                        }
                    } else {
                        k = (INV_LN2 * x + (if sign { -0.5 } else { 0.5 })) as i32;
                        let t = k as f32;
                        hi = x - t * LN2_HI; /* t*ln2_hi is exact here */
                        lo = t * LN2_LO;
                    }
                    x = hi - lo;
                    c = (hi - x) - lo;
                } else if hx < 0x33000000 {
                    /* when |x|<2**-25, return x */
                    if hx < 0x00800000 {
                        force_eval!(x * x);
                    }
                    return x;
                } else {
                    k = 0;
                }

                /* x is now in primary range */
                let hfx = 0.5 * x;
                let hxs = x * hfx;
                let r1 = 1. + hxs * (Q1 + hxs * Q2);
                let t = 3. - r1 * hfx;
                let mut e = hxs * ((r1 - t) / (6. - x * t));
                if k == 0 {
                    /* c is 0 */
                    return x - (x * e - hxs);
                }
                e = x * (e - c) - c;
                e -= hxs;
                /* exp(x) ~ 2^k (x_reduced - e + 1) */
                if k == -1 {
                    return 0.5 * (x - e) - 0.5;
                }
                if k == 1 {
                    if x < -0.25 {
                        return -2. * (e - (x + 0.5));
                    }
                    return 1. + 2. * (x - e);
                }
                let twopk = f32::from_bits(((0x7f + k) << 23) as u32); /* 2^k */
                if (k < 0) || (k > 56) {
                    /* suffice to return exp(x)-1 */
                    let mut y = x - e + 1.;
                    if k == 128 {
                        y = y * 2. * x1p127;
                    } else {
                        y = y * twopk;
                    }
                    return y - 1.;
                }
                let uf = f32::from_bits(((0x7f - k) << 23) as u32); /* 2^-k */
                if k < 23 {
                    (x - e + (1. - uf)) * twopk
                } else {
                    (x - (e + uf) + 1.) * twopk
                }
            }
        }

        mod fabs
        {
            use ::
            {
                *,
            };
            /*
            */
            /// Absolute value (magnitude) (f64).
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn fabs(x: f64) -> f64
            {
                f64::from_bits(x.to_bits() & (u64::MAX / 2))
            }
        }

        mod fabsf
        {
            use ::
            {
                *,
            };
            /*
            */
            /// Absolute value (magnitude) (f32).
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn fabsf(x: f32) -> f32
            {
                f32::from_bits(x.to_bits() & 0x7fffffff)
            }
        }

        mod fdim
        {
            use ::
            {
                *,
            };
            /*
            */
            /// Positive difference (f64).
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn fdim(x: f64, y: f64) -> f64
            {
                if x.is_nan() {
                    x
                } else if y.is_nan() {
                    y
                } else if x > y {
                    x - y
                } else {
                    0.0
                }
            }
        }

        mod fdimf
        {
            use ::
            {
                *,
            };
            /*
            */
            /// Positive difference (f32).
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn fdimf(x: f32, y: f32) -> f32
            {
                if x.is_nan() {
                    x
                } else if y.is_nan() {
                    y
                } else if x > y {
                    x - y
                } else {
                    0.0
                }
            }
        }

        mod floor
        {
            use ::
            {
                *,
            };
            /*
            */
            const TOINT: f64 = 1. / f64::EPSILON;
            /// Floor (f64)
            ///
            /// Finds the nearest integer less than or equal to `x`.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn floor(x: f64) -> f64
            {
                let ui = x.to_bits();
                let e = ((ui >> 52) & 0x7ff) as i32;

                if (e >= 0x3ff + 52) || (x == 0.) {
                    return x;
                }
                /* y = int(x) - x, where int(x) is an integer neighbor of x */
                let y = if (ui >> 63) != 0 {
                    x - TOINT + TOINT - x
                } else {
                    x + TOINT - TOINT - x
                };
                /* special case because of non-nearest rounding modes */
                if e < 0x3ff {
                    force_eval!(y);
                    return if (ui >> 63) != 0 { -1. } else { 0. };
                }
                if y > 0. {
                    x + y - 1.
                } else {
                    x + y
                }
            }

        }

        mod floorf
        {
            use ::
            {
                *,
            };
            /*
            */
            /// Floor (f32)
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn floorf(x: f32) -> f32
            {
                let mut ui = x.to_bits();
                let e = (((ui >> 23) as i32) & 0xff) - 0x7f;

                if e >= 23 {
                    return x;
                }
                if e >= 0 {
                    let m: u32 = 0x007fffff >> e;
                    if (ui & m) == 0 {
                        return x;
                    }
                    force_eval!(x + f32::from_bits(0x7b800000));
                    if ui >> 31 != 0 {
                        ui += m;
                    }
                    ui &= !m;
                } else {
                    force_eval!(x + f32::from_bits(0x7b800000));
                    if ui >> 31 == 0 {
                        ui = 0;
                    } else if ui << 1 != 0 {
                        return -1.0;
                    }
                }
                f32::from_bits(ui)
            }
        }

        mod fma
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::scalbn;

            const ZEROINFNAN: i32 = 0x7ff - 0x3ff - 52 - 1;

            struct Num {
                m: u64,
                e: i32,
                sign: i32,
            }

            fn normalize(x: f64) -> Num {
                let x1p63: f64 = f64::from_bits(0x43e0000000000000); // 0x1p63 === 2 ^ 63

                let mut ix: u64 = x.to_bits();
                let mut e: i32 = (ix >> 52) as i32;
                let sign: i32 = e & 0x800;
                e &= 0x7ff;
                if e == 0 {
                    ix = (x * x1p63).to_bits();
                    e = (ix >> 52) as i32 & 0x7ff;
                    e = if e != 0 { e - 63 } else { 0x800 };
                }
                ix &= (1 << 52) - 1;
                ix |= 1 << 52;
                ix <<= 1;
                e -= 0x3ff + 52 + 1;
                Num { m: ix, e, sign }
            }

            fn mul(x: u64, y: u64) -> (u64, u64) {
                let t1: u64;
                let t2: u64;
                let t3: u64;
                let xlo: u64 = x as u32 as u64;
                let xhi: u64 = x >> 32;
                let ylo: u64 = y as u32 as u64;
                let yhi: u64 = y >> 32;

                t1 = xlo * ylo;
                t2 = xlo * yhi + xhi * ylo;
                t3 = xhi * yhi;
                let lo = t1.wrapping_add(t2 << 32);
                let hi = t3 + (t2 >> 32) + (t1 > lo) as u64;
                (hi, lo)
            }

            /// Floating multiply add (f64)
            ///
            /// Computes `(x*y)+z`, rounded as one ternary operation:
            /// Computes the value (as if) to infinite precision and rounds once to the result format,
            /// according to the rounding mode characterized by the value of FLT_ROUNDS.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn fma(x: f64, y: f64, z: f64) -> f64 {
                let x1p63: f64 = f64::from_bits(0x43e0000000000000); // 0x1p63 === 2 ^ 63
                let x0_ffffff8p_63 = f64::from_bits(0x3bfffffff0000000); // 0x0.ffffff8p-63

                /* normalize so top 10bits and last bit are 0 */
                let nx = normalize(x);
                let ny = normalize(y);
                let nz = normalize(z);

                if nx.e >= ZEROINFNAN || ny.e >= ZEROINFNAN {
                    return x * y + z;
                }
                if nz.e >= ZEROINFNAN {
                    if nz.e > ZEROINFNAN {
                        /* z==0 */
                        return x * y + z;
                    }
                    return z;
                }

                /* mul: r = x*y */
                let zhi: u64;
                let zlo: u64;
                let (mut rhi, mut rlo) = mul(nx.m, ny.m);
                /* either top 20 or 21 bits of rhi and last 2 bits of rlo are 0 */

                /* align exponents */
                let mut e: i32 = nx.e + ny.e;
                let mut d: i32 = nz.e - e;
                /* shift bits z<<=kz, r>>=kr, so kz+kr == d, set e = e+kr (== ez-kz) */
                if d > 0 {
                    if d < 64 {
                        zlo = nz.m << d;
                        zhi = nz.m >> (64 - d);
                    } else {
                        zlo = 0;
                        zhi = nz.m;
                        e = nz.e - 64;
                        d -= 64;
                        if d == 0 {
                        } else if d < 64 {
                            rlo = rhi << (64 - d) | rlo >> d | ((rlo << (64 - d)) != 0) as u64;
                            rhi = rhi >> d;
                        } else {
                            rlo = 1;
                            rhi = 0;
                        }
                    }
                } else {
                    zhi = 0;
                    d = -d;
                    if d == 0 {
                        zlo = nz.m;
                    } else if d < 64 {
                        zlo = nz.m >> d | ((nz.m << (64 - d)) != 0) as u64;
                    } else {
                        zlo = 1;
                    }
                }

                /* add */
                let mut sign: i32 = nx.sign ^ ny.sign;
                let samesign: bool = (sign ^ nz.sign) == 0;
                let mut nonzero: i32 = 1;
                if samesign {
                    /* r += z */
                    rlo = rlo.wrapping_add(zlo);
                    rhi += zhi + (rlo < zlo) as u64;
                } else {
                    /* r -= z */
                    let t = rlo;
                    rlo = rlo.wrapping_sub(zlo);
                    rhi = rhi.wrapping_sub(zhi.wrapping_sub((t < rlo) as u64));
                    if (rhi >> 63) != 0 {
                        rlo = (-(rlo as i64)) as u64;
                        rhi = (-(rhi as i64)) as u64 - (rlo != 0) as u64;
                        sign = (sign == 0) as i32;
                    }
                    nonzero = (rhi != 0) as i32;
                }

                /* set rhi to top 63bit of the result (last bit is sticky) */
                if nonzero != 0 {
                    e += 64;
                    d = rhi.leading_zeros() as i32 - 1;
                    /* note: d > 0 */
                    rhi = rhi << d | rlo >> (64 - d) | ((rlo << d) != 0) as u64;
                } else if rlo != 0 {
                    d = rlo.leading_zeros() as i32 - 1;
                    if d < 0 {
                        rhi = rlo >> 1 | (rlo & 1);
                    } else {
                        rhi = rlo << d;
                    }
                } else {
                    /* exact +-0 */
                    return x * y + z;
                }
                e -= d;

                /* convert to double */
                let mut i: i64 = rhi as i64; /* i is in [1<<62,(1<<63)-1] */
                if sign != 0 {
                    i = -i;
                }
                let mut r: f64 = i as f64; /* |r| is in [0x1p62,0x1p63] */

                if e < -1022 - 62 {
                    /* result is subnormal before rounding */
                    if e == -1022 - 63 {
                        let mut c: f64 = x1p63;
                        if sign != 0 {
                            c = -c;
                        }
                        if r == c {
                            /* min normal after rounding, underflow depends
                            on arch behaviour which can be imitated by
                            a double to float conversion */
                            let fltmin: f32 = (x0_ffffff8p_63 * f32::MIN_POSITIVE as f64 * r) as f32;
                            return f64::MIN_POSITIVE / f32::MIN_POSITIVE as f64 * fltmin as f64;
                        }
                        /* one bit is lost when scaled, add another top bit to
                        only round once at conversion if it is inexact */
                        if (rhi << 53) != 0 {
                            i = (rhi >> 1 | (rhi & 1) | 1 << 62) as i64;
                            if sign != 0 {
                                i = -i;
                            }
                            r = i as f64;
                            r = 2. * r - c; /* remove top bit */

                            /* raise underflow portably, such that it
                            cannot be optimized away */
                            {
                                let tiny: f64 = f64::MIN_POSITIVE / f32::MIN_POSITIVE as f64 * r;
                                r += (tiny * tiny) * (r - r);
                            }
                        }
                    } else {
                        /* only round once when scaled */
                        d = 10;
                        i = ((rhi >> d | ((rhi << (64 - d)) != 0) as u64) << d) as i64;
                        if sign != 0 {
                            i = -i;
                        }
                        r = i as f64;
                    }
                }
                scalbn(r, e)
            }
        }

        mod fmaf
        {
            use ::
            {
                ptr::read_volatile,
                *,
            };
            /*
            */
            use super::fenv::{
                feclearexcept, fegetround, feraiseexcept, fesetround, fetestexcept, FE_INEXACT, FE_TONEAREST,
                FE_TOWARDZERO, FE_UNDERFLOW,
            };

            /*
            * Fused multiply-add: Compute x * y + z with a single rounding error.
            *
            * A double has more than twice as much precision than a float, so
            * direct double-precision arithmetic suffices, except where double
            * rounding occurs.
            */

            /// Floating multiply add (f32)
            ///
            /// Computes `(x*y)+z`, rounded as one ternary operation:
            /// Computes the value (as if) to infinite precision and rounds once to the result format,
            /// according to the rounding mode characterized by the value of FLT_ROUNDS.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn fmaf(x: f32, y: f32, mut z: f32) -> f32
            {
                let xy: f64;
                let mut result: f64;
                let mut ui: u64;
                let e: i32;

                xy = x as f64 * y as f64;
                result = xy + z as f64;
                ui = result.to_bits();
                e = (ui >> 52) as i32 & 0x7ff;
                /* Common case: The double precision result is fine. */
                if (
                    /* not a halfway case */
                    ui & 0x1fffffff) != 0x10000000 ||
                    /* NaN */
                    e == 0x7ff ||
                    /* exact */
                    (result - xy == z as f64 && result - z as f64 == xy) ||
                    /* not round-to-nearest */
                    fegetround() != FE_TONEAREST
                {
                    /*
                        underflow may not be raised correctly, example:
                        fmaf(0x1p-120f, 0x1p-120f, 0x1p-149f)
                    */
                    if e < 0x3ff - 126 && e >= 0x3ff - 149 && fetestexcept(FE_INEXACT) != 0 {
                        feclearexcept(FE_INEXACT);
                        // prevent `xy + vz` from being CSE'd with `xy + z` above
                        let vz: f32 = unsafe { read_volatile(&z) };
                        result = xy + vz as f64;
                        if fetestexcept(FE_INEXACT) != 0 {
                            feraiseexcept(FE_UNDERFLOW);
                        } else {
                            feraiseexcept(FE_INEXACT);
                        }
                    }
                    z = result as f32;
                    return z;
                }

                /*
                * If result is inexact, and exactly halfway between two float values,
                * we need to adjust the low-order bit in the direction of the error.
                */
                fesetround(FE_TOWARDZERO);
                // prevent `vxy + z` from being CSE'd with `xy + z` above
                let vxy: f64 = unsafe { read_volatile(&xy) };
                let mut adjusted_result: f64 = vxy + z as f64;
                fesetround(FE_TONEAREST);
                if result == adjusted_result {
                    ui = adjusted_result.to_bits();
                    ui += 1;
                    adjusted_result = f64::from_bits(ui);
                }
                z = adjusted_result as f32;
                z
            }
        }

        mod fmax
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn fmax(x: f64, y: f64) -> f64
            {
                (if x.is_nan() || x < y { y } else { x }) * 1.0
            }
        }

        mod fmaxf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn fmaxf(x: f32, y: f32) -> f32
            {
                (if x.is_nan() || x < y { y } else { x }) * 1.0
            }
        }

        mod fmin
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn fmin(x: f64, y: f64) -> f64
            {
                (if y.is_nan() || x < y { x } else { y }) * 1.0
            }
        }

        mod fminf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn fminf(x: f32, y: f32) -> f32
            {
                (if y.is_nan() || x < y { x } else { y }) * 1.0
            }
        }

        mod fmod
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn fmod(x: f64, y: f64) -> f64
            {
                let mut uxi = x.to_bits();
                let mut uyi = y.to_bits();
                let mut ex = (uxi >> 52 & 0x7ff) as i64;
                let mut ey = (uyi >> 52 & 0x7ff) as i64;
                let sx = uxi >> 63;
                let mut i;

                if uyi << 1 == 0 || y.is_nan() || ex == 0x7ff {
                    return (x * y) / (x * y);
                }
                if uxi << 1 <= uyi << 1 {
                    if uxi << 1 == uyi << 1 {
                        return 0.0 * x;
                    }
                    return x;
                }

                /* normalize x and y */
                if ex == 0 {
                    i = uxi << 12;
                    while i >> 63 == 0 {
                        ex -= 1;
                        i <<= 1;
                    }
                    uxi <<= -ex + 1;
                } else {
                    uxi &= u64::MAX >> 12;
                    uxi |= 1 << 52;
                }
                if ey == 0 {
                    i = uyi << 12;
                    while i >> 63 == 0 {
                        ey -= 1;
                        i <<= 1;
                    }
                    uyi <<= -ey + 1;
                } else {
                    uyi &= u64::MAX >> 12;
                    uyi |= 1 << 52;
                }

                /* x mod y */
                while ex > ey {
                    i = uxi.wrapping_sub(uyi);
                    if i >> 63 == 0 {
                        if i == 0 {
                            return 0.0 * x;
                        }
                        uxi = i;
                    }
                    uxi <<= 1;
                    ex -= 1;
                }
                i = uxi.wrapping_sub(uyi);
                if i >> 63 == 0 {
                    if i == 0 {
                        return 0.0 * x;
                    }
                    uxi = i;
                }
                while uxi >> 52 == 0 {
                    uxi <<= 1;
                    ex -= 1;
                }

                /* scale result */
                if ex > 0 {
                    uxi -= 1 << 52;
                    uxi |= (ex as u64) << 52;
                } else {
                    uxi >>= -ex + 1;
                }
                uxi |= (sx as u64) << 63;

                f64::from_bits(uxi)
            }
        }

        mod fmodf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn fmodf(x: f32, y: f32) -> f32
            {
                let mut uxi = x.to_bits();
                let mut uyi = y.to_bits();
                let mut ex = (uxi >> 23 & 0xff) as i32;
                let mut ey = (uyi >> 23 & 0xff) as i32;
                let sx = uxi & 0x80000000;
                let mut i;

                if uyi << 1 == 0 || y.is_nan() || ex == 0xff {
                    return (x * y) / (x * y);
                }

                if uxi << 1 <= uyi << 1 {
                    if uxi << 1 == uyi << 1 {
                        return 0.0 * x;
                    }

                    return x;
                }

                /* normalize x and y */
                if ex == 0 {
                    i = uxi << 9;
                    while i >> 31 == 0 {
                        ex -= 1;
                        i <<= 1;
                    }

                    uxi <<= -ex + 1;
                } else {
                    uxi &= u32::MAX >> 9;
                    uxi |= 1 << 23;
                }

                if ey == 0 {
                    i = uyi << 9;
                    while i >> 31 == 0 {
                        ey -= 1;
                        i <<= 1;
                    }

                    uyi <<= -ey + 1;
                } else {
                    uyi &= u32::MAX >> 9;
                    uyi |= 1 << 23;
                }

                /* x mod y */
                while ex > ey {
                    i = uxi.wrapping_sub(uyi);
                    if i >> 31 == 0 {
                        if i == 0 {
                            return 0.0 * x;
                        }
                        uxi = i;
                    }
                    uxi <<= 1;

                    ex -= 1;
                }

                i = uxi.wrapping_sub(uyi);
                if i >> 31 == 0 {
                    if i == 0 {
                        return 0.0 * x;
                    }
                    uxi = i;
                }

                while uxi >> 23 == 0 {
                    uxi <<= 1;
                    ex -= 1;
                }

                /* scale result up */
                if ex > 0 {
                    uxi -= 1 << 23;
                    uxi |= (ex as u32) << 23;
                } else {
                    uxi >>= -ex + 1;
                }
                uxi |= sx;

                f32::from_bits(uxi)
            }
        }

        mod frexp
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn frexp(x: f64) -> (f64, i32)
            {
                let mut y = x.to_bits();
                let ee = ((y >> 52) & 0x7ff) as i32;

                if ee == 0 {
                    if x != 0.0 {
                        let x1p64 = f64::from_bits(0x43f0000000000000);
                        let (x, e) = frexp(x * x1p64);
                        return (x, e - 64);
                    }
                    return (x, 0);
                } else if ee == 0x7ff {
                    return (x, 0);
                }

                let e = ee - 0x3fe;
                y &= 0x800fffffffffffff;
                y |= 0x3fe0000000000000;
                return (f64::from_bits(y), e);
            }
        }

        mod frexpf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn frexpf(x: f32) -> (f32, i32)
            {
                let mut y = x.to_bits();
                let ee: i32 = ((y >> 23) & 0xff) as i32;

                if ee == 0 {
                    if x != 0.0 {
                        let x1p64 = f32::from_bits(0x5f800000);
                        let (x, e) = frexpf(x * x1p64);
                        return (x, e - 64);
                    } else {
                        return (x, 0);
                    }
                } else if ee == 0xff {
                    return (x, 0);
                }

                let e = ee - 0x7e;
                y &= 0x807fffff;
                y |= 0x3f000000;
                (f32::from_bits(y), e)
            }
        }

        mod hypot
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::sqrt;

            const SPLIT: f64 = 134217728. + 1.; // 0x1p27 + 1 === (2 ^ 27) + 1

            fn sq(x: f64) -> (f64, f64) {
                let xh: f64;
                let xl: f64;
                let xc: f64;

                xc = x * SPLIT;
                xh = x - xc + xc;
                xl = x - xh;
                let hi = x * x;
                let lo = xh * xh - hi + 2. * xh * xl + xl * xl;
                (hi, lo)
            }

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn hypot(mut x: f64, mut y: f64) -> f64 {
                let x1p700 = f64::from_bits(0x6bb0000000000000); // 0x1p700 === 2 ^ 700
                let x1p_700 = f64::from_bits(0x1430000000000000); // 0x1p-700 === 2 ^ -700

                let mut uxi = x.to_bits();
                let mut uyi = y.to_bits();
                let uti;
                let ex: i64;
                let ey: i64;
                let mut z: f64;

                /* arrange |x| >= |y| */
                uxi &= -1i64 as u64 >> 1;
                uyi &= -1i64 as u64 >> 1;
                if uxi < uyi {
                    uti = uxi;
                    uxi = uyi;
                    uyi = uti;
                }

                /* special cases */
                ex = (uxi >> 52) as i64;
                ey = (uyi >> 52) as i64;
                x = f64::from_bits(uxi);
                y = f64::from_bits(uyi);
                /* note: hypot(inf,nan) == inf */
                if ey == 0x7ff {
                    return y;
                }
                if ex == 0x7ff || uyi == 0 {
                    return x;
                }
                /* note: hypot(x,y) ~= x + y*y/x/2 with inexact for small y/x */
                /* 64 difference is enough for ld80 double_t */
                if ex - ey > 64 {
                    return x + y;
                }

                /* precise sqrt argument in nearest rounding mode without overflow */
                /* xh*xh must not overflow and xl*xl must not underflow in sq */
                z = 1.;
                if ex > 0x3ff + 510 {
                    z = x1p700;
                    x *= x1p_700;
                    y *= x1p_700;
                } else if ey < 0x3ff - 450 {
                    z = x1p_700;
                    x *= x1p700;
                    y *= x1p700;
                }
                let (hx, lx) = sq(x);
                let (hy, ly) = sq(y);
                z * sqrt(ly + lx + hy + hx)
            }
        }

        mod hypotf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::sqrtf;

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn hypotf(mut x: f32, mut y: f32) -> f32 {
                let x1p90 = f32::from_bits(0x6c800000); // 0x1p90f === 2 ^ 90
                let x1p_90 = f32::from_bits(0x12800000); // 0x1p-90f === 2 ^ -90

                let mut uxi = x.to_bits();
                let mut uyi = y.to_bits();
                let uti;
                let mut z: f32;

                uxi &= -1i32 as u32 >> 1;
                uyi &= -1i32 as u32 >> 1;
                if uxi < uyi {
                    uti = uxi;
                    uxi = uyi;
                    uyi = uti;
                }

                x = f32::from_bits(uxi);
                y = f32::from_bits(uyi);
                if uyi == 0xff << 23 {
                    return y;
                }
                if uxi >= 0xff << 23 || uyi == 0 || uxi - uyi >= 25 << 23 {
                    return x + y;
                }

                z = 1.;
                if uxi >= (0x7f + 60) << 23 {
                    z = x1p90;
                    x *= x1p_90;
                    y *= x1p_90;
                } else if uyi < (0x7f - 60) << 23 {
                    z = x1p_90;
                    x *= x1p90;
                    y *= x1p90;
                }
                z * sqrtf((x as f64 * x as f64 + y as f64 * y as f64) as f32)
            }
        }

        mod ilogb
        {
            use ::
            {
                *,
            };
            /*
            */
            const FP_ILOGBNAN: i32 = -1 - 0x7fffffff;
            const FP_ILOGB0: i32 = FP_ILOGBNAN;

            pub fn ilogb(x: f64) -> i32 {
                let mut i: u64 = x.to_bits();
                let e = ((i >> 52) & 0x7ff) as i32;

                if e == 0 {
                    i <<= 12;
                    if i == 0 {
                        force_eval!(0.0 / 0.0);
                        return FP_ILOGB0;
                    }
                    /* subnormal x */
                    let mut e = -0x3ff;
                    while (i >> 63) == 0 {
                        e -= 1;
                        i <<= 1;
                    }
                    e
                } else if e == 0x7ff {
                    force_eval!(0.0 / 0.0);
                    if (i << 12) != 0 {
                        FP_ILOGBNAN
                    } else {
                        i32::max_value()
                    }
                } else {
                    e - 0x3ff
                }
            }
        }

        mod ilogbf
        {
            use ::
            {
                *,
            };
            /*
            */
            const FP_ILOGBNAN: i32 = -1 - 0x7fffffff;
            const FP_ILOGB0: i32 = FP_ILOGBNAN;

            pub fn ilogbf(x: f32) -> i32 {
                let mut i = x.to_bits();
                let e = ((i >> 23) & 0xff) as i32;

                if e == 0 {
                    i <<= 9;
                    if i == 0 {
                        force_eval!(0.0 / 0.0);
                        return FP_ILOGB0;
                    }
                    /* subnormal x */
                    let mut e = -0x7f;
                    while (i >> 31) == 0 {
                        e -= 1;
                        i <<= 1;
                    }
                    e
                } else if e == 0xff {
                    force_eval!(0.0 / 0.0);
                    if (i << 9) != 0 {
                        FP_ILOGBNAN
                    } else {
                        i32::max_value()
                    }
                } else {
                    e - 0x7f
                }
            }
        }

        mod j0
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{cos, fabs, get_high_word, get_low_word, log, sin, sqrt};
            const INVSQRTPI: f64 = 5.64189583547756279280e-01; /* 0x3FE20DD7, 0x50429B6D */
            const TPI: f64 = 6.36619772367581382433e-01; /* 0x3FE45F30, 0x6DC9C883 */

            /* common method when |x|>=2 */
            fn common(ix: u32, x: f64, y0: bool) -> f64 {
                let s: f64;
                let mut c: f64;
                let mut ss: f64;
                let mut cc: f64;
                let z: f64;

                /*
                * j0(x) = sqrt(2/(pi*x))*(p0(x)*cos(x-pi/4)-q0(x)*sin(x-pi/4))
                * y0(x) = sqrt(2/(pi*x))*(p0(x)*sin(x-pi/4)+q0(x)*cos(x-pi/4))
                *
                * sin(x-pi/4) = (sin(x) - cos(x))/sqrt(2)
                * cos(x-pi/4) = (sin(x) + cos(x))/sqrt(2)
                * sin(x) +- cos(x) = -cos(2x)/(sin(x) -+ cos(x))
                */
                s = sin(x);
                c = cos(x);
                if y0 {
                    c = -c;
                }
                cc = s + c;
                /* avoid overflow in 2*x, big ulp error when x>=0x1p1023 */
                if ix < 0x7fe00000 {
                    ss = s - c;
                    z = -cos(2.0 * x);
                    if s * c < 0.0 {
                        cc = z / ss;
                    } else {
                        ss = z / cc;
                    }
                    if ix < 0x48000000 {
                        if y0 {
                            ss = -ss;
                        }
                        cc = pzero(x) * cc - qzero(x) * ss;
                    }
                }
                return INVSQRTPI * cc / sqrt(x);
            }

            /* R0/S0 on [0, 2.00] */
            const R02: f64 = 1.56249999999999947958e-02; /* 0x3F8FFFFF, 0xFFFFFFFD */
            const R03: f64 = -1.89979294238854721751e-04; /* 0xBF28E6A5, 0xB61AC6E9 */
            const R04: f64 = 1.82954049532700665670e-06; /* 0x3EBEB1D1, 0x0C503919 */
            const R05: f64 = -4.61832688532103189199e-09; /* 0xBE33D5E7, 0x73D63FCE */
            const S01: f64 = 1.56191029464890010492e-02; /* 0x3F8FFCE8, 0x82C8C2A4 */
            const S02: f64 = 1.16926784663337450260e-04; /* 0x3F1EA6D2, 0xDD57DBF4 */
            const S03: f64 = 5.13546550207318111446e-07; /* 0x3EA13B54, 0xCE84D5A9 */
            const S04: f64 = 1.16614003333790000205e-09; /* 0x3E1408BC, 0xF4745D8F */

            pub fn j0(mut x: f64) -> f64 {
                let z: f64;
                let r: f64;
                let s: f64;
                let mut ix: u32;

                ix = get_high_word(x);
                ix &= 0x7fffffff;

                /* j0(+-inf)=0, j0(nan)=nan */
                if ix >= 0x7ff00000 {
                    return 1.0 / (x * x);
                }
                x = fabs(x);

                if ix >= 0x40000000 {
                    /* |x| >= 2 */
                    /* large ulp error near zeros: 2.4, 5.52, 8.6537,.. */
                    return common(ix, x, false);
                }

                /* 1 - x*x/4 + x*x*R(x^2)/S(x^2) */
                if ix >= 0x3f200000 {
                    /* |x| >= 2**-13 */
                    /* up to 4ulp error close to 2 */
                    z = x * x;
                    r = z * (R02 + z * (R03 + z * (R04 + z * R05)));
                    s = 1.0 + z * (S01 + z * (S02 + z * (S03 + z * S04)));
                    return (1.0 + x / 2.0) * (1.0 - x / 2.0) + z * (r / s);
                }

                /* 1 - x*x/4 */
                /* prevent underflow */
                /* inexact should be raised when x!=0, this is not done correctly */
                if ix >= 0x38000000 {
                    /* |x| >= 2**-127 */
                    x = 0.25 * x * x;
                }
                return 1.0 - x;
            }

            const U00: f64 = -7.38042951086872317523e-02; /* 0xBFB2E4D6, 0x99CBD01F */
            const U01: f64 = 1.76666452509181115538e-01; /* 0x3FC69D01, 0x9DE9E3FC */
            const U02: f64 = -1.38185671945596898896e-02; /* 0xBF8C4CE8, 0xB16CFA97 */
            const U03: f64 = 3.47453432093683650238e-04; /* 0x3F36C54D, 0x20B29B6B */
            const U04: f64 = -3.81407053724364161125e-06; /* 0xBECFFEA7, 0x73D25CAD */
            const U05: f64 = 1.95590137035022920206e-08; /* 0x3E550057, 0x3B4EABD4 */
            const U06: f64 = -3.98205194132103398453e-11; /* 0xBDC5E43D, 0x693FB3C8 */
            const V01: f64 = 1.27304834834123699328e-02; /* 0x3F8A1270, 0x91C9C71A */
            const V02: f64 = 7.60068627350353253702e-05; /* 0x3F13ECBB, 0xF578C6C1 */
            const V03: f64 = 2.59150851840457805467e-07; /* 0x3E91642D, 0x7FF202FD */
            const V04: f64 = 4.41110311332675467403e-10; /* 0x3DFE5018, 0x3BD6D9EF */

            pub fn y0(x: f64) -> f64 {
                let z: f64;
                let u: f64;
                let v: f64;
                let ix: u32;
                let lx: u32;

                ix = get_high_word(x);
                lx = get_low_word(x);

                /* y0(nan)=nan, y0(<0)=nan, y0(0)=-inf, y0(inf)=0 */
                if ((ix << 1) | lx) == 0 {
                    return -1.0 / 0.0;
                }
                if (ix >> 31) != 0 {
                    return 0.0 / 0.0;
                }
                if ix >= 0x7ff00000 {
                    return 1.0 / x;
                }

                if ix >= 0x40000000 {
                    /* x >= 2 */
                    /* large ulp errors near zeros: 3.958, 7.086,.. */
                    return common(ix, x, true);
                }

                /* U(x^2)/V(x^2) + (2/pi)*j0(x)*log(x) */
                if ix >= 0x3e400000 {
                    /* x >= 2**-27 */
                    /* large ulp error near the first zero, x ~= 0.89 */
                    z = x * x;
                    u = U00 + z * (U01 + z * (U02 + z * (U03 + z * (U04 + z * (U05 + z * U06)))));
                    v = 1.0 + z * (V01 + z * (V02 + z * (V03 + z * V04)));
                    return u / v + TPI * (j0(x) * log(x));
                }
                return U00 + TPI * log(x);
            }

            /* The asymptotic expansions of pzero is
            *      1 - 9/128 s^2 + 11025/98304 s^4 - ...,  where s = 1/x.
            * For x >= 2, We approximate pzero by
            *      pzero(x) = 1 + (R/S)
            * where  R = pR0 + pR1*s^2 + pR2*s^4 + ... + pR5*s^10
            *        S = 1 + pS0*s^2 + ... + pS4*s^10
            * and
            *      | pzero(x)-1-R/S | <= 2  ** ( -60.26)
            */
            const PR8: [f64; 6] = [
                /* for x in [inf, 8]=1/[0,0.125] */
                0.00000000000000000000e+00,  /* 0x00000000, 0x00000000 */
                -7.03124999999900357484e-02, /* 0xBFB1FFFF, 0xFFFFFD32 */
                -8.08167041275349795626e+00, /* 0xC02029D0, 0xB44FA779 */
                -2.57063105679704847262e+02, /* 0xC0701102, 0x7B19E863 */
                -2.48521641009428822144e+03, /* 0xC0A36A6E, 0xCD4DCAFC */
                -5.25304380490729545272e+03, /* 0xC0B4850B, 0x36CC643D */
            ];
            const PS8: [f64; 5] = [
                1.16534364619668181717e+02, /* 0x405D2233, 0x07A96751 */
                3.83374475364121826715e+03, /* 0x40ADF37D, 0x50596938 */
                4.05978572648472545552e+04, /* 0x40E3D2BB, 0x6EB6B05F */
                1.16752972564375915681e+05, /* 0x40FC810F, 0x8F9FA9BD */
                4.76277284146730962675e+04, /* 0x40E74177, 0x4F2C49DC */
            ];

            const PR5: [f64; 6] = [
                /* for x in [8,4.5454]=1/[0.125,0.22001] */
                -1.14125464691894502584e-11, /* 0xBDA918B1, 0x47E495CC */
                -7.03124940873599280078e-02, /* 0xBFB1FFFF, 0xE69AFBC6 */
                -4.15961064470587782438e+00, /* 0xC010A370, 0xF90C6BBF */
                -6.76747652265167261021e+01, /* 0xC050EB2F, 0x5A7D1783 */
                -3.31231299649172967747e+02, /* 0xC074B3B3, 0x6742CC63 */
                -3.46433388365604912451e+02, /* 0xC075A6EF, 0x28A38BD7 */
            ];
            const PS5: [f64; 5] = [
                6.07539382692300335975e+01, /* 0x404E6081, 0x0C98C5DE */
                1.05125230595704579173e+03, /* 0x40906D02, 0x5C7E2864 */
                5.97897094333855784498e+03, /* 0x40B75AF8, 0x8FBE1D60 */
                9.62544514357774460223e+03, /* 0x40C2CCB8, 0xFA76FA38 */
                2.40605815922939109441e+03, /* 0x40A2CC1D, 0xC70BE864 */
            ];

            const PR3: [f64; 6] = [
                /* for x in [4.547,2.8571]=1/[0.2199,0.35001] */
                -2.54704601771951915620e-09, /* 0xBE25E103, 0x6FE1AA86 */
                -7.03119616381481654654e-02, /* 0xBFB1FFF6, 0xF7C0E24B */
                -2.40903221549529611423e+00, /* 0xC00345B2, 0xAEA48074 */
                -2.19659774734883086467e+01, /* 0xC035F74A, 0x4CB94E14 */
                -5.80791704701737572236e+01, /* 0xC04D0A22, 0x420A1A45 */
                -3.14479470594888503854e+01, /* 0xC03F72AC, 0xA892D80F */
            ];
            const PS3: [f64; 5] = [
                3.58560338055209726349e+01, /* 0x4041ED92, 0x84077DD3 */
                3.61513983050303863820e+02, /* 0x40769839, 0x464A7C0E */
                1.19360783792111533330e+03, /* 0x4092A66E, 0x6D1061D6 */
                1.12799679856907414432e+03, /* 0x40919FFC, 0xB8C39B7E */
                1.73580930813335754692e+02, /* 0x4065B296, 0xFC379081 */
            ];

            const PR2: [f64; 6] = [
                /* for x in [2.8570,2]=1/[0.3499,0.5] */
                -8.87534333032526411254e-08, /* 0xBE77D316, 0xE927026D */
                -7.03030995483624743247e-02, /* 0xBFB1FF62, 0x495E1E42 */
                -1.45073846780952986357e+00, /* 0xBFF73639, 0x8A24A843 */
                -7.63569613823527770791e+00, /* 0xC01E8AF3, 0xEDAFA7F3 */
                -1.11931668860356747786e+01, /* 0xC02662E6, 0xC5246303 */
                -3.23364579351335335033e+00, /* 0xC009DE81, 0xAF8FE70F */
            ];
            const PS2: [f64; 5] = [
                2.22202997532088808441e+01, /* 0x40363865, 0x908B5959 */
                1.36206794218215208048e+02, /* 0x4061069E, 0x0EE8878F */
                2.70470278658083486789e+02, /* 0x4070E786, 0x42EA079B */
                1.53875394208320329881e+02, /* 0x40633C03, 0x3AB6FAFF */
                1.46576176948256193810e+01, /* 0x402D50B3, 0x44391809 */
            ];

            fn pzero(x: f64) -> f64 {
                let p: &[f64; 6];
                let q: &[f64; 5];
                let z: f64;
                let r: f64;
                let s: f64;
                let mut ix: u32;

                ix = get_high_word(x);
                ix &= 0x7fffffff;
                if ix >= 0x40200000 {
                    p = &PR8;
                    q = &PS8;
                } else if ix >= 0x40122E8B {
                    p = &PR5;
                    q = &PS5;
                } else if ix >= 0x4006DB6D {
                    p = &PR3;
                    q = &PS3;
                } else
                /*ix >= 0x40000000*/
                {
                    p = &PR2;
                    q = &PS2;
                }
                z = 1.0 / (x * x);
                r = p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))));
                s = 1.0 + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * q[4]))));
                return 1.0 + r / s;
            }

            /* For x >= 8, the asymptotic expansions of qzero is
            *      -1/8 s + 75/1024 s^3 - ..., where s = 1/x.
            * We approximate pzero by
            *      qzero(x) = s*(-1.25 + (R/S))
            * where  R = qR0 + qR1*s^2 + qR2*s^4 + ... + qR5*s^10
            *        S = 1 + qS0*s^2 + ... + qS5*s^12
            * and
            *      | qzero(x)/s +1.25-R/S | <= 2  ** ( -61.22)
            */
            const QR8: [f64; 6] = [
                /* for x in [inf, 8]=1/[0,0.125] */
                0.00000000000000000000e+00, /* 0x00000000, 0x00000000 */
                7.32421874999935051953e-02, /* 0x3FB2BFFF, 0xFFFFFE2C */
                1.17682064682252693899e+01, /* 0x40278952, 0x5BB334D6 */
                5.57673380256401856059e+02, /* 0x40816D63, 0x15301825 */
                8.85919720756468632317e+03, /* 0x40C14D99, 0x3E18F46D */
                3.70146267776887834771e+04, /* 0x40E212D4, 0x0E901566 */
            ];
            const QS8: [f64; 6] = [
                1.63776026895689824414e+02,  /* 0x406478D5, 0x365B39BC */
                8.09834494656449805916e+03,  /* 0x40BFA258, 0x4E6B0563 */
                1.42538291419120476348e+05,  /* 0x41016652, 0x54D38C3F */
                8.03309257119514397345e+05,  /* 0x412883DA, 0x83A52B43 */
                8.40501579819060512818e+05,  /* 0x4129A66B, 0x28DE0B3D */
                -3.43899293537866615225e+05, /* 0xC114FD6D, 0x2C9530C5 */
            ];

            const QR5: [f64; 6] = [
                /* for x in [8,4.5454]=1/[0.125,0.22001] */
                1.84085963594515531381e-11, /* 0x3DB43D8F, 0x29CC8CD9 */
                7.32421766612684765896e-02, /* 0x3FB2BFFF, 0xD172B04C */
                5.83563508962056953777e+00, /* 0x401757B0, 0xB9953DD3 */
                1.35111577286449829671e+02, /* 0x4060E392, 0x0A8788E9 */
                1.02724376596164097464e+03, /* 0x40900CF9, 0x9DC8C481 */
                1.98997785864605384631e+03, /* 0x409F17E9, 0x53C6E3A6 */
            ];
            const QS5: [f64; 6] = [
                8.27766102236537761883e+01,  /* 0x4054B1B3, 0xFB5E1543 */
                2.07781416421392987104e+03,  /* 0x40A03BA0, 0xDA21C0CE */
                1.88472887785718085070e+04,  /* 0x40D267D2, 0x7B591E6D */
                5.67511122894947329769e+04,  /* 0x40EBB5E3, 0x97E02372 */
                3.59767538425114471465e+04,  /* 0x40E19118, 0x1F7A54A0 */
                -5.35434275601944773371e+03, /* 0xC0B4EA57, 0xBEDBC609 */
            ];

            const QR3: [f64; 6] = [
                /* for x in [4.547,2.8571]=1/[0.2199,0.35001] */
                4.37741014089738620906e-09, /* 0x3E32CD03, 0x6ADECB82 */
                7.32411180042911447163e-02, /* 0x3FB2BFEE, 0x0E8D0842 */
                3.34423137516170720929e+00, /* 0x400AC0FC, 0x61149CF5 */
                4.26218440745412650017e+01, /* 0x40454F98, 0x962DAEDD */
                1.70808091340565596283e+02, /* 0x406559DB, 0xE25EFD1F */
                1.66733948696651168575e+02, /* 0x4064D77C, 0x81FA21E0 */
            ];
            const QS3: [f64; 6] = [
                4.87588729724587182091e+01,  /* 0x40486122, 0xBFE343A6 */
                7.09689221056606015736e+02,  /* 0x40862D83, 0x86544EB3 */
                3.70414822620111362994e+03,  /* 0x40ACF04B, 0xE44DFC63 */
                6.46042516752568917582e+03,  /* 0x40B93C6C, 0xD7C76A28 */
                2.51633368920368957333e+03,  /* 0x40A3A8AA, 0xD94FB1C0 */
                -1.49247451836156386662e+02, /* 0xC062A7EB, 0x201CF40F */
            ];

            const QR2: [f64; 6] = [
                /* for x in [2.8570,2]=1/[0.3499,0.5] */
                1.50444444886983272379e-07, /* 0x3E84313B, 0x54F76BDB */
                7.32234265963079278272e-02, /* 0x3FB2BEC5, 0x3E883E34 */
                1.99819174093815998816e+00, /* 0x3FFFF897, 0xE727779C */
                1.44956029347885735348e+01, /* 0x402CFDBF, 0xAAF96FE5 */
                3.16662317504781540833e+01, /* 0x403FAA8E, 0x29FBDC4A */
                1.62527075710929267416e+01, /* 0x403040B1, 0x71814BB4 */
            ];
            const QS2: [f64; 6] = [
                3.03655848355219184498e+01,  /* 0x403E5D96, 0xF7C07AED */
                2.69348118608049844624e+02,  /* 0x4070D591, 0xE4D14B40 */
                8.44783757595320139444e+02,  /* 0x408A6645, 0x22B3BF22 */
                8.82935845112488550512e+02,  /* 0x408B977C, 0x9C5CC214 */
                2.12666388511798828631e+02,  /* 0x406A9553, 0x0E001365 */
                -5.31095493882666946917e+00, /* 0xC0153E6A, 0xF8B32931 */
            ];

            fn qzero(x: f64) -> f64 {
                let p: &[f64; 6];
                let q: &[f64; 6];
                let s: f64;
                let r: f64;
                let z: f64;
                let mut ix: u32;

                ix = get_high_word(x);
                ix &= 0x7fffffff;
                if ix >= 0x40200000 {
                    p = &QR8;
                    q = &QS8;
                } else if ix >= 0x40122E8B {
                    p = &QR5;
                    q = &QS5;
                } else if ix >= 0x4006DB6D {
                    p = &QR3;
                    q = &QS3;
                } else
                /*ix >= 0x40000000*/
                {
                    p = &QR2;
                    q = &QS2;
                }
                z = 1.0 / (x * x);
                r = p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))));
                s = 1.0 + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * (q[4] + z * q[5])))));
                return (-0.125 + r / s) / x;
            }
        }

        mod j0f
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{cosf, fabsf, logf, sinf, sqrtf};

            const INVSQRTPI: f32 = 5.6418961287e-01; /* 0x3f106ebb */
            const TPI: f32 = 6.3661974669e-01; /* 0x3f22f983 */

            fn common(ix: u32, x: f32, y0: bool) -> f32 {
                let z: f32;
                let s: f32;
                let mut c: f32;
                let mut ss: f32;
                let mut cc: f32;
                /*
                * j0(x) = 1/sqrt(pi) * (P(0,x)*cc - Q(0,x)*ss) / sqrt(x)
                * y0(x) = 1/sqrt(pi) * (P(0,x)*ss + Q(0,x)*cc) / sqrt(x)
                */
                s = sinf(x);
                c = cosf(x);
                if y0 {
                    c = -c;
                }
                cc = s + c;
                if ix < 0x7f000000 {
                    ss = s - c;
                    z = -cosf(2.0 * x);
                    if s * c < 0.0 {
                        cc = z / ss;
                    } else {
                        ss = z / cc;
                    }
                    if ix < 0x58800000 {
                        if y0 {
                            ss = -ss;
                        }
                        cc = pzerof(x) * cc - qzerof(x) * ss;
                    }
                }
                return INVSQRTPI * cc / sqrtf(x);
            }

            /* R0/S0 on [0, 2.00] */
            const R02: f32 = 1.5625000000e-02; /* 0x3c800000 */
            const R03: f32 = -1.8997929874e-04; /* 0xb947352e */
            const R04: f32 = 1.8295404516e-06; /* 0x35f58e88 */
            const R05: f32 = -4.6183270541e-09; /* 0xb19eaf3c */
            const S01: f32 = 1.5619102865e-02; /* 0x3c7fe744 */
            const S02: f32 = 1.1692678527e-04; /* 0x38f53697 */
            const S03: f32 = 5.1354652442e-07; /* 0x3509daa6 */
            const S04: f32 = 1.1661400734e-09; /* 0x30a045e8 */

            pub fn j0f(mut x: f32) -> f32 {
                let z: f32;
                let r: f32;
                let s: f32;
                let mut ix: u32;

                ix = x.to_bits();
                ix &= 0x7fffffff;
                if ix >= 0x7f800000 {
                    return 1.0 / (x * x);
                }
                x = fabsf(x);

                if ix >= 0x40000000 {
                    /* |x| >= 2 */
                    /* large ulp error near zeros */
                    return common(ix, x, false);
                }
                if ix >= 0x3a000000 {
                    /* |x| >= 2**-11 */
                    /* up to 4ulp error near 2 */
                    z = x * x;
                    r = z * (R02 + z * (R03 + z * (R04 + z * R05)));
                    s = 1.0 + z * (S01 + z * (S02 + z * (S03 + z * S04)));
                    return (1.0 + x / 2.0) * (1.0 - x / 2.0) + z * (r / s);
                }
                if ix >= 0x21800000 {
                    /* |x| >= 2**-60 */
                    x = 0.25 * x * x;
                }
                return 1.0 - x;
            }

            const U00: f32 = -7.3804296553e-02; /* 0xbd9726b5 */
            const U01: f32 = 1.7666645348e-01; /* 0x3e34e80d */
            const U02: f32 = -1.3818567619e-02; /* 0xbc626746 */
            const U03: f32 = 3.4745343146e-04; /* 0x39b62a69 */
            const U04: f32 = -3.8140706238e-06; /* 0xb67ff53c */
            const U05: f32 = 1.9559013964e-08; /* 0x32a802ba */
            const U06: f32 = -3.9820518410e-11; /* 0xae2f21eb */
            const V01: f32 = 1.2730483897e-02; /* 0x3c509385 */
            const V02: f32 = 7.6006865129e-05; /* 0x389f65e0 */
            const V03: f32 = 2.5915085189e-07; /* 0x348b216c */
            const V04: f32 = 4.4111031494e-10; /* 0x2ff280c2 */

            pub fn y0f(x: f32) -> f32 {
                let z: f32;
                let u: f32;
                let v: f32;
                let ix: u32;

                ix = x.to_bits();
                if (ix & 0x7fffffff) == 0 {
                    return -1.0 / 0.0;
                }
                if (ix >> 31) != 0 {
                    return 0.0 / 0.0;
                }
                if ix >= 0x7f800000 {
                    return 1.0 / x;
                }
                if ix >= 0x40000000 {
                    /* |x| >= 2.0 */
                    /* large ulp error near zeros */
                    return common(ix, x, true);
                }
                if ix >= 0x39000000 {
                    /* x >= 2**-13 */
                    /* large ulp error at x ~= 0.89 */
                    z = x * x;
                    u = U00 + z * (U01 + z * (U02 + z * (U03 + z * (U04 + z * (U05 + z * U06)))));
                    v = 1.0 + z * (V01 + z * (V02 + z * (V03 + z * V04)));
                    return u / v + TPI * (j0f(x) * logf(x));
                }
                return U00 + TPI * logf(x);
            }

            /* The asymptotic expansions of pzero is
            *      1 - 9/128 s^2 + 11025/98304 s^4 - ...,  where s = 1/x.
            * For x >= 2, We approximate pzero by
            *      pzero(x) = 1 + (R/S)
            * where  R = pR0 + pR1*s^2 + pR2*s^4 + ... + pR5*s^10
            *        S = 1 + pS0*s^2 + ... + pS4*s^10
            * and
            *      | pzero(x)-1-R/S | <= 2  ** ( -60.26)
            */
            const PR8: [f32; 6] = [
                /* for x in [inf, 8]=1/[0,0.125] */
                0.0000000000e+00,  /* 0x00000000 */
                -7.0312500000e-02, /* 0xbd900000 */
                -8.0816707611e+00, /* 0xc1014e86 */
                -2.5706311035e+02, /* 0xc3808814 */
                -2.4852163086e+03, /* 0xc51b5376 */
                -5.2530439453e+03, /* 0xc5a4285a */
            ];
            const PS8: [f32; 5] = [
                1.1653436279e+02, /* 0x42e91198 */
                3.8337448730e+03, /* 0x456f9beb */
                4.0597855469e+04, /* 0x471e95db */
                1.1675296875e+05, /* 0x47e4087c */
                4.7627726562e+04, /* 0x473a0bba */
            ];
            const PR5: [f32; 6] = [
                /* for x in [8,4.5454]=1/[0.125,0.22001] */
                -1.1412546255e-11, /* 0xad48c58a */
                -7.0312492549e-02, /* 0xbd8fffff */
                -4.1596107483e+00, /* 0xc0851b88 */
                -6.7674766541e+01, /* 0xc287597b */
                -3.3123129272e+02, /* 0xc3a59d9b */
                -3.4643338013e+02, /* 0xc3ad3779 */
            ];
            const PS5: [f32; 5] = [
                6.0753936768e+01, /* 0x42730408 */
                1.0512523193e+03, /* 0x44836813 */
                5.9789707031e+03, /* 0x45bad7c4 */
                9.6254453125e+03, /* 0x461665c8 */
                2.4060581055e+03, /* 0x451660ee */
            ];

            const PR3: [f32; 6] = [
                /* for x in [4.547,2.8571]=1/[0.2199,0.35001] */
                -2.5470459075e-09, /* 0xb12f081b */
                -7.0311963558e-02, /* 0xbd8fffb8 */
                -2.4090321064e+00, /* 0xc01a2d95 */
                -2.1965976715e+01, /* 0xc1afba52 */
                -5.8079170227e+01, /* 0xc2685112 */
                -3.1447946548e+01, /* 0xc1fb9565 */
            ];
            const PS3: [f32; 5] = [
                3.5856033325e+01, /* 0x420f6c94 */
                3.6151397705e+02, /* 0x43b4c1ca */
                1.1936077881e+03, /* 0x44953373 */
                1.1279968262e+03, /* 0x448cffe6 */
                1.7358093262e+02, /* 0x432d94b8 */
            ];

            const PR2: [f32; 6] = [
                /* for x in [2.8570,2]=1/[0.3499,0.5] */
                -8.8753431271e-08, /* 0xb3be98b7 */
                -7.0303097367e-02, /* 0xbd8ffb12 */
                -1.4507384300e+00, /* 0xbfb9b1cc */
                -7.6356959343e+00, /* 0xc0f4579f */
                -1.1193166733e+01, /* 0xc1331736 */
                -3.2336456776e+00, /* 0xc04ef40d */
            ];
            const PS2: [f32; 5] = [
                2.2220300674e+01, /* 0x41b1c32d */
                1.3620678711e+02, /* 0x430834f0 */
                2.7047027588e+02, /* 0x43873c32 */
                1.5387539673e+02, /* 0x4319e01a */
                1.4657617569e+01, /* 0x416a859a */
            ];

            fn pzerof(x: f32) -> f32 {
                let p: &[f32; 6];
                let q: &[f32; 5];
                let z: f32;
                let r: f32;
                let s: f32;
                let mut ix: u32;

                ix = x.to_bits();
                ix &= 0x7fffffff;
                if ix >= 0x41000000 {
                    p = &PR8;
                    q = &PS8;
                } else if ix >= 0x409173eb {
                    p = &PR5;
                    q = &PS5;
                } else if ix >= 0x4036d917 {
                    p = &PR3;
                    q = &PS3;
                } else
                /*ix >= 0x40000000*/
                {
                    p = &PR2;
                    q = &PS2;
                }
                z = 1.0 / (x * x);
                r = p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))));
                s = 1.0 + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * q[4]))));
                return 1.0 + r / s;
            }

            /* For x >= 8, the asymptotic expansions of qzero is
            *      -1/8 s + 75/1024 s^3 - ..., where s = 1/x.
            * We approximate pzero by
            *      qzero(x) = s*(-1.25 + (R/S))
            * where  R = qR0 + qR1*s^2 + qR2*s^4 + ... + qR5*s^10
            *        S = 1 + qS0*s^2 + ... + qS5*s^12
            * and
            *      | qzero(x)/s +1.25-R/S | <= 2  ** ( -61.22)
            */
            const QR8: [f32; 6] = [
                /* for x in [inf, 8]=1/[0,0.125] */
                0.0000000000e+00, /* 0x00000000 */
                7.3242187500e-02, /* 0x3d960000 */
                1.1768206596e+01, /* 0x413c4a93 */
                5.5767340088e+02, /* 0x440b6b19 */
                8.8591972656e+03, /* 0x460a6cca */
                3.7014625000e+04, /* 0x471096a0 */
            ];
            const QS8: [f32; 6] = [
                1.6377603149e+02,  /* 0x4323c6aa */
                8.0983447266e+03,  /* 0x45fd12c2 */
                1.4253829688e+05,  /* 0x480b3293 */
                8.0330925000e+05,  /* 0x49441ed4 */
                8.4050156250e+05,  /* 0x494d3359 */
                -3.4389928125e+05, /* 0xc8a7eb69 */
            ];

            const QR5: [f32; 6] = [
                /* for x in [8,4.5454]=1/[0.125,0.22001] */
                1.8408595828e-11, /* 0x2da1ec79 */
                7.3242180049e-02, /* 0x3d95ffff */
                5.8356351852e+00, /* 0x40babd86 */
                1.3511157227e+02, /* 0x43071c90 */
                1.0272437744e+03, /* 0x448067cd */
                1.9899779053e+03, /* 0x44f8bf4b */
            ];
            const QS5: [f32; 6] = [
                8.2776611328e+01,  /* 0x42a58da0 */
                2.0778142090e+03,  /* 0x4501dd07 */
                1.8847289062e+04,  /* 0x46933e94 */
                5.6751113281e+04,  /* 0x475daf1d */
                3.5976753906e+04,  /* 0x470c88c1 */
                -5.3543427734e+03, /* 0xc5a752be */
            ];

            const QR3: [f32; 6] = [
                /* for x in [4.547,2.8571]=1/[0.2199,0.35001] */
                4.3774099900e-09, /* 0x3196681b */
                7.3241114616e-02, /* 0x3d95ff70 */
                3.3442313671e+00, /* 0x405607e3 */
                4.2621845245e+01, /* 0x422a7cc5 */
                1.7080809021e+02, /* 0x432acedf */
                1.6673394775e+02, /* 0x4326bbe4 */
            ];
            const QS3: [f32; 6] = [
                4.8758872986e+01,  /* 0x42430916 */
                7.0968920898e+02,  /* 0x44316c1c */
                3.7041481934e+03,  /* 0x4567825f */
                6.4604252930e+03,  /* 0x45c9e367 */
                2.5163337402e+03,  /* 0x451d4557 */
                -1.4924745178e+02, /* 0xc3153f59 */
            ];

            const QR2: [f32; 6] = [
                /* for x in [2.8570,2]=1/[0.3499,0.5] */
                1.5044444979e-07, /* 0x342189db */
                7.3223426938e-02, /* 0x3d95f62a */
                1.9981917143e+00, /* 0x3fffc4bf */
                1.4495602608e+01, /* 0x4167edfd */
                3.1666231155e+01, /* 0x41fd5471 */
                1.6252708435e+01, /* 0x4182058c */
            ];
            const QS2: [f32; 6] = [
                3.0365585327e+01,  /* 0x41f2ecb8 */
                2.6934811401e+02,  /* 0x4386ac8f */
                8.4478375244e+02,  /* 0x44533229 */
                8.8293585205e+02,  /* 0x445cbbe5 */
                2.1266638184e+02,  /* 0x4354aa98 */
                -5.3109550476e+00, /* 0xc0a9f358 */
            ];

            fn qzerof(x: f32) -> f32 {
                let p: &[f32; 6];
                let q: &[f32; 6];
                let s: f32;
                let r: f32;
                let z: f32;
                let mut ix: u32;

                ix = x.to_bits();
                ix &= 0x7fffffff;
                if ix >= 0x41000000 {
                    p = &QR8;
                    q = &QS8;
                } else if ix >= 0x409173eb {
                    p = &QR5;
                    q = &QS5;
                } else if ix >= 0x4036d917 {
                    p = &QR3;
                    q = &QS3;
                } else
                /*ix >= 0x40000000*/
                {
                    p = &QR2;
                    q = &QS2;
                }
                z = 1.0 / (x * x);
                r = p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))));
                s = 1.0 + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * (q[4] + z * q[5])))));
                return (-0.125 + r / s) / x;
            }
        }

        mod j1
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{cos, fabs, get_high_word, get_low_word, log, sin, sqrt};

            const INVSQRTPI: f64 = 5.64189583547756279280e-01; /* 0x3FE20DD7, 0x50429B6D */
            const TPI: f64 = 6.36619772367581382433e-01; /* 0x3FE45F30, 0x6DC9C883 */

            fn common(ix: u32, x: f64, y1: bool, sign: bool) -> f64 {
                let z: f64;
                let mut s: f64;
                let c: f64;
                let mut ss: f64;
                let mut cc: f64;

                /*
                * j1(x) = sqrt(2/(pi*x))*(p1(x)*cos(x-3pi/4)-q1(x)*sin(x-3pi/4))
                * y1(x) = sqrt(2/(pi*x))*(p1(x)*sin(x-3pi/4)+q1(x)*cos(x-3pi/4))
                *
                * sin(x-3pi/4) = -(sin(x) + cos(x))/sqrt(2)
                * cos(x-3pi/4) = (sin(x) - cos(x))/sqrt(2)
                * sin(x) +- cos(x) = -cos(2x)/(sin(x) -+ cos(x))
                */
                s = sin(x);
                if y1 {
                    s = -s;
                }
                c = cos(x);
                cc = s - c;
                if ix < 0x7fe00000 {
                    /* avoid overflow in 2*x */
                    ss = -s - c;
                    z = cos(2.0 * x);
                    if s * c > 0.0 {
                        cc = z / ss;
                    } else {
                        ss = z / cc;
                    }
                    if ix < 0x48000000 {
                        if y1 {
                            ss = -ss;
                        }
                        cc = pone(x) * cc - qone(x) * ss;
                    }
                }
                if sign {
                    cc = -cc;
                }
                return INVSQRTPI * cc / sqrt(x);
            }

            /* R0/S0 on [0,2] */
            const R00: f64 = -6.25000000000000000000e-02; /* 0xBFB00000, 0x00000000 */
            const R01: f64 = 1.40705666955189706048e-03; /* 0x3F570D9F, 0x98472C61 */
            const R02: f64 = -1.59955631084035597520e-05; /* 0xBEF0C5C6, 0xBA169668 */
            const R03: f64 = 4.96727999609584448412e-08; /* 0x3E6AAAFA, 0x46CA0BD9 */
            const S01: f64 = 1.91537599538363460805e-02; /* 0x3F939D0B, 0x12637E53 */
            const S02: f64 = 1.85946785588630915560e-04; /* 0x3F285F56, 0xB9CDF664 */
            const S03: f64 = 1.17718464042623683263e-06; /* 0x3EB3BFF8, 0x333F8498 */
            const S04: f64 = 5.04636257076217042715e-09; /* 0x3E35AC88, 0xC97DFF2C */
            const S05: f64 = 1.23542274426137913908e-11; /* 0x3DAB2ACF, 0xCFB97ED8 */

            pub fn j1(x: f64) -> f64 {
                let mut z: f64;
                let r: f64;
                let s: f64;
                let mut ix: u32;
                let sign: bool;

                ix = get_high_word(x);
                sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;
                if ix >= 0x7ff00000 {
                    return 1.0 / (x * x);
                }
                if ix >= 0x40000000 {
                    /* |x| >= 2 */
                    return common(ix, fabs(x), false, sign);
                }
                if ix >= 0x38000000 {
                    /* |x| >= 2**-127 */
                    z = x * x;
                    r = z * (R00 + z * (R01 + z * (R02 + z * R03)));
                    s = 1.0 + z * (S01 + z * (S02 + z * (S03 + z * (S04 + z * S05))));
                    z = r / s;
                } else {
                    /* avoid underflow, raise inexact if x!=0 */
                    z = x;
                }
                return (0.5 + z) * x;
            }

            const U0: [f64; 5] = [
                -1.96057090646238940668e-01, /* 0xBFC91866, 0x143CBC8A */
                5.04438716639811282616e-02,  /* 0x3FA9D3C7, 0x76292CD1 */
                -1.91256895875763547298e-03, /* 0xBF5F55E5, 0x4844F50F */
                2.35252600561610495928e-05,  /* 0x3EF8AB03, 0x8FA6B88E */
                -9.19099158039878874504e-08, /* 0xBE78AC00, 0x569105B8 */
            ];
            const V0: [f64; 5] = [
                1.99167318236649903973e-02, /* 0x3F94650D, 0x3F4DA9F0 */
                2.02552581025135171496e-04, /* 0x3F2A8C89, 0x6C257764 */
                1.35608801097516229404e-06, /* 0x3EB6C05A, 0x894E8CA6 */
                6.22741452364621501295e-09, /* 0x3E3ABF1D, 0x5BA69A86 */
                1.66559246207992079114e-11, /* 0x3DB25039, 0xDACA772A */
            ];

            pub fn y1(x: f64) -> f64 {
                let z: f64;
                let u: f64;
                let v: f64;
                let ix: u32;
                let lx: u32;

                ix = get_high_word(x);
                lx = get_low_word(x);

                /* y1(nan)=nan, y1(<0)=nan, y1(0)=-inf, y1(inf)=0 */
                if (ix << 1 | lx) == 0 {
                    return -1.0 / 0.0;
                }
                if (ix >> 31) != 0 {
                    return 0.0 / 0.0;
                }
                if ix >= 0x7ff00000 {
                    return 1.0 / x;
                }

                if ix >= 0x40000000 {
                    /* x >= 2 */
                    return common(ix, x, true, false);
                }
                if ix < 0x3c900000 {
                    /* x < 2**-54 */
                    return -TPI / x;
                }
                z = x * x;
                u = U0[0] + z * (U0[1] + z * (U0[2] + z * (U0[3] + z * U0[4])));
                v = 1.0 + z * (V0[0] + z * (V0[1] + z * (V0[2] + z * (V0[3] + z * V0[4]))));
                return x * (u / v) + TPI * (j1(x) * log(x) - 1.0 / x);
            }

            /* For x >= 8, the asymptotic expansions of pone is
            *      1 + 15/128 s^2 - 4725/2^15 s^4 - ...,   where s = 1/x.
            * We approximate pone by
            *      pone(x) = 1 + (R/S)
            * where  R = pr0 + pr1*s^2 + pr2*s^4 + ... + pr5*s^10
            *        S = 1 + ps0*s^2 + ... + ps4*s^10
            * and
            *      | pone(x)-1-R/S | <= 2  ** ( -60.06)
            */

            const PR8: [f64; 6] = [
                /* for x in [inf, 8]=1/[0,0.125] */
                0.00000000000000000000e+00, /* 0x00000000, 0x00000000 */
                1.17187499999988647970e-01, /* 0x3FBDFFFF, 0xFFFFFCCE */
                1.32394806593073575129e+01, /* 0x402A7A9D, 0x357F7FCE */
                4.12051854307378562225e+02, /* 0x4079C0D4, 0x652EA590 */
                3.87474538913960532227e+03, /* 0x40AE457D, 0xA3A532CC */
                7.91447954031891731574e+03, /* 0x40BEEA7A, 0xC32782DD */
            ];
            const PS8: [f64; 5] = [
                1.14207370375678408436e+02, /* 0x405C8D45, 0x8E656CAC */
                3.65093083420853463394e+03, /* 0x40AC85DC, 0x964D274F */
                3.69562060269033463555e+04, /* 0x40E20B86, 0x97C5BB7F */
                9.76027935934950801311e+04, /* 0x40F7D42C, 0xB28F17BB */
                3.08042720627888811578e+04, /* 0x40DE1511, 0x697A0B2D */
            ];

            const PR5: [f64; 6] = [
                /* for x in [8,4.5454]=1/[0.125,0.22001] */
                1.31990519556243522749e-11, /* 0x3DAD0667, 0xDAE1CA7D */
                1.17187493190614097638e-01, /* 0x3FBDFFFF, 0xE2C10043 */
                6.80275127868432871736e+00, /* 0x401B3604, 0x6E6315E3 */
                1.08308182990189109773e+02, /* 0x405B13B9, 0x452602ED */
                5.17636139533199752805e+02, /* 0x40802D16, 0xD052D649 */
                5.28715201363337541807e+02, /* 0x408085B8, 0xBB7E0CB7 */
            ];
            const PS5: [f64; 5] = [
                5.92805987221131331921e+01, /* 0x404DA3EA, 0xA8AF633D */
                9.91401418733614377743e+02, /* 0x408EFB36, 0x1B066701 */
                5.35326695291487976647e+03, /* 0x40B4E944, 0x5706B6FB */
                7.84469031749551231769e+03, /* 0x40BEA4B0, 0xB8A5BB15 */
                1.50404688810361062679e+03, /* 0x40978030, 0x036F5E51 */
            ];

            const PR3: [f64; 6] = [
                3.02503916137373618024e-09, /* 0x3E29FC21, 0xA7AD9EDD */
                1.17186865567253592491e-01, /* 0x3FBDFFF5, 0x5B21D17B */
                3.93297750033315640650e+00, /* 0x400F76BC, 0xE85EAD8A */
                3.51194035591636932736e+01, /* 0x40418F48, 0x9DA6D129 */
                9.10550110750781271918e+01, /* 0x4056C385, 0x4D2C1837 */
                4.85590685197364919645e+01, /* 0x4048478F, 0x8EA83EE5 */
            ];
            const PS3: [f64; 5] = [
                3.47913095001251519989e+01, /* 0x40416549, 0xA134069C */
                3.36762458747825746741e+02, /* 0x40750C33, 0x07F1A75F */
                1.04687139975775130551e+03, /* 0x40905B7C, 0x5037D523 */
                8.90811346398256432622e+02, /* 0x408BD67D, 0xA32E31E9 */
                1.03787932439639277504e+02, /* 0x4059F26D, 0x7C2EED53 */
            ];

            const PR2: [f64; 6] = [
                /* for x in [2.8570,2]=1/[0.3499,0.5] */
                1.07710830106873743082e-07, /* 0x3E7CE9D4, 0xF65544F4 */
                1.17176219462683348094e-01, /* 0x3FBDFF42, 0xBE760D83 */
                2.36851496667608785174e+00, /* 0x4002F2B7, 0xF98FAEC0 */
                1.22426109148261232917e+01, /* 0x40287C37, 0x7F71A964 */
                1.76939711271687727390e+01, /* 0x4031B1A8, 0x177F8EE2 */
                5.07352312588818499250e+00, /* 0x40144B49, 0xA574C1FE */
            ];
            const PS2: [f64; 5] = [
                2.14364859363821409488e+01, /* 0x40356FBD, 0x8AD5ECDC */
                1.25290227168402751090e+02, /* 0x405F5293, 0x14F92CD5 */
                2.32276469057162813669e+02, /* 0x406D08D8, 0xD5A2DBD9 */
                1.17679373287147100768e+02, /* 0x405D6B7A, 0xDA1884A9 */
                8.36463893371618283368e+00, /* 0x4020BAB1, 0xF44E5192 */
            ];

            fn pone(x: f64) -> f64 {
                let p: &[f64; 6];
                let q: &[f64; 5];
                let z: f64;
                let r: f64;
                let s: f64;
                let mut ix: u32;

                ix = get_high_word(x);
                ix &= 0x7fffffff;
                if ix >= 0x40200000 {
                    p = &PR8;
                    q = &PS8;
                } else if ix >= 0x40122E8B {
                    p = &PR5;
                    q = &PS5;
                } else if ix >= 0x4006DB6D {
                    p = &PR3;
                    q = &PS3;
                } else
                /*ix >= 0x40000000*/
                {
                    p = &PR2;
                    q = &PS2;
                }
                z = 1.0 / (x * x);
                r = p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))));
                s = 1.0 + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * q[4]))));
                return 1.0 + r / s;
            }

            /* For x >= 8, the asymptotic expansions of qone is
            *      3/8 s - 105/1024 s^3 - ..., where s = 1/x.
            * We approximate pone by
            *      qone(x) = s*(0.375 + (R/S))
            * where  R = qr1*s^2 + qr2*s^4 + ... + qr5*s^10
            *        S = 1 + qs1*s^2 + ... + qs6*s^12
            * and
            *      | qone(x)/s -0.375-R/S | <= 2  ** ( -61.13)
            */

            const QR8: [f64; 6] = [
                /* for x in [inf, 8]=1/[0,0.125] */
                0.00000000000000000000e+00,  /* 0x00000000, 0x00000000 */
                -1.02539062499992714161e-01, /* 0xBFBA3FFF, 0xFFFFFDF3 */
                -1.62717534544589987888e+01, /* 0xC0304591, 0xA26779F7 */
                -7.59601722513950107896e+02, /* 0xC087BCD0, 0x53E4B576 */
                -1.18498066702429587167e+04, /* 0xC0C724E7, 0x40F87415 */
                -4.84385124285750353010e+04, /* 0xC0E7A6D0, 0x65D09C6A */
            ];
            const QS8: [f64; 6] = [
                1.61395369700722909556e+02,  /* 0x40642CA6, 0xDE5BCDE5 */
                7.82538599923348465381e+03,  /* 0x40BE9162, 0xD0D88419 */
                1.33875336287249578163e+05,  /* 0x4100579A, 0xB0B75E98 */
                7.19657723683240939863e+05,  /* 0x4125F653, 0x72869C19 */
                6.66601232617776375264e+05,  /* 0x412457D2, 0x7719AD5C */
                -2.94490264303834643215e+05, /* 0xC111F969, 0x0EA5AA18 */
            ];

            const QR5: [f64; 6] = [
                /* for x in [8,4.5454]=1/[0.125,0.22001] */
                -2.08979931141764104297e-11, /* 0xBDB6FA43, 0x1AA1A098 */
                -1.02539050241375426231e-01, /* 0xBFBA3FFF, 0xCB597FEF */
                -8.05644828123936029840e+00, /* 0xC0201CE6, 0xCA03AD4B */
                -1.83669607474888380239e+02, /* 0xC066F56D, 0x6CA7B9B0 */
                -1.37319376065508163265e+03, /* 0xC09574C6, 0x6931734F */
                -2.61244440453215656817e+03, /* 0xC0A468E3, 0x88FDA79D */
            ];
            const QS5: [f64; 6] = [
                8.12765501384335777857e+01,  /* 0x405451B2, 0xFF5A11B2 */
                1.99179873460485964642e+03,  /* 0x409F1F31, 0xE77BF839 */
                1.74684851924908907677e+04,  /* 0x40D10F1F, 0x0D64CE29 */
                4.98514270910352279316e+04,  /* 0x40E8576D, 0xAABAD197 */
                2.79480751638918118260e+04,  /* 0x40DB4B04, 0xCF7C364B */
                -4.71918354795128470869e+03, /* 0xC0B26F2E, 0xFCFFA004 */
            ];

            const QR3: [f64; 6] = [
                -5.07831226461766561369e-09, /* 0xBE35CFA9, 0xD38FC84F */
                -1.02537829820837089745e-01, /* 0xBFBA3FEB, 0x51AEED54 */
                -4.61011581139473403113e+00, /* 0xC01270C2, 0x3302D9FF */
                -5.78472216562783643212e+01, /* 0xC04CEC71, 0xC25D16DA */
                -2.28244540737631695038e+02, /* 0xC06C87D3, 0x4718D55F */
                -2.19210128478909325622e+02, /* 0xC06B66B9, 0x5F5C1BF6 */
            ];
            const QS3: [f64; 6] = [
                4.76651550323729509273e+01,  /* 0x4047D523, 0xCCD367E4 */
                6.73865112676699709482e+02,  /* 0x40850EEB, 0xC031EE3E */
                3.38015286679526343505e+03,  /* 0x40AA684E, 0x448E7C9A */
                5.54772909720722782367e+03,  /* 0x40B5ABBA, 0xA61D54A6 */
                1.90311919338810798763e+03,  /* 0x409DBC7A, 0x0DD4DF4B */
                -1.35201191444307340817e+02, /* 0xC060E670, 0x290A311F */
            ];

            const QR2: [f64; 6] = [
                /* for x in [2.8570,2]=1/[0.3499,0.5] */
                -1.78381727510958865572e-07, /* 0xBE87F126, 0x44C626D2 */
                -1.02517042607985553460e-01, /* 0xBFBA3E8E, 0x9148B010 */
                -2.75220568278187460720e+00, /* 0xC0060484, 0x69BB4EDA */
                -1.96636162643703720221e+01, /* 0xC033A9E2, 0xC168907F */
                -4.23253133372830490089e+01, /* 0xC04529A3, 0xDE104AAA */
                -2.13719211703704061733e+01, /* 0xC0355F36, 0x39CF6E52 */
            ];
            const QS2: [f64; 6] = [
                2.95333629060523854548e+01,  /* 0x403D888A, 0x78AE64FF */
                2.52981549982190529136e+02,  /* 0x406F9F68, 0xDB821CBA */
                7.57502834868645436472e+02,  /* 0x4087AC05, 0xCE49A0F7 */
                7.39393205320467245656e+02,  /* 0x40871B25, 0x48D4C029 */
                1.55949003336666123687e+02,  /* 0x40637E5E, 0x3C3ED8D4 */
                -4.95949898822628210127e+00, /* 0xC013D686, 0xE71BE86B */
            ];

            fn qone(x: f64) -> f64 {
                let p: &[f64; 6];
                let q: &[f64; 6];
                let s: f64;
                let r: f64;
                let z: f64;
                let mut ix: u32;

                ix = get_high_word(x);
                ix &= 0x7fffffff;
                if ix >= 0x40200000 {
                    p = &QR8;
                    q = &QS8;
                } else if ix >= 0x40122E8B {
                    p = &QR5;
                    q = &QS5;
                } else if ix >= 0x4006DB6D {
                    p = &QR3;
                    q = &QS3;
                } else
                /*ix >= 0x40000000*/
                {
                    p = &QR2;
                    q = &QS2;
                }
                z = 1.0 / (x * x);
                r = p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))));
                s = 1.0 + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * (q[4] + z * q[5])))));
                return (0.375 + r / s) / x;
            }
        }

        mod j1f
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{cosf, fabsf, logf, sinf, sqrtf};

            const INVSQRTPI: f32 = 5.6418961287e-01; /* 0x3f106ebb */
            const TPI: f32 = 6.3661974669e-01; /* 0x3f22f983 */

            fn common(ix: u32, x: f32, y1: bool, sign: bool) -> f32 {
                let z: f64;
                let mut s: f64;
                let c: f64;
                let mut ss: f64;
                let mut cc: f64;

                s = sinf(x) as f64;
                if y1 {
                    s = -s;
                }
                c = cosf(x) as f64;
                cc = s - c;
                if ix < 0x7f000000 {
                    ss = -s - c;
                    z = cosf(2.0 * x) as f64;
                    if s * c > 0.0 {
                        cc = z / ss;
                    } else {
                        ss = z / cc;
                    }
                    if ix < 0x58800000 {
                        if y1 {
                            ss = -ss;
                        }
                        cc = (ponef(x) as f64) * cc - (qonef(x) as f64) * ss;
                    }
                }
                if sign {
                    cc = -cc;
                }
                return (((INVSQRTPI as f64) * cc) / (sqrtf(x) as f64)) as f32;
            }

            /* R0/S0 on [0,2] */
            const R00: f32 = -6.2500000000e-02; /* 0xbd800000 */
            const R01: f32 = 1.4070566976e-03; /* 0x3ab86cfd */
            const R02: f32 = -1.5995563444e-05; /* 0xb7862e36 */
            const R03: f32 = 4.9672799207e-08; /* 0x335557d2 */
            const S01: f32 = 1.9153760746e-02; /* 0x3c9ce859 */
            const S02: f32 = 1.8594678841e-04; /* 0x3942fab6 */
            const S03: f32 = 1.1771846857e-06; /* 0x359dffc2 */
            const S04: f32 = 5.0463624390e-09; /* 0x31ad6446 */
            const S05: f32 = 1.2354227016e-11; /* 0x2d59567e */

            pub fn j1f(x: f32) -> f32 {
                let mut z: f32;
                let r: f32;
                let s: f32;
                let mut ix: u32;
                let sign: bool;

                ix = x.to_bits();
                sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;
                if ix >= 0x7f800000 {
                    return 1.0 / (x * x);
                }
                if ix >= 0x40000000 {
                    /* |x| >= 2 */
                    return common(ix, fabsf(x), false, sign);
                }
                if ix >= 0x39000000 {
                    /* |x| >= 2**-13 */
                    z = x * x;
                    r = z * (R00 + z * (R01 + z * (R02 + z * R03)));
                    s = 1.0 + z * (S01 + z * (S02 + z * (S03 + z * (S04 + z * S05))));
                    z = 0.5 + r / s;
                } else {
                    z = 0.5;
                }
                return z * x;
            }

            const U0: [f32; 5] = [
                -1.9605709612e-01, /* 0xbe48c331 */
                5.0443872809e-02,  /* 0x3d4e9e3c */
                -1.9125689287e-03, /* 0xbafaaf2a */
                2.3525259166e-05,  /* 0x37c5581c */
                -9.1909917899e-08, /* 0xb3c56003 */
            ];
            const V0: [f32; 5] = [
                1.9916731864e-02, /* 0x3ca3286a */
                2.0255257550e-04, /* 0x3954644b */
                1.3560879779e-06, /* 0x35b602d4 */
                6.2274145840e-09, /* 0x31d5f8eb */
                1.6655924903e-11, /* 0x2d9281cf */
            ];

            pub fn y1f(x: f32) -> f32 {
                let z: f32;
                let u: f32;
                let v: f32;
                let ix: u32;

                ix = x.to_bits();
                if (ix & 0x7fffffff) == 0 {
                    return -1.0 / 0.0;
                }
                if (ix >> 31) != 0 {
                    return 0.0 / 0.0;
                }
                if ix >= 0x7f800000 {
                    return 1.0 / x;
                }
                if ix >= 0x40000000 {
                    /* |x| >= 2.0 */
                    return common(ix, x, true, false);
                }
                if ix < 0x33000000 {
                    /* x < 2**-25 */
                    return -TPI / x;
                }
                z = x * x;
                u = U0[0] + z * (U0[1] + z * (U0[2] + z * (U0[3] + z * U0[4])));
                v = 1.0 + z * (V0[0] + z * (V0[1] + z * (V0[2] + z * (V0[3] + z * V0[4]))));
                return x * (u / v) + TPI * (j1f(x) * logf(x) - 1.0 / x);
            }

            /* For x >= 8, the asymptotic expansions of pone is
            *      1 + 15/128 s^2 - 4725/2^15 s^4 - ...,   where s = 1/x.
            * We approximate pone by
            *      pone(x) = 1 + (R/S)
            * where  R = pr0 + pr1*s^2 + pr2*s^4 + ... + pr5*s^10
            *        S = 1 + ps0*s^2 + ... + ps4*s^10
            * and
            *      | pone(x)-1-R/S | <= 2  ** ( -60.06)
            */

            const PR8: [f32; 6] = [
                /* for x in [inf, 8]=1/[0,0.125] */
                0.0000000000e+00, /* 0x00000000 */
                1.1718750000e-01, /* 0x3df00000 */
                1.3239480972e+01, /* 0x4153d4ea */
                4.1205184937e+02, /* 0x43ce06a3 */
                3.8747453613e+03, /* 0x45722bed */
                7.9144794922e+03, /* 0x45f753d6 */
            ];
            const PS8: [f32; 5] = [
                1.1420736694e+02, /* 0x42e46a2c */
                3.6509309082e+03, /* 0x45642ee5 */
                3.6956207031e+04, /* 0x47105c35 */
                9.7602796875e+04, /* 0x47bea166 */
                3.0804271484e+04, /* 0x46f0a88b */
            ];

            const PR5: [f32; 6] = [
                /* for x in [8,4.5454]=1/[0.125,0.22001] */
                1.3199052094e-11, /* 0x2d68333f */
                1.1718749255e-01, /* 0x3defffff */
                6.8027510643e+00, /* 0x40d9b023 */
                1.0830818176e+02, /* 0x42d89dca */
                5.1763616943e+02, /* 0x440168b7 */
                5.2871520996e+02, /* 0x44042dc6 */
            ];
            const PS5: [f32; 5] = [
                5.9280597687e+01, /* 0x426d1f55 */
                9.9140142822e+02, /* 0x4477d9b1 */
                5.3532670898e+03, /* 0x45a74a23 */
                7.8446904297e+03, /* 0x45f52586 */
                1.5040468750e+03, /* 0x44bc0180 */
            ];

            const PR3: [f32; 6] = [
                3.0250391081e-09, /* 0x314fe10d */
                1.1718686670e-01, /* 0x3defffab */
                3.9329774380e+00, /* 0x407bb5e7 */
                3.5119403839e+01, /* 0x420c7a45 */
                9.1055007935e+01, /* 0x42b61c2a */
                4.8559066772e+01, /* 0x42423c7c */
            ];
            const PS3: [f32; 5] = [
                3.4791309357e+01, /* 0x420b2a4d */
                3.3676245117e+02, /* 0x43a86198 */
                1.0468714600e+03, /* 0x4482dbe3 */
                8.9081134033e+02, /* 0x445eb3ed */
                1.0378793335e+02, /* 0x42cf936c */
            ];

            const PR2: [f32; 6] = [
                /* for x in [2.8570,2]=1/[0.3499,0.5] */
                1.0771083225e-07, /* 0x33e74ea8 */
                1.1717621982e-01, /* 0x3deffa16 */
                2.3685150146e+00, /* 0x401795c0 */
                1.2242610931e+01, /* 0x4143e1bc */
                1.7693971634e+01, /* 0x418d8d41 */
                5.0735230446e+00, /* 0x40a25a4d */
            ];
            const PS2: [f32; 5] = [
                2.1436485291e+01, /* 0x41ab7dec */
                1.2529022980e+02, /* 0x42fa9499 */
                2.3227647400e+02, /* 0x436846c7 */
                1.1767937469e+02, /* 0x42eb5bd7 */
                8.3646392822e+00, /* 0x4105d590 */
            ];

            fn ponef(x: f32) -> f32 {
                let p: &[f32; 6];
                let q: &[f32; 5];
                let z: f32;
                let r: f32;
                let s: f32;
                let mut ix: u32;

                ix = x.to_bits();
                ix &= 0x7fffffff;
                if ix >= 0x41000000 {
                    p = &PR8;
                    q = &PS8;
                } else if ix >= 0x409173eb {
                    p = &PR5;
                    q = &PS5;
                } else if ix >= 0x4036d917 {
                    p = &PR3;
                    q = &PS3;
                } else
                /*ix >= 0x40000000*/
                {
                    p = &PR2;
                    q = &PS2;
                }
                z = 1.0 / (x * x);
                r = p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))));
                s = 1.0 + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * q[4]))));
                return 1.0 + r / s;
            }

            /* For x >= 8, the asymptotic expansions of qone is
            *      3/8 s - 105/1024 s^3 - ..., where s = 1/x.
            * We approximate pone by
            *      qone(x) = s*(0.375 + (R/S))
            * where  R = qr1*s^2 + qr2*s^4 + ... + qr5*s^10
            *        S = 1 + qs1*s^2 + ... + qs6*s^12
            * and
            *      | qone(x)/s -0.375-R/S | <= 2  ** ( -61.13)
            */

            const QR8: [f32; 6] = [
                /* for x in [inf, 8]=1/[0,0.125] */
                0.0000000000e+00,  /* 0x00000000 */
                -1.0253906250e-01, /* 0xbdd20000 */
                -1.6271753311e+01, /* 0xc1822c8d */
                -7.5960174561e+02, /* 0xc43de683 */
                -1.1849806641e+04, /* 0xc639273a */
                -4.8438511719e+04, /* 0xc73d3683 */
            ];
            const QS8: [f32; 6] = [
                1.6139537048e+02,  /* 0x43216537 */
                7.8253862305e+03,  /* 0x45f48b17 */
                1.3387534375e+05,  /* 0x4802bcd6 */
                7.1965775000e+05,  /* 0x492fb29c */
                6.6660125000e+05,  /* 0x4922be94 */
                -2.9449025000e+05, /* 0xc88fcb48 */
            ];

            const QR5: [f32; 6] = [
                /* for x in [8,4.5454]=1/[0.125,0.22001] */
                -2.0897993405e-11, /* 0xadb7d219 */
                -1.0253904760e-01, /* 0xbdd1fffe */
                -8.0564479828e+00, /* 0xc100e736 */
                -1.8366960144e+02, /* 0xc337ab6b */
                -1.3731937256e+03, /* 0xc4aba633 */
                -2.6124443359e+03, /* 0xc523471c */
            ];
            const QS5: [f32; 6] = [
                8.1276550293e+01,  /* 0x42a28d98 */
                1.9917987061e+03,  /* 0x44f8f98f */
                1.7468484375e+04,  /* 0x468878f8 */
                4.9851425781e+04,  /* 0x4742bb6d */
                2.7948074219e+04,  /* 0x46da5826 */
                -4.7191835938e+03, /* 0xc5937978 */
            ];

            const QR3: [f32; 6] = [
                -5.0783124372e-09, /* 0xb1ae7d4f */
                -1.0253783315e-01, /* 0xbdd1ff5b */
                -4.6101160049e+00, /* 0xc0938612 */
                -5.7847221375e+01, /* 0xc267638e */
                -2.2824453735e+02, /* 0xc3643e9a */
                -2.1921012878e+02, /* 0xc35b35cb */
            ];
            const QS3: [f32; 6] = [
                4.7665153503e+01,  /* 0x423ea91e */
                6.7386511230e+02,  /* 0x4428775e */
                3.3801528320e+03,  /* 0x45534272 */
                5.5477290039e+03,  /* 0x45ad5dd5 */
                1.9031191406e+03,  /* 0x44ede3d0 */
                -1.3520118713e+02, /* 0xc3073381 */
            ];

            const QR2: [f32; 6] = [
                /* for x in [2.8570,2]=1/[0.3499,0.5] */
                -1.7838172539e-07, /* 0xb43f8932 */
                -1.0251704603e-01, /* 0xbdd1f475 */
                -2.7522056103e+00, /* 0xc0302423 */
                -1.9663616180e+01, /* 0xc19d4f16 */
                -4.2325313568e+01, /* 0xc2294d1f */
                -2.1371921539e+01, /* 0xc1aaf9b2 */
            ];
            const QS2: [f32; 6] = [
                2.9533363342e+01,  /* 0x41ec4454 */
                2.5298155212e+02,  /* 0x437cfb47 */
                7.5750280762e+02,  /* 0x443d602e */
                7.3939318848e+02,  /* 0x4438d92a */
                1.5594900513e+02,  /* 0x431bf2f2 */
                -4.9594988823e+00, /* 0xc09eb437 */
            ];

            fn qonef(x: f32) -> f32 {
                let p: &[f32; 6];
                let q: &[f32; 6];
                let s: f32;
                let r: f32;
                let z: f32;
                let mut ix: u32;

                ix = x.to_bits();
                ix &= 0x7fffffff;
                if ix >= 0x41000000 {
                    p = &QR8;
                    q = &QS8;
                } else if ix >= 0x409173eb {
                    p = &QR5;
                    q = &QS5;
                } else if ix >= 0x4036d917 {
                    p = &QR3;
                    q = &QS3;
                } else
                /*ix >= 0x40000000*/
                {
                    p = &QR2;
                    q = &QS2;
                }
                z = 1.0 / (x * x);
                r = p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))));
                s = 1.0 + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * (q[4] + z * q[5])))));
                return (0.375 + r / s) / x;
            }

        }

        mod jn
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{cos, fabs, get_high_word, get_low_word, j0, j1, log, sin, sqrt, y0, y1};

            const INVSQRTPI: f64 = 5.64189583547756279280e-01; /* 0x3FE20DD7, 0x50429B6D */

            pub fn jn(n: i32, mut x: f64) -> f64 {
                let mut ix: u32;
                let lx: u32;
                let nm1: i32;
                let mut i: i32;
                let mut sign: bool;
                let mut a: f64;
                let mut b: f64;
                let mut temp: f64;

                ix = get_high_word(x);
                lx = get_low_word(x);
                sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;

                // -lx == !lx + 1
                if (ix | (lx | ((!lx).wrapping_add(1))) >> 31) > 0x7ff00000 {
                    /* nan */
                    return x;
                }

                /* J(-n,x) = (-1)^n * J(n, x), J(n, -x) = (-1)^n * J(n, x)
                * Thus, J(-n,x) = J(n,-x)
                */
                /* nm1 = |n|-1 is used instead of |n| to handle n==INT_MIN */
                if n == 0 {
                    return j0(x);
                }
                if n < 0 {
                    nm1 = -(n + 1);
                    x = -x;
                    sign = !sign;
                } else {
                    nm1 = n - 1;
                }
                if nm1 == 0 {
                    return j1(x);
                }

                sign &= (n & 1) != 0; /* even n: 0, odd n: signbit(x) */
                x = fabs(x);
                if (ix | lx) == 0 || ix == 0x7ff00000 {
                    /* if x is 0 or inf */
                    b = 0.0;
                } else if (nm1 as f64) < x {
                    /* Safe to use J(n+1,x)=2n/x *J(n,x)-J(n-1,x) */
                    if ix >= 0x52d00000 {
                        /* x > 2**302 */
                        /* (x >> n**2)
                        *      Jn(x) = cos(x-(2n+1)*pi/4)*sqrt(2/x*pi)
                        *      Yn(x) = sin(x-(2n+1)*pi/4)*sqrt(2/x*pi)
                        *      Let s=sin(x), c=cos(x),
                        *          xn=x-(2n+1)*pi/4, sqt2 = sqrt(2),then
                        *
                        *             n    sin(xn)*sqt2    cos(xn)*sqt2
                        *          ----------------------------------
                        *             0     s-c             c+s
                        *             1    -s-c            -c+s
                        *             2    -s+c            -c-s
                        *             3     s+c             c-s
                        */
                        temp = match nm1 & 3 {
                            0 => -cos(x) + sin(x),
                            1 => -cos(x) - sin(x),
                            2 => cos(x) - sin(x),
                            3 | _ => cos(x) + sin(x),
                        };
                        b = INVSQRTPI * temp / sqrt(x);
                    } else {
                        a = j0(x);
                        b = j1(x);
                        i = 0;
                        while i < nm1 {
                            i += 1;
                            temp = b;
                            b = b * (2.0 * (i as f64) / x) - a; /* avoid underflow */
                            a = temp;
                        }
                    }
                } else {
                    if ix < 0x3e100000 {
                        /* x < 2**-29 */
                        /* x is tiny, return the first Taylor expansion of J(n,x)
                        * J(n,x) = 1/n!*(x/2)^n  - ...
                        */
                        if nm1 > 32 {
                            /* underflow */
                            b = 0.0;
                        } else {
                            temp = x * 0.5;
                            b = temp;
                            a = 1.0;
                            i = 2;
                            while i <= nm1 + 1 {
                                a *= i as f64; /* a = n! */
                                b *= temp; /* b = (x/2)^n */
                                i += 1;
                            }
                            b = b / a;
                        }
                    } else {
                        /* use backward recurrence */
                        /*                      x      x^2      x^2
                        *  J(n,x)/J(n-1,x) =  ----   ------   ------   .....
                        *                      2n  - 2(n+1) - 2(n+2)
                        *
                        *                      1      1        1
                        *  (for large x)   =  ----  ------   ------   .....
                        *                      2n   2(n+1)   2(n+2)
                        *                      -- - ------ - ------ -
                        *                       x     x         x
                        *
                        * Let w = 2n/x and h=2/x, then the above quotient
                        * is equal to the continued fraction:
                        *                  1
                        *      = -----------------------
                        *                     1
                        *         w - -----------------
                        *                        1
                        *              w+h - ---------
                        *                     w+2h - ...
                        *
                        * To determine how many terms needed, let
                        * Q(0) = w, Q(1) = w(w+h) - 1,
                        * Q(k) = (w+k*h)*Q(k-1) - Q(k-2),
                        * When Q(k) > 1e4      good for single
                        * When Q(k) > 1e9      good for double
                        * When Q(k) > 1e17     good for quadruple
                        */
                        /* determine k */
                        let mut t: f64;
                        let mut q0: f64;
                        let mut q1: f64;
                        let mut w: f64;
                        let h: f64;
                        let mut z: f64;
                        let mut tmp: f64;
                        let nf: f64;

                        let mut k: i32;

                        nf = (nm1 as f64) + 1.0;
                        w = 2.0 * nf / x;
                        h = 2.0 / x;
                        z = w + h;
                        q0 = w;
                        q1 = w * z - 1.0;
                        k = 1;
                        while q1 < 1.0e9 {
                            k += 1;
                            z += h;
                            tmp = z * q1 - q0;
                            q0 = q1;
                            q1 = tmp;
                        }
                        t = 0.0;
                        i = k;
                        while i >= 0 {
                            t = 1.0 / (2.0 * ((i as f64) + nf) / x - t);
                            i -= 1;
                        }
                        a = t;
                        b = 1.0;
                        /*  estimate log((2/x)^n*n!) = n*log(2/x)+n*ln(n)
                        *  Hence, if n*(log(2n/x)) > ...
                        *  single 8.8722839355e+01
                        *  double 7.09782712893383973096e+02
                        *  long double 1.1356523406294143949491931077970765006170e+04
                        *  then recurrent value may overflow and the result is
                        *  likely underflow to zero
                        */
                        tmp = nf * log(fabs(w));
                        if tmp < 7.09782712893383973096e+02 {
                            i = nm1;
                            while i > 0 {
                                temp = b;
                                b = b * (2.0 * (i as f64)) / x - a;
                                a = temp;
                                i -= 1;
                            }
                        } else {
                            i = nm1;
                            while i > 0 {
                                temp = b;
                                b = b * (2.0 * (i as f64)) / x - a;
                                a = temp;
                                /* scale b to avoid spurious overflow */
                                let x1p500 = f64::from_bits(0x5f30000000000000); // 0x1p500 == 2^500
                                if b > x1p500 {
                                    a /= b;
                                    t /= b;
                                    b = 1.0;
                                }
                                i -= 1;
                            }
                        }
                        z = j0(x);
                        w = j1(x);
                        if fabs(z) >= fabs(w) {
                            b = t * z / b;
                        } else {
                            b = t * w / a;
                        }
                    }
                }

                if sign {
                    -b
                } else {
                    b
                }
            }

            pub fn yn(n: i32, x: f64) -> f64 {
                let mut ix: u32;
                let lx: u32;
                let mut ib: u32;
                let nm1: i32;
                let mut sign: bool;
                let mut i: i32;
                let mut a: f64;
                let mut b: f64;
                let mut temp: f64;

                ix = get_high_word(x);
                lx = get_low_word(x);
                sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;

                // -lx == !lx + 1
                if (ix | (lx | ((!lx).wrapping_add(1))) >> 31) > 0x7ff00000 {
                    /* nan */
                    return x;
                }
                if sign && (ix | lx) != 0 {
                    /* x < 0 */
                    return 0.0 / 0.0;
                }
                if ix == 0x7ff00000 {
                    return 0.0;
                }

                if n == 0 {
                    return y0(x);
                }
                if n < 0 {
                    nm1 = -(n + 1);
                    sign = (n & 1) != 0;
                } else {
                    nm1 = n - 1;
                    sign = false;
                }
                if nm1 == 0 {
                    if sign {
                        return -y1(x);
                    } else {
                        return y1(x);
                    }
                }

                if ix >= 0x52d00000 {
                    /* x > 2**302 */
                    /* (x >> n**2)
                    *      Jn(x) = cos(x-(2n+1)*pi/4)*sqrt(2/x*pi)
                    *      Yn(x) = sin(x-(2n+1)*pi/4)*sqrt(2/x*pi)
                    *      Let s=sin(x), c=cos(x),
                    *          xn=x-(2n+1)*pi/4, sqt2 = sqrt(2),then
                    *
                    *             n    sin(xn)*sqt2    cos(xn)*sqt2
                    *          ----------------------------------
                    *             0     s-c             c+s
                    *             1    -s-c            -c+s
                    *             2    -s+c            -c-s
                    *             3     s+c             c-s
                    */
                    temp = match nm1 & 3 {
                        0 => -sin(x) - cos(x),
                        1 => -sin(x) + cos(x),
                        2 => sin(x) + cos(x),
                        3 | _ => sin(x) - cos(x),
                    };
                    b = INVSQRTPI * temp / sqrt(x);
                } else {
                    a = y0(x);
                    b = y1(x);
                    /* quit if b is -inf */
                    ib = get_high_word(b);
                    i = 0;
                    while i < nm1 && ib != 0xfff00000 {
                        i += 1;
                        temp = b;
                        b = (2.0 * (i as f64) / x) * b - a;
                        ib = get_high_word(b);
                        a = temp;
                    }
                }

                if sign {
                    -b
                } else {
                    b
                }
            }
        }

        mod jnf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{fabsf, j0f, j1f, logf, y0f, y1f};

            pub fn jnf(n: i32, mut x: f32) -> f32 {
                let mut ix: u32;
                let mut nm1: i32;
                let mut sign: bool;
                let mut i: i32;
                let mut a: f32;
                let mut b: f32;
                let mut temp: f32;

                ix = x.to_bits();
                sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;
                if ix > 0x7f800000 {
                    /* nan */
                    return x;
                }

                /* J(-n,x) = J(n,-x), use |n|-1 to avoid overflow in -n */
                if n == 0 {
                    return j0f(x);
                }
                if n < 0 {
                    nm1 = -(n + 1);
                    x = -x;
                    sign = !sign;
                } else {
                    nm1 = n - 1;
                }
                if nm1 == 0 {
                    return j1f(x);
                }

                sign &= (n & 1) != 0; /* even n: 0, odd n: signbit(x) */
                x = fabsf(x);
                if ix == 0 || ix == 0x7f800000 {
                    /* if x is 0 or inf */
                    b = 0.0;
                } else if (nm1 as f32) < x {
                    /* Safe to use J(n+1,x)=2n/x *J(n,x)-J(n-1,x) */
                    a = j0f(x);
                    b = j1f(x);
                    i = 0;
                    while i < nm1 {
                        i += 1;
                        temp = b;
                        b = b * (2.0 * (i as f32) / x) - a;
                        a = temp;
                    }
                } else {
                    if ix < 0x35800000 {
                        /* x < 2**-20 */
                        /* x is tiny, return the first Taylor expansion of J(n,x)
                        * J(n,x) = 1/n!*(x/2)^n  - ...
                        */
                        if nm1 > 8 {
                            /* underflow */
                            nm1 = 8;
                        }
                        temp = 0.5 * x;
                        b = temp;
                        a = 1.0;
                        i = 2;
                        while i <= nm1 + 1 {
                            a *= i as f32; /* a = n! */
                            b *= temp; /* b = (x/2)^n */
                            i += 1;
                        }
                        b = b / a;
                    } else {
                        /* use backward recurrence */
                        /*                      x      x^2      x^2
                        *  J(n,x)/J(n-1,x) =  ----   ------   ------   .....
                        *                      2n  - 2(n+1) - 2(n+2)
                        *
                        *                      1      1        1
                        *  (for large x)   =  ----  ------   ------   .....
                        *                      2n   2(n+1)   2(n+2)
                        *                      -- - ------ - ------ -
                        *                       x     x         x
                        *
                        * Let w = 2n/x and h=2/x, then the above quotient
                        * is equal to the continued fraction:
                        *                  1
                        *      = -----------------------
                        *                     1
                        *         w - -----------------
                        *                        1
                        *              w+h - ---------
                        *                     w+2h - ...
                        *
                        * To determine how many terms needed, let
                        * Q(0) = w, Q(1) = w(w+h) - 1,
                        * Q(k) = (w+k*h)*Q(k-1) - Q(k-2),
                        * When Q(k) > 1e4      good for single
                        * When Q(k) > 1e9      good for double
                        * When Q(k) > 1e17     good for quadruple
                        */
                        /* determine k */
                        let mut t: f32;
                        let mut q0: f32;
                        let mut q1: f32;
                        let mut w: f32;
                        let h: f32;
                        let mut z: f32;
                        let mut tmp: f32;
                        let nf: f32;
                        let mut k: i32;

                        nf = (nm1 as f32) + 1.0;
                        w = 2.0 * (nf as f32) / x;
                        h = 2.0 / x;
                        z = w + h;
                        q0 = w;
                        q1 = w * z - 1.0;
                        k = 1;
                        while q1 < 1.0e4 {
                            k += 1;
                            z += h;
                            tmp = z * q1 - q0;
                            q0 = q1;
                            q1 = tmp;
                        }
                        t = 0.0;
                        i = k;
                        while i >= 0 {
                            t = 1.0 / (2.0 * ((i as f32) + nf) / x - t);
                            i -= 1;
                        }
                        a = t;
                        b = 1.0;
                        /*  estimate log((2/x)^n*n!) = n*log(2/x)+n*ln(n)
                        *  Hence, if n*(log(2n/x)) > ...
                        *  single 8.8722839355e+01
                        *  double 7.09782712893383973096e+02
                        *  long double 1.1356523406294143949491931077970765006170e+04
                        *  then recurrent value may overflow and the result is
                        *  likely underflow to zero
                        */
                        tmp = nf * logf(fabsf(w));
                        if tmp < 88.721679688 {
                            i = nm1;
                            while i > 0 {
                                temp = b;
                                b = 2.0 * (i as f32) * b / x - a;
                                a = temp;
                                i -= 1;
                            }
                        } else {
                            i = nm1;
                            while i > 0 {
                                temp = b;
                                b = 2.0 * (i as f32) * b / x - a;
                                a = temp;
                                /* scale b to avoid spurious overflow */
                                let x1p60 = f32::from_bits(0x5d800000); // 0x1p60 == 2^60
                                if b > x1p60 {
                                    a /= b;
                                    t /= b;
                                    b = 1.0;
                                }
                                i -= 1;
                            }
                        }
                        z = j0f(x);
                        w = j1f(x);
                        if fabsf(z) >= fabsf(w) {
                            b = t * z / b;
                        } else {
                            b = t * w / a;
                        }
                    }
                }

                if sign {
                    -b
                } else {
                    b
                }
            }

            pub fn ynf(n: i32, x: f32) -> f32 {
                let mut ix: u32;
                let mut ib: u32;
                let nm1: i32;
                let mut sign: bool;
                let mut i: i32;
                let mut a: f32;
                let mut b: f32;
                let mut temp: f32;

                ix = x.to_bits();
                sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;
                if ix > 0x7f800000 {
                    /* nan */
                    return x;
                }
                if sign && ix != 0 {
                    /* x < 0 */
                    return 0.0 / 0.0;
                }
                if ix == 0x7f800000 {
                    return 0.0;
                }

                if n == 0 {
                    return y0f(x);
                }
                if n < 0 {
                    nm1 = -(n + 1);
                    sign = (n & 1) != 0;
                } else {
                    nm1 = n - 1;
                    sign = false;
                }
                if nm1 == 0 {
                    if sign {
                        return -y1f(x);
                    } else {
                        return y1f(x);
                    }
                }

                a = y0f(x);
                b = y1f(x);
                /* quit if b is -inf */
                ib = b.to_bits();
                i = 0;
                while i < nm1 && ib != 0xff800000 {
                    i += 1;
                    temp = b;
                    b = (2.0 * (i as f32) / x) * b - a;
                    ib = b.to_bits();
                    a = temp;
                }

                if sign {
                    -b
                } else {
                    b
                }
            }
        }

        mod ldexp
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn ldexp(x: f64, n: i32) -> f64
            {
                super::scalbn(x, n)
            }
        }

        mod ldexpf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn ldexpf(x: f32, n: i32) -> f32
            {
                super::scalbnf(x, n)
            }
        }

        mod lgamma
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::lgamma_r;

            pub fn lgamma(x: f64) -> f64 {
                lgamma_r(x).0
            }
        }

        mod lgamma_r
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{floor, k_cos, k_sin, log};

            const PI: f64 = 3.14159265358979311600e+00; /* 0x400921FB, 0x54442D18 */
            const A0: f64 = 7.72156649015328655494e-02; /* 0x3FB3C467, 0xE37DB0C8 */
            const A1: f64 = 3.22467033424113591611e-01; /* 0x3FD4A34C, 0xC4A60FAD */
            const A2: f64 = 6.73523010531292681824e-02; /* 0x3FB13E00, 0x1A5562A7 */
            const A3: f64 = 2.05808084325167332806e-02; /* 0x3F951322, 0xAC92547B */
            const A4: f64 = 7.38555086081402883957e-03; /* 0x3F7E404F, 0xB68FEFE8 */
            const A5: f64 = 2.89051383673415629091e-03; /* 0x3F67ADD8, 0xCCB7926B */
            const A6: f64 = 1.19270763183362067845e-03; /* 0x3F538A94, 0x116F3F5D */
            const A7: f64 = 5.10069792153511336608e-04; /* 0x3F40B6C6, 0x89B99C00 */
            const A8: f64 = 2.20862790713908385557e-04; /* 0x3F2CF2EC, 0xED10E54D */
            const A9: f64 = 1.08011567247583939954e-04; /* 0x3F1C5088, 0x987DFB07 */
            const A10: f64 = 2.52144565451257326939e-05; /* 0x3EFA7074, 0x428CFA52 */
            const A11: f64 = 4.48640949618915160150e-05; /* 0x3F07858E, 0x90A45837 */
            const TC: f64 = 1.46163214496836224576e+00; /* 0x3FF762D8, 0x6356BE3F */
            const TF: f64 = -1.21486290535849611461e-01; /* 0xBFBF19B9, 0xBCC38A42 */
            /* tt = -(tail of TF) */
            const TT: f64 = -3.63867699703950536541e-18; /* 0xBC50C7CA, 0xA48A971F */
            const T0: f64 = 4.83836122723810047042e-01; /* 0x3FDEF72B, 0xC8EE38A2 */
            const T1: f64 = -1.47587722994593911752e-01; /* 0xBFC2E427, 0x8DC6C509 */
            const T2: f64 = 6.46249402391333854778e-02; /* 0x3FB08B42, 0x94D5419B */
            const T3: f64 = -3.27885410759859649565e-02; /* 0xBFA0C9A8, 0xDF35B713 */
            const T4: f64 = 1.79706750811820387126e-02; /* 0x3F9266E7, 0x970AF9EC */
            const T5: f64 = -1.03142241298341437450e-02; /* 0xBF851F9F, 0xBA91EC6A */
            const T6: f64 = 6.10053870246291332635e-03; /* 0x3F78FCE0, 0xE370E344 */
            const T7: f64 = -3.68452016781138256760e-03; /* 0xBF6E2EFF, 0xB3E914D7 */
            const T8: f64 = 2.25964780900612472250e-03; /* 0x3F6282D3, 0x2E15C915 */
            const T9: f64 = -1.40346469989232843813e-03; /* 0xBF56FE8E, 0xBF2D1AF1 */
            const T10: f64 = 8.81081882437654011382e-04; /* 0x3F4CDF0C, 0xEF61A8E9 */
            const T11: f64 = -5.38595305356740546715e-04; /* 0xBF41A610, 0x9C73E0EC */
            const T12: f64 = 3.15632070903625950361e-04; /* 0x3F34AF6D, 0x6C0EBBF7 */
            const T13: f64 = -3.12754168375120860518e-04; /* 0xBF347F24, 0xECC38C38 */
            const T14: f64 = 3.35529192635519073543e-04; /* 0x3F35FD3E, 0xE8C2D3F4 */
            const U0: f64 = -7.72156649015328655494e-02; /* 0xBFB3C467, 0xE37DB0C8 */
            const U1: f64 = 6.32827064025093366517e-01; /* 0x3FE4401E, 0x8B005DFF */
            const U2: f64 = 1.45492250137234768737e+00; /* 0x3FF7475C, 0xD119BD6F */
            const U3: f64 = 9.77717527963372745603e-01; /* 0x3FEF4976, 0x44EA8450 */
            const U4: f64 = 2.28963728064692451092e-01; /* 0x3FCD4EAE, 0xF6010924 */
            const U5: f64 = 1.33810918536787660377e-02; /* 0x3F8B678B, 0xBF2BAB09 */
            const V1: f64 = 2.45597793713041134822e+00; /* 0x4003A5D7, 0xC2BD619C */
            const V2: f64 = 2.12848976379893395361e+00; /* 0x40010725, 0xA42B18F5 */
            const V3: f64 = 7.69285150456672783825e-01; /* 0x3FE89DFB, 0xE45050AF */
            const V4: f64 = 1.04222645593369134254e-01; /* 0x3FBAAE55, 0xD6537C88 */
            const V5: f64 = 3.21709242282423911810e-03; /* 0x3F6A5ABB, 0x57D0CF61 */
            const S0: f64 = -7.72156649015328655494e-02; /* 0xBFB3C467, 0xE37DB0C8 */
            const S1: f64 = 2.14982415960608852501e-01; /* 0x3FCB848B, 0x36E20878 */
            const S2: f64 = 3.25778796408930981787e-01; /* 0x3FD4D98F, 0x4F139F59 */
            const S3: f64 = 1.46350472652464452805e-01; /* 0x3FC2BB9C, 0xBEE5F2F7 */
            const S4: f64 = 2.66422703033638609560e-02; /* 0x3F9B481C, 0x7E939961 */
            const S5: f64 = 1.84028451407337715652e-03; /* 0x3F5E26B6, 0x7368F239 */
            const S6: f64 = 3.19475326584100867617e-05; /* 0x3F00BFEC, 0xDD17E945 */
            const R1: f64 = 1.39200533467621045958e+00; /* 0x3FF645A7, 0x62C4AB74 */
            const R2: f64 = 7.21935547567138069525e-01; /* 0x3FE71A18, 0x93D3DCDC */
            const R3: f64 = 1.71933865632803078993e-01; /* 0x3FC601ED, 0xCCFBDF27 */
            const R4: f64 = 1.86459191715652901344e-02; /* 0x3F9317EA, 0x742ED475 */
            const R5: f64 = 7.77942496381893596434e-04; /* 0x3F497DDA, 0xCA41A95B */
            const R6: f64 = 7.32668430744625636189e-06; /* 0x3EDEBAF7, 0xA5B38140 */
            const W0: f64 = 4.18938533204672725052e-01; /* 0x3FDACFE3, 0x90C97D69 */
            const W1: f64 = 8.33333333333329678849e-02; /* 0x3FB55555, 0x5555553B */
            const W2: f64 = -2.77777777728775536470e-03; /* 0xBF66C16C, 0x16B02E5C */
            const W3: f64 = 7.93650558643019558500e-04; /* 0x3F4A019F, 0x98CF38B6 */
            const W4: f64 = -5.95187557450339963135e-04; /* 0xBF4380CB, 0x8C0FE741 */
            const W5: f64 = 8.36339918996282139126e-04; /* 0x3F4B67BA, 0x4CDAD5D1 */
            const W6: f64 = -1.63092934096575273989e-03; /* 0xBF5AB89D, 0x0B9E43E4 */

            /* sin(PI*x) assuming x > 2^-100, if sin(PI*x)==0 the sign is arbitrary */
            fn sin_pi(mut x: f64) -> f64 {
                let mut n: i32;

                /* spurious inexact if odd int */
                x = 2.0 * (x * 0.5 - floor(x * 0.5)); /* x mod 2.0 */

                n = (x * 4.0) as i32;
                n = (n + 1) / 2;
                x -= (n as f64) * 0.5;
                x *= PI;

                match n {
                    1 => k_cos(x, 0.0),
                    2 => k_sin(-x, 0.0, 0),
                    3 => -k_cos(x, 0.0),
                    0 | _ => k_sin(x, 0.0, 0),
                }
            }

            pub fn lgamma_r(mut x: f64) -> (f64, i32) {
                let u: u64 = x.to_bits();
                let mut t: f64;
                let y: f64;
                let mut z: f64;
                let nadj: f64;
                let p: f64;
                let p1: f64;
                let p2: f64;
                let p3: f64;
                let q: f64;
                let mut r: f64;
                let w: f64;
                let ix: u32;
                let sign: bool;
                let i: i32;
                let mut signgam: i32;

                /* purge off +-inf, NaN, +-0, tiny and negative arguments */
                signgam = 1;
                sign = (u >> 63) != 0;
                ix = ((u >> 32) as u32) & 0x7fffffff;
                if ix >= 0x7ff00000 {
                    return (x * x, signgam);
                }
                if ix < (0x3ff - 70) << 20 {
                    /* |x|<2**-70, return -log(|x|) */
                    if sign {
                        x = -x;
                        signgam = -1;
                    }
                    return (-log(x), signgam);
                }
                if sign {
                    x = -x;
                    t = sin_pi(x);
                    if t == 0.0 {
                        /* -integer */
                        return (1.0 / (x - x), signgam);
                    }
                    if t > 0.0 {
                        signgam = -1;
                    } else {
                        t = -t;
                    }
                    nadj = log(PI / (t * x));
                } else {
                    nadj = 0.0;
                }

                /* purge off 1 and 2 */
                if (ix == 0x3ff00000 || ix == 0x40000000) && (u & 0xffffffff) == 0 {
                    r = 0.0;
                }
                /* for x < 2.0 */
                else if ix < 0x40000000 {
                    if ix <= 0x3feccccc {
                        /* lgamma(x) = lgamma(x+1)-log(x) */
                        r = -log(x);
                        if ix >= 0x3FE76944 {
                            y = 1.0 - x;
                            i = 0;
                        } else if ix >= 0x3FCDA661 {
                            y = x - (TC - 1.0);
                            i = 1;
                        } else {
                            y = x;
                            i = 2;
                        }
                    } else {
                        r = 0.0;
                        if ix >= 0x3FFBB4C3 {
                            /* [1.7316,2] */
                            y = 2.0 - x;
                            i = 0;
                        } else if ix >= 0x3FF3B4C4 {
                            /* [1.23,1.73] */
                            y = x - TC;
                            i = 1;
                        } else {
                            y = x - 1.0;
                            i = 2;
                        }
                    }
                    match i {
                        0 => {
                            z = y * y;
                            p1 = A0 + z * (A2 + z * (A4 + z * (A6 + z * (A8 + z * A10))));
                            p2 = z * (A1 + z * (A3 + z * (A5 + z * (A7 + z * (A9 + z * A11)))));
                            p = y * p1 + p2;
                            r += p - 0.5 * y;
                        }
                        1 => {
                            z = y * y;
                            w = z * y;
                            p1 = T0 + w * (T3 + w * (T6 + w * (T9 + w * T12))); /* parallel comp */
                            p2 = T1 + w * (T4 + w * (T7 + w * (T10 + w * T13)));
                            p3 = T2 + w * (T5 + w * (T8 + w * (T11 + w * T14)));
                            p = z * p1 - (TT - w * (p2 + y * p3));
                            r += TF + p;
                        }
                        2 => {
                            p1 = y * (U0 + y * (U1 + y * (U2 + y * (U3 + y * (U4 + y * U5)))));
                            p2 = 1.0 + y * (V1 + y * (V2 + y * (V3 + y * (V4 + y * V5))));
                            r += -0.5 * y + p1 / p2;
                        }
                        #[cfg(debug_assertions)]
                        _ => unreachable!(),
                        #[cfg(not(debug_assertions))]
                        _ => {}
                    }
                } else if ix < 0x40200000 {
                    /* x < 8.0 */
                    i = x as i32;
                    y = x - (i as f64);
                    p = y * (S0 + y * (S1 + y * (S2 + y * (S3 + y * (S4 + y * (S5 + y * S6))))));
                    q = 1.0 + y * (R1 + y * (R2 + y * (R3 + y * (R4 + y * (R5 + y * R6)))));
                    r = 0.5 * y + p / q;
                    z = 1.0; /* lgamma(1+s) = log(s) + lgamma(s) */
                    // TODO: In C, this was implemented using switch jumps with fallthrough.
                    // Does this implementation have performance problems?
                    if i >= 7 {
                        z *= y + 6.0;
                    }
                    if i >= 6 {
                        z *= y + 5.0;
                    }
                    if i >= 5 {
                        z *= y + 4.0;
                    }
                    if i >= 4 {
                        z *= y + 3.0;
                    }
                    if i >= 3 {
                        z *= y + 2.0;
                        r += log(z);
                    }
                } else if ix < 0x43900000 {
                    /* 8.0 <= x < 2**58 */
                    t = log(x);
                    z = 1.0 / x;
                    y = z * z;
                    w = W0 + z * (W1 + y * (W2 + y * (W3 + y * (W4 + y * (W5 + y * W6)))));
                    r = (x - 0.5) * (t - 1.0) + w;
                } else {
                    /* 2**58 <= x <= inf */
                    r = x * (log(x) - 1.0);
                }
                if sign {
                    r = nadj - r;
                }
                return (r, signgam);
            }
        }

        mod lgammaf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::lgammaf_r;

            pub fn lgammaf(x: f32) -> f32 {
                lgammaf_r(x).0
            }
        }

        mod lgammaf_r
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{floorf, k_cosf, k_sinf, logf};

            const PI: f32 = 3.1415927410e+00; /* 0x40490fdb */
            const A0: f32 = 7.7215664089e-02; /* 0x3d9e233f */
            const A1: f32 = 3.2246702909e-01; /* 0x3ea51a66 */
            const A2: f32 = 6.7352302372e-02; /* 0x3d89f001 */
            const A3: f32 = 2.0580807701e-02; /* 0x3ca89915 */
            const A4: f32 = 7.3855509982e-03; /* 0x3bf2027e */
            const A5: f32 = 2.8905137442e-03; /* 0x3b3d6ec6 */
            const A6: f32 = 1.1927076848e-03; /* 0x3a9c54a1 */
            const A7: f32 = 5.1006977446e-04; /* 0x3a05b634 */
            const A8: f32 = 2.2086278477e-04; /* 0x39679767 */
            const A9: f32 = 1.0801156895e-04; /* 0x38e28445 */
            const A10: f32 = 2.5214456400e-05; /* 0x37d383a2 */
            const A11: f32 = 4.4864096708e-05; /* 0x383c2c75 */
            const TC: f32 = 1.4616321325e+00; /* 0x3fbb16c3 */
            const TF: f32 = -1.2148628384e-01; /* 0xbdf8cdcd */
            /* TT = -(tail of TF) */
            const TT: f32 = 6.6971006518e-09; /* 0x31e61c52 */
            const T0: f32 = 4.8383611441e-01; /* 0x3ef7b95e */
            const T1: f32 = -1.4758771658e-01; /* 0xbe17213c */
            const T2: f32 = 6.4624942839e-02; /* 0x3d845a15 */
            const T3: f32 = -3.2788541168e-02; /* 0xbd064d47 */
            const T4: f32 = 1.7970675603e-02; /* 0x3c93373d */
            const T5: f32 = -1.0314224288e-02; /* 0xbc28fcfe */
            const T6: f32 = 6.1005386524e-03; /* 0x3bc7e707 */
            const T7: f32 = -3.6845202558e-03; /* 0xbb7177fe */
            const T8: f32 = 2.2596477065e-03; /* 0x3b141699 */
            const T9: f32 = -1.4034647029e-03; /* 0xbab7f476 */
            const T10: f32 = 8.8108185446e-04; /* 0x3a66f867 */
            const T11: f32 = -5.3859531181e-04; /* 0xba0d3085 */
            const T12: f32 = 3.1563205994e-04; /* 0x39a57b6b */
            const T13: f32 = -3.1275415677e-04; /* 0xb9a3f927 */
            const T14: f32 = 3.3552918467e-04; /* 0x39afe9f7 */
            const U0: f32 = -7.7215664089e-02; /* 0xbd9e233f */
            const U1: f32 = 6.3282704353e-01; /* 0x3f2200f4 */
            const U2: f32 = 1.4549225569e+00; /* 0x3fba3ae7 */
            const U3: f32 = 9.7771751881e-01; /* 0x3f7a4bb2 */
            const U4: f32 = 2.2896373272e-01; /* 0x3e6a7578 */
            const U5: f32 = 1.3381091878e-02; /* 0x3c5b3c5e */
            const V1: f32 = 2.4559779167e+00; /* 0x401d2ebe */
            const V2: f32 = 2.1284897327e+00; /* 0x4008392d */
            const V3: f32 = 7.6928514242e-01; /* 0x3f44efdf */
            const V4: f32 = 1.0422264785e-01; /* 0x3dd572af */
            const V5: f32 = 3.2170924824e-03; /* 0x3b52d5db */
            const S0: f32 = -7.7215664089e-02; /* 0xbd9e233f */
            const S1: f32 = 2.1498242021e-01; /* 0x3e5c245a */
            const S2: f32 = 3.2577878237e-01; /* 0x3ea6cc7a */
            const S3: f32 = 1.4635047317e-01; /* 0x3e15dce6 */
            const S4: f32 = 2.6642270386e-02; /* 0x3cda40e4 */
            const S5: f32 = 1.8402845599e-03; /* 0x3af135b4 */
            const S6: f32 = 3.1947532989e-05; /* 0x3805ff67 */
            const R1: f32 = 1.3920053244e+00; /* 0x3fb22d3b */
            const R2: f32 = 7.2193557024e-01; /* 0x3f38d0c5 */
            const R3: f32 = 1.7193385959e-01; /* 0x3e300f6e */
            const R4: f32 = 1.8645919859e-02; /* 0x3c98bf54 */
            const R5: f32 = 7.7794247773e-04; /* 0x3a4beed6 */
            const R6: f32 = 7.3266842264e-06; /* 0x36f5d7bd */
            const W0: f32 = 4.1893854737e-01; /* 0x3ed67f1d */
            const W1: f32 = 8.3333335817e-02; /* 0x3daaaaab */
            const W2: f32 = -2.7777778450e-03; /* 0xbb360b61 */
            const W3: f32 = 7.9365057172e-04; /* 0x3a500cfd */
            const W4: f32 = -5.9518753551e-04; /* 0xba1c065c */
            const W5: f32 = 8.3633989561e-04; /* 0x3a5b3dd2 */
            const W6: f32 = -1.6309292987e-03; /* 0xbad5c4e8 */

            /* sin(PI*x) assuming x > 2^-100, if sin(PI*x)==0 the sign is arbitrary */
            fn sin_pi(mut x: f32) -> f32 {
                let mut y: f64;
                let mut n: isize;

                /* spurious inexact if odd int */
                x = 2.0 * (x * 0.5 - floorf(x * 0.5)); /* x mod 2.0 */

                n = (x * 4.0) as isize;
                n = (n + 1) / 2;
                y = (x as f64) - (n as f64) * 0.5;
                y *= 3.14159265358979323846;
                match n {
                    1 => k_cosf(y),
                    2 => k_sinf(-y),
                    3 => -k_cosf(y),
                    0 | _ => k_sinf(y),
                }
            }

            pub fn lgammaf_r(mut x: f32) -> (f32, i32) {
                let u = x.to_bits();
                let mut t: f32;
                let y: f32;
                let mut z: f32;
                let nadj: f32;
                let p: f32;
                let p1: f32;
                let p2: f32;
                let p3: f32;
                let q: f32;
                let mut r: f32;
                let w: f32;
                let ix: u32;
                let i: i32;
                let sign: bool;
                let mut signgam: i32;

                /* purge off +-inf, NaN, +-0, tiny and negative arguments */
                signgam = 1;
                sign = (u >> 31) != 0;
                ix = u & 0x7fffffff;
                if ix >= 0x7f800000 {
                    return (x * x, signgam);
                }
                if ix < 0x35000000 {
                    /* |x| < 2**-21, return -log(|x|) */
                    if sign {
                        signgam = -1;
                        x = -x;
                    }
                    return (-logf(x), signgam);
                }
                if sign {
                    x = -x;
                    t = sin_pi(x);
                    if t == 0.0 {
                        /* -integer */
                        return (1.0 / (x - x), signgam);
                    }
                    if t > 0.0 {
                        signgam = -1;
                    } else {
                        t = -t;
                    }
                    nadj = logf(PI / (t * x));
                } else {
                    nadj = 0.0;
                }

                /* purge off 1 and 2 */
                if ix == 0x3f800000 || ix == 0x40000000 {
                    r = 0.0;
                }
                /* for x < 2.0 */
                else if ix < 0x40000000 {
                    if ix <= 0x3f666666 {
                        /* lgamma(x) = lgamma(x+1)-log(x) */
                        r = -logf(x);
                        if ix >= 0x3f3b4a20 {
                            y = 1.0 - x;
                            i = 0;
                        } else if ix >= 0x3e6d3308 {
                            y = x - (TC - 1.0);
                            i = 1;
                        } else {
                            y = x;
                            i = 2;
                        }
                    } else {
                        r = 0.0;
                        if ix >= 0x3fdda618 {
                            /* [1.7316,2] */
                            y = 2.0 - x;
                            i = 0;
                        } else if ix >= 0x3F9da620 {
                            /* [1.23,1.73] */
                            y = x - TC;
                            i = 1;
                        } else {
                            y = x - 1.0;
                            i = 2;
                        }
                    }
                    match i {
                        0 => {
                            z = y * y;
                            p1 = A0 + z * (A2 + z * (A4 + z * (A6 + z * (A8 + z * A10))));
                            p2 = z * (A1 + z * (A3 + z * (A5 + z * (A7 + z * (A9 + z * A11)))));
                            p = y * p1 + p2;
                            r += p - 0.5 * y;
                        }
                        1 => {
                            z = y * y;
                            w = z * y;
                            p1 = T0 + w * (T3 + w * (T6 + w * (T9 + w * T12))); /* parallel comp */
                            p2 = T1 + w * (T4 + w * (T7 + w * (T10 + w * T13)));
                            p3 = T2 + w * (T5 + w * (T8 + w * (T11 + w * T14)));
                            p = z * p1 - (TT - w * (p2 + y * p3));
                            r += TF + p;
                        }
                        2 => {
                            p1 = y * (U0 + y * (U1 + y * (U2 + y * (U3 + y * (U4 + y * U5)))));
                            p2 = 1.0 + y * (V1 + y * (V2 + y * (V3 + y * (V4 + y * V5))));
                            r += -0.5 * y + p1 / p2;
                        }
                        #[cfg(debug_assertions)]
                        _ => unreachable!(),
                        #[cfg(not(debug_assertions))]
                        _ => {}
                    }
                } else if ix < 0x41000000 {
                    /* x < 8.0 */
                    i = x as i32;
                    y = x - (i as f32);
                    p = y * (S0 + y * (S1 + y * (S2 + y * (S3 + y * (S4 + y * (S5 + y * S6))))));
                    q = 1.0 + y * (R1 + y * (R2 + y * (R3 + y * (R4 + y * (R5 + y * R6)))));
                    r = 0.5 * y + p / q;
                    z = 1.0; /* lgamma(1+s) = log(s) + lgamma(s) */
                    // TODO: In C, this was implemented using switch jumps with fallthrough.
                    // Does this implementation have performance problems?
                    if i >= 7 {
                        z *= y + 6.0;
                    }
                    if i >= 6 {
                        z *= y + 5.0;
                    }
                    if i >= 5 {
                        z *= y + 4.0;
                    }
                    if i >= 4 {
                        z *= y + 3.0;
                    }
                    if i >= 3 {
                        z *= y + 2.0;
                        r += logf(z);
                    }
                } else if ix < 0x5c800000 {
                    /* 8.0 <= x < 2**58 */
                    t = logf(x);
                    z = 1.0 / x;
                    y = z * z;
                    w = W0 + z * (W1 + y * (W2 + y * (W3 + y * (W4 + y * (W5 + y * W6)))));
                    r = (x - 0.5) * (t - 1.0) + w;
                } else {
                    /* 2**58 <= x <= inf */
                    r = x * (logf(x) - 1.0);
                }
                if sign {
                    r = nadj - r;
                }
                return (r, signgam);
            }
        }

        mod log
        {
            use ::
            {
                *,
            };
            /*
            */
            const LN2_HI: f64 = 6.93147180369123816490e-01; /* 3fe62e42 fee00000 */
            const LN2_LO: f64 = 1.90821492927058770002e-10; /* 3dea39ef 35793c76 */
            const LG1: f64 = 6.666666666666735130e-01; /* 3FE55555 55555593 */
            const LG2: f64 = 3.999999999940941908e-01; /* 3FD99999 9997FA04 */
            const LG3: f64 = 2.857142874366239149e-01; /* 3FD24924 94229359 */
            const LG4: f64 = 2.222219843214978396e-01; /* 3FCC71C5 1D8E78AF */
            const LG5: f64 = 1.818357216161805012e-01; /* 3FC74664 96CB03DE */
            const LG6: f64 = 1.531383769920937332e-01; /* 3FC39A09 D078C69F */
            const LG7: f64 = 1.479819860511658591e-01; /* 3FC2F112 DF3E5244 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn log(mut x: f64) -> f64 {
                let x1p54 = f64::from_bits(0x4350000000000000); // 0x1p54 === 2 ^ 54

                let mut ui = x.to_bits();
                let mut hx: u32 = (ui >> 32) as u32;
                let mut k: i32 = 0;

                if (hx < 0x00100000) || ((hx >> 31) != 0) {
                    /* x < 2**-126  */
                    if ui << 1 == 0 {
                        return -1. / (x * x); /* log(+-0)=-inf */
                    }
                    if hx >> 31 != 0 {
                        return (x - x) / 0.0; /* log(-#) = NaN */
                    }
                    /* subnormal number, scale x up */
                    k -= 54;
                    x *= x1p54;
                    ui = x.to_bits();
                    hx = (ui >> 32) as u32;
                } else if hx >= 0x7ff00000 {
                    return x;
                } else if hx == 0x3ff00000 && ui << 32 == 0 {
                    return 0.;
                }

                /* reduce x into [sqrt(2)/2, sqrt(2)] */
                hx += 0x3ff00000 - 0x3fe6a09e;
                k += ((hx >> 20) as i32) - 0x3ff;
                hx = (hx & 0x000fffff) + 0x3fe6a09e;
                ui = ((hx as u64) << 32) | (ui & 0xffffffff);
                x = f64::from_bits(ui);

                let f: f64 = x - 1.0;
                let hfsq: f64 = 0.5 * f * f;
                let s: f64 = f / (2.0 + f);
                let z: f64 = s * s;
                let w: f64 = z * z;
                let t1: f64 = w * (LG2 + w * (LG4 + w * LG6));
                let t2: f64 = z * (LG1 + w * (LG3 + w * (LG5 + w * LG7)));
                let r: f64 = t2 + t1;
                let dk: f64 = k as f64;
                s * (hfsq + r) + dk * LN2_LO - hfsq + f + dk * LN2_HI
            }
        }

        mod log10
        {
            use ::
            {
                *,
            };
            /*
            */
            const IVLN10HI: f64 = 4.34294481878168880939e-01; /* 0x3fdbcb7b, 0x15200000 */
            const IVLN10LO: f64 = 2.50829467116452752298e-11; /* 0x3dbb9438, 0xca9aadd5 */
            const LOG10_2HI: f64 = 3.01029995663611771306e-01; /* 0x3FD34413, 0x509F6000 */
            const LOG10_2LO: f64 = 3.69423907715893078616e-13; /* 0x3D59FEF3, 0x11F12B36 */
            const LG1: f64 = 6.666666666666735130e-01; /* 3FE55555 55555593 */
            const LG2: f64 = 3.999999999940941908e-01; /* 3FD99999 9997FA04 */
            const LG3: f64 = 2.857142874366239149e-01; /* 3FD24924 94229359 */
            const LG4: f64 = 2.222219843214978396e-01; /* 3FCC71C5 1D8E78AF */
            const LG5: f64 = 1.818357216161805012e-01; /* 3FC74664 96CB03DE */
            const LG6: f64 = 1.531383769920937332e-01; /* 3FC39A09 D078C69F */
            const LG7: f64 = 1.479819860511658591e-01; /* 3FC2F112 DF3E5244 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn log10(mut x: f64) -> f64 {
                let x1p54 = f64::from_bits(0x4350000000000000); // 0x1p54 === 2 ^ 54

                let mut ui: u64 = x.to_bits();
                let hfsq: f64;
                let f: f64;
                let s: f64;
                let z: f64;
                let r: f64;
                let mut w: f64;
                let t1: f64;
                let t2: f64;
                let dk: f64;
                let y: f64;
                let mut hi: f64;
                let lo: f64;
                let mut val_hi: f64;
                let mut val_lo: f64;
                let mut hx: u32;
                let mut k: i32;

                hx = (ui >> 32) as u32;
                k = 0;
                if hx < 0x00100000 || (hx >> 31) > 0 {
                    if ui << 1 == 0 {
                        return -1. / (x * x); /* log(+-0)=-inf */
                    }
                    if (hx >> 31) > 0 {
                        return (x - x) / 0.0; /* log(-#) = NaN */
                    }
                    /* subnormal number, scale x up */
                    k -= 54;
                    x *= x1p54;
                    ui = x.to_bits();
                    hx = (ui >> 32) as u32;
                } else if hx >= 0x7ff00000 {
                    return x;
                } else if hx == 0x3ff00000 && ui << 32 == 0 {
                    return 0.;
                }

                /* reduce x into [sqrt(2)/2, sqrt(2)] */
                hx += 0x3ff00000 - 0x3fe6a09e;
                k += (hx >> 20) as i32 - 0x3ff;
                hx = (hx & 0x000fffff) + 0x3fe6a09e;
                ui = (hx as u64) << 32 | (ui & 0xffffffff);
                x = f64::from_bits(ui);

                f = x - 1.0;
                hfsq = 0.5 * f * f;
                s = f / (2.0 + f);
                z = s * s;
                w = z * z;
                t1 = w * (LG2 + w * (LG4 + w * LG6));
                t2 = z * (LG1 + w * (LG3 + w * (LG5 + w * LG7)));
                r = t2 + t1;

                /* See log2.c for details. */
                /* hi+lo = f - hfsq + s*(hfsq+R) ~ log(1+f) */
                hi = f - hfsq;
                ui = hi.to_bits();
                ui &= (-1i64 as u64) << 32;
                hi = f64::from_bits(ui);
                lo = f - hi - hfsq + s * (hfsq + r);

                /* val_hi+val_lo ~ log10(1+f) + k*log10(2) */
                val_hi = hi * IVLN10HI;
                dk = k as f64;
                y = dk * LOG10_2HI;
                val_lo = dk * LOG10_2LO + (lo + hi) * IVLN10LO + lo * IVLN10HI;

                /*
                * Extra precision in for adding y is not strictly needed
                * since there is no very large cancellation near x = sqrt(2) or
                * x = 1/sqrt(2), but we do it anyway since it costs little on CPUs
                * with some parallelism and it reduces the error for many args.
                */
                w = y + val_hi;
                val_lo += (y - w) + val_hi;
                val_hi = w;

                val_lo + val_hi
            }
        }

        mod log10f
        {
            use ::
            {
                *,
            };
            /*
            */
            const IVLN10HI: f32 = 4.3432617188e-01; /* 0x3ede6000 */
            const IVLN10LO: f32 = -3.1689971365e-05; /* 0xb804ead9 */
            const LOG10_2HI: f32 = 3.0102920532e-01; /* 0x3e9a2080 */
            const LOG10_2LO: f32 = 7.9034151668e-07; /* 0x355427db */
            /* |(log(1+s)-log(1-s))/s - Lg(s)| < 2**-34.24 (~[-4.95e-11, 4.97e-11]). */
            const LG1: f32 = 0.66666662693; /* 0xaaaaaa.0p-24 */
            const LG2: f32 = 0.40000972152; /* 0xccce13.0p-25 */
            const LG3: f32 = 0.28498786688; /* 0x91e9ee.0p-25 */
            const LG4: f32 = 0.24279078841; /* 0xf89e26.0p-26 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn log10f(mut x: f32) -> f32 {
                let x1p25f = f32::from_bits(0x4c000000); // 0x1p25f === 2 ^ 25

                let mut ui: u32 = x.to_bits();
                let hfsq: f32;
                let f: f32;
                let s: f32;
                let z: f32;
                let r: f32;
                let w: f32;
                let t1: f32;
                let t2: f32;
                let dk: f32;
                let mut hi: f32;
                let lo: f32;
                let mut ix: u32;
                let mut k: i32;

                ix = ui;
                k = 0;
                if ix < 0x00800000 || (ix >> 31) > 0 {
                    /* x < 2**-126  */
                    if ix << 1 == 0 {
                        return -1. / (x * x); /* log(+-0)=-inf */
                    }
                    if (ix >> 31) > 0 {
                        return (x - x) / 0.0; /* log(-#) = NaN */
                    }
                    /* subnormal number, scale up x */
                    k -= 25;
                    x *= x1p25f;
                    ui = x.to_bits();
                    ix = ui;
                } else if ix >= 0x7f800000 {
                    return x;
                } else if ix == 0x3f800000 {
                    return 0.;
                }

                /* reduce x into [sqrt(2)/2, sqrt(2)] */
                ix += 0x3f800000 - 0x3f3504f3;
                k += (ix >> 23) as i32 - 0x7f;
                ix = (ix & 0x007fffff) + 0x3f3504f3;
                ui = ix;
                x = f32::from_bits(ui);

                f = x - 1.0;
                s = f / (2.0 + f);
                z = s * s;
                w = z * z;
                t1 = w * (LG2 + w * LG4);
                t2 = z * (LG1 + w * LG3);
                r = t2 + t1;
                hfsq = 0.5 * f * f;

                hi = f - hfsq;
                ui = hi.to_bits();
                ui &= 0xfffff000;
                hi = f32::from_bits(ui);
                lo = f - hi - hfsq + s * (hfsq + r);
                dk = k as f32;
                dk * LOG10_2LO + (lo + hi) * IVLN10LO + lo * IVLN10HI + hi * IVLN10HI + dk * LOG10_2HI
            }
        }

        mod log1p
        {
            use ::
            {
                *,
            };
            /*
            */
            const LN2_HI: f64 = 6.93147180369123816490e-01; /* 3fe62e42 fee00000 */
            const LN2_LO: f64 = 1.90821492927058770002e-10; /* 3dea39ef 35793c76 */
            const LG1: f64 = 6.666666666666735130e-01; /* 3FE55555 55555593 */
            const LG2: f64 = 3.999999999940941908e-01; /* 3FD99999 9997FA04 */
            const LG3: f64 = 2.857142874366239149e-01; /* 3FD24924 94229359 */
            const LG4: f64 = 2.222219843214978396e-01; /* 3FCC71C5 1D8E78AF */
            const LG5: f64 = 1.818357216161805012e-01; /* 3FC74664 96CB03DE */
            const LG6: f64 = 1.531383769920937332e-01; /* 3FC39A09 D078C69F */
            const LG7: f64 = 1.479819860511658591e-01; /* 3FC2F112 DF3E5244 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn log1p(x: f64) -> f64 {
                let mut ui: u64 = x.to_bits();
                let hfsq: f64;
                let mut f: f64 = 0.;
                let mut c: f64 = 0.;
                let s: f64;
                let z: f64;
                let r: f64;
                let w: f64;
                let t1: f64;
                let t2: f64;
                let dk: f64;
                let hx: u32;
                let mut hu: u32;
                let mut k: i32;

                hx = (ui >> 32) as u32;
                k = 1;
                if hx < 0x3fda827a || (hx >> 31) > 0 {
                    /* 1+x < sqrt(2)+ */
                    if hx >= 0xbff00000 {
                        /* x <= -1.0 */
                        if x == -1. {
                            return x / 0.0; /* log1p(-1) = -inf */
                        }
                        return (x - x) / 0.0; /* log1p(x<-1) = NaN */
                    }
                    if hx << 1 < 0x3ca00000 << 1 {
                        /* |x| < 2**-53 */
                        /* underflow if subnormal */
                        if (hx & 0x7ff00000) == 0 {
                            force_eval!(x as f32);
                        }
                        return x;
                    }
                    if hx <= 0xbfd2bec4 {
                        /* sqrt(2)/2- <= 1+x < sqrt(2)+ */
                        k = 0;
                        c = 0.;
                        f = x;
                    }
                } else if hx >= 0x7ff00000 {
                    return x;
                }
                if k > 0 {
                    ui = (1. + x).to_bits();
                    hu = (ui >> 32) as u32;
                    hu += 0x3ff00000 - 0x3fe6a09e;
                    k = (hu >> 20) as i32 - 0x3ff;
                    /* correction term ~ log(1+x)-log(u), avoid underflow in c/u */
                    if k < 54 {
                        c = if k >= 2 {
                            1. - (f64::from_bits(ui) - x)
                        } else {
                            x - (f64::from_bits(ui) - 1.)
                        };
                        c /= f64::from_bits(ui);
                    } else {
                        c = 0.;
                    }
                    /* reduce u into [sqrt(2)/2, sqrt(2)] */
                    hu = (hu & 0x000fffff) + 0x3fe6a09e;
                    ui = (hu as u64) << 32 | (ui & 0xffffffff);
                    f = f64::from_bits(ui) - 1.;
                }
                hfsq = 0.5 * f * f;
                s = f / (2.0 + f);
                z = s * s;
                w = z * z;
                t1 = w * (LG2 + w * (LG4 + w * LG6));
                t2 = z * (LG1 + w * (LG3 + w * (LG5 + w * LG7)));
                r = t2 + t1;
                dk = k as f64;
                s * (hfsq + r) + (dk * LN2_LO + c) - hfsq + f + dk * LN2_HI
            }
        }

        mod log1pf
        {
            use ::
            {
                *,
            };
            /*
            */
            const LN2_HI: f32 = 6.9313812256e-01; /* 0x3f317180 */
            const LN2_LO: f32 = 9.0580006145e-06; /* 0x3717f7d1 */
            /* |(log(1+s)-log(1-s))/s - Lg(s)| < 2**-34.24 (~[-4.95e-11, 4.97e-11]). */
            const LG1: f32 = 0.66666662693; /* 0xaaaaaa.0p-24 */
            const LG2: f32 = 0.40000972152; /* 0xccce13.0p-25 */
            const LG3: f32 = 0.28498786688; /* 0x91e9ee.0p-25 */
            const LG4: f32 = 0.24279078841; /* 0xf89e26.0p-26 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn log1pf(x: f32) -> f32
            {
                let mut ui: u32 = x.to_bits();
                let hfsq: f32;
                let mut f: f32 = 0.;
                let mut c: f32 = 0.;
                let s: f32;
                let z: f32;
                let r: f32;
                let w: f32;
                let t1: f32;
                let t2: f32;
                let dk: f32;
                let ix: u32;
                let mut iu: u32;
                let mut k: i32;

                ix = ui;
                k = 1;
                if ix < 0x3ed413d0 || (ix >> 31) > 0 {
                    /* 1+x < sqrt(2)+  */
                    if ix >= 0xbf800000 {
                        /* x <= -1.0 */
                        if x == -1. {
                            return x / 0.0; /* log1p(-1)=+inf */
                        }
                        return (x - x) / 0.0; /* log1p(x<-1)=NaN */
                    }
                    if ix << 1 < 0x33800000 << 1 {
                        /* |x| < 2**-24 */
                        /* underflow if subnormal */
                        if (ix & 0x7f800000) == 0 {
                            force_eval!(x * x);
                        }
                        return x;
                    }
                    if ix <= 0xbe95f619 {
                        /* sqrt(2)/2- <= 1+x < sqrt(2)+ */
                        k = 0;
                        c = 0.;
                        f = x;
                    }
                } else if ix >= 0x7f800000 {
                    return x;
                }
                if k > 0 {
                    ui = (1. + x).to_bits();
                    iu = ui;
                    iu += 0x3f800000 - 0x3f3504f3;
                    k = (iu >> 23) as i32 - 0x7f;
                    /* correction term ~ log(1+x)-log(u), avoid underflow in c/u */
                    if k < 25 {
                        c = if k >= 2 {
                            1. - (f32::from_bits(ui) - x)
                        } else {
                            x - (f32::from_bits(ui) - 1.)
                        };
                        c /= f32::from_bits(ui);
                    } else {
                        c = 0.;
                    }
                    /* reduce u into [sqrt(2)/2, sqrt(2)] */
                    iu = (iu & 0x007fffff) + 0x3f3504f3;
                    ui = iu;
                    f = f32::from_bits(ui) - 1.;
                }
                s = f / (2.0 + f);
                z = s * s;
                w = z * z;
                t1 = w * (LG2 + w * LG4);
                t2 = z * (LG1 + w * LG3);
                r = t2 + t1;
                hfsq = 0.5 * f * f;
                dk = k as f32;
                s * (hfsq + r) + (dk * LN2_LO + c) - hfsq + f + dk * LN2_HI
            }
        }

        mod log2
        {
            use ::
            {
                *,
            };
            /*
            */
            const IVLN2HI: f64 = 1.44269504072144627571e+00; /* 0x3ff71547, 0x65200000 */
            const IVLN2LO: f64 = 1.67517131648865118353e-10; /* 0x3de705fc, 0x2eefa200 */
            const LG1: f64 = 6.666666666666735130e-01; /* 3FE55555 55555593 */
            const LG2: f64 = 3.999999999940941908e-01; /* 3FD99999 9997FA04 */
            const LG3: f64 = 2.857142874366239149e-01; /* 3FD24924 94229359 */
            const LG4: f64 = 2.222219843214978396e-01; /* 3FCC71C5 1D8E78AF */
            const LG5: f64 = 1.818357216161805012e-01; /* 3FC74664 96CB03DE */
            const LG6: f64 = 1.531383769920937332e-01; /* 3FC39A09 D078C69F */
            const LG7: f64 = 1.479819860511658591e-01; /* 3FC2F112 DF3E5244 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn log2(mut x: f64) -> f64 {
                let x1p54 = f64::from_bits(0x4350000000000000); // 0x1p54 === 2 ^ 54

                let mut ui: u64 = x.to_bits();
                let hfsq: f64;
                let f: f64;
                let s: f64;
                let z: f64;
                let r: f64;
                let mut w: f64;
                let t1: f64;
                let t2: f64;
                let y: f64;
                let mut hi: f64;
                let lo: f64;
                let mut val_hi: f64;
                let mut val_lo: f64;
                let mut hx: u32;
                let mut k: i32;

                hx = (ui >> 32) as u32;
                k = 0;
                if hx < 0x00100000 || (hx >> 31) > 0 {
                    if ui << 1 == 0 {
                        return -1. / (x * x); /* log(+-0)=-inf */
                    }
                    if (hx >> 31) > 0 {
                        return (x - x) / 0.0; /* log(-#) = NaN */
                    }
                    /* subnormal number, scale x up */
                    k -= 54;
                    x *= x1p54;
                    ui = x.to_bits();
                    hx = (ui >> 32) as u32;
                } else if hx >= 0x7ff00000 {
                    return x;
                } else if hx == 0x3ff00000 && ui << 32 == 0 {
                    return 0.;
                }

                /* reduce x into [sqrt(2)/2, sqrt(2)] */
                hx += 0x3ff00000 - 0x3fe6a09e;
                k += (hx >> 20) as i32 - 0x3ff;
                hx = (hx & 0x000fffff) + 0x3fe6a09e;
                ui = (hx as u64) << 32 | (ui & 0xffffffff);
                x = f64::from_bits(ui);

                f = x - 1.0;
                hfsq = 0.5 * f * f;
                s = f / (2.0 + f);
                z = s * s;
                w = z * z;
                t1 = w * (LG2 + w * (LG4 + w * LG6));
                t2 = z * (LG1 + w * (LG3 + w * (LG5 + w * LG7)));
                r = t2 + t1;

                /* hi+lo = f - hfsq + s*(hfsq+R) ~ log(1+f) */
                hi = f - hfsq;
                ui = hi.to_bits();
                ui &= (-1i64 as u64) << 32;
                hi = f64::from_bits(ui);
                lo = f - hi - hfsq + s * (hfsq + r);

                val_hi = hi * IVLN2HI;
                val_lo = (lo + hi) * IVLN2LO + lo * IVLN2HI;

                /* spadd(val_hi, val_lo, y), except for not using double_t: */
                y = k.into();
                w = y + val_hi;
                val_lo += (y - w) + val_hi;
                val_hi = w;

                val_lo + val_hi
            }
        }

        mod log2f
        {
            use ::
            {
                *,
            };
            /*
            */
            const IVLN2HI: f32 = 1.4428710938e+00; /* 0x3fb8b000 */
            const IVLN2LO: f32 = -1.7605285393e-04; /* 0xb9389ad4 */
            /* |(log(1+s)-log(1-s))/s - Lg(s)| < 2**-34.24 (~[-4.95e-11, 4.97e-11]). */
            const LG1: f32 = 0.66666662693; /* 0xaaaaaa.0p-24 */
            const LG2: f32 = 0.40000972152; /* 0xccce13.0p-25 */
            const LG3: f32 = 0.28498786688; /* 0x91e9ee.0p-25 */
            const LG4: f32 = 0.24279078841; /* 0xf89e26.0p-26 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn log2f(mut x: f32) -> f32 {
                let x1p25f = f32::from_bits(0x4c000000); // 0x1p25f === 2 ^ 25

                let mut ui: u32 = x.to_bits();
                let hfsq: f32;
                let f: f32;
                let s: f32;
                let z: f32;
                let r: f32;
                let w: f32;
                let t1: f32;
                let t2: f32;
                let mut hi: f32;
                let lo: f32;
                let mut ix: u32;
                let mut k: i32;

                ix = ui;
                k = 0;
                if ix < 0x00800000 || (ix >> 31) > 0 {
                    /* x < 2**-126  */
                    if ix << 1 == 0 {
                        return -1. / (x * x); /* log(+-0)=-inf */
                    }
                    if (ix >> 31) > 0 {
                        return (x - x) / 0.0; /* log(-#) = NaN */
                    }
                    /* subnormal number, scale up x */
                    k -= 25;
                    x *= x1p25f;
                    ui = x.to_bits();
                    ix = ui;
                } else if ix >= 0x7f800000 {
                    return x;
                } else if ix == 0x3f800000 {
                    return 0.;
                }

                /* reduce x into [sqrt(2)/2, sqrt(2)] */
                ix += 0x3f800000 - 0x3f3504f3;
                k += (ix >> 23) as i32 - 0x7f;
                ix = (ix & 0x007fffff) + 0x3f3504f3;
                ui = ix;
                x = f32::from_bits(ui);

                f = x - 1.0;
                s = f / (2.0 + f);
                z = s * s;
                w = z * z;
                t1 = w * (LG2 + w * LG4);
                t2 = z * (LG1 + w * LG3);
                r = t2 + t1;
                hfsq = 0.5 * f * f;

                hi = f - hfsq;
                ui = hi.to_bits();
                ui &= 0xfffff000;
                hi = f32::from_bits(ui);
                lo = f - hi - hfsq + s * (hfsq + r);
                (lo + hi) * IVLN2LO + lo * IVLN2HI + hi * IVLN2HI + k as f32
            }
        }

        mod logf
        {
            use ::
            {
                *,
            };
            /*
            */
            const LN2_HI: f32 = 6.9313812256e-01; /* 0x3f317180 */
            const LN2_LO: f32 = 9.0580006145e-06; /* 0x3717f7d1 */
            /* |(log(1+s)-log(1-s))/s - Lg(s)| < 2**-34.24 (~[-4.95e-11, 4.97e-11]). */
            const LG1: f32 = 0.66666662693; /*  0xaaaaaa.0p-24*/
            const LG2: f32 = 0.40000972152; /*  0xccce13.0p-25 */
            const LG3: f32 = 0.28498786688; /*  0x91e9ee.0p-25 */
            const LG4: f32 = 0.24279078841; /*  0xf89e26.0p-26 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn logf(mut x: f32) -> f32 {
                let x1p25 = f32::from_bits(0x4c000000); // 0x1p25f === 2 ^ 25

                let mut ix = x.to_bits();
                let mut k = 0i32;

                if (ix < 0x00800000) || ((ix >> 31) != 0) {
                    /* x < 2**-126  */
                    if ix << 1 == 0 {
                        return -1. / (x * x); /* log(+-0)=-inf */
                    }
                    if (ix >> 31) != 0 {
                        return (x - x) / 0.; /* log(-#) = NaN */
                    }
                    /* subnormal number, scale up x */
                    k -= 25;
                    x *= x1p25;
                    ix = x.to_bits();
                } else if ix >= 0x7f800000 {
                    return x;
                } else if ix == 0x3f800000 {
                    return 0.;
                }

                /* reduce x into [sqrt(2)/2, sqrt(2)] */
                ix += 0x3f800000 - 0x3f3504f3;
                k += ((ix >> 23) as i32) - 0x7f;
                ix = (ix & 0x007fffff) + 0x3f3504f3;
                x = f32::from_bits(ix);

                let f = x - 1.;
                let s = f / (2. + f);
                let z = s * s;
                let w = z * z;
                let t1 = w * (LG2 + w * LG4);
                let t2 = z * (LG1 + w * LG3);
                let r = t2 + t1;
                let hfsq = 0.5 * f * f;
                let dk = k as f32;
                s * (hfsq + r) + dk * LN2_LO - hfsq + f + dk * LN2_HI
            }
        }

        mod modf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn modf(x: f64) -> (f64, f64)
            {
                let rv2: f64;
                let mut u = x.to_bits();
                let mask: u64;
                let e = ((u >> 52 & 0x7ff) as i32) - 0x3ff;

                /* no fractional part */
                if e >= 52 {
                    rv2 = x;
                    if e == 0x400 && (u << 12) != 0 {
                        /* nan */
                        return (x, rv2);
                    }
                    u &= 1 << 63;
                    return (f64::from_bits(u), rv2);
                }

                /* no integral part*/
                if e < 0 {
                    u &= 1 << 63;
                    rv2 = f64::from_bits(u);
                    return (x, rv2);
                }

                mask = ((!0) >> 12) >> e;
                if (u & mask) == 0 {
                    rv2 = x;
                    u &= 1 << 63;
                    return (f64::from_bits(u), rv2);
                }
                u &= !mask;
                rv2 = f64::from_bits(u);
                return (x - rv2, rv2);
            }
        }

        mod modff
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn modff(x: f32) -> (f32, f32)
            {
                let rv2: f32;
                let mut u: u32 = x.to_bits();
                let mask: u32;
                let e = ((u >> 23 & 0xff) as i32) - 0x7f;

                /* no fractional part */
                if e >= 23 {
                    rv2 = x;
                    if e == 0x80 && (u << 9) != 0 {
                        /* nan */
                        return (x, rv2);
                    }
                    u &= 0x80000000;
                    return (f32::from_bits(u), rv2);
                }
                /* no integral part */
                if e < 0 {
                    u &= 0x80000000;
                    rv2 = f32::from_bits(u);
                    return (x, rv2);
                }

                mask = 0x007fffff >> e;
                if (u & mask) == 0 {
                    rv2 = x;
                    u &= 0x80000000;
                    return (f32::from_bits(u), rv2);
                }
                u &= !mask;
                rv2 = f32::from_bits(u);
                return (x - rv2, rv2);
            }
        }

        mod nextafter
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn nextafter(x: f64, y: f64) -> f64
            {
                if x.is_nan() || y.is_nan() {
                    return x + y;
                }

                let mut ux_i = x.to_bits();
                let uy_i = y.to_bits();
                if ux_i == uy_i {
                    return y;
                }

                let ax = ux_i & !1_u64 / 2;
                let ay = uy_i & !1_u64 / 2;
                if ax == 0 {
                    if ay == 0 {
                        return y;
                    }
                    ux_i = (uy_i & 1_u64 << 63) | 1;
                } else if ax > ay || ((ux_i ^ uy_i) & 1_u64 << 63) != 0 {
                    ux_i -= 1;
                } else {
                    ux_i += 1;
                }

                let e = ux_i.wrapping_shr(52 & 0x7ff);
                // raise overflow if ux.f is infinite and x is finite
                if e == 0x7ff {
                    force_eval!(x + x);
                }
                let ux_f = f64::from_bits(ux_i);
                // raise underflow if ux.f is subnormal or zero
                if e == 0 {
                    force_eval!(x * x + ux_f * ux_f);
                }
                ux_f
            }
        }

        mod nextafterf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn nextafterf(x: f32, y: f32) -> f32 {
                if x.is_nan() || y.is_nan() {
                    return x + y;
                }

                let mut ux_i = x.to_bits();
                let uy_i = y.to_bits();
                if ux_i == uy_i {
                    return y;
                }

                let ax = ux_i & 0x7fff_ffff_u32;
                let ay = uy_i & 0x7fff_ffff_u32;
                if ax == 0 {
                    if ay == 0 {
                        return y;
                    }
                    ux_i = (uy_i & 0x8000_0000_u32) | 1;
                } else if ax > ay || ((ux_i ^ uy_i) & 0x8000_0000_u32) != 0 {
                    ux_i -= 1;
                } else {
                    ux_i += 1;
                }

                let e = ux_i.wrapping_shr(0x7f80_0000_u32);
                // raise overflow if ux_f is infinite and x is finite
                if e == 0x7f80_0000_u32 {
                    force_eval!(x + x);
                }
                let ux_f = f32::from_bits(ux_i);
                // raise underflow if ux_f is subnormal or zero
                if e == 0 {
                    force_eval!(x * x + ux_f * ux_f);
                }
                ux_f
            }
        }

        mod pow
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{fabs, get_high_word, scalbn, sqrt, with_set_high_word, with_set_low_word};

            const BP: [f64; 2] = [1.0, 1.5];
            const DP_H: [f64; 2] = [0.0, 5.84962487220764160156e-01]; /* 0x3fe2b803_40000000 */
            const DP_L: [f64; 2] = [0.0, 1.35003920212974897128e-08]; /* 0x3E4CFDEB, 0x43CFD006 */
            const TWO53: f64 = 9007199254740992.0; /* 0x43400000_00000000 */
            const HUGE: f64 = 1.0e300;
            const TINY: f64 = 1.0e-300;

            // poly coefs for (3/2)*(log(x)-2s-2/3*s**3:
            const L1: f64 = 5.99999999999994648725e-01; /* 0x3fe33333_33333303 */
            const L2: f64 = 4.28571428578550184252e-01; /* 0x3fdb6db6_db6fabff */
            const L3: f64 = 3.33333329818377432918e-01; /* 0x3fd55555_518f264d */
            const L4: f64 = 2.72728123808534006489e-01; /* 0x3fd17460_a91d4101 */
            const L5: f64 = 2.30660745775561754067e-01; /* 0x3fcd864a_93c9db65 */
            const L6: f64 = 2.06975017800338417784e-01; /* 0x3fca7e28_4a454eef */
            const P1: f64 = 1.66666666666666019037e-01; /* 0x3fc55555_5555553e */
            const P2: f64 = -2.77777777770155933842e-03; /* 0xbf66c16c_16bebd93 */
            const P3: f64 = 6.61375632143793436117e-05; /* 0x3f11566a_af25de2c */
            const P4: f64 = -1.65339022054652515390e-06; /* 0xbebbbd41_c5d26bf1 */
            const P5: f64 = 4.13813679705723846039e-08; /* 0x3e663769_72bea4d0 */
            const LG2: f64 = 6.93147180559945286227e-01; /* 0x3fe62e42_fefa39ef */
            const LG2_H: f64 = 6.93147182464599609375e-01; /* 0x3fe62e43_00000000 */
            const LG2_L: f64 = -1.90465429995776804525e-09; /* 0xbe205c61_0ca86c39 */
            const OVT: f64 = 8.0085662595372944372e-017; /* -(1024-log2(ovfl+.5ulp)) */
            const CP: f64 = 9.61796693925975554329e-01; /* 0x3feec709_dc3a03fd =2/(3ln2) */
            const CP_H: f64 = 9.61796700954437255859e-01; /* 0x3feec709_e0000000 =(float)cp */
            const CP_L: f64 = -7.02846165095275826516e-09; /* 0xbe3e2fe0_145b01f5 =tail of cp_h*/
            const IVLN2: f64 = 1.44269504088896338700e+00; /* 0x3ff71547_652b82fe =1/ln2 */
            const IVLN2_H: f64 = 1.44269502162933349609e+00; /* 0x3ff71547_60000000 =24b 1/ln2*/
            const IVLN2_L: f64 = 1.92596299112661746887e-08; /* 0x3e54ae0b_f85ddf44 =1/ln2 tail*/

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn pow(x: f64, y: f64) -> f64 {
                let t1: f64;
                let t2: f64;

                let (hx, lx): (i32, u32) = ((x.to_bits() >> 32) as i32, x.to_bits() as u32);
                let (hy, ly): (i32, u32) = ((y.to_bits() >> 32) as i32, y.to_bits() as u32);

                let mut ix: i32 = (hx & 0x7fffffff) as i32;
                let iy: i32 = (hy & 0x7fffffff) as i32;

                /* x**0 = 1, even if x is NaN */
                if ((iy as u32) | ly) == 0 {
                    return 1.0;
                }

                /* 1**y = 1, even if y is NaN */
                if hx == 0x3ff00000 && lx == 0 {
                    return 1.0;
                }

                /* NaN if either arg is NaN */
                if ix > 0x7ff00000
                    || (ix == 0x7ff00000 && lx != 0)
                    || iy > 0x7ff00000
                    || (iy == 0x7ff00000 && ly != 0)
                {
                    return x + y;
                }

                /* determine if y is an odd int when x < 0
                * yisint = 0       ... y is not an integer
                * yisint = 1       ... y is an odd int
                * yisint = 2       ... y is an even int
                */
                let mut yisint: i32 = 0;
                let mut k: i32;
                let mut j: i32;
                if hx < 0 {
                    if iy >= 0x43400000 {
                        yisint = 2; /* even integer y */
                    } else if iy >= 0x3ff00000 {
                        k = (iy >> 20) - 0x3ff; /* exponent */

                        if k > 20 {
                            j = (ly >> (52 - k)) as i32;

                            if (j << (52 - k)) == (ly as i32) {
                                yisint = 2 - (j & 1);
                            }
                        } else if ly == 0 {
                            j = iy >> (20 - k);

                            if (j << (20 - k)) == iy {
                                yisint = 2 - (j & 1);
                            }
                        }
                    }
                }

                if ly == 0 {
                    /* special value of y */
                    if iy == 0x7ff00000 {
                        /* y is +-inf */

                        return if ((ix - 0x3ff00000) | (lx as i32)) == 0 {
                            /* (-1)**+-inf is 1 */
                            1.0
                        } else if ix >= 0x3ff00000 {
                            /* (|x|>1)**+-inf = inf,0 */
                            if hy >= 0 {
                                y
                            } else {
                                0.0
                            }
                        } else {
                            /* (|x|<1)**+-inf = 0,inf */
                            if hy >= 0 {
                                0.0
                            } else {
                                -y
                            }
                        };
                    }

                    if iy == 0x3ff00000 {
                        /* y is +-1 */
                        return if hy >= 0 { x } else { 1.0 / x };
                    }

                    if hy == 0x40000000 {
                        /* y is 2 */
                        return x * x;
                    }

                    if hy == 0x3fe00000 {
                        /* y is 0.5 */
                        if hx >= 0 {
                            /* x >= +0 */
                            return sqrt(x);
                        }
                    }
                }

                let mut ax: f64 = fabs(x);
                if lx == 0 {
                    /* special value of x */
                    if ix == 0x7ff00000 || ix == 0 || ix == 0x3ff00000 {
                        /* x is +-0,+-inf,+-1 */
                        let mut z: f64 = ax;

                        if hy < 0 {
                            /* z = (1/|x|) */
                            z = 1.0 / z;
                        }

                        if hx < 0 {
                            if ((ix - 0x3ff00000) | yisint) == 0 {
                                z = (z - z) / (z - z); /* (-1)**non-int is NaN */
                            } else if yisint == 1 {
                                z = -z; /* (x<0)**odd = -(|x|**odd) */
                            }
                        }

                        return z;
                    }
                }

                let mut s: f64 = 1.0; /* sign of result */
                if hx < 0 {
                    if yisint == 0 {
                        /* (x<0)**(non-int) is NaN */
                        return (x - x) / (x - x);
                    }

                    if yisint == 1 {
                        /* (x<0)**(odd int) */
                        s = -1.0;
                    }
                }

                /* |y| is HUGE */
                if iy > 0x41e00000 {
                    /* if |y| > 2**31 */
                    if iy > 0x43f00000 {
                        /* if |y| > 2**64, must o/uflow */
                        if ix <= 0x3fefffff {
                            return if hy < 0 { HUGE * HUGE } else { TINY * TINY };
                        }

                        if ix >= 0x3ff00000 {
                            return if hy > 0 { HUGE * HUGE } else { TINY * TINY };
                        }
                    }

                    /* over/underflow if x is not close to one */
                    if ix < 0x3fefffff {
                        return if hy < 0 {
                            s * HUGE * HUGE
                        } else {
                            s * TINY * TINY
                        };
                    }
                    if ix > 0x3ff00000 {
                        return if hy > 0 {
                            s * HUGE * HUGE
                        } else {
                            s * TINY * TINY
                        };
                    }

                    /* now |1-x| is TINY <= 2**-20, suffice to compute
                    log(x) by x-x^2/2+x^3/3-x^4/4 */
                    let t: f64 = ax - 1.0; /* t has 20 trailing zeros */
                    let w: f64 = (t * t) * (0.5 - t * (0.3333333333333333333333 - t * 0.25));
                    let u: f64 = IVLN2_H * t; /* ivln2_h has 21 sig. bits */
                    let v: f64 = t * IVLN2_L - w * IVLN2;
                    t1 = with_set_low_word(u + v, 0);
                    t2 = v - (t1 - u);
                } else {
                    // double ss,s2,s_h,s_l,t_h,t_l;
                    let mut n: i32 = 0;

                    if ix < 0x00100000 {
                        /* take care subnormal number */
                        ax *= TWO53;
                        n -= 53;
                        ix = get_high_word(ax) as i32;
                    }

                    n += (ix >> 20) - 0x3ff;
                    j = ix & 0x000fffff;

                    /* determine interval */
                    let k: i32;
                    ix = j | 0x3ff00000; /* normalize ix */
                    if j <= 0x3988E {
                        /* |x|<sqrt(3/2) */
                        k = 0;
                    } else if j < 0xBB67A {
                        /* |x|<sqrt(3)   */
                        k = 1;
                    } else {
                        k = 0;
                        n += 1;
                        ix -= 0x00100000;
                    }
                    ax = with_set_high_word(ax, ix as u32);

                    /* compute ss = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5) */
                    let u: f64 = ax - BP[k as usize]; /* bp[0]=1.0, bp[1]=1.5 */
                    let v: f64 = 1.0 / (ax + BP[k as usize]);
                    let ss: f64 = u * v;
                    let s_h = with_set_low_word(ss, 0);

                    /* t_h=ax+bp[k] High */
                    let t_h: f64 = with_set_high_word(
                        0.0,
                        ((ix as u32 >> 1) | 0x20000000) + 0x00080000 + ((k as u32) << 18),
                    );
                    let t_l: f64 = ax - (t_h - BP[k as usize]);
                    let s_l: f64 = v * ((u - s_h * t_h) - s_h * t_l);

                    /* compute log(ax) */
                    let s2: f64 = ss * ss;
                    let mut r: f64 = s2 * s2 * (L1 + s2 * (L2 + s2 * (L3 + s2 * (L4 + s2 * (L5 + s2 * L6)))));
                    r += s_l * (s_h + ss);
                    let s2: f64 = s_h * s_h;
                    let t_h: f64 = with_set_low_word(3.0 + s2 + r, 0);
                    let t_l: f64 = r - ((t_h - 3.0) - s2);

                    /* u+v = ss*(1+...) */
                    let u: f64 = s_h * t_h;
                    let v: f64 = s_l * t_h + t_l * ss;

                    /* 2/(3log2)*(ss+...) */
                    let p_h: f64 = with_set_low_word(u + v, 0);
                    let p_l = v - (p_h - u);
                    let z_h: f64 = CP_H * p_h; /* cp_h+cp_l = 2/(3*log2) */
                    let z_l: f64 = CP_L * p_h + p_l * CP + DP_L[k as usize];

                    /* log2(ax) = (ss+..)*2/(3*log2) = n + dp_h + z_h + z_l */
                    let t: f64 = n as f64;
                    t1 = with_set_low_word(((z_h + z_l) + DP_H[k as usize]) + t, 0);
                    t2 = z_l - (((t1 - t) - DP_H[k as usize]) - z_h);
                }

                /* split up y into y1+y2 and compute (y1+y2)*(t1+t2) */
                let y1: f64 = with_set_low_word(y, 0);
                let p_l: f64 = (y - y1) * t1 + y * t2;
                let mut p_h: f64 = y1 * t1;
                let z: f64 = p_l + p_h;
                let mut j: i32 = (z.to_bits() >> 32) as i32;
                let i: i32 = z.to_bits() as i32;
                // let (j, i): (i32, i32) = ((z.to_bits() >> 32) as i32, z.to_bits() as i32);

                if j >= 0x40900000 {
                    /* z >= 1024 */
                    if (j - 0x40900000) | i != 0 {
                        /* if z > 1024 */
                        return s * HUGE * HUGE; /* overflow */
                    }

                    if p_l + OVT > z - p_h {
                        return s * HUGE * HUGE; /* overflow */
                    }
                } else if (j & 0x7fffffff) >= 0x4090cc00 {
                    /* z <= -1075 */
                    // FIXME: instead of abs(j) use unsigned j

                    if (((j as u32) - 0xc090cc00) | (i as u32)) != 0 {
                        /* z < -1075 */
                        return s * TINY * TINY; /* underflow */
                    }

                    if p_l <= z - p_h {
                        return s * TINY * TINY; /* underflow */
                    }
                }

                /* compute 2**(p_h+p_l) */
                let i: i32 = j & (0x7fffffff as i32);
                k = (i >> 20) - 0x3ff;
                let mut n: i32 = 0;

                if i > 0x3fe00000 {
                    /* if |z| > 0.5, set n = [z+0.5] */
                    n = j + (0x00100000 >> (k + 1));
                    k = ((n & 0x7fffffff) >> 20) - 0x3ff; /* new k for n */
                    let t: f64 = with_set_high_word(0.0, (n & !(0x000fffff >> k)) as u32);
                    n = ((n & 0x000fffff) | 0x00100000) >> (20 - k);
                    if j < 0 {
                        n = -n;
                    }
                    p_h -= t;
                }

                let t: f64 = with_set_low_word(p_l + p_h, 0);
                let u: f64 = t * LG2_H;
                let v: f64 = (p_l - (t - p_h)) * LG2 + t * LG2_L;
                let mut z: f64 = u + v;
                let w: f64 = v - (z - u);
                let t: f64 = z * z;
                let t1: f64 = z - t * (P1 + t * (P2 + t * (P3 + t * (P4 + t * P5))));
                let r: f64 = (z * t1) / (t1 - 2.0) - (w + z * w);
                z = 1.0 - (r - z);
                j = get_high_word(z) as i32;
                j += n << 20;

                if (j >> 20) <= 0 {
                    /* subnormal output */
                    z = scalbn(z, n);
                } else {
                    z = with_set_high_word(z, j as u32);
                }

                s * z
            }
        }

        mod powf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{fabsf, scalbnf, sqrtf};

            const BP: [f32; 2] = [1.0, 1.5];
            const DP_H: [f32; 2] = [0.0, 5.84960938e-01]; /* 0x3f15c000 */
            const DP_L: [f32; 2] = [0.0, 1.56322085e-06]; /* 0x35d1cfdc */
            const TWO24: f32 = 16777216.0; /* 0x4b800000 */
            const HUGE: f32 = 1.0e30;
            const TINY: f32 = 1.0e-30;
            const L1: f32 = 6.0000002384e-01; /* 0x3f19999a */
            const L2: f32 = 4.2857143283e-01; /* 0x3edb6db7 */
            const L3: f32 = 3.3333334327e-01; /* 0x3eaaaaab */
            const L4: f32 = 2.7272811532e-01; /* 0x3e8ba305 */
            const L5: f32 = 2.3066075146e-01; /* 0x3e6c3255 */
            const L6: f32 = 2.0697501302e-01; /* 0x3e53f142 */
            const P1: f32 = 1.6666667163e-01; /* 0x3e2aaaab */
            const P2: f32 = -2.7777778450e-03; /* 0xbb360b61 */
            const P3: f32 = 6.6137559770e-05; /* 0x388ab355 */
            const P4: f32 = -1.6533901999e-06; /* 0xb5ddea0e */
            const P5: f32 = 4.1381369442e-08; /* 0x3331bb4c */
            const LG2: f32 = 6.9314718246e-01; /* 0x3f317218 */
            const LG2_H: f32 = 6.93145752e-01; /* 0x3f317200 */
            const LG2_L: f32 = 1.42860654e-06; /* 0x35bfbe8c */
            const OVT: f32 = 4.2995665694e-08; /* -(128-log2(ovfl+.5ulp)) */
            const CP: f32 = 9.6179670095e-01; /* 0x3f76384f =2/(3ln2) */
            const CP_H: f32 = 9.6191406250e-01; /* 0x3f764000 =12b cp */
            const CP_L: f32 = -1.1736857402e-04; /* 0xb8f623c6 =tail of cp_h */
            const IVLN2: f32 = 1.4426950216e+00;
            const IVLN2_H: f32 = 1.4426879883e+00;
            const IVLN2_L: f32 = 7.0526075433e-06;

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn powf(x: f32, y: f32) -> f32 {
                let mut z: f32;
                let mut ax: f32;
                let z_h: f32;
                let z_l: f32;
                let mut p_h: f32;
                let mut p_l: f32;
                let y1: f32;
                let mut t1: f32;
                let t2: f32;
                let mut r: f32;
                let s: f32;
                let mut sn: f32;
                let mut t: f32;
                let mut u: f32;
                let mut v: f32;
                let mut w: f32;
                let i: i32;
                let mut j: i32;
                let mut k: i32;
                let mut yisint: i32;
                let mut n: i32;
                let hx: i32;
                let hy: i32;
                let mut ix: i32;
                let iy: i32;
                let mut is: i32;

                hx = x.to_bits() as i32;
                hy = y.to_bits() as i32;

                ix = hx & 0x7fffffff;
                iy = hy & 0x7fffffff;

                /* x**0 = 1, even if x is NaN */
                if iy == 0 {
                    return 1.0;
                }

                /* 1**y = 1, even if y is NaN */
                if hx == 0x3f800000 {
                    return 1.0;
                }

                /* NaN if either arg is NaN */
                if ix > 0x7f800000 || iy > 0x7f800000 {
                    return x + y;
                }

                /* determine if y is an odd int when x < 0
                * yisint = 0       ... y is not an integer
                * yisint = 1       ... y is an odd int
                * yisint = 2       ... y is an even int
                */
                yisint = 0;
                if hx < 0 {
                    if iy >= 0x4b800000 {
                        yisint = 2; /* even integer y */
                    } else if iy >= 0x3f800000 {
                        k = (iy >> 23) - 0x7f; /* exponent */
                        j = iy >> (23 - k);
                        if (j << (23 - k)) == iy {
                            yisint = 2 - (j & 1);
                        }
                    }
                }

                /* special value of y */
                if iy == 0x7f800000 {
                    /* y is +-inf */
                    if ix == 0x3f800000 {
                        /* (-1)**+-inf is 1 */
                        return 1.0;
                    } else if ix > 0x3f800000 {
                        /* (|x|>1)**+-inf = inf,0 */
                        return if hy >= 0 { y } else { 0.0 };
                    } else {
                        /* (|x|<1)**+-inf = 0,inf */
                        return if hy >= 0 { 0.0 } else { -y };
                    }
                }
                if iy == 0x3f800000 {
                    /* y is +-1 */
                    return if hy >= 0 { x } else { 1.0 / x };
                }

                if hy == 0x40000000 {
                    /* y is 2 */
                    return x * x;
                }

                if hy == 0x3f000000
                /* y is  0.5 */
                && hx >= 0
                {
                    /* x >= +0 */
                    return sqrtf(x);
                }

                ax = fabsf(x);
                /* special value of x */
                if ix == 0x7f800000 || ix == 0 || ix == 0x3f800000 {
                    /* x is +-0,+-inf,+-1 */
                    z = ax;
                    if hy < 0 {
                        /* z = (1/|x|) */
                        z = 1.0 / z;
                    }

                    if hx < 0 {
                        if ((ix - 0x3f800000) | yisint) == 0 {
                            z = (z - z) / (z - z); /* (-1)**non-int is NaN */
                        } else if yisint == 1 {
                            z = -z; /* (x<0)**odd = -(|x|**odd) */
                        }
                    }
                    return z;
                }

                sn = 1.0; /* sign of result */
                if hx < 0 {
                    if yisint == 0 {
                        /* (x<0)**(non-int) is NaN */
                        return (x - x) / (x - x);
                    }

                    if yisint == 1 {
                        /* (x<0)**(odd int) */
                        sn = -1.0;
                    }
                }

                /* |y| is HUGE */
                if iy > 0x4d000000 {
                    /* if |y| > 2**27 */
                    /* over/underflow if x is not close to one */
                    if ix < 0x3f7ffff8 {
                        return if hy < 0 {
                            sn * HUGE * HUGE
                        } else {
                            sn * TINY * TINY
                        };
                    }

                    if ix > 0x3f800007 {
                        return if hy > 0 {
                            sn * HUGE * HUGE
                        } else {
                            sn * TINY * TINY
                        };
                    }

                    /* now |1-x| is TINY <= 2**-20, suffice to compute
                    log(x) by x-x^2/2+x^3/3-x^4/4 */
                    t = ax - 1.; /* t has 20 trailing zeros */
                    w = (t * t) * (0.5 - t * (0.333333333333 - t * 0.25));
                    u = IVLN2_H * t; /* IVLN2_H has 16 sig. bits */
                    v = t * IVLN2_L - w * IVLN2;
                    t1 = u + v;
                    is = t1.to_bits() as i32;
                    t1 = f32::from_bits(is as u32 & 0xfffff000);
                    t2 = v - (t1 - u);
                } else {
                    let mut s2: f32;
                    let mut s_h: f32;
                    let s_l: f32;
                    let mut t_h: f32;
                    let mut t_l: f32;

                    n = 0;
                    /* take care subnormal number */
                    if ix < 0x00800000 {
                        ax *= TWO24;
                        n -= 24;
                        ix = ax.to_bits() as i32;
                    }
                    n += ((ix) >> 23) - 0x7f;
                    j = ix & 0x007fffff;
                    /* determine interval */
                    ix = j | 0x3f800000; /* normalize ix */
                    if j <= 0x1cc471 {
                        /* |x|<sqrt(3/2) */
                        k = 0;
                    } else if j < 0x5db3d7 {
                        /* |x|<sqrt(3)   */
                        k = 1;
                    } else {
                        k = 0;
                        n += 1;
                        ix -= 0x00800000;
                    }
                    ax = f32::from_bits(ix as u32);

                    /* compute s = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5) */
                    u = ax - BP[k as usize]; /* bp[0]=1.0, bp[1]=1.5 */
                    v = 1.0 / (ax + BP[k as usize]);
                    s = u * v;
                    s_h = s;
                    is = s_h.to_bits() as i32;
                    s_h = f32::from_bits(is as u32 & 0xfffff000);
                    /* t_h=ax+bp[k] High */
                    is = (((ix as u32 >> 1) & 0xfffff000) | 0x20000000) as i32;
                    t_h = f32::from_bits(is as u32 + 0x00400000 + ((k as u32) << 21));
                    t_l = ax - (t_h - BP[k as usize]);
                    s_l = v * ((u - s_h * t_h) - s_h * t_l);
                    /* compute log(ax) */
                    s2 = s * s;
                    r = s2 * s2 * (L1 + s2 * (L2 + s2 * (L3 + s2 * (L4 + s2 * (L5 + s2 * L6)))));
                    r += s_l * (s_h + s);
                    s2 = s_h * s_h;
                    t_h = 3.0 + s2 + r;
                    is = t_h.to_bits() as i32;
                    t_h = f32::from_bits(is as u32 & 0xfffff000);
                    t_l = r - ((t_h - 3.0) - s2);
                    /* u+v = s*(1+...) */
                    u = s_h * t_h;
                    v = s_l * t_h + t_l * s;
                    /* 2/(3log2)*(s+...) */
                    p_h = u + v;
                    is = p_h.to_bits() as i32;
                    p_h = f32::from_bits(is as u32 & 0xfffff000);
                    p_l = v - (p_h - u);
                    z_h = CP_H * p_h; /* cp_h+cp_l = 2/(3*log2) */
                    z_l = CP_L * p_h + p_l * CP + DP_L[k as usize];
                    /* log2(ax) = (s+..)*2/(3*log2) = n + dp_h + z_h + z_l */
                    t = n as f32;
                    t1 = ((z_h + z_l) + DP_H[k as usize]) + t;
                    is = t1.to_bits() as i32;
                    t1 = f32::from_bits(is as u32 & 0xfffff000);
                    t2 = z_l - (((t1 - t) - DP_H[k as usize]) - z_h);
                };

                /* split up y into y1+y2 and compute (y1+y2)*(t1+t2) */
                is = y.to_bits() as i32;
                y1 = f32::from_bits(is as u32 & 0xfffff000);
                p_l = (y - y1) * t1 + y * t2;
                p_h = y1 * t1;
                z = p_l + p_h;
                j = z.to_bits() as i32;
                if j > 0x43000000 {
                    /* if z > 128 */
                    return sn * HUGE * HUGE; /* overflow */
                } else if j == 0x43000000 {
                    /* if z == 128 */
                    if p_l + OVT > z - p_h {
                        return sn * HUGE * HUGE; /* overflow */
                    }
                } else if (j & 0x7fffffff) > 0x43160000 {
                    /* z < -150 */
                    // FIXME: check should be  (uint32_t)j > 0xc3160000
                    return sn * TINY * TINY; /* underflow */
                } else if j as u32 == 0xc3160000
                        /* z == -150 */
                        && p_l <= z - p_h
                {
                    return sn * TINY * TINY; /* underflow */
                }

                /*
                * compute 2**(p_h+p_l)
                */
                i = j & 0x7fffffff;
                k = (i >> 23) - 0x7f;
                n = 0;
                if i > 0x3f000000 {
                    /* if |z| > 0.5, set n = [z+0.5] */
                    n = j + (0x00800000 >> (k + 1));
                    k = ((n & 0x7fffffff) >> 23) - 0x7f; /* new k for n */
                    t = f32::from_bits(n as u32 & !(0x007fffff >> k));
                    n = ((n & 0x007fffff) | 0x00800000) >> (23 - k);
                    if j < 0 {
                        n = -n;
                    }
                    p_h -= t;
                }
                t = p_l + p_h;
                is = t.to_bits() as i32;
                t = f32::from_bits(is as u32 & 0xffff8000);
                u = t * LG2_H;
                v = (p_l - (t - p_h)) * LG2 + t * LG2_L;
                z = u + v;
                w = v - (z - u);
                t = z * z;
                t1 = z - t * (P1 + t * (P2 + t * (P3 + t * (P4 + t * P5))));
                r = (z * t1) / (t1 - 2.0) - (w + z * w);
                z = 1.0 - (r - z);
                j = z.to_bits() as i32;
                j += n << 23;
                if (j >> 23) <= 0 {
                    /* subnormal output */
                    z = scalbnf(z, n);
                } else {
                    z = f32::from_bits(j as u32);
                }
                sn * z
            }
        }

        mod remainder
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn remainder(x: f64, y: f64) -> f64 {
                let (result, _) = super::remquo(x, y);
                result
            }
        }

        mod remainderf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn remainderf(x: f32, y: f32) -> f32 {
                let (result, _) = super::remquof(x, y);
                result
            }
        }

        mod remquo
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn remquo(mut x: f64, mut y: f64) -> (f64, i32)
            {
                let ux: u64 = x.to_bits();
                let mut uy: u64 = y.to_bits();
                let mut ex = ((ux >> 52) & 0x7ff) as i32;
                let mut ey = ((uy >> 52) & 0x7ff) as i32;
                let sx = (ux >> 63) != 0;
                let sy = (uy >> 63) != 0;
                let mut q: u32;
                let mut i: u64;
                let mut uxi: u64 = ux;

                if (uy << 1) == 0 || y.is_nan() || ex == 0x7ff {
                    return ((x * y) / (x * y), 0);
                }
                if (ux << 1) == 0 {
                    return (x, 0);
                }

                /* normalize x and y */
                if ex == 0 {
                    i = uxi << 12;
                    while (i >> 63) == 0 {
                        ex -= 1;
                        i <<= 1;
                    }
                    uxi <<= -ex + 1;
                } else {
                    uxi &= (!0) >> 12;
                    uxi |= 1 << 52;
                }
                if ey == 0 {
                    i = uy << 12;
                    while (i >> 63) == 0 {
                        ey -= 1;
                        i <<= 1;
                    }
                    uy <<= -ey + 1;
                } else {
                    uy &= (!0) >> 12;
                    uy |= 1 << 52;
                }

                q = 0;

                if ex + 1 != ey {
                    if ex < ey {
                        return (x, 0);
                    }
                    /* x mod y */
                    while ex > ey {
                        i = uxi.wrapping_sub(uy);
                        if (i >> 63) == 0 {
                            uxi = i;
                            q += 1;
                        }
                        uxi <<= 1;
                        q <<= 1;
                        ex -= 1;
                    }
                    i = uxi.wrapping_sub(uy);
                    if (i >> 63) == 0 {
                        uxi = i;
                        q += 1;
                    }
                    if uxi == 0 {
                        ex = -60;
                    } else {
                        while (uxi >> 52) == 0 {
                            uxi <<= 1;
                            ex -= 1;
                        }
                    }
                }

                /* scale result and decide between |x| and |x|-|y| */
                if ex > 0 {
                    uxi -= 1 << 52;
                    uxi |= (ex as u64) << 52;
                } else {
                    uxi >>= -ex + 1;
                }
                x = f64::from_bits(uxi);
                if sy {
                    y = -y;
                }
                if ex == ey || (ex + 1 == ey && (2.0 * x > y || (2.0 * x == y && (q % 2) != 0))) {
                    x -= y;
                    // TODO: this matches musl behavior, but it is incorrect
                    q = q.wrapping_add(1);
                }
                q &= 0x7fffffff;
                let quo = if sx ^ sy { -(q as i32) } else { q as i32 };
                if sx {
                    (-x, quo)
                } else {
                    (x, quo)
                }
            }
        }

        mod remquof
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn remquof(mut x: f32, mut y: f32) -> (f32, i32)
            {
                let ux: u32 = x.to_bits();
                let mut uy: u32 = y.to_bits();
                let mut ex = ((ux >> 23) & 0xff) as i32;
                let mut ey = ((uy >> 23) & 0xff) as i32;
                let sx = (ux >> 31) != 0;
                let sy = (uy >> 31) != 0;
                let mut q: u32;
                let mut i: u32;
                let mut uxi: u32 = ux;

                if (uy << 1) == 0 || y.is_nan() || ex == 0xff {
                    return ((x * y) / (x * y), 0);
                }
                if (ux << 1) == 0 {
                    return (x, 0);
                }

                /* normalize x and y */
                if ex == 0 {
                    i = uxi << 9;
                    while (i >> 31) == 0 {
                        ex -= 1;
                        i <<= 1;
                    }
                    uxi <<= -ex + 1;
                } else {
                    uxi &= (!0) >> 9;
                    uxi |= 1 << 23;
                }
                if ey == 0 {
                    i = uy << 9;
                    while (i >> 31) == 0 {
                        ey -= 1;
                        i <<= 1;
                    }
                    uy <<= -ey + 1;
                } else {
                    uy &= (!0) >> 9;
                    uy |= 1 << 23;
                }

                q = 0;
                if ex + 1 != ey {
                    if ex < ey {
                        return (x, 0);
                    }
                    /* x mod y */
                    while ex > ey {
                        i = uxi.wrapping_sub(uy);
                        if (i >> 31) == 0 {
                            uxi = i;
                            q += 1;
                        }
                        uxi <<= 1;
                        q <<= 1;
                        ex -= 1;
                    }
                    i = uxi.wrapping_sub(uy);
                    if (i >> 31) == 0 {
                        uxi = i;
                        q += 1;
                    }
                    if uxi == 0 {
                        ex = -30;
                    } else {
                        while (uxi >> 23) == 0 {
                            uxi <<= 1;
                            ex -= 1;
                        }
                    }
                }

                /* scale result and decide between |x| and |x|-|y| */
                if ex > 0 {
                    uxi -= 1 << 23;
                    uxi |= (ex as u32) << 23;
                } else {
                    uxi >>= -ex + 1;
                }
                x = f32::from_bits(uxi);
                if sy {
                    y = -y;
                }
                if ex == ey || (ex + 1 == ey && (2.0 * x > y || (2.0 * x == y && (q % 2) != 0))) {
                    x -= y;
                    q += 1;
                }
                q &= 0x7fffffff;
                let quo = if sx ^ sy { -(q as i32) } else { q as i32 };
                if sx {
                    (-x, quo)
                } else {
                    (x, quo)
                }
            }
        }

        mod round
        {
            use ::
            {
                *,
            };
            /*
            */
            const TOINT: f64 = 1.0 / f64::EPSILON;
            
            pub fn round(mut x: f64) -> f64 {
                let i = x.to_bits();
                let e: u64 = i >> 52 & 0x7ff;
                let mut y: f64;

                if e >= 0x3ff + 52 {
                    return x;
                }
                if e < 0x3ff - 1 {
                    // raise inexact if x!=0
                    force_eval!(x + TOINT);
                    return 0.0 * x;
                }
                if i >> 63 != 0 {
                    x = -x;
                }
                y = x + TOINT - TOINT - x;
                if y > 0.5 {
                    y = y + x - 1.0;
                } else if y <= -0.5 {
                    y = y + x + 1.0;
                } else {
                    y = y + x;
                }

                if i >> 63 != 0 {
                    -y
                } else {
                    y
                }
            }
        }

        mod roundf
        {
            use ::
            {
                *,
            };
            /*
            */
            const TOINT: f32 = 1.0 / f32::EPSILON;

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn roundf(mut x: f32) -> f32 {
                let i = x.to_bits();
                let e: u32 = i >> 23 & 0xff;
                let mut y: f32;

                if e >= 0x7f + 23 {
                    return x;
                }
                if e < 0x7f - 1 {
                    force_eval!(x + TOINT);
                    return 0.0 * x;
                }
                if i >> 31 != 0 {
                    x = -x;
                }
                y = x + TOINT - TOINT - x;
                if y > 0.5f32 {
                    y = y + x - 1.0;
                } else if y <= -0.5f32 {
                    y = y + x + 1.0;
                } else {
                    y = y + x;
                }
                if i >> 31 != 0 {
                    -y
                } else {
                    y
                }
            }
        }

        mod scalbn
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn scalbn(x: f64, mut n: i32) -> f64
            {
                let x1p1023 = f64::from_bits(0x7fe0000000000000); // 0x1p1023 === 2 ^ 1023
                let x1p53 = f64::from_bits(0x4340000000000000); // 0x1p53 === 2 ^ 53
                let x1p_1022 = f64::from_bits(0x0010000000000000); // 0x1p-1022 === 2 ^ (-1022)

                let mut y = x;

                if n > 1023 {
                    y *= x1p1023;
                    n -= 1023;
                    if n > 1023 {
                        y *= x1p1023;
                        n -= 1023;
                        if n > 1023 {
                            n = 1023;
                        }
                    }
                } else if n < -1022 {
                    /* make sure final n < -53 to avoid double
                    rounding in the subnormal range */
                    y *= x1p_1022 * x1p53;
                    n += 1022 - 53;
                    if n < -1022 {
                        y *= x1p_1022 * x1p53;
                        n += 1022 - 53;
                        if n < -1022 {
                            n = -1022;
                        }
                    }
                }
                y * f64::from_bits(((0x3ff + n) as u64) << 52)
            }
        }

        mod scalbnf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn scalbnf(mut x: f32, mut n: i32) -> f32
            {
                let x1p127 = f32::from_bits(0x7f000000); // 0x1p127f === 2 ^ 127
                let x1p_126 = f32::from_bits(0x800000); // 0x1p-126f === 2 ^ -126
                let x1p24 = f32::from_bits(0x4b800000); // 0x1p24f === 2 ^ 24

                if n > 127 {
                    x *= x1p127;
                    n -= 127;
                    if n > 127 {
                        x *= x1p127;
                        n -= 127;
                        if n > 127 {
                            n = 127;
                        }
                    }
                } else if n < -126 {
                    x *= x1p_126 * x1p24;
                    n += 126 - 24;
                    if n < -126 {
                        x *= x1p_126 * x1p24;
                        n += 126 - 24;
                        if n < -126 {
                            n = -126;
                        }
                    }
                }
                x * f32::from_bits(((0x7f + n) as u32) << 23)
            }
        }

        mod sin
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{k_cos, k_sin, rem_pio2};

            /// Return sine function of x.
            pub fn sin(x: f64) -> f64
            {
                let x1p120 = f64::from_bits(0x4770000000000000); // 0x1p120f === 2 ^ 120

                /* High word of x. */
                let ix = (f64::to_bits(x) >> 32) as u32 & 0x7fffffff;

                /* |x| ~< pi/4 */
                if ix <= 0x3fe921fb {
                    if ix < 0x3e500000 {
                        /* |x| < 2**-26 */
                        /* raise inexact if x != 0 and underflow if subnormal*/
                        if ix < 0x00100000 {
                            force_eval!(x / x1p120);
                        } else {
                            force_eval!(x + x1p120);
                        }
                        return x;
                    }
                    return k_sin(x, 0.0, 0);
                }

                /* sin(Inf or NaN) is NaN */
                if ix >= 0x7ff00000 {
                    return x - x;
                }

                /* argument reduction needed */
                let (n, y0, y1) = rem_pio2(x);
                match n & 3 {
                    0 => k_sin(y0, y1, 1),
                    1 => k_cos(y0, y1),
                    2 => -k_sin(y0, y1, 1),
                    _ => -k_cos(y0, y1),
                }
            }
        }

        mod sincos
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{get_high_word, k_cos, k_sin, rem_pio2};

            pub fn sincos(x: f64) -> (f64, f64)
            {
                let s: f64;
                let c: f64;
                let mut ix: u32;

                ix = get_high_word(x);
                ix &= 0x7fffffff;

                /* |x| ~< pi/4 */
                if ix <= 0x3fe921fb {
                    /* if |x| < 2**-27 * sqrt(2) */
                    if ix < 0x3e46a09e {
                        /* raise inexact if x!=0 and underflow if subnormal */
                        let x1p120 = f64::from_bits(0x4770000000000000); // 0x1p120 == 2^120
                        if ix < 0x00100000 {
                            force_eval!(x / x1p120);
                        } else {
                            force_eval!(x + x1p120);
                        }
                        return (x, 1.0);
                    }
                    return (k_sin(x, 0.0, 0), k_cos(x, 0.0));
                }

                /* sincos(Inf or NaN) is NaN */
                if ix >= 0x7ff00000 {
                    let rv = x - x;
                    return (rv, rv);
                }

                /* argument reduction needed */
                let (n, y0, y1) = rem_pio2(x);
                s = k_sin(y0, y1, 1);
                c = k_cos(y0, y1);
                match n & 3 {
                    0 => (s, c),
                    1 => (c, -s),
                    2 => (-s, -c),
                    3 => (-c, s),
                    #[cfg(debug_assertions)]
                    _ => unreachable!(),
                    #[cfg(not(debug_assertions))]
                    _ => (0.0, 1.0),
                }
            }
        }

        mod sincosf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{k_cosf, k_sinf, rem_pio2f};

            /* Small multiples of pi/2 rounded to double precision. */
            const PI_2: f32 = 0.5 * 3.1415926535897931160E+00;
            const S1PIO2: f32 = 1.0 * PI_2; /* 0x3FF921FB, 0x54442D18 */
            const S2PIO2: f32 = 2.0 * PI_2; /* 0x400921FB, 0x54442D18 */
            const S3PIO2: f32 = 3.0 * PI_2; /* 0x4012D97C, 0x7F3321D2 */
            const S4PIO2: f32 = 4.0 * PI_2; /* 0x401921FB, 0x54442D18 */

            pub fn sincosf(x: f32) -> (f32, f32) {
                let s: f32;
                let c: f32;
                let mut ix: u32;
                let sign: bool;

                ix = x.to_bits();
                sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;

                /* |x| ~<= pi/4 */
                if ix <= 0x3f490fda {
                    /* |x| < 2**-12 */
                    if ix < 0x39800000 {
                        /* raise inexact if x!=0 and underflow if subnormal */

                        let x1p120 = f32::from_bits(0x7b800000); // 0x1p120 == 2^120
                        if ix < 0x00100000 {
                            force_eval!(x / x1p120);
                        } else {
                            force_eval!(x + x1p120);
                        }
                        return (x, 1.0);
                    }
                    return (k_sinf(x as f64), k_cosf(x as f64));
                }

                /* |x| ~<= 5*pi/4 */
                if ix <= 0x407b53d1 {
                    if ix <= 0x4016cbe3 {
                        /* |x| ~<= 3pi/4 */
                        if sign {
                            s = -k_cosf((x + S1PIO2) as f64);
                            c = k_sinf((x + S1PIO2) as f64);
                        } else {
                            s = k_cosf((S1PIO2 - x) as f64);
                            c = k_sinf((S1PIO2 - x) as f64);
                        }
                    }
                    /* -sin(x+c) is not correct if x+c could be 0: -0 vs +0 */
                    else {
                        if sign {
                            s = -k_sinf((x + S2PIO2) as f64);
                            c = -k_cosf((x + S2PIO2) as f64);
                        } else {
                            s = -k_sinf((x - S2PIO2) as f64);
                            c = -k_cosf((x - S2PIO2) as f64);
                        }
                    }

                    return (s, c);
                }

                /* |x| ~<= 9*pi/4 */
                if ix <= 0x40e231d5 {
                    if ix <= 0x40afeddf {
                        /* |x| ~<= 7*pi/4 */
                        if sign {
                            s = k_cosf((x + S3PIO2) as f64);
                            c = -k_sinf((x + S3PIO2) as f64);
                        } else {
                            s = -k_cosf((x - S3PIO2) as f64);
                            c = k_sinf((x - S3PIO2) as f64);
                        }
                    } else {
                        if sign {
                            s = k_cosf((x + S4PIO2) as f64);
                            c = k_sinf((x + S4PIO2) as f64);
                        } else {
                            s = k_cosf((x - S4PIO2) as f64);
                            c = k_sinf((x - S4PIO2) as f64);
                        }
                    }

                    return (s, c);
                }

                /* sin(Inf or NaN) is NaN */
                if ix >= 0x7f800000 {
                    let rv = x - x;
                    return (rv, rv);
                }

                /* general argument reduction needed */
                let (n, y) = rem_pio2f(x);
                s = k_sinf(y);
                c = k_cosf(y);
                match n & 3 {
                    0 => (s, c),
                    1 => (c, -s),
                    2 => (-s, -c),
                    3 => (-c, s),
                    #[cfg(debug_assertions)]
                    _ => unreachable!(),
                    #[cfg(not(debug_assertions))]
                    _ => (0.0, 1.0),
                }
            }
        }

        mod sinf
        {
            use ::
            {
                f64::consts::FRAC_PI_2,
                *,
            };
            /*
            */
            use super::{k_cosf, k_sinf, rem_pio2f};

            /* Small multiples of pi/2 rounded to double precision. */
            const S1_PIO2: f64 = 1. * FRAC_PI_2; /* 0x3FF921FB, 0x54442D18 */
            const S2_PIO2: f64 = 2. * FRAC_PI_2; /* 0x400921FB, 0x54442D18 */
            const S3_PIO2: f64 = 3. * FRAC_PI_2; /* 0x4012D97C, 0x7F3321D2 */
            const S4_PIO2: f64 = 4. * FRAC_PI_2; /* 0x401921FB, 0x54442D18 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn sinf(x: f32) -> f32 {
                let x64 = x as f64;

                let x1p120 = f32::from_bits(0x7b800000); // 0x1p120f === 2 ^ 120

                let mut ix = x.to_bits();
                let sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;

                if ix <= 0x3f490fda {
                    /* |x| ~<= pi/4 */
                    if ix < 0x39800000 {
                        /* |x| < 2**-12 */
                        /* raise inexact if x!=0 and underflow if subnormal */
                        force_eval!(if ix < 0x00800000 {
                            x / x1p120
                        } else {
                            x + x1p120
                        });
                        return x;
                    }
                    return k_sinf(x64);
                }
                if ix <= 0x407b53d1 {
                    /* |x| ~<= 5*pi/4 */
                    if ix <= 0x4016cbe3 {
                        /* |x| ~<= 3pi/4 */
                        if sign {
                            return -k_cosf(x64 + S1_PIO2);
                        } else {
                            return k_cosf(x64 - S1_PIO2);
                        }
                    }
                    return k_sinf(if sign {
                        -(x64 + S2_PIO2)
                    } else {
                        -(x64 - S2_PIO2)
                    });
                }
                if ix <= 0x40e231d5 {
                    /* |x| ~<= 9*pi/4 */
                    if ix <= 0x40afeddf {
                        /* |x| ~<= 7*pi/4 */
                        if sign {
                            return k_cosf(x64 + S3_PIO2);
                        } else {
                            return -k_cosf(x64 - S3_PIO2);
                        }
                    }
                    return k_sinf(if sign { x64 + S4_PIO2 } else { x64 - S4_PIO2 });
                }

                /* sin(Inf or NaN) is NaN */
                if ix >= 0x7f800000 {
                    return x - x;
                }

                /* general argument reduction needed */
                let (n, y) = rem_pio2f(x);
                match n & 3 {
                    0 => k_sinf(y),
                    1 => k_cosf(y),
                    2 => k_sinf(-y),
                    _ => -k_cosf(y),
                }
            }
        }

        mod sinh
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{expm1, expo2};
            pub fn sinh(x: f64) -> f64
            {
                // union {double f; uint64_t i;} u = {.f = x};
                // uint32_t w;
                // double t, h, absx;

                let mut uf: f64 = x;
                let mut ui: u64 = f64::to_bits(uf);
                let w: u32;
                let t: f64;
                let mut h: f64;
                let absx: f64;

                h = 0.5;
                if ui >> 63 != 0 {
                    h = -h;
                }
                /* |x| */
                ui &= !1 / 2;
                uf = f64::from_bits(ui);
                absx = uf;
                w = (ui >> 32) as u32;

                /* |x| < log(DBL_MAX) */
                if w < 0x40862e42 {
                    t = expm1(absx);
                    if w < 0x3ff00000 {
                        if w < 0x3ff00000 - (26 << 20) {
                            /* note: inexact and underflow are raised by expm1 */
                            /* note: this branch avoids spurious underflow */
                            return x;
                        }
                        return h * (2.0 * t - t * t / (t + 1.0));
                    }
                    /* note: |x|>log(0x1p26)+eps could be just h*exp(x) */
                    return h * (t + t / (t + 1.0));
                }

                /* |x| > log(DBL_MAX) or nan */
                /* note: the result is stored to handle overflow */
                t = 2.0 * h * expo2(absx);
                t
            }
        }

        mod sinhf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::expm1f;
            use super::k_expo2f;

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn sinhf(x: f32) -> f32 {
                let mut h = 0.5f32;
                let mut ix = x.to_bits();
                if (ix >> 31) != 0 {
                    h = -h;
                }
                /* |x| */
                ix &= 0x7fffffff;
                let absx = f32::from_bits(ix);
                let w = ix;

                /* |x| < log(FLT_MAX) */
                if w < 0x42b17217 {
                    let t = expm1f(absx);
                    if w < 0x3f800000 {
                        if w < (0x3f800000 - (12 << 23)) {
                            return x;
                        }
                        return h * (2. * t - t * t / (t + 1.));
                    }
                    return h * (t + t / (t + 1.));
                }

                /* |x| > logf(FLT_MAX) or nan */
                2. * h * k_expo2f(absx)
            }
        }

        mod sqrt
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn sqrt(x: f64) -> f64
            {
                #[cfg(target_feature = "sse2")]
                {
                    // Note: This path is unlikely since LLVM will usually have already
                    // optimized sqrt calls into hardware instructions if sse2 is available,
                    // but if someone does end up here they'll apprected the speed increase.
                    #[cfg(target_arch = "x86")]
                    use ::arch::x86::*;
                    #[cfg(target_arch = "x86_64")]
                    use ::arch::x86_64::*;
                    unsafe {
                        let m = _mm_set_sd(x);
                        let m_sqrt = _mm_sqrt_pd(m);
                        _mm_cvtsd_f64(m_sqrt)
                    }
                }
                #[cfg(not(target_feature = "sse2"))]
                {
                    use ::num::Wrapping;

                    const TINY: f64 = 1.0e-300;

                    let mut z: f64;
                    let sign: Wrapping<u32> = Wrapping(0x80000000);
                    let mut ix0: i32;
                    let mut s0: i32;
                    let mut q: i32;
                    let mut m: i32;
                    let mut t: i32;
                    let mut i: i32;
                    let mut r: Wrapping<u32>;
                    let mut t1: Wrapping<u32>;
                    let mut s1: Wrapping<u32>;
                    let mut ix1: Wrapping<u32>;
                    let mut q1: Wrapping<u32>;

                    ix0 = (x.to_bits() >> 32) as i32;
                    ix1 = Wrapping(x.to_bits() as u32);

                    /* take care of Inf and NaN */
                    if (ix0 & 0x7ff00000) == 0x7ff00000 {
                        return x * x + x; /* sqrt(NaN)=NaN, sqrt(+inf)=+inf, sqrt(-inf)=sNaN */
                    }
                    /* take care of zero */
                    if ix0 <= 0 {
                        if ((ix0 & !(sign.0 as i32)) | ix1.0 as i32) == 0 {
                            return x; /* sqrt(+-0) = +-0 */
                        }
                        if ix0 < 0 {
                            return (x - x) / (x - x); /* sqrt(-ve) = sNaN */
                        }
                    }
                    /* normalize x */
                    m = ix0 >> 20;
                    if m == 0 {
                        /* subnormal x */
                        while ix0 == 0 {
                            m -= 21;
                            ix0 |= (ix1 >> 11).0 as i32;
                            ix1 <<= 21;
                        }
                        i = 0;
                        while (ix0 & 0x00100000) == 0 {
                            i += 1;
                            ix0 <<= 1;
                        }
                        m -= i - 1;
                        ix0 |= (ix1 >> (32 - i) as usize).0 as i32;
                        ix1 = ix1 << i as usize;
                    }
                    m -= 1023; /* unbias exponent */
                    ix0 = (ix0 & 0x000fffff) | 0x00100000;
                    if (m & 1) == 1 {
                        /* odd m, double x to make it even */
                        ix0 += ix0 + ((ix1 & sign) >> 31).0 as i32;
                        ix1 += ix1;
                    }
                    m >>= 1; /* m = [m/2] */

                    /* generate sqrt(x) bit by bit */
                    ix0 += ix0 + ((ix1 & sign) >> 31).0 as i32;
                    ix1 += ix1;
                    q = 0; /* [q,q1] = sqrt(x) */
                    q1 = Wrapping(0);
                    s0 = 0;
                    s1 = Wrapping(0);
                    r = Wrapping(0x00200000); /* r = moving bit from right to left */

                    while r != Wrapping(0) {
                        t = s0 + r.0 as i32;
                        if t <= ix0 {
                            s0 = t + r.0 as i32;
                            ix0 -= t;
                            q += r.0 as i32;
                        }
                        ix0 += ix0 + ((ix1 & sign) >> 31).0 as i32;
                        ix1 += ix1;
                        r >>= 1;
                    }

                    r = sign;
                    while r != Wrapping(0) {
                        t1 = s1 + r;
                        t = s0;
                        if t < ix0 || (t == ix0 && t1 <= ix1) {
                            s1 = t1 + r;
                            if (t1 & sign) == sign && (s1 & sign) == Wrapping(0) {
                                s0 += 1;
                            }
                            ix0 -= t;
                            if ix1 < t1 {
                                ix0 -= 1;
                            }
                            ix1 -= t1;
                            q1 += r;
                        }
                        ix0 += ix0 + ((ix1 & sign) >> 31).0 as i32;
                        ix1 += ix1;
                        r >>= 1;
                    }

                    /* use floating add to find out rounding direction */
                    if (ix0 as u32 | ix1.0) != 0 {
                        z = 1.0 - TINY; /* raise inexact flag */
                        if z >= 1.0 {
                            z = 1.0 + TINY;
                            if q1.0 == 0xffffffff {
                                q1 = Wrapping(0);
                                q += 1;
                            } else if z > 1.0 {
                                if q1.0 == 0xfffffffe {
                                    q += 1;
                                }
                                q1 += Wrapping(2);
                            } else {
                                q1 += q1 & Wrapping(1);
                            }
                        }
                    }
                    ix0 = (q >> 1) + 0x3fe00000;
                    ix1 = q1 >> 1;
                    if (q & 1) == 1 {
                        ix1 |= sign;
                    }
                    ix0 += m << 20;
                    f64::from_bits((ix0 as u64) << 32 | ix1.0 as u64)
                }
            }

        }

        mod sqrtf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn sqrtf(x: f32) -> f32
            {
                #[cfg(target_feature = "sse")]
                {
                    // Note: This path is unlikely since LLVM will usually have already
                    // optimized sqrt calls into hardware instructions if sse is available,
                    // but if someone does end up here they'll apprected the speed increase.
                    #[cfg(target_arch = "x86")]
                    use ::arch::x86::*;
                    #[cfg(target_arch = "x86_64")]
                    use ::arch::x86_64::*;
                    unsafe {
                        let m = _mm_set_ss(x);
                        let m_sqrt = _mm_sqrt_ss(m);
                        _mm_cvtss_f32(m_sqrt)
                    }
                }
                #[cfg(not(target_feature = "sse"))]
                {
                    const TINY: f32 = 1.0e-30;

                    let mut z: f32;
                    let sign: i32 = 0x80000000u32 as i32;
                    let mut ix: i32;
                    let mut s: i32;
                    let mut q: i32;
                    let mut m: i32;
                    let mut t: i32;
                    let mut i: i32;
                    let mut r: u32;

                    ix = x.to_bits() as i32;

                    /* take care of Inf and NaN */
                    if (ix as u32 & 0x7f800000) == 0x7f800000 {
                        return x * x + x; /* sqrt(NaN)=NaN, sqrt(+inf)=+inf, sqrt(-inf)=sNaN */
                    }

                    /* take care of zero */
                    if ix <= 0 {
                        if (ix & !sign) == 0 {
                            return x; /* sqrt(+-0) = +-0 */
                        }
                        if ix < 0 {
                            return (x - x) / (x - x); /* sqrt(-ve) = sNaN */
                        }
                    }

                    /* normalize x */
                    m = ix >> 23;
                    if m == 0 {
                        /* subnormal x */
                        i = 0;
                        while ix & 0x00800000 == 0 {
                            ix <<= 1;
                            i = i + 1;
                        }
                        m -= i - 1;
                    }
                    m -= 127; /* unbias exponent */
                    ix = (ix & 0x007fffff) | 0x00800000;
                    if m & 1 == 1 {
                        /* odd m, double x to make it even */
                        ix += ix;
                    }
                    m >>= 1; /* m = [m/2] */

                    /* generate sqrt(x) bit by bit */
                    ix += ix;
                    q = 0;
                    s = 0;
                    r = 0x01000000; /* r = moving bit from right to left */

                    while r != 0 {
                        t = s + r as i32;
                        if t <= ix {
                            s = t + r as i32;
                            ix -= t;
                            q += r as i32;
                        }
                        ix += ix;
                        r >>= 1;
                    }

                    /* use floating add to find out rounding direction */
                    if ix != 0 {
                        z = 1.0 - TINY; /* raise inexact flag */
                        if z >= 1.0 {
                            z = 1.0 + TINY;
                            if z > 1.0 {
                                q += 2;
                            } else {
                                q += q & 1;
                            }
                        }
                    }

                    ix = (q >> 1) + 0x3f000000;
                    ix += m << 23;
                    f32::from_bits(ix as u32)
                }
            }
        }

        mod tan
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{k_tan, rem_pio2};
            // tan(x)
            // Return tangent function of x.
            //
            // kernel function:
            //      k_tan           ... tangent function on [-pi/4,pi/4]
            //      rem_pio2        ... argument reduction routine
            //
            // Method.
            //      Let S,C and T denote the sin, cos and tan respectively on
            //      [-PI/4, +PI/4]. Reduce the argument x to y1+y2 = x-k*pi/2
            //      in [-pi/4 , +pi/4], and let n = k mod 4.
            //      We have
            //
            //          n        sin(x)      cos(x)        tan(x)
            //     ----------------------------------------------------------
            //          0          S           C             T
            //          1          C          -S            -1/T
            //          2         -S          -C             T
            //          3         -C           S            -1/T
            //     ----------------------------------------------------------
            //
            // Special cases:
            //      Let trig be any of sin, cos, or tan.
            //      trig(+-INF)  is NaN, with signals;
            //      trig(NaN)    is that NaN;
            //
            // Accuracy:
            //      TRIG(x) returns trig(x) nearly rounded
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn tan(x: f64) -> f64 {
                let x1p120 = f32::from_bits(0x7b800000); // 0x1p120f === 2 ^ 120

                let ix = (f64::to_bits(x) >> 32) as u32 & 0x7fffffff;
                /* |x| ~< pi/4 */
                if ix <= 0x3fe921fb {
                    if ix < 0x3e400000 {
                        /* |x| < 2**-27 */
                        /* raise inexact if x!=0 and underflow if subnormal */
                        force_eval!(if ix < 0x00100000 {
                            x / x1p120 as f64
                        } else {
                            x + x1p120 as f64
                        });
                        return x;
                    }
                    return k_tan(x, 0.0, 0);
                }

                /* tan(Inf or NaN) is NaN */
                if ix >= 0x7ff00000 {
                    return x - x;
                }

                /* argument reduction */
                let (n, y0, y1) = rem_pio2(x);
                k_tan(y0, y1, n & 1)
            }
        }

        mod tanf
        {
            use ::
            {
                f64::consts::FRAC_PI_2,
                *,
            };

            use super::{k_tanf, rem_pio2f};
            /*
            */
            /* Small multiples of pi/2 rounded to double precision. */
            const T1_PIO2: f64 = 1. * FRAC_PI_2; /* 0x3FF921FB, 0x54442D18 */
            const T2_PIO2: f64 = 2. * FRAC_PI_2; /* 0x400921FB, 0x54442D18 */
            const T3_PIO2: f64 = 3. * FRAC_PI_2; /* 0x4012D97C, 0x7F3321D2 */
            const T4_PIO2: f64 = 4. * FRAC_PI_2; /* 0x401921FB, 0x54442D18 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn tanf(x: f32) -> f32 {
                let x64 = x as f64;

                let x1p120 = f32::from_bits(0x7b800000); // 0x1p120f === 2 ^ 120

                let mut ix = x.to_bits();
                let sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;

                if ix <= 0x3f490fda {
                    /* |x| ~<= pi/4 */
                    if ix < 0x39800000 {
                        /* |x| < 2**-12 */
                        /* raise inexact if x!=0 and underflow if subnormal */
                        force_eval!(if ix < 0x00800000 {
                            x / x1p120
                        } else {
                            x + x1p120
                        });
                        return x;
                    }
                    return k_tanf(x64, false);
                }
                if ix <= 0x407b53d1 {
                    /* |x| ~<= 5*pi/4 */
                    if ix <= 0x4016cbe3 {
                        /* |x| ~<= 3pi/4 */
                        return k_tanf(if sign { x64 + T1_PIO2 } else { x64 - T1_PIO2 }, true);
                    } else {
                        return k_tanf(if sign { x64 + T2_PIO2 } else { x64 - T2_PIO2 }, false);
                    }
                }
                if ix <= 0x40e231d5 {
                    /* |x| ~<= 9*pi/4 */
                    if ix <= 0x40afeddf {
                        /* |x| ~<= 7*pi/4 */
                        return k_tanf(if sign { x64 + T3_PIO2 } else { x64 - T3_PIO2 }, true);
                    } else {
                        return k_tanf(if sign { x64 + T4_PIO2 } else { x64 - T4_PIO2 }, false);
                    }
                }

                /* tan(Inf or NaN) is NaN */
                if ix >= 0x7f800000 {
                    return x - x;
                }

                /* argument reduction */
                let (n, y) = rem_pio2f(x);
                k_tanf(y, n & 1 != 0)
            }
        }

        mod tanh
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::expm1;

            /* tanh(x) = (exp(x) - exp(-x))/(exp(x) + exp(-x))
            *         = (exp(2*x) - 1)/(exp(2*x) - 1 + 2)
            *         = (1 - exp(-2*x))/(exp(-2*x) - 1 + 2)
            */
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn tanh(mut x: f64) -> f64 {
                let mut uf: f64 = x;
                let mut ui: u64 = f64::to_bits(uf);

                let w: u32;
                let sign: bool;
                let mut t: f64;

                /* x = |x| */
                sign = ui >> 63 != 0;
                ui &= !1 / 2;
                uf = f64::from_bits(ui);
                x = uf;
                w = (ui >> 32) as u32;

                if w > 0x3fe193ea {
                    /* |x| > log(3)/2 ~= 0.5493 or nan */
                    if w > 0x40340000 {
                        /* |x| > 20 or nan */
                        /* note: this branch avoids raising overflow */
                        t = 1.0 - 0.0 / x;
                    } else {
                        t = expm1(2.0 * x);
                        t = 1.0 - 2.0 / (t + 2.0);
                    }
                } else if w > 0x3fd058ae {
                    /* |x| > log(5/3)/2 ~= 0.2554 */
                    t = expm1(2.0 * x);
                    t = t / (t + 2.0);
                } else if w >= 0x00100000 {
                    /* |x| >= 0x1p-1022, up to 2ulp error in [0.1,0.2554] */
                    t = expm1(-2.0 * x);
                    t = -t / (t + 2.0);
                } else {
                    /* |x| is subnormal */
                    /* note: the branch above would not raise underflow in [0x1p-1023,0x1p-1022) */
                    force_eval!(x as f32);
                    t = x;
                }

                if sign {
                    -t
                } else {
                    t
                }
            }
        }

        mod tanhf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::expm1f;

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub fn tanhf(mut x: f32) -> f32 {
                /* x = |x| */
                let mut ix = x.to_bits();
                let sign = (ix >> 31) != 0;
                ix &= 0x7fffffff;
                x = f32::from_bits(ix);
                let w = ix;

                let tt = if w > 0x3f0c9f54 {
                    /* |x| > log(3)/2 ~= 0.5493 or nan */
                    if w > 0x41200000 {
                        /* |x| > 10 */
                        1. + 0. / x
                    } else {
                        let t = expm1f(2. * x);
                        1. - 2. / (t + 2.)
                    }
                } else if w > 0x3e82c578 {
                    /* |x| > log(5/3)/2 ~= 0.2554 */
                    let t = expm1f(2. * x);
                    t / (t + 2.)
                } else if w >= 0x00800000 {
                    /* |x| >= 0x1p-126 */
                    let t = expm1f(-2. * x);
                    -t / (t + 2.)
                } else {
                    /* |x| is subnormal */
                    force_eval!(x * x);
                    x
                };
                if sign {
                    -tt
                } else {
                    tt
                }
            }
        }

        mod tgamma
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{exp, floor, k_cos, k_sin, pow};

            const PI: f64 = 3.141592653589793238462643383279502884;

            /* sin(pi x) with x > 0x1p-100, if sin(pi*x)==0 the sign is arbitrary */
            fn sinpi(mut x: f64) -> f64 {
                let mut n: isize;

                /* argument reduction: x = |x| mod 2 */
                /* spurious inexact when x is odd int */
                x = x * 0.5;
                x = 2.0 * (x - floor(x));

                /* reduce x into [-.25,.25] */
                n = (4.0 * x) as isize;
                n = (n + 1) / 2;
                x -= (n as f64) * 0.5;

                x *= PI;
                match n {
                    1 => k_cos(x, 0.0),
                    2 => k_sin(-x, 0.0, 0),
                    3 => -k_cos(x, 0.0),
                    0 | _ => k_sin(x, 0.0, 0),
                }
            }

            const N: usize = 12;
            //static const double g = 6.024680040776729583740234375;
            const GMHALF: f64 = 5.524680040776729583740234375;
            const SNUM: [f64; N + 1] = [
                23531376880.410759688572007674451636754734846804940,
                42919803642.649098768957899047001988850926355848959,
                35711959237.355668049440185451547166705960488635843,
                17921034426.037209699919755754458931112671403265390,
                6039542586.3520280050642916443072979210699388420708,
                1439720407.3117216736632230727949123939715485786772,
                248874557.86205415651146038641322942321632125127801,
                31426415.585400194380614231628318205362874684987640,
                2876370.6289353724412254090516208496135991145378768,
                186056.26539522349504029498971604569928220784236328,
                8071.6720023658162106380029022722506138218516325024,
                210.82427775157934587250973392071336271166969580291,
                2.5066282746310002701649081771338373386264310793408,
            ];
            const SDEN: [f64; N + 1] = [
                0.0,
                39916800.0,
                120543840.0,
                150917976.0,
                105258076.0,
                45995730.0,
                13339535.0,
                2637558.0,
                357423.0,
                32670.0,
                1925.0,
                66.0,
                1.0,
            ];
            /* n! for small integer n */
            const FACT: [f64; 23] = [
                1.0,
                1.0,
                2.0,
                6.0,
                24.0,
                120.0,
                720.0,
                5040.0,
                40320.0,
                362880.0,
                3628800.0,
                39916800.0,
                479001600.0,
                6227020800.0,
                87178291200.0,
                1307674368000.0,
                20922789888000.0,
                355687428096000.0,
                6402373705728000.0,
                121645100408832000.0,
                2432902008176640000.0,
                51090942171709440000.0,
                1124000727777607680000.0,
            ];

            /* S(x) rational function for positive x */
            fn s(x: f64) -> f64 {
                let mut num: f64 = 0.0;
                let mut den: f64 = 0.0;

                /* to avoid overflow handle large x differently */
                if x < 8.0 {
                    for i in (0..=N).rev() {
                        num = num * x + SNUM[i];
                        den = den * x + SDEN[i];
                    }
                } else {
                    for i in 0..=N {
                        num = num / x + SNUM[i];
                        den = den / x + SDEN[i];
                    }
                }
                return num / den;
            }

            pub fn tgamma(mut x: f64) -> f64 {
                let u: u64 = x.to_bits();
                let absx: f64;
                let mut y: f64;
                let mut dy: f64;
                let mut z: f64;
                let mut r: f64;
                let ix: u32 = ((u >> 32) as u32) & 0x7fffffff;
                let sign: bool = (u >> 63) != 0;

                /* special cases */
                if ix >= 0x7ff00000 {
                    /* tgamma(nan)=nan, tgamma(inf)=inf, tgamma(-inf)=nan with invalid */
                    return x + ::f64::INFINITY;
                }
                if ix < ((0x3ff - 54) << 20) {
                    /* |x| < 2^-54: tgamma(x) ~ 1/x, +-0 raises div-by-zero */
                    return 1.0 / x;
                }

                /* integer arguments */
                /* raise inexact when non-integer */
                if x == floor(x) {
                    if sign {
                        return 0.0 / 0.0;
                    }
                    if x <= FACT.len() as f64 {
                        return FACT[(x as usize) - 1];
                    }
                }

                /* x >= 172: tgamma(x)=inf with overflow */
                /* x =< -184: tgamma(x)=+-0 with underflow */
                if ix >= 0x40670000 {
                    /* |x| >= 184 */
                    if sign {
                        let x1p_126 = f64::from_bits(0x3810000000000000); // 0x1p-126 == 2^-126
                        force_eval!((x1p_126 / x) as f32);
                        if floor(x) * 0.5 == floor(x * 0.5) {
                            return 0.0;
                        } else {
                            return -0.0;
                        }
                    }
                    let x1p1023 = f64::from_bits(0x7fe0000000000000); // 0x1p1023 == 2^1023
                    x *= x1p1023;
                    return x;
                }

                absx = if sign { -x } else { x };

                /* handle the error of x + g - 0.5 */
                y = absx + GMHALF;
                if absx > GMHALF {
                    dy = y - absx;
                    dy -= GMHALF;
                } else {
                    dy = y - GMHALF;
                    dy -= absx;
                }

                z = absx - 0.5;
                r = s(absx) * exp(-y);
                if x < 0.0 {
                    /* reflection formula for negative x */
                    /* sinpi(absx) is not 0, integers are already handled */
                    r = -PI / (sinpi(absx) * absx * r);
                    dy = -dy;
                    z = -z;
                }
                r += dy * (GMHALF + 0.5) * r / y;
                z = pow(y, 0.5 * z);
                y = r * z * z;
                return y;
            }
        }

        mod tgammaf
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::tgamma;

            pub fn tgammaf(x: f32) -> f32 {
                tgamma(x as f64) as f32
            }
        }

        mod trunc
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn trunc(x: f64) -> f64 
            {
                let x1p120 = f64::from_bits(0x4770000000000000); // 0x1p120f === 2 ^ 120

                let mut i: u64 = x.to_bits();
                let mut e: i64 = (i >> 52 & 0x7ff) as i64 - 0x3ff + 12;
                let m: u64;

                if e >= 52 + 12 {
                    return x;
                }
                if e < 12 {
                    e = 1;
                }
                m = -1i64 as u64 >> e;
                if (i & m) == 0 {
                    return x;
                }
                force_eval!(x + x1p120);
                i &= !m;
                f64::from_bits(i)
            }
        }

        mod truncf
        {
            use ::
            {
                *,
            };
            /*
            */
            pub fn truncf(x: f32) -> f32
            {
                let x1p120 = f32::from_bits(0x7b800000); // 0x1p120f === 2 ^ 120

                let mut i: u32 = x.to_bits();
                let mut e: i32 = (i >> 23 & 0xff) as i32 - 0x7f + 9;
                let m: u32;

                if e >= 23 + 9 {
                    return x;
                }
                if e < 9 {
                    e = 1;
                }
                m = -1i32 as u32 >> e;
                if (i & m) == 0 {
                    return x;
                }
                force_eval!(x + x1p120);
                i &= !m;
                f32::from_bits(i)
            }

        }


        // Use separated imports instead of {}-grouped imports for easier merging.
        pub use self::acos::acos;
        pub use self::acosf::acosf;
        pub use self::acosh::acosh;
        pub use self::acoshf::acoshf;
        pub use self::asin::asin;
        pub use self::asinf::asinf;
        pub use self::asinh::asinh;
        pub use self::asinhf::asinhf;
        pub use self::atan::atan;
        pub use self::atan2::atan2;
        pub use self::atan2f::atan2f;
        pub use self::atanf::atanf;
        pub use self::atanh::atanh;
        pub use self::atanhf::atanhf;
        pub use self::cbrt::cbrt;
        pub use self::cbrtf::cbrtf;
        pub use self::ceil::ceil;
        pub use self::ceilf::ceilf;
        pub use self::copysign::copysign;
        pub use self::copysignf::copysignf;
        pub use self::cos::cos;
        pub use self::cosf::cosf;
        pub use self::cosh::cosh;
        pub use self::coshf::coshf;
        pub use self::erf::erf;
        pub use self::erf::erfc;
        pub use self::erff::erfcf;
        pub use self::erff::erff;
        pub use self::exp::exp;
        pub use self::exp10::exp10;
        pub use self::exp10f::exp10f;
        pub use self::exp2::exp2;
        pub use self::exp2f::exp2f;
        pub use self::expf::expf;
        pub use self::expm1::expm1;
        pub use self::expm1f::expm1f;
        pub use self::fabs::fabs;
        pub use self::fabsf::fabsf;
        pub use self::fdim::fdim;
        pub use self::fdimf::fdimf;
        pub use self::floor::floor;
        pub use self::floorf::floorf;
        pub use self::fma::fma;
        pub use self::fmaf::fmaf;
        pub use self::fmax::fmax;
        pub use self::fmaxf::fmaxf;
        pub use self::fmin::fmin;
        pub use self::fminf::fminf;
        pub use self::fmod::fmod;
        pub use self::fmodf::fmodf;
        pub use self::frexp::frexp;
        pub use self::frexpf::frexpf;
        pub use self::hypot::hypot;
        pub use self::hypotf::hypotf;
        pub use self::ilogb::ilogb;
        pub use self::ilogbf::ilogbf;
        pub use self::j0::j0;
        pub use self::j0::y0;
        pub use self::j0f::j0f;
        pub use self::j0f::y0f;
        pub use self::j1::j1;
        pub use self::j1::y1;
        pub use self::j1f::j1f;
        pub use self::j1f::y1f;
        pub use self::jn::jn;
        pub use self::jn::yn;
        pub use self::jnf::jnf;
        pub use self::jnf::ynf;
        pub use self::ldexp::ldexp;
        pub use self::ldexpf::ldexpf;
        pub use self::lgamma::lgamma;
        pub use self::lgamma_r::lgamma_r;
        pub use self::lgammaf::lgammaf;
        pub use self::lgammaf_r::lgammaf_r;
        pub use self::log::log;
        pub use self::log10::log10;
        pub use self::log10f::log10f;
        pub use self::log1p::log1p;
        pub use self::log1pf::log1pf;
        pub use self::log2::log2;
        pub use self::log2f::log2f;
        pub use self::logf::logf;
        pub use self::modf::modf;
        pub use self::modff::modff;
        pub use self::nextafter::nextafter;
        pub use self::nextafterf::nextafterf;
        pub use self::pow::pow;
        pub use self::powf::powf;
        pub use self::remainder::remainder;
        pub use self::remainderf::remainderf;
        pub use self::remquo::remquo;
        pub use self::remquof::remquof;
        pub use self::round::round;
        pub use self::roundf::roundf;
        pub use self::scalbn::scalbn;
        pub use self::scalbnf::scalbnf;
        pub use self::sin::sin;
        pub use self::sincos::sincos;
        pub use self::sincosf::sincosf;
        pub use self::sinf::sinf;
        pub use self::sinh::sinh;
        pub use self::sinhf::sinhf;
        pub use self::sqrt::sqrt;
        pub use self::sqrtf::sqrtf;
        pub use self::tan::tan;
        pub use self::tanf::tanf;
        pub use self::tanh::tanh;
        pub use self::tanhf::tanhf;
        pub use self::tgamma::tgamma;
        pub use self::tgammaf::tgammaf;
        pub use self::trunc::trunc;
        pub use self::truncf::truncf;

        // Private modules
        mod expo2
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::{combine_words, exp};

            /* exp(x)/2 for x >= log(DBL_MAX), slightly better than 0.5*exp(x/2)*exp(x/2) */
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn expo2(x: f64) -> f64 {
                /* k is such that k*ln2 has minimal relative error and x - kln2 > log(DBL_MIN) */
                const K: i32 = 2043;
                let kln2 = f64::from_bits(0x40962066151add8b);

                /* note that k is odd and scale*scale overflows */
                let scale = combine_words(((0x3ff + K / 2) as u32) << 20, 0);
                /* exp(x - k ln2) * 2**(k-1) */
                exp(x - kln2) * scale * scale
            }
        }

        mod fenv
        {
            use ::
            {
                *,
            };
            /*
            */
            pub(crate) const FE_UNDERFLOW: i32 = 0;
            pub(crate) const FE_INEXACT: i32 = 0;

            pub(crate) const FE_TONEAREST: i32 = 0;
            pub(crate) const FE_TOWARDZERO: i32 = 0;

            #[inline]
            pub(crate) fn feclearexcept(_mask: i32) -> i32 {
                0
            }

            #[inline]
            pub(crate) fn feraiseexcept(_mask: i32) -> i32 {
                0
            }

            #[inline]
            pub(crate) fn fetestexcept(_mask: i32) -> i32 {
                0
            }

            #[inline]
            pub(crate) fn fegetround() -> i32 {
                FE_TONEAREST
            }

            #[inline]
            pub(crate) fn fesetround(_r: i32) -> i32 {
                0
            }
        }
        
        mod k_cos
        {
            use ::
            {
                *,
            };
            /*
            */
            const C1: f64 = 4.16666666666666019037e-02; /* 0x3FA55555, 0x5555554C */
            const C2: f64 = -1.38888888888741095749e-03; /* 0xBF56C16C, 0x16C15177 */
            const C3: f64 = 2.48015872894767294178e-05; /* 0x3EFA01A0, 0x19CB1590 */
            const C4: f64 = -2.75573143513906633035e-07; /* 0xBE927E4F, 0x809C52AD */
            const C5: f64 = 2.08757232129817482790e-09; /* 0x3E21EE9E, 0xBDB4B1C4 */
            const C6: f64 = -1.13596475577881948265e-11; /* 0xBDA8FAE9, 0xBE8838D4 */

            // kernel cos function on [-pi/4, pi/4], pi/4 ~ 0.785398164
            // Input x is assumed to be bounded by ~pi/4 in magnitude.
            // Input y is the tail of x.
            //
            // Algorithm
            //      1. Since cos(-x) = cos(x), we need only to consider positive x.
            //      2. if x < 2^-27 (hx<0x3e400000 0), return 1 with inexact if x!=0.
            //      3. cos(x) is approximated by a polynomial of degree 14 on
            //         [0,pi/4]
            //                                       4            14
            //              cos(x) ~ 1 - x*x/2 + C1*x + ... + C6*x
            //         where the remez error is
            //
            //      |              2     4     6     8     10    12     14 |     -58
            //      |cos(x)-(1-.5*x +C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  )| <= 2
            //      |                                                      |
            //
            //                     4     6     8     10    12     14
            //      4. let r = C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  , then
            //             cos(x) ~ 1 - x*x/2 + r
            //         since cos(x+y) ~ cos(x) - sin(x)*y
            //                        ~ cos(x) - x*y,
            //         a correction term is necessary in cos(x) and hence
            //              cos(x+y) = 1 - (x*x/2 - (r - x*y))
            //         For better accuracy, rearrange to
            //              cos(x+y) ~ w + (tmp + (r-x*y))
            //         where w = 1 - x*x/2 and tmp is a tiny correction term
            //         (1 - x*x/2 == w + tmp exactly in infinite precision).
            //         The exactness of w + tmp in infinite precision depends on w
            //         and tmp having the same precision as x.  If they have extra
            //         precision due to compiler bugs, then the extra precision is
            //         only good provided it is retained in all terms of the final
            //         expression for cos().  Retention happens in all cases tested
            //         under FreeBSD, so don't pessimize things by forcibly clipping
            //         any extra precision in w.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn k_cos(x: f64, y: f64) -> f64 {
                let z = x * x;
                let w = z * z;
                let r = z * (C1 + z * (C2 + z * C3)) + w * w * (C4 + z * (C5 + z * C6));
                let hz = 0.5 * z;
                let w = 1.0 - hz;
                w + (((1.0 - w) - hz) + (z * r - x * y))
            }
        }
        
        mod k_cosf
        {
            use ::
            {
                *,
            };
            /*
            */
            const C0: f64 = -0.499999997251031003120; /* -0x1ffffffd0c5e81.0p-54 */
            const C1: f64 = 0.0416666233237390631894; /*  0x155553e1053a42.0p-57 */
            const C2: f64 = -0.00138867637746099294692; /* -0x16c087e80f1e27.0p-62 */
            const C3: f64 = 0.0000243904487962774090654; /*  0x199342e0ee5069.0p-68 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn k_cosf(x: f64) -> f32 {
                let z = x * x;
                let w = z * z;
                let r = C2 + z * C3;
                (((1.0 + z * C0) + w * C1) + (w * z) * r) as f32
            }
        }
        
        mod k_expo2
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::exp;

            /* k is such that k*ln2 has minimal relative error and x - kln2 > log(FLT_MIN) */
            const K: i32 = 2043;

            /* expf(x)/2 for x >= log(FLT_MAX), slightly better than 0.5f*expf(x/2)*expf(x/2) */
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn k_expo2(x: f64) -> f64 {
                let k_ln2 = f64::from_bits(0x40962066151add8b);
                /* note that k is odd and scale*scale overflows */
                let scale = f64::from_bits(((((0x3ff + K / 2) as u32) << 20) as u64) << 32);
                /* exp(x - k ln2) * 2**(k-1) */
                exp(x - k_ln2) * scale * scale
            }
        }
        
        mod k_expo2f
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::expf;

            /* k is such that k*ln2 has minimal relative error and x - kln2 > log(FLT_MIN) */
            const K: i32 = 235;

            /* expf(x)/2 for x >= log(FLT_MAX), slightly better than 0.5f*expf(x/2)*expf(x/2) */
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn k_expo2f(x: f32) -> f32 {
                let k_ln2 = f32::from_bits(0x4322e3bc);
                /* note that k is odd and scale*scale overflows */
                let scale = f32::from_bits(((0x7f + K / 2) as u32) << 23);
                /* exp(x - k ln2) * 2**(k-1) */
                expf(x - k_ln2) * scale * scale
            }
        }
        
        mod k_sin
        {
            use ::
            {
                *,
            };
            /*
            */
            const S1: f64 = -1.66666666666666324348e-01; /* 0xBFC55555, 0x55555549 */
            const S2: f64 = 8.33333333332248946124e-03; /* 0x3F811111, 0x1110F8A6 */
            const S3: f64 = -1.98412698298579493134e-04; /* 0xBF2A01A0, 0x19C161D5 */
            const S4: f64 = 2.75573137070700676789e-06; /* 0x3EC71DE3, 0x57B1FE7D */
            const S5: f64 = -2.50507602534068634195e-08; /* 0xBE5AE5E6, 0x8A2B9CEB */
            const S6: f64 = 1.58969099521155010221e-10; /* 0x3DE5D93A, 0x5ACFD57C */

            // kernel sin function on ~[-pi/4, pi/4] (except on -0), pi/4 ~ 0.7854
            // Input x is assumed to be bounded by ~pi/4 in magnitude.
            // Input y is the tail of x.
            // Input iy indicates whether y is 0. (if iy=0, y assume to be 0).
            //
            // Algorithm
            //      1. Since sin(-x) = -sin(x), we need only to consider positive x.
            //      2. Callers must return sin(-0) = -0 without calling here since our
            //         odd polynomial is not evaluated in a way that preserves -0.
            //         Callers may do the optimization sin(x) ~ x for tiny x.
            //      3. sin(x) is approximated by a polynomial of degree 13 on
            //         [0,pi/4]
            //                               3            13
            //              sin(x) ~ x + S1*x + ... + S6*x
            //         where
            //
            //      |sin(x)         2     4     6     8     10     12  |     -58
            //      |----- - (1+S1*x +S2*x +S3*x +S4*x +S5*x  +S6*x   )| <= 2
            //      |  x                                               |
            //
            //      4. sin(x+y) = sin(x) + sin'(x')*y
            //                  ~ sin(x) + (1-x*x/2)*y
            //         For better accuracy, let
            //                   3      2      2      2      2
            //              r = x *(S2+x *(S3+x *(S4+x *(S5+x *S6))))
            //         then                   3    2
            //              sin(x) = x + (S1*x + (x *(r-y/2)+y))
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn k_sin(x: f64, y: f64, iy: i32) -> f64 {
                let z = x * x;
                let w = z * z;
                let r = S2 + z * (S3 + z * S4) + z * w * (S5 + z * S6);
                let v = z * x;
                if iy == 0 {
                    x + v * (S1 + z * r)
                } else {
                    x - ((z * (0.5 * y - v * r) - y) - v * S1)
                }
            }
        }
        
        mod k_sinf
        {
            use ::
            {
                *,
            };
            /*
            */
            const S1: f64 = -0.166666666416265235595; /* -0x15555554cbac77.0p-55 */
            const S2: f64 = 0.0083333293858894631756; /*  0x111110896efbb2.0p-59 */
            const S3: f64 = -0.000198393348360966317347; /* -0x1a00f9e2cae774.0p-65 */
            const S4: f64 = 0.0000027183114939898219064; /*  0x16cd878c3b46a7.0p-71 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn k_sinf(x: f64) -> f32 {
                let z = x * x;
                let w = z * z;
                let r = S3 + z * S4;
                let s = z * x;
                ((x + s * (S1 + z * S2)) + s * w * r) as f32
            }
        }
        
        mod k_tan
        {
            use ::
            {
                *,
            };
            /*
            */
            static T: [f64; 13] = [
                3.33333333333334091986e-01,  /* 3FD55555, 55555563 */
                1.33333333333201242699e-01,  /* 3FC11111, 1110FE7A */
                5.39682539762260521377e-02,  /* 3FABA1BA, 1BB341FE */
                2.18694882948595424599e-02,  /* 3F9664F4, 8406D637 */
                8.86323982359930005737e-03,  /* 3F8226E3, E96E8493 */
                3.59207910759131235356e-03,  /* 3F6D6D22, C9560328 */
                1.45620945432529025516e-03,  /* 3F57DBC8, FEE08315 */
                5.88041240820264096874e-04,  /* 3F4344D8, F2F26501 */
                2.46463134818469906812e-04,  /* 3F3026F7, 1A8D1068 */
                7.81794442939557092300e-05,  /* 3F147E88, A03792A6 */
                7.14072491382608190305e-05,  /* 3F12B80F, 32F0A7E9 */
                -1.85586374855275456654e-05, /* BEF375CB, DB605373 */
                2.59073051863633712884e-05,  /* 3EFB2A70, 74BF7AD4 */
            ];
            const PIO4: f64 = 7.85398163397448278999e-01; /* 3FE921FB, 54442D18 */
            const PIO4_LO: f64 = 3.06161699786838301793e-17; /* 3C81A626, 33145C07 */

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn k_tan(mut x: f64, mut y: f64, odd: i32) -> f64 {
                let hx = (f64::to_bits(x) >> 32) as u32;
                let big = (hx & 0x7fffffff) >= 0x3FE59428; /* |x| >= 0.6744 */
                if big {
                    let sign = hx >> 31;
                    if sign != 0 {
                        x = -x;
                        y = -y;
                    }
                    x = (PIO4 - x) + (PIO4_LO - y);
                    y = 0.0;
                }
                let z = x * x;
                let w = z * z;
                /*
                * Break x^5*(T[1]+x^2*T[2]+...) into
                * x^5(T[1]+x^4*T[3]+...+x^20*T[11]) +
                * x^5(x^2*(T[2]+x^4*T[4]+...+x^22*[T12]))
                */
                let r = T[1] + w * (T[3] + w * (T[5] + w * (T[7] + w * (T[9] + w * T[11]))));
                let v = z * (T[2] + w * (T[4] + w * (T[6] + w * (T[8] + w * (T[10] + w * T[12])))));
                let s = z * x;
                let r = y + z * (s * (r + v) + y) + s * T[0];
                let w = x + r;
                if big {
                    let sign = hx >> 31;
                    let s = 1.0 - 2.0 * odd as f64;
                    let v = s - 2.0 * (x + (r - w * w / (w + s)));
                    return if sign != 0 { -v } else { v };
                }
                if odd == 0 {
                    return w;
                }
                /* -1.0/(x+r) has up to 2ulp error, so compute it accurately */
                let w0 = zero_low_word(w);
                let v = r - (w0 - x); /* w0+v = r+x */
                let a = -1.0 / w;
                let a0 = zero_low_word(a);
                a0 + a * (1.0 + a0 * w0 + a0 * v)
            }

            fn zero_low_word(x: f64) -> f64 {
                f64::from_bits(f64::to_bits(x) & 0xFFFF_FFFF_0000_0000)
            }
        }
        
        mod k_tanf
        {
            use ::
            {
                *,
            };
            /*
            */
            const T: [f64; 6] = [
                0.333331395030791399758,   /* 0x15554d3418c99f.0p-54 */
                0.133392002712976742718,   /* 0x1112fd38999f72.0p-55 */
                0.0533812378445670393523,  /* 0x1b54c91d865afe.0p-57 */
                0.0245283181166547278873,  /* 0x191df3908c33ce.0p-58 */
                0.00297435743359967304927, /* 0x185dadfcecf44e.0p-61 */
                0.00946564784943673166728, /* 0x1362b9bf971bcd.0p-59 */
            ];

            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn k_tanf(x: f64, odd: bool) -> f32 {
                let z = x * x;
                let mut r = T[4] + z * T[5];
                let t = T[2] + z * T[3];
                let w = z * z;
                let s = z * x;
                let u = T[0] + z * T[1];
                r = (x + s * u) + (s * w) * (t + w * r);
                (if odd { -1. / r } else { r }) as f32
            }
        }
        
        mod rem_pio2
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::rem_pio2_large;

            // #if FLT_EVAL_METHOD==0 || FLT_EVAL_METHOD==1
            // #define EPS DBL_EPSILON
            const EPS: f64 = 2.2204460492503131e-16;
            // #elif FLT_EVAL_METHOD==2
            // #define EPS LDBL_EPSILON
            // #endif

            // TODO: Support FLT_EVAL_METHOD?

            const TO_INT: f64 = 1.5 / EPS;
            /// 53 bits of 2/pi
            const INV_PIO2: f64 = 6.36619772367581382433e-01; /* 0x3FE45F30, 0x6DC9C883 */
            /// first 33 bits of pi/2
            const PIO2_1: f64 = 1.57079632673412561417e+00; /* 0x3FF921FB, 0x54400000 */
            /// pi/2 - PIO2_1
            const PIO2_1T: f64 = 6.07710050650619224932e-11; /* 0x3DD0B461, 0x1A626331 */
            /// second 33 bits of pi/2
            const PIO2_2: f64 = 6.07710050630396597660e-11; /* 0x3DD0B461, 0x1A600000 */
            /// pi/2 - (PIO2_1+PIO2_2)
            const PIO2_2T: f64 = 2.02226624879595063154e-21; /* 0x3BA3198A, 0x2E037073 */
            /// third 33 bits of pi/2
            const PIO2_3: f64 = 2.02226624871116645580e-21; /* 0x3BA3198A, 0x2E000000 */
            /// pi/2 - (PIO2_1+PIO2_2+PIO2_3)
            const PIO2_3T: f64 = 8.47842766036889956997e-32; /* 0x397B839A, 0x252049C1 */

            // return the remainder of x rem pi/2 in y[0]+y[1]
            // use rem_pio2_large() for large x
            //
            // caller must handle the case when reduction is not needed: |x| ~<= pi/4 */
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn rem_pio2(x: f64) -> (i32, f64, f64) {
                let x1p24 = f64::from_bits(0x4170000000000000);

                let sign = (f64::to_bits(x) >> 63) as i32;
                let ix = (f64::to_bits(x) >> 32) as u32 & 0x7fffffff;

                fn medium(x: f64, ix: u32) -> (i32, f64, f64) {
                    /* rint(x/(pi/2)), Assume round-to-nearest. */
                    let f_n = x as f64 * INV_PIO2 + TO_INT - TO_INT;
                    let n = f_n as i32;
                    let mut r = x - f_n * PIO2_1;
                    let mut w = f_n * PIO2_1T; /* 1st round, good to 85 bits */
                    let mut y0 = r - w;
                    let ui = f64::to_bits(y0);
                    let ey = (ui >> 52) as i32 & 0x7ff;
                    let ex = (ix >> 20) as i32;
                    if ex - ey > 16 {
                        /* 2nd round, good to 118 bits */
                        let t = r;
                        w = f_n * PIO2_2;
                        r = t - w;
                        w = f_n * PIO2_2T - ((t - r) - w);
                        y0 = r - w;
                        let ey = (f64::to_bits(y0) >> 52) as i32 & 0x7ff;
                        if ex - ey > 49 {
                            /* 3rd round, good to 151 bits, covers all cases */
                            let t = r;
                            w = f_n * PIO2_3;
                            r = t - w;
                            w = f_n * PIO2_3T - ((t - r) - w);
                            y0 = r - w;
                        }
                    }
                    let y1 = (r - y0) - w;
                    (n, y0, y1)
                }

                if ix <= 0x400f6a7a {
                    /* |x| ~<= 5pi/4 */
                    if (ix & 0xfffff) == 0x921fb {
                        /* |x| ~= pi/2 or 2pi/2 */
                        return medium(x, ix); /* cancellation -- use medium case */
                    }
                    if ix <= 0x4002d97c {
                        /* |x| ~<= 3pi/4 */
                        if sign == 0 {
                            let z = x - PIO2_1; /* one round good to 85 bits */
                            let y0 = z - PIO2_1T;
                            let y1 = (z - y0) - PIO2_1T;
                            return (1, y0, y1);
                        } else {
                            let z = x + PIO2_1;
                            let y0 = z + PIO2_1T;
                            let y1 = (z - y0) + PIO2_1T;
                            return (-1, y0, y1);
                        }
                    } else if sign == 0 {
                        let z = x - 2.0 * PIO2_1;
                        let y0 = z - 2.0 * PIO2_1T;
                        let y1 = (z - y0) - 2.0 * PIO2_1T;
                        return (2, y0, y1);
                    } else {
                        let z = x + 2.0 * PIO2_1;
                        let y0 = z + 2.0 * PIO2_1T;
                        let y1 = (z - y0) + 2.0 * PIO2_1T;
                        return (-2, y0, y1);
                    }
                }
                if ix <= 0x401c463b {
                    /* |x| ~<= 9pi/4 */
                    if ix <= 0x4015fdbc {
                        /* |x| ~<= 7pi/4 */
                        if ix == 0x4012d97c {
                            /* |x| ~= 3pi/2 */
                            return medium(x, ix);
                        }
                        if sign == 0 {
                            let z = x - 3.0 * PIO2_1;
                            let y0 = z - 3.0 * PIO2_1T;
                            let y1 = (z - y0) - 3.0 * PIO2_1T;
                            return (3, y0, y1);
                        } else {
                            let z = x + 3.0 * PIO2_1;
                            let y0 = z + 3.0 * PIO2_1T;
                            let y1 = (z - y0) + 3.0 * PIO2_1T;
                            return (-3, y0, y1);
                        }
                    } else {
                        if ix == 0x401921fb {
                            /* |x| ~= 4pi/2 */
                            return medium(x, ix);
                        }
                        if sign == 0 {
                            let z = x - 4.0 * PIO2_1;
                            let y0 = z - 4.0 * PIO2_1T;
                            let y1 = (z - y0) - 4.0 * PIO2_1T;
                            return (4, y0, y1);
                        } else {
                            let z = x + 4.0 * PIO2_1;
                            let y0 = z + 4.0 * PIO2_1T;
                            let y1 = (z - y0) + 4.0 * PIO2_1T;
                            return (-4, y0, y1);
                        }
                    }
                }
                if ix < 0x413921fb {
                    /* |x| ~< 2^20*(pi/2), medium size */
                    return medium(x, ix);
                }
                /*
                * all other (large) arguments
                */
                if ix >= 0x7ff00000 {
                    /* x is inf or NaN */
                    let y0 = x - x;
                    let y1 = y0;
                    return (0, y0, y1);
                }
                /* set z = scalbn(|x|,-ilogb(x)+23) */
                let mut ui = f64::to_bits(x);
                ui &= (!1) >> 12;
                ui |= (0x3ff + 23) << 52;
                let mut z = f64::from_bits(ui);
                let mut tx = [0.0; 3];
                for i in 0..2 {
                    tx[i] = z as i32 as f64;
                    z = (z - tx[i]) * x1p24;
                }
                tx[2] = z;
                /* skip zero terms, first term is non-zero */
                let mut i = 2;
                while i != 0 && tx[i] == 0.0 {
                    i -= 1;
                }
                let mut ty = [0.0; 3];
                let n = rem_pio2_large(&tx[..=i], &mut ty, ((ix as i32) >> 20) - (0x3ff + 23), 1);
                if sign != 0 {
                    return (-n, -ty[0], -ty[1]);
                }
                (n, ty[0], ty[1])
            }
        }
        
        mod rem_pio2_large
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::floor;
            use super::scalbn;

            // initial value for jk
            const INIT_JK: [usize; 4] = [3, 4, 4, 6];

            // Table of constants for 2/pi, 396 Hex digits (476 decimal) of 2/pi
            //
            //              integer array, contains the (24*i)-th to (24*i+23)-th
            //              bit of 2/pi after binary point. The corresponding
            //              floating value is
            //
            //                      ipio2[i] * 2^(-24(i+1)).
            //
            // NB: This table must have at least (e0-3)/24 + jk terms.
            //     For quad precision (e0 <= 16360, jk = 6), this is 686.
            #[cfg(target_pointer_width = "32")]
            const IPIO2: [i32; 66] = [
                0xA2F983, 0x6E4E44, 0x1529FC, 0x2757D1, 0xF534DD, 0xC0DB62, 0x95993C, 0x439041, 0xFE5163,
                0xABDEBB, 0xC561B7, 0x246E3A, 0x424DD2, 0xE00649, 0x2EEA09, 0xD1921C, 0xFE1DEB, 0x1CB129,
                0xA73EE8, 0x8235F5, 0x2EBB44, 0x84E99C, 0x7026B4, 0x5F7E41, 0x3991D6, 0x398353, 0x39F49C,
                0x845F8B, 0xBDF928, 0x3B1FF8, 0x97FFDE, 0x05980F, 0xEF2F11, 0x8B5A0A, 0x6D1F6D, 0x367ECF,
                0x27CB09, 0xB74F46, 0x3F669E, 0x5FEA2D, 0x7527BA, 0xC7EBE5, 0xF17B3D, 0x0739F7, 0x8A5292,
                0xEA6BFB, 0x5FB11F, 0x8D5D08, 0x560330, 0x46FC7B, 0x6BABF0, 0xCFBC20, 0x9AF436, 0x1DA9E3,
                0x91615E, 0xE61B08, 0x659985, 0x5F14A0, 0x68408D, 0xFFD880, 0x4D7327, 0x310606, 0x1556CA,
                0x73A8C9, 0x60E27B, 0xC08C6B,
            ];

            #[cfg(target_pointer_width = "64")]
            const IPIO2: [i32; 690] = [
                0xA2F983, 0x6E4E44, 0x1529FC, 0x2757D1, 0xF534DD, 0xC0DB62, 0x95993C, 0x439041, 0xFE5163,
                0xABDEBB, 0xC561B7, 0x246E3A, 0x424DD2, 0xE00649, 0x2EEA09, 0xD1921C, 0xFE1DEB, 0x1CB129,
                0xA73EE8, 0x8235F5, 0x2EBB44, 0x84E99C, 0x7026B4, 0x5F7E41, 0x3991D6, 0x398353, 0x39F49C,
                0x845F8B, 0xBDF928, 0x3B1FF8, 0x97FFDE, 0x05980F, 0xEF2F11, 0x8B5A0A, 0x6D1F6D, 0x367ECF,
                0x27CB09, 0xB74F46, 0x3F669E, 0x5FEA2D, 0x7527BA, 0xC7EBE5, 0xF17B3D, 0x0739F7, 0x8A5292,
                0xEA6BFB, 0x5FB11F, 0x8D5D08, 0x560330, 0x46FC7B, 0x6BABF0, 0xCFBC20, 0x9AF436, 0x1DA9E3,
                0x91615E, 0xE61B08, 0x659985, 0x5F14A0, 0x68408D, 0xFFD880, 0x4D7327, 0x310606, 0x1556CA,
                0x73A8C9, 0x60E27B, 0xC08C6B, 0x47C419, 0xC367CD, 0xDCE809, 0x2A8359, 0xC4768B, 0x961CA6,
                0xDDAF44, 0xD15719, 0x053EA5, 0xFF0705, 0x3F7E33, 0xE832C2, 0xDE4F98, 0x327DBB, 0xC33D26,
                0xEF6B1E, 0x5EF89F, 0x3A1F35, 0xCAF27F, 0x1D87F1, 0x21907C, 0x7C246A, 0xFA6ED5, 0x772D30,
                0x433B15, 0xC614B5, 0x9D19C3, 0xC2C4AD, 0x414D2C, 0x5D000C, 0x467D86, 0x2D71E3, 0x9AC69B,
                0x006233, 0x7CD2B4, 0x97A7B4, 0xD55537, 0xF63ED7, 0x1810A3, 0xFC764D, 0x2A9D64, 0xABD770,
                0xF87C63, 0x57B07A, 0xE71517, 0x5649C0, 0xD9D63B, 0x3884A7, 0xCB2324, 0x778AD6, 0x23545A,
                0xB91F00, 0x1B0AF1, 0xDFCE19, 0xFF319F, 0x6A1E66, 0x615799, 0x47FBAC, 0xD87F7E, 0xB76522,
                0x89E832, 0x60BFE6, 0xCDC4EF, 0x09366C, 0xD43F5D, 0xD7DE16, 0xDE3B58, 0x929BDE, 0x2822D2,
                0xE88628, 0x4D58E2, 0x32CAC6, 0x16E308, 0xCB7DE0, 0x50C017, 0xA71DF3, 0x5BE018, 0x34132E,
                0x621283, 0x014883, 0x5B8EF5, 0x7FB0AD, 0xF2E91E, 0x434A48, 0xD36710, 0xD8DDAA, 0x425FAE,
                0xCE616A, 0xA4280A, 0xB499D3, 0xF2A606, 0x7F775C, 0x83C2A3, 0x883C61, 0x78738A, 0x5A8CAF,
                0xBDD76F, 0x63A62D, 0xCBBFF4, 0xEF818D, 0x67C126, 0x45CA55, 0x36D9CA, 0xD2A828, 0x8D61C2,
                0x77C912, 0x142604, 0x9B4612, 0xC459C4, 0x44C5C8, 0x91B24D, 0xF31700, 0xAD43D4, 0xE54929,
                0x10D5FD, 0xFCBE00, 0xCC941E, 0xEECE70, 0xF53E13, 0x80F1EC, 0xC3E7B3, 0x28F8C7, 0x940593,
                0x3E71C1, 0xB3092E, 0xF3450B, 0x9C1288, 0x7B20AB, 0x9FB52E, 0xC29247, 0x2F327B, 0x6D550C,
                0x90A772, 0x1FE76B, 0x96CB31, 0x4A1679, 0xE27941, 0x89DFF4, 0x9794E8, 0x84E6E2, 0x973199,
                0x6BED88, 0x365F5F, 0x0EFDBB, 0xB49A48, 0x6CA467, 0x427271, 0x325D8D, 0xB8159F, 0x09E5BC,
                0x25318D, 0x3974F7, 0x1C0530, 0x010C0D, 0x68084B, 0x58EE2C, 0x90AA47, 0x02E774, 0x24D6BD,
                0xA67DF7, 0x72486E, 0xEF169F, 0xA6948E, 0xF691B4, 0x5153D1, 0xF20ACF, 0x339820, 0x7E4BF5,
                0x6863B2, 0x5F3EDD, 0x035D40, 0x7F8985, 0x295255, 0xC06437, 0x10D86D, 0x324832, 0x754C5B,
                0xD4714E, 0x6E5445, 0xC1090B, 0x69F52A, 0xD56614, 0x9D0727, 0x50045D, 0xDB3BB4, 0xC576EA,
                0x17F987, 0x7D6B49, 0xBA271D, 0x296996, 0xACCCC6, 0x5414AD, 0x6AE290, 0x89D988, 0x50722C,
                0xBEA404, 0x940777, 0x7030F3, 0x27FC00, 0xA871EA, 0x49C266, 0x3DE064, 0x83DD97, 0x973FA3,
                0xFD9443, 0x8C860D, 0xDE4131, 0x9D3992, 0x8C70DD, 0xE7B717, 0x3BDF08, 0x2B3715, 0xA0805C,
                0x93805A, 0x921110, 0xD8E80F, 0xAF806C, 0x4BFFDB, 0x0F9038, 0x761859, 0x15A562, 0xBBCB61,
                0xB989C7, 0xBD4010, 0x04F2D2, 0x277549, 0xF6B6EB, 0xBB22DB, 0xAA140A, 0x2F2689, 0x768364,
                0x333B09, 0x1A940E, 0xAA3A51, 0xC2A31D, 0xAEEDAF, 0x12265C, 0x4DC26D, 0x9C7A2D, 0x9756C0,
                0x833F03, 0xF6F009, 0x8C402B, 0x99316D, 0x07B439, 0x15200C, 0x5BC3D8, 0xC492F5, 0x4BADC6,
                0xA5CA4E, 0xCD37A7, 0x36A9E6, 0x9492AB, 0x6842DD, 0xDE6319, 0xEF8C76, 0x528B68, 0x37DBFC,
                0xABA1AE, 0x3115DF, 0xA1AE00, 0xDAFB0C, 0x664D64, 0xB705ED, 0x306529, 0xBF5657, 0x3AFF47,
                0xB9F96A, 0xF3BE75, 0xDF9328, 0x3080AB, 0xF68C66, 0x15CB04, 0x0622FA, 0x1DE4D9, 0xA4B33D,
                0x8F1B57, 0x09CD36, 0xE9424E, 0xA4BE13, 0xB52333, 0x1AAAF0, 0xA8654F, 0xA5C1D2, 0x0F3F0B,
                0xCD785B, 0x76F923, 0x048B7B, 0x721789, 0x53A6C6, 0xE26E6F, 0x00EBEF, 0x584A9B, 0xB7DAC4,
                0xBA66AA, 0xCFCF76, 0x1D02D1, 0x2DF1B1, 0xC1998C, 0x77ADC3, 0xDA4886, 0xA05DF7, 0xF480C6,
                0x2FF0AC, 0x9AECDD, 0xBC5C3F, 0x6DDED0, 0x1FC790, 0xB6DB2A, 0x3A25A3, 0x9AAF00, 0x9353AD,
                0x0457B6, 0xB42D29, 0x7E804B, 0xA707DA, 0x0EAA76, 0xA1597B, 0x2A1216, 0x2DB7DC, 0xFDE5FA,
                0xFEDB89, 0xFDBE89, 0x6C76E4, 0xFCA906, 0x70803E, 0x156E85, 0xFF87FD, 0x073E28, 0x336761,
                0x86182A, 0xEABD4D, 0xAFE7B3, 0x6E6D8F, 0x396795, 0x5BBF31, 0x48D784, 0x16DF30, 0x432DC7,
                0x356125, 0xCE70C9, 0xB8CB30, 0xFD6CBF, 0xA200A4, 0xE46C05, 0xA0DD5A, 0x476F21, 0xD21262,
                0x845CB9, 0x496170, 0xE0566B, 0x015299, 0x375550, 0xB7D51E, 0xC4F133, 0x5F6E13, 0xE4305D,
                0xA92E85, 0xC3B21D, 0x3632A1, 0xA4B708, 0xD4B1EA, 0x21F716, 0xE4698F, 0x77FF27, 0x80030C,
                0x2D408D, 0xA0CD4F, 0x99A520, 0xD3A2B3, 0x0A5D2F, 0x42F9B4, 0xCBDA11, 0xD0BE7D, 0xC1DB9B,
                0xBD17AB, 0x81A2CA, 0x5C6A08, 0x17552E, 0x550027, 0xF0147F, 0x8607E1, 0x640B14, 0x8D4196,
                0xDEBE87, 0x2AFDDA, 0xB6256B, 0x34897B, 0xFEF305, 0x9EBFB9, 0x4F6A68, 0xA82A4A, 0x5AC44F,
                0xBCF82D, 0x985AD7, 0x95C7F4, 0x8D4D0D, 0xA63A20, 0x5F57A4, 0xB13F14, 0x953880, 0x0120CC,
                0x86DD71, 0xB6DEC9, 0xF560BF, 0x11654D, 0x6B0701, 0xACB08C, 0xD0C0B2, 0x485551, 0x0EFB1E,
                0xC37295, 0x3B06A3, 0x3540C0, 0x7BDC06, 0xCC45E0, 0xFA294E, 0xC8CAD6, 0x41F3E8, 0xDE647C,
                0xD8649B, 0x31BED9, 0xC397A4, 0xD45877, 0xC5E369, 0x13DAF0, 0x3C3ABA, 0x461846, 0x5F7555,
                0xF5BDD2, 0xC6926E, 0x5D2EAC, 0xED440E, 0x423E1C, 0x87C461, 0xE9FD29, 0xF3D6E7, 0xCA7C22,
                0x35916F, 0xC5E008, 0x8DD7FF, 0xE26A6E, 0xC6FDB0, 0xC10893, 0x745D7C, 0xB2AD6B, 0x9D6ECD,
                0x7B723E, 0x6A11C6, 0xA9CFF7, 0xDF7329, 0xBAC9B5, 0x5100B7, 0x0DB2E2, 0x24BA74, 0x607DE5,
                0x8AD874, 0x2C150D, 0x0C1881, 0x94667E, 0x162901, 0x767A9F, 0xBEFDFD, 0xEF4556, 0x367ED9,
                0x13D9EC, 0xB9BA8B, 0xFC97C4, 0x27A831, 0xC36EF1, 0x36C594, 0x56A8D8, 0xB5A8B4, 0x0ECCCF,
                0x2D8912, 0x34576F, 0x89562C, 0xE3CE99, 0xB920D6, 0xAA5E6B, 0x9C2A3E, 0xCC5F11, 0x4A0BFD,
                0xFBF4E1, 0x6D3B8E, 0x2C86E2, 0x84D4E9, 0xA9B4FC, 0xD1EEEF, 0xC9352E, 0x61392F, 0x442138,
                0xC8D91B, 0x0AFC81, 0x6A4AFB, 0xD81C2F, 0x84B453, 0x8C994E, 0xCC2254, 0xDC552A, 0xD6C6C0,
                0x96190B, 0xB8701A, 0x649569, 0x605A26, 0xEE523F, 0x0F117F, 0x11B5F4, 0xF5CBFC, 0x2DBC34,
                0xEEBC34, 0xCC5DE8, 0x605EDD, 0x9B8E67, 0xEF3392, 0xB817C9, 0x9B5861, 0xBC57E1, 0xC68351,
                0x103ED8, 0x4871DD, 0xDD1C2D, 0xA118AF, 0x462C21, 0xD7F359, 0x987AD9, 0xC0549E, 0xFA864F,
                0xFC0656, 0xAE79E5, 0x362289, 0x22AD38, 0xDC9367, 0xAAE855, 0x382682, 0x9BE7CA, 0xA40D51,
                0xB13399, 0x0ED7A9, 0x480569, 0xF0B265, 0xA7887F, 0x974C88, 0x36D1F9, 0xB39221, 0x4A827B,
                0x21CF98, 0xDC9F40, 0x5547DC, 0x3A74E1, 0x42EB67, 0xDF9DFE, 0x5FD45E, 0xA4677B, 0x7AACBA,
                0xA2F655, 0x23882B, 0x55BA41, 0x086E59, 0x862A21, 0x834739, 0xE6E389, 0xD49EE5, 0x40FB49,
                0xE956FF, 0xCA0F1C, 0x8A59C5, 0x2BFA94, 0xC5C1D3, 0xCFC50F, 0xAE5ADB, 0x86C547, 0x624385,
                0x3B8621, 0x94792C, 0x876110, 0x7B4C2A, 0x1A2C80, 0x12BF43, 0x902688, 0x893C78, 0xE4C4A8,
                0x7BDBE5, 0xC23AC4, 0xEAF426, 0x8A67F7, 0xBF920D, 0x2BA365, 0xB1933D, 0x0B7CBD, 0xDC51A4,
                0x63DD27, 0xDDE169, 0x19949A, 0x9529A8, 0x28CE68, 0xB4ED09, 0x209F44, 0xCA984E, 0x638270,
                0x237C7E, 0x32B90F, 0x8EF5A7, 0xE75614, 0x08F121, 0x2A9DB5, 0x4D7E6F, 0x5119A5, 0xABF9B5,
                0xD6DF82, 0x61DD96, 0x023616, 0x9F3AC4, 0xA1A283, 0x6DED72, 0x7A8D39, 0xA9B882, 0x5C326B,
                0x5B2746, 0xED3400, 0x7700D2, 0x55F4FC, 0x4D5901, 0x8071E0,
            ];

            const PIO2: [f64; 8] = [
                1.57079625129699707031e+00, /* 0x3FF921FB, 0x40000000 */
                7.54978941586159635335e-08, /* 0x3E74442D, 0x00000000 */
                5.39030252995776476554e-15, /* 0x3CF84698, 0x80000000 */
                3.28200341580791294123e-22, /* 0x3B78CC51, 0x60000000 */
                1.27065575308067607349e-29, /* 0x39F01B83, 0x80000000 */
                1.22933308981111328932e-36, /* 0x387A2520, 0x40000000 */
                2.73370053816464559624e-44, /* 0x36E38222, 0x80000000 */
                2.16741683877804819444e-51, /* 0x3569F31D, 0x00000000 */
            ];

            // fn rem_pio2_large(x : &[f64], y : &mut [f64], e0 : i32, prec : usize) -> i32
            //
            // Input parameters:
            //      x[]     The input value (must be positive) is broken into nx
            //              pieces of 24-bit integers in double precision format.
            //              x[i] will be the i-th 24 bit of x. The scaled exponent
            //              of x[0] is given in input parameter e0 (i.e., x[0]*2^e0
            //              match x's up to 24 bits.
            //
            //              Example of breaking a double positive z into x[0]+x[1]+x[2]:
            //                      e0 = ilogb(z)-23
            //                      z  = scalbn(z,-e0)
            //              for i = 0,1,2
            //                      x[i] = floor(z)
            //                      z    = (z-x[i])*2**24
            //
            //      y[]     ouput result in an array of double precision numbers.
            //              The dimension of y[] is:
            //                      24-bit  precision       1
            //                      53-bit  precision       2
            //                      64-bit  precision       2
            //                      113-bit precision       3
            //              The actual value is the sum of them. Thus for 113-bit
            //              precison, one may have to do something like:
            //
            //              long double t,w,r_head, r_tail;
            //              t = (long double)y[2] + (long double)y[1];
            //              w = (long double)y[0];
            //              r_head = t+w;
            //              r_tail = w - (r_head - t);
            //
            //      e0      The exponent of x[0]. Must be <= 16360 or you need to
            //              expand the ipio2 table.
            //
            //      prec    an integer indicating the precision:
            //                      0       24  bits (single)
            //                      1       53  bits (double)
            //                      2       64  bits (extended)
            //                      3       113 bits (quad)
            //
            // Here is the description of some local variables:
            //
            //      jk      jk+1 is the initial number of terms of ipio2[] needed
            //              in the computation. The minimum and recommended value
            //              for jk is 3,4,4,6 for single, double, extended, and quad.
            //              jk+1 must be 2 larger than you might expect so that our
            //              recomputation test works. (Up to 24 bits in the integer
            //              part (the 24 bits of it that we compute) and 23 bits in
            //              the fraction part may be lost to cancelation before we
            //              recompute.)
            //
            //      jz      local integer variable indicating the number of
            //              terms of ipio2[] used.
            //
            //      jx      nx - 1
            //
            //      jv      index for pointing to the suitable ipio2[] for the
            //              computation. In general, we want
            //                      ( 2^e0*x[0] * ipio2[jv-1]*2^(-24jv) )/8
            //              is an integer. Thus
            //                      e0-3-24*jv >= 0 or (e0-3)/24 >= jv
            //              Hence jv = max(0,(e0-3)/24).
            //
            //      jp      jp+1 is the number of terms in PIo2[] needed, jp = jk.
            //
            //      q[]     double array with integral value, representing the
            //              24-bits chunk of the product of x and 2/pi.
            //
            //      q0      the corresponding exponent of q[0]. Note that the
            //              exponent for q[i] would be q0-24*i.
            //
            //      PIo2[]  double precision array, obtained by cutting pi/2
            //              into 24 bits chunks.
            //
            //      f[]     ipio2[] in floating point
            //
            //      iq[]    integer array by breaking up q[] in 24-bits chunk.
            //
            //      fq[]    final product of x*(2/pi) in fq[0],..,fq[jk]
            //
            //      ih      integer. If >0 it indicates q[] is >= 0.5, hence
            //              it also indicates the *sign* of the result.

            /// Return the last three digits of N with y = x - N*pi/2
            /// so that |y| < pi/2.
            ///
            /// The method is to compute the integer (mod 8) and fraction parts of
            /// (2/pi)*x without doing the full multiplication. In general we
            /// skip the part of the product that are known to be a huge integer (
            /// more accurately, = 0 mod 8 ). Thus the number of operations are
            /// independent of the exponent of the input.
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn rem_pio2_large(x: &[f64], y: &mut [f64], e0: i32, prec: usize) -> i32 {
                let x1p24 = f64::from_bits(0x4170000000000000); // 0x1p24 === 2 ^ 24
                let x1p_24 = f64::from_bits(0x3e70000000000000); // 0x1p_24 === 2 ^ (-24)

                #[cfg(all(target_pointer_width = "64", feature = "checked"))]
                assert!(e0 <= 16360);

                let nx = x.len();

                let mut fw: f64;
                let mut n: i32;
                let mut ih: i32;
                let mut z: f64;
                let mut f: [f64; 20] = [0.; 20];
                let mut fq: [f64; 20] = [0.; 20];
                let mut q: [f64; 20] = [0.; 20];
                let mut iq: [i32; 20] = [0; 20];

                /* initialize jk*/
                let jk = INIT_JK[prec];
                let jp = jk;

                /* determine jx,jv,q0, note that 3>q0 */
                let jx = nx - 1;
                let mut jv = (e0 - 3) / 24;
                if jv < 0 {
                    jv = 0;
                }
                let mut q0 = e0 - 24 * (jv + 1);
                let jv = jv as usize;

                /* set up f[0] to f[jx+jk] where f[jx+jk] = ipio2[jv+jk] */
                let mut j = (jv as i32) - (jx as i32);
                let m = jx + jk;
                for i in 0..=m {
                    i!(f, i, =, if j < 0 {
                        0.
                    } else {
                        i!(IPIO2, j as usize) as f64
                    });
                    j += 1;
                }

                /* compute q[0],q[1],...q[jk] */
                for i in 0..=jk {
                    fw = 0f64;
                    for j in 0..=jx {
                        fw += i!(x, j) * i!(f, jx + i - j);
                    }
                    i!(q, i, =, fw);
                }

                let mut jz = jk;

                'recompute: loop {
                    /* distill q[] into iq[] reversingly */
                    let mut i = 0i32;
                    z = i!(q, jz);
                    for j in (1..=jz).rev() {
                        fw = (x1p_24 * z) as i32 as f64;
                        i!(iq, i as usize, =, (z - x1p24 * fw) as i32);
                        z = i!(q, j - 1) + fw;
                        i += 1;
                    }

                    /* compute n */
                    z = scalbn(z, q0); /* actual value of z */
                    z -= 8.0 * floor(z * 0.125); /* trim off integer >= 8 */
                    n = z as i32;
                    z -= n as f64;
                    ih = 0;
                    if q0 > 0 {
                        /* need iq[jz-1] to determine n */
                        i = i!(iq, jz - 1) >> (24 - q0);
                        n += i;
                        i!(iq, jz - 1, -=, i << (24 - q0));
                        ih = i!(iq, jz - 1) >> (23 - q0);
                    } else if q0 == 0 {
                        ih = i!(iq, jz - 1) >> 23;
                    } else if z >= 0.5 {
                        ih = 2;
                    }

                    if ih > 0 {
                        /* q > 0.5 */
                        n += 1;
                        let mut carry = 0i32;
                        for i in 0..jz {
                            /* compute 1-q */
                            let j = i!(iq, i);
                            if carry == 0 {
                                if j != 0 {
                                    carry = 1;
                                    i!(iq, i, =, 0x1000000 - j);
                                }
                            } else {
                                i!(iq, i, =, 0xffffff - j);
                            }
                        }
                        if q0 > 0 {
                            /* rare case: chance is 1 in 12 */
                            match q0 {
                                1 => {
                                    i!(iq, jz - 1, &=, 0x7fffff);
                                }
                                2 => {
                                    i!(iq, jz - 1, &=, 0x3fffff);
                                }
                                _ => {}
                            }
                        }
                        if ih == 2 {
                            z = 1. - z;
                            if carry != 0 {
                                z -= scalbn(1., q0);
                            }
                        }
                    }

                    /* check if recomputation is needed */
                    if z == 0. {
                        let mut j = 0;
                        for i in (jk..=jz - 1).rev() {
                            j |= i!(iq, i);
                        }
                        if j == 0 {
                            /* need recomputation */
                            let mut k = 1;
                            while i!(iq, jk - k, ==, 0) {
                                k += 1; /* k = no. of terms needed */
                            }

                            for i in (jz + 1)..=(jz + k) {
                                /* add q[jz+1] to q[jz+k] */
                                i!(f, jx + i, =, i!(IPIO2, jv + i) as f64);
                                fw = 0f64;
                                for j in 0..=jx {
                                    fw += i!(x, j) * i!(f, jx + i - j);
                                }
                                i!(q, i, =, fw);
                            }
                            jz += k;
                            continue 'recompute;
                        }
                    }

                    break;
                }

                /* chop off zero terms */
                if z == 0. {
                    jz -= 1;
                    q0 -= 24;
                    while i!(iq, jz) == 0 {
                        jz -= 1;
                        q0 -= 24;
                    }
                } else {
                    /* break z into 24-bit if necessary */
                    z = scalbn(z, -q0);
                    if z >= x1p24 {
                        fw = (x1p_24 * z) as i32 as f64;
                        i!(iq, jz, =, (z - x1p24 * fw) as i32);
                        jz += 1;
                        q0 += 24;
                        i!(iq, jz, =, fw as i32);
                    } else {
                        i!(iq, jz, =, z as i32);
                    }
                }

                /* convert integer "bit" chunk to floating-point value */
                fw = scalbn(1., q0);
                for i in (0..=jz).rev() {
                    i!(q, i, =, fw * (i!(iq, i) as f64));
                    fw *= x1p_24;
                }

                /* compute PIo2[0,...,jp]*q[jz,...,0] */
                for i in (0..=jz).rev() {
                    fw = 0f64;
                    let mut k = 0;
                    while (k <= jp) && (k <= jz - i) {
                        fw += i!(PIO2, k) * i!(q, i + k);
                        k += 1;
                    }
                    i!(fq, jz - i, =, fw);
                }

                /* compress fq[] into y[] */
                match prec {
                    0 => {
                        fw = 0f64;
                        for i in (0..=jz).rev() {
                            fw += i!(fq, i);
                        }
                        i!(y, 0, =, if ih == 0 { fw } else { -fw });
                    }
                    1 | 2 => {
                        fw = 0f64;
                        for i in (0..=jz).rev() {
                            fw += i!(fq, i);
                        }
                        // TODO: drop excess precision here once double_t is used
                        fw = fw as f64;
                        i!(y, 0, =, if ih == 0 { fw } else { -fw });
                        fw = i!(fq, 0) - fw;
                        for i in 1..=jz {
                            fw += i!(fq, i);
                        }
                        i!(y, 1, =, if ih == 0 { fw } else { -fw });
                    }
                    3 => {
                        /* painful */
                        for i in (1..=jz).rev() {
                            fw = i!(fq, i - 1) + i!(fq, i);
                            i!(fq, i, +=, i!(fq, i - 1) - fw);
                            i!(fq, i - 1, =, fw);
                        }
                        for i in (2..=jz).rev() {
                            fw = i!(fq, i - 1) + i!(fq, i);
                            i!(fq, i, +=, i!(fq, i - 1) - fw);
                            i!(fq, i - 1, =, fw);
                        }
                        fw = 0f64;
                        for i in (2..=jz).rev() {
                            fw += i!(fq, i);
                        }
                        if ih == 0 {
                            i!(y, 0, =, i!(fq, 0));
                            i!(y, 1, =, i!(fq, 1));
                            i!(y, 2, =, fw);
                        } else {
                            i!(y, 0, =, -i!(fq, 0));
                            i!(y, 1, =, -i!(fq, 1));
                            i!(y, 2, =, -fw);
                        }
                    }
                    #[cfg(debug_assertions)]
                    _ => unreachable!(),
                    #[cfg(not(debug_assertions))]
                    _ => {}
                }
                n & 7
            }
        }
        
        mod rem_pio2f
        {
            use ::
            {
                *,
            };
            /*
            */
            use super::rem_pio2_large;

            const TOINT: f64 = 1.5 / f64::EPSILON;

            /// 53 bits of 2/pi
            const INV_PIO2: f64 = 6.36619772367581382433e-01; /* 0x3FE45F30, 0x6DC9C883 */
            /// first 25 bits of pi/2
            const PIO2_1: f64 = 1.57079631090164184570e+00; /* 0x3FF921FB, 0x50000000 */
            /// pi/2 - pio2_1
            const PIO2_1T: f64 = 1.58932547735281966916e-08; /* 0x3E5110b4, 0x611A6263 */

            /// Return the remainder of x rem pi/2 in *y
            ///
            /// use double precision for everything except passing x
            /// use __rem_pio2_large() for large x
            #[cfg_attr(all(test, assert_no_panic), no_panic::no_panic)]
            pub(crate) fn rem_pio2f(x: f32) -> (i32, f64) {
                let x64 = x as f64;

                let mut tx: [f64; 1] = [0.];
                let mut ty: [f64; 1] = [0.];

                let ix = x.to_bits() & 0x7fffffff;
                /* 25+53 bit pi is good enough for medium size */
                if ix < 0x4dc90fdb {
                    /* |x| ~< 2^28*(pi/2), medium size */
                    /* Use a specialized rint() to get fn.  Assume round-to-nearest. */
                    let f_n = x64 * INV_PIO2 + TOINT - TOINT;
                    return (f_n as i32, x64 - f_n * PIO2_1 - f_n * PIO2_1T);
                }
                if ix >= 0x7f800000 {
                    /* x is inf or NaN */
                    return (0, x64 - x64);
                }
                /* scale x into [2^23, 2^24-1] */
                let sign = (x.to_bits() >> 31) != 0;
                let e0 = ((ix >> 23) - (0x7f + 23)) as i32; /* e0 = ilogb(|x|)-23, positive */
                tx[0] = f32::from_bits(ix - (e0 << 23) as u32) as f64;
                let n = rem_pio2_large(&tx, &mut ty, e0, 0);
                if sign {
                    return (-n, -ty[0]);
                }
                (n, ty[0])
            }
        }
        

        // Private re-imports
        use self::expo2::expo2;
        use self::k_cos::k_cos;
        use self::k_cosf::k_cosf;
        use self::k_expo2::k_expo2;
        use self::k_expo2f::k_expo2f;
        use self::k_sin::k_sin;
        use self::k_sinf::k_sinf;
        use self::k_tan::k_tan;
        use self::k_tanf::k_tanf;
        use self::rem_pio2::rem_pio2;
        use self::rem_pio2_large::rem_pio2_large;
        use self::rem_pio2f::rem_pio2f;

        #[inline]
        fn get_high_word(x: f64) -> u32 {
            (x.to_bits() >> 32) as u32
        }

        #[inline]
        fn get_low_word(x: f64) -> u32 {
            x.to_bits() as u32
        }

        #[inline]
        fn with_set_high_word(f: f64, hi: u32) -> f64 {
            let mut tmp = f.to_bits();
            tmp &= 0x00000000_ffffffff;
            tmp |= (hi as u64) << 32;
            f64::from_bits(tmp)
        }

        #[inline]
        fn with_set_low_word(f: f64, lo: u32) -> f64 {
            let mut tmp = f.to_bits();
            tmp &= 0xffffffff_00000000;
            tmp |= lo as u64;
            f64::from_bits(tmp)
        }

        #[inline]
        fn combine_words(hi: u32, lo: u32) -> f64 {
            f64::from_bits((hi as u64) << 32 | lo as u64)
        }
    } pub use self::lib::*;

    pub fn _eqf(a: f32, b: f32) -> Result<(), u32>
    {
        if a.is_nan() && b.is_nan() {
            Ok(())
        } else {
            let err = (a.to_bits() as i32).wrapping_sub(b.to_bits() as i32).abs();

            if err <= 1 {
                Ok(())
            } else {
                Err(err as u32)
            }
        }
    }
    
    #[inline] pub fn _eq(a: f64, b: f64) -> Result<(), u64>
    {
        if a.is_nan() && b.is_nan() {
            Ok(())
        } else {
            let err = (a.to_bits() as i64).wrapping_sub(b.to_bits() as i64).abs();

            if err <= 1 {
                Ok(())
            } else {
                Err(err as u64)
            }
        }
    }
}

pub mod ptr
{
    pub use std::ptr::{ * };
}

pub mod u32
{
    pub use std::u32::{ * };
}

pub mod u64
{
    pub use std::u64::{ * };
}
// 10974 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
