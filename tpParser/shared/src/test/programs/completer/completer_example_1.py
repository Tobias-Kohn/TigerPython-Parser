# 49
# acos;acosh;asin;asinh;atan;atan2;atanh;ceil;copysign;cos;cosh;degrees;e;erf;erfc;exp;expm1;fabs;factorial;floor;fmod;frexp;fsum;gamma;hypot;isinf;isnan;ldexp;lgamma;log;log10;log1p;modf;pi;pow;radians;sin;sinh;sqrt;tan;tanh;trunc
import math

def rotate(x, y, phi):
    a = math.atan2(y, x) + phi
    r = math.sqrt(x**2 + y**2)
    return (r * math.cos(a), r * math.sin(a))
