// -*- Mode: C++; -*-
// TinyCl - Float Layout
// tinycl_float.inc
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_layout.inc#9 $
//
#include "./tinycl_begin_list.h"

// double-float
defloat64(cl, double_float_epsilon,                    0, -105, 0, 1)
defloat64(cl, double_float_negative_epsilon,           1, -106, 0, 1)

defloat64(cl, least_negative_double_float,             1, 0, 0, 1)
defloat64(cl, least_negative_normalized_double_float,  1, 1, 0, 0)

defloat64(cl, least_positive_double_float,             0, 0, 0, 1)
defloat64(cl, least_positive_normalized_double_float,  0, 1, 0, 0)

defloat64(cl, most_negative_double_float,             1, 2046, (1<<20)-1, -1)
defloat64(cl, most_positive_double_float,             0, 2046, (1<<20),   -1)

// single-float

// single-float-epsilon 5.960465E-8 (8388609 -47 1)
defloat32(cl, single_float_epsilon,                    0, -47, 1)

// single-float-negative-epsilon 2.9802326E-8 (8388609 -48 1)
defloat32(cl, single_float_negative_epsilon,           1, -48, 1)

defloat32(cl, least_negative_single_float,             1, 0, 1)
defloat32(cl, least_negative_normalized_single_float,  1, 1, 0)

defloat32(cl, least_positive_single_float,             0, 0, 1)
defloat32(cl, least_positive_normalized_single_float,  0, 1, 0)

defloat32(cl, most_negative_single_float,             1, 254, (1<<23)-1)
defloat32(cl, most_positive_single_float,             0, 254, (1<<23)-1)

#if 0
// We don't need infinity and NaN as constant. We should generate
// these non-number by expression to signal exception if exception
// isn't masked. See libm implementation arch/generic/lisp/math/.
defloat64(cl, double_float_infinity,             0, 2047, 0, 0)
defloat64(cl, double_float_negative_infinity,   1, 2047, 0, 0)

defloat64(cl, double_float_nan,  0, 2047, 1)
defloat64(cl, double_float_snan, 0, 2047, 1<<(50-32), 0)

defloat32(cl, single_float_infinity,            0, 255, 0, 0)
defloat32(cl, single_float_negative_infinity,   1, 255, 0, 0)

defloat64(cl, single_float_nan,  0, 255, 1)
defloat64(cl, single_float_snan, 0, 255, 1<<21, 0)
#endif

#include "./tinycl_end_list.h"
