// @(#)$Id: //proj/evedit2/mainline/tinycl/init/tinycl_init_entry.h#1 $

#define FUN_BOOL(mp_name, mp_n) \
    { \
        Q ## mp_name, \
        mp_n, mp_n, 0, FunRet_Bool, \
        #mp_name \
    },

#define FUN_ENTRY(mp_name, mp_min, mp_max, mp_rest, mp_vals, mp_fn) \
    { \
        mp_name, \
        mp_min, mp_max, mp_rest, mp_vals, \
        #mp_fn \
    },

#define FUN_FIX(mp_name, mp_n, mp_vals) \
    FUN_ENTRY(Q ## mp_name, mp_n, mp_n, 0, mp_vals, mp_name)

#define FUN_FIX_SETF(mp_name, mp_n) \
    FUN_ENTRY(SETF_ ## mp_name, mp_n, mp_n, 0, 1, \
        setf_ ## mp_name )

#define FUN_REST(mp_name, mp_min, mp_max, mp_vals) \
    FUN_ENTRY(Q ## mp_name, mp_min, mp_max, 1, mp_vals, \
        mp_name ## V )

#define FUN_VAR(mp_name, mp_min, mp_max, mp_vals) \
    FUN_ENTRY(Q ## mp_name, mp_min, mp_max, 0, mp_vals, \
        mp_name ## _ )

#define FUN_VAR_SETF(mp_name, mp_min, mp_max) \
    FUN_ENTRY(SETF_ ## mp_name, mp_min, mp_max, 0, 1, \
        setf_ ## mp_name ## _ )
