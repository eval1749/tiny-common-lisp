#define deflayout(mp_cname, mp_meta, mp_layout) \
    InstallStaticClass(CLASS_ ## mp_cname,

#define direct_super_(mp_cname)  CLASS_ ## mp_cname,

#define direct_slot_(mp_cname, mp_initarg, mp_ty) \
    Q ## mp_cname, mp_initarg, Q ## mp_ty,

#define endlayout(mp_cname) nil );
