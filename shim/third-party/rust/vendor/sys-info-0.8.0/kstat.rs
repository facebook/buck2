#![cfg(any(target_os = "solaris", target_os = "illumos"))]

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

// There is presently no static CStr constructor, so use these constants with
// the c() wrapper below:
const MODULE_CPU_INFO: &[u8] = b"cpu_info\0";

const STAT_CLOCK_MHZ: &[u8] = b"clock_MHz\0";

const MODULE_UNIX: &[u8] = b"unix\0";

const NAME_SYSTEM_MISC: &[u8] = b"system_misc\0";
const STAT_BOOT_TIME: &[u8] = b"boot_time\0";
const STAT_NPROC: &[u8] = b"nproc\0";

const NAME_SYSTEM_PAGES: &[u8] = b"system_pages\0";
const STAT_FREEMEM: &[u8] = b"freemem\0";
const STAT_PHYSMEM: &[u8] = b"physmem\0";

fn c(buf: &[u8]) -> &std::ffi::CStr {
    std::ffi::CStr::from_bytes_with_nul(buf).expect("invalid string constant")
}


mod wrapper {
    use std::os::raw::c_int;
    use std::os::raw::c_uint;
    use std::os::raw::c_char;
    use std::os::raw::c_uchar;
    use std::os::raw::c_void;
    use std::os::raw::c_long;
    use std::os::raw::c_ulong;
    use std::os::raw::c_longlong;
    use std::ptr::{null, null_mut, NonNull};
    use std::ffi::CStr;

    const KSTAT_TYPE_NAMED: c_uchar = 1;

    const KSTAT_STRLEN: usize = 31;

    #[repr(C)]
    struct Kstat {
        ks_crtime: c_longlong,
        ks_next: *mut Kstat,
        ks_kid: c_uint,
        ks_module: [c_char; KSTAT_STRLEN],
        ks_resv: c_uchar,
        ks_instance: c_int,
        ks_name: [c_char; KSTAT_STRLEN],
        ks_type: c_uchar,
        ks_class: [c_char; KSTAT_STRLEN],
        ks_flags: c_uchar,
        ks_data: *mut c_void,
        ks_ndata: c_uint,
        ks_data_size: usize,
        ks_snaptime: c_longlong,
    }

    impl Kstat {
        fn name(&self) -> &CStr {
            unsafe { CStr::from_ptr(self.ks_name.as_ptr()) }
        }

        fn module(&self) -> &CStr {
            unsafe { CStr::from_ptr(self.ks_module.as_ptr()) }
        }
    }

    #[repr(C)]
    struct KstatCtl {
        kc_chain_id: c_uint,
        kc_chain: *mut Kstat,
        kc_kd: c_int,
    }

    #[repr(C)]
    #[derive(Copy, Clone)]
    union KstatValue {
        c: [c_char; 16],
        l: c_long,
        ul: c_ulong,
        ui32: u32,
    }

    #[repr(C)]
    struct KstatNamed {
        name: [c_char; KSTAT_STRLEN],
        data_type: c_uchar,
        value: KstatValue,
    }

    extern "C" {
        fn kstat_open() -> *mut KstatCtl;
        fn kstat_close(kc: *mut KstatCtl) -> c_int;
        fn kstat_lookup(kc: *mut KstatCtl, module: *const c_char,
            instance: c_int, name: *const c_char) -> *mut Kstat;
        fn kstat_read(kc: *mut KstatCtl, ksp: *mut Kstat, buf: *mut c_void)
            -> c_int;
        fn kstat_data_lookup(ksp: *mut Kstat, name: *const c_char)
            -> *mut c_void;
    }

    /// Minimal wrapper around libkstat(3LIB) on illumos and Solaris systems.
    pub struct KstatWrapper {
        kc: NonNull<KstatCtl>,
        ks: Option<NonNull<Kstat>>,
        stepping: bool,
    }

    /// Turn an optional CStr into a (const char *) for Some, or NULL for None.
    fn cp(p: &Option<&CStr>) -> *const c_char {
        p.map_or_else(|| null(), |p| p.as_ptr())
    }

    impl KstatWrapper {
        pub fn open() -> super::Result<Self> {
            let kc = NonNull::new(unsafe { kstat_open() });
            if let Some(kc) = kc {
                Ok(KstatWrapper {
                    kc: kc,
                    ks: None,
                    stepping: false,
                })
            } else {
                let e = std::io::Error::last_os_error();
                Err(format!("kstat_open(3KSTAT) failed: {}", e).into())
            }
        }

        /// Call kstat_lookup(3KSTAT) and store the result, if there is a match.
        pub fn lookup(&mut self, module: Option<&CStr>, name: Option<&CStr>) {
            self.ks = NonNull::new(unsafe {
                kstat_lookup(self.kc.as_ptr(), cp(&module), -1, cp(&name))
            });

            self.stepping = false;
        }

        /// Call once to start iterating, and then repeatedly for each
        /// additional kstat in the chain.  Returns false once there are no more
        /// kstat entries.
        pub fn step(&mut self) -> bool {
            if !self.stepping {
                self.stepping = true;
            } else {
                self.ks = self.ks.map_or(None,
                    |ks| NonNull::new(unsafe { ks.as_ref() }.ks_next));
            }

            if self.ks.is_none() {
                self.stepping = false;
                false
            } else {
                true
            }
        }

        /// Return the module name of the current kstat.  This routine will
        /// panic if step() has not returned true.
        pub fn module(&self) -> &CStr {
            let ks = self.ks.as_ref().expect("step() must return true first");
            unsafe { ks.as_ref() }.module()
        }

        /// Return the name of the current kstat.  This routine will panic if
        /// step() has not returned true.
        pub fn name(&self) -> &CStr {
            let ks = self.ks.as_ref().expect("step() must return true first");
            unsafe { ks.as_ref() }.name()
        }

        /// Look up a named kstat value.  For internal use by typed accessors.
        fn data_value(&self, statistic: &CStr) -> Option<NonNull<KstatNamed>> {
            let (ks, ksp) = if let Some(ks) = &self.ks {
                (unsafe { ks.as_ref() }, ks.as_ptr())
            } else {
                return None;
            };

            if unsafe { kstat_read(self.kc.as_ptr(), ksp, null_mut()) } == -1 {
                return None;
            }

            if ks.ks_type != KSTAT_TYPE_NAMED || ks.ks_ndata < 1 {
                // This is not a named kstat, or it has no data payload.
                return None;
            }

            NonNull::new(unsafe {
                kstat_data_lookup(ksp, cp(&Some(statistic)))
            }).map(|voidp| voidp.cast())
        }

        /// Look up a named kstat value and interpret it as a "long_t".
        pub fn data_long(&self, statistic: &CStr) -> Option<i64> {
            self.data_value(statistic).map(|kn| unsafe {
                kn.as_ref().value.l
            } as i64)
        }

        /// Look up a named kstat value and interpret it as a "ulong_t".
        pub fn data_ulong(&self, statistic: &CStr) -> Option<u64> {
            self.data_value(statistic).map(|kn| unsafe {
                kn.as_ref().value.ul
            } as u64)
        }

        /// Look up a named kstat value and interpret it as a "uint32_t".
        pub fn data_u32(&self, statistic: &CStr) -> Option<u32> {
            self.data_value(statistic).map(|kn| unsafe {
                kn.as_ref().value.ui32
            })
        }
    }

    impl Drop for KstatWrapper {
        fn drop(&mut self) {
            unsafe { kstat_close(self.kc.as_ptr()) };
        }
    }
}

pub fn cpu_mhz() -> Result<u64> {
    let mut k = wrapper::KstatWrapper::open()?;

    k.lookup(Some(c(MODULE_CPU_INFO)), None);
    while k.step() {
        if k.module() != c(MODULE_CPU_INFO) {
            continue;
        }

        if let Some(mhz) = k.data_long(c(STAT_CLOCK_MHZ)) {
            return Ok(mhz as u64);
        }
    }

    return Err("cpu speed kstat not found".into());
}

pub fn boot_time() -> Result<u64> {
    let mut k = wrapper::KstatWrapper::open()?;

    k.lookup(Some(c(MODULE_UNIX)), Some(c(NAME_SYSTEM_MISC)));
    while k.step() {
        if k.module() != c(MODULE_UNIX) || k.name() != c(NAME_SYSTEM_MISC) {
            continue;
        }

        if let Some(boot_time) = k.data_u32(c(STAT_BOOT_TIME)) {
            return Ok(boot_time as u64);
        }
    }

    return Err("boot time kstat not found".into());
}

pub fn nproc() -> Result<u64> {
    let mut k = wrapper::KstatWrapper::open()?;

    k.lookup(Some(c(MODULE_UNIX)), Some(c(NAME_SYSTEM_MISC)));
    while k.step() {
        if k.module() != c(MODULE_UNIX) || k.name() != c(NAME_SYSTEM_MISC) {
            continue;
        }

        if let Some(nproc) = k.data_u32(c(STAT_NPROC)) {
            return Ok(nproc as u64);
        }
    }

    return Err("process count kstat not found".into());
}

pub struct Pages {
    pub freemem: u64,
    pub physmem: u64,
}

pub fn pages() -> Result<Pages> {
    let mut k = wrapper::KstatWrapper::open()?;

    k.lookup(Some(c(MODULE_UNIX)), Some(c(NAME_SYSTEM_PAGES)));
    while k.step() {
        if k.module() != c(MODULE_UNIX) || k.name() != c(NAME_SYSTEM_PAGES) {
            continue;
        }

        let freemem = k.data_ulong(c(STAT_FREEMEM));
        let physmem = k.data_ulong(c(STAT_PHYSMEM));

        if freemem.is_some() && physmem.is_some() {
            return Ok(Pages {
                freemem: freemem.unwrap(),
                physmem: physmem.unwrap(),
            });
        }
    }

    return Err("system pages kstat not available".into());
}
