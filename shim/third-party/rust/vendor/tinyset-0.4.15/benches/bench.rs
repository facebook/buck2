use rand::Rng;
use scaling::{bench_gen_env, bench_scaling_gen};

use stats_alloc::{Region, StatsAlloc, INSTRUMENTED_SYSTEM};
use std::alloc::System;

#[global_allocator]
static GLOBAL: &StatsAlloc<System> = &INSTRUMENTED_SYSTEM;

static SIZES: &[usize] = &[0, 1, 2, 3, 4, 10, 100, 1000];

fn mem_used<T>(f: impl Fn() -> T) -> (T, usize) {
    let reg = Region::new(&GLOBAL);
    let v = f();
    let stats = reg.change();
    let total = stats.bytes_allocated as i64 + stats.bytes_reallocated as i64
        - stats.bytes_deallocated as i64;
    if total > 0 {
        (v, std::mem::size_of::<T>() + total as usize)
    } else {
        (v, std::mem::size_of::<T>())
    }
}

fn bench_sets(density: f64, num_elements: usize) {
    assert!(density <= 1.0);
    let gen32 = move || {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u32 + 1;
        let mut set = tinyset::SetU32::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        let x = rng.gen_range(0..mx);
        set.insert(x);
        set.remove(x); // ensure there is room for one more
        (rng.gen_range(0..mx), set)
    };
    let genroaring = move || {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u32 + 1;
        let mut set = roaring::RoaringBitmap::new();
        while set.len() < num_elements as u64 {
            set.insert(rng.gen_range(0..mx));
        }
        let x = rng.gen_range(0..mx);
        set.insert(x);
        set.remove(x); // ensure there is room for one more
        (rng.gen_range(0..mx), set)
    };
    let genstd32 = move || {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u32 + 1;
        let mut set = std::collections::HashSet::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        let x = rng.gen_range(0..mx);
        set.insert(x);
        set.remove(&x); // ensure there is room for one more
        (rng.gen_range(0..mx), set)
    };
    let gen = move || {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u64 + 1;
        let mut set = tinyset::SetU64::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        let x = rng.gen_range(0..mx);
        set.insert(x);
        set.remove(x); // ensure there is room for one more
        (rng.gen_range(0..mx), set)
    };
    let gen_hashset = move || {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u64 + 1;
        let mut set = std::collections::HashSet::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        let x = rng.gen_range(0..mx);
        set.insert(x);
        set.remove(&x); // ensure there is room for one more
        (rng.gen_range(0..mx), set)
    };
    let gen_tiny = move || {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u64 + 1;
        let mut set = tinyset::Set64::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        let x = rng.gen_range(0..mx);
        set.insert(x);
        set.remove(&x); // ensure there is room for one more
        (rng.gen_range(0..mx), set)
    };
    let gen_idset = move || {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as usize + 1;
        let mut set = id_set::IdSet::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        let x = rng.gen_range(0..mx);
        set.insert(x);
        set.remove(x); // ensure there is room for one more
        (rng.gen_range(0..mx), set)
    };

    println!(
        "\n{:5}, {:3}:        {:>7} {:>7} {:>7} {:>7} {:>7} {:>7} {:>7}",
        density, num_elements, "u32", "roaring", "std32", "u64", "old64", "std", "id-set"
    );

    // ensure that we've used the random number generator at least
    // once, so we won't be confused by any memory that it allocates.
    gen32();
    println!(
        "{:>17}: {:6.0}b {:6.0}b {:6.0}b {:6.0}b {:6.0}b {:6.0}b {:6.0}b",
        "size",
        (0..100).map(|_| mem_used(|| gen32().1).1).sum::<usize>() as f64 / 100.0,
        (0..100)
            .map(|_| mem_used(|| genroaring().1).1)
            .sum::<usize>() as f64
            / 100.0,
        (0..100).map(|_| mem_used(|| genstd32().1).1).sum::<usize>() as f64 / 100.0,
        (0..100).map(|_| mem_used(|| gen().1).1).sum::<usize>() as f64 / 100.0,
        (0..100).map(|_| mem_used(|| gen_tiny().1).1).sum::<usize>() as f64 / 100.0,
        (0..100)
            .map(|_| mem_used(|| gen_hashset().1).1)
            .sum::<usize>() as f64
            / 100.0,
        (0..100)
            .map(|_| mem_used(|| gen_idset().1).1)
            .sum::<usize>() as f64
            / 100.0,
    );

    println!(
        "{:>17}: {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns",
        ".contains(ran)",
        bench_gen_env(gen32, |(idx, set)| { set.contains(*idx) }).ns_per_iter,
        bench_gen_env(genroaring, |(idx, set)| { set.contains(*idx) }).ns_per_iter,
        bench_gen_env(genstd32, |(idx, set)| { set.contains(idx) }).ns_per_iter,
        bench_gen_env(gen, |(idx, set)| { set.contains(*idx) }).ns_per_iter,
        bench_gen_env(gen_tiny, |(idx, set)| { set.contains(*idx) }).ns_per_iter,
        bench_gen_env(gen_hashset, |(idx, set)| { set.contains(idx) }).ns_per_iter,
        bench_gen_env(gen_idset, |(idx, set)| { set.contains(*idx) }).ns_per_iter,
    );
    println!(
        "{:>17}: {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns",
        ".remove(ran)",
        bench_gen_env(gen32, |(idx, set)| { set.remove(*idx) }).ns_per_iter,
        bench_gen_env(genroaring, |(idx, set)| { set.remove(*idx) }).ns_per_iter,
        bench_gen_env(genstd32, |(idx, set)| { set.remove(idx) }).ns_per_iter,
        bench_gen_env(gen, |(idx, set)| { set.remove(*idx) }).ns_per_iter,
        bench_gen_env(gen_tiny, |(idx, set)| { set.remove(idx) }).ns_per_iter,
        bench_gen_env(gen_hashset, |(idx, set)| { set.remove(idx) }).ns_per_iter,
        bench_gen_env(gen_idset, |(idx, set)| { set.remove(*idx) }).ns_per_iter,
    );
    println!(
        "{:>17}: {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns {:5.0}ns",
        ".insert(ran)",
        bench_gen_env(gen32, |(idx, set)| { set.insert(*idx) }).ns_per_iter,
        bench_gen_env(genroaring, |(idx, set)| { set.insert(*idx) }).ns_per_iter,
        bench_gen_env(genstd32, |(idx, set)| { set.insert(*idx) }).ns_per_iter,
        bench_gen_env(gen, |(idx, set)| { set.insert(*idx) }).ns_per_iter,
        bench_gen_env(gen_tiny, |(idx, set)| { set.insert(*idx) }).ns_per_iter,
        bench_gen_env(gen_hashset, |(idx, set)| { set.insert(*idx) }).ns_per_iter,
        bench_gen_env(gen_idset, |(idx, set)| { set.insert(*idx) }).ns_per_iter,
    );
}

fn bench_collect(density: f64) {
    assert!(density <= 1.0);
    println!(
        "\ncollect {:5}:{:>12} {:>13} {:>13} {:>13} {:>13} {:>13}",
        density, "setu32", "seturoaring", "std32", "setu64", "set64", "std"
    );
    for &sz in SIZES.iter() {
        let mut gen = move || {
            let mut rng = rand::thread_rng();
            let mx = (sz as f64 / density) as u64 + 1;
            let mut vec = Vec::new();
            while vec.iter().cloned().collect::<tinyset::SetU64>().len() < sz {
                vec.push(rng.gen_range(0..mx));
            }
            vec
        };
        let mut gen32 = move || {
            let mut rng = rand::thread_rng();
            let mx = (sz as f64 / density) as u32 + 1;
            let mut vec = Vec::new();
            while vec.iter().cloned().collect::<tinyset::SetU32>().len() < sz {
                vec.push(rng.gen_range(0..mx));
            }
            vec
        };
        let nsize = 10;
        print_times(
            sz,
            &[
                bench_gen_env(&mut gen32, |v| {
                    v.iter().cloned().collect::<tinyset::SetU32>().len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen32, |v| {
                    v.iter().cloned().collect::<roaring::RoaringBitmap>().len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen32, |v| {
                    v.iter()
                        .cloned()
                        .collect::<std::collections::HashSet<_>>()
                        .len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen, |v| {
                    v.iter().cloned().collect::<tinyset::SetU64>().len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen, |v| {
                    v.iter().cloned().collect::<tinyset::Set64<_>>().len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen, |v| {
                    v.iter()
                        .cloned()
                        .collect::<std::collections::HashSet<_>>()
                        .len()
                })
                .ns_per_iter,
            ],
        );
        print_sizes(
            sz,
            &[
                (0..nsize)
                    .map(|_| {
                        let v = gen32();
                        mem_used(|| v.iter().cloned().collect::<tinyset::SetU32>()).1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen32();
                        mem_used(|| v.iter().cloned().collect::<roaring::RoaringBitmap>()).1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen32();
                        mem_used(|| v.iter().cloned().collect::<std::collections::HashSet<_>>()).1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen();
                        mem_used(|| v.iter().cloned().collect::<tinyset::SetU64>()).1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen();
                        mem_used(|| v.iter().cloned().collect::<tinyset::Set64<_>>()).1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen();
                        mem_used(|| v.iter().cloned().collect::<std::collections::HashSet<_>>()).1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
            ],
        );
    }
    let mut gen = move |sz| {
        let mut rng = rand::thread_rng();
        let mx = (sz as f64 / density) as u64 + 1;
        let mut vec = Vec::new();
        while vec.iter().cloned().collect::<tinyset::SetU64>().len() < sz {
            vec.push(rng.gen_range(0..mx));
        }
        vec
    };
    let mut gen32 = move |sz| {
        let mut rng = rand::thread_rng();
        let mx = (sz as f64 / density) as u32 + 1;
        let mut vec = Vec::new();
        while vec.iter().cloned().collect::<tinyset::SetU32>().len() < sz {
            vec.push(rng.gen_range(0..mx));
        }
        vec
    };
    println!(
        "{:>11}: {:13.0} {:13.0} {:13.0} {:13.0} {:13.0} {:13.0}",
        ".collect()",
        bench_scaling_gen(
            &mut gen32,
            |v| { v.iter().cloned().collect::<tinyset::SetU32>().len() },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen32,
            |v| { v.iter().cloned().collect::<roaring::RoaringBitmap>().len() },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen32,
            |v| {
                v.iter()
                    .cloned()
                    .collect::<std::collections::HashSet<_>>()
                    .len()
            },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen,
            |v| { v.iter().cloned().collect::<tinyset::SetU64>().len() },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen,
            |v| { v.iter().cloned().collect::<tinyset::Set64<_>>().len() },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen,
            |v| {
                v.iter()
                    .cloned()
                    .collect::<std::collections::HashSet<_>>()
                    .len()
            },
            20
        )
        .scaling,
    );
}

fn print_sizes(n: usize, sizes: &[f64]) {
    print!("{:>11} ", "");
    for s in sizes.iter().cloned() {
        if n < 2 {
            print!(" {:6}  {:<6.0}", "", s);
        } else {
            print!(" {:6.0}N {:<6.0}", s / n as f64, s);
        }
    }
    println!("");
}
fn print_times(n: usize, times: &[f64]) {
    print!("{:>11}:    ", n);
    for t in times.iter().cloned() {
        print!(" {:^13}", format!("{:.0}ns", t));
    }
    println!("");
}

fn bench_fill_with_inserts(density: f64) {
    assert!(density <= 1.0);
    println!(
        "\ninserts {:5}:{:>12} {:>13} {:>13} {:>13} {:>13} {:>13} {:>13}",
        density, "setu32", "roaring", "std32", "setu64", "set64", "std", "id-set"
    );
    for &sz in SIZES.iter() {
        let mut gen = move || {
            let mut rng = rand::thread_rng();
            let mx = (sz as f64 / density) as u64 + 1;
            let mut vec = Vec::new();
            while vec.iter().cloned().collect::<tinyset::SetU64>().len() < sz {
                vec.push(rng.gen_range(0..mx));
            }
            vec
        };
        let mut gen32 = move || {
            let mut rng = rand::thread_rng();
            let mx = (sz as f64 / density) as u32 + 1;
            let mut vec = Vec::new();
            while vec.iter().cloned().collect::<tinyset::SetU32>().len() < sz {
                vec.push(rng.gen_range(0..mx));
            }
            vec
        };
        let mut genusize = move || {
            let mut rng = rand::thread_rng();
            let mx = (sz as f64 / density) as usize + 1;
            let mut vec = Vec::new();
            while vec.iter().cloned().collect::<tinyset::SetUsize>().len() < sz {
                vec.push(rng.gen_range(0..mx));
            }
            vec
        };
        let nsize = 10;
        print_times(
            sz,
            &[
                bench_gen_env(&mut gen32, |v| {
                    let mut s = tinyset::SetU32::new();
                    for x in v.iter().cloned() {
                        s.insert(x);
                    }
                    s.len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen32, |v| {
                    let mut s = roaring::RoaringBitmap::new();
                    for x in v.iter().cloned() {
                        s.insert(x);
                    }
                    s.len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen32, |v| {
                    let mut s = std::collections::HashSet::new();
                    for x in v.iter().cloned() {
                        s.insert(x);
                    }
                    s.len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen, |v| {
                    let mut s = tinyset::SetU64::new();
                    for x in v.iter().cloned() {
                        s.insert(x);
                    }
                    s.len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen, |v| {
                    let mut s = tinyset::Set64::new();
                    for x in v.iter().cloned() {
                        s.insert(x);
                    }
                    s.len()
                })
                .ns_per_iter,
                bench_gen_env(&mut gen, |v| {
                    let mut s = std::collections::HashSet::new();
                    for x in v.iter().cloned() {
                        s.insert(x);
                    }
                    s.len()
                })
                .ns_per_iter,
                bench_gen_env(&mut genusize, |v| {
                    let mut s = id_set::IdSet::new();
                    for x in v.iter().cloned() {
                        s.insert(x);
                    }
                    s.len()
                })
                .ns_per_iter,
            ],
        );
        print_sizes(
            sz,
            &[
                (0..nsize)
                    .map(|_| {
                        let v = gen32();
                        mem_used(|| {
                            let mut s = tinyset::SetU32::new();
                            for x in v.iter().cloned() {
                                s.insert(x);
                            }
                            s
                        })
                        .1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen32();
                        mem_used(|| {
                            let mut s = roaring::RoaringBitmap::new();
                            for x in v.iter().cloned() {
                                s.insert(x);
                            }
                            s
                        })
                        .1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen32();
                        mem_used(|| {
                            let mut s = std::collections::HashSet::new();
                            for x in v.iter().cloned() {
                                s.insert(x);
                            }
                            s
                        })
                        .1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen();
                        mem_used(|| {
                            let mut s = tinyset::SetU64::new();
                            for x in v.iter().cloned() {
                                s.insert(x);
                            }
                            s
                        })
                        .1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen();
                        mem_used(|| {
                            let mut s = tinyset::Set64::new();
                            for x in v.iter().cloned() {
                                s.insert(x);
                            }
                            s
                        })
                        .1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = gen();
                        mem_used(|| {
                            let mut s = std::collections::HashSet::new();
                            for x in v.iter().cloned() {
                                s.insert(x);
                            }
                            s
                        })
                        .1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
                (0..nsize)
                    .map(|_| {
                        let v = genusize();
                        mem_used(|| {
                            let mut s = id_set::IdSet::new();
                            for x in v.iter().cloned() {
                                s.insert(x);
                            }
                            s
                        })
                        .1
                    })
                    .sum::<usize>() as f64
                    / nsize as f64,
            ],
        );
    }
    let mut gen = move |sz| {
        let mut rng = rand::thread_rng();
        let mx = (sz as f64 / density) as u64 + 1;
        let mut vec = Vec::new();
        while vec.iter().cloned().collect::<tinyset::SetU64>().len() < sz {
            vec.push(rng.gen_range(0..mx));
        }
        vec
    };
    let mut gen32 = move |sz| {
        let mut rng = rand::thread_rng();
        let mx = (sz as f64 / density) as u32 + 1;
        let mut vec = Vec::new();
        while vec.iter().cloned().collect::<tinyset::SetU32>().len() < sz {
            vec.push(rng.gen_range(0..mx));
        }
        vec
    };
    let mut genusize = move |sz| {
        let mut rng = rand::thread_rng();
        let mx = (sz as f64 / density) as usize + 1;
        let mut vec = Vec::new();
        while vec.iter().cloned().collect::<tinyset::SetUsize>().len() < sz {
            vec.push(rng.gen_range(0..mx));
        }
        vec
    };
    println!(
        "{:>11}: {:13.0} {:13.0} {:13.0} {:13.0} {:13.0} {:13.0} {:13.0}",
        "inserts",
        bench_scaling_gen(
            &mut gen32,
            |v| {
                let mut s = tinyset::SetU32::new();
                for x in v.iter().cloned() {
                    s.insert(x);
                }
                s.len()
            },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen32,
            |v| {
                let mut s = roaring::RoaringBitmap::new();
                for x in v.iter().cloned() {
                    s.insert(x);
                }
                s.len()
            },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen32,
            |v| {
                let mut s = std::collections::HashSet::new();
                for x in v.iter().cloned() {
                    s.insert(x);
                }
                s.len()
            },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen,
            |v| {
                let mut s = tinyset::SetU64::new();
                for x in v.iter().cloned() {
                    s.insert(x);
                }
                s.len()
            },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen,
            |v| {
                let mut s = tinyset::Set64::new();
                for x in v.iter().cloned() {
                    s.insert(x);
                }
                s.len()
            },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut gen,
            |v| {
                let mut s = std::collections::HashSet::new();
                for x in v.iter().cloned() {
                    s.insert(x);
                }
                s.len()
            },
            20
        )
        .scaling,
        bench_scaling_gen(
            &mut genusize,
            |v| {
                let mut s = id_set::IdSet::new();
                for x in v.iter().cloned() {
                    s.insert(x);
                }
                s.len()
            },
            20
        )
        .scaling,
    );
}

fn format_sz(sz: f64) -> String {
    if sz >= 1e4 {
        format!("{:.0}k", sz / 1000.0)
    } else {
        format!("{:.0}", sz)
    }
}

fn bench_funcs<O>(
    name: &str,
    density: f64,
    func32: impl Copy + Fn(&mut tinyset::SetU32) -> O,
    funcroaring: impl Copy + Fn(&mut roaring::RoaringBitmap) -> O,
    funchash32: impl Copy + Fn(&mut std::collections::HashSet<u32>) -> O,
    func64: impl Copy + Fn(&mut tinyset::SetU64) -> O,
    oldtiny: impl Copy + Fn(&mut tinyset::Set64<u64>) -> O,
    funchash: impl Copy + Fn(&mut std::collections::HashSet<u64>) -> O,
    idset: impl Copy + Fn(&mut id_set::IdSet) -> O,
) {
    assert!(density <= 1.0);
    println!(
        "\n{:<5}{:5}:{:>11} {:>13} {:>13} {:>13} {:>13} {:>13} {:>13}",
        name, density, "setu32", "roaring", "std32", "setu64", "set64", "std64", "id-set"
    );
    for &sz in SIZES.iter() {
        let gen = move || {
            let mut rng = rand::thread_rng();
            let mx = (sz as f64 / density) as u64 + 1;
            let mut vec = Vec::new();
            while vec.iter().cloned().collect::<tinyset::SetU64>().len() < sz {
                vec.push(rng.gen_range(0..mx));
            }
            vec
        };
        println!("{:>9}:{:7.0}ns/{:<4} {:6.0}ns/{:<4} {:7.0}ns/{:<4} {:6.0}ns/{:<4} {:6.0}ns/{:<4} {:6.0}ns/{:<4} {:6.0}ns/{:<4}",
                 sz,
                 bench_gen_env(|| { gen().iter().cloned()
                                    .map(|x| x as u32).collect::<tinyset::SetU32>() },
                               func32).ns_per_iter,
                 format_sz((0..100).map(|_| mem_used(|| gen().iter().cloned()
                                                     .map(|x| x as u32)
                                                     .collect::<tinyset::SetU32>()).1)
                           .sum::<usize>() as f64/100.0),
                 bench_gen_env(|| { gen().iter().cloned()
                                    .map(|x| x as u32).collect::<roaring::RoaringBitmap>() },
                               funcroaring).ns_per_iter,
                 format_sz((0..100).map(|_| mem_used(|| gen().iter().cloned()
                                                     .map(|x| x as u32)
                                                     .collect::<std::collections::HashSet<_>>()).1)
                           .sum::<usize>() as f64/100.0),
                 bench_gen_env(|| { gen().iter().cloned()
                                    .map(|x| x as u32)
                                    .collect::<std::collections::HashSet<_>>() },
                               funchash32).ns_per_iter,
                 format_sz((0..100).map(|_| mem_used(|| gen().iter().cloned()
                                                     .map(|x| x as u32)
                                                     .collect::<std::collections::HashSet<_>>()).1)
                           .sum::<usize>() as f64/100.0),
                 bench_gen_env(|| { gen().iter().cloned().collect::<tinyset::SetU64>() },
                               func64).ns_per_iter,
                 format_sz((0..100).map(|_| mem_used(|| gen().iter().cloned()
                                                     .collect::<tinyset::SetU64>()).1)
                           .sum::<usize>() as f64/100.0),
                 bench_gen_env(|| { gen().iter().cloned().collect::<tinyset::Set64<_>>() },
                               oldtiny).ns_per_iter,
                 format_sz((0..100).map(|_| mem_used(|| gen().iter().cloned()
                                                     .collect::<tinyset::Set64<_>>()).1)
                           .sum::<usize>() as f64/100.0),
                 bench_gen_env(|| { gen().iter().cloned().collect::<std::collections::HashSet<_>>() },
                               funchash).ns_per_iter,
                 format_sz((0..100).map(|_| mem_used(|| gen().iter().cloned()
                                                     .collect::<std::collections::HashSet<_>>()).1)
                           .sum::<usize>() as f64/100.0),
                 bench_gen_env(|| { gen().iter().cloned()
                                    .map(|x| x as usize)
                                    .collect::<id_set::IdSet>() },
                               idset).ns_per_iter,
                 format_sz((0..100).map(|_| mem_used(|| gen().iter().cloned()
                                                     .map(|x| x as usize)
                                                     .collect::<id_set::IdSet>()).1)
                           .sum::<usize>() as f64/100.0),
        );
    }
    let gen = move |sz| {
        let mut rng = rand::thread_rng();
        let mx = (sz as f64 / density) as u64 + 1;
        let mut vec = Vec::new();
        while vec.iter().cloned().collect::<tinyset::SetU64>().len() < sz {
            vec.push(rng.gen_range(0..mx));
        }
        vec
    };
    println!(
        "{:>10}: {:13.0} {:13.0} {:13.0} {:13.0} {:13.0} {:13.0}",
        "scaling",
        bench_scaling_gen(
            |sz| {
                gen(sz)
                    .iter()
                    .cloned()
                    .map(|x| x as u32)
                    .collect::<tinyset::SetU32>()
            },
            func32,
            10
        )
        .scaling,
        bench_scaling_gen(
            |sz| {
                gen(sz)
                    .iter()
                    .cloned()
                    .map(|x| x as u32)
                    .collect::<roaring::RoaringBitmap>()
            },
            funcroaring,
            10
        )
        .scaling,
        bench_scaling_gen(
            |sz| {
                gen(sz)
                    .iter()
                    .map(|&x| x as u32)
                    .collect::<std::collections::HashSet<_>>()
            },
            funchash32,
            10
        )
        .scaling,
        bench_scaling_gen(
            |sz| { gen(sz).iter().cloned().collect::<tinyset::SetU64>() },
            func64,
            10
        )
        .scaling,
        bench_scaling_gen(
            |sz| { gen(sz).iter().cloned().collect::<tinyset::Set64<u64>>() },
            oldtiny,
            10
        )
        .scaling,
        bench_scaling_gen(
            |sz| {
                gen(sz)
                    .iter()
                    .cloned()
                    .collect::<std::collections::HashSet<_>>()
            },
            funchash,
            10
        )
        .scaling,
    );
}

fn bench_min(density: f64) {
    bench_funcs(
        "min",
        density,
        |s| s.iter().min().map(|x| x as u64),
        |s| s.iter().min().map(|x| x as u64),
        |s| s.iter().min().map(|x| *x as u64),
        |s| s.iter().min(),
        |s| s.iter().min(),
        |s| s.iter().cloned().min(),
        |s| s.iter().min().map(|x| x as u64),
    );
}
fn bench_max(density: f64) {
    bench_funcs(
        "max",
        density,
        |s| s.iter().max().map(|x| x as u64),
        |s| s.iter().max().map(|x| x as u64),
        |s| s.iter().max().map(|x| *x as u64),
        |s| s.iter().max(),
        |s| s.iter().max(),
        |s| s.iter().cloned().max(),
        |s| s.iter().max().map(|x| x as u64),
    );
}
fn bench_last(density: f64) {
    bench_funcs(
        "last",
        density,
        |s| s.iter().last().map(|x| x as u64),
        |s| s.iter().last().map(|x| x as u64),
        |s| s.iter().last().map(|x| *x as u64),
        |s| s.iter().last(),
        |s| s.iter().last(),
        |s| s.iter().cloned().last(),
        |s| s.iter().last().map(|x| x as u64),
    );
}

fn bench_sum(density: f64) {
    bench_funcs(
        "sum",
        density,
        |s| s.iter().sum::<u32>() as u64,
        |s| s.iter().sum::<u32>() as u64,
        |s| s.iter().sum::<u32>() as u64,
        |s| s.iter().sum::<u64>(),
        |s| s.iter().sum::<u64>(),
        |s| s.iter().cloned().sum::<u64>(),
        |s| s.iter().sum::<usize>() as u64,
    );
}

fn bench_scaling(density: f64, min: usize) {
    assert!(density <= 1.0);
    let mut gen = move |num_elements| {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u64 + 1;
        let mut set = tinyset::SetU64::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        (rng.gen_range(0..mx), set)
    };
    let mut gen_hashset = move |num_elements| {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u64 + 1;
        let mut set = std::collections::HashSet::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        (rng.gen_range(0..mx), set)
    };
    let mut gen_tiny = move |num_elements| {
        let mut rng = rand::thread_rng();
        let mx = (num_elements as f64 / density) as u64 + 1;
        let mut set = tinyset::Set64::new();
        while set.len() < num_elements {
            set.insert(rng.gen_range(0..mx));
        }
        (rng.gen_range(0..mx), set)
    };

    println!(
        "\n{},      {:5}-: {:>13} {:>13} {:>13}",
        density, min, "this", "set64", "std"
    );
    println!(
        "{:>18}: {:5.0} {:5.0} {:5.0}",
        ".len()",
        bench_scaling_gen(&mut gen, |(_, set)| { set.len() }, min).scaling,
        bench_scaling_gen(&mut gen_tiny, |(_, set)| { set.len() }, min).scaling,
        bench_scaling_gen(&mut gen_hashset, |(_, set)| { set.len() }, min).scaling,
    );
    println!(
        "{:>18}: {:5.0} {:5.0} {:5.0}",
        ".contains(ran)",
        bench_scaling_gen(gen, |(idx, set)| { set.contains(*idx) }, min).scaling,
        bench_scaling_gen(gen_tiny, |(idx, set)| { set.contains(*idx) }, min).scaling,
        bench_scaling_gen(gen_hashset, |(idx, set)| { set.contains(idx) }, min).scaling,
    );
    println!(
        "{:>18}: {:5.0} {:5.0} {:5.0}",
        ".remove(ran)",
        bench_scaling_gen(gen, |(idx, set)| { set.remove(*idx) }, min).scaling,
        bench_scaling_gen(gen_tiny, |(idx, set)| { set.remove(idx) }, min).scaling,
        bench_scaling_gen(gen_hashset, |(idx, set)| { set.remove(idx) }, min).scaling,
    );
    println!(
        "{:>18}: {:5.0} {:5.0} {:5.0}",
        ".insert(ran)",
        bench_scaling_gen(gen, |(idx, set)| { set.insert(*idx) }, min).scaling,
        bench_scaling_gen(gen_tiny, |(idx, set)| { set.insert(*idx) }, min).scaling,
        bench_scaling_gen(gen_hashset, |(idx, set)| { set.insert(*idx) }, min).scaling,
    );
}

fn main() {
    // let mut s = tinyset::SetU32::new();
    // while s.len() < 10000 {
    //     s.insert(rand::random::<u32>() % 15000);
    // }
    // assert_eq!(s.len(), 5);
    for &sz in SIZES.iter() {
        bench_sets(0.8, sz);
    }

    for f in [0.001, 0.05, 0.8].iter().cloned() {
        bench_fill_with_inserts(f);
        bench_collect(f);
    }

    for &sz in SIZES.iter() {
        bench_sets(0.001, sz);
    }
    for &sz in SIZES.iter() {
        bench_sets(0.05, sz);
    }
    for &sz in SIZES.iter() {
        bench_sets(0.5, sz);
    }

    bench_max(0.001);
    bench_max(0.05);
    bench_max(0.8);

    bench_min(0.001);
    bench_min(0.05);
    bench_min(0.8);

    bench_sum(0.001);
    bench_sum(0.05);
    bench_sum(0.8);

    bench_last(0.001);
    bench_last(0.05);
    bench_last(0.8);

    bench_scaling(0.05, 8);
    bench_scaling(0.5, 8);
    bench_scaling(0.8, 8);

    use tinyset::Fits64;
    println!(
        "i64<->u64: {}",
        bench_gen_env(
            || rand::random::<i64>(),
            |x| unsafe { i64::from_u64(x.to_u64()) }
        )
        .ns_per_iter
    );
    println!(
        "i32<->u64: {}",
        bench_gen_env(
            || rand::random::<i8>(),
            |x| unsafe { i32::from_u64(x.to_u64()) }
        )
        .ns_per_iter
    );
    println!(
        "i16<->u64: {}",
        bench_gen_env(
            || rand::random::<i8>(),
            |x| unsafe { i16::from_u64(x.to_u64()) }
        )
        .ns_per_iter
    );
    println!(
        " i8<->u64: {}",
        bench_gen_env(
            || rand::random::<i8>(),
            |x| unsafe { i8::from_u64(x.to_u64()) }
        )
        .ns_per_iter
    );
}
