#[macro_use]
extern crate criterion;
#[macro_use]
extern crate lazy_static;
extern crate lab;
extern crate rand;

use criterion::Criterion;
use rand::distributions::Standard;
use rand::Rng;

lazy_static! {
    static ref LABS: Vec<lab::Lab> = {
        let rand_seed = [0u8; 32];
        let rng: rand::rngs::StdRng = rand::SeedableRng::from_seed(rand_seed);
        let labs: Vec<[f32; 8]> = rng.sample_iter(&Standard).take(512).collect();
        labs.iter()
            .map(|lab| lab::Lab {
                l: lab[0],
                a: lab[1],
                b: lab[2],
            })
            .collect()
    };
}

fn labs_to_rgbs(c: &mut Criterion) {
    c.bench_function("[Lab] -> [RGB]", move |b| {
        b.iter(|| lab::__scalar::labs_to_rgbs(&LABS))
    });
}

fn labs_to_rgb_bytes(c: &mut Criterion) {
    c.bench_function("[Lab] -> [u8]", move |b| {
        b.iter(|| lab::__scalar::labs_to_rgb_bytes(&LABS))
    });
}

fn labs_to_rgbs_simd(c: &mut Criterion) {
    c.bench_function("[Lab] -> [RGB] (simd)", move |b| {
        b.iter(|| lab::labs_to_rgbs(&LABS))
    });
}

fn labs_to_rgb_bytes_simd(c: &mut Criterion) {
    c.bench_function("[Lab] -> [u8] (simd)", move |b| {
        b.iter(|| lab::labs_to_rgb_bytes(&LABS))
    });
}

criterion_group!(
    benches,
    labs_to_rgbs,
    labs_to_rgb_bytes,
    labs_to_rgbs_simd,
    labs_to_rgb_bytes_simd
);
criterion_main!(benches);
