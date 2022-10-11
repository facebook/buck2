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
    static ref RGBS: Vec<[u8; 3]> = {
        let rand_seed = [0u8; 32];
        let rng: rand::rngs::StdRng = rand::SeedableRng::from_seed(rand_seed);
        rng.sample_iter(&Standard).take(512).collect()
    };
    static ref RGBS_FLAT: Vec<u8> =
        RGBS.iter()
            .fold(Vec::with_capacity(RGBS.len() * 3), |mut acc, rgb| {
                acc.extend_from_slice(rgb);
                acc
            });
}

fn rgbs_to_labs(c: &mut Criterion) {
    c.bench_function("[RGB] -> [Lab]", move |b| {
        b.iter(|| lab::__scalar::rgbs_to_labs(&RGBS))
    });
}

fn rgb_bytes_to_labs(c: &mut Criterion) {
    c.bench_function("[u8] -> [Lab]", move |b| {
        b.iter(|| lab::__scalar::rgb_bytes_to_labs(&RGBS_FLAT))
    });
}

fn rgbs_to_labs_simd(c: &mut Criterion) {
    c.bench_function("[RGB] -> [Lab] (simd)", move |b| {
        b.iter(|| lab::rgbs_to_labs(&RGBS))
    });
}

fn rgb_bytes_to_labs_simd(c: &mut Criterion) {
    c.bench_function("[u8] -> [Lab] (simd)", move |b| {
        b.iter(|| lab::rgb_bytes_to_labs(&RGBS_FLAT))
    });
}

criterion_group!(
    benches,
    rgbs_to_labs,
    rgb_bytes_to_labs,
    rgbs_to_labs_simd,
    rgb_bytes_to_labs_simd
);
criterion_main!(benches);
