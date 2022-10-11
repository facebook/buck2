use std::path::PathBuf;

use criterion::{Criterion, criterion_group, criterion_main};

mod finl_test {
    use finl_unicode::categories::CharacterCategories;

    #[inline]
    pub fn letter_test(c: &char) -> bool {
        c.is_letter()
    }

    #[inline]
    pub fn lc_test(c: &char) -> bool {
        c.is_letter_lowercase()
    }
}

mod uc_test {
    use unicode_categories::UnicodeCategories;

    #[inline]
    pub fn letter_test(c: &char) -> bool {
        c.is_letter()
    }

    #[inline]
    pub fn lc_test(c: &char) -> bool {
        c.is_letter_lowercase()
    }
}

pub fn text_benchmark(c: &mut Criterion, source_file_name: &str, group_name: &str, do_lowercase: bool) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("resources");
    path.push("benchmark-texts");
    path.push(source_file_name);

    let input_text = std::fs::read_to_string(path).unwrap();
    let mut group = c.benchmark_group(group_name);
    group.bench_function("finl_unicode",
                         |b| b.iter(|| {
                             input_text.chars().filter(|c| finl_test::letter_test(c)).count();
                         }),
    );
    group.bench_function("unicode_categories",
                         |b| b.iter(|| {
                             input_text.chars().filter(|c| uc_test::letter_test(c)).count();
                         }),
    );
    group.finish();

    if do_lowercase {
        let mut group_name = group_name.to_string();
        group_name.push_str(" for lowercase");
        let mut group = c.benchmark_group(group_name);
        group.bench_function("finl_unicode",
                             |b| b.iter(|| {
                                 input_text.chars().filter(|c| finl_test::lc_test(c)).count();
                             }),
        );
        group.bench_function("unicode_categories",
                             |b| b.iter(|| {
                                 input_text.chars().filter(|c| uc_test::lc_test(c)).count();
                             }),
        );
        group.finish();
    }
}

pub fn criterion_benchmark(c: &mut Criterion) {
    text_benchmark(c, "35018-0.txt", "Process Japanese text file", false);
    text_benchmark(c, "59765-0.txt", "Proces Czech text file", true);
    text_benchmark(c, "84-0.txt", "Process English text file", true);
    text_benchmark(c, "source_code.txt", "Process source code", true);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);