use std::path::PathBuf;
use criterion::{criterion_group, criterion_main, Criterion};

mod finl_test {
    use finl_unicode::grapheme_clusters::Graphemes;

    pub fn read_clusters(input: &String) -> usize {
        let mut cnt = 0;
        Graphemes::new(input).for_each(
            |c| {
                if c.len() == 1 {
                    cnt += 1;
                }
            }
        );
        cnt
    }
}

mod unicode_rs {
   use unicode_segmentation::UnicodeSegmentation;

    pub fn read_clusters(input: &String) -> usize {
        let mut cnt = 0;
        input.graphemes(true).for_each(
            |c| {
                if c.len() == 1 {
                    cnt += 1;
                }
            }
        );
        cnt
    }
}

mod bstr {
    use bstr::ByteSlice;

    pub fn read_clusters(input: &String) -> usize {
        let mut cnt = 0;
        input.as_bytes().graphemes().for_each(
            |c| {
                if c.len() == 1 {
                    cnt += 1;
                }
            }
        );
        cnt
    }
}

fn text_benchmark(c: &mut Criterion, source_file_name: &str, group_name: &str) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("resources");
    path.push("benchmark-texts");
    path.push(source_file_name);
    let input_text = std::fs::read_to_string(path).unwrap();
    let mut group = c.benchmark_group(group_name);
    group.bench_function("finl_unicode",
                         |b| b.iter(|| {
                             finl_test::read_clusters(&input_text);
                         })
    );

    group.bench_function("unicode-rs",
                         |b| b.iter(|| {
                             unicode_rs::read_clusters(&input_text);
                         })
    );

    group.bench_function("bstr",
                         |b| b.iter(|| {
                             bstr::read_clusters(&input_text);
                         })
    );
}

pub fn criterion_benchmark(c: &mut Criterion) {
    text_benchmark(c, "graphemes.txt", "Unicode grapheme test text");
    text_benchmark(c, "arabic.txt", "Arabic text");
    text_benchmark(c, "english.txt", "English text");
    text_benchmark(c, "hindi.txt", "Hindi text");
    text_benchmark(c, "japanese.txt", "Japanese text");
    text_benchmark(c, "korean.txt", "Korean text");
    text_benchmark(c, "mandarin.txt", "Mandarin text");
    text_benchmark(c, "russian.txt", "Russian text");
    text_benchmark(c, "source_code.txt", "Source code text");
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);