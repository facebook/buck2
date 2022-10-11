# finl Unicode support

This crate is designed for the Unicode needs of the finl project, but is designed to be usable by other software as well.
In the current release (1.0.x), support is provided for character code identification and grapheme segmentation and Unicode14.0.0.

## Overview 

### Category identification

Loading the `finl_unicode` crate with the `categories` feature will add methods onto the char type to test the category of a character
or identify its category. See the rustdoc for detail.

### Grapheme clusters

Loading the `finl_unicode` crate with the `grapheme_clusters` feature will extend `Peekable<CharIndices>` to have a `next_cluster()` method which will return the next grapheme cluster from the iterator.
There is also a pure cluster iterator available by calling `Graphemes::new(s)` on a `&str`. I don’t use this in finl, but wrote it using the same algorithm as the extension of `Peekable<CharIndices>` for the purposes of benchmarking.¹

## Why?

There *are* existing crates for these purposes, but segmentation lacked the interface for segmentation that I wanted (which was to be able to extend `Peekable<CharIndices>` with a method to fetch the next grapheme cluster if it existed). 
I incorrectly assumed that this would require character code identification, which turned out to be incorrect, but it turned out that the crate I was using was outdated and possibly abandoned and had an inefficient algorithm so it turned out to be a good thing that I wrote it.
I did benchmarks comparing my code against existing crates and discovered that I had managed to eke out performance gains against all of them, so that’s an added bonus.

###  Benchmark results

All benchmarks are generated using Criterion You can replicate them by running `cargo bench` from the project directory. Three numbers are given for all results: low/mean/high, all from the output of Criterion. The mean value is given in **bold**. 

#### Unicode categories
I ran three benchmarks to compare the performance of the crates. 
The Japanese text benchmark reads the Project Gutenberg EBook of *Kumogata monsho* by John Falkner and counts the characters in it which are Unicode letters.
The Czech text benchmark reads the Project Gutenberg EBook of *Cítanka pro skoly obecné* by Jan Stastný and Jan Lepar and Josef Sokol (this was to exercise testing against a Latin-alphabet text with lots of diacriticals). 
All letters and lowercase letters are counted.
The English text benchmark reads the Project Gutenberg eBook of *Frankenstein* by Mary Wollstonecraft Shelley (to run against a text which is pure ASCII).
All letters and lowercase letters are counted. The source code check is from neovim. Again, letters and lowercase letters are counted in the sample.

I compared against [unicode_categories](https://docs.rs/unicode_categories/latest/unicode_categories/) 0.1.1. All times are in ms. Smaller is better.

| Benchmark                | `finl_unicode`              | `unicode_categories`     |
|--------------------------|-----------------------------|--------------------------|
| Japanese text            | 0.62484/**0.64200**/0.66311 | 15.382/**15.719**/16.092 |
| Czech text               | 0.18248/**0.19137**/0.19975 | 3.2322/**3.3329**/3.4435 |
| Czech text (lowercase)   | 0.20361/**0.20529**/0.20724 | 1.8496/**1.8742**/1.9026 |
| English text             | 0.52260/**0.54461**/0.56682 | 13.038/**13.330**/13.655 |
| English text (lowercase) | 0.72885/**0.74219**/0.75668 | 8.3998/**8.5037**/8.6233 |
| Source code              | 0.05544/**0.05785**/0.06046 | 1.6512/**1.7063**/1.7656 |
| Source code (lowercase)  | 0.07506/**0.07673**/0.07895 | 0.7285/**0.7536**/0.7821 | 

As you can see, this is a clear win (the difference is the choice of algorithm. `finl_unicode` uses two-step table lookup to be able to store categories compactly while `unicode_categories` uses a combination of range checks and binary searches on tables).

#### Grapheme clusters

I compared against [unicode_segmentation](https://docs.rs/unicode-segmentation/latest/unicode_segmentation/) 1.9.0 (part of the unicode-rs project) and [bstr](https://docs.rs/bstr/latest/bstr/) 1.0.0. 
Comparisons are run against graphemes.txt, derived from the Unicode test suite, plus several language
texts that were part of the `unicode_segmentation` benchmark suite. 

All times are in µs, smaller is better.

| Benchmark        | `finl_unicde`            | `unicode_segmentation`   | `bstr`                   |
|------------------|--------------------------|--------------------------|--------------------------|
| Unicode graphemes | 130.34/**133.31**/137.00 | 209.51/**217.50**/225.53 | 337.68/**354.59**/372.75 |
| Arabic text      | 262.05/**268.78**/273.65 | 443.11/**463.19**/482.25 | 842.78/**872.47**/906.84 |
| English text     | 387.88/**395.08**/404.00 | 527.29/**552.92**/586.04 | 424.73/**437.04**/449.23 |
| Hindi text       | 204.88/**216.04**/228.14 | 489.75/**500.55**/512.20 | 638.01/**641.28**/644.87 |
| Japanese text    | 181.65/**190.87**/202.92 | 437.98/**451.51**/467.17 | 855.04/**880.48**/904.88 |
| Korean text      | 298.19/**304.42**/312.47 | 813.45/**844.54**/880.53 | 1259.2/**1304.7**/1350.6 |
| Mandarin text    | 154.55/**159.33**/164.22 | 284.59/**293.63**/306.59 | 679.67/**704.13**/730.46 |
| Russian text     | 300.56/**312.86**/327.44 | 372.59/**392.12**/419.40 | 783.41/**838.96**/896.44 |
| Source code      | 424.39/**443.88**/463.77 | 501.16/**506.81**/513.27 | 513.79/**531.82**/551.31 |

Adding some additional tests reveals some interesting contrasts in performance. On text with minimal
clustering (English and source code), my code is faster than `unicode_segmentation` and `bstr` (but not dramatically so) and it's
interesting to see that `bstr` is slightly faster than `unicode_segmentation` on the English text benchmark,
but where grapheme clusters become more common (Arabic and Hindi), the performance is dramatically better 
with my crate. I wouldn’t expect clusters in the Japanese, but it and Korean show the most dramatic
differences in performance.

## Why not?

You may want to avoid this if you need `no_std` (maybe I’ll cover that in a future version, but probably not). 
If you need other clustering algorithms, I have no near future plans to implement them (but I would do it for money). 

There is no equivalent to `unicode_segmentation`’s `GraphemeCursor` as I don’t need that functionality 
for finl. Reverse iteration over graphemes is not supported, nor do I have plans to support it.

I do not support legacy clustering algorithms which are supported by `unicode-segmentation`. However, the Unicode
specification discourages the use of legacy clustering which is only documented for backwards compatability with very old versions of the Unicode standard.²


## Unicode copyright notice

This package incorporates data from Unicode Inc.
Copyright © 1991–2022 Unicode, Inc. All rights reserved.

## Support

I’ve released this under an MIT/Apache license. Do what you like with it. 
I wouldn’t mind contributions to the ongoing support of developing finl, but they’re not necessary (although if you’re Microsoft or Google and you use my code, surely you can throw some dollars in my bank account).
I guarantee no warranty or support, although if you care to throw some money my way, I can prioritize your requests.

## Version history

- **1.0.0** Initial release
- **1.0.1** Build-process changes to make docs.rs documentation build
- **1.0.2** More changes because the first round apparently weren’t enough
- **1.1.0** Add support for Unicode 15.0.0, added new benchmark comparisons.

---

1. For technical reasons, the iterator extension returns `Option<String>` rather than `Option<&str>` and thus will autmoatically underperform other implementations which are returning *all* the grapheme clusters. 
For finl, however, I would need an owned value for the string containing the cluster anyway and since I only occasionally need a cluster, I decided it was acceptable to take the performance hit. 
But see the benchmark results for the fact that I apparently managed to implement a faster algorithm anyway when doing an apples-to-apples comparison of speeds. 
2. Pure speculation, but I think that this might be the entire reason for the difference in performance between `finl_unicode` and `unicode_segmentation`. However, I have not looked at the source code to confirm my suspicion.