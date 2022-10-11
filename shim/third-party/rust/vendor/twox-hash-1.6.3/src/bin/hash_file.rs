use std::env;
use std::fs::File;
use std::hash::Hasher;
use std::io::{BufRead, BufReader};
use twox_hash::XxHash64;

fn main() {
    for arg in env::args().skip(1) {
        let f = File::open(&arg).unwrap();
        let mut f = BufReader::new(f);

        let mut hasher = XxHash64::with_seed(0);

        loop {
            let consumed = {
                let bytes = f.fill_buf().unwrap();
                if bytes.is_empty() {
                    break;
                }
                hasher.write(bytes);
                bytes.len()
            };
            f.consume(consumed);
        }

        println!("{:16x}   {}", hasher.finish(), arg);
    }
}
