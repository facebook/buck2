extern crate quickcheck;

use quickcheck::quickcheck;

fn sieve(n: usize) -> Vec<usize> {
    if n <= 1 {
        return vec![];
    }

    let mut marked = vec![false; n + 1];
    marked[0] = true;
    marked[1] = true;
    marked[2] = true;
    for p in 2..n {
        for i in (2 * p..n).filter(|&n| n % p == 0) {
            marked[i] = true;
        }
    }
    marked
        .iter()
        .enumerate()
        .filter_map(|(i, &m)| if m { None } else { Some(i) })
        .collect()
}

fn is_prime(n: usize) -> bool {
    n != 0 && n != 1 && (2..).take_while(|i| i * i <= n).all(|i| n % i != 0)
}

fn main() {
    fn prop_all_prime(n: usize) -> bool {
        sieve(n).into_iter().all(is_prime)
    }

    fn prop_prime_iff_in_the_sieve(n: usize) -> bool {
        sieve(n) == (0..(n + 1)).filter(|&i| is_prime(i)).collect::<Vec<_>>()
    }

    quickcheck(prop_all_prime as fn(usize) -> bool);
    quickcheck(prop_prime_iff_in_the_sieve as fn(usize) -> bool);
}
