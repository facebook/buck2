// This is a buggy quick sort implementation, QuickCheck will find the bug for
// you.

extern crate quickcheck;

use quickcheck::quickcheck;

fn smaller_than<T: Clone + Ord>(xs: &[T], pivot: &T) -> Vec<T> {
    return xs.iter().filter(|&x| *x < *pivot).map(|x| x.clone()).collect();
}

fn larger_than<T: Clone + Ord>(xs: &[T], pivot: &T) -> Vec<T> {
    return xs.iter().filter(|&x| *x > *pivot).map(|x| x.clone()).collect();
}

fn sortk<T: Clone + Ord>(x: &T, xs: &[T]) -> Vec<T> {
    let mut result: Vec<T> = sort(&*smaller_than(xs, x));
    let last_part = sort(&*larger_than(xs, x));
    result.push(x.clone());
    result.extend(last_part.iter().map(|x| x.clone()));
    result
}

fn sort<T: Clone + Ord>(list: &[T]) -> Vec<T> {
    if list.is_empty() {
        vec![]
    } else {
        sortk(&list[0], &list[1..])
    }
}

fn main() {
    fn is_sorted(xs: Vec<isize>) -> bool {
        for win in xs.windows(2) {
            if win[0] > win[1] {
                return false;
            }
        }
        true
    }

    fn keeps_length(xs: Vec<isize>) -> bool {
        xs.len() == sort(&*xs).len()
    }
    quickcheck(keeps_length as fn(Vec<isize>) -> bool);

    quickcheck(is_sorted as fn(Vec<isize>) -> bool)
}
