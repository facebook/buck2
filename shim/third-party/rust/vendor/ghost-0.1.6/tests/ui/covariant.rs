use ghost::phantom;

#[phantom]
struct CovariantPhantom<T>;

fn require_covariant<'a>(phantom: CovariantPhantom<&'static str>) -> CovariantPhantom<&'a str> {
    phantom
}

fn require_contravariant<'a>(phantom: CovariantPhantom<&'a str>) -> CovariantPhantom<&'static str> {
    phantom
}

fn main() {}
