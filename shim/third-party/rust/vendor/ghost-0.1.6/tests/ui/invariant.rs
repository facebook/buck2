use ghost::phantom;

#[phantom]
struct InvariantPhantom<#[invariant] T>;

fn require_covariant<'a>(phantom: InvariantPhantom<&'static str>) -> InvariantPhantom<&'a str> {
    phantom
}

fn require_contravariant<'a>(phantom: InvariantPhantom<&'a str>) -> InvariantPhantom<&'static str> {
    phantom
}

fn main() {}
