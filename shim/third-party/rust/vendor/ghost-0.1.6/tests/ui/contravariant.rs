use ghost::phantom;

#[phantom]
struct ContravariantPhantom<#[contra] T>;

fn require_covariant<'a>(phantom: ContravariantPhantom<&'static str>) -> ContravariantPhantom<&'a str> {
    phantom
}

fn require_contravariant<'a>(phantom: ContravariantPhantom<&'a str>) -> ContravariantPhantom<&'static str> {
    phantom
}

fn main() {}
