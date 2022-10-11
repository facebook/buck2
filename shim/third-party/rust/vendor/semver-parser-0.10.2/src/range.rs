use crate::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Range {
    pub comparator_set: Vec<Comparator>,
    pub compat: range_set::Compat,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Comparator {
    pub op: Op,
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub pre: Vec<Identifier>,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Op {
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Identifier {
    Numeric(u64),
    AlphaNumeric(String),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Partial {
    major: Option<u64>,
    minor: Option<u64>,
    patch: Option<u64>,
    pre: Vec<Identifier>,
    kind: PartialKind,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum PartialKind {
    XRangeOnly,
    MajorOnly,
    MajorMinor,
    MajorMinorPatch,
}

impl Partial {
    pub fn new() -> Self {
        Self {
            major: None,
            minor: None,
            patch: None,
            pre: Vec::new(),
            kind: PartialKind::XRangeOnly,
        }
    }

    pub fn as_comparator(&self, op: Op) -> Comparator {
        Comparator {
            op,
            major: self.major.unwrap_or(0),
            minor: self.minor.unwrap_or(0),
            patch: self.patch.unwrap_or(0),
            pre: self.pre.clone(),
        }
    }

    pub fn inc_major(&mut self) -> &mut Self {
        self.major = Some(self.major.unwrap_or(0) + 1);
        self
    }

    pub fn inc_minor(&mut self) -> &mut Self {
        self.minor = Some(self.minor.unwrap_or(0) + 1);
        self
    }

    pub fn inc_patch(&mut self) -> &mut Self {
        self.patch = Some(self.patch.unwrap_or(0) + 1);
        self
    }

    pub fn zero_missing(&mut self) -> &mut Self {
        self.major = Some(self.major.unwrap_or(0));
        self.minor = Some(self.minor.unwrap_or(0));
        self.patch = Some(self.patch.unwrap_or(0));
        self
    }

    pub fn zero_minor(&mut self) -> &mut Self {
        self.minor = Some(0);
        self
    }

    pub fn zero_patch(&mut self) -> &mut Self {
        self.patch = Some(0);
        self
    }

    pub fn no_pre(&mut self) -> &mut Self {
        self.pre = Vec::new();
        self
    }
}

pub fn from_pair_iterator(
    parsed_range: pest::iterators::Pair<'_, Rule>,
    compat: range_set::Compat,
) -> Result<Range, String> {
    // First of all, do we have the correct iterator?
    if parsed_range.as_rule() != Rule::range {
        return Err(String::from("Error parsing range"));
    }

    let mut comparator_set = Vec::new();

    // Now we need to parse each comparator set out of the range
    for record in parsed_range.into_inner() {
        match record.as_rule() {
            Rule::hyphen => {
                let mut hyphen_set = simple::from_hyphen_range(record)?;
                comparator_set.append(&mut hyphen_set);
            }
            Rule::simple => {
                let mut comparators = simple::from_pair_iterator(record, compat)?;
                comparator_set.append(&mut comparators);
            }
            Rule::empty => {
                comparator_set.push(Partial::new().zero_missing().as_comparator(Op::Gte));
            }
            _ => unreachable!(),
        }
    }

    Ok(Range {
        comparator_set,
        compat,
    })
}

pub mod simple {
    use super::*;

    pub fn from_pair_iterator(
        parsed_simple: pest::iterators::Pair<'_, Rule>,
        compat: range_set::Compat,
    ) -> Result<Vec<Comparator>, String> {
        // First of all, do we have the correct iterator?
        if parsed_simple.as_rule() != Rule::simple {
            return Err(String::from("Error parsing comparator set"));
        }

        let mut comparators = Vec::new();

        // Now we need to parse each comparator set out of the range
        for record in parsed_simple.into_inner() {
            match record.as_rule() {
                Rule::partial => {
                    let components: Vec<_> = record.into_inner().collect();

                    let mut partial = parse_partial(components);

                    match partial.kind {
                        PartialKind::XRangeOnly => {
                            // '*', 'x', 'X' --> ">=0.0.0"
                            comparators.push(partial.zero_missing().as_comparator(Op::Gte));
                        }
                        PartialKind::MajorOnly => {
                            // "1", "1.*", or "1.*.*" --> ">=1.0.0 <2.0.0"
                            // "1.*.3" == "1.*"
                            comparators.push(partial.clone().zero_missing().as_comparator(Op::Gte));
                            comparators
                                .push(partial.inc_major().zero_missing().as_comparator(Op::Lt));
                        }
                        PartialKind::MajorMinor => {
                            // "1.2" or "1.2.*" --> ">=1.2.0 <1.3.0"
                            comparators.push(partial.clone().zero_patch().as_comparator(Op::Gte));
                            comparators
                                .push(partial.inc_minor().zero_patch().as_comparator(Op::Lt));
                        }
                        PartialKind::MajorMinorPatch => {
                            match compat {
                                range_set::Compat::Npm => {
                                    // for node, "1.2.3" is "=1.2.3"
                                    comparators.push(partial.as_comparator(Op::Eq));
                                }
                                range_set::Compat::Cargo => {
                                    // for cargo, "1.2.3" is parsed as "^1.2.3"
                                    handle_caret_range(partial, &mut comparators);
                                }
                            }
                        }
                    }
                }
                Rule::primitive => {
                    let mut components: Vec<_> = record.into_inner().collect();
                    let op_component = components.remove(0);

                    let op = match op_component.as_str() {
                        "=" => Op::Eq,
                        "<" => Op::Lt,
                        "<=" => Op::Lte,
                        ">" => Op::Gt,
                        ">=" => Op::Gte,
                        _ => unreachable!(),
                    };

                    let partial_component = components.remove(0);
                    let components: Vec<_> = partial_component.into_inner().collect();
                    let mut partial = parse_partial(components);

                    // equal is different because it can be a range with 2 comparators
                    if op == Op::Eq {
                        match partial.kind {
                            PartialKind::XRangeOnly => {
                                // '=*' --> ">=0.0.0"
                                comparators.push(partial.zero_missing().as_comparator(Op::Gte));
                            }
                            PartialKind::MajorOnly => {
                                // "=1", "=1.*", or "=1.*.*" --> ">=1.0.0 <2.0.0"
                                comparators
                                    .push(partial.clone().zero_missing().as_comparator(Op::Gte));
                                comparators
                                    .push(partial.inc_major().zero_missing().as_comparator(Op::Lt));
                            }
                            PartialKind::MajorMinor => {
                                // "=1.2" or "=1.2.*" --> ">=1.2.0 <1.3.0"
                                comparators
                                    .push(partial.clone().zero_patch().as_comparator(Op::Gte));
                                comparators
                                    .push(partial.inc_minor().zero_patch().as_comparator(Op::Lt));
                            }
                            PartialKind::MajorMinorPatch => {
                                comparators.push(partial.as_comparator(Op::Eq));
                            }
                        }
                    } else {
                        match partial.kind {
                            PartialKind::XRangeOnly => {
                                match op {
                                    Op::Eq => comparators
                                        .push(partial.zero_missing().as_comparator(Op::Gte)),
                                    Op::Lt => comparators
                                        .push(partial.zero_missing().as_comparator(Op::Lt)),
                                    Op::Lte => comparators
                                        .push(partial.zero_missing().as_comparator(Op::Gte)),
                                    Op::Gt => comparators
                                        .push(partial.zero_missing().as_comparator(Op::Lt)),
                                    Op::Gte => comparators
                                        .push(partial.zero_missing().as_comparator(Op::Gte)),
                                }
                            }
                            PartialKind::MajorOnly => {
                                // ">1", "=1", etc.
                                // ">1.*.3" == ">1.*"
                                match op {
                                    Op::Lte => comparators.push(
                                        partial
                                            .inc_major()
                                            .zero_minor()
                                            .zero_patch()
                                            .as_comparator(Op::Lt),
                                    ),
                                    _ => comparators.push(partial.zero_missing().as_comparator(op)),
                                }
                            }
                            PartialKind::MajorMinor => {
                                // ">1.2", "<1.2.*", etc.
                                match op {
                                    Op::Lte => comparators.push(
                                        partial.inc_minor().zero_patch().as_comparator(Op::Lt),
                                    ),
                                    _ => comparators.push(partial.zero_patch().as_comparator(op)),
                                }
                            }
                            PartialKind::MajorMinorPatch => {
                                comparators.push(partial.as_comparator(op));
                            }
                        }
                    }
                }
                Rule::caret => {
                    let mut components: Vec<_> = record.into_inner().collect();

                    let partial_component = components.remove(0);
                    let components: Vec<_> = partial_component.into_inner().collect();
                    let partial = parse_partial(components);

                    handle_caret_range(partial, &mut comparators);
                }
                Rule::tilde => {
                    let mut components: Vec<_> = record.into_inner().collect();

                    let partial_component = components.remove(0);
                    let components: Vec<_> = partial_component.into_inner().collect();
                    let mut partial = parse_partial(components);

                    comparators.push(partial.clone().zero_missing().as_comparator(Op::Gte));

                    match partial.kind {
                        PartialKind::XRangeOnly => {
                            // "~*" --> ">=0.0.0"
                            // which has already been added, so nothing to do here
                        }
                        PartialKind::MajorOnly => {
                            // "~0" --> ">=0.0.0 <1.0.0"
                            comparators.push(
                                partial
                                    .inc_major()
                                    .zero_missing()
                                    .no_pre()
                                    .as_comparator(Op::Lt),
                            );
                        }
                        PartialKind::MajorMinor | PartialKind::MajorMinorPatch => {
                            // "~1.2" --> ">=1.2.0 <1.3.0"
                            // "~1.2.3" --> ">=1.2.3 <1.3.0"
                            comparators.push(
                                partial
                                    .inc_minor()
                                    .zero_patch()
                                    .no_pre()
                                    .as_comparator(Op::Lt),
                            );
                        }
                    }
                }
                _ => unreachable!(),
            }
        }

        Ok(comparators)
    }

    fn handle_caret_range(mut partial: Partial, comparators: &mut Vec<Comparator>) {
        // major version 0 is a special case for caret
        if partial.major == Some(0) {
            match partial.kind {
                PartialKind::XRangeOnly => unreachable!(),
                PartialKind::MajorOnly => {
                    // "^0", "^0.*" --> ">=0.0.0 <1.0.0"
                    comparators.push(partial.clone().zero_missing().as_comparator(Op::Gte));
                    comparators.push(
                        partial
                            .inc_major()
                            .zero_missing()
                            .no_pre()
                            .as_comparator(Op::Lt),
                    );
                }
                PartialKind::MajorMinor => {
                    // "^0.2", "^0.2.*" --> ">=0.2.0 <0.3.0"
                    comparators.push(partial.clone().zero_missing().as_comparator(Op::Gte));
                    comparators.push(
                        partial
                            .inc_minor()
                            .zero_patch()
                            .no_pre()
                            .as_comparator(Op::Lt),
                    );
                }
                PartialKind::MajorMinorPatch => {
                    if partial.minor == Some(0) {
                        // "^0.0.1" --> ">=0.0.1 <0.0.2"
                        comparators.push(partial.as_comparator(Op::Gte));
                        comparators.push(partial.inc_patch().no_pre().as_comparator(Op::Lt));
                    } else {
                        // "^0.2.3" --> ">=0.2.3 <0.3.0"
                        comparators.push(partial.as_comparator(Op::Gte));
                        comparators.push(
                            partial
                                .inc_minor()
                                .zero_patch()
                                .no_pre()
                                .as_comparator(Op::Lt),
                        );
                    }
                }
            }
        } else {
            match partial.kind {
                PartialKind::XRangeOnly => {
                    // "^*" --> ">=0.0.0"
                    comparators.push(partial.zero_missing().as_comparator(Op::Gte));
                }
                _ => {
                    // "^1", "^1.*" --> ">=1.0.0 <2.0.0"
                    // "^1.2", "^1.2.*" --> ">=1.2.0 <2.0.0"
                    // "^1.2.3" --> ">=1.2.3 <2.0.0"
                    comparators.push(partial.clone().zero_missing().as_comparator(Op::Gte));
                    comparators.push(
                        partial
                            .inc_major()
                            .zero_minor()
                            .zero_patch()
                            .no_pre()
                            .as_comparator(Op::Lt),
                    );
                }
            }
        }
    }

    pub fn from_hyphen_range(
        parsed_simple: pest::iterators::Pair<'_, Rule>,
    ) -> Result<Vec<Comparator>, String> {
        // First of all, do we have the correct iterator?
        if parsed_simple.as_rule() != Rule::hyphen {
            return Err(String::from("Error parsing comparator set"));
        }

        let mut comparators = Vec::new();

        // At this point, we have 2 partial records
        let mut records = parsed_simple.into_inner();

        let components1: Vec<_> = records.next().unwrap().into_inner().collect();
        let mut partial1 = parse_partial(components1);
        match partial1.kind {
            PartialKind::XRangeOnly => {
                // don't need to include this - the range will be limited by the 2nd part of hyphen
                // range
            }
            _ => comparators.push(partial1.zero_missing().as_comparator(Op::Gte)),
        }

        let components2: Vec<_> = records.next().unwrap().into_inner().collect();
        let mut partial2 = parse_partial(components2);

        match partial2.kind {
            PartialKind::XRangeOnly => {
                // only include this if the first part of the hyphen range was also '*'
                if partial1.kind == PartialKind::XRangeOnly {
                    comparators.push(partial2.zero_missing().as_comparator(Op::Gte));
                }
            }
            PartialKind::MajorOnly => {
                // "1.2.3 - 2" --> ">=1.2.3 <3.0.0"
                comparators.push(
                    partial2
                        .inc_major()
                        .zero_minor()
                        .zero_patch()
                        .as_comparator(Op::Lt),
                );
            }
            PartialKind::MajorMinor => {
                // "1.2.3 - 2.3.x" --> ">=1.2.3 <2.4.0"
                comparators.push(partial2.inc_minor().zero_patch().as_comparator(Op::Lt));
            }
            PartialKind::MajorMinorPatch => {
                // "1.2.3 - 2.3.4" --> ">=1.2.3 <=2.3.4"
                comparators.push(partial2.as_comparator(Op::Lte));
            }
        }

        Ok(comparators)
    }

    fn parse_partial(mut components: Vec<pest::iterators::Pair<'_, Rule>>) -> Partial {
        let mut partial = Partial::new();

        // there will be at least one component
        let one = components.remove(0);

        match one.as_rule() {
            Rule::xr => {
                let inner = one.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::xr_op => {
                        // for "*", ">=*", etc.
                        partial.major = None;
                        partial.kind = PartialKind::XRangeOnly;
                        // end the pattern here
                        return partial;
                    }
                    Rule::nr => {
                        partial.major = Some(inner.as_str().parse::<u64>().unwrap());
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }

        if components.is_empty() {
            // only the major has been given
            partial.kind = PartialKind::MajorOnly;
            return partial;
        } else {
            let two = components.remove(0);

            match two.as_rule() {
                Rule::xr => {
                    let inner = two.into_inner().next().unwrap();
                    match inner.as_rule() {
                        Rule::xr_op => {
                            partial.minor = None;
                            // only the major has been given, minor is xrange (ignore anything after)
                            partial.kind = PartialKind::MajorOnly;
                            return partial;
                        }
                        Rule::nr => {
                            partial.minor = Some(inner.as_str().parse::<u64>().unwrap());
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }
        }

        if components.is_empty() {
            // only major and minor have been given
            partial.kind = PartialKind::MajorMinor;
            return partial;
        } else {
            let three = components.remove(0);

            match three.as_rule() {
                Rule::xr => {
                    let inner = three.into_inner().next().unwrap();
                    match inner.as_rule() {
                        Rule::xr_op => {
                            partial.patch = None;
                            // only major and minor have been given, patch is xrange
                            partial.kind = PartialKind::MajorMinor;
                            return partial;
                        }
                        Rule::nr => {
                            partial.patch = Some(inner.as_str().parse::<u64>().unwrap());
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }
        }

        // at this point we at least have all three fields
        partial.kind = PartialKind::MajorMinorPatch;

        if !components.is_empty() {
            // there's only going to be one, let's move it out
            let pre = components.remove(0);
            // now we want to look at the inner bit, so that we don't have the leading -
            let mut pre: Vec<_> = pre.into_inner().collect();
            let pre = pre.remove(0);
            let pre = pre.as_str();

            // now we have all of the stuff in pre, so we split by . to get each bit
            for bit in pre.split('.') {
                let identifier = match bit.parse::<u64>() {
                    Ok(num) => Identifier::Numeric(num),
                    Err(_) => Identifier::AlphaNumeric(bit.to_string()),
                };

                partial.pre.push(identifier);
            }
        }

        partial
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    fn parse_range(input: &str) -> pest::iterators::Pair<'_, Rule> {
        match SemverParser::parse(Rule::range, input) {
            Ok(mut parsed) => match parsed.next() {
                Some(parsed) => parsed,
                None => panic!("Could not parse {}", input),
            },
            Err(e) => panic!("Parse error:\n{}", e),
        }
    }

    // macros to handle the test boilerplate

    macro_rules! range_tests {
        ( $( $name:ident: $value:expr, )* ) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_range) = $value;

                    let parsed_range = parse_range(input);
                    let range = from_pair_iterator(parsed_range, range_set::Compat::Cargo).expect("parsing failed");

                    // get the expected length from the input range
                    let num_comparators = range.comparator_set.len();
                    let expected_comparators = expected_range.comparator_set.len();
                    assert_eq!(expected_comparators, num_comparators, "expected number of comparators: {}, got: {}", expected_comparators, num_comparators);

                    assert_eq!(range, expected_range);
                }
             )*
        };
    }

    macro_rules! range_tests_nodecompat {
        ( $( $name:ident: $value:expr, )* ) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_range) = $value;

                    let parsed_range = parse_range(input);
                    let range = from_pair_iterator(parsed_range, range_set::Compat::Npm).expect("parsing failed");

                    // get the expected length from the input range
                    let num_comparators = range.comparator_set.len();
                    let expected_comparators = expected_range.comparator_set.len();
                    assert_eq!(expected_comparators, num_comparators, "expected number of comparators: {}, got: {}", expected_comparators, num_comparators);

                    assert_eq!(range, expected_range);
                }
             )*
        };
    }

    macro_rules! comp_sets {
        ( $( [$op:expr, $major:expr, $minor:expr, $patch:expr] ),* ) => {
            Range {
                comparator_set: vec![
                    $(
                        Comparator {
                            op: $op,
                            major: $major,
                            minor: $minor,
                            patch: $patch,
                            pre: pre!(None),
                        },
                    )*
                ],
                compat: range_set::Compat::Cargo
            }
        };
        // if you specify pre for one item, you have to do it for all of them
        ( $( [$op:expr, $major:expr, $minor:expr, $patch:expr, $pre:expr] ),* ) => {
            Range {
                comparator_set: vec![
                    $(
                        Comparator {
                            op: $op,
                            major: $major,
                            minor: $minor,
                            patch: $patch,
                            pre: $pre,
                        },
                    )*
                ],
                compat: range_set::Compat::Cargo
            }
        };
    }

    // for node compatibility
    macro_rules! comp_sets_node {
        ( $( [$op:expr, $major:expr, $minor:expr, $patch:expr] ),* ) => {
            Range {
                comparator_set: vec![
                    $(
                        Comparator {
                            op: $op,
                            major: $major,
                            minor: $minor,
                            patch: $patch,
                            pre: pre!(None),
                        },
                    )*
                ],
                compat: range_set::Compat::Npm
            }
        };
    }

    macro_rules! id_num {
        ( $num:expr ) => {
            Identifier::Numeric($num)
        };
    }

    macro_rules! id_alpha {
        ( $alpha:expr ) => {
            Identifier::AlphaNumeric(String::from($alpha))
        };
    }

    macro_rules! pre {
        ( None ) => {
            Vec::new()
        };
        ( $( $e:expr ),* ) => {
            vec![
                $(
                    $e,
                )*
            ]
        };
    }

    macro_rules! op {
        ( "=" ) => {
            Op::Eq
        };
        ( "<" ) => {
            Op::Lt
        };
        ( "<=" ) => {
            Op::Lte
        };
        ( ">" ) => {
            Op::Gt
        };
        ( ">=" ) => {
            Op::Gte
        };
    }

    // tests

    range_tests! {
        major: ("1", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        major_minor: ("1.2", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),
        major_minor_patch: ("1.2.3", comp_sets!( [op!(">="), 1, 2, 3], [op!("<"), 2, 0, 0] )),
        major_0_minor_patch: ("0.2.3", comp_sets!( [op!(">="), 0, 2, 3], [op!("<"), 0, 3, 0] )),
        major_0_minor_0_patch: ("0.0.1", comp_sets!( [op!(">="), 0, 0, 1], [op!("<"), 0, 0, 2] )),

        eq_major: ("=1", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        eq_major_minor: ("=1.2", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),
        eq_major_minor_patch: ("=1.2.3", comp_sets!( [op!("="), 1, 2, 3] )),
        eq_all: ("=*", comp_sets!( [op!(">="), 0, 0, 0] )),
        eq_major_star: ("=1.*", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        eq_major_minor_star: ("=1.2.*", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),

        lt_major: ("<1", comp_sets!( [op!("<"), 1, 0, 0] )),
        lt_major_minor: ("<1.2", comp_sets!( [op!("<"), 1, 2, 0] )),
        lt_major_minor_patch: ("<1.2.3", comp_sets!( [op!("<"), 1, 2, 3] )),
        lt_all: ("<*", comp_sets!( [op!("<"), 0, 0, 0] )),
        lt_major_star: ("<1.*", comp_sets!( [op!("<"), 1, 0, 0] )),
        lt_major_minor_star: ("<1.2.*", comp_sets!( [op!("<"), 1, 2, 0] )),

        lte_major: ("<=1", comp_sets!( [op!("<"), 2, 0, 0] )),
        lte_major_minor: ("<=1.2", comp_sets!( [op!("<"), 1, 3, 0] )),
        lte_major_minor_patch: ("<=1.2.3", comp_sets!( [op!("<="), 1, 2, 3] )),
        lte_all: ("<=*", comp_sets!( [op!(">="), 0, 0, 0] )),
        lte_major_star: ("<=1.*", comp_sets!( [op!("<"), 2, 0, 0] )),
        lte_major_minor_star: ("<=1.2.*", comp_sets!( [op!("<"), 1, 3, 0] )),

        gt_major: (">1", comp_sets!( [op!(">"), 1, 0, 0] )),
        gt_major_minor: (">1.2", comp_sets!( [op!(">"), 1, 2, 0] )),
        gt_major_minor_patch: (">1.2.3", comp_sets!( [op!(">"), 1, 2, 3] )),
        gt_all: (">*", comp_sets!( [op!("<"), 0, 0, 0] )),
        gt_major_star: (">1.*", comp_sets!( [op!(">"), 1, 0, 0] )),
        gt_major_minor_star: (">1.2.*", comp_sets!( [op!(">"), 1, 2, 0] )),

        gte_major: (">=1", comp_sets!( [op!(">="), 1, 0, 0] )),
        gte_major_minor: (">=1.2", comp_sets!( [op!(">="), 1, 2, 0] )),
        gte_major_minor_patch: (">=1.2.3", comp_sets!( [op!(">="), 1, 2, 3] )),
        gte_all: (">=*", comp_sets!( [op!(">="), 0, 0, 0] )),
        gte_major_star: (">=1.*", comp_sets!( [op!(">="), 1, 0, 0] )),
        gte_major_minor_star: (">=1.2.*", comp_sets!( [op!(">="), 1, 2, 0] )),

        tilde_major: ("~1", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        tilde_major_0: ("~0", comp_sets!( [op!(">="), 0, 0, 0], [op!("<"), 1, 0, 0] )),
        tilde_major_xrange: ("~1.x", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        tilde_major_2: ("~>1", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        tilde_major_minor: ("~1.2", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),
        tilde_major_minor_xrange: ("~1.2.x", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),
        tilde_major_minor_2: ("~>1.2", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),
        tilde_major_minor_patch: ("~1.2.3", comp_sets!( [op!(">="), 1, 2, 3], [op!("<"), 1, 3, 0] )),
        tilde_major_minor_patch_pre: ("~1.2.3-beta", comp_sets!( [op!(">="), 1, 2, 3, pre!(id_alpha!("beta"))], [op!("<"), 1, 3, 0, pre!()] )),
        tilde_major_minor_patch_2: ("~>1.2.3", comp_sets!( [op!(">="), 1, 2, 3], [op!("<"), 1, 3, 0] )),
        tilde_major_0_minor_patch: ("~0.2.3", comp_sets!( [op!(">="), 0, 2, 3], [op!("<"), 0, 3, 0] )),
        tilde_all: ("~*", comp_sets!( [op!(">="), 0, 0, 0] )),

        caret_major: ("^1", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        caret_major_xrange: ("^1.x", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        caret_major_minor: ("^1.2", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 2, 0, 0] )),
        caret_major_minor_xrange: ("^1.2.x", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 2, 0, 0] )),
        caret_major_minor_patch: ("^1.2.3", comp_sets!( [op!(">="), 1, 2, 3], [op!("<"), 2, 0, 0] )),
        caret_major_minor_patch_pre: ("^1.2.3-beta.4", comp_sets!( [op!(">="), 1, 2, 3, pre!(id_alpha!("beta"), id_num!(4))], [op!("<"), 2, 0, 0, pre!()] )),

        caret_major_0: ("^0", comp_sets!( [op!(">="), 0, 0, 0], [op!("<"), 1, 0, 0] )),
        caret_major_0_xrange: ("^0.x", comp_sets!( [op!(">="), 0, 0, 0], [op!("<"), 1, 0, 0] )),
        caret_major_0_minor_0: ("^0.0", comp_sets!( [op!(">="), 0, 0, 0], [op!("<"), 0, 1, 0] )),
        caret_major_0_minor_0_xrange: ("^0.0.x", comp_sets!( [op!(">="), 0, 0, 0], [op!("<"), 0, 1, 0] )),
        caret_major_0_minor: ("^0.1", comp_sets!( [op!(">="), 0, 1, 0], [op!("<"), 0, 2, 0] )),
        caret_major_0_minor_xrange: ("^0.1.x", comp_sets!( [op!(">="), 0, 1, 0], [op!("<"), 0, 2, 0] )),
        caret_major_0_minor_patch: ("^0.1.2", comp_sets!( [op!(">="), 0, 1, 2], [op!("<"), 0, 2, 0] )),
        caret_major_0_minor_0_patch: ("^0.0.1", comp_sets!( [op!(">="), 0, 0, 1], [op!("<"), 0, 0, 2] )),
        caret_major_0_minor_0_pre: ("^0.0.1-beta", comp_sets!( [op!(">="), 0, 0, 1, pre!(id_alpha!("beta"))], [op!("<"), 0, 0, 2, pre!()] )),
        caret_all: ("^*", comp_sets!( [op!(">="), 0, 0, 0] )),

        two_comparators_1: (">1.2.3 <4.5.6", comp_sets!( [op!(">"), 1, 2, 3], [op!("<"), 4, 5, 6] )),
        two_comparators_2: ("^1.2 ^1", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 2, 0, 0], [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),

        comparator_with_pre: ("=1.2.3-rc.1", comp_sets!( [op!("="), 1, 2, 3, pre!(id_alpha!("rc"), id_num!(1))] )),

        hyphen_major: ("1 - 4", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 5, 0, 0] )),
        hyphen_major_x: ("1.* - 4.*", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 5, 0, 0] )),
        hyphen_major_minor_x: ("1.2.x - 4.5.x", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 4, 6, 0] )),
        hyphen_major_minor_patch: ("1.2.3 - 4.5.6", comp_sets!( [op!(">="), 1, 2, 3], [op!("<="), 4, 5, 6] )),
        hyphen_with_pre: ("1.2.3-rc1 - 4.5.6", comp_sets!( [op!(">="), 1, 2, 3, pre!(id_alpha!("rc1"))], [op!("<="), 4, 5, 6, pre!()] )),
        hyphen_xrange_minor_only1: ("1.*.3 - 3.4.5", comp_sets!( [op!(">="), 1, 0, 0], [op!("<="), 3, 4, 5] )),
        hyphen_xrange_minor_only2: ("1.2.3 - 3.*.5", comp_sets!( [op!(">="), 1, 2, 3], [op!("<"), 4, 0, 0] )),

        hyphen_all_to_something: ("* - 3.4.5", comp_sets!( [op!("<="), 3, 4, 5] )),
        hyphen_to_all: ("1.2.3 - *", comp_sets!( [op!(">="), 1, 2, 3] )),
        hyphen_all_to_all: ("* - *", comp_sets!( [op!(">="), 0, 0, 0] )),

        gte_space: (">= 1.2.3", comp_sets!( [op!(">="), 1, 2, 3] )),
        gte_tab: (">=\t1.2.3", comp_sets!( [op!(">="), 1, 2, 3] )),
        gte_two_spaces: (">=  1.2.3", comp_sets!( [op!(">="), 1, 2, 3] )),
        gt_space: ("> 1.2.3", comp_sets!( [op!(">"), 1, 2, 3] )),
        gt_two_spaces: (">  1.2.3", comp_sets!( [op!(">"), 1, 2, 3] )),
        lte_space: ("<= 1.2.3", comp_sets!( [op!("<="), 1, 2, 3] )),
        lte_two_spaces: ("<=  1.2.3", comp_sets!( [op!("<="), 1, 2, 3] )),
        lt_space: ("< 1.2.3", comp_sets!( [op!("<"), 1, 2, 3] )),
        lt_two_spaces: ("<  1.2.3", comp_sets!( [op!("<"), 1, 2, 3] )),
        eq_space: ("= 1.2.3", comp_sets!( [op!("="), 1, 2, 3] )),
        eq_two_spaces: ("=  1.2.3", comp_sets!( [op!("="), 1, 2, 3] )),
        caret_space: ("^ 1.2.3", comp_sets!( [op!(">="), 1, 2, 3], [op!("<"), 2, 0, 0] )),
        tilde_space: ("~ 1.2.3", comp_sets!( [op!(">="), 1, 2, 3], [op!("<"), 1, 3, 0] )),
        hyphen_spacing: ("1.2.3 -  4.5.6", comp_sets!( [op!(">="), 1, 2, 3], [op!("<="), 4, 5, 6] )),

        // digit options
        digits: ("=0.2.3", comp_sets!( [op!("="), 0, 2, 3] )),
        digits_2: ("=11.2.3", comp_sets!( [op!("="), 11, 2, 3] )),
        digits_3: ("=1.12.3", comp_sets!( [op!("="), 1, 12, 3] )),
        digits_4: ("=1.2.13", comp_sets!( [op!("="), 1, 2, 13] )),
        digits_5: ("=1.2.5678", comp_sets!( [op!("="), 1, 2, 5678] )),

        xrange_major_x: ("1.x", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        xrange_major_x_x: ("1.x.x", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        xrange_major_minor_x: ("1.2.x", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),
        xrange_major_xx: ("1.X", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        xrange_major_xx_xx: ("1.X.X", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        xrange_major_minor_xx: ("1.2.X", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),
        xrange_star: ("*", comp_sets!( [op!(">="), 0, 0, 0] )),
        xrange_x: ("x", comp_sets!( [op!(">="), 0, 0, 0] )),
        xrange_xx: ("X", comp_sets!( [op!(">="), 0, 0, 0] )),
        xrange_major_star: ("1.*", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        xrange_major_star_star: ("1.*.*", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        xrange_major_minor_star: ("1.2.*", comp_sets!( [op!(">="), 1, 2, 0], [op!("<"), 1, 3, 0] )),
        xrange_with_pre: ("1.*.*-beta", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),
        // this is handled as "1.*":
        xrange_minor_only: ("1.*.3", comp_sets!( [op!(">="), 1, 0, 0], [op!("<"), 2, 0, 0] )),

        // special cases
        gte_star: (">=*", comp_sets!( [op!(">="), 0, 0, 0] )),
        empty: ("", comp_sets!( [op!(">="), 0, 0, 0] )),
    }

    range_tests_nodecompat! {
        node_major_minor_patch: ("1.2.3", comp_sets_node!( [op!("="), 1, 2, 3] )),
    }
}
