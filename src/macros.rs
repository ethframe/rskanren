#[macro_export]
macro_rules! conde_conj {
    ($lhs:expr, $($rhs:expr),+) => {
        $crate::goal::Goal::Conj(
            Box::new($lhs),
            Box::new($crate::conde_conj!($($rhs),+))
        )
    };
    ($expr:expr) => {
        $expr
    };
}

#[macro_export]
macro_rules! conde {
    ({ $($lhs:expr),+ }, $($rhs:tt)+) => {
        $crate::goal::Goal::Disj(
            Box::new($crate::conde_conj!($($lhs),+)),
            Box::new($crate::conde!($($rhs)+))
        )
    };
    ({ $($expr:expr),+ }) => {
        $crate::conde_conj!($($expr),+)
    };
    ($lhs:expr, $($rhs:tt)+) => {
        $crate::goal::Goal::Disj(
            Box::new($lhs),
            Box::new($crate::conde!($($rhs)+))
        )
    };
    ($expr:expr) => {
        $expr
    };
}
