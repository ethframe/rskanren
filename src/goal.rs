use crate::stream::{Stream, StreamFn};
use crate::unify::{State, Unify};
use std::rc::Rc;

#[derive(Clone)]
pub enum Goal<U>
where
    U: Unify,
{
    Disj(Box<Goal<U>>, Box<Goal<U>>),
    Conj(Box<Goal<U>>, Box<Goal<U>>),
    Thunk(Rc<dyn Fn(&mut State<U>) -> Goal<U>>),
    Unify(U, U),
}

impl<U> StreamFn<State<U>> for Goal<U>
where
    U: Unify,
{
    fn apply(self, mut state: State<U>) -> Stream<State<U>, Self> {
        match self {
            Goal::Disj(g1, g2) => Stream::unit(state.clone())
                .bind(*g1)
                .mplus(Stream::unit(state).bind(*g2)),
            Goal::Conj(g1, g2) => Stream::unit(state).bind(*g1).bind(*g2),
            Goal::Thunk(t) => {
                let goal = t(&mut state);
                Stream::unit(state).bind(goal)
            }
            Goal::Unify(u1, u2) => match u1.unify(&u2, &mut state) {
                true => Stream::unit(state),
                _ => Stream::mzero(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::conde;
    use crate::goal;
    use crate::goal::Goal::*;
    use crate::stream::Stream;
    use crate::unify;
    use crate::unify::{Reify, State, Term};
    use std::fmt;
    use std::rc::Rc;

    fn repeato<T>(x: &unify::Term<T>, out: &unify::Term<T>) -> goal::Goal<unify::Term<T>>
    where
        T: PartialEq + Clone + fmt::Debug + 'static,
    {
        let xc = x.clone();
        let outc = out.clone();
        conde!(
            Unify(
                Term::Pair(Box::new(x.clone()), Box::new(Term::Nil)),
                out.clone(),
            ),
            Thunk(Rc::new(move |state| {
                let res = Term::Var(state.fresh());
                let xt = xc.clone();
                let rt = res.clone();
                conde!(
                    {
                        Unify(
                            Term::Pair(Box::new(xc.clone()), Box::new(res.clone())),
                            outc.clone(),
                        ),
                        Thunk(Rc::new(move |_| repeato(&xt, &rt)))
                    }
                )
            }))
        )
    }

    #[test]
    fn test_repeato() {
        let mut state = State::<Term<char>>::new();
        let out = Term::Var(state.fresh());
        let result = Stream::unit(state)
            .bind(repeato(&Term::Val('*'), &out.clone()))
            .into_iter()
            .take(2)
            .collect::<Vec<State<Term<char>>>>();
        assert_eq!(result.len(), 2);
        assert_eq!(
            out.reify(&result[0]),
            Term::Pair(Box::new(Term::Val('*')), Box::new(Term::Nil))
        );
        assert_eq!(
            out.reify(&result[1]),
            Term::Pair(
                Box::new(Term::Val('*')),
                Box::new(Term::Pair(Box::new(Term::Val('*')), Box::new(Term::Nil)))
            )
        );
    }
}
