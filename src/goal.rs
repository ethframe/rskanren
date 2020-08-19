use crate::stream::{Stream, StreamFn};
use crate::unify::{State, Unify, Var};
use std::rc::Rc;

#[derive(Clone)]
pub enum Goal<U>
where
    U: Unify,
{
    Disj(Box<Goal<U>>, Box<Goal<U>>),
    Conj(Box<Goal<U>>, Box<Goal<U>>),
    Thunk(Rc<dyn Fn(&mut State<U>) -> Goal<U>>),
    Unify(Var<U>, Var<U>),
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
    use crate::goal::Goal;
    use crate::goal::Goal::*;
    use crate::stream::Stream;
    use crate::unify::{State, Term, Var};
    use std::fmt;
    use std::rc::Rc;

    fn repeato<T>(x: Var<Term<T>>, out: Var<Term<T>>) -> Goal<Term<T>>
    where
        T: PartialEq + Clone + fmt::Debug + 'static,
    {
        conde!(
            Unify(Term::Pair(x.clone(), Term::Nil.into()).into(), out.clone(),),
            Thunk(Rc::new(move |state| {
                let res = state.fresh();
                let xc = x.clone();
                let resc = res.clone();
                conde!(
                    {
                        Unify(
                            Term::Pair(x.clone(), res.clone()).into(),
                            out.clone(),
                        ),
                        Thunk(Rc::new(move |_| repeato(xc.clone(), resc.clone())))
                    }
                )
            }))
        )
    }

    #[test]
    fn test_repeato() {
        let mut state = State::<Term<char>>::new();
        let out = state.fresh();
        let result = Stream::unit(state)
            .bind(repeato(Term::Val('*').into(), out.clone()))
            .into_iter()
            .map(|state| out.reify(&state))
            .take(2)
            .collect::<Vec<_>>();
        assert_eq!(
            result[0],
            Term::Pair(Term::Val('*').into(), Term::Nil.into()).into()
        );
        assert_eq!(
            result[1],
            Term::Pair(
                Term::Val('*').into(),
                Term::Pair(Term::Val('*').into(), Term::Nil.into()).into()
            )
            .into()
        );
    }
}
