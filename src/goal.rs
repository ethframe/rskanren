use crate::unify::{State, Unify};
use std::rc::Rc;

#[derive(Clone)]
pub enum Goal<U>
where
    U: Unify,
{
    Disj(Box<Goal<U>>, Box<Goal<U>>),
    Conj(Box<Goal<U>>, Box<Goal<U>>),
    Thunk(Rc<Fn(&mut State<U>) -> Goal<U>>),
    Unify(U, U),
}

impl<U> Goal<U>
where
    U: Unify,
{
    fn start(self, mut state: State<U>) -> Stream<U> {
        match self {
            Goal::Disj(g1, g2) => Stream::MPlus(
                Box::new(Stream::Pause(Box::new(state.clone()), g1)),
                Box::new(Stream::Pause(Box::new(state), g2)),
            )
            .step(),
            Goal::Conj(g1, g2) => {
                Stream::Bind(Box::new(Stream::Pause(Box::new(state), g1)), g2).step()
            }
            Goal::Thunk(t) => {
                let goal = t(&mut state);
                Stream::Pause(Box::new(state), Box::new(goal))
            }
            Goal::Unify(u1, u2) => match u1.unify(&u2, &mut state) {
                true => Stream::Pair(Box::new(state), Box::new(Stream::MZero)),
                _ => Stream::MZero,
            },
        }
    }
}

#[derive(Clone)]
pub enum Stream<U>
where
    U: Unify,
{
    MZero,
    Pair(Box<State<U>>, Box<Stream<U>>),
    MPlus(Box<Stream<U>>, Box<Stream<U>>),
    Bind(Box<Stream<U>>, Box<Goal<U>>),
    Pause(Box<State<U>>, Box<Goal<U>>),
}

impl<U> Stream<U>
where
    U: Unify,
{
    fn step(self) -> Stream<U> {
        match self {
            Stream::MZero | Stream::Pair(_, _) => self,
            Stream::MPlus(s1, s2) => match s1.step() {
                Stream::MZero => *s2,
                Stream::Pair(car, cdr) => Stream::Pair(car, Box::new(Stream::MPlus(s2, cdr))),
                s1 => Stream::MPlus(s2, Box::new(s1)),
            },
            Stream::Bind(s, g) => match s.step() {
                Stream::MZero => Stream::MZero,
                Stream::Pair(car, cdr) => Stream::MPlus(
                    Box::new(Stream::Pause(car, g.clone())),
                    Box::new(Stream::Bind(cdr, g)),
                )
                .step(),
                s => Stream::Bind(Box::new(s), g),
            },
            Stream::Pause(s, g) => g.start(*s),
        }
    }

    fn take(self) -> (Stream<U>, Option<State<U>>) {
        let mut stream = self;
        loop {
            match stream {
                Stream::MZero => {
                    return (stream, None);
                }
                Stream::Pair(car, cdr) => {
                    return (*cdr, Some(*car));
                }
                _ => stream = stream.step(),
            }
        }
    }

    fn take_n(self, n: usize) -> (Stream<U>, Vec<State<U>>) {
        let mut vec = Vec::new();
        let mut stream = self;
        let mut remaining = n;
        while remaining != 0 {
            remaining -= 1;
            match stream.take() {
                (tail, Some(state)) => {
                    vec.push(state);
                    stream = tail;
                }
                (tail, None) => {
                    stream = tail;
                    break;
                }
            }
        }
        return (stream, vec);
    }
}

#[cfg(test)]
mod tests {
    use crate::goal;
    use crate::goal::Goal::*;
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
        Disj(
            Box::new(Unify(
                Term::Pair(Box::new(x.clone()), Box::new(Term::Nil)),
                out.clone(),
            )),
            Box::new(Thunk(Rc::new(move |state| {
                let res = Term::Var(state.fresh());
                let xt = xc.clone();
                let rt = res.clone();
                Conj(
                    Box::new(Unify(
                        Term::Pair(Box::new(xc.clone()), Box::new(res.clone())),
                        outc.clone(),
                    )),
                    Box::new(Thunk(Rc::new(move |_| repeato(&xt, &rt)))),
                )
            }))),
        )
    }

    #[test]
    fn test_take_n() {
        let mut state = State::<Term<char>>::new();
        let out = Term::Var(state.fresh());
        let goal = repeato(&Term::Val('*'), &out.clone());
        let (_, result) = goal.start(state).take_n(2);
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
