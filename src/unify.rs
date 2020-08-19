use im::HashMap;
use std::convert::From;
use std::fmt;

#[derive(Clone)]
pub struct State<U>
where
    U: Unify,
{
    subst: HashMap<VarIndex, Var<U>>,
    next: usize,
}

impl<U> State<U>
where
    U: Unify,
{
    pub fn new() -> Self {
        State {
            subst: HashMap::new(),
            next: 0,
        }
    }

    pub fn fresh(&mut self) -> Var<U> {
        let var = Var::Var(VarIndex(self.next));
        self.next += 1;
        var
    }

    pub fn get<'a>(&'a self, mut val: &'a Var<U>) -> &'a Var<U> {
        loop {
            match val {
                Var::Var(v) => match self.subst.get(v) {
                    Some(v) => val = v,
                    None => return val,
                },
                _ => return val,
            }
        }
    }

    pub fn set(&mut self, var: VarIndex, val: Var<U>) {
        self.subst.insert(var, val);
    }

    fn assoc_val(var: VarIndex, val: Var<U>, state: &mut State<U>) -> bool {
        if val.occurs(var, state) {
            false
        } else {
            state.set(var, val);
            true
        }
    }

    fn assoc_var(var: VarIndex, val: VarIndex, state: &mut State<U>) -> bool {
        if var != val {
            state.set(var, Var::Var(val));
        }
        true
    }
}

pub trait Unify: Clone {
    fn occurs(&self, var: VarIndex, state: &State<Self>) -> bool;
    fn unify(&self, other: &Self, state: &mut State<Self>) -> bool;
    fn reify(&self, state: &State<Self>) -> Self;
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct VarIndex(usize);

#[derive(PartialEq, Clone)]
pub enum Var<U>
where
    U: Unify,
{
    Var(VarIndex),
    Val(Box<U>),
}

impl<U> fmt::Debug for Var<U>
where
    U: Unify + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var::Var(var) => write!(f, "Var({:?})", var.0),
            Var::Val(val) => write!(f, "{:?}", val),
        }
    }
}

impl<U> Var<U>
where
    U: Unify,
{
    pub fn unwrap(&self) -> &U {
        match self {
            Var::Val(v) => v,
            _ => panic!(),
        }
    }

    pub fn is_var(&self) -> bool {
        match self {
            Var::Var(_) => true,
            _ => false,
        }
    }

    pub fn occurs(&self, var: VarIndex, state: &State<U>) -> bool {
        match self {
            Var::Val(v) => v.occurs(var, state),
            Var::Var(v) => var == *v,
        }
    }

    pub fn unify(&self, other: &Var<U>, state: &mut State<U>) -> bool {
        let lhs = state.get(self);
        let rhs = state.get(other);
        match (lhs, rhs) {
            (Var::Var(lvar), Var::Var(rvar)) => State::assoc_var(*lvar, *rvar, state),
            (Var::Var(lvar), Var::Val(_)) => State::assoc_val(*lvar, rhs.clone(), state),
            (Var::Val(_), Var::Var(rvar)) => State::assoc_val(*rvar, lhs.clone(), state),
            (Var::Val(lval), Var::Val(rval)) => lval.clone().unify(&*rval.clone(), state),
        }
    }

    pub fn reify(&self, state: &State<U>) -> Var<U> {
        match state.get(self) {
            Var::Var(var) => Var::Var(*var),
            Var::Val(val) => Var::Val(Box::new(val.reify(state))),
        }
    }
}

impl<U> From<U> for Var<U>
where
    U: Unify,
{
    fn from(u: U) -> Var<U> {
        return Var::Val(Box::new(u));
    }
}

#[derive(PartialEq, Clone)]
pub enum Term<T>
where
    T: PartialEq + Clone,
{
    Val(T),
    Pair(Var<Term<T>>, Var<Term<T>>),
    Nil,
}

impl<T> fmt::Debug for Term<T>
where
    T: PartialEq + Clone + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Val(val) => write!(f, "Val({:?})", val),
            Term::Pair(car, cdr) => write!(f, "Pair({:?}, {:?})", car, cdr),
            Term::Nil => write!(f, "Nil"),
        }
    }
}

impl<T> Unify for Term<T>
where
    T: PartialEq + Clone,
{
    fn occurs(&self, var: VarIndex, state: &State<Self>) -> bool {
        match self {
            Term::Pair(a, d) => a.occurs(var, state) || d.occurs(var, state),
            _ => false,
        }
    }

    fn unify(&self, other: &Self, state: &mut State<Self>) -> bool {
        match (self, other) {
            (Term::Val(s), Term::Val(o)) => s == o,
            (Term::Pair(sa, sd), Term::Pair(oa, od)) => {
                sa.unify(&oa, state) && sd.unify(&od, state)
            }
            (Term::Nil, Term::Nil) => true,
            _ => false,
        }
    }

    fn reify(&self, state: &State<Self>) -> Self {
        match self {
            Term::Val(val) => Term::Val(val.clone()),
            Term::Pair(car, cdr) => Term::Pair(car.reify(state), cdr.reify(state)),
            Term::Nil => Term::Nil,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::unify::{State, Term, Unify};

    #[test]
    fn set_and_get() {
        let mut state = State::<Term<i32>>::new();
        let var = state.fresh();
        var.unify(&Term::Val(1).into(), &mut state);
        assert_eq!(state.get(&var).unwrap(), &Term::Val(1));
    }

    #[test]
    fn get_missing() {
        let mut state = State::<Term<i32>>::new();
        let var = state.fresh();
        assert!(state.get(&var).is_var());
    }

    #[test]
    fn set_and_get_fresh() {
        let mut state = State::<Term<i32>>::new();
        let var = state.fresh();
        var.unify(&Term::Val(1).into(), &mut state);
        let var = state.fresh();
        assert!(state.get(&var).is_var());
    }

    #[test]
    fn unify_terms() {
        let mut state = State::<Term<i32>>::new();
        let lvar = state.fresh();
        let rvar = state.fresh();
        let lpair = Term::Pair(Term::Val(1).into(), rvar.clone());
        let rpair = Term::Pair(lvar.clone(), Term::Val(2).into());
        assert!(lpair.unify(&rpair, &mut state));
        assert_eq!(state.get(&lvar).unwrap(), &Term::Val(1));
        assert_eq!(state.get(&rvar).unwrap(), &Term::Val(2));
    }

    #[test]
    fn chain_unify() {
        let mut state = State::<Term<i32>>::new();
        let fst = state.fresh();
        let snd = state.fresh();
        fst.unify(&snd, &mut state);
        assert_eq!(state.get(&fst), &snd);
        fst.unify(&Term::Val(1).into(), &mut state);
        assert_eq!(state.get(&fst).unwrap(), &Term::Val(1));
        assert_eq!(state.get(&snd).unwrap(), &Term::Val(1));
    }

    #[test]
    fn reify_term() {
        assert_eq!(0, 0);
        let mut state = State::<Term<String>>::new();
        let var = state.fresh();
        let fst = Term::Pair(Term::Val("foo".to_owned()).into(), var.clone());
        let snd = Term::Pair(
            Term::Val("foo".to_owned()).into(),
            Term::Val("bar".to_owned()).into(),
        );
        assert_eq!(fst.unify(&snd, &mut state), true);
        assert_eq!(fst.reify(&state), snd.reify(&state));
    }
}
