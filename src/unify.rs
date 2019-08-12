use im::HashMap;
use std::fmt;
use std::hash::Hash;

#[derive(Clone)]
pub struct State<U>
where
    U: Unify,
{
    subst: HashMap<U::Var, U>,
    gen: U::Fresh,
}

impl<U> State<U>
where
    U: Unify,
{
    pub fn new() -> Self {
        State {
            subst: HashMap::new(),
            gen: U::Fresh::default(),
        }
    }

    pub fn fresh(&mut self) -> U::Var {
        self.gen.fresh()
    }

    fn get(&self, key: &U::Var) -> Option<&U> {
        self.subst.get(key)
    }

    fn set(&mut self, key: U::Var, value: U) -> bool {
        if value.occurs(&key, self) {
            false
        } else {
            self.subst.insert(key, value);
            true
        }
    }
}

pub trait Fresh: Clone + Default {
    type Var: Eq + Hash + Clone;
    fn fresh(&mut self) -> Self::Var;
}

pub trait Unify: Clone {
    type Var: Eq + Hash + Clone;
    type Fresh: Fresh<Var = Self::Var>;
    fn unify(&self, other: &Self, state: &mut State<Self>) -> bool;
    fn occurs(&self, var: &Self::Var, state: &State<Self>) -> bool;
    fn to_var(&self) -> Option<&Self::Var>;
    fn walk<'a>(&'a self, state: &'a State<Self>) -> &'a Self {
        let mut res = self;
        loop {
            match res.to_var() {
                Some(var) => match state.get(var) {
                    Some(val) => res = val,
                    None => return res,
                },
                None => return res,
            }
        }
    }
}

pub trait Reify: Unify {
    fn reify(&self, state: &State<Self>) -> Self;
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Var(usize);

#[derive(Clone, Default)]
pub struct Gen(usize);

impl Fresh for Gen {
    type Var = Var;
    fn fresh(&mut self) -> Var {
        let var = Var(self.0);
        self.0 += 1;
        var
    }
}

#[derive(PartialEq, Clone)]
pub enum Term<T>
where
    T: PartialEq + Clone,
{
    Var(Var),
    Val(T),
    Pair(Box<Term<T>>, Box<Term<T>>),
    Nil,
}

impl<T> fmt::Debug for Term<T>
where
    T: PartialEq + Clone + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Var(var) => write!(f, "Var({:?})", var),
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
    type Var = Var;
    type Fresh = Gen;
    fn unify(&self, other: &Self, state: &mut State<Self>) -> bool {
        let lhs = self.walk(state).clone();
        let rhs = other.walk(state).clone();
        match &lhs {
            Term::Var(lvar) => match &rhs {
                Term::Var(rvar) if lvar == rvar => true,
                _ => state.set(lvar.clone(), rhs),
            },
            Term::Val(lval) => match &rhs {
                Term::Var(rvar) => state.set(rvar.clone(), lhs),
                Term::Val(rval) => lval == rval,
                _ => false,
            },
            Term::Pair(lcar, lcdr) => match &rhs {
                Term::Var(rvar) => state.set(rvar.clone(), lhs),
                Term::Pair(rcar, rcdr) => lcar.unify(rcar, state) && lcdr.unify(rcdr, state),
                _ => false,
            },
            Term::Nil => match &rhs {
                Term::Var(rvar) => state.set(rvar.clone(), lhs),
                Term::Nil => true,
                _ => false,
            },
        }
    }
    fn occurs(&self, var: &Var, state: &State<Self>) -> bool {
        match self.walk(state) {
            Term::Var(svar) => svar == var,
            Term::Pair(scar, scdr) => scar.occurs(var, state) || scdr.occurs(var, state),
            _ => false,
        }
    }
    fn to_var(&self) -> Option<&Var> {
        match self {
            Term::Var(var) => Some(var),
            _ => None,
        }
    }
}

impl<T> Reify for Term<T>
where
    T: PartialEq + Clone,
{
    fn reify(&self, state: &State<Self>) -> Self {
        let val = self.walk(state);
        match val {
            Term::Pair(car, cdr) => {
                Term::Pair(Box::new(car.reify(state)), Box::new(cdr.reify(state)))
            }
            Term::Var(_) | Term::Val(_) | Term::Nil => val.clone(),
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
        state.set(var.clone(), Term::Val(1));
        assert_eq!(state.get(&var).unwrap(), &Term::Val(1));
    }

    #[test]
    fn get_missing() {
        let mut state = State::<Term<i32>>::new();
        let var = state.fresh();
        assert!(state.get(&var).is_none());
    }

    #[test]
    fn set_and_get_fresh() {
        let mut state = State::<Term<i32>>::new();
        let var = state.fresh();
        state.set(var, Term::Val(1));
        let var = state.fresh();
        assert!(state.get(&var).is_none());
    }

    #[test]
    fn unify_terms() {
        let mut state = State::<Term<i32>>::new();
        let lvar = state.fresh();
        let rvar = state.fresh();
        let lpair = Term::Pair(Box::new(Term::Val(1)), Box::new(Term::Var(rvar.clone())));
        let rpair = Term::Pair(Box::new(Term::Var(lvar.clone())), Box::new(Term::Val(2)));
        assert!(lpair.unify(&rpair, &mut state));
        assert_eq!(state.get(&lvar).unwrap(), &Term::Val(1));
        assert_eq!(state.get(&rvar).unwrap(), &Term::Val(2));
    }

    #[test]
    fn chain_unify() {
        let mut state = State::<Term<i32>>::new();
        let fst = Term::Var(state.fresh());
        let snd = Term::Var(state.fresh());
        fst.unify(&snd, &mut state);
        assert_eq!(fst.walk(&state), &snd);
        fst.unify(&Term::Val(1), &mut state);
        assert_eq!(fst.walk(&state), &Term::Val(1));
        assert_eq!(snd.walk(&state), &Term::Val(1));
    }
}
