use std::iter::FromIterator;
use std::mem::swap;
use std::rc::Rc;

pub trait StreamFn<T>: Clone {
    fn apply(self, v: T) -> Stream<T, Self>;
}

pub enum Stream<T, F>
where
    F: StreamFn<T>,
{
    MZero,
    MPlus(Box<Stream<T, F>>, Box<Stream<T, F>>),
    Bind(Box<Stream<T, F>>, F),
    Pair(T, Box<Stream<T, F>>),
    Pause(T, F),
}

use Stream::*;

impl<T, F> Stream<T, F>
where
    F: StreamFn<T>,
{
    pub fn mzero() -> Stream<T, F> {
        MZero
    }

    pub fn unit(value: T) -> Stream<T, F> {
        Pair(value, Box::new(MZero))
    }

    pub fn mplus(self, other: Stream<T, F>) -> Stream<T, F> {
        MPlus(Box::new(self), Box::new(other))
    }

    pub fn bind(self, func: F) -> Stream<T, F> {
        Bind(Box::new(self), func)
    }

    pub fn step(stream: Stream<T, F>) -> Stream<T, F> {
        match stream {
            MPlus(s1, s2) => match Stream::step(*s1) {
                MZero => *s2,
                Pair(v, s1) => Pair(v, Box::new(MPlus(s2, s1))),
                s1 => MPlus(s2, Box::new(s1)),
            },
            Bind(s, f) => match Stream::step(*s) {
                MZero => MZero,
                Pair(v, t) => Stream::step(Pause(v, f.clone()).mplus(Bind(t, f))),
                s => Bind(Box::new(s), f),
            },
            Pause(s, f) => f.apply(s),
            _ => stream,
        }
    }
}

pub struct StreamIter<T, F>(Stream<T, F>)
where
    F: StreamFn<T>;

impl<T, F> Iterator for StreamIter<T, F>
where
    F: Clone + StreamFn<T>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let mut stream = MZero;
        swap(&mut self.0, &mut stream);
        loop {
            match stream {
                Pair(v, s) => {
                    self.0 = *s;
                    return Some(v);
                }
                MZero => {
                    self.0 = MZero;
                    return None;
                }
                _ => stream = Stream::step(stream),
            }
        }
    }
}

impl<T, F> IntoIterator for Stream<T, F>
where
    F: StreamFn<T>,
{
    type Item = T;
    type IntoIter = StreamIter<T, F>;
    fn into_iter(self) -> Self::IntoIter {
        StreamIter(self)
    }
}

impl<T, F> FromIterator<T> for Stream<T, F>
where
    F: StreamFn<T>,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let it = iter.into_iter().collect::<Vec<T>>().into_iter();
        let mut s = MZero;
        for i in it.rev() {
            s = Pair(i, Box::new(s));
        }
        s
    }
}

pub struct FnWrapper<T>(Rc<dyn Fn(T) -> Stream<T, FnWrapper<T>>>);

impl<T> FnWrapper<T> {
    pub fn new<F: Fn(T) -> Stream<T, FnWrapper<T>> + 'static>(func: F) -> FnWrapper<T> {
        FnWrapper(Rc::new(func))
    }
}

impl<T> Clone for FnWrapper<T> {
    fn clone(&self) -> Self {
        FnWrapper(self.0.clone())
    }
}

impl<T> StreamFn<T> for FnWrapper<T> {
    fn apply(self, val: T) -> Stream<T, Self> {
        self.0(val)
    }
}

#[cfg(test)]
mod tests {
    use crate::stream::{FnWrapper, Stream};
    use std::iter::FromIterator;

    #[test]
    fn add_1() {
        let res = Stream::from_iter([1, 2, 3, 4, 5].iter().copied())
            .bind(FnWrapper::new(|x| Stream::unit(x + 1)))
            .into_iter()
            .collect::<Vec<i32>>();
        assert_eq!(res, [2, 3, 4, 5, 6]);
    }

    #[test]
    fn dup() {
        let res = Stream::from_iter([1, 2, 3].iter().copied())
            .bind(FnWrapper::new(|x| Stream::unit(x).mplus(Stream::unit(x))))
            .into_iter()
            .collect::<Vec<i32>>();
        assert_eq!(res, [1, 1, 2, 2, 3, 3]);
    }
}
