use std::collections::VecDeque;

pub mod enum_with_tags;

pub struct MultiPeek<I: Iterator> {
    itr: I,
    peeked: VecDeque<I::Item>,
}

/// MultiPeek iterator that
/// can look an arbitrary number
/// of elements ahead
impl<V, I: Iterator<Item = V>> MultiPeek<I> {
    pub fn new(iterator: I) -> Self {
        Self {
            itr: iterator,
            peeked: VecDeque::new(),
        }
    }

    pub fn peek_nth(&mut self, i: usize) -> Option<&V> {
        while i >= self.peeked.len() {
            let elem = self.itr.next()?;
            self.peeked.push_back(elem);
        }
        self.peeked.get(i)
    }

    pub fn peek(&mut self) -> Option<&V> {
        self.peek_nth(0)
    }

    pub fn next_if(&mut self, f: impl FnOnce(&V) -> bool) -> Option<V> {
        if f(self.peek()?) { self.next() } else { None }
    }

    pub fn inner(&self) -> &I {
        &self.itr
    }
}

impl<V, I: Iterator<Item = V>> Iterator for MultiPeek<I> {
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(elem) = self.peeked.pop_front() {
            return Some(elem);
        }
        self.itr.next()
    }
}

#[cfg(test)]
mod tests {
    use super::MultiPeek;

    #[test]
    fn test_multi_peek() {
        let mut itr = MultiPeek::new([1, 2, 3, 4, 5].into_iter());
        assert_eq!(itr.next(), Some(1));
        assert_eq!(itr.peek(), Some(&2));
        assert_eq!(itr.peek_nth(1), Some(&3));
        assert_eq!(itr.next(), Some(2));
        assert_eq!(itr.peek_nth(1), Some(&4));
    }
}
