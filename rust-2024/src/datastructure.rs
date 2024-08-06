use std::fmt::Debug;

use crate::short_addr;
/// ソートされたLinkedList
/// 中身は降順
#[derive(Clone, PartialEq, Eq)]
pub struct SortedSet<T: Ord>(Link<T>);
#[derive(Debug, Clone, PartialEq, Eq)]
struct Node<T> {
    v: T,
    next: Link<T>,
}
impl<T> Node<T> {
    fn new(v: T) -> Self {
        Self { v, next: None }
    }
}

type Link<T> = Option<Box<Node<T>>>;
impl<T: Ord + Debug> SortedSet<T> {
    pub fn new() -> Self {
        Self(None)
    }
    pub fn insert(&mut self, t: T) {
        Self::_insert_mut(&mut self.0, t);
    }
    fn _insert_mut(lnk: &mut Link<T>, t: T) {
        let mut tmp = None;
        std::mem::swap(&mut tmp, lnk);
        let tmp = Self::_insert(tmp, t);
        *lnk = tmp;
    }
    fn _insert(lnk: Link<T>, t: T) -> Link<T> {
        match lnk {
            None => Some(Box::new(Node::new(t))),
            Some(mut nd) => {
                if &nd.v == &t {
                    Some(nd)
                } else if &nd.v < &t {
                    Some(Box::new(Node {
                        v: t,
                        next: Some(nd),
                    }))
                } else {
                    // nd.v > t
                    let nxt = Self::_insert(nd.next, t);
                    nd.next = nxt;
                    Some(nd)
                }
            }
        }
    }
    pub fn find_or_insert(&mut self, t: T) -> &T {
        let mut p = &mut self.0;

        while p.is_some() && &p.as_ref().unwrap().v > &t {
            p = &mut p.as_mut().unwrap().next;
        }
        Self::_insert_mut(p, t);
        &p.as_ref().unwrap().v
    }
    pub fn iter<'a>(&'a self) -> IteForSortedSet<'a, T> {
        IteForSortedSet {
            ite: self.0.as_deref(),
        }
    }
    pub fn iter_mut<'a>(&'a mut self) -> IteMutForSortedSet<'a, T> {
        IteMutForSortedSet {
            ite: self.0.as_deref_mut(),
        }
    }
}

impl<T: Debug + Ord> Debug for SortedSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn helper<T: Debug + Ord>(
            slf: &Option<Box<Node<T>>>,
            f: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            match slf {
                None => write!(f, "()"),
                Some(nd) => {
                    let v = &nd.as_ref().v;
                    if cfg![debug_assertions] {
                        write!(f, "{:?}@{} -> ", v, short_addr(v))?;
                    } else {
                        write!(f, "{:?} -> ", v)?;
                    }

                    helper(&nd.next, f)
                }
            }
        }
        write!(f, "SortedList[")?;
        helper(&self.0, f)?;
        write!(f, "]")
    }
}
pub struct IteForSortedSet<'a, T: Ord> {
    ite: Option<&'a Node<T>>,
}
impl<'a, T: Ord> Iterator for IteForSortedSet<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.ite {
            Some(nd) => {
                let ret = &nd.v;
                self.ite = nd.next.as_ref().map(|v| &**v);
                Some(ret)
            }
            None => None,
        }
    }
}
pub struct IteMutForSortedSet<'a, T: Ord> {
    ite: Option<&'a mut Node<T>>,
}
impl<'a, T: Ord> Iterator for IteMutForSortedSet<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.ite.take().map(|node| {
            self.ite = node.next.as_mut().map(|v| v.as_mut());
            &mut node.v
        })
    }
}

#[test]
fn sorted_set_test() {
    use std::cmp::Reverse;
    use std::collections::BTreeSet;
    let mut st = BTreeSet::new();
    let mut st2 = SortedSet::new();
    for v in vec![4, 3, 8, 3, 2, 9, 8, 3] {
        st.insert(Reverse(v));
        let ret = st2.find_or_insert(v);
        assert_eq!(*ret, v);
        for (v1, v2) in st.iter().zip(st2.iter()) {
            assert_eq!(v1.0, *v2);
        }
    }
}
