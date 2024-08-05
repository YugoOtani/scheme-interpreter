use std::collections::LinkedList;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SortedList<T: Ord> {
    list: LinkedList<T>,
}
impl<T: Ord> SortedList<T> {
    pub fn new() -> Self {
        SortedList {
            list: LinkedList::new(),
        }
    }
    pub fn insert(&mut self, t: T) {
        let mut new = LinkedList::new();
        loop {
            match self.list.pop_front() {
                None => break,
                Some(front) => {
                    if front == t {
                        break;
                    } else if front < t {
                        new.push_back(front);
                        continue;
                    } else {
                        // front > t
                        self.list.push_front(front);
                        break;
                    }
                }
            }
        }
        new.push_back(t);
        new.append(&mut self.list);
        self.list = new;
    }
}
#[test]
fn sorted_list_test() {
    use std::collections::BTreeSet;
    let mut lst = SortedList::new();
    let mut st = BTreeSet::new();
    for i in vec![8, 6, 9, 0, 2, 6, 0, 7] {
        lst.insert(i);
        st.insert(i);
        assert!(lst.list.iter().zip(st.iter()).all(|(a, b)| *a == *b))
    }
}
