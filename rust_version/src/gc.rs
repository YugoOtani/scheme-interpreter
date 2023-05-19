use crate::token::*;
use std::cell::*;
use std::rc::*;
pub struct VManager {
    ONSTACK: Vec<(*const V, bool)>,
}
impl VManager {
    pub fn new() -> VManager {
        VManager { ONSTACK: vec![] }
    }
}
pub struct V2 {
    val: SchemeVal,
}
