use crate::function::Function;
use crate::table::Table;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq)]
pub enum Object {
    Function(Function),
    UserData, // TODO
    Thread,   // TODO
    Table(Table),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjectReference(pub Rc<RefCell<Object>>);

impl ObjectReference {
    pub fn new(obj: Object) -> ObjectReference {
        ObjectReference(Rc::new(RefCell::new(obj)))
    }

    pub fn type_str(&self) -> &str {
        match *self.0.borrow() {
            Object::Function(_) => "function",
            // Object::UserData,
            // Object::Thread,
            Object::Table(_) => "table",
            _ => unimplemented!(),
        }
    }

    pub fn tostring(&self) -> String {
        match *self.0.borrow() {
            Object::Function(_) => format!("function: {:?}", self.0.as_ptr()),
            // Object::UserData
            // Object::Thread
            Object::Table(_) => format!("table: {:?}", self.0.as_ptr()),
            _ => unimplemented!(),
        }
    }
}

impl Hash for ObjectReference {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}
