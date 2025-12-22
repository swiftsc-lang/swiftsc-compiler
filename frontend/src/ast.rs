// Abstract Syntax Tree Definitions

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Contract(Contract),
    Function(Function),
    Dummy, // For skipping unimplemented items in MVP
           // Struct, Enum, etc. omitted for MVP
}

#[derive(Debug, Clone, PartialEq)]
pub struct Contract {
    pub name: String,
    pub members: Vec<ContractMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContractMember {
    Field(Field),
    Function(Function),
    Init(Function), // Constructor
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub is_pub: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        name: String,
        destruct_names: Vec<String>,
        ty: Option<Type>,
        init: Expression,
        is_mut: bool,
    },
    Return(Option<Expression>),
    Expr(Expression),
    // If, While, For, Emit omitted for MVP step 1
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    FieldAccess {
        expr: Box<Expression>,
        field: String,
    },
    Index {
        expr: Box<Expression>,
        index: Box<Expression>,
    },
    // StructInit etc.
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(u64),
    String(String),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Path(String), // e.g. "u64", "Address", "std::vec::Vec"
                  // Array, Tuple, etc.
}
