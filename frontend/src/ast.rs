// Abstract Syntax Tree Definitions

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Use(UseDeclaration),
    Contract(Contract),
    Function(Function),
    Struct(Struct),
    Enum(Enum),
    Trait(Trait),
    Impl(Impl),
    Resource(Struct), // Resources are structured like structs but with linear rules
    Dummy,            // For skipping unimplemented items in MVP
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDeclaration {
    pub path: Vec<String>,          // e.g., ["std", "collections", "Map"]
    pub items: Option<Vec<String>>, // e.g., Some(["Map", "Vec"]) for use std::{Map, Vec}
    pub alias: Option<String>,      // e.g., Some("M") for use std::Map as M
}

#[derive(Debug, Clone, PartialEq)]
pub struct Contract {
    pub name: String,
    pub members: Vec<ContractMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
    pub generics: Vec<String>,
    pub is_pub: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<String>,
    pub is_pub: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub name: String,
    pub methods: Vec<FunctionSignature>,
    pub is_pub: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub generics: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Impl {
    pub trait_name: Option<String>, // Optional for 'impl Type' or mandatory for 'impl Trait for Type'
    pub target_name: String,
    pub methods: Vec<Function>,
    pub generics: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContractMember {
    Storage(Vec<Field>),
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
    pub generics: Vec<String>,
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
    If {
        condition: Expression,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    While {
        condition: Expression,
        body: Block,
    },
    For {
        var_name: String,
        start: Expression,
        end: Expression,
        body: Block,
    },
    Break,
    Continue,
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
        type_args: Vec<Type>,
    },
    FieldAccess {
        expr: Box<Expression>,
        field: String,
    },
    Index {
        expr: Box<Expression>,
        index: Box<Expression>,
    },
    StructInit {
        name: String,
        type_args: Vec<Type>,
        fields: Vec<(String, Expression)>,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
    },
    Match {
        value: Box<Expression>,
        arms: Vec<MatchArm>,
    },
    Closure {
        params: Vec<Param>,
        body: Box<Block>,
    },
    GenericInst {
        target: Box<Expression>,
        type_args: Vec<Type>,
    },
    Try(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    EnumVariant {
        enum_name: String,
        variant_name: String,
    },
    Wildcard,
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
    Assign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(u64),
    String(String),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Path(String),               // e.g. "u64", "Address", "std::vec::Vec"
    Generic(String, Vec<Type>), // e.g. "Vec<u64>"
    Map(Box<Type>, Box<Type>),
    // Array, Tuple, etc.
}
