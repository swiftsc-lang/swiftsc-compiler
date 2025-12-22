use crate::ast::*;
use crate::sema::error::SemanticError;

#[derive(Debug, Clone)]
pub enum SecurityWarning {
    PotentialOverflow { operation: String, location: String },
    UninitializedVariable { name: String },
    UncheckedArithmetic { operation: String },
}

pub struct SecurityAnalyzer {
    warnings: Vec<SecurityWarning>,
}

impl SecurityAnalyzer {
    pub fn new() -> Self {
        SecurityAnalyzer {
            warnings: Vec::new(),
        }
    }
    
    pub fn analyze_program(&mut self, program: &Program) {
        for item in &program.items {
            match item {
                Item::Contract(contract) => self.analyze_contract(contract),
                Item::Function(func) => self.analyze_function(func),
                Item::Dummy => {},
            }
        }
    }
    
    fn analyze_contract(&mut self, contract: &Contract) {
        for member in &contract.members {
            if let ContractMember::Function(func) = member {
                self.analyze_function(func);
            } else if let ContractMember::Init(func) = member {
                self.analyze_function(func);
            }
        }
    }
    
    fn analyze_function(&mut self, func: &Function) {
        self.analyze_block(&func.body);
    }
    
    fn analyze_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.analyze_statement(stmt);
        }
    }
    
    fn analyze_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Let { init, .. } => {
                self.analyze_expression(init);
            }
            Statement::Expr(expr) => {
                self.analyze_expression(expr);
            }
            Statement::Return(Some(expr)) => {
                self.analyze_expression(expr);
            }
            _ => {}
        }
    }
    
    fn analyze_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Binary { left, op, right } => {
                // Check for potential integer overflow in arithmetic
                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => {
                        self.warnings.push(SecurityWarning::UncheckedArithmetic {
                            operation: format!("{:?}", op),
                        });
                    }
                    _ => {}
                }
                self.analyze_expression(left);
                self.analyze_expression(right);
            }
            Expression::Call { func, args } => {
                self.analyze_expression(func);
                for arg in args {
                    self.analyze_expression(arg);
                }
            }
            Expression::FieldAccess { expr, .. } => {
                self.analyze_expression(expr);
            }
            _ => {}
        }
    }
    
    pub fn get_warnings(&self) -> &[SecurityWarning] {
        &self.warnings
    }
    
    pub fn has_critical_warnings(&self) -> bool {
        // For MVP, we don't have critical warnings yet
        false
    }
}
