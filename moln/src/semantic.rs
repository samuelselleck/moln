use std::ops::Deref;

use symbol_table::{SymbolTable, ValueInfo};

use crate::{
    error::{CResult, CompilerError},
    parser::ast::{
        Ast, BinaryOp, Block, Expression, ExpressionKind, Function, RootStatement, Statement, Type,
        UnaryPostfixOp, UnaryPrefixOp, VariableDeclaration, Variant, VariantDefinition,
    },
    symbol_interning::SymbolId,
};

pub mod symbol_table;
pub mod t_ast;

pub struct SemanticContext {
    table: SymbolTable,
}

impl SemanticContext {
    pub fn new() -> Self {
        Self {
            table: SymbolTable::new(),
        }
    }

    pub fn semantic_analysis(&mut self, ast: Ast) -> CResult<Block> {
        let generalized_ast = Block {
            statements: ast
                .elems
                .into_iter()
                .map(|e| {
                    Statement::Expression(e.map(|r| match r {
                        RootStatement::Function(function) => {
                            ExpressionKind::FunctionDefinition(function)
                        }
                        RootStatement::Type(symbol_id, typ) => {
                            ExpressionKind::TypeDefinition(symbol_id, typ)
                        }
                    }))
                })
                .collect::<Vec<_>>(),
            unit_return: true,
        };

        // is this scope needed/wanted?
        self.scope(|scope| scope.check_block(&generalized_ast))?;
        self.table.validate()?;

        // TODO construct typed AST and return
        Ok(generalized_ast)
    }

    // Top-level root for semantic analysis
    pub fn check_block(&mut self, block: &Block) -> CResult<Type> {
        // pre-visit certain types to populate table
        for node in &block.statements {
            if let Statement::Expression(s) = node {
                match &s.node {
                    ExpressionKind::FunctionDefinition(func) => {
                        self.collect_function(func)?;
                    }
                    ExpressionKind::TypeDefinition(symbol_id, typ) => {
                        self.table.define_type(*symbol_id, typ.clone())?;
                    }
                    ExpressionKind::Variable(_)
                    | ExpressionKind::Float(_)
                    | ExpressionKind::Int(_)
                    | ExpressionKind::String(_)
                    | ExpressionKind::Boolean(_)
                    | ExpressionKind::Variant(_)
                    | ExpressionKind::List(_)
                    | ExpressionKind::Unit
                    | ExpressionKind::Block(_)
                    | ExpressionKind::Parenthesis(_)
                    | ExpressionKind::Conditional(_)
                    | ExpressionKind::UnaryPrefix { .. }
                    | ExpressionKind::UnaryPostfix { .. }
                    | ExpressionKind::Binary { .. } => (),
                }
            }
        }

        let mut last_type = Type::Unit;
        for node in &block.statements {
            match node {
                Statement::Expression(expr) => {
                    last_type = self.check_expression(&expr)?;
                }
                Statement::Declaration(VariableDeclaration { definition, init }) => {
                    let symbol = &definition.variable;
                    let mut resolved_known_type = None;
                    if let Some(init) = init {
                        let expr_type = self.check_expression(&**init)?;
                        if let Some(known_type) = &definition.known_type {
                            resolved_known_type = Some((**known_type).clone());
                            if **known_type != expr_type {
                                return Err(CompilerError::new("type missmatch")
                                    .annotation(known_type.span, "type specified here")
                                    .annotation(
                                        init.span,
                                        format!("type of this expression is {}", expr_type),
                                    ));
                            }
                        } else {
                            resolved_known_type = Some(expr_type);
                        }
                    }
                    // TODO also check here that the type exists in the type table here.
                    self.table.define_value(
                        **symbol,
                        ValueInfo {
                            known_type: resolved_known_type,
                            declaration_span: symbol.span,
                        },
                    )?;
                    last_type = Type::Unit;
                }
            }
        }

        if block.unit_return {
            last_type = Type::Unit;
        }

        Ok(last_type)
    }

    pub fn check_expression(&mut self, expr: &Expression) -> CResult<Type> {
        match &expr.node {
            ExpressionKind::Variable(symbol_id) => {
                let val = self.table.resolve_value(&symbol_id).ok_or_else(|| {
                    CompilerError::new(format!("undefined value {}", symbol_id.as_str()))
                        .annotation(expr.span, "not found in current or any parent scope")
                })?;
                val.known_type.clone().ok_or_else(|| {
                    // TODO improve type inference
                    CompilerError::new(format!("unknown type for variable {}", symbol_id.as_str()))
                        .annotation(expr.span, "type must be known here")
                })
            }
            ExpressionKind::Float(_) => Ok(Type::f64()),
            ExpressionKind::Int(_) => Ok(Type::i64()),
            ExpressionKind::String(_) => Ok(Type::str()),
            ExpressionKind::Boolean(_) => Ok(Type::bool()),
            ExpressionKind::TypeDefinition(_, _) => {
                // NOTE: this was already handled during collection phase - no further processing needed
                Ok(Type::Unit)
            }
            ExpressionKind::FunctionDefinition(function) => {
                let body_type = self.scope(|scope| {
                    for param in &*function.parameters {
                        if let Type::Named(symbol_id) = &*param.r#type {
                            scope.table.resolve_type(&symbol_id).ok_or_else(|| {
                                CompilerError::new(format!("undefined type {}", symbol_id))
                                    .annotation(param.r#type.span, "not defined in this scope")
                            })?;
                        }
                        scope.table.define_value(
                            *param.identifier,
                            ValueInfo {
                                known_type: Some((&*param.r#type).clone()),
                                declaration_span: param.identifier.span,
                            },
                        )?;
                    }
                    scope.check_block(&function.body)
                })?;
                if *function.return_type != body_type {
                    return Err(
                        CompilerError::new("return value type missmatch").annotation(
                            function.return_type.span,
                            format!(
                                "expected return type {}, got {}",
                                *function.return_type, body_type
                            ),
                        ),
                    );
                }
                Ok(Type::Function {
                    parameters: function
                        .parameters
                        .iter()
                        .map(|v| v.r#type.deref().clone())
                        .collect::<Vec<_>>(),
                    return_type: Box::new((*function.return_type).clone()),
                })
            }
            ExpressionKind::Variant(Variant(name, fields)) => {
                Ok(Type::SumType(Vec::from_iter([VariantDefinition(
                    *name,
                    fields
                        .iter()
                        .map(|(k, v)| Ok((*k, self.check_expression(v)?)))
                        .collect::<CResult<Vec<_>>>()?,
                )])))
            }
            ExpressionKind::List(elems) => {
                let expr_types = elems
                    .iter()
                    .map(|e| Ok((self.check_expression(e)?, e.span)))
                    .collect::<CResult<Vec<_>>>()?;
                let mut windows = expr_types.windows(2);
                while let Some([(a_type, a_span), (b_type, b_span)]) = windows.next() {
                    if a_type != b_type {
                        return Err(CompilerError::new("missmatched types")
                            .annotation(*a_span, format!("type of this is {}", a_type))
                            .annotation(*b_span, format!("type of this is {}", b_type)));
                    }
                }
                // TODO support inferring the type of empty arrays
                let known_type = expr_types.first().map(|(t, _)| t).ok_or_else(|| {
                    CompilerError::new("can't infer type of empty array")
                        .annotation(expr.span, "need to include at least one value")
                })?;
                Ok(Type::Array(Box::new(known_type.clone()), elems.len()))
            }
            ExpressionKind::Parenthesis(expr) => self.check_expression(expr),
            ExpressionKind::Unit => Ok(Type::Unit),
            ExpressionKind::Block(block) => self.scope(|scope| scope.check_block(block)),
            ExpressionKind::Conditional(conditional) => {
                let cond_type = self.check_expression(&conditional.condition)?;
                if Type::bool() != cond_type {
                    return Err(CompilerError::new("expected bool").annotation(
                        conditional.condition.span,
                        format!("expected boolean, found {}", cond_type),
                    ));
                }
                let pass_type = self.scope(|scope| scope.check_block(&conditional.pass))?;
                let fail_type = if let Some(fail) = &conditional.fail {
                    self.scope(|scope| scope.check_block(&fail))?
                } else {
                    Type::Unit
                };
                if pass_type != fail_type {
                    return Err(CompilerError::new("type missmatch").annotation(
                        expr.span,
                        format!(
                            "branches of if statement need to return the same type, \
                            currently returns {} when condition is true and {} if condition is false",
                            pass_type, fail_type
                        ),
                    ));
                }
                Ok(pass_type)
            }
            ExpressionKind::UnaryPrefix { op, val } => match **op {
                UnaryPrefixOp::Neg => {
                    let expr_type = self.check_expression(val)?;
                    // TODO make negation work on anything implementing the "Neg" trait!
                    // TODO make nicer way to construct/compare builtins
                    if expr_type != Type::i64() && expr_type != Type::f64() {
                        return Err(CompilerError::new("can't negate non-numeric type")
                            .annotation(val.span, format!("has type {}", expr_type)));
                    }
                    Ok(expr_type)
                }
                UnaryPrefixOp::Not => {
                    let expr_type = self.check_expression(val)?;
                    // TODO make negation work on anything implementing the "Not" trait!
                    if expr_type != Type::bool() {
                        return Err(CompilerError::new("can't not non-bool type")
                            .annotation(val.span, format!("has type {}", expr_type)));
                    }
                    Ok(expr_type)
                }
            },
            ExpressionKind::UnaryPostfix { val, op } => {
                let expr_type = self.check_expression(val)?;
                match &**op {
                    UnaryPostfixOp::Call(call) => {
                        if let Type::Function {
                            parameters,
                            return_type,
                        } = expr_type
                        {
                            let argument_types = call
                                .arguments
                                .iter()
                                .map(|a| Ok((self.check_expression(a)?, a.span)))
                                .collect::<CResult<Vec<_>>>()?;

                            let mut params_itr = parameters.iter();
                            let mut arg_itr = argument_types.iter();
                            loop {
                                match (params_itr.next(), arg_itr.next()) {
                                    (None, Some((_, span))) => {
                                        return Err(CompilerError::new(format!(
                                            "function takes {} arguments, was given {}",
                                            parameters.len(),
                                            argument_types.len()
                                        ))
                                        .annotation(*span, "unexpected argument"));
                                    }
                                    (Some(param), None) => {
                                        return Err(CompilerError::new(
                                            "missing function parameter",
                                        )
                                        .annotation(
                                            expr.span,
                                            format!("expected another parameter of type {}", param),
                                        ));
                                    }
                                    (Some(param), Some((arg, span))) => {
                                        if arg != param {
                                            return Err(CompilerError::new("type missmatch")
                                                .annotation(
                                                    *span,
                                                    format!(
                                                        "expected type {}, found {}",
                                                        param, arg
                                                    ),
                                                ));
                                        }
                                    }
                                    (None, None) => break,
                                }
                            }
                            Ok((*return_type).clone())
                        } else {
                            return Err(CompilerError::new("cannot call non-function type")
                                .annotation(
                                    val.span,
                                    format!("tried to call this, of type {}", expr_type),
                                ));
                        }
                    }
                    UnaryPostfixOp::FieldAccess(field) => {
                        Ok(if let Type::SumType(variants) = &expr_type {
                            if variants.len() != 1 {
                                return Err(CompilerError::new("can't access fields on sumtype with more than one possible variant").annotation(expr.span, format!("type of this expression is {}", expr_type)).annotation(op.span, "tried to access here"));
                            }
                            let VariantDefinition(_, fields) = &variants[0];
                            fields
                                .iter()
                                .find_map(|(f, t)| (f == field).then_some(t))
                                .ok_or_else(|| {
                                    CompilerError::new("no such field").annotation(
                                        expr.span,
                                        format!("this expression is of type {}", expr_type),
                                    )
                                })?
                                .clone()
                        } else {
                            return Err(CompilerError::new(
                                "can't access field on non-struct type",
                            )
                            .annotation(
                                expr.span,
                                format!("this expression has type {}", expr_type),
                            )
                            .annotation(op.span, format!("tried to access field {}", field)));
                        })
                    }
                }
            }
            ExpressionKind::Binary { left, op, right } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                match **op {
                    // Arithmetic operators - require numeric types
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mult
                    | BinaryOp::Div
                    | BinaryOp::Mod
                    | BinaryOp::Exp => {
                        if left_type != Type::i64() && left_type != Type::f64() {
                            return Err(CompilerError::new("expected numeric type").annotation(
                                left.span,
                                format!("type of left operand is {}", left_type),
                            ));
                        }

                        if right_type != Type::i64() && right_type != Type::f64() {
                            return Err(CompilerError::new("expected numeric type").annotation(
                                right.span,
                                format!("type of right operand is {}", right_type),
                            ));
                        }

                        // Both operands must be the same type
                        if left_type != right_type {
                            return Err(CompilerError::new("type mismatch in binary operation")
                                .annotation(
                                    left.span,
                                    format!("left operand has type {}", left_type),
                                )
                                .annotation(
                                    right.span,
                                    format!("right operand has type {}", right_type),
                                ));
                        }

                        Ok(left_type)
                    }

                    // Comparison operators - require same types, return bool
                    BinaryOp::Eq
                    | BinaryOp::NotEq
                    | BinaryOp::LessOrEq
                    | BinaryOp::MoreOrEq
                    | BinaryOp::LargerThan
                    | BinaryOp::SmallerThan => {
                        if left_type != right_type {
                            return Err(CompilerError::new("type mismatch in comparison")
                                .annotation(
                                    left.span,
                                    format!("left operand has type {}", left_type),
                                )
                                .annotation(
                                    right.span,
                                    format!("right operand has type {}", right_type),
                                ));
                        }

                        Ok(Type::bool())
                    }

                    // Logical operators - require bool operands, return bool
                    BinaryOp::Or | BinaryOp::And => {
                        if left_type != Type::bool() {
                            return Err(CompilerError::new("expected boolean type").annotation(
                                left.span,
                                format!("left operand has type {}", left_type),
                            ));
                        }

                        if right_type != Type::bool() {
                            return Err(CompilerError::new("expected boolean type").annotation(
                                right.span,
                                format!("right operand has type {}", right_type),
                            ));
                        }

                        Ok(Type::bool())
                    }

                    // Assignment operator - special case, returns unit
                    BinaryOp::Assign => {
                        // For now, just check that left side is a variable and types match
                        // A more complete implementation would handle field access, array indexing, etc.
                        if let ExpressionKind::Variable(var_id) = &left.node {
                            let var_info = self.table.resolve_value(var_id).ok_or_else(|| {
                                CompilerError::new(format!(
                                    "undefined variable {}",
                                    var_id.as_str()
                                ))
                                .annotation(left.span, "variable not found")
                            })?;

                            if let Some(var_type) = &var_info.known_type {
                                if *var_type != right_type {
                                    return Err(CompilerError::new("type mismatch in assignment")
                                        .annotation(
                                            left.span,
                                            format!("variable has type {}", var_type),
                                        )
                                        .annotation(
                                            right.span,
                                            format!("assigned value has type {}", right_type),
                                        ));
                                }
                            }
                        } else {
                            return Err(CompilerError::new("invalid assignment target")
                                .annotation(left.span, "can only assign to variables for now"));
                        }

                        Ok(Type::Unit)
                    }

                    // Range operator - for now just return unit, could return a range type later
                    BinaryOp::Range => {
                        // Both operands should be integers
                        let i64_type = Type::i64();

                        if left_type != i64_type {
                            return Err(CompilerError::new("range start must be an integer")
                                .annotation(left.span, format!("type is {}", left_type)));
                        }

                        if right_type != i64_type {
                            return Err(CompilerError::new("range end must be an integer")
                                .annotation(right.span, format!("type is {}", right_type)));
                        }

                        // TODO: return a proper Range type when implemented
                        Ok(Type::Unit)
                    }
                }
            }
        }
    }

    pub fn collect_function(&mut self, func: &Function) -> CResult<()> {
        let parameters = func
            .parameters
            .iter()
            .map(|a| a.r#type.deref().clone())
            .collect::<Vec<_>>();

        self.table.define_value(
            *func.name,
            ValueInfo {
                known_type: Some(Type::Function {
                    parameters,
                    return_type: Box::new((*func.return_type).clone()),
                }),
                declaration_span: func.name.span,
            },
        )?;
        Ok(())
    }
}

// Phase 1: Complete Symbol Table Population
// rust// TODO: Collect all top-level declarations first
// - [ ] Add struct/enum/type alias declarations to symbol table
// - [ ] Handle imports/modules if your language has them
// - [ ] Add global constants/variables
// - [ ] Parse and store complex types (arrays, tuples, pointers)
// Phase 2: Type Resolution & Validation
// rust// TODO: Validate all type references exist
// - [ ] Implement `validate()` to check all Type::Named references
// - [ ] Detect circular type dependencies (struct A { b: B }, struct B { a: A })
// - [ ] Resolve type aliases (type X = Y; type Y = i32;)
// - [ ] Check for duplicate definitions across namespaces
// Phase 3: Function Body Analysis
// rust// TODO: Analyze each function body
// - [ ] Create new scope for function body
// - [ ] Add function parameters to scope
// - [ ] Process statements in order
// - [ ] Exit scope after function
// Phase 4: Statement Processing
// rust// TODO: Handle each statement type
// - [ ] Variable declarations (let x: i32 = 5)
//   - [ ] Infer type if not specified
//   - [ ] Check initializer type matches declared type
//   - [ ] Add to current scope
// - [ ] Assignments (x = 10)
//   - [ ] Check variable exists
//   - [ ] Check mutability
//   - [ ] Type check RHS matches LHS
// - [ ] Control flow
//   - [ ] If statements - check condition is bool
//   - [ ] While/for loops - check condition, handle break/continue
//   - [ ] Return statements - check type matches function return
// - [ ] Block statements - create new scope
// Phase 5: Expression Type Checking
// rust// TODO: Implement type checking for all expressions
// - [ ] Literals (infer types: 42 -> i32, 3.14 -> f64)
// - [ ] Variables (lookup in symbol table)
// - [ ] Binary operators
//   - [ ] Arithmetic (+, -, *, /) - check numeric types
//   - [ ] Comparison (<, >, ==) - check compatible types
//   - [ ] Logical (&&, ||) - check bool operands
// - [ ] Unary operators (-, !, etc.)
// - [ ] Function calls
//   - [ ] Resolve function name
//   - [ ] Check argument count
//   - [ ] Check argument types match parameters
// - [ ] Field access (struct.field)
//   - [ ] Expand struct type
//   - [ ] Check field exists
//   - [ ] Return field type
// - [ ] Array indexing
//   - [ ] Check array type
//   - [ ] Check index is integer
// - [ ] Type casts/conversions
// Phase 6: Advanced Semantic Checks
// rust// TODO: Additional validation
// - [ ] Unreachable code detection
// - [ ] Unused variable warnings
// - [ ] Check all paths return a value (for non-void functions)
// - [ ] Initialization before use
// - [ ] Move/borrow checking (if applicable)
// Phase 7: Type Inference
// rust// TODO: Fill in missing types
// - [ ] Local variable type inference (let x = 5)
// - [ ] Generic function instantiation
// - [ ] Closure type inference
// Phase 8: Build Typed AST
// rust// TODO: Construct TAst with all type information
// - [ ] Convert each AST node to typed version
// - [ ] Attach resolved types to all expressions
// - [ ] Store symbol references (for later phases)
// Implementation Structure:
// rustimpl SemanticContext {
//     // Phase 1
//     fn collect_declarations(&mut self, ast: &Ast) -> CResult<()>

//     // Phase 2
//     fn validate_types(&mut self) -> CResult<()>

//     // Phase 3-5
//     fn check_function_body(&mut self, func: &Function) -> CResult<TFunction>
//     fn check_statement(&mut self, stmt: &Statement) -> CResult<TStatement>
//     fn check_expression(&mut self, expr: &Expression) -> CResult<(TExpression, Type)>

//     // Helper
//     fn type_check(&self, expected: &Type, actual: &Type) -> CResult<()>
//     fn expand_type(&self, ty: &Type) -> CResult<ExpandedType>
// }
