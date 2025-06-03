use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{ BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue };
use inkwell::{ IntPredicate };
use inkwell::targets::{
    CodeModel,
    FileType,
    InitializationConfig,
    RelocMode,
    Target,
    TargetMachine,
};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

use crate::parser::ast::*;

#[derive(Debug, Clone)]
struct VariableScope<'ctx> {
    variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> VariableScope<'ctx> {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn insert(&mut self, name: String, ptr: PointerValue<'ctx>) {
        self.variables.insert(name, ptr);
    }

    fn get(&self, name: &str) -> Option<&PointerValue<'ctx>> {
        self.variables.get(name)
    }
}

struct ScopeStack<'ctx> {
    scopes: Vec<VariableScope<'ctx>>,
}

impl<'ctx> ScopeStack<'ctx> {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(VariableScope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert_variable(&mut self, name: String, ptr: PointerValue<'ctx>) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, ptr);
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<PointerValue<'ctx>> {
        for scope in self.scopes.iter().rev() {
            if let Some(ptr) = scope.get(name) {
                return Some(*ptr);
            }
        }
        None
    }
}

pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    variable_scopes: ScopeStack<'ctx>,
    functions: HashMap<String, FunctionValue<'ctx>>,

    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module_name: &str
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Ok(CodeGenerator {
            context,
            module,
            builder,
            variable_scopes: ScopeStack::new(),
            functions: HashMap::new(),
            current_function: None,
        })
    }

    pub fn generate(&mut self, program: &Program) -> Result<(), Box<dyn std::error::Error>> {
        for func in &program.functions {
            self.declare_function(func)?;
        }

        for func in &program.functions {
            self.implement_function(func)?;
        }

        Ok(())
    }

    fn declare_function(
        &mut self,
        func: &FunctionDeclaration
    ) -> Result<(), Box<dyn std::error::Error>> {
        let i64_type = self.context.i64_type();

        let param_types: Vec<BasicMetadataTypeEnum> = func.arguments
            .iter()
            .map(|_| i64_type.into())
            .collect();

        let fn_type = if func.returns_value {
            i64_type.fn_type(&param_types, false)
        } else {
            self.context.void_type().fn_type(&param_types, false)
        };

        let function = self.module.add_function(&func.name, fn_type, None);

        for (i, arg_name) in func.arguments.iter().enumerate() {
            function
                .get_nth_param(i as u32)
                .unwrap()
                .set_name(arg_name);
        }

        self.functions.insert(func.name.clone(), function);
        Ok(())
    }

    fn implement_function(
        &mut self,
        func: &FunctionDeclaration
    ) -> Result<(), Box<dyn std::error::Error>> {
        let function = *self.functions.get(&func.name).unwrap();
        self.current_function = Some(function);

        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);

        self.variable_scopes = ScopeStack::new();
        self.variable_scopes.push_scope();

        for (i, arg_name) in func.arguments.iter().enumerate() {
            let param = function
                .get_nth_param(i as u32)
                .unwrap()
                .into_int_value();
            let alloca = self.builder.build_alloca(self.context.i64_type(), arg_name)?;
            self.builder.build_store(alloca, param)?;
            self.variable_scopes.insert_variable(arg_name.clone(), alloca);
        }

        if let Some(body) = &func.body {
            self.generate_code_block(body)?;
        }

        if !func.returns_value {
            self.builder.build_return(None)?;
        }

        self.variable_scopes.pop_scope();

        Ok(())
    }

    fn generate_code_block(&mut self, block: &CodeBlock) -> Result<(), Box<dyn std::error::Error>> {
        self.variable_scopes.push_scope();

        for statement in &block.0 {
            self.generate_statement(statement)?;
        }

        self.variable_scopes.pop_scope();
        Ok(())
    }

    fn generate_statement(&mut self, stmt: &Statement) -> Result<(), Box<dyn std::error::Error>> {
        match stmt {
            Statement::Expression { expression } => {
                self.generate_expression(expression)?;
            }

            Statement::VariableDeclaration { name, expression } => {
                let alloca = self.builder.build_alloca(self.context.i64_type(), name)?;

                if let Some(expr) = expression {
                    let value = self.generate_expression(expr)?;
                    self.builder.build_store(alloca, value)?;
                } else {
                    let zero = self.context.i64_type().const_int(0, false);
                    self.builder.build_store(alloca, zero)?;
                }

                self.variable_scopes.insert_variable(name.clone(), alloca);
            }

            Statement::VariableAssignment { name, expression } => {
                let value = self.generate_expression(expression)?;
                let var_ptr = self.variable_scopes
                    .lookup_variable(name)
                    .ok_or(format!("Undefined variable: {}", name))?;
                self.builder.build_store(var_ptr, value)?;
            }

            Statement::IfStatement { condition, then_block, else_block } => {
                let condition_value = self.generate_expression(condition)?;
                let zero = self.context.i64_type().const_int(0, false);
                let condition_bool = self.builder.build_int_compare(
                    IntPredicate::NE,
                    condition_value,
                    zero,
                    "if_cond"
                )?;

                let current_fn = self.current_function.unwrap();
                let then_bb = self.context.append_basic_block(current_fn, "then");
                let else_bb = self.context.append_basic_block(current_fn, "else");
                let merge_bb = self.context.append_basic_block(current_fn, "merge");

                self.builder.build_conditional_branch(condition_bool, then_bb, else_bb)?;

                self.builder.position_at_end(then_bb);
                self.generate_code_block(then_block)?;
                self.builder.build_unconditional_branch(merge_bb)?;

                self.builder.position_at_end(else_bb);
                if let Some(else_block) = else_block {
                    self.generate_code_block(else_block)?;
                }
                self.builder.build_unconditional_branch(merge_bb)?;

                self.builder.position_at_end(merge_bb);
            }

            Statement::WhileStatement { condition, body } => {
                let current_fn = self.current_function.unwrap();
                let loop_bb = self.context.append_basic_block(current_fn, "loop");
                let body_bb = self.context.append_basic_block(current_fn, "loop_body");
                let end_bb = self.context.append_basic_block(current_fn, "loop_end");

                self.builder.build_unconditional_branch(loop_bb)?;

                self.builder.position_at_end(loop_bb);
                let condition_value = self.generate_expression(condition)?;
                let zero = self.context.i64_type().const_int(0, false);
                let condition_bool = self.builder.build_int_compare(
                    IntPredicate::NE,
                    condition_value,
                    zero,
                    "while_cond"
                )?;
                self.builder.build_conditional_branch(condition_bool, body_bb, end_bb)?;

                self.builder.position_at_end(body_bb);
                self.generate_code_block(body)?;
                self.builder.build_unconditional_branch(loop_bb)?;

                self.builder.position_at_end(end_bb);
            }

            Statement::ForStatement { initializer, condition, increment, body } => {
                self.variable_scopes.push_scope();

                self.generate_statement(initializer)?;

                let current_fn = self.current_function.unwrap();
                let loop_bb = self.context.append_basic_block(current_fn, "for_loop");
                let body_bb = self.context.append_basic_block(current_fn, "for_body");
                let increment_bb = self.context.append_basic_block(current_fn, "for_increment");
                let end_bb = self.context.append_basic_block(current_fn, "for_end");

                self.builder.build_unconditional_branch(loop_bb)?;

                self.builder.position_at_end(loop_bb);
                let condition_value = self.generate_expression(condition)?;
                let zero = self.context.i64_type().const_int(0, false);
                let condition_bool = self.builder.build_int_compare(
                    IntPredicate::NE,
                    condition_value,
                    zero,
                    "for_cond"
                )?;
                self.builder.build_conditional_branch(condition_bool, body_bb, end_bb)?;

                self.builder.position_at_end(body_bb);
                self.generate_code_block(body)?;
                self.builder.build_unconditional_branch(increment_bb)?;

                self.builder.position_at_end(increment_bb);
                self.generate_statement(increment)?;
                self.builder.build_unconditional_branch(loop_bb)?;

                self.builder.position_at_end(end_bb);

                self.variable_scopes.pop_scope();
            }

            Statement::Return { value } => {
                if let Some(expr) = value {
                    let return_value = self.generate_expression(expr)?;
                    self.builder.build_return(Some(&return_value))?;
                } else {
                    self.builder.build_return(None)?;
                }
            }
        }

        Ok(())
    }

    fn generate_expression(
        &mut self,
        expr: &Expression
    ) -> Result<IntValue<'ctx>, Box<dyn std::error::Error>> {
        match expr {
            Expression::Constant(Constant::Integer(value)) => {
                Ok(self.context.i64_type().const_int(*value as u64, false))
            }

            Expression::Variable(name) => {
                let var_ptr = self.variable_scopes
                    .lookup_variable(name)
                    .ok_or(format!("Undefined variable: {}", name))?;
                Ok(
                    self.builder
                        .build_load(self.context.i64_type(), var_ptr, name)?
                        .into_int_value()
                )
            }

            Expression::BinaryExpression { left, operator, right } => {
                let left_val = self.generate_expression(left)?;
                let right_val = self.generate_expression(right)?;

                match operator {
                    Operator::Add => Ok(self.builder.build_int_add(left_val, right_val, "add")?),
                    Operator::Sub => Ok(self.builder.build_int_sub(left_val, right_val, "sub")?),
                    Operator::Mul => Ok(self.builder.build_int_mul(left_val, right_val, "mul")?),
                    Operator::Div =>
                        Ok(self.builder.build_int_signed_div(left_val, right_val, "div")?),
                    Operator::Mod =>
                        Ok(self.builder.build_int_signed_rem(left_val, right_val, "mod")?),
                    Operator::And => Ok(self.builder.build_and(left_val, right_val, "and")?),
                    Operator::Or => Ok(self.builder.build_or(left_val, right_val, "or")?),
                    Operator::Equal => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            left_val,
                            right_val,
                            "eq"
                        )?;
                        Ok(self.builder.build_int_z_extend(cmp, self.context.i64_type(), "eq_ext")?)
                    }
                    Operator::NotEqual => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::NE,
                            left_val,
                            right_val,
                            "ne"
                        )?;
                        Ok(self.builder.build_int_z_extend(cmp, self.context.i64_type(), "ne_ext")?)
                    }
                    Operator::LessThan => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::SLT,
                            left_val,
                            right_val,
                            "lt"
                        )?;
                        Ok(self.builder.build_int_z_extend(cmp, self.context.i64_type(), "lt_ext")?)
                    }
                    Operator::GreaterThan => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::SGT,
                            left_val,
                            right_val,
                            "gt"
                        )?;
                        Ok(self.builder.build_int_z_extend(cmp, self.context.i64_type(), "gt_ext")?)
                    }
                    Operator::LessThanEqual => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::SLE,
                            left_val,
                            right_val,
                            "le"
                        )?;
                        Ok(self.builder.build_int_z_extend(cmp, self.context.i64_type(), "le_ext")?)
                    }
                    Operator::GreaterThanEqual => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::SGE,
                            left_val,
                            right_val,
                            "ge"
                        )?;
                        Ok(self.builder.build_int_z_extend(cmp, self.context.i64_type(), "ge_ext")?)
                    }
                    _ => Err(format!("Unsupported binary operator: {:?}", operator).into()),
                }
            }

            Expression::UnaryExpression { operator, expression } => {
                let val = self.generate_expression(expression)?;

                match operator {
                    Operator::Negate => {
                        let zero = self.context.i64_type().const_int(0, false);
                        Ok(self.builder.build_int_sub(zero, val, "neg")?)
                    }
                    Operator::Not => {
                        let zero = self.context.i64_type().const_int(0, false);
                        let is_zero = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            val,
                            zero,
                            "is_zero"
                        )?;
                        Ok(
                            self.builder.build_int_z_extend(
                                is_zero,
                                self.context.i64_type(),
                                "not"
                            )?
                        )
                    }
                    _ => Err(format!("Unsupported unary operator: {:?}", operator).into()),
                }
            }

            Expression::TernaryExpression { condition, true_expression, false_expression } => {
                let condition_val = self.generate_expression(condition)?;
                let zero = self.context.i64_type().const_int(0, false);
                let condition_bool = self.builder.build_int_compare(
                    IntPredicate::NE,
                    condition_val,
                    zero,
                    "ternary_cond"
                )?;

                let current_fn = self.current_function.unwrap();
                let then_bb = self.context.append_basic_block(current_fn, "ternary_then");
                let else_bb = self.context.append_basic_block(current_fn, "ternary_else");
                let merge_bb = self.context.append_basic_block(current_fn, "ternary_merge");

                self.builder.build_conditional_branch(condition_bool, then_bb, else_bb)?;

                self.builder.position_at_end(then_bb);
                let then_val = self.generate_expression(true_expression)?;
                self.builder.build_unconditional_branch(merge_bb)?;
                let then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                let else_val = self.generate_expression(false_expression)?;
                self.builder.build_unconditional_branch(merge_bb)?;
                let else_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(merge_bb);
                let phi = self.builder.build_phi(self.context.i64_type(), "ternary_result")?;
                phi.add_incoming(
                    &[
                        (&then_val, then_bb),
                        (&else_val, else_bb),
                    ]
                );

                Ok(phi.as_basic_value().into_int_value())
            }

            Expression::FunctionCall { function_name, arguments } => {
                let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                for arg in arguments {
                    let arg_val = self.generate_expression(arg)?;
                    args.push(arg_val.into());
                }

                let function = self.functions
                    .get(function_name)
                    .ok_or(format!("Undefined function: {}", function_name))?;

                let call_result = self.builder.build_call(*function, &args, "call")?;

                if let Some(result) = call_result.try_as_basic_value().left() {
                    Ok(result.into_int_value())
                } else {
                    Ok(self.context.i64_type().const_int(0, false))
                }
            }
        }
    }

    pub fn write_object_file(
        &self,
        output_path: &str,
        output_extension: Option<&str>,
        produce_ll: bool
    ) -> Result<(), Box<dyn std::error::Error>> {
        Target::initialize_native(&InitializationConfig::default())?;

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)?;

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default
            )
            .ok_or("Could not create target machine")?;

        let obj_path = match output_extension {
            Some(ext) => format!("{}.{}", output_path, ext),
            None => format!("{}.o", output_path),
        };
        target_machine.write_to_file(&self.module, FileType::Object, Path::new(&obj_path))?;

        println!("Object file written to: {}", obj_path);

        if produce_ll {
            let ir_path = format!("{}.ll", output_path);
            self.module.print_to_file(&ir_path)?;
            println!("LLVM IR written to: {}", ir_path);
        }

        Ok(())
    }

    pub fn write_executable(
        &self,
        output_path: &str,
        output_extension: Option<&str>,
        produce_ll: bool
    ) -> Result<(), Box<dyn std::error::Error>> {
        Target::initialize_native(&InitializationConfig::default())?;

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)?;

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default
            )
            .ok_or("Could not create target machine")?;

        let obj_path = format!("{}.o", output_path);
        target_machine.write_to_file(&self.module, FileType::Object, Path::new(&obj_path))?;

        let output_file = match output_extension {
            Some(ext) => format!("{}.{}", output_path, ext),
            None => String::from(output_path),
        };
        self.link_executable(&obj_path, output_file)?;

        if produce_ll {
            let ir_path = format!("{}.ll", output_path);
            self.module.print_to_file(&ir_path)?;
            println!("LLVM IR written to: {}", ir_path);
        }

        std::fs::remove_file(&obj_path).ok();

        Ok(())
    }

    fn link_executable(
        &self,
        obj_path: &str,
        output_path: String
    ) -> Result<(), Box<dyn std::error::Error>> {
        use std::process::Command;

        let linkers = [
            ("clang", vec![obj_path, "-o", output_path.as_str()]),
            ("gcc", vec![obj_path, "-o", output_path.as_str()]),
            ("ld", vec![obj_path, "-o", output_path.as_str(), "-lc", "-e", "main"]),
        ];

        for (linker, args) in &linkers {
            if let Ok(output) = Command::new(linker).args(args).output() {
                if output.status.success() {
                    println!("Successfully linked with {}: {}", linker, output_path);
                    return Ok(());
                }
            }
        }

        Err(
            "No suitable linker found. Please install clang, gcc, or ensure ld is available.".into()
        )
    }

    pub fn compile_to_assembly(
        &self,
        output_path: &str,
        output_extension: Option<&str>,
        produce_ll: bool
    ) -> Result<(), Box<dyn std::error::Error>> {
        Target::initialize_native(&InitializationConfig::default())?;

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)?;

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default
            )
            .ok_or("Could not create target machine")?;

        let asm_path = match output_extension {
            Some(ext) => format!("{}.{}", output_path, ext),
            None => format!("{}.s", output_path),
        };
        target_machine.write_to_file(&self.module, FileType::Assembly, Path::new(&asm_path))?;

        if produce_ll {
            let ir_path = format!("{}.ll", output_path);
            self.module.print_to_file(&ir_path)?;
            println!("LLVM IR written to: {}", ir_path);
        }

        println!("Assembly written to: {}", asm_path);
        Ok(())
    }
}

pub fn compile_to_object(
    program: &Program,
    output_path: &str,
    output_extension: Option<&str>,
    produce_ll: bool
) -> Result<(), Box<dyn std::error::Error>> {
    let context = Context::create();
    let mut codegen = CodeGenerator::new(&context, "main")?;

    codegen.generate(program)?;
    codegen.write_object_file(output_path, output_extension, produce_ll)?;

    let obj_path = match output_extension {
        Some(ext) => format!("{}.{}", output_path, ext),
        None => format!("{}.o", output_path),
    };

    println!("Successfully compiled to object file: {}", obj_path);
    Ok(())
}

pub fn compile_to_executable(
    program: &Program,
    output_path: &str,
    output_extension: Option<&str>,
    produce_ll: bool
) -> Result<(), Box<dyn std::error::Error>> {
    let context = Context::create();
    let mut codegen = CodeGenerator::new(&context, "main")?;

    codegen.generate(program)?;
    codegen.write_executable(output_path, output_extension, produce_ll)?;

    let exe_path = match output_extension {
        Some(ext) => format!("{}.{}", output_path, ext),
        None => format!("{}", output_path),
    };

    println!("Successfully compiled to executable: {}", exe_path);
    Ok(())
}

pub fn compile_to_assembly(
    program: &Program,
    output_path: &str,
    output_extension: Option<&str>,
    produce_ll: bool
) -> Result<(), Box<dyn std::error::Error>> {
    let context = Context::create();
    let mut codegen = CodeGenerator::new(&context, "main")?;

    codegen.generate(program)?;
    codegen.compile_to_assembly(output_path, output_extension, produce_ll)?;

    let asm_path = match output_extension {
        Some(ext) => format!("{}.{}", output_path, ext),
        None => format!("{}.s", output_path),
    };

    println!("Successfully compiled to assembly: {}", asm_path);
    Ok(())
}
