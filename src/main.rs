#![allow(clippy::vec_box)]

use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::path::PathBuf;

use anyhow::{Context, Result};
use case::CaseExt;
use clap::Clap;
use codegen::{Field, Module, Struct};
use itertools::Itertools;
use nekoton_utils::NoFailure;
use tap::Pipe;
use ton_abi::{Event, Function, Param, ParamType};

use crate::reserved_words::process_field;

mod reserved_words;

#[derive(Clap)]
struct Args {
    #[clap(short)]
    data_path: PathBuf,
    #[clap(short)]
    out_path: Option<PathBuf>,
    #[clap(short)]
    /// Run or not cargo fmt
    fmt: bool,
}

fn main() -> Result<()> {
    let args: Args = Args::try_parse()?;
    let mut abi = std::fs::File::open(&args.data_path).unwrap();
    let contract_name = args
        .data_path
        .file_name()
        .expect("No filename in contract path")
        .to_string_lossy();

    let contract_name = contract_name
        .strip_suffix(".json")
        .expect("no json in abi name")
        .replace(|x| !char::is_alphanumeric(x), "_")
        .to_camel();
    let result = generate_contract_binding(&mut abi, &contract_name)?;

    match args.out_path {
        None => {
            println!("{}", result);
        }
        Some(a) => {
            std::fs::write(&a, result)?;
            if args.fmt {
                std::process::Command::new("rustfmt").args(&[a]).spawn()?;
            }
        }
    };
    Ok(())
}

fn generate_contract_binding(fh: &mut dyn std::io::Read, contract_name: &str) -> Result<String> {
    let data = ton_abi::Contract::load(fh)
        .convert()
        .context("Failed parsing json as contract")?;
    let mut scope = codegen::Scope::new();

    let functions = data.functions();
    if !functions.is_empty() {
        let functions = functions_gen(BTreeMap::from_iter(functions.clone()), &contract_name)?;
        *scope.new_module("functions") = functions;
    }
    let events = data.events();
    if !events.is_empty() {
        let events = events_gen(BTreeMap::from_iter(events.clone()), contract_name)?;
        *scope.new_module("events") = events;
    }
    Ok(scope.to_string())
}

fn module_imports(mut module: Module) -> Module {
    module
        .vis("pub")
        .import("serde", "Serialize")
        .import("serde", "Deserialize")
        .import("nekoton_abi", "UnpackAbi")
        .import("nekoton_abi", "UnpackAbiPlain")
        .import("nekoton_abi", "PackAbi")
        .import("nekoton_abi", "UnpackToken")
        .import("nekoton_abi", "UnpackerError")
        .import("nekoton_abi", "UnpackerResult")
        .import("nekoton_abi", "BuildTokenValue")
        .import("nekoton_abi", "TokenValueExt")
        .import("ton_abi", "Param")
        .import("ton_abi", "ParamType")
        .import("std::collections", "HashMap")
        .import("once_cell::sync", "OnceCell")
        .clone()
}

fn events_gen(input: BTreeMap<String, Event>, contract_name: &str) -> Result<Module> {
    let mut md = Module::new("events");
    let mut struct_id = 0;
    let mut structs = vec![];
    let mut events = vec![];
    for (name, event) in input {
        if event.inputs.is_empty() {
            continue;
        }
        for strct in gen_struct(&event.inputs, name.clone() + "Input", &mut struct_id)? {
            structs.push(strct);
        }
        events.push(event_impl(event));
    }
    struct_impl(contract_name, events, &mut md);
    dedup(&mut md, &mut structs);
    Ok(module_imports(md)
        .import("nekoton_abi", "EventBuilder")
        .clone())
}

fn params_to_string(params: Vec<Param>) -> String {
    fn param_to_string(param: ParamType, name: &str) -> Vec<String> {
        let res = match param.clone() {
            ParamType::Tuple(a) => {
                return a
                    .into_iter()
                    .map(|x| param_to_string(x.kind, &x.name))
                    .flatten()
                    .collect()
            }
            ParamType::Array(a) => {
                let ty = param_to_string(*a, "");
                format!(
                    "Param{{name: \"{}\".to_string(), kind: ParamType::Array(Box::new({}))}}",
                    name,
                    ty.first().unwrap()
                )
            }
            ParamType::FixedArray(a, _) => {
                let ty = param_to_string(*a, "");
                format!(
                    "Param{{name: \"{}\".to_string(), kind: ParamType::FixedArray(Box::new({}))}}",
                    name,
                    ty.first().unwrap()
                )
            }
            ParamType::Map(a, b) => {
                let a = param_to_string(*a, "");
                let b = param_to_string(*b, "");
                format!(
                    "Param{{name: \"{}\".to_string(), kind: ParamType::Map(Box::new({}), Box::new({}))}}",
                    name,
                    a.first().unwrap(),
                    b.first().unwrap()
                )
            }
            _ => {
                if name.is_empty() {
                    format!("ParamType::{:?}", param)
                } else {
                    format!(
                        "Param{{name: \"{}\".to_string(), kind: ParamType::{:?}}}",
                        name, param
                    )
                }
            }
        };
        vec![res]
    }
    let mut res = "vec![".to_string();
    let joined = params
        .into_iter()
        .map(|x| param_to_string(x.kind, &x.name))
        .flatten()
        .join(",");
    res += &joined;
    res += "];";
    res
}

fn function_impl(function: Function) -> codegen::Function {
    let mut fun = codegen::Function::new(&function.name.to_snake())
        .vis("pub")
        .ret("ton_abi::Function")
        .line("static FUNCTION: OnceCell<ton_abi::Function> = OnceCell::new();")
        .line("FUNCTION.get_or_init(|| ({")
        .clone();

    if !(function.inputs.is_empty() && function.outputs.is_empty()) {
        fun = fun
            .line(format!(
                "let mut builder = FunctionBuilder::new(\"{}\");",
                function.name
            ))
            .clone();
    }
    if !function.inputs.is_empty() {
        let mut res = "let input = ".to_string();
        res += &params_to_string(function.inputs);
        fun = fun.line(res).clone();
        fun = fun.line("builder = builder.inputs(input);").clone();
    }

    if !function.outputs.is_empty() {
        let mut res = "let output = ".to_string();
        res += &params_to_string(function.outputs);
        fun = fun.line(res).clone();
        fun = fun.line("builder = builder.outputs(output);").clone();
    }
    fun.line("builder.build()").line("})").clone()
}

fn event_impl(event: Event) -> codegen::Function {
    let mut fun = codegen::Function::new(&event.name.to_snake())
        .vis("pub")
        .ret("ton_abi::Event")
        .line("static EVENT: OnceCell<ton_abi::Event> = OnceCell::new();")
        .line("EVENT.get_or_init(|| ({")
        .clone();

    if !(event.inputs.is_empty()) {
        fun = fun
            .line(format!(
                "let mut builder = EventBuilder::new(\"{}\");",
                event.name
            ))
            .clone();
        let mut res = "let input = ".to_string();
        res += &params_to_string(event.inputs);
        fun = fun.line(res).clone();
        fun = fun.line("builder = builder.inputs(input);").clone();
    }
    fun.line("builder.build()").line("})").clone()
}

fn functions_gen(input: BTreeMap<String, Function>, contract_name: &str) -> Result<Module> {
    let mut md = Module::new("functions");
    let mut struct_id = 0;
    let mut structs = vec![];
    let mut funs = vec![];
    for (name, fun) in input {
        if fun.inputs.is_empty() && fun.outputs.is_empty() {
            continue;
        }
        funs.push(function_impl(fun.clone()));
        for strct in gen_struct(&fun.inputs, name.clone() + "Input", &mut struct_id)? {
            structs.push(strct);
        }
        for strct in gen_struct(&fun.outputs, name + "Output", &mut struct_id)? {
            structs.push(strct);
        }
    }
    struct_impl(contract_name, funs, &mut md);
    dedup(&mut md, &mut structs);

    Ok(module_imports(md)
        .import("nekoton_abi", "FunctionBuilder")
        .clone())
}

fn struct_impl(struct_name: &str, impls: Vec<codegen::Function>, md: &mut Module) {
    let str = codegen::Struct::new(struct_name)
        .vis("pub")
        .derive("Copy, Clone, Debug")
        .clone();
    *md = md.push_struct(str.clone()).clone();
    let mut struct_impl = codegen::Impl::new(str.ty());
    for fun in impls {
        struct_impl = struct_impl.push_fn(fun).clone();
    }
    *md = md.push_impl(struct_impl).clone();
}

/// Searching structs with the same args and merging them
fn dedup(md: &mut Module, structs: &mut Vec<StructData>) {
    let mut known_types = BTreeMap::new();

    let mut deduped_structs = vec![];
    let mut tuple_structs = BTreeMap::new(); //Map of same tuples. Like Tuple1(int) -> Tuple2(int)
    let mut tuple_ctr = 0;
    for str in structs {
        let name = struct_name(&str.str);
        if !name.contains("TupleStruct") {
            if str.types.is_empty() {
                continue;
            }
            deduped_structs.push(str.clone());
            continue;
        }
        let new_str = known_types.entry(str.types.clone()).or_insert_with(|| {
            construct_struct(str.types.clone(), &format!("TupleStruct{}", tuple_ctr))
        });
        tuple_ctr += 1;
        tuple_structs.insert(name, new_str.clone());
    }
    for str in &mut deduped_structs {
        str.types
            .iter_mut()
            .for_each(|x| match tuple_structs.get(&x.1) {
                None => (),
                Some(ty) => {
                    x.1 = struct_name(ty);
                }
            });
    }
    tuple_structs
        .values()
        .into_iter()
        .sorted_by(|a, b| struct_name(a).cmp(&struct_name(b)))
        .dedup_by(|a, b| struct_name(a).eq(&struct_name(b)))
        .for_each(|x| {
            *md = md.push_struct(x.clone()).clone();
        });

    deduped_structs
        .into_iter()
        .map(|x| construct_struct(x.types, &struct_name(&x.str)))
        .for_each(|x| {
            *md = md.push_struct(x).clone();
        });
}

fn construct_struct(types: Vec<(String, String)>, name: &str) -> Struct {
    let mut str = Struct::new(&name.to_camel());
    for (name, ty) in types {
        let annotation = format!("#[abi(name = \"{}\")]", name);
        let field_name = name
            .strip_prefix("_")
            .unwrap_or_else(|| name.as_str())
            .pipe(|x| process_field(x.to_string()))
            .to_snake();
        let field = "pub ".to_string() + &field_name;
        let mut field = if field_name != name {
            Field::new(&field, &ty)
                .annotation(vec![annotation.as_str()])
                .clone()
        } else {
            Field::new(&field, &ty).clone()
        };
        if &ty == "ton_types::Cell" {
            field = field
                .annotation(vec!["#[serde(with = \"nekoton_utils::serde_cell\")]"])
                .clone();
        }
        if &ty == "ton_block::MsgAddressInt" {
            field = field
                .annotation(vec!["#[serde(with = \"nekoton_utils::serde_address\")]"])
                .clone();
        }
        str = str.push_field(field).clone();
    }
    str = str
        .derive("Serialize")
        .derive("Deserialize")
        .derive("Debug")
        .derive("Clone")
        .derive("PackAbi")
        .vis("pub")
        .clone();
    if name.contains("Output") {
        str.derive("UnpackAbiPlain").clone()
    } else {
        str.derive("UnpackAbi").clone()
    }
}

#[derive(Debug)]
enum MappedType {
    Ty {
        ty: String,
        name: String,
    },
    Tuple {
        types: Vec<Box<MappedType>>,
        name: String,
    },
}

fn struct_name(str: &Struct) -> String {
    let mut ty = String::new();
    let mut formatter = codegen::Formatter::new(&mut ty);
    str.ty().fmt(&mut formatter).unwrap();
    ty
}

fn map(types: Vec<Box<MappedType>>, id: &mut usize) -> Vec<StructData> {
    let mut structs = vec![];
    let mut strct = Struct::new(&format!("TupleStruct{}", id));
    let mut struct_types = vec![];
    *id += 1;
    for ty in types {
        match *ty {
            MappedType::Ty { ty, name } => {
                struct_types.push((name.clone(), ty.clone()));
                strct = strct.push_field(Field::new(&name, ty)).clone();
            }
            MappedType::Tuple { types, name } => {
                let res = map(types, id);
                let mapped_struct = res.last().unwrap();

                let ty = struct_name(&mapped_struct.str);

                struct_types.push((name.clone(), ty));
                strct = strct
                    .push_field(Field::new(&name, mapped_struct.str.ty()))
                    .clone();
                structs.extend(&mut res.into_iter());
            }
        }
    }
    structs.push(StructData {
        str: strct,
        types: struct_types,
    });
    structs
}

#[derive(Clone)]
struct StructData {
    str: Struct,
    /// name, type
    types: Vec<(String, String)>,
}

impl StructData {
    fn new(str: Struct, types: Vec<(String, String)>) -> Self {
        Self { str, types }
    }
}

fn gen_struct(types: &[Param], name: String, struct_id: &mut usize) -> Result<Vec<StructData>> {
    let mut res = vec![];
    let mut str = Struct::new(&name);
    let mut struct_types: Vec<(String, String)> = vec![];
    for param in types {
        let mapped_types = map_ton_types(param.kind.clone(), param.name.clone())?;
        match mapped_types {
            MappedType::Ty { ty, name } => {
                struct_types.push((name.to_string(), ty.clone()));
                str = str.field(&name, ty).clone();
            }
            MappedType::Tuple { types, name } => {
                let ty = map(types, struct_id);
                res.extend(ty.clone());

                let mapped_struct = ty.last().unwrap();
                str = str.field(&name, mapped_struct.str.ty()).clone();

                let ty = struct_name(&mapped_struct.str);
                struct_types.push((name, ty.clone()));
            }
        };
    }
    res.push(StructData::new(str, struct_types));
    Ok(res)
}

fn map_ton_types(ty: ParamType, field_name: String) -> Result<MappedType> {
    let ty = match ty {
        ParamType::Unknown => {
            anyhow::bail!("Bad type")
        }
        ParamType::Uint(a) => match a {
            8 => "u8",
            16 => "u16",
            32 => "u32",
            64 => "u64",
            128 => "num_bigint::BigUint",
            160 => "num_bigint::BigUint",
            256 => "ton_types::UInt256",
            _ => "num_bigint::BigUint",
        }
        .to_string(),
        ParamType::Int(a) => match a {
            8 => "i8",
            16 => "i16",
            32 => "i32",
            64 => "i64",
            128 => "i128",
            _ => "num_bigint::BigInt",
        }
        .to_string(),
        ParamType::Bool => "bool".to_string(),
        ParamType::Tuple(a) => {
            let ty: Vec<Box<MappedType>> = a
                .into_iter()
                .map(|x| map_ton_types(x.kind, x.name).unwrap())
                .map(Box::new)
                .collect();
            return Ok(MappedType::Tuple {
                types: ty,
                name: field_name,
            });
        }
        ParamType::Array(a) => return map_ton_types(*a, field_name),
        ParamType::FixedArray(a, _) => return map_ton_types(*a, field_name),
        ParamType::Cell => "ton_types::Cell".to_string(),
        ParamType::Map(a, b) => {
            let a = match map_ton_types(*a, field_name.clone()).unwrap() {
                MappedType::Ty { ty, .. } => (ty),
                MappedType::Tuple { .. } => {
                    todo!()
                }
            };
            let b = match map_ton_types(*b, field_name.clone()).unwrap() {
                MappedType::Ty { ty, .. } => (ty),
                MappedType::Tuple { .. } => {
                    todo!()
                }
            };
            format!("HashMap<{},{}>", a, b)
        }
        ParamType::Address => "ton_block::MsgAddressInt".to_string(),
        ParamType::Bytes => "Vec<u8>".to_string(),
        ParamType::FixedBytes(_) => "Vec<u8>".to_string(), //todo array?
        ParamType::Gram => "ton_block::Grams".to_string(),
        ParamType::Time => "u64".to_string(),
        ParamType::Expire => "u32".to_string(),
        ParamType::PublicKey => "ed25519_dalek::PublicKey".to_string(),
    };
    Ok(MappedType::Ty {
        ty,
        name: field_name,
    })
}

#[cfg(test)]
mod test {
    // use pretty_assertions::assert_eq;
    #[test]
    fn test_abi() {
        let abi = include_str!("../test/SafeMultisigWallet.abi.json");
        let mut fh = std::io::Cursor::new(abi);
        let res = super::generate_contract_binding(&mut fh, "SafeMultisigWalletAbi").unwrap();
        let expected = include_str!("../test/test.txt");
        assert_eq!(res, expected);
    }
}
