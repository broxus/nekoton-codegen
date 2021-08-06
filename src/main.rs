#![allow(clippy::vec_box)]

use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use case::CaseExt;
use clap::Clap;
use codegen::{Field, Module, Scope, Struct};
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
    /// output dir path
    out_path: Option<PathBuf>,
    #[clap(short)]
    /// Run or not cargo fmt
    fmt: bool,
}

fn main() -> Result<()> {
    let args: Args = Args::try_parse()?;
    let mut models = Vec::new();
    let mut impls = Vec::new();
    let mut scopes = BTreeMap::new();
    let mut struct_id = 0;
    match &args.out_path {
        None => {}
        Some(a) => {
            std::fs::create_dir_all(a.join("abi"))?;
            let mut imports = Module::new("")
                .import("serde", "Serialize")
                .import("serde", "Deserialize")
                .import("nekoton_abi", "UnpackAbi")
                .import("nekoton_abi", "UnpackAbiPlain")
                .import("nekoton_abi", "PackAbi")
                .import("nekoton_abi", "UnpackerError")
                .import("nekoton_abi", "UnpackerResult")
                .import("nekoton_abi", "BuildTokenValue")
                .import("nekoton_abi", "TokenValueExt")
                .import("ton_abi", "Param")
                .import("ton_abi", "ParamType")
                .import("std::collections", "HashMap")
                .import("once_cell::sync", "OnceCell")
                .clone();
            let str = imports.scope().to_string();
            let str = str
                .split('\n')
                .filter(|x| !x.is_empty())
                .map(|x| "pub ".to_string() + x)
                .join("\n");
            std::fs::write(a.join("prelude.rs"), str)?;
        }
    }
    for p in std::fs::read_dir(args.data_path)? {
        let p = p?;
        let meta = p.metadata()?;
        if meta.is_dir() {
            continue;
        }
        let contract_name = p.file_name().to_string_lossy().to_string();
        let contract_name = match contract_name.strip_suffix(".json") {
            None => continue,
            Some(a) => a,
        };
        match &args.out_path {
            None => {}
            Some(a) => {
                std::fs::copy(p.path(), a.join("abi").join(p.file_name())).context("Can't copy")?;
            }
        };
        let contract_name = contract_name
            .replace(|x| !char::is_alphanumeric(x), "_")
            .to_camel();
        let abi = std::fs::read_to_string(&p.path())?;
        let result = generate_contract_binding(abi, &contract_name, &mut struct_id)?;
        models.extend(result.1);
        impls.extend(result.2);
        scopes.insert(contract_name, result.0);
    }

    let mut models_ = Module::new("models");
    dedup(models)
        .into_iter()
        .sorted_by(|a, b| a.ty.cmp(&b.ty))
        .dedup_by(|a, b| a.ty.eq(&b.ty))
        .map(construct_struct)
        .for_each(|x| models_ = models_.push_struct(x).clone());
    impls.into_iter().for_each(|x| match x {
        (Some(a), Some(b)) => {
            models_ = models_.push_impl(a).push_impl(b).clone();
        }
        (Some(a), _) => models_ = models_.push_impl(a).clone(),
        (_, Some(a)) => models_ = models_.push_impl(a).clone(),
        _ => {}
    });
    let mut models = models_.import("super::prelude", "*").clone();
    match args.out_path {
        None => {
            for md in scopes {
                println!("{}\n{}", md.0, md.1.to_string());
            }
            println!("{}", models.scope().to_string());
        }
        Some(a) => {
            let mut imports = vec![];
            std::fs::create_dir_all(&a)?;
            let models_path = a.join(Path::new("models.rs"));
            imports.push("models".to_string());
            imports.push("prelude".to_string());
            for (name, mode) in scopes {
                let name = name.to_snake();
                imports.push(name.clone());
                let file_path = PathBuf::from(name + ".rs").pipe(|x| a.join(x));
                std::fs::write(&file_path, mode.to_string())?;
                if args.fmt {
                    std::process::Command::new("rustfmt")
                        .args(&[file_path])
                        .spawn()?;
                }
            }
            std::fs::write(&models_path, models.scope().to_string())?;
            if args.fmt {
                std::process::Command::new("rustfmt")
                    .args(&[models_path])
                    .spawn()?;
            };
            let md_path = a.join(Path::new("mod.rs"));
            let res = imports
                .into_iter()
                .map(|x| format!("pub mod {};", x))
                .join("\n");
            std::fs::write(&md_path, res)?;
        }
    };
    Ok(())
}

fn generate_contract_binding(
    abi: String,
    contract_name: &str,
    struct_id: &mut usize,
) -> Result<(Scope, Vec<StructData>, ImplData)> {
    let data = ton_abi::Contract::load(std::io::Cursor::new(abi))
        .convert()
        .context("Failed parsing json as contract")?;
    let mut scope = codegen::Scope::new();
    let mut models_data = vec![];
    let mut impl_data = vec![];
    let functions = data.functions();
    if !functions.is_empty() {
        let functions = functions_gen(
            BTreeMap::from_iter(functions.clone()),
            contract_name,
            struct_id,
        )?;
        *scope.new_module("functions") = functions.0;
        impl_data.extend(functions.2);
        models_data.extend(functions.1)
    }
    let events = data.events();
    if !events.is_empty() {
        let events = events_gen(
            BTreeMap::from_iter(events.clone()),
            contract_name,
            struct_id,
        )?;
        *scope.new_module("events") = events.0;
        models_data.extend(events.1);
        impl_data.extend(events.2);
    }
    let models_data = dedup(models_data);
    Ok((scope, models_data, impl_data))
}

fn module_imports(mut module: Module) -> Module {
    module
        .vis("pub")
        .import("super::super::prelude", "*")
        .import("nekoton_abi;", "TupleBuilder")
        .clone()
}

type ImplData = Vec<(Option<codegen::Impl>, Option<codegen::Impl>)>;

fn events_gen(
    input: BTreeMap<String, Event>,
    contract_name: &str,
    struct_id: &mut usize,
) -> Result<(Module, Vec<StructData>, ImplData)> {
    let mut md = Module::new("events");
    let mut structs = vec![];
    let mut events = vec![];
    let mut impls = vec![];
    for (name, event) in input {
        if event.inputs.is_empty() {
            continue;
        }
        for strct in gen_struct(&event.inputs, name.clone() + "Event", struct_id) {
            structs.push(strct);
        }
        let impl_data = event_impl(event);
        impls.push(impl_data.builder_impl);
        events.push(impl_data.struct_impl);
    }
    struct_impl(contract_name, events, &mut md);
    let structs = dedup(structs);
    Ok((
        module_imports(md)
            .import("nekoton_abi", "EventBuilder")
            .clone(),
        structs,
        impls,
    ))
}

fn params_to_string(params: Vec<Param>) -> String {
    fn param_type_to_string(param: ParamType) -> String {
        match param.clone() {
            ParamType::Tuple(a) => {
                let components = a
                    .into_iter()
                    .map(|x| param_to_string(x.kind, &x.name))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("ParamType::Tuple(vec![{}])", components)
            }
            ParamType::Array(a) => {
                let ty = param_type_to_string(*a);
                format!("ParamType::Array(Box::new({}))", ty)
            }
            ParamType::FixedArray(a, _) => {
                let ty = param_type_to_string(*a);
                format!("ParamType::FixedArray(Box::new({}))", ty)
            }
            ParamType::Map(a, b) => {
                let a = param_type_to_string(*a);
                let b = param_type_to_string(*b);
                format!("ParamType::Map(Box::new({}), Box::new({}))", a, b)
            }
            _ => {
                format!("ParamType::{:?}", param)
            }
        }
    }

    fn param_to_string(param: ParamType, name: &str) -> String {
        let param_type = param_type_to_string(param);
        format!(
            "Param{{name: \"{}\".to_string(), kind: {}}}",
            name, param_type
        )
    }

    let mut res = "vec![".to_string();
    let joined = params
        .into_iter()
        .map(|x| param_to_string(x.kind, &x.name))
        .join(",");
    res += &joined;
    res += "];";
    res
}

struct FunctionImpl {
    struct_impl: codegen::Function,
    builder_impl: (Option<codegen::Impl>, Option<codegen::Impl>), //Input output
}

fn impl_gen(tokens: &str, name: &str) -> codegen::Impl {
    let mut imp = codegen::Impl::new(name);
    let fun = codegen::Function::new("make_params_tuple")
        .vis("pub")
        .ret("ton_abi::ParamType")
        .line("use std::iter::FromIterator;")
        .line(format!("let tokens  = {}", tokens))
        .line("TupleBuilder::from_iter(tokens).build()")
        .clone();
    imp.push_fn(fun);
    imp
}

fn function_impl(function: Function) -> FunctionImpl {
    let headers = params_to_string(function.header);
    let mut fun = codegen::Function::new(&function.name.to_snake())
        .vis("pub")
        .ret("&'static ton_abi::Function")
        .line("static FUNCTION: OnceCell<ton_abi::Function> = OnceCell::new();")
        .line("FUNCTION.get_or_init(|| {")
        .line(format!("let header = {}", headers))
        .clone();

    if !(function.inputs.is_empty() && function.outputs.is_empty()) {
        fun = fun
            .line(format!(
                "let mut builder = FunctionBuilder::new(\"{}\");",
                function.name
            ))
            .clone();
    }
    let input = if !function.inputs.is_empty() {
        let mut res = "let input = ".to_string();
        let input = params_to_string(function.inputs);
        res += &input;
        fun = fun.line(res).clone();
        fun = fun.line("builder = builder.inputs(input);").clone();

        let name = format!("{}Input", function.name)
            .replace(|x| !char::is_alphanumeric(x), "_")
            .to_camel();
        Some(impl_gen(&input, &name))
    } else {
        None
    };
    let output = if !function.outputs.is_empty() {
        let mut res = "let output = ".to_string();
        let output = params_to_string(function.outputs);
        res += &output;
        fun = fun.line(res).clone();
        fun = fun.line("builder = builder.outputs(output);").clone();
        let name = format!("{}Output", function.name)
            .replace(|x| !char::is_alphanumeric(x), "_")
            .to_camel();
        Some(impl_gen(&output, &name))
    } else {
        None
    };
    let struct_impl = fun
        .line("builder.headers(header)")
        .line(".build()")
        .line("})")
        .clone();
    FunctionImpl {
        struct_impl,
        builder_impl: (input, output),
    }
}

fn event_impl(event: Event) -> FunctionImpl {
    let mut fun = codegen::Function::new(&event.name.to_snake())
        .vis("pub")
        .ret("&'static ton_abi::Event")
        .line("static EVENT: OnceCell<ton_abi::Event> = OnceCell::new();")
        .line("EVENT.get_or_init(|| {")
        .clone();
    let input = if !(event.inputs.is_empty()) {
        fun = fun
            .line(format!(
                "let mut builder = EventBuilder::new(\"{}\");",
                event.name
            ))
            .clone();
        let mut res = "let input = ".to_string();
        let input = params_to_string(event.inputs);
        res += &input;
        fun = fun.line(res).clone();
        fun = fun.line("builder = builder.inputs(input);").clone();
        let name = format!("{}Event", event.name)
            .replace(|x| !char::is_alphanumeric(x), "_")
            .to_camel();
        Some(impl_gen(&input, &name))
    } else {
        None
    };
    let fun = fun.line("builder.build()").line("})").clone();
    FunctionImpl {
        struct_impl: fun,
        builder_impl: (input, None),
    }
}

fn functions_gen(
    input: BTreeMap<String, Function>,
    contract_name: &str,
    struct_id: &mut usize,
) -> Result<(Module, Vec<StructData>, ImplData)> {
    let mut md = Module::new("functions");
    let mut structs = vec![];
    let mut funs = vec![];
    let mut impls = vec![];
    for (name, fun) in input {
        if fun.inputs.is_empty() && fun.outputs.is_empty() {
            continue;
        }
        let impl_data = function_impl(fun.clone());
        funs.push(impl_data.struct_impl);
        impls.push(impl_data.builder_impl);
        for strct in gen_struct(&fun.inputs, name.clone() + "Input", struct_id) {
            structs.push(strct);
        }
        for strct in gen_struct(&fun.outputs, name + "Output", struct_id) {
            structs.push(strct);
        }
    }
    struct_impl(contract_name, funs, &mut md);
    let structs = dedup(structs);

    Ok((
        module_imports(md)
            .import("nekoton_abi", "FunctionBuilder")
            .clone(),
        structs,
        impls,
    ))
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
fn dedup(structs: Vec<StructData>) -> Vec<StructData> {
    let mut output = vec![];
    let mut types_to_struct_map = BTreeMap::new();
    let mut normal_structs = vec![];
    let mut tuple_structs = BTreeMap::new(); //Map of same tuples. Like Tuple1(int) -> Tuple2(int)
    for str in structs {
        let name = str.ty.clone();
        if !name.contains("TupleStruct") {
            if str.fields.is_empty() {
                continue;
            }
            normal_structs.push(str.clone());
            continue;
        }
        let new_str = types_to_struct_map
            .entry(str.fields.clone())
            .or_insert_with(|| str.clone());
        tuple_structs.insert(name, new_str.clone());
    }
    for str in normal_structs.iter_mut() {
        for field in str.fields.iter_mut() {
            if field.ty.contains("HashMap") {
                let map_tys: Vec<_> = field.ty.split(',').collect();
                let value = map_tys[1].strip_suffix('>').unwrap();
                match tuple_structs.get(value) {
                    None => {}
                    Some(a) => {
                        let new_type = format!("{},{}>", map_tys[0], a.ty);
                        field.ty = new_type;
                    }
                }
                continue;
            }
            match tuple_structs.get(&field.ty) {
                None => {}
                Some(str) => {
                    field.ty = str.ty.clone();
                }
            }
        }
    }
    tuple_structs
        .values()
        .into_iter()
        .sorted_by(|a, b| a.ty.cmp(&b.ty))
        .dedup_by(|a, b| a.ty.eq(&b.ty))
        .for_each(|x| {
            output.push(x.clone());
        });

    normal_structs.into_iter().for_each(|x| {
        output.push(x);
    });
    output
}

fn construct_struct(data: StructData) -> Struct {
    let mut str = Struct::new(&data.ty.to_camel());

    for field in data.fields {
        let FieldData { name, ty, .. } = field;
        let field_name = name
            .strip_prefix('_')
            .unwrap_or_else(|| name.as_str())
            .pipe(process_field)
            .to_snake();
        let named_abi = format!("#[abi(name = \"{}\")]", name);

        let field = "pub ".to_string() + &field_name;
        let mut annotations = Vec::new();

        if field_name != name {
            annotations.push(named_abi.as_str())
        } else {
            annotations.push("#[abi]")
        };

        if &ty == "ton_types::Cell" {
            annotations.push("#[serde(with = \"nekoton_utils::serde_cell\")]")
        } else if &ty == "ton_block::MsgAddressInt" {
            annotations.push("#[serde(with = \"nekoton_utils::serde_address\")]");
        }

        str = str
            .push_field(Field::new(&field, &ty).annotation(annotations).clone())
            .clone();
    }

    str = str
        .derive("Serialize")
        .derive("Deserialize")
        .derive("Debug")
        .derive("Clone")
        .derive("PackAbi")
        .vis("pub")
        .clone();

    if data.ty.contains("Output") {
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
        abi_type: String,
    },
    Tuple(Vec<StructData>),
    HashMap {
        ty: String,
        name: String,
        abi_type: String,
        tuple: Vec<StructData>,
    },
}

#[derive(Clone, Debug)]
struct StructData {
    ty: String,
    fields: Vec<FieldData>,
}

#[derive(Clone, Hash, Eq, PartialOrd, PartialEq, Ord, Debug)]
struct FieldData {
    name: String,
    ty: String,
    abi_type: String,
}

impl StructData {
    fn new(types: Vec<FieldData>, ty: String) -> Self {
        Self { fields: types, ty }
    }
}

fn gen_struct(types: &[Param], name: String, struct_id: &mut usize) -> Vec<StructData> {
    let mut res = vec![];
    let mut struct_types = vec![];
    for param in types {
        let mapped_types = map_ton_types(param.kind.clone(), param.name.clone(), struct_id);
        match mapped_types {
            MappedType::Ty { ty, name, abi_type } => {
                struct_types.push(FieldData { name, ty, abi_type });
            }
            MappedType::Tuple(a) => {
                res.extend(a.clone());
                let mapped_struct = a.last().unwrap();
                struct_types.push(FieldData {
                    name: param.name.clone(),
                    ty: mapped_struct.ty.clone(),
                    abi_type: param.kind.to_string(),
                });
            }
            MappedType::HashMap {
                ty,
                name,
                abi_type,
                tuple,
            } => {
                struct_types.push(FieldData { name, ty, abi_type });
                res.extend(tuple);
            }
        };
    }
    res.push(StructData::new(struct_types, name));
    res
}

/// Mapped structure name
fn map(types: Vec<Box<MappedType>>, ty: String) -> Vec<StructData> {
    let mut struct_types = vec![];
    let mut structs = vec![];
    for ty in types {
        match *ty {
            MappedType::Ty { ty, name, abi_type } => {
                struct_types.push(FieldData {
                    name,
                    ty: ty.clone(),
                    abi_type,
                });
            }
            MappedType::Tuple(a) => return a,
            MappedType::HashMap {
                ty,
                name,
                abi_type,
                tuple,
            } => {
                struct_types.push(FieldData { name, ty, abi_type });
                structs.extend(tuple.into_iter());
            }
        }
    }
    structs.push(StructData {
        ty,
        fields: struct_types,
    });
    structs
}

/// Maps top types to `StructData`
fn map_ton_types(ty: ParamType, field_name: String, id: &mut usize) -> MappedType {
    let abi_type = ty.to_string();
    let ty = match ty {
        ParamType::Unknown => {
            panic!("Bad type")
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
            *id += 1;
            let types: Vec<Box<MappedType>> = a
                .into_iter()
                .map(|x| map_ton_types(x.kind, x.name, id))
                .map(Box::new)
                .collect();
            return MappedType::Tuple(map(types, format!("TupleStruct{}", id)));
        }
        ParamType::Array(a) => return map_ton_types(*a, field_name, id),
        ParamType::FixedArray(a, _) => return map_ton_types(*a, field_name, id),
        ParamType::Cell => "ton_types::Cell".to_string(),
        ParamType::Map(a, b) => {
            let a = match map_ton_types(*a, field_name.clone(), id) {
                MappedType::Ty { ty, .. } => (ty),
                MappedType::Tuple(..) => {
                    unimplemented!("Tuple can't be key")
                }
                MappedType::HashMap { .. } => {
                    unimplemented!("Map can't be key")
                }
            };
            let b = match map_ton_types(*b, field_name.clone(), id) {
                MappedType::Ty { ty, .. } => (ty),
                MappedType::Tuple(d) => {
                    let struct_name = d.first().unwrap().ty.clone();
                    let ty = format!("HashMap<{},{}>", a, struct_name);
                    return MappedType::HashMap {
                        ty,
                        name: field_name,
                        abi_type,
                        tuple: d,
                    };
                }
                MappedType::HashMap { .. } => {
                    unimplemented!("Map can't be a value")
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
    MappedType::Ty {
        ty,
        name: field_name,
        abi_type,
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;
    use std::fs::File;
    use std::iter::FromIterator;

    #[test]
    fn test() {
        let abi = File::open("test/EthereumEventConfiguration.abi.json").unwrap();
        let abi = ton_abi::Contract::load(abi).unwrap();
        let res = super::functions_gen(BTreeMap::from_iter(abi.functions().clone()), "wd", &mut 1)
            .unwrap();
        dbg!(res.1);
    }
}
