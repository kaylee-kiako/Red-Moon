#![warn(missing_docs)]
//! A Lua VM designed for multiplayer games with player-made scripts.
//!
//! Red Moon aims to be **safe and sandboxed** when handling user scripts,
//! with **full rollback and serialization support.**
//!
//! ## Safety
//!
//! Parts of the standard library that are unsafe for untrusted code are kept obvious in Red Moon, such as `io` and `os`.
//!
//! For unsafe functions in non-obvious parts of Lua's standard library:
//!
//! - `load()` + `loadfile()`, currently unimplemented. Loading arbitrary bytecode should be memory safe, but currently can cause panics from `unreachable!()` / `unwrap()` checks, this may change in the future to prevent deserialized VMs from causing crashes.
//! - `require()` is currently unimplemented. When it is implemented: there will be no support for loading C libraries (to prevent execution of untrusted files that could be on the system from other programs, and from inherent compatibility issues).
//!
//! ## Examples
//!
//! #### Running Lua Scripts
//!
//! Red Moon supports loading arbitrary scripts:
//!
//! ```rust
//! use red_moon::interpreter::Vm;
//! use red_moon::languages::lua::{LuaCompiler, std::impl_basic};
//!
//! let mut vm = Vm::default();
//! let compiler = LuaCompiler::default();
//! let ctx = &mut vm.context();
//! // Implements `print` (among others).
//! // See "Native Functions" for defining your own functions.
//! impl_basic(ctx).unwrap();
//!
//! let script = r#"print("Hello, world!")"#;
//!
//! let compiled_script = compiler.compile(script).unwrap();
//! let function_ref = ctx.load_function(
//!   "hello_world.lua",
//!   None, // equivalent to `Some(ctx.default_environment())`
//!   compiled_script,
//! ).unwrap();
//!
//! // Call our script with no args.
//! function_ref.call::<_, ()>((), ctx).unwrap();
//! /* Hello, world! */
//! ```
//!
//! #### Native Functions
//!
//! Access to native Rust functions can be provided with
//! [`VmContext::create_function`](crate::interpreter::VmContext::create_function).
//!
//! Note that functions must be `Clone + 'static` to support rollback, and for
//! this reason should avoid shared mutability (e.g., with `Rc<Cell<T>>`).
//!
//! To support serialization, functions should also avoid using captures to
//! store state, as these cannot be serialized by the VM and must be handled
//! seperately.
//!
//! ```
//! use red_moon::interpreter::Vm;
//!
//! let mut vm = Vm::default();
//! let ctx = &mut vm.context();
//!
//! let greet = ctx.create_function(|call_ctx, ctx| {
//!   // We can fetch arguments provided with `get_arg` or `get_args`.
//!   let name = call_ctx.get_arg::<String>(0, ctx)?;
//!   // And can push return values with `return_values`.
//!   call_ctx.return_values(format!("Hello, {}!", &name), ctx)
//! });
//!
//! // The function can now be accessed in Lua as `greet`!
//! let env = ctx.default_environment();
//! env.set("greet", greet, ctx).unwrap();
//! ```
//!
//! #### App Data
//!
//! App data can be provided to the VM with
//! [`VmContext::set_app_data`](crate::interpreter::VmContext::set_app_data),
//! and retrieved from anywhere with
//! [`VmContext::app_data`](crate::interpreter::VmContext::app_data) or
//! [`VmContext::app_data_mut`](crate::interpreter::VmContext::app_data_mut).
//!
//! App data must be `Clone + 'static` for rollback support.
//!
//! ```rust
//! use red_moon::interpreter::Vm;
//!
//! // App data must be `Clone + 'static`!
//! #[derive(Clone, Debug, PartialEq, Eq)]
//! struct MyData(i32);
//!
//! let my_data = MyData(-32);
//!
//! let mut vm = Vm::default();
//! let ctx = &mut vm.context();
//!
//! ctx.set_app_data::<MyData>(my_data);
//!
//! assert_eq!(
//!   Some(MyData(-32)),
//!   ctx.app_data::<MyData>().cloned(),
//! );
//! ```
//!
//! ## Snapshots & Rollback
//!
//! Red Moon supports rollback, with some limitations (see below). Snapshots are
//! created with [`Vm::clone`](red_moon::interpreter::Vm::clone), which creates
//! an independent\* copy of the VM, and can be restored to the VM via
//! `Vm::clone_from`.
//!
//! To support rollback functionality, all data added to the system (app data
//! and native functions) **must** be `Clone + 'static`. To maintain rollback
//! integrity, app data and native functions **must not** allow mutating shared
//! data. This means:
//! - Non-shared mutable data (e.g. `Cell<T>`) is fine.
//! - Shared immutable data (e.g. `Rc<T>`) is fine.
//! - Shared mutable data (e.g., `Rc<Cell<T>>`) will cause issues, since
//!   changes in one snapshot will affect others.
//!
//!
//! ```rust
//! use red_moon::interpreter::Vm;
//!
//! let mut vm = Vm::default();
//! let ctx = &mut vm.context();
//!
//! let env = ctx.default_environment();
//! env.set("a", 1, ctx).unwrap();
//!
//! // Capture our snapshot where `a = 1`.
//! let snapshot = vm.clone();
//!
//! // We can set `a = 2` in our VM.
//! let ctx = &mut vm.context();
//! env.set("a", 2, ctx).unwrap();
//! let a = env.get::<_, i32>("a", ctx).unwrap();
//! assert_eq!(a, 2);
//!
//! // Rewind!
//! vm.clone_from(&snapshot);
//!
//! // And now `a = 1` again!
//! let ctx = &mut vm.context();
//! assert_eq!(1, env.get::<_, i32>("a", ctx).unwrap());
//! ```
//!
//! ## Serialization
//!
//! The `serde` feature enables serialization support through [serde].
//!
//! Built-in Lua values and functions are serialized without issue, but special
//! attention is necessary for app data and native Rust functions:
//!
//! - App data is not serialized and must be handled seperately from the VM.
//! - Rust functions are not serialized directly, but can be reloaded through
//!   rehydration after deserialization.
//! - Captures (e.g., of a closure) are not serialized and must be handled
//!   seperately.
//! - Dynamically-created Rust functions should generally be avoided when
//!   possible.
//!
//! ```
//! #![cfg(feature = "serde")]
//!
//! use red_moon::interpreter::{Vm, VmContext, FunctionRef, Value};
//! use red_moon::errors::RuntimeError;
//!
//! /// Implements `greet`, returning whether it was rehydrated.
//! fn implement_greet(ctx: &mut VmContext) -> Result<bool, RuntimeError> {
//!   let greet = ctx.create_function(|call_ctx, ctx| {
//!     call_ctx.return_values("hi!", ctx)
//!   });
//!
//!   // Rehydrate our function by using a tag.
//!   // The first time, this just tags our function and returns `false`.
//!   // If we've seen this tag before, it'll overwrite those functions with
//!   // this one and return `true`.
//!   let rehydrating = greet.rehydrate("greet", ctx)?;
//!
//!   // We don't want to set values when rehydrating, since a script might have
//!   // moved or replaced it.
//!   if !rehydrating {
//!     let env = ctx.default_environment();
//!     env.set("greet", greet, ctx).unwrap();
//!   }
//!   Ok(rehydrating)
//! }
//!
//! let mut vm = Vm::default();
//! let ctx = &mut vm.context();
//!
//! implement_greet(ctx).unwrap();
//!
//! // Copy our function somewhere else.
//! let env = ctx.default_environment();
//! let foo: Value = env.get("greet", ctx).unwrap();
//! env.set("welcome", foo, ctx).unwrap();
//!
//! // Serializing to send or store.
//! let serialized_vm = bincode::serialize(&vm).unwrap();
//!
//! // Later, when we want to deserialize.
//! let mut vm: Vm = bincode::deserialize(&serialized_vm).unwrap();
//! let ctx = &mut vm.context();
//!
//! // We rehydrate the function!
//! assert!(implement_greet(ctx).unwrap());
//!
//! let env = ctx.default_environment();
//!
//! // `greet` was restored!
//! let greet: FunctionRef = env.get("greet", ctx).unwrap();
//! let greet_result: String = greet.call((), ctx).unwrap();
//! assert_eq!(greet_result, "hi!");
//!
//! // `welcome` was also restored!
//! let welcome: FunctionRef = env.get("welcome", ctx).unwrap();
//! let welcome_result: String = welcome.call((), ctx).unwrap();
//! assert_eq!(welcome_result, "hi!");
//! ```

mod vec_cell;

#[cfg(feature = "serde")]
mod serde_util;

pub mod errors;
pub mod interpreter;
pub mod languages;

type BuildFastHasher = rustc_hash::FxBuildHasher;
type FastHashMap<K, V> = std::collections::HashMap<K, V, BuildFastHasher>;
type FastHashSet<K> = std::collections::HashSet<K, BuildFastHasher>;

// https://github.com/rust-lang/cargo/issues/383#issuecomment-720873790
#[cfg(doctest)]
mod test_readme {
    macro_rules! external_doc_test {
        ($x:expr) => {
            #[doc = $x]
            extern "C" {}
        };
    }

    external_doc_test!(include_str!("../README.md"));
}

macro_rules! debug_unreachable {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        unreachable!($($arg)*)
    };
}

pub(crate) use debug_unreachable;
