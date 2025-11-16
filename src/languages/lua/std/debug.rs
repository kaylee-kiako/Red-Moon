use crate::errors::RuntimeError;
use crate::interpreter::{TableRef, Value, VmContext};

pub fn impl_debug(ctx: &mut VmContext) -> Result<(), RuntimeError> {
    // getregistry
    let getregistry =
        ctx.create_function(|call_ctx, ctx| call_ctx.return_values(ctx.registry(), ctx));
    let rehydrating = getregistry.rehydrate("debug.getregistry", ctx)?;

    // getmetatable
    let getmetatable = ctx.create_function(|call_ctx, ctx| {
        let value: Value = call_ctx.get_args(ctx)?;

        let metatable = match value {
            Value::String(_) => Some(ctx.string_metatable()),
            Value::Table(table) => table.metatable(ctx)?,
            _ => return Ok(()),
        };

        call_ctx.return_values(metatable, ctx)
    });
    getmetatable.rehydrate("debug.getmetatable", ctx)?;

    // setmetatable
    let setmetatable = ctx.create_function(|call_ctx, ctx| {
        let (value, metatable): (Value, Option<TableRef>) = call_ctx.get_args(ctx)?;

        if let Value::Table(table) = &value {
            table.set_metatable(metatable.as_ref(), ctx)?;
        }

        call_ctx.return_values(value, ctx)
    });
    setmetatable.rehydrate("debug.setmetatable", ctx)?;

    if !rehydrating {
        let debug = ctx.create_table();
        debug.raw_set("debug", getmetatable, ctx)?;

        let env = ctx.default_environment();
        env.set("debug", debug, ctx)?;
    }

    Ok(())
}
