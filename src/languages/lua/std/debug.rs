use crate::errors::RuntimeError;
use crate::interpreter::{ByteString, FunctionRef, HookMask, TableRef, Value, VmContext};

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

    // sethook
    let sethook = ctx.create_function(|call_ctx, ctx| {
        let Some(callback) = call_ctx.get_arg::<Option<FunctionRef>>(0, ctx)? else {
            // when the first arg is nil, ignore the remaining args and remove the hook
            ctx.remove_hook();
            return Ok(());
        };

        let (mask_string, count): (ByteString, Option<usize>) = call_ctx.get_args_at(1, ctx)?;

        // resolve mask
        let mut mask = HookMask::default();

        for byte in mask_string.as_bytes() {
            match byte {
                b'c' => mask.set(HookMask::CALL, true),
                b'r' => mask.set(HookMask::RETURN, true),
                b'l' => mask.set(HookMask::LINE, true),
                _ => {}
            }
        }

        let count = count.unwrap_or(0);

        if count > 0 {
            mask.set(HookMask::INSTRUCTION, true);
        }

        ctx.set_hook(mask, count, callback)?;

        Ok(())
    });
    sethook.rehydrate("debug.sethook", ctx)?;

    if !rehydrating {
        let debug = ctx.create_table();
        debug.raw_set("getregistry", getregistry, ctx)?;
        debug.raw_set("getmetatable", getmetatable, ctx)?;
        debug.raw_set("setmetatable", setmetatable, ctx)?;
        debug.raw_set("sethook", sethook, ctx)?;

        let env = ctx.default_environment();
        env.set("debug", debug, ctx)?;
    }

    Ok(())
}
