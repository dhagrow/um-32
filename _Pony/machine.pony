actor Main
    new create(env: Env) =>
        let buffer = recover val Array[U8].create() end
        env.out.print(buffer)
