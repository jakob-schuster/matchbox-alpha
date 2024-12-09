use std::{fs::File, io::Read};

use ast::{
    core::{
        elab::{ElabError, Elaborate},
        eval::EvalError,
        handler::EffectHandler,
        Context,
    },
    parse::ParseError,
    read,
};
use cli::GlobalConfig;
use indicatif::ProgressBar;

use crate::util::{Arena, Par};

use crate::*;

mod ast;

fn read_code_from_script(script: &str) -> String {
    let mut code = String::new();

    File::open(script)
        .unwrap()
        .read_to_string(&mut code)
        .unwrap();

    code
}

pub fn debug(code_file: &str, global_config: &GlobalConfig) {
    let code = read_code_from_script(code_file);

    let prog = ast::Prog::parse(&code[..], global_config).unwrap();
    let arena = Arena::new();
    let ctx = Context::default();
    let cprog = ast::core::Prog::elab(&prog, &arena)
        .unwrap()
        .simplify(&arena, &ctx);

    println!("cprog: {:?}", cprog);
}

pub fn script(reads: &str, script: &str, global_config: &GlobalConfig) {
    run(reads, &read_code_from_script(script), global_config).unwrap();
}

#[derive(Debug)]
pub enum RunError<'a> {
    ParseError(ParseError),
    ElabError(ElabError),
    EvalError(EvalError<'a>),
}
pub fn run<'a>(reads: &str, code: &str, global_config: &GlobalConfig) -> Result<(), RunError<'a>> {
    rayon::ThreadPoolBuilder::new()
        .num_threads(global_config.threads)
        .build_global()
        .unwrap();

    eprintln!("Matchbox v0.1");

    // parse the program into an AST
    let prog = ast::Prog::parse(code, global_config).map_err(RunError::ParseError)?;

    // arena for memory allocation
    let arena = Arena::new();

    // compile the AST into a core program
    let core_prog = ast::core::Prog::elab(&prog, &arena).map_err(RunError::ElabError)?;

    // context of execution executing things in
    let ctx = Context::default();
    // simplify the core program
    let simple_core_prog = core_prog.simplify(&arena, &ctx).unwrap();

    // create a reader for the input reads
    let reader = read::ReaderWrapped::new(reads, global_config.chunk_size, None).unwrap();

    let mut handler = EffectHandler::default();

    // map across all reads in the input
    reader.map(
        |read| {
            let quick_arena = Arena::new();
            match simple_core_prog.eval(&read, &quick_arena) {
                Ok(a) => a,
                Err(e) => {
                    println!("{:?}", e);
                    vec![]
                }
            }
        },
        |effs, handler| {
            effs.iter().for_each(|eff| handler.handle(eff));
        },
        &mut handler,
    );

    handler.finish();

    Ok(())
}

// pub fn repl(input: &str, global_config: &GlobalConfig) {
//     let mut rl = DefaultEditor::new().unwrap();

//     // #[cfg(feature = "with-file-history")]
//     // if rl.load_history("history.txt").is_err() {
//     //     println!("No previous history.");
//     // }
//     loop {
//         let readline = rl.readline("> ");
//         match readline {
//             Ok(line) => {
//                 println!();
//                 match run(input, &line, global_config) {
//                     Ok(_) => {}
//                     Err(e) => println!("{:?}", e),
//                 }
//                 println!();
//             }
//             Err(ReadlineError::Interrupted) => {
//                 println!("CTRL-C");
//                 break;
//             }
//             Err(ReadlineError::Eof) => {
//                 println!("CTRL-D");
//                 break;
//             }
//             Err(err) => {
//                 println!("Error: {:?}", err);
//                 break;
//             }
//         }
//     }
// }
