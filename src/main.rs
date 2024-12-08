mod lang;
mod myers;
mod util;

fn main() {
    cli::main()
}

mod cli {
    pub fn main() {
        let args = Cli::parse();

        match args.command {
            Commands::Run {
                reads,
                code,
                global_config,
            } => lang::run(&reads, &code, &global_config).unwrap(),
            Commands::Script {
                reads,
                script,
                global_config,
            } => lang::script(&reads, &script, &global_config),
            Commands::Debug {
                script,
                global_config,
            } => lang::debug(&script, &global_config),
        }
    }

    use clap::{Parser, Subcommand};

    use crate::lang;

    #[derive(Debug, Parser)]
    #[command(name = "matchbox")]
    struct Cli {
        #[command(subcommand)]
        command: Commands,
    }

    #[derive(Debug, Parser, Clone)]
    pub struct GlobalConfig {
        #[arg(short, long, default_value_t = 0.2)]
        pub error: f32,

        #[arg(short, long, default_value_t = 1)]
        pub threads: usize,

        #[arg(short, long, default_value_t = 10000)]
        pub chunk_size: usize,
    }

    #[derive(Debug, Subcommand)]
    enum Commands {
        /// Run Matchbox code directly on the command line
        Run {
            code: String,
            reads: String,
            #[command(flatten)]
            global_config: GlobalConfig,
        },

        /// Execute a Matchbox script
        Script {
            script: String,
            reads: String,
            #[command(flatten)]
            global_config: GlobalConfig,
        },

        /// Parse and compile a script, and print out the core operations
        Debug {
            script: String,
            #[command(flatten)]
            global_config: GlobalConfig,
        },
    }
}
