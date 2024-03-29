use crate::dict::FactoryParameters;
use crate::util::PrefixSuffix;

use clap::{App, Arg};
use std::path::Path;

mod dict;
mod util;


const EXIT_INVALID_FILE: i32 = 1;
const EXIT_PARSING_ERROR: i32 = 2;
const EXIT_BUILDING_ERROR: i32 = 3;


fn main() {

    let matches = App::new("Dictionary Generator")
        .version("1.0.3")
        .author("Théo Rozier <contact@theorozier.fr>")
        .about("Generate dictionary C source files from dictionary and factory parameters files")
        .arg(Arg::with_name("DICT")
            .index(1)
            .required(true)
            .help("Dictionary file path (.dict)"))
        .arg(Arg::with_name("DEST")
            .index(2)
            .required(true)
            .help("Destination directory for source files"))
        .arg(Arg::with_name("factory_params_path")
            .long("factory")
            .short("f")
            .takes_value(true)
            .help("Factory parameters file path (.fact)"))
        .arg(Arg::with_name("header_extension_path")
            .long("header-ext")
            .short("h")
            .takes_value(true)
            .help("An optional path to a file that will be included at the top of the header file"))
        .arg(Arg::with_name("base_file_name")
            .long("base-name")
            .default_value("dictionary")
            .help("Change the default name of the header and source files following the pattern '<name>.h' and '<name>.c'"))
        .arg(Arg::with_name("include_guard")
            .long("inc-guard")
            .default_value("__DICTIONARY_H")
            .help("Change the default include guard of the header source file (#ifndef [...] #define [...] #endif)"))
        .arg(Arg::with_name("index_define_prefix")
            .long("idx-def-prefix")
            .default_value("DICT_")
            .help("Change the default prefix prepended to '#define' of entry indices"))
        .arg(Arg::with_name("index_define_suffix")
            .long("idx-def-suffix")
            .default_value("_IDX")
            .help("Change the default suffix appended to '#define' of entry indices"))
        .get_matches();

    println!("Parsing dictionary file...");
    let dict_path = matches.value_of("DICT").unwrap();
    let dict = match dict::parser::parse_dictionary_from_file(dict_path) {
        Ok(res) => match res {
            Ok(dict) => dict,
            Err(err) => {
                eprintln!("Failed to parse dictionary file at '{}':\n{}", dict_path, err);
                std::process::exit(EXIT_PARSING_ERROR);
            }
        },
        Err(err) => {
            eprintln!("Dictionary file was not found at '{}': {}", dict_path, err);
            std::process::exit(EXIT_INVALID_FILE);
        }
    };

    let mut fact = FactoryParameters::new(&dict);

    if let Some(fact_path) = matches.value_of("factory_params_path") {
        println!("Parsing factory parameters file...");
        match dict::parser::parse_factory_parameters_from_file(fact_path, &mut fact) {
            Ok(res) => match res {
                Ok(_) => {},
                Err(err) => {
                    eprintln!("Failed to parse factory parameters file at '{}':\n{}", fact_path, err);
                    std::process::exit(EXIT_PARSING_ERROR);
                }
            },
            Err(err) => {
                eprintln!("Factory parameters file was not found at '{}': {}", dict_path, err);
                std::process::exit(EXIT_INVALID_FILE);
            }
        };
    }

    fact.fix_missing_values();

    let header_extension_path = matches.value_of("header_extension_path").map(|raw_path| Path::new(raw_path));
    let base_file_name = matches.value_of("base_file_name").unwrap();
    let include_guard = matches.value_of("include_guard").unwrap();

    let dir_path = Path::new(matches.value_of("DEST").unwrap());

    if let Err(err) = std::fs::create_dir_all(dir_path) {
        eprintln!("Failed to make the destination directory: {}", err);
        std::process::exit(EXIT_BUILDING_ERROR);
    }

    println!("Building header file...");

    let index_define_template = PrefixSuffix {
        prefix: matches.value_of("index_define_prefix").unwrap(),
        suffix: matches.value_of("index_define_suffix").unwrap()
    };

    let header_file_name = format!("{}.h", base_file_name);
    let header_path = dir_path.join(&header_file_name);
    if let Err(err) = dict::source::build_header(&dict, header_path, include_guard, header_extension_path, index_define_template) {
        eprintln!("Failed to build header file: {}", err);
        std::process::exit(EXIT_BUILDING_ERROR);
    }

    println!("Building source file...");
    let source_path = dir_path.join(format!("{}.c", base_file_name));
    if let Err(err) = dict::source::build_source(&fact, source_path, header_file_name.as_str()) {
        eprintln!("Failed to build source file: {}", err);
        std::process::exit(EXIT_BUILDING_ERROR);
    }

    println!("Successfully built header and source files.");

}
