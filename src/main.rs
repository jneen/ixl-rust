use std::fs::File;
use std::env;
use parser::Scanner;

mod parser;

fn main() {
	let args: Vec<String> = env::args().collect();
	let file_path = &args[1];
	let mut file = File::open(file_path).unwrap();
	
	let mut scanner = Scanner::from_reader(&mut file);
	let program = scanner.parse();
	println!("{:#?}", program);
}
