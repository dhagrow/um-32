use std::mem;
use std::fs::File;
use std::vec::Vec;
use std::io::prelude::*;

struct Machine {
    memory: Vec<Vec<u32>>,
}

impl Machine {
    fn new() -> Machine {
        Machine{ memory: Vec::new() }
    }

    fn load(&mut self, filename: &str) {
        let mut f = File::open(filename).expect("file not found");

        let mut buffer = [0; 4];
        let mut platter: u32;

        let mut program = Vec::new();

        loop {
            f.read_exact(&mut buffer).expect("failed to read platter");
            unsafe { platter = mem::transmute::<[u8; 4], u32>(buffer).to_be(); }

            program.push(platter);
        }

        println!("{:?}", program.len());
        self.memory.push(program);
    }
}

fn main() {
    let mut machine = Machine::new();
    let filename = "../scrolls/sandmark.umz";

    machine.load(filename);
}
