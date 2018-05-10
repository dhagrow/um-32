use std::mem;
use std::fs::File;
use std::vec::Vec;
use std::io::prelude::*;

struct Machine {
    memory: Vec<Vec<u32>>,
    stop: bool,
}

impl Machine {
    fn new() -> Machine {
        Machine{ memory: Vec::new(), stop: false }
    }

    fn load(&mut self, filename: &str) {
        let mut f = File::open(filename).expect("file not found");

        let mut buffer = [0; 4];
        let mut platter: u32;

        let mut program = Vec::new();

        while f.read_exact(&mut buffer).is_ok() {
            unsafe { platter = mem::transmute::<[u8; 4], u32>(buffer).to_be(); }
            program.push(platter);
        }

        self.memory.push(program);
    }

    fn run(&self) {
        let program = &self.memory[0];
        let mut finger: u32 = 0;
        let mut platter: u32;

        let mut code: u8;

        while !self.stop {
            if finger == 10 {
                break;
            }
            platter = program[finger as usize];
            println!("{:032b}", platter);

            code = (platter >> 28) as u8;
            println!("code: {}", code);

            finger += 1;
        }
    }
}

fn main() {
    let mut machine = Machine::new();
    let filename = "../scrolls/sandmark.umz";

    machine.load(filename);
    machine.run();
}
