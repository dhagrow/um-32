use std::io;
use std::mem;
use std::char;
use std::fs::File;
use std::vec::Vec;
use std::io::prelude::*;

#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Debug)]
enum Operator {
    cmv, aix, aam, add, mul, dvi, nad, hlt, alc, abd, out, inp, lod, ort,
}

fn from_u8(n: u8) -> Option<Operator> {
    if n <= 13 {
        Some(unsafe { mem::transmute(n) })
    } else {
        None
    }
}

struct Machine {
    memory: Vec<Vec<u32>>,
    reg: [u32; 8],
    abandoned_indexes: Vec<u32>,
    stop: bool,
}

impl Machine {
    fn new() -> Machine {
        Machine{
            memory: vec![],
            reg: [0; 8],
            abandoned_indexes: vec![],
            stop: false,
            }
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

    fn run(&mut self) {
        let reg = &mut self.reg;
        let mut finger: u32 = 0;
        let mut platter: u32;

        let mut code: u8;
        let mut a: usize;
        let mut b: usize;
        let mut c: usize;
        let mut val: u32;

        while !self.stop {
            platter = self.memory[0][finger as usize];

            code = (platter >> 28) as u8;

            match from_u8(code) {
                Some(Operator::ort) => {
                    a = ((platter >> 25) & 7) as usize;
                    val = platter & 0x1ffffff;

                    reg[a] = val as u32;
                },
                Some(code) => {
                    a = ((platter >> 6) & 7) as usize;
                    b = ((platter >> 3) & 7) as usize;
                    c = (platter & 7) as usize;

                    match code {
                        Operator::cmv => {
                            if reg[c] != 0 {
                                reg[a] = reg[b];
                            }
                        },
                        Operator::aix => reg[a] = self.memory[reg[b] as usize][reg[c] as usize],
                        Operator::aam => self.memory[reg[a] as usize][reg[b] as usize] = reg[c],
                        Operator::add => reg[a] = reg[b].wrapping_add(reg[c]),
                        Operator::mul => reg[a] = reg[b].wrapping_mul(reg[c]),
                        Operator::dvi => reg[a] = reg[b].wrapping_div(reg[c]),
                        Operator::nad => reg[a] = !(reg[b] & reg[c]),
                        Operator::alc => {
                            Machine::state(reg, code, a, b, c);
                            let new_mem = vec![0, reg[c]];
                            match self.abandoned_indexes.pop() {
                                Some(index) => {
                                    self.memory[index as usize] = new_mem;
                                    reg[b] = index;
                                },
                                None => {
                                    self.memory.push(new_mem);
                                    reg[b] = (self.memory.len()-1) as u32;
                                },
                            };
                        },
                        Operator::abd => {
                            self.memory.remove(reg[c] as usize);
                            self.abandoned_indexes.push(reg[c]);
                        },
                        Operator::out => {
                            print!("{}", char::from_u32(reg[c]).unwrap());
                            io::stdout().flush().unwrap();
                        },
                        Operator::lod => {
                            if reg[b] != 0 {
                                self.memory[0] = self.memory[reg[b] as usize].to_vec();
                            }
                            finger = reg[c] - 1;
                        },
                        _ => {
                            println!("unknown code: {:?}", code);
                            break;
                        }
                    }
                },
                None => {
                    println!("unknown code: {}", code);
                    break;
                },
            }

            finger += 1;
        }
    }

    fn state(reg: &[u32; 8], code: Operator, a: usize, b: usize, c: usize) {
        println!("{:?}({}, {}, {})", code, a, b, c);
        println!("reg {:?}", reg);
    }
}

fn main() {
    let mut machine = Machine::new();
    let filename = "../scrolls/sandmark.umz";

    machine.load(filename);
    machine.run();
}
